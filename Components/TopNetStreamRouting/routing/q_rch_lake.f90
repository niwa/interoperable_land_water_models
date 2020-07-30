!SUBROUTINE Q_RCH_LAKE(IENS,JRCH,NLIN,NLST,NLUT,NHYD,ISTEP,RSTEP)
! ----------------------------------------------------------------------------------------
!
! Creator:
!   David Rupp and Martyn Clark, 2006
!   Update Suzanne Poyck, 2008
!
! Purpose:
!   Route flow thru lakes and thru reaches (via qroute.f90)
!
! ----------------------------------------------------------------------------------------
! I/O::
!
!   Input(s):
!      IENS: ensemble member
!      JRCH: index of stream segment
!      NLIN: number of lake inflow observation sites
!      NLST: number of lake stage observation sites
!      NLUT: number of lake outflow observation sites
!      NHYD: number of river gauge observation sites
!     ISTEP: index of time step
!     RSTEP: index offset for retrospective time step
!
!   Outputs:
!     (none, updates data structures)
!
! ----------------------------------------------------------------------------------------
! Structures Used:
!
!   (0) MODEL_TIME
!   --------------
!
!   (1) REACHPARAM
!   --------------
!
!   (2) LAKE_PARAM
!   --------------
!
!   (3) LAKE_STATE
!   --------------
!
!   (4) REACH_FLUX
!   --------------
!
!   (5) BASIN_FLUX
!   --------------
!
!   (6) LAKES_FLUX
!   --------------
!
!   (7) BASINFORCE
!   --------------
!
!   (8) BASINPARAM
!   --------------
!
! ----------------------------------------------------------------------------------------
FUNCTION  Q_RCH_LAKE(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,APRECIP,LPARAM,LSTATE,LAKFLX,MAXQPAR,RSTEP)
!!DIR$ ATTRIBUTES DLLEXPORT :: Q_RCH_LAKE
!!DIR$ ATTRIBUTES ALIAS:'_q_rch_lake_' :: Q_RCH_LAKE

USE nrtype
!USE userparam
!USE user_state
!USE inputdat2d
!USE ret_moddat
USE interreach, ONLY: qroute_rch
use reachtype
!use bastype
use laketype
IMPLICIT NONE
INTEGER(I4B)                                  :: Q_RCH_LAKE        ! 
! inputs
REAL(DP), INTENT(IN)                          :: DT       !
REAL(DP),DIMENSION(0:1),INTENT(IN)            :: TBOUNDS     !
INTEGER(I4B), INTENT(IN)                      :: JRCH        ! loop through the stream segments
INTEGER(I4B),INTENT(IN)                       :: NRCH
TYPE(RCHTOPO), DIMENSION(NRCH),INTENT(IN)     :: NETOPO
TYPE(KREACH),DIMENSION(NRCH),INTENT(INOUT)    :: KROUTE
TYPE(RCHPRP),DIMENSION(NRCH),INTENT(IN)       :: RPARAM
TYPE(MODFLX), DIMENSION(NRCH),INTENT(IN)      :: BASFLX
TYPE(STRFLX), INTENT(INOUT)                   :: RCHFLX
INTEGER(I4B), INTENT(IN)                      :: LAKEFLAG        ! 
INTEGER(I4B), INTENT(IN)                      :: NLAK       ! 
type(LAKTOPO), dimension(NLAK), INTENT(IN)    :: LKTOPO 
REAL(DP), INTENT(IN)                          :: APRECIP       ! 
TYPE(LAKPRP), DIMENSION(NLAK), INTENT(IN)     :: LPARAM  ! Lake Parameters
TYPE(LKSTATE), DIMENSION(NLAK), INTENT(INOUT) :: LSTATE
TYPE(LKFLX),DIMENSION(NLAK),INTENT(INOUT)     :: LAKFLX
INTEGER(I4B),INTENT(IN)                       :: MAXQPAR
INTEGER(I4B), INTENT(IN), OPTIONAL            :: RSTEP       ! retrospective time step offset
! looping variable
INTEGER(I4B)                           :: ILAK        ! lake index
INTEGER(I4B)                           :: ILAK_I      ! lake index for inlet reach
INTEGER(I4B)                           :: ILAK_O      ! lake index for outlet reach
INTEGER(I4B)                           :: IOBL        ! looping over lake stations
INTEGER(I4B)                           :: IHYD        ! looping over streamq stations
! lake states and fluxes
REAL(DP)                               :: Q_OUT       ! flow out of lake
REAL(DP)                               :: QAV         ! temporary value for calculating average outflow from lake for timestep
REAL(DP)                               :: LKLVL       ! lake level
REAL(DP)                               :: LKARE       ! lake area
REAL(DP)                               :: LKVOL       ! lake volume
REAL(DP)                               :: M           ! lake shape parameter
REAL(DP)                               :: C           ! level to volume parameter
REAL(DP)                               :: D           ! level to volume parameter
REAL(DP)                               :: A0          ! lake reference area
REAL(DP)                               :: H           ! lake level
REAL(DP)                               :: H0          ! lake reference level
REAL(DP)                               :: D0          ! lake reference depth
REAL(DP)                               :: HL          ! lake level below which outflow is zero
REAL(DP)                               :: HE          ! minimum lake level for ecological functions
REAL(DP)                               :: HS          ! spillway level
REAL(DP)                               :: QE          ! minimum outflow for ecological functions
REAL(DP)                               :: QS          ! outflow at spillway level
REAL(DP)                               :: A           ! outflow rating curve parameter
REAL(DP)                               :: B           ! outflow rating curve parameter
REAL(DP)                               :: PRCP        ! direct precip to lake over time step (m3)
REAL(DP)                               :: EVAP        ! evaporation from lake over time step (m3)
REAL(DP)                               :: MODELSUBTIME !
INTEGER(I4B)                           :: ISUB        ! loop trough model subtimesteps
REAL(DP)                               :: P           ! percentage lake volume change requirement for nsub
INTEGER(I4B)                           :: NSUB_MAX    ! max nr of model subtimesteps
INTEGER(I4B)                           :: NSUB        ! nr of model subtimesteps
INTEGER(I4B)                           :: IUSER       ! loop through users
INTEGER(I4B)                           :: NUSERX      ! number of users in this lake
REAL(DP)                               :: DEMAND      ! demand from this lake (temporary value)
REAL(DP)                               :: DEMANDPS    ! demand per second from this lake (temporary value)
REAL(DP)                               :: TAKE        ! total take from this lake
INTEGER(I4B)                           :: ROFFSET     ! retrospective offset due to rstep
REAL(DP)                               :: LFLX1       ! partial lake fluxes per substep
REAL(DP)                               :: LFLX2       ! partial lake fluxes per substep
REAL(DP)                               :: ALFA        ! constant, 5/3
REAL(DP)                               :: K           ! sqrt(slope)/mannings N
REAL(DP)                               :: W           ! stream width
LOGICAL(LGT)                           :: OUTLET      ! true if lake outlet
LOGICAL(LGT)                           :: INLET       ! true if lake inlet
!INTEGER(I4B)                           :: ret
!IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'in q_rch_lake'
! set the retrospective offset
IF (.NOT.PRESENT(RSTEP)) THEN
  ROFFSET = 0
ELSE
  ROFFSET = RSTEP
END IF
! ----------------------------------------------------------------------------------------
! (1) ADD PRECIPITATION AND SUBTRACT EVAPORATION FROM JRCHth PART OF LAKE
! ----------------------------------------------------------------------------------------
INLET = NETOPO(JRCH)%LAKINLT ! true if reach is a lake inlet
OUTLET = ANY(LKTOPO(:)%DREACHI.EQ.JRCH.AND.LKTOPO(:)%LAKE_ID.GT.0) ! true if reach is a lake outlet
ILAK = NETOPO(JRCH)%LAKE_IX  ! reach is associated with lake ILAK
ILAK_I = -9999; ILAK_O = -9999
IF (INLET)  THEN
  ! reach is an inlet reach into lake ilak
  ILAK_I = ILAK
END IF
IF (OUTLET) THEN
  ! reach is an outlet reach
  ILAK_O = MINVAL(LKTOPO(:)%LAKE_IX, MASK=LKTOPO(:)%DREACHI.EQ.JRCH.AND.LKTOPO(:)%LAKE_ID.GT.0)
END IF
! calculate direct precipitation onto lake area in basin JRCH
! NOTE: all lake fluxes were initialized in init_lakes.f90
IF (OUTLET) THEN
  ! use the lktopo(ilak)%butulak for basin area undar lake for outlet reaches
  PRCP = APRECIP * LKTOPO(ILAK_O)%BUTULAK
  ! compute cumulative precipitation for all basins (until this basin) in lake ilak_o
  LAKFLX(ILAK_O)%LAKE_P = LAKFLX(ILAK_O)%LAKE_P + PRCP / DT
ELSE
  ! use the netopo(jrch)%basulak for basin area under lake for non-outlet reaches
  PRCP = APRECIP * NETOPO(JRCH)%BASULAK
  ! compute cumulative precipitation for all basins (until this basin) in lake ilak
  LAKFLX(ILAK)%LAKE_P = LAKFLX(ILAK)%LAKE_P + PRCP / DT
END IF
! calculate evaporation from lake area in basin JRCH
! NOTE: all lake fluxes were initialized in init_lakes.f90
IF (OUTLET) THEN
  ! use the lktopo(ilak)%butulak for basin area undar lake for outlet reaches
  EVAP = BASFLX(JRCH)%POTEVAP * LKTOPO(ILAK_O)%BUTULAK
  ! compute cumulative evaporation for all basins (until this basin) in lake ilak
  LAKFLX(ILAK_O)%LAKE_E = LAKFLX(ILAK_O)%LAKE_E + EVAP / DT
ELSE
  ! use the netopo(jrch)%basulak for basin area undar lake for non-outlet reaches
  EVAP = BASFLX(JRCH)%POTEVAP * NETOPO(JRCH)%BASULAK
  ! compute cumulative evaporation for all basins (until this basin) in lake ilak
  LAKFLX(ILAK)%LAKE_E = LAKFLX(ILAK)%LAKE_E + EVAP / DT
END IF
! reaches that are totally covered by lake are not processed by the routing
! routines -- therefore it is necessary to add sub-basin water to the cumulative
! lake inflow for all such reaches (sub-basin water is added to reaches by the
! routing routines for all other reaches)
IF (.NOT.OUTLET.AND..NOT.INLET) THEN
  ! add basin water from part of sub-basin that is not covered by lake to lake inflow
  ! NOTE: all lake fluxes were initialized in init_lakes.f90
  LAKFLX(ILAK)%LAKE_I = LAKFLX(ILAK)%LAKE_I + BASFLX(JRCH)%INSTN_Q(1)
END IF
!-------------------------------------------------------------------------------------------
! (5) GET ALL FLUXES TOGETHER, CALCULATE OUTFLOW OF LAKE AND UPDATE LAKE STATUS
!-------------------------------------------------------------------------------------------
! check that we are not in the outlet reach of the lake
IF (OUTLET) THEN
 ! we are in a lake outlet
 IF (LPARAM(ILAK_O)%EXIST) THEN
  ! lake parameters do exist that enable us to calculate lake outflow
  ! GET EXTRACTION FROM IRRIGATION MODULE
  DEMAND=0
  NUSERX=0
  !IF (SIMDAT%UCFFLAG.GE.1) THEN ! only if the irrigation module is active
  !  DO IUSER=1, NUSER
  !    IF (USRPARAM(IUSER)%USER_TAKE_ID.EQ.NETOPO(JRCH)%LAKE_ID .AND. USRPARAM(IUSER)%USER_TAKE_TYPE.EQ. 3) THEN
  !      DEMAND=DEMAND+USERSTATE(IENS,IUSER)%USERDEMAND
  !      NUSERX=NUSERX+1
  !    ENDIF
  !  ENDDO
  !END IF
  DEMANDPS=DEMAND/DT
  TAKE=0 !initialize take
  ! get lake discharge parameters
  HL = LPARAM(ILAK_O)%HGHTLOW
  HE = LPARAM(ILAK_O)%HGHTECO
  HS = LPARAM(ILAK_O)%HGHTSPL
  QE = LPARAM(ILAK_O)%DSCHECO
  QS = LPARAM(ILAK_O)%DSCHSPL
  IF (LPARAM(ILAK_O)%DSCHFUN) THEN
    ! need alpha, k and w for calculating the otuflow using discharge function
    ALFA = 5._DP/3._DP
    K = SQRT(RPARAM(JRCH)%R_SLOPE)/RPARAM(JRCH)%R_MAN_N
    W = RPARAM(JRCH)%R_WIDTH
  ELSE
    ! no need for a and b if they have not been defined (see spldat_get.f90),
    ! because then we calculate lake outflow using the discharge function (not
    ! the stage-discharge relationship).
    A = LPARAM(ILAK_O)%RATECVA
    B = LPARAM(ILAK_O)%RATECVB
  END IF
  ! initial state variables and diagnostic variables at the start of the time step
  LKLVL = LSTATE(ILAK_O)%LAKELVL
  H0 = LPARAM(ILAK_O)%ELEVREF
  D0 = LPARAM(ILAK_O)%DEPHREF
  H = LKLVL-(H0-D0) ! in units of lake depth
  IF (LAKEFLAG.EQ.1) THEN
    ! get lake shape parameters
    M = LPARAM(ILAK_O)%SHAPE_M
    ! get lake reference area and level
    A0 = LPARAM(ILAK_O)%AREAREF
    LKVOL = (A0*D0/M)*(H/D0)**M
  ELSE IF (LAKEFLAG.EQ.2) THEN
    ! get lake shape parameters
    C = LPARAM(ILAK_O)%HE2AR_C
    D = LPARAM(ILAK_O)%HE2AR_D
    LKVOL = (C / (D + 1)) * H**(D + 1)
  END IF
  ! sum of inflow, precipitation and evaporation over one whole model time step (m3)
  LFLX1 = (LAKFLX(ILAK_O)%LAKE_I+LAKFLX(ILAK_O)%LAKE_P-LAKFLX(ILAK_O)%LAKE_E)*DT
  ! define the number of sub-steps so that for each sub-step, the fluxes in lflx1 are never more
  ! than p% of the current total lake volume
  P=10._DP  ! lflx1 is never more than p percentage of lake volume for each substep
  NSUB_MAX=1000 ! maximum number of substeps
  IF (LKVOL.GT.0._DP) THEN
    NSUB=MAX(1,CEILING(100._DP/P*ABS(LFLX1)*DT/LKVOL))
    NSUB=MIN(NSUB,NSUB_MAX)
  ELSE
    NSUB=1
  END IF
  MODELSUBTIME = DT / REAL(NSUB, KIND(MODELSUBTIME) )
  ! loop through sub-steps
  Q_OUT=0
  QAV=0
  IF (NSUB.GT.1) LFLX1 = LFLX1*MODELSUBTIME/DT
  DO ISUB=1,NSUB
    ! calculate outflow for a sub-step (m3 s-1)
    IF (LPARAM(ILAK_O)%DSCHFUN) THEN
      IF (LKLVL.GT.HL) THEN
        Q_OUT = K*W*(LKLVL-HL)**ALFA    ! natural reservoirs (lakes) without a discharge structure
      ELSE
        Q_OUT = 0._DP  ! at level too low for any outflow
      END IF
    ELSE
      IF (LKLVL.GT.HS) THEN
        Q_OUT = QS + A * (LKLVL - HS) ** B  ! unmanaged flows or flood flows in managed reservoirs
      ELSEIF (LKLVL.GT.HE) THEN
        Q_OUT = QS - (QS - QE) * (HS - LKLVL) / (HS - HE) ! reservoir operating flow (managed)
      ELSEIF (LKLVL.GT.HL) THEN
        Q_OUT = QE    ! established minumum flow for ecological function
      ELSE
        Q_OUT = 0._DP   ! at level too low for any outflow
      ENDIF
    ENDIF
    ! compute cumulative outflux from lake (up to this substep)
    ! NOTE: all lake fluxes were initialized in init_lakes.f90
    QAV= QAV + Q_OUT*MODELSUBTIME/DT
    ! update lake states for substep
    LFLX2 = Q_OUT*MODELSUBTIME
    LKVOL = MAX(LKVOL + LFLX1 - LFLX2, 0._DP)
    IF (DEMANDPS .GT. 0) THEN          ! if there is a demand
      ! check if there is enough water available to extract water for irrigation
      IF (LKVOL.GE. (DEMANDPS*MODELSUBTIME)) THEN
        ! here we can make another statement, e.g. minvol has to be ecological level/volume
        LKVOL = LKVOL - DEMANDPS*MODELSUBTIME
        TAKE = TAKE + DEMANDPS*MODELSUBTIME
      ELSE
        TAKE = TAKE + LKVOL
        ! also this will be different when we make e.g. minvol equal to ecological level/volume
        LKVOL = 0._DP
      ENDIF
    ENDIF
    IF (LAKEFLAG.EQ.1) THEN
      LKLVL = D0 * (LKVOL*M/(A0*D0)) ** (1._DP/M) + (H0-D0)
    ELSE IF (LAKEFLAG.EQ.2) THEN
      LKLVL = (LKVOL * (D + 1._DP) / C) ** (1._DP / (D + 1._DP)) + (H0-D0)
    END IF
    IF (LKVOL.LE.0._DP) EXIT
  END DO  ! (end looping through model sub-steps)
  ! reduce evaporation to keep the water balance correct, i.e. so
  ! LKVOL-LAKEVOL = (LAKE_I + LAKE_P - LAKE_E - QAV)*DELTIME -TAKE, with LKVOL=0, will be valid
  IF (LKVOL.LE.0) LAKFLX(ILAK_O)%LAKE_E = &
    (  LSTATE(ILAK_O)%LAKEVOL - TAKE + &
      (LAKFLX(ILAK_O)%LAKE_I + LAKFLX(ILAK_O)%LAKE_P - QAV)*DT &
    ) / DT
  ! just an annoying check
  IF (LAKFLX(ILAK_O)%LAKE_E.LT.0._DP) THEN
    PRINT *,' *** NOTE:  negative lake evaporation [q_rch_lake.f90]'
    pause
  END IF
  !-------------------------------------------------------------------------------------------
  ! (5) CALCULATE INSTANTANEOUS OUTFLOW AS INPUT FOR THE REACH ROUTING MODEL AND SAVE VARIABLES
  !-------------------------------------------------------------------------------------------
  ! calculate instantanious outflow at the end of a time step (m3 s**-1)
  ! get the instantenous lake outflow based on current lake level
  IF (LPARAM(ILAK_O)%DSCHFUN) THEN
    IF (LKLVL.GT.HL) THEN
      Q_OUT = K*W*(LKLVL-HL)**ALFA    ! natural reservoirs (lakes) without a discharge structure
    ELSE
      Q_OUT = 0._DP  ! at level too low for any outflow
    END IF
  ELSE
    IF (LKLVL.GT.HS) THEN
      Q_OUT = QS + A * (LKLVL - HS) ** B  ! unmanaged flows or flood flows in managed reservoirs
    ELSEIF (LKLVL.GT.HE) THEN
      Q_OUT = QS - (QS - QE) * (HS - LKLVL) / (HS - HE) ! reservoir operating flow (managed)
    ELSEIF (LKLVL.GT.HL) THEN
      Q_OUT = QE    ! established minumum flow for ecological function
    ELSE
      Q_OUT = 0._DP   ! at level too low for any outflow
    ENDIF
  END IF
  ! get the lake area based on current lake level
  H = LKLVL-(H0-D0) ! in units of lake depth
  IF (LAKEFLAG.EQ.1) THEN
    LKARE = A0 *(H/D0)**(M-1._DP)
  ELSE IF (LAKEFLAG.EQ.2) THEN
    LKARE = C * (H ** D)
  END IF
  ! put state variables and diagnostic variables in model structures
  LAKFLX(ILAK_O)%LAKE_Qav = QAV
  LSTATE(ILAK_O)%LAKEVOL = LKVOL
  LSTATE(ILAK_O)%LAKELVL = LKLVL
  LSTATE(ILAK_O)%LAKEARE = LKARE
  LAKFLX(ILAK_O)%LAKE_Q = MAX(1.D-15, Q_OUT ) ! NB: k.w routing algorithm does not like zero flux
  !CALL QROUTE_RCH(IENS,JRCH,RSTEP) ! add lake outflow to outlet reach and route through reach
  q_rch_lake = QROUTE_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,MAXQPAR,RSTEP)
  if(q_rch_lake > 0) return
  !-------------------------------------------------------------------------------------------
  ! (6) CALCULATE ACTUAL TAKE OF USERS
  !-------------------------------------------------------------------------------------------
!  IF (NUSER.GT.0.AND.SIMDAT%UCFFLAG.GE.1) THEN  ! only if the irrigation module is active and nuser>0 (see init_basin.f90)
!    IF (TAKE .LT. 1.D-15) THEN  ! TAKE == 0
!      ! this can probably more efficient, with a mask or so... but as long as there are not
!      !many users, it won't be too bad.
!      DO IUSER=1, NUSER
!        IF (USRPARAM(IUSER)%USER_TAKE_ID.EQ.NETOPO(JRCH)%LAKE_ID .AND. &
!          USRPARAM(IUSER)%USER_TAKE_TYPE.EQ. 3) THEN
!            USERSTATE(IENS,IUSER)%USERTAKE = 0._DP
!        ENDIF
!      ENDDO
!    ELSEIF (ABS(TAKE - DEMAND).LT. 1.D-15) THEN  ! TAKE != 0 and TAKE == DEMAND
!      DO IUSER=1, NUSER
!        IF (USRPARAM(IUSER)%USER_TAKE_ID.EQ.NETOPO(JRCH)%LAKE_ID .AND. &
!          USRPARAM(IUSER)%USER_TAKE_TYPE.EQ. 3) THEN
!            USERSTATE(IENS,IUSER)%USERTAKE = USERSTATE(IENS,IUSER)%USERDEMAND
!        ENDIF
!      ENDDO
!    ELSE ! TAKE != 0  and TAKE != DEMAND
!      DO IUSER=1, NUSER
!        IF (USRPARAM(IUSER)%USER_TAKE_ID.EQ.NETOPO(JRCH)%LAKE_ID .AND. &
!          USRPARAM(IUSER)%USER_TAKE_TYPE.EQ. 3) THEN
!            ! later maybe build in user priority here... --> how to do that?
!            USERSTATE(IENS,IUSER)%USERTAKE = USERSTATE(IENS,IUSER)%USERDEMAND*(TAKE/DEMAND)
!        ENDIF
!      ENDDO
!    ENDIF
!    DO IUSER=1, NUSER
!      IF (USRPARAM(IUSER)%USER_TAKE_ID.EQ.NETOPO(JRCH)%LAKE_ID .AND. &
!        USRPARAM(IUSER)%USER_TAKE_TYPE.EQ. 3) THEN
!          USERSTATE(IENS,IUSER)%USERSTOR = USERSTATE(IENS,IUSER)%USERSTOR + &
!          USERSTATE(IENS,IUSER)%USERTAKE
!      ENDIF
!    ENDDO
!  END IF
 ELSE  ! we are in lake outlet reach and lake parameters do not exist
  IF (NETOPO(JRCH)%DREACHI.GT.0) THEN ! not a catchment outlet reach
    PRINT *,'necessary lake parameters missing for lake with &
            &lake outlet not one of the catchment outlets'
    !CALL EXIT_TOPNET(1,'unable to calculate lake outflow needed to route further &
    !                   &downstream [q_rch_lake.f90]')
       q_rch_lake = 1
       RETURN
  ENDIF
  LAKFLX(ILAK_O)%LAKE_Qav = -9999._DP
  LAKFLX(ILAK_O)%LAKE_Q = 1.D-15 ! avoid zero for the routing routines
  !CALL QROUTE_RCH(IENS,JRCH,RSTEP) ! add lake outflow to outlet reach and route through reach
  q_rch_lake = QROUTE_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,MAXQPAR,RSTEP)
  if(q_rch_lake > 0) return

 END IF ! (lake parameters do exist)
END IF ! (in an outlet reach)
! ----------------------------------------------------------------------------------------
! (7) ADD FLUX FROM INLET REACHES AND UPDATE TOTAL INFLOW
! ----------------------------------------------------------------------------------------
! Get flow from reaches that drain into the lake
IF (INLET) THEN
  ! we are in a lake inlet reach
  IF (OUTLET) THEN
    ! this is also an outlet reach from an upstream lake so add the outlet reach flow
    ! into cumulative lake inflow
    ! NOTE: all lake fluxes were initialized in init_lakes.f90
    LAKFLX(ILAK_I)%LAKE_I = LAKFLX(ILAK_I)%LAKE_I + RCHFLX%REACH_Q
  ELSE
    ! strip out routed particles from the inlet reaches
    !CALL QROUTE_RCH(IENS,JRCH,RSTEP)  ! gets flow from reach
    q_rch_lake = QROUTE_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,MAXQPAR,RSTEP)
    if(q_rch_lake > 0) return

    ! compute cumulative inflow (until this reach) in lake
    ! NOTE: all lake fluxes were initialized in init_lakes.f90
    LAKFLX(ILAK_I)%LAKE_I = LAKFLX(ILAK_I)%LAKE_I + RCHFLX%REACH_Q
  END IF
END IF   ! if reach is an inlet reach
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
Q_RCH_LAKE = 0
END function Q_RCH_LAKE
