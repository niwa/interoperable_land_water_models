!SUBROUTINE INIT_BASIN(NENS,NRCH,MRCH,NLAK,NHYD,NLIN,NLST,NLUT,NSNW)
!function INIT_BASIN(LAKEFLAG,NRCH,NLAK)
function INIT_BASIN()
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Martyn Clark, 2006
!   Lakes added 2006 (David Rupp)
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to allocate space for state and flux arrays and read initial basin states from
!    the model parameter file
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   INPUT:
!      NENS: Number of ensemble members
!      NRCH: Number of stream segments in the river network (reaches)
!      MRCH: Number of basins selected for output (ts_out_put)
!      NLAK: Number of lakes and reservoirs
!      NHYD: Number of streamflow stations
!      NLIN: Number of lake inflow stations
!      NLST: Number of lake stage stations
!      NLUT: Number of lake outflow stations
!      NSNW: Number of snow sites
!
! ----------------------------------------------------------------------------------------
! Structures Populated:
!
!   BINPUT(NRCH)               = basin input  (e.g., precipitation, temperature)
!   BSTATE(NENS,NRCH)% [state] = basin states (e.g., soil moisture, depth to water table)
!   BASFLX(NENS,NRCH)% [flux]  = basin fluxes (e.g., evaporation, drainage, baseflow, etc.)
!   KROUTE(NENS,NRCH)% [state] = reach states (particles in the river network)
!   RCHFLX(NENS,NRCH)% [flux]  = reach fluxes (streamflow)
!   SSTATS(NENS,NRCH)% [stat]  = summary statistics
!   LSTATE(NENS,NLAK)% [state] = lake states (e.g., level, area, volume)
!   LAKFLX(NENS,NLAK)% [flux]  = lake discharge
!
! ----------------------------------------------------------------------------------------
! Future revisions:
!
!   (none planned)
!
! ----------------------------------------------------------------------------------------
USE nrtype                                                ! variable types (DP, I4B, etc.)
!USE nrutil, ONLY : arth                                   ! Numerical Recipes utilities
!USE ioinfo                                                ! information on input/output files
!USE constants, ONLY : tfreeze                             ! fixed parameters
!USE model_time                                            ! model time structures
!USE basinforce                                            ! sub-basin forcing data
!USE basinparam                                            ! sub-basin parameters
!USE basinstate                                            ! sub-basin states
!USE basin_flux                                            ! sub-basin fluxes
USE reachvars                                            ! reach states (flow particles)
!USE reach_flux                                            ! reach fluxes (streamflow)
!USE gridweight                                            ! grid correlation structure
!USE ret_moddat                                            ! retrospective input data
!USE ret_modsta                                            ! retrospective model states
!USE modsummary                                            ! summary statistics
USE lakevars                                            ! lake parameters
!USE lake_state                                            ! lake states
!USE lakes_flux                                            ! lake fluxes (lake discharge)
!USE assimparam                                            ! data assimilation parameters
!USE error_types                                           ! model error measures
!USE simul_info                                            ! simulation information
!USE inputdat2d, ONLY: sfmeta,lkinmeta,lkstmeta, &         ! streamflow data structures
!                      lkutmeta,snwmeta
!USE user_state
!USE userparam
IMPLICIT NONE
INTEGER(I4B)                           :: init_basin            ! 
! Input variables
!INTEGER(I4B), INTENT(IN)               :: NENS            ! Number of ensemble members
!INTEGER(I4B), INTENT(IN)               :: LAKEFLAG            ! lake model
!INTEGER(I4B), INTENT(IN)               :: NRCH            ! number of reaches
!INTEGER(I4B), INTENT(IN)               :: MRCH            ! number of selected sub-basins
!INTEGER(I4B), INTENT(IN)               :: NLAK            ! number of lakes
!INTEGER(I4B), INTENT(IN)               :: NHYD            ! number of streamflow stations
!INTEGER(I4B), INTENT(IN)               :: NLIN            ! Number of lake inflow stations
!INTEGER(I4B), INTENT(IN)               :: NLST            ! Number of lake level stations
!INTEGER(I4B), INTENT(IN)               :: NLUT            ! Number of lake outflow stations
!INTEGER(I4B), INTENT(IN)               :: NSNW            ! number of streamflow stations
! Looping variables
!INTEGER(I4B)                           :: IENS            ! loop thru ensembles
!INTEGER(I4B)                           :: IRET            ! loop thru retrospective time steps
INTEGER(I4B)                           :: IRCH            ! loop thru reaches
INTEGER(I4B)                           :: ILAK            ! loop thru lakes
!INTEGER(I4B)                           :: IDAT            ! loop thru saved data time steps
!INTEGER(I4B)                           :: ISTA            ! loop thru saved model state time steps
!INTEGER(I4B)                           :: IHYD            ! loop thru hydro stations
! Local variables
INTEGER(I4B)                           :: IERR            ! error code for allocate/deallocate statements
!INTEGER(I4B)                           :: NTIMDEL         ! # elements in the time delay histogram
!INTEGER(I4B),DIMENSION(NRCH)           :: NBAND           ! number of elevation bands for sub-basins
!INTEGER(I4B)                           :: NVHYD           ! number of valid streamflow stations
!INTEGER(I4B)                           :: NVLIN           ! number of valid lake inflow stations
!INTEGER(I4B)                           :: NVLST           ! number of valid lake stage stations
!INTEGER(I4B)                           :: NVLUT           ! number of valid lake outflow stations
!INTEGER(I4B)                           :: NVSNW           ! number of valid snow site stations
LOGICAL(LGT),SAVE                      :: INIT=.TRUE.     ! true for first call to init_basin
!include 'netcdf.inc'
INIT_BASIN = 0
!IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'init_basin'
! ---------------------------------------------------------------------------------------
! get the number of valid stream flow stations (within the modelled region)
!IF (NHYD.GT.0) THEN
!  NVHYD = COUNT(SFMETA(:)%RCH_IDV.GT.0)
!ELSE
!  NVHYD = 0
!END IF
! get the number of valid lake inflow stations (within the modelled region)
!IF (NLIN.GT.0) THEN
!  NVLIN = COUNT(LKINMETA(:)%LAKE_IV.GT.0)
!ELSE
!  NVLIN = 0
!END IF
!! get the number of valid lake stage stations (within the modelled region)
!IF (NLST.GT.0) THEN
!  NVLST = COUNT(LKSTMETA(:)%LAKE_IV.GT.0)
!ELSE
!  NVLST = 0
!END IF
! get the number of valid lake outflow stations (within the modelled region)
!IF (NLUT.GT.0) THEN
!  NVLUT = COUNT(LKUTMETA(:)%LAKE_IV.GT.0)
!ELSE
!  NVLUT = 0
!END IF
! get the number of valid snow site stations (within the modelled region)
!IF (NSNW.GT.0) THEN
!  NVSNW = COUNT(SNWMETA(:)%RCH_IDV.GT.0)
!ELSE
!  NVSNW = 0
!END IF
! ---------------------------------------------------------------------------------------
! (1) INITIALIZE ARRAYS
! ---------------------------------------------------------------------------------------
! deallocate the basin input
! ---------------------------------------------------------------------------------------
! deallocate the basin states
! ---------------------------------------------------------------------------------------
! deallocate the basin fluxes
! ---------------------------------------------------------------------------------------
! deallocate the reach fluxes
IF (INIT) THEN
 NULLIFY(RCHFLX)
ELSE
 IF (ASSOCIATED(RCHFLX)) THEN
  DEALLOCATE(RCHFLX,STAT=IERR)
  IF (IERR.NE.0) then
    !CALL EXIT_TOPNET(1,'problem deallocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
  end if
 END IF
END IF
! ---------------------------------------------------------------------------------------
! deallocate the lake fluxes
IF (INIT) THEN
 NULLIFY(LAKFLX)
ELSE
 IF (ASSOCIATED(LAKFLX)) THEN
  DEALLOCATE(LAKFLX,STAT=IERR)
  IF (IERR.NE.0) then
!    CALL EXIT_TOPNET(1,'problem deallocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
  end if
 END IF
END IF
! ---------------------------------------------------------------------------------------
! deallocate the routing structure
IF (INIT) THEN
 NULLIFY(KROUTE)
ELSE
 IF (ASSOCIATED(KROUTE)) THEN
!  DO IENS=1,NENS
   DO IRCH=1,NRCH
    IF (ASSOCIATED(KROUTE(IRCH)%KWAVE)) THEN
      DEALLOCATE(KROUTE(IRCH)%KWAVE,STAT=IERR)
      IF (IERR.NE.0) then
!        CALL EXIT_TOPNET(1,'problem deallocating array [init_basin.f90]')
        INIT_BASIN = -1
        return
      end if
    END IF
   END DO  ! ibas
!  END DO  ! iens
  DEALLOCATE(KROUTE,STAT=IERR)
  IF (IERR.NE.0) then
!    CALL EXIT_TOPNET(1,'problem deallocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
  end if
 ENDIF ! if routing structure allocated
ENDIF
! ---------------------------------------------------------------------------------------
! deallocate the SSTATS structure
! ---------------------------------------------------------------------------------------
! deallocate soil moisture probability distributions
! ---------------------------------------------------------------------------------------
! deallocate soil moisture ordinates
! ---------------------------------------------------------------------------------------
! deallocate runoff probability distributions
! ---------------------------------------------------------------------------------------
! deallocate streamflow probability distributions
! ---------------------------------------------------------------------------------------
! deallocate flow ordinates
! ---------------------------------------------------------------------------------------
! deallocate user states
!IF (INIT) THEN
! NULLIFY(USERSTATE)
!ELSE
! IF (ASSOCIATED(USERSTATE)) THEN
!  DEALLOCATE(USERSTATE,STAT=IERR)
!  IF (IERR.NE.0) &
!    CALL EXIT_TOPNET(1,'problem deallocating array [init_basin.f90]')
! END IF
!END IF
! ---------------------------------------------------------------------------------------
! deallocate the lake states
IF (INIT) THEN
 NULLIFY(LSTATE)
ELSE
 IF (ASSOCIATED(LSTATE)) THEN
  DEALLOCATE(LSTATE,STAT=IERR)
  IF (IERR.NE.0) then
!    CALL EXIT_TOPNET(1,'problem deallocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
  end if
 END IF
END IF
! ---------------------------------------------------------------------------------------
! deallocate the decorrelation time structures
! ---------------------------------------------------------------------------------------
! deallocate the correlated random numbers and innovations used for data assimilation
! ---------------------------------------------------------------------------------------
! deallocate the model error array
! ---------------------------------------------------------------------------------------
! deallocate the retrospective data structure
! ---------------------------------------------------------------------------------------
! allocate arrays
ALLOCATE(RCHFLX(NRCH),STAT=IERR)            ! in MODULE reach_flux
IF (IERR.NE.0) then
!  CALL EXIT_TOPNET(1,'problem allocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
end if
ALLOCATE(KROUTE(NRCH),STAT=IERR)            ! in MODULE reachstate
IF (IERR.NE.0) then
!  CALL EXIT_TOPNET(1,'problem allocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
end if
!DO IENS=1,NENS
 DO IRCH=1,NRCH
  ! the kwave part of this structure is allocated/deallocated in
  ! routing routines because its size varies with timestep
  !NULLIFY(KROUTE(IENS,IRCH)%KWAVE)
  NULLIFY(KROUTE(IRCH)%KWAVE)
 END DO
!END DO
IF (NLAK.GT.0) THEN
 !ALLOCATE(LSTATE(NENS,NLAK),STAT=IERR)           ! in MODULE lake_state
 ALLOCATE(LSTATE(NLAK),STAT=IERR)           ! in MODULE lake_state
 IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
 end if
 !ALLOCATE(LAKFLX(NENS,NLAK),STAT=IERR)           ! in MODULE lakes_flux
 ALLOCATE(LAKFLX(NLAK),STAT=IERR)           ! in MODULE lakes_flux
 IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [init_basin.f90]')
    INIT_BASIN = -1
    return
 end if
ENDIF
!IF (NUSER.GT.0.AND.SIMDAT%UCFFLAG.GE.1) THEN
!  ALLOCATE(USERSTATE(NENS,NUSER),STAT=IERR)
!  IF (IERR.NE.0) &
!    CALL EXIT_TOPNET(1,'problem allocating array [init_basin.f90]')
!ENDIF
! ---------------------------------------------------------------------------------------
! number of elevation bands for sub-basins
! allocate the retrospective states
! ---------------------------------------------------------------------------------------
! preliminary initialization of data structures
! ----------------------------------------------------------------------------------------
! (2) INITIALIZE SUMMARY STATISTICS
! ----------------------------------------------------------------------------------------
! time counter (NUMSTAT is shared in module modsummary)
!MODTIM%SUMTIME      = 0.0_DP         ! total time of simulation (s)
!NUMSTAT             = 0               ! number of time steps included in stats calculations
! ----------------------------------------------------------------------------------------
! (3) INITIALIZE BASIN STATES
! ----------------------------------------------------------------------------------------
! If topmodn > 0, reset initial water table depth (nm) to slightly less than aquifer depth (nm)
! initialize the time-delay histogram
! ----------------------------------------------------------------------------------------
! (5) INITIALIZE LAKE STATES
! ----------------------------------------------------------------------------------------
IF (NLAK.GE.1) THEN
! DO IENS=1,NENS
   DO ILAK=1,NLAK
     IF (LPARAM(ILAK)%EXIST.AND.LKTOPO(ILAK)%LAKE_ID.GT.0) THEN
       IF (LAKEFLAG.EQ.1) THEN  ! Nilsson model
         LSTATE(ILAK)%LAKELVL = LPARAM(ILAK)%ELEVREF                        ! lake level
       ELSE IF (LAKEFLAG.EQ.2) THEN ! Rupp model
         LSTATE(ILAK)%LAKELVL = (LPARAM(ILAK)%AREAREF / LPARAM(ILAK)%HE2AR_C) &
                            ** (1._DP / LPARAM(ILAK)%HE2AR_D) &
                            + (LPARAM(ILAK)%ELEVREF-LPARAM(ILAK)%DEPHREF)        ! lake level
       END IF
     ELSE
       LSTATE(ILAK)%LAKELVL = -9999._DP
     ENDIF
   ENDDO
   WHERE(LPARAM(:)%EXIST.AND.LKTOPO(:)%LAKE_ID.GT.0)
     LSTATE(:)%LAKEARE = LPARAM(:)%AREAREF                                    ! lake area
   ELSEWHERE
     LSTATE(:)%LAKEARE = -9999._DP
   END WHERE
   IF (LAKEFLAG.EQ.1) THEN ! Nilsson model
     WHERE(LPARAM(:)%EXIST.AND.LKTOPO(:)%LAKE_ID.GT.0)
       LSTATE(:)%LAKEVOL = LPARAM(:)%AREAREF * LPARAM(:)%DEPHREF / &
                                LPARAM(:)%SHAPE_M                                ! lake volume
     ELSEWHERE
       LSTATE(:)%LAKEVOL = -9999._DP
     END WHERE
   ELSE IF (LAKEFLAG.EQ.2) THEN ! Rupp model
     WHERE(LPARAM(:)%EXIST.AND.LKTOPO(:)%LAKE_ID.GT.0)
       LSTATE(:)%LAKEVOL = (LPARAM(:)%HE2AR_C / (LPARAM(:)%HE2AR_D + 1._DP)) * &
                                 (LSTATE(:)%LAKELVL - (LPARAM(:)%ELEVREF-LPARAM(:)%DEPHREF)) &
                                  ** ( LPARAM(:)%HE2AR_D + 1._DP) ! lake volume
     ELSEWHERE
       LSTATE(:)%LAKEVOL = -9999._DP
     END WHERE
   END IF
   LAKFLX(:)%LAKE_I = 0.0_DP  ! lake inflows
   LAKFLX(:)%LAKE_P = 0.0_DP  ! lake precipitation
   LAKFLX(:)%LAKE_E = 0.0_DP  ! lake evaporation
   LAKFLX(:)%LAKE_Q = 0.0_DP  ! lake discharge
   LAKFLX(:)%LAKE_Qav = 0.0_DP  ! lake discharge
! END DO
ENDIF
! ------------------------------------------------------------------------------------
! INITIALIZE USER STATES
! ------------------------------------------------------------------------------------
!IF  (NUSER.GT.0.AND.SIMDAT%UCFFLAG.GE.1) THEN
!  USERSTATE(:,:)%USERDEMAND = 0
!  USERSTATE(:,:)%USERTAKE = 0
!  USERSTATE(:,:)%USERRETURN = 0
!ENDIF
! ----------------------------------------------------------------------------------------
! (6) INITIALIZE CORRELATED RANDOM NUMBERS (FOR DATA ASSIMILATION)
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! Ambroise (1996) give baseflow recession curves as a function of time as
!   Qb = Qs exp(-tau Q0/Am)
! where Qb is baseflow, Qs is baseflow at a specified time, tau is the time between
! Qb and Qs, Q0 is the baseflow at saturation (i.e., when zbar = 0), A is area, and
! m is the depth of the aquifer (i.e., 1./f).
!
! Given Q0 = A k0 m exp(-lambda), and rearranging, tau can be calculated directly
! from the ratio of baseflow to specified discharge
!   ln(Qb/Qs) = -tau k0 exp(-lambda)
! such that
!   tau = -ln(Qb/Qs) / (k0 exp(-lambda))
! where lambda is the mean value of the naural log of the topographic index ln[a/tan(b)]
! ----------------------------------------------------------------------------------------
! set the init variable to false
IF (INIT) INIT= .FALSE.
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
END function INIT_BASIN
