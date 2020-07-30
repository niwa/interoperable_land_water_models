! to do list:
! 1) test lake module
! 2) multplier to R_MAN_N
! 3) user take
! 4) error code/message
! 5) documentation
module TopRoute
contains

function Initialization(strRivFile,strLakFile,strStateFile,nLakMod,deltat,dtime,MAXPAR,nMaxUStr,nrcho,nlako)
!DEC$ ATTRIBUTES DLLEXPORT::Initialization
!use logging
use nrtype
use lakevars
use reachvars
use interreach, only: SPLDAT_GET_NONC, ReadRivStates,init_basin,REACHORDER
implicit none
INTEGER(I4B)                           :: Initialization
character(len=LEN_PATH), intent(in)    :: strRivFile      !river file (network)
character(len=LEN_PATH), intent(in)    :: strLakFile      !lake file (network)
character(len=LEN_PATH), intent(in)    :: strStateFile   !restart file (from topnet)
INTEGER(I4B), INTENT(INOUT)            :: nLakMod        !type of lake model (0, 1, 2)
REAL(DP),INTENT(IN)                    :: deltat          ! time step (seconds)
REAL(DP),INTENT(IN)                    :: dtime          ! time diff between restart file and reference
INTEGER(I4B), INTENT(IN)               :: MAXPAR        !maximum flow particles
INTEGER(I4B), INTENT(IN)               :: nMaxUStr           !number of maximum upstream reaches
INTEGER(I4B), INTENT(OUT)              :: nrcho         ! number of rches
INTEGER(I4B), INTENT(OUT)              :: nlako         !number of lakes
LOGICAL(LGT)              :: LEXIST         ! .TRUE. if re-start file exists
LOGICAL(LGT)              :: FEXIST         ! if .TRUE., only check for file existance

Initialization = 0
LAKEFLAG = nLakMod
DT = deltat
MAXQPAR = MAXPAR
!the lakeflag might be changed in here
Initialization = SPLDAT_GET_nonc(strRivFile,strLakFile,nMaxUStr) 
Initialization = Init_basin()
Initialization = REACHORDER()
if(len_trim(strStateFile) > 0) then
    INQUIRE(FILE=TRIM(strStateFile),EXIST=LEXIST)
    !if(LEXIST) Initiate = RSTART_GET_nonc(RESTART_FILE,dtime,LEXIST,FEXIST)
    if(LEXIST) Initialization = ReadRivStates(strStateFile,MAXPAR,DTIME) !RSTART_GET_nonc()
    if(Initialization < 0) return
end if
allocate(BASFLX(NRCH),STAT=Initialization)
if(Initialization < 0) return
BASFLX(:)%INSTN_Q(0) = -1.0_DP
BASFLX(:)%INSTN_Q(1) = 0.0_DP
nLakMod = LAKEFLAG
nrcho = NRCH
nlako = NLAK
end function Initialization

function Finalization()
!DEC$ ATTRIBUTES DLLEXPORT::Finalize
use nrtype
use lakevars
use reachvars
implicit none
INTEGER(I4B)                           :: Finalization
INTEGER(I4B)                           :: IRCH
Finalization = 0
 IF(ASSOCIATED(NETOPO)) then
     deallocate(NETOPO,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
 IF(ASSOCIATED(RPARAM)) then
    deallocate(RPARAM,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
 IF(ASSOCIATED(LKTOPO)) then
    deallocate(LKTOPO,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
 IF(ASSOCIATED(LPARAM)) then
    deallocate(LPARAM,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
do irch = 1, nrch 
    IF(ASSOCIATED(KROUTE(IRCH)%KWAVE)) then
        DEALLOCATE(KROUTE(IRCH)%KWAVE,STAT=Finalization)
        IF (Finalization.NE.0) return
    end if
end do 
 IF(ASSOCIATED(BASFLX)) then
    deallocate(BASFLX,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
 IF(ASSOCIATED(RCHFLX)) then
    deallocate(RCHFLX,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
 IF(ASSOCIATED(LSTATE)) then
    deallocate(LSTATE,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if
 IF(ASSOCIATED(LAKFLX)) then
    deallocate(LAKFLX,STAT=Finalization)      ! in MODULE reachparam
     IF(Finalization.NE.0) return
 end if

end function Finalization

!function Route1Step(DT,TBOUNDS,PRECIP,POTVAP,INSTN_Q,QOUT,RSTEP)
function Route1Step(TBOUNDS,PRECIP,POTVAP,INSTN_Q,QOUT,VOUT)
!DIR$ ATTRIBUTES DLLEXPORT::Route1Step
use reachvars
use lakevars
use interlake
use interreach
IMPLICIT NONE
INTEGER(I4B)                                :: Route1Step
REAL(DP),DIMENSION(0:1),INTENT(IN)          :: TBOUNDS     ! time bounds (start + end of timestep)
REAL(DP),DIMENSION(NRCH),INTENT(IN)         :: PRECIP     ! precipitation for lake
REAL(DP),DIMENSION(NRCH),INTENT(IN)         :: POTVAP     ! potential evaporation for lake
REAL(DP),DIMENSION(NRCH),INTENT(IN)         :: INSTN_Q    ! instant flow from each basin
REAL(DP),DIMENSION(NRCH),INTENT(out)        :: QOUT
REAL(DP),DIMENSION(NRCH),INTENT(out)        :: VOUT
!REAL(DP),INTENT(IN)          :: TBOUNDS(0:1)     ! time bounds (start + end of timestep)
!REAL(DP),INTENT(IN)         :: PRECIP(NRCH)     ! precipitation for lake
!REAL(DP),INTENT(IN)         :: POTVAP(NRCH)     ! potential evaporation for lake
!REAL(DP),INTENT(IN)         :: INSTN_Q(NRCH)    ! instant flow from each basin
!REAL(DP),INTENT(out)        :: QOUT(NRCH)
INTEGER(I4B)                    :: RSTEP = 0       ! retrospective time step offset
!INTEGER(I4B), INTENT(IN), OPTIONAL          :: RSTEP       ! retrospective time step offset
INTEGER(I4B)                                :: IRCH,JRCH

Route1Step = 0
if(lakeflag) then 
    BASFLX(:)%POTEVAP = POTVAP
    Route1Step = INIT_LAKES(NLAK,lakflx)
end if
BASFLX(:)%INSTN_Q(1) = INSTN_Q
if(any(BASFLX(:)%INSTN_Q(0) < 0))BASFLX(:)%INSTN_Q(0) = BASFLX(:)%INSTN_Q(1) 

DO IRCH=1,NRCH
   ! Identify the reach to process
   JRCH = NETOPO(IRCH)%RHORDER           ! RHORDER is passed through the MODULE reachparam
    IF ((LAKEFLAG.GT.0).AND.(NETOPO(JRCH)%LAKE_IX.GT.0)) THEN ! we are in a lake
        Route1Step = Q_RCH_LAKE(DT,TBOUNDS, &
                        JRCH,NRCH,NETOPO,KROUTE,RPARAM, &
                        BASFLX,RCHFLX(JRCH), &
                        LAKEFLAG,NLAK,LKTOPO,PRECIP(JRCH), &
                        LPARAM,LSTATE,LAKFLX,MAXQPAR,RSTEP)
    ELSE
    ! route streamflow through the river network
        Route1Step = QROUTE_RCH(DT,TBOUNDS, &
                        JRCH,NRCH,NETOPO,KROUTE,RPARAM, &
                        BASFLX,RCHFLX(JRCH), &
                        LAKEFLAG,NLAK,LKTOPO,LAKFLX,MAXQPAR,RSTEP)
    ENDIF
end do
BASFLX(:)%INSTN_Q(1) = BASFLX(:)%INSTN_Q(0)
QOUT(:)= RCHFLX(:)%REACH_Q

!calculate the flow velocity based on manning equation

Route1Step = GetVelfromQ(NRCH,RPARAM,QOUT,VOUT)
!write(*,*) (Qout(irch), irch = 1, nrch)
end function Route1Step

!function SaveStates(strRestartFile)
!!DEC$ ATTRIBUTES DLLEXPORT::SaveStates
!use nrtype
!use lakevars
!use reachvars
!implicit none
!INTEGER(I4B)                           :: SaveStates
!character(len=LEN_PATH), intent(in)    :: strRestartFile   !restart file (from topnet)
!LOGICAL(LGT)              :: LEXIST         ! .TRUE. if re-start file exists
!LOGICAL(LGT)              :: FEXIST         ! if .TRUE., only check for file existance
!
!SaveStates = 0
!LAKEFLAG = LAKFLAG
!DT = deltat
!MAXQPAR = MAXPAR
!open(95, file = strRestartFile)
!write(95,*) 'rch,qi_hist1,'
!close(95)
!end function SaveStates

function GetVelfromQ(NRCH,RPARAM,QOUT,VOUT)

!USE nrtype
USE reachtype
IMPLICIT NONE
INTEGER(I4B)                                :: GetVelfromQ
INTEGER(I4B),INTENT(in)                     :: NRCH
TYPE(RCHPRP),DIMENSION(NRCH),INTENT(IN)     :: RPARAM
REAL(DP),DIMENSION(NRCH),INTENT(in)         :: QOUT
REAL(DP),DIMENSION(NRCH),INTENT(out)        :: VOUT
REAL(DP),DIMENSION(NRCH)        :: K,H
REAL(DP) :: ALFA
GetVelfromQ = 0
ALFA = 5D0/3D0        ! should this be initialized here or in a parameter file?
K    = SQRT(RPARAM(:)%R_SLOPE)/RPARAM(:)%R_MAN_N
H = (QOUT/K)**(1./ALFA)
VOUT = (1./RPARAM(:)%R_MAN_N)* H**(2/3)*sqrt(RPARAM(:)%R_SLOPE)
end function GetVelfromQ

end module TopRoute