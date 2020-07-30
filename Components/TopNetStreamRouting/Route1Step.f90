! to do list:
! 1) test lake module
! 2) multplier to R_MAN_N
! 3) user take
! 4) error code/message
! 5) documentation
module TopRoute
contains

function Initiate(ncSpatial,RESTART_FILE,LAKFLAG,deltat,dtime,MAXPAR,nrcho,nlako)
!!DEC$ ATTRIBUTES DLLEXPORT::Initiate
use nrtype
use lakevars
use reachvars
use interreach, only: SPLDAT_GET, RSTART_GET,init_basin,REACHORDER
implicit none
INTEGER(I4B)                           :: Initiate
character(len=LEN_PATH), intent(in)    :: ncSpatial      !spatial file (network)
character(len=LEN_PATH), intent(in)    :: RESTART_FILE   !restart file (from topnet)
INTEGER(I4B), INTENT(INOUT)            :: LAKFLAG        !type of lake model (0, 1, 2)
REAL(DP),INTENT(IN)                    :: deltat          ! time step (seconds)
REAL(DP),INTENT(IN)                    :: dtime          ! time diff between restart file and reference
INTEGER(I4B), INTENT(IN)               :: MAXPAR        !maximum flow particles
INTEGER(I4B), INTENT(OUT)              :: nrcho         ! number of rches
INTEGER(I4B), INTENT(OUT)              :: nlako         !number of lakes
LOGICAL(LGT)              :: LEXIST         ! .TRUE. if re-start file exists
LOGICAL(LGT)              :: FEXIST         ! if .TRUE., only check for file existance

Initiate = 0
LAKEFLAG = LAKFLAG
DT = deltat
MAXQPAR = MAXPAR
!the lakeflag might be changed in here
Initiate = SPLDAT_GET(1,ncSpatial) 
Initiate = Init_basin()
Initiate = REACHORDER()
if(len_trim(RESTART_FILE) > 0) then
    INQUIRE(FILE=TRIM(RESTART_FILE),EXIST=LEXIST)
    if(LEXIST) Initiate = RSTART_GET(RESTART_FILE,dtime,LEXIST,FEXIST)
    if(Initiate < 0) return
end if
allocate(BASFLX(NRCH),STAT=Initiate)
if(Initiate < 0) return
BASFLX(:)%INSTN_Q(0) = -1.0_DP
BASFLX(:)%INSTN_Q(1) = 0.0_DP
LAKFLAG = LAKEFLAG
nrcho = NRCH
nlako = NLAK
end function Initiate

function Finalization()
!!DEC$ ATTRIBUTES DLLEXPORT::Finalize
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
function Route1Step(TBOUNDS,PRECIP,POTVAP,INSTN_Q,QOUT)
!!DIR$ ATTRIBUTES DLLEXPORT::Route1Step
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
!write(*,*) (Qout(irch), irch = 1, nrch)
end function Route1Step

end module TopRoute