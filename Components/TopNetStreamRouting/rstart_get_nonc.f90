!function RSTART_GET(LAKEFLAG,NRCH,NLAK,DTIME,LEXIST,FEXIST,MAXQPAR,RESTART_FILE)
function RSTART_GET_NONC()
! ----------------------------------------------------------------------------------------
USE nrtype                                               ! variable types (DP, I4B, etc.)
IMPLICIT NONE
INTEGER(I4B)                           :: RSTART_GET_NONC
write(*,*)'this is an void function'
RSTART_GET_NONC = 0
! ----------------------------------------------------------------------------------------
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
! ----------------------------------------------------------------------------------------
END function RSTART_GET_NONC

function ReadRivStates(strStateFile,nMaxPars,DTIME)
!DEC$ ATTRIBUTES DLLEXPORT::ReadStates
use nrtype
!use lakevars
use reachvars
implicit none
INTEGER(I4B)                           :: ReadRivStates
character(len=LEN_PATH), intent(in)    :: strStateFile   !restart file (from topnet)
INTEGER(I4B),intent(in)                :: nMaxPars
REAL(DP), INTENT(IN)                   :: DTIME          ! Time difference between restart file and model reference time
LOGICAL(LGT)              :: LEXIST         ! .TRUE. if re-start file exists
LOGICAL(LGT)              :: FEXIST         ! if .TRUE., only check for file existance
LOGICAL(LGT),DIMENSION(NRCH)              :: TMPFLG         ! if .TRUE., only check for file existance
INTEGER(I4B)                                :: nTmp,numqpar, ipar,irch
REAL(DP),DIMENSION(nMaxPars)           :: q_ratep         ! temporary data array (time)
REAL(DP),DIMENSION(nMaxPars)           :: qm_ratep         ! temporary data array (time)
REAL(DP),DIMENSION(nMaxPars)           :: tentryp         ! temporary data array (time)
REAL(DP),DIMENSION(nMaxPars)           :: t_exitp         ! temporary data array (time)
REAL(DP),DIMENSION(nMaxPars)           :: q_flagp         ! temporary data array (time)
INTEGER(I4B)                           :: ierr 

ReadRivStates = 0
open(95, file = strStateFile)
read(95,*) 
do irch = 1, nrch
    read(95,*)numqpar,(q_ratep(ipar), ipar=1,nMaxPars),(qm_ratep(ipar), ipar=1,nMaxPars), &
        (tentryp(ipar), ipar=1,nMaxPars),(t_exitp(ipar), ipar=1,nMaxPars),(q_flagp(ipar), ipar=1,nMaxPars)
   ! -------------------------------------------------------------------------------------
   ! (3) READ STREAMFLOW PARTICLES
   ! -------------------------------------------------------------------------------------
   ! read the number of streamflow particles
   IF (numqpar.GE.1) THEN
    IF (ASSOCIATED(KROUTE(IRCH)%KWAVE)) THEN
      DEALLOCATE(KROUTE(IRCH)%KWAVE,STAT=IERR)
    END IF
    ALLOCATE(KROUTE(IRCH)%KWAVE(0:numqpar-1),STAT=IERR)

    ! read the flow particles
    KROUTE(IRCH)%KWAVE(0:numqpar-1)%QF = q_ratep(1:numqpar)
    ! read the modified flow particles (if needed)
    KROUTE(IRCH)%KWAVE(0:numqpar-1)%QM = qm_ratep(1:numqpar)
    ! read the entry time -- NOTE: use of DTIME to convert to seconds since model reference time
    KROUTE(IRCH)%KWAVE(0:numqpar-1)%TI = tentryp(1:numqpar) + DTIME
    ! read the exit time -- NOTE: use of DTIME to convert to seconds since model reference time
    KROUTE(IRCH)%KWAVE(0:numqpar-1)%TR = t_exitp(1:numqpar) + DTIME
    ! read the routing flag
    TMPFLG(1:numqpar) = .FALSE.; WHERE(q_flagp(1:numqpar).EQ.1) TMPFLG(1:numqpar)=.TRUE. ! convert to characters
    KROUTE(IRCH)%KWAVE(0:numqpar-1)%RF = TMPFLG(1:numqpar) ! get logical flags
   ENDIF   ! (only read if there are particles)
enddo

close(95)
end function ReadRivStates

function WriteRivStates(strStateFile,nMaxPars)
!DEC$ ATTRIBUTES DLLEXPORT::WriteRivStates
use nrtype
!use lakevars
use reachvars
implicit none
INTEGER(I4B)                           :: WriteRivStates
character(len=LEN_PATH), intent(in)    :: strStateFile   !restart file (from topnet)
INTEGER(I4B),intent(in)                :: nMaxPars
LOGICAL(LGT)              :: LEXIST         ! .TRUE. if re-start file exists
LOGICAL(LGT)              :: FEXIST         ! if .TRUE., only check for file existance
LOGICAL(LGT),DIMENSION(NRCH)              :: TMPFLG         ! if .TRUE., only check for file existance
INTEGER(I4B)                                :: nTmp,numqpar, ipar,irch
!INTEGER(I4B)                           :: ierr 

WriteRivStates = 0
open(95, file = strStateFile)
write(95,*)'irch numqpar q_ratep qm_ratep tentryp t_exitp q_flagp' 
do irch = 1, nrch
    numqpar = minval(KROUTE(IRCH)%KWAVE(0:nMaxPars-1)%QF)
    write(95,*)nTmp,numqpar,KROUTE(IRCH)%KWAVE(0:nMaxPars-1)%QF,KROUTE(IRCH)%KWAVE(0:nMaxPars-1)%QM, &
        KROUTE(IRCH)%KWAVE(0:nMaxPars-1)%TI,KROUTE(IRCH)%KWAVE(0:nMaxPars-1)%TR,KROUTE(IRCH)%KWAVE(0:nMaxPars-1)%RF
!    read(95,*)irch,numqpar,(q_ratep(ipar), ipar=1,nMaxPars),(qm_ratep(ipar), ipar=1,nMaxPars), &
!        (tentryp(ipar), ipar=1,nMaxPars),(t_exitp(ipar), ipar=1,nMaxPars),(q_flagp(ipar), ipar=1,nMaxPars)
   ! -------------------------------------------------------------------------------------
enddo

close(95)
end function WriteRivStates
