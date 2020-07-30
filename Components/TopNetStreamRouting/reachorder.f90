function REACHORDER()
! ----------------------------------------------------------------------------------------
! Creator(s):
!   David Tarboton, 1997
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Defines the processing order for the individual stream segments in the river network
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   INPUTS:
!    NRCH: Number of stream segments in the river network (reaches)
!
! ----------------------------------------------------------------------------------------
! Structures modified:
!
!   Updates structure RHORDER in module reachparam
!
! ----------------------------------------------------------------------------------------
! Source:
!
!   Subroutine MDDATA within TOPNET version 7
!
! ----------------------------------------------------------------------------------------
! Modifications to source (mp.clark@niwa.co.nz):
!
!   * All variables are now defined (IMPLICIT NONE) and described (comments)
!
! ----------------------------------------------------------------------------------------
! Future revisions:
!
!   (none planned)
!
! ----------------------------------------------------------------------------------------
USE nrtype
!USE ioinfo                                               ! input/output structures
USE reachvars
IMPLICIT NONE
INTEGER(I4B)                           :: REACHORDER  ! 
! input variables
!INTEGER(I4B), INTENT(IN)               :: NRCH            ! number of stream segments
INTEGER(I4B)                           :: IRCH,JRCH,KRCH  ! loop through reaches
INTEGER(I4B)                           :: IUPS            ! loop through upstream reaches
INTEGER(I4B)                           :: IERR            ! error code (allocate statements)
INTEGER(I4B)                           :: ICOUNT          ! counter for the gutters
INTEGER(I4B)                           :: NASSIGN         ! # reaches currently assigned
LOGICAL(LGT),DIMENSION(:),ALLOCATABLE  :: RCHFLAG         ! TRUE if reach is processed
INTEGER(I4B)                           :: NUPS            ! number of upstream reaches
INTEGER(I4B)                           :: UINDEX          ! upstream reach index
!IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'reachorder'
! ----------------------------------------------------------------------------------------
REACHORDER  = 0;

NASSIGN = 0
ALLOCATE(RCHFLAG(NRCH),STAT=IERR)
IF (IERR.NE.0) then
!  CALL EXIT_TOPNET(1,'problem allocating array [reachorder.f90]')
    REACHORDER  = -1
    return
end if
RCHFLAG(1:NRCH) = .FALSE.
! ----------------------------------------------------------------------------------------
ICOUNT=0
DO  ! do until all reaches are assigned
 NASSIGN = 0
 DO IRCH=1,NRCH
  ! check if the reach is assigned yet
  IF(RCHFLAG(IRCH)) THEN
   NASSIGN = NASSIGN + 1
   CYCLE
  ENDIF
  ! climb upstream as far as possible
  JRCH = IRCH    ! the first reach under investigation
  DO  ! do until get to a "most upstream" reach that is not assigned
   NUPS = SIZE(NETOPO(JRCH)%UREACHI)    ! get number of upstream reaches
   IF (NUPS.GE.1) THEN     ! (if NUPS = 0, then it is a first-order basin)
    KRCH = JRCH   ! the reach under investigation
    ! loop through upstream reaches
    DO IUPS=1,NUPS
     UINDEX = NETOPO(JRCH)%UREACHI(IUPS)  ! POSITION of the upstream reach
     ! check if the reach is NOT assigned
     IF (.NOT.RCHFLAG(UINDEX)) THEN
      JRCH = UINDEX
      EXIT    ! exit IUPS loop
     END IF  ! if the reach is assigned
    END DO  ! (looping through upstream reaches)
    ! check if all upstream reaches are already assigned (only if KRCH=JRCH)
    IF (JRCH.EQ.KRCH) THEN
     ! assign JRCH
     ICOUNT=ICOUNT+1
     RCHFLAG(JRCH) = .TRUE.
     NETOPO(ICOUNT)%RHORDER = JRCH
     EXIT
    ENDIF
    CYCLE   ! if jrch changes, keep looping (move upstream)
   ELSE    ! if the reach is a first-order basin
    ! assign JRCH
    ICOUNT=ICOUNT+1
    RCHFLAG(JRCH) = .TRUE.
    NETOPO(ICOUNT)%RHORDER = JRCH
    EXIT
   ENDIF
  END DO   !  climbing upstream (do-forever)
 END DO   ! looping through reaches
 IF (NASSIGN.EQ.NRCH) EXIT
END DO  ! do forever (do until all reaches are assigned)
DEALLOCATE(RCHFLAG,STAT=IERR)
IF (IERR.NE.0) &
    REACHORDER  = -1
!  CALL EXIT_TOPNET(1,'problem deallocating array [reachorder.f90]')
! ----------------------------------------------------------------------------------------
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
! ----------------------------------------------------------------------------------------
END function REACHORDER
