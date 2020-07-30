!SUBROUTINE REMOVE_RCH(Q_JRCH,TENTRY,T_EXIT)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Martyn Clark, 2006
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to remove flow particles from the routing structure, to decrease memory usage
!    and processing time
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!  Q_JRCH(:): Vector of merged flow particles in reach JRCH
!  TENTRY(:): Vector of times flow particles entered reach JRCH (exited upstream reaches)
!  T EXIT(:): Vector of times flow particles are EXPECTED to exit reach JRCH
!
! ----------------------------------------------------------------------------------------
FUNCTION REMOVE_RCH(Q_JRCH,TENTRY,T_EXIT,MAXQPAR)
! ----------------------------------------------------------------------------------------
USE nrtype
USE nrutil, ONLY : arth                                 ! Num. Recipies utilities
IMPLICIT NONE
INTEGER(I4B)                                ::REMOVE_RCH
! I/O
REAL(DP), DIMENSION(:), POINTER             :: Q_JRCH   ! merged (non-routed) flow in JRCH
REAL(DP), DIMENSION(:), POINTER             :: TENTRY   ! time flow particles entered JRCH
REAL(DP), DIMENSION(:), POINTER             :: T_EXIT   ! time flow particles exited JRCH
INTEGER(I4B),INTENT(IN)                     :: MAXQPAR
! Local variables
INTEGER(I4B)                                :: NPRT     ! number of flow particles
INTEGER(I4B)                                :: IPRT     ! loop through flow particles
REAL(DP), DIMENSION(:), ALLOCATABLE         :: Q,T,Z    ! copies of Q_JRCH and T_JRCH
LOGICAL(LGT), DIMENSION(:), ALLOCATABLE     :: PARFLG   ! .FALSE. if particle removed
INTEGER(I4B), DIMENSION(:), ALLOCATABLE     :: INDEX0   ! indices of original vectors
REAL(DP), DIMENSION(:), ALLOCATABLE         :: ABSERR   ! absolute error btw interp and orig
REAL(DP)                                    :: Q_INTP   ! interpolated particle
INTEGER(I4B)                                :: MPRT     ! local number of flow particles
INTEGER(I4B), DIMENSION(:), ALLOCATABLE     :: INDEX1   ! indices of particles retained
REAL(DP), DIMENSION(:), ALLOCATABLE         :: E_TEMP   ! temp abs error btw interp and orig
INTEGER(I4B), DIMENSION(1)                  :: ITMP     ! result of minloc function
INTEGER(I4B)                                :: ISEL     ! index of local minimum value
INTEGER(I4B)                                :: INEG     ! lower boundary for interpolation
INTEGER(I4B)                                :: IMID     ! desired point for interpolation
INTEGER(I4B)                                :: IPOS     ! upper boundary for interpolation
INTEGER(I4B)                                :: IERR     ! error code (allocate statements)
! ----------------------------------------------------------------------------------------
! (1) INITIALIZATION
! ----------------------------------------------------------------------------------------
! get the number of particles
NPRT = SIZE(Q_JRCH)-1                       ! -1 because of zero element
! allocate and initialize arrays
ALLOCATE(Q(0:NPRT),T(0:NPRT),Z(0:NPRT),PARFLG(0:NPRT),INDEX0(0:NPRT),ABSERR(0:NPRT),STAT=IERR)
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem allocating array [remove_rch.f90]')
  print *,'problem allocating array [remove_rch.f90]'
  REMOVE_RCH = 1
  RETURN
END IF
Q = Q_JRCH; T = TENTRY     ! get copies of Q_JRCH and TENTRY
Z = T_EXIT                 ! (not used in the interp, but include for consistency)
PARFLG = .TRUE.            ! particle flag = start with all points
INDEX0 = ARTH(0,1,NPRT+1)  ! index = (0,1,2,...,NPRT)
ABSERR = HUGE(Q)           ! largest possible double-precision number
! get the absolte difference between actual points and interpolated points
DO IPRT=1,NPRT-1
 ! interpolate at point (iprt)
 Q_INTP = INTERP(T(IPRT),Q(IPRT-1),Q(IPRT+1),T(IPRT-1),T(IPRT+1))
 ! save the absolute difference between the actual value and the interpolated value
 ABSERR(IPRT) = ABS(Q_INTP-Q(IPRT))
END DO
! ----------------------------------------------------------------------------------------
! (2) REMOVAL
! ----------------------------------------------------------------------------------------
DO  ! continue looping until the number of particles is below the limit
 ! get the number of particles still in the structure
 MPRT = COUNT(PARFLG)-1       ! -1 because of the zero element
 ! get a copy of (1) indices of selected points, and (2) the interpolation errors
 ALLOCATE(INDEX1(0:MPRT),E_TEMP(0:MPRT),STAT=IERR)
 IF (IERR.NE.0) THEN
   !CALL EXIT_TOPNET(1,'problem allocating array [remove_rch.f90]')
   PRINT *, 'problem allocating array [remove_rch.f90]'
   REMOVE_RCH = 1
   RETURN
 END IF
 INDEX1 = PACK(INDEX0,PARFLG) ! (restrict attention to the elements still present)
 E_TEMP = PACK(ABSERR,PARFLG)
 ! check for exit condition (exit after "pack" b.c. indices used to construct final vectors)
 IF (MPRT.LT.MAXQPAR) EXIT
 ! get the index of the minimum value
 ITMP = MINLOC(E_TEMP)
 ISEL = LBOUND(E_TEMP,DIM=1) + ITMP(1) - 1 ! MINLOC assumes count from 1, here (0,1,2,...NPRT)
 ! re-interpolate the point immediately before the point flagged for removal
 IF (INDEX1(ISEL-1).GT.0) THEN
  INEG=INDEX1(ISEL-2); IMID=INDEX1(ISEL-1); IPOS=INDEX1(ISEL+1)
  Q_INTP = INTERP(T(IMID),Q(INEG),Q(IPOS),T(INEG),T(IPOS))
  ABSERR(IMID) = ABS(Q_INTP-Q(IMID))
 ENDIF
 ! re-interpolate the point immediately after the point flagged for removal
 IF (INDEX1(ISEL+1).LT.NPRT) THEN
  INEG=INDEX1(ISEL-1); IMID=INDEX1(ISEL+1); IPOS=INDEX1(ISEL+2)
  Q_INTP = INTERP(T(IMID),Q(INEG),Q(IPOS),T(INEG),T(IPOS))
  ABSERR(IMID) = ABS(Q_INTP-Q(IMID))
 ENDIF
 ! flag the point as "removed"
 PARFLG(INDEX1(ISEL)) = .FALSE.
 ! de-allocate arrays
 DEALLOCATE(INDEX1,E_TEMP,STAT=IERR)
 IF (IERR.NE.0) THEN
   !CALL EXIT_TOPNET(1,'problem deallocating array [remove_rch.f90]')
   PRINT *, 'problem deallocating array [remove_rch.f90]'
   REMOVE_RCH = 1
   RETURN
 END IF
END DO  ! keep looping until a sufficient number of points are removed
! ----------------------------------------------------------------------------------------
! (3) RE-SIZE DATA STRUCTURES
! ----------------------------------------------------------------------------------------
DEALLOCATE(Q_JRCH,TENTRY,T_EXIT,STAT=IERR)
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem deallocating array [remove_rch.f90]')
   PRINT *, 'problem deallocating array [remove_rch.f90]'
   REMOVE_RCH = 1
   RETURN
END IF
ALLOCATE(Q_JRCH(0:MPRT),TENTRY(0:MPRT),T_EXIT(0:MPRT),STAT=IERR)
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem allocating array [remove_rch.f90]')
   PRINT *, 'problem deallocating array [remove_rch.f90]'
   REMOVE_RCH = 1
   RETURN
END IF
Q_JRCH = Q(INDEX1)
TENTRY = T(INDEX1)
T_EXIT = Z(INDEX1)
DEALLOCATE(INDEX1,E_TEMP,STAT=IERR)
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem deallocating array [remove_rch.f90]')
   PRINT *, 'problem deallocating array [remove_rch.f90]'
   REMOVE_RCH = 1
   RETURN
END IF
DEALLOCATE(Q,T,Z,PARFLG,ABSERR,INDEX0,STAT=IERR)
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem deallocating array [remove_rch.f90]')
   PRINT *, 'problem deallocating array [remove_rch.f90]'
   REMOVE_RCH = 1
   RETURN
END IF
REMOVE_RCH = 0
! ----------------------------------------------------------------------------------------
CONTAINS
 FUNCTION INTERP(T0,Q1,Q2,T1,T2)
 REAL(DP),INTENT(IN)                        :: Q1,Q2  ! flow at neighbouring times
 REAL(DP),INTENT(IN)                        :: T1,T2  ! neighbouring times
 REAL(DP),INTENT(IN)                        :: T0     ! desired time
 REAL(DP)                                   :: INTERP ! function name
 !                     dQ/dT              dT
 INTERP = Q1 + ( (Q2-Q1) / (T2-T1) ) * (T0-T1)
 END FUNCTION INTERP
END FUNCTION REMOVE_RCH
