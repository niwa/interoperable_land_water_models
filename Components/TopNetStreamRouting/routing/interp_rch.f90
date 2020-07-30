!SUBROUTINE INTERP_RCH(TOLD,QOLD,TNEW,QNEW,IERR)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Unknown (original Tideda routine?), fairly old
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to calculate time step averages from a set of irregularly spaced values
!    (provides one less output data value -- average BETWEEN time steps)
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   Input(s):
!   ---------
!     TOLD: vector of times
!     QOLD: vector of flows
!     TNEW: vector of desired output times
!
!   Outputs:
!   --------
!     QNEW: vector of interpolated output [dimension = TNEW-1 (average BETWEEN intervals)]
!     IERR: error code (1 = bad bounds)
!
! ----------------------------------------------------------------------------------------
! Structures Used:
!
!   NONE
!
! ----------------------------------------------------------------------------------------
! Method:
!
!   Loop through all output times (can be just 2, start and end of the time step)
!
!   For each pair of output times, integrate over the irregularly-spaced data
!   values.
!
!   First, estimate the area under the curve at the start and end of the time step.
!   Identify index values IBEG and IEND that span the start and end of the time
!   step: TOLD(IBEG-1) < T0 <= TOLD(IBEG), and TOLD(IEND-1) < T1 <= TOLD(IEND).
!   Use linear interpolation to estimate the value at T0 (QEST0) and T1 (QEST1).
!   The area under the curve at the start/end of the time step is then simply
!   (TOLD(IBEG) - T0) and (T1 - TOLD(IEND-1)), and the corresponding data values
!   are 0.5*(QEST0 + QOLD(IBEG)) and 0.5*(QOLD(IEND-1) + QEST1).
!
!   Second, integrate over the remaining datapoints between T0 and T1.  The area
!   for the middle points is (TOLD(i) - TOLD(i-1)), and the corresponding data
!   values are (QOLD(i) - QOLD(i-1)).
!
!   Finally, divide the sum of the data values * area by the sum of the areas
!   (which in this case is simply T1-T0).
!
!   A special case occurs when both the start and the end points of the time step
!   lie between two of the irregularly-spaced input data values.  Here, use linear
!   interpolation to estimate values at T0 and T1, and simply average the
!   interpolated values.
!
!   NB: values of TOLD with times <= T0 and >= T1 are required for reliable
!   estimates of time step averages.
!
! ----------------------------------------------------------------------------------------
! Source:
!
!   This routine is essentially a direct copy of the subroutine interp, located in
!   the file kinwav_v7.f
!
! ----------------------------------------------------------------------------------------
! Modifications to source (mp.clark@niwa.co.nz):
!
!   * All variables are now defined (IMPLICIT NONE) and described (comments)
!
!   * Added extra comments
!
!   * Replaced GOTO statements with DO loops and IF statements
!
! ----------------------------------------------------------------------------------------
FUNCTION INTERP_RCH(TOLD,QOLD,TNEW,QNEW,IERR)
! --------------------------------------------------------------------------------------------
USE nrtype
IMPLICIT NONE
INTEGER(I4B)                                :: INTERP_RCH
! Input
REAL(DP), DIMENSION(:), INTENT(IN)          :: TOLD     ! input time array
REAL(DP), DIMENSION(:), INTENT(IN)          :: QOLD     ! input flow array
REAL(DP), DIMENSION(:), INTENT(IN)          :: TNEW     ! desired output times
! Output
REAL(DP), DIMENSION(:), INTENT(OUT)         :: QNEW     ! flow averaged for desired times
INTEGER(I4B), INTENT(OUT)                   :: IERR     ! error, 1= bad bounds
! Internal
INTEGER(I4B)                                :: NOLD     ! number of elements in input array
INTEGER(I4B)                                :: NNEW     ! number of desired new times
INTEGER(I4B)                                :: IOLDLOOP ! loop through input times
INTEGER(I4B)                                :: INEWLOOP ! loop through desired times
REAL(DP)                                    :: T0,T1    ! time at start/end of the time step
INTEGER(I4B)                                :: IBEG     ! identify input times spanning T0
INTEGER(I4B)                                :: IEND     ! identify input times spanning T1
INTEGER(I4B)                                :: IMID     ! input times in middle of the curve
REAL(DP)                                    :: AREAB    ! area at the start of the time step
REAL(DP)                                    :: AREAE    ! area at the end of the time step
REAL(DP)                                    :: AREAM    ! area at the middle of the time step
REAL(DP)                                    :: AREAS    ! sum of all areas
REAL(DP)                                    :: SLOPE    ! slope between two input data values
REAL(DP)                                    :: QEST0    ! flow estimate at point T0
REAL(DP)                                    :: QEST1    ! flow estimate at point T1
! --------------------------------------------------------------------------------------------
! get array size
NOLD = SIZE(TOLD); NNEW = SIZE(TNEW)
!
IERR=0
! check that the input time series starts before the first required output time
! and ends after the last required output time
IF( (TOLD(1).GT.TNEW(1)) .OR. (TOLD(NOLD).LT.TNEW(NNEW)) ) THEN
 IERR=1
 RETURN
ENDIF
!
! loop through the output times
DO INEWLOOP=2,NNEW
 !
 T0 = TNEW(INEWLOOP-1)                      ! start of the time step
 T1 = TNEW(INEWLOOP)                        ! end of the time step
 !
 IBEG=1
 ! identify the index values that span the start of the time step
 BEG_ID: DO IOLDLOOP=2,NOLD
  IF(T0.LE.TOLD(IOLDLOOP)) THEN
   IBEG = IOLDLOOP
   EXIT
  ENDIF
 END DO BEG_ID
 !
 IEND=1
 ! identify the index values that span the end of the time step
 END_ID: DO IOLDLOOP=1,NOLD
  IF(T1.LE.TOLD(IOLDLOOP)) THEN
   IEND = IOLDLOOP
   EXIT
  ENDIF
 END DO END_ID
 !
 ! initialize the areas
 AREAB=0D0; AREAE=0D0; AREAM=0D0
 !
 ! special case: both TNEW(INEWLOOP-1) and TNEW(INEWLOOP) are within two original values
 ! (implies IBEG=IEND) -- estimate values at both end-points and average
 IF(T1.LT.TOLD(IBEG)) THEN
  SLOPE = (QOLD(IBEG)-QOLD(IBEG-1))/(TOLD(IBEG)-TOLD(IBEG-1))
  QEST0 = SLOPE*(T0-TOLD(IBEG-1)) + QOLD(IBEG-1)
  QEST1 = SLOPE*(T1-TOLD(IBEG-1)) + QOLD(IBEG-1)
  QNEW(INEWLOOP-1) = 0.5*(QEST0 + QEST1)
  CYCLE ! loop back to the next desired time
 ENDIF
 !
 ! estimate the area under the curve at the start of the time step
 IF(T0.LT.TOLD(IBEG)) THEN  ! if equal process as AREAM
  SLOPE = (QOLD(IBEG)-QOLD(IBEG-1))/(TOLD(IBEG)-TOLD(IBEG-1))
  QEST0 = SLOPE*(T0-TOLD(IBEG-1)) + QOLD(IBEG-1)
  AREAB = (TOLD(IBEG)-T0) * 0.5*(QEST0 + QOLD(IBEG))
 ENDIF
 !
 ! estimate the area under the curve at the end of the time step
 IF(T1.LT.TOLD(IEND)) THEN  ! if equal process as AREAM
  SLOPE = (QOLD(IEND)-QOLD(IEND-1))/(TOLD(IEND)-TOLD(IEND-1))
  QEST1 = SLOPE*(T1-TOLD(IEND-1)) + QOLD(IEND-1)
  AREAE = (T1-TOLD(IEND-1)) * 0.5*(QOLD(IEND-1) + QEST1)
 ENDIF
 !
 ! check if there are extra points to process
 IF(IBEG.LT.IEND) THEN
  ! loop through remaining points
  DO IMID=IBEG+1,IEND
   IF(IMID.LT.IEND .OR. &
     ! process the end slice as AREAM, but only if not already AREAB
     (IMID.EQ.IEND.AND.T1.EQ.TOLD(IEND).AND.T0.LT.TOLD(IEND-1)) ) THEN
      ! compute AREAM
      AREAM = AREAM + (TOLD(IMID) - TOLD(IMID-1)) * 0.5*(QOLD(IMID-1) + QOLD(IMID))
   ENDIF   ! if point is valid
  END DO  ! IMID
 ENDIF   ! If there is a possibility that middle points even exist
 !
 ! compute time step average
 AREAS = AREAB + AREAE + AREAM            ! sum of all areas
 QNEW(INEWLOOP-1) = AREAS / (T1-T0)       ! T1-T0 is the sum of all time slices
 !
END DO
INTERP_RCH = 0
RETURN
END FUNCTION INTERP_RCH
