!SUBROUTINE KINWAV_RCH(JRCH,T_START,T_END,Q_JRCH,TENTRY,FROUTE,T_EXIT,NQ2)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Derek Goring, 1994; modified by Ross Woods, 2002
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to calculate the propagation of kinematic waves in an individual stream
!   segment, including the formation and propagation of a kinematic shock.
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   Input(s):
!   ---------
!      JRCH: index of reach processed
!   T_START: start of the time step
!     T_END: end of the time step
!    Q_JRCH: array of flow elements -- neighbouring times are merged if a shock forms, but
!                                      then merged flows are dis-aggregated to save the
!                                      flow either side of a shock -- thus we may have
!                                      fewer elements on output if several particles are
!                                      merged, INTENT(INOUT)
!    TENTRY: array of time elements -- neighbouring times are merged if a shock forms,
!                                      then merged times are dis-aggregated, one second is
!                                      added to the time corresponding to the higer merged
!                                      flow (note also fewer elements), INTENT(INOUT)
!
!   Outputs:
!   --------
!    FROUTE: array of routing flags -- All inputs are .FALSE., but flags change to .TRUE.
!                                      if element is routed INTENT(OUT)
!    T_EXIT: array of time elements -- identify the time each element is EXPECTED to exit
!                                      the stream segment, INTENT(OUT).  Used in INTERPTS
!       NQ2: number of particles    -- <= input becuase multiple particles may merge
!
! ----------------------------------------------------------------------------------------
! Structures Used:
!
!   REACHPARAM:  Use channel slope, width, length, etc.
!
! ----------------------------------------------------------------------------------------
! Method:
!
!   Flow routing through an individual stream segment is performed using the method
!   described by Goring (1994).  For each wave in the stream segment, Goring's
!   method calculates the celerity (m/s) and travel time, length/celerity (s).  If
!   the initial time of the wave plus the travel time is less than the time at the
!   end of a time step, then the wave is routed to the end of the stream segment and
!   the time stamp is updated.  Otherwise, the wave remains in the stream segment
!   and the time stamp remains constant.  In this context "earlier" times imply that
!   a kinematic shock is located nearer the downstream end of a stream segment.
!
!   A decrease in the time between kinematic waves deceases eventually produces a
!   discontinuity known as a kinematic shock.  When this occurs the kinematic waves
!   are merged and the celerity is modified.  See Goring (1994) for more details.
!
! ----------------------------------------------------------------------------------------
! Source:
!
!   This routine is essentially a direct copy of the subroutine kinwav, located in
!   the file kinwav_v7.f
!
! ----------------------------------------------------------------------------------------
! Modifications to source (mp.clark@niwa.co.nz):
!
!   * All variables are now defined (IMPLICIT NONE) and described (comments)
!
!   * Parameters are defined within the subroutine (for ease of readibility)
!
!   * Added many comments
!
!   * Replaced GOTO statements with DO-CYCLE loops and DO-FOREVER loops with EXIT clause
!      (for ease of readability), and replaced some do-continue loops w/ array operations
!      and use F90 dynamic memory features
!
! ----------------------------------------------------------------------------------------
FUNCTION KINWAV_RCH(RPARAM,JRCH,T_START,T_END,Q_JRCH,TENTRY,FROUTE,T_EXIT,NQ2)
! ----------------------------------------------------------------------------------------
USE nrtype
USE reachtype
USE nrutil, ONLY : arth
IMPLICIT NONE
INTEGER(I4B)                                :: KINWAV_RCH
! Input
TYPE(RCHPRP)                                :: RPARAM
INTEGER(I4B), INTENT(IN)                    :: JRCH     ! Reach to process
REAL(DP), INTENT(IN)                        :: T_START  ! start of the time step
REAL(DP), INTENT(IN)                        :: T_END    ! end of the time step
! Input/Output
REAL(DP), DIMENSION(:), INTENT(INOUT)       :: Q_JRCH   ! flow to be routed
REAL(DP), DIMENSION(:), INTENT(INOUT)       :: TENTRY   ! time to be routed
REAL(DP), DIMENSION(:), INTENT(INOUT)       :: T_EXIT   ! time pts expected exit segment
LOGICAL(LGT), DIMENSION(:), INTENT(INOUT)   :: FROUTE   ! routing flag, T=routed
! Output
INTEGER(I4B), INTENT(OUT)                   :: NQ2      ! # particles (<= input b/c merge)
! Internal
REAL(DP)                                    :: ALFA     ! constant, 5/3
REAL(DP)                                    :: K        ! sqrt(slope)/mannings N
REAL(DP)                                    :: XMX      ! length of the stream segment
INTEGER(I4B)                                :: NN       ! number of input points
INTEGER(I4B)                                :: NI       ! original size of the input
INTEGER(I4B)                                :: NM       ! mumber of merged elements
INTEGER(I4B), DIMENSION(SIZE(Q_JRCH))       :: IX       ! minimum index of each merged element
INTEGER(I4B), DIMENSION(SIZE(Q_JRCH))       :: MF       ! index for input element merged
REAL(DP), DIMENSION(SIZE(Q_JRCH))           :: T0,T1,T2 ! copy of input time
REAL(DP), DIMENSION(SIZE(Q_JRCH))           :: Q0,Q1,Q2 ! flow series
REAL(DP), DIMENSION(SIZE(Q_JRCH))           :: WC       ! wave celerity
INTEGER(I4B)                                :: IW,JW    ! looping variables, break check
REAL(DP)                                    :: X,XB     ! define smallest, biggest shock
REAL(DP)                                    :: WDIFF    ! difference in wave celerity-1
REAL(DP)                                    :: XXB      ! wave break
INTEGER(I4B)                                :: IXB,JXB  ! define position of wave break
REAL(DP)                                    :: A1,A2    ! stage - different sides of break
REAL(DP)                                    :: CM       ! merged celerity
REAL(DP)                                    :: TEXIT    ! expected exit time of "current" particle
REAL(DP)                                    :: TNEXT    ! expected exit time of "next" particle
REAL(DP)                                    :: TEXIT2   ! exit time of "bottom" of merged element
INTEGER(I4B)                                :: IROUTE   ! looping variable for routing
INTEGER(I4B)                                :: JROUTE   ! looping variable for routing
INTEGER(I4B)                                :: ICOUNT   ! used to account for merged pts
! ----------------------------------------------------------------------------------------
! NOTE: If merged particles DO NOT exit the reach in the current time step, they are
!       disaggregated into the original particles; if the merged particles DO exit the
!       reach, then we save only the "slowest" and "fastest" particle.
! ----------------------------------------------------------------------------------------
!       To disaggregate particles we need to keep track of the output element for which
!       each input element is merged. This is done using the integer vector MF:  If, for
!       example, MF = (1,2,2,2,3,4,5,5,5,5,6,7,8), this means the 2nd, 3rd, and 4th input
!       particles have been merged into the 2nd particle of the output, and that the 7th,
!       8th, 9th, and 10th input particles have been merged into the 5th particle of the
!       output.  We only store the "slowest" and "fastest" particle within the merged set.
! ----------------------------------------------------------------------------------------
!       Disaggregating the particles proceeds as follows:  If particles have been merged,
!       then the flow in Q1 will be different from the flow in Q2, that is, continuing with
!       the above example, Q1(IROUTE).NE.Q2(IROUTE), where IROUTE = 2 or 5.  In the case of
!       a merged particle we identify all elements of MF that are equal to IROUTE (that is,
!       the 2nd, 3rd, and 4th elements of MF = 2), populate the output vector with the
!       selected elements (2,3,4) of the input vector.
! ----------------------------------------------------------------------------------------
! Get the reach parameters
ALFA = 5D0/3D0        ! should this be initialized here or in a parameter file?
K    = SQRT(RPARAM%R_SLOPE)/RPARAM%R_MAN_N
XMX  = RPARAM%RLENGTH
! Identify the number of points to route
NN = SIZE(Q1)                                ! modified when elements are merged
NI = NN                                      ! original size of the input
IF(NN.EQ.0) RETURN                           ! don't do anything if no points in the reach
! Initialize the vector that indicates which output element the input elements are merged
MF = ARTH(1,1,NI)                            ! Num. Rec. intrinsic: see MODULE nrutil.f90
! Initialize the vector that indicates the minumum index of each merged element
IX = ARTH(1,1,NI)                            ! Num. Rec. intrinsic: see MODULE nrutil.f90
! Get copies of the flow/time particles
Q0=Q_JRCH; Q1=Q_JRCH; Q2=Q_JRCH
T0=TENTRY; T1=TENTRY; T2=TENTRY
! compute wave celerity for all flow points (array operation)
WC(1:NN) = ALFA*K**(1./ALFA)*Q1(1:NN)**((ALFA-1.)/ALFA)
GT_ONE: IF(NN.GT.1) THEN                     ! no breaking if just one point
 X = 0.                                      ! altered later to describe "closest" shock
 GOTALL: DO                                  ! keep going until all shocks are merged
  XB = XMX                                   ! initialized to length of the stream segment
  ! --------------------------------------------------------------------------------------
  ! check for breaking
  ! --------------------------------------------------------------------------------------
  WCHECK: DO IW=2,NN
   JW=IW-1
   IF(WC(IW).EQ.0. .OR. WC(JW).EQ.0.) CYCLE  ! waves not moving
   WDIFF = 1./WC(JW) - 1./WC(IW)             ! difference in wave celerity
   IF(WDIFF.EQ.0.) CYCLE                     ! waves moving at the same speed
   IF(WC(IW).EQ.WC(JW)) CYCLE                ! identical statement to the above?
   XXB = (T1(IW)-T1(JW)) / WDIFF             ! XXB is point of breaking in x direction
   IF(XXB.LT.X .OR. XXB.GT.XB) CYCLE         ! XB init at LENGTH, so > XB do in next reach
   ! if get to here, the wave is breaking
   XB  = XXB                                 ! identify break "closest to upstream" first
   IXB = IW
  END DO WCHECK
  ! --------------------------------------------------------------------------------------
  IF(XB.EQ.XMX) EXIT                         ! got all breaking waves, exit gotall
  ! --------------------------------------------------------------------------------------
  ! combine waves
  ! --------------------------------------------------------------------------------------
  NN  = NN-1
  JXB = IXB-1                                ! indices for the point of breaking
  NM  = NI-NN                                ! number of merged elements
  ! calculate merged shockwave celerity (CM) using finite-difference approximation
  Q2(JXB) =MAX(Q2(JXB),Q2(IXB))              ! flow of largest merged point
  Q1(JXB) =MIN(Q1(JXB),Q1(IXB))              ! flow of smallest merged point
  A2 = (Q2(JXB)/K)**(1./ALFA)                ! Q = (1./MAN_N) H**(ALFA) sqrt(SLOPE)
  A1 = (Q1(JXB)/K)**(1./ALFA)                ! H = (Q/K)**(1./ALFA) (K=sqrt(SLOPE)/MAN_N)
  CM = (Q2(JXB)-Q1(JXB))/(A2-A1)             ! NB:  A1,A2 are river stage
  ! update merged point
  T1(JXB) = T1(JXB) + XB/WC(JXB) - XB/CM     ! updated starting point
  WC(JXB) = CM
  ! if input elements are merged, then reduce index of merged element plus all remaining elements
  MF(IX(IXB):NI) = MF(IX(IXB):NI)-1         ! NI is the original size of the input
  ! re-number elements, ommitting the element just merged
  IX(IXB:NN) = IX(IXB+1:NN+1)               ! index (minimum index value of each merged particle)
  T1(IXB:NN) = T1(IXB+1:NN+1)               ! entry time
  WC(IXB:NN) = WC(IXB+1:NN+1)               ! wave celerity
  Q1(IXB:NN) = Q1(IXB+1:NN+1)               ! unmodified flows
  Q2(IXB:NN) = Q2(IXB+1:NN+1)               ! unmodified flows
  ! update X - already got the "closest shock to start", see if there are any other shocks
  X = XB
  ! --------------------------------------------------------------------------------------
 END DO GOTALL
ENDIF GT_ONE
!
ICOUNT=0
! ----------------------------------------------------------------------------------------
! perform the routing
! ----------------------------------------------------------------------------------------
DO IROUTE = 1,NN    ! loop through the remaining particles (shocks,waves) (NM=NI-NN have been merged)
 ! compute the time the shock will exit the reach
 TEXIT = MIN(XMX/WC(IROUTE) + T1(IROUTE), HUGE(T1))
 ! compute the time the next shock will exit the reach
 IF (IROUTE.LT.NN) TNEXT = MIN(XMX/WC(IROUTE+1) + T1(IROUTE+1), HUGE(T1))
 IF (IROUTE.EQ.NN) TNEXT = HUGE(T1)
 ! check if element is merged
 MERGED: IF(Q1(IROUTE).NE.Q2(IROUTE)) THEN
  ! check if merged element has exited
  IF(TEXIT.LT.T_END) THEN
   ! when a merged element exits, save just the top and the bottom of the shock
   ! (identify the exit time for the "slower" particle)
   TEXIT2 = MIN(TEXIT+1.0D0, TEXIT + 0.5D0*(MIN(TNEXT,T_END)-TEXIT))
   ! unsure what will happen in the rare case if TEXIT and TEXIT2 are the same
   ! this case in taken care of in the first check of rupdate subroutine below
   !IF (TEXIT2.EQ.TEXIT) THEN
   ! IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'TEXIT equals TEXIT2 in kinwav'
   ! CALL EXIT_TOPNET(1,'TEXIT equals TEXIT2 in kinwav [kinwav_rch.f90]')
   !ENDIF
   ! fill output arrays
   !CALL RUPDATE(Q1(IROUTE),T1(IROUTE),TEXIT)    ! fill arrays w/ Q1, T1, + run checks
   !CALL RUPDATE(Q2(IROUTE),T1(IROUTE),TEXIT2)   ! fill arrays w/ Q2, T1, + run checks
   KINWAV_RCH = RUPDATE(Q1(IROUTE),T1(IROUTE),TEXIT)
   if(KINWAV_RCH > 0) return
   KINWAV_RCH = RUPDATE(Q2(IROUTE),T1(IROUTE),TEXIT2)
   if(KINWAV_RCH > 0) return
  ELSE                                      ! merged elements have not exited
   ! when a merged element does not exit, need to disaggregate into original particles
   DO JROUTE=1,NI                           ! loop thru # original inputs
!    IF(MF(JROUTE).EQ.IROUTE) &
!     CALL RUPDATE(Q0(JROUTE),T0(JROUTE),TEXIT)  ! fill arrays w/ Q0, T0, + run checks
    IF(MF(JROUTE).EQ.IROUTE) then
     KINWAV_RCH = RUPDATE(Q0(JROUTE),T0(JROUTE),TEXIT)  ! fill arrays w/ Q0, T0, + run checks
     if(KINWAV_RCH > 0) return
    end if
   END DO  ! JROUTE
  ENDIF   ! TEXIT
 ! now process un-merged particles
 ELSE MERGED  ! (i.e., not merged)
  !CALL RUPDATE(Q1(IROUTE),T1(IROUTE),TEXIT)     ! fill arrays w/ Q1, T1, + run checks
  KINWAV_RCH = RUPDATE(Q1(IROUTE),T1(IROUTE),TEXIT)
  if(KINWAV_RCH > 0) return
 ENDIF MERGED
END DO
! update arrays
NQ2 = ICOUNT
KINWAV_RCH = 0
RETURN
! ----------------------------------------------------------------------------------------
CONTAINS
 function RUPDATE(QNEW,TOLD,TNEW)
 REAL(DP),INTENT(IN)                        :: QNEW      ! Q0,Q1, or Q2
 REAL(DP),INTENT(IN)                        :: TOLD,TNEW ! entry/exit times
 INTEGER(I4B)                               :: rupdate
 ! ---------------------------------------------------------------------------------------
 ! Used to compute the time each element will exit stream segment & update routing flag
 ! NB: internal subroutine so all data from host is available
 ! ---------------------------------------------------------------------------------------
 ICOUNT=ICOUNT+1
 ! check for array bounds exceeded
 IF (ICOUNT.GT.SIZE(Q_JRCH)) THEN
  !CALL EXIT_TOPNET(1,'array bounds exceeded, in qroute_rch/kinwav_rch/rupdate [kinwav_rch.f90]')
  print *, 'array bounds exceeded, in qroute_rch/kinwav_rch/rupdate [kinwav_rch.f90]'
  rupdate = 0
  return
 ENDIF
 ! fill output arrays
 Q_JRCH(ICOUNT) = QNEW                         ! flow (Q1 always smaller than Q2)
 TENTRY(ICOUNT) = TOLD                         ! time - note, T1 altered if element merged
 T_EXIT(ICOUNT) = TNEW
 ! time check -- occurs when disaggregating merged elements
 IF (ICOUNT.GT.1) THEN
  IF (T_EXIT(ICOUNT).LE.T_EXIT(ICOUNT-1)) T_EXIT(ICOUNT)=T_EXIT(ICOUNT-1)+1.
 ENDIF
 ! another time check -- rare problem when the shock can get the same time as tstart
 IF(ICOUNT.EQ.1.AND.T_EXIT(ICOUNT).LE.T_START) T_EXIT(ICOUNT)=T_START+1.
 ! update flag for routed elements
 IF(T_EXIT(ICOUNT).LT.T_END) FROUTE(ICOUNT) =.TRUE.
 rupdate = 0
 RETURN
 END function RUPDATE
END FUNCTION KINWAV_RCH
