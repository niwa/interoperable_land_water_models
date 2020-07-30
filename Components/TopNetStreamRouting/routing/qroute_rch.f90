!SUBROUTINE QROUTE_RCH(IENS,JRCH,RSTEP)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Ross Woods, 1997
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Route kinematic waves through the river network
!
! ----------------------------------------------------------------------------------------
! I/O::
!
!   Input(s):
!      IENS: ensemble member
!      JRCH: index of stream segment
!     ISTEP: index of time step
!
!   Outputs:
!     (none, updates data structures)
!
! ----------------------------------------------------------------------------------------
! Structures Used/Modified:
!
!   (1) MODEL_TIME
!   --------------
!   Uses the vector  MODTIM%TBOUNDS(:) to add time information to basin outflows
!
!   (2) BASIN_FLUX
!   --------------
!   Uses basin outflows [BASFLX(:)%INSTN_Q]
!
!   (3) REACHPARAM
!   --------------
!   Uses the network topology to process gutters [NETOPO(:)+] and the processing sequence
!    to identify the reach to process and the last reach in the network [RHRODER(:)]
!
!   (4) REACHSTATE
!   --------------
!   Uses the data structure KROUTE to track flow particles through the river network.
!    KROUTE is a data structure that contains the collection of flow points (in the
!    structure KWAVE) for each stream segment.  Each flow point has attributes QF (flow),
!    TI (time point entered a stream segment), TR (time point exited the stream segment,
!    or is expected to exit), and RF (logical routing flag [.TRUE. if point has exited]).
!    Hence [KROUTE(JRCH)%KWAVE(:)QF] defines the collection of flow points in the JRCH-th
!    reach. KROUTE must be saved for model re-starts
!
!   (5) REACH_FLUX
!   --------------
!   Contains timestep-average flow for each stream segment (computed here)
!
!   (6) INTERBLOCK
!   --------------
!   Includes an explicit interface to the sub-programs
!
! ----------------------------------------------------------------------------------------
! Method:
!
!   Flow routing is performed using a lagrangian one-dimensional Kinematic routing
!   scheme.  Flow generated from each catchment is tracked through the river network.
!   For each river segment, we compute the time each particle is expected to exit the
!   river segment (travel time = length/celerity).  If the "exit time" occurs before
!   the end of the time step, the particle is propagated to the downstream segment.
!   The "exit time" then becomes the time the particle entered the downstream segment.
!   This process is repeated for all river segments.  River segments are processed in
!   order from upstream segments to downstream segments, meaning that it is possible
!   for a particle to travel through several segments in a given time step.
!
!   Time step averages for each time step are computed by integrating over all the
!   flow points that exit a river segment in a given time step.  To avoid boundary
!   problems in the interpolation, the last routed wave from the previous time step
!   and the next wave that is expected to exit the segment are used.
!
! ----------------------------------------------------------------------------------------
! Source:
!
!   Most computations were originally performed within calcts in Topnet ver7, with calls
!   to subroutines in kinwav_v7.f
!
! ----------------------------------------------------------------------------------------
! Modifications to Source (mp.clark@niwa.co.nz):
!
!   * code is more modular
!
!   * added additional comments
!
!   * all variables are defined (IMPLICIT NONE) and described (comments)
!
!   * use of a new data structure (KROUTE) to hold and update the flow particles
!
!   * upgrade to F90 (especially structured variables and dynamic memory allocation
!
! ----------------------------------------------------------------------------------------
FUNCTION QROUTE_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,MAXQPAR,RSTEP)
!!DIR$ ATTRIBUTES DLLEXPORT :: QROUTE_RCH
!!DIR$ ATTRIBUTES ALIAS:'_qroute_rch_' :: QROUTE_RCH

! ----------------------------------------------------------------------------------------
USE nrtype
!USE bastype
USE laketype
USE reachtype
USE interreach, ONLY: GETUSQ_RCH,REMOVE_RCH,KINWAV_RCH,INTERP_RCH
!USE userparam
!USE user_state
IMPLICIT NONE
INTEGER(I4B)                                :: QROUTE_RCH
! Input
REAL(DP),INTENT(IN)                         :: DT     !
REAL(DP),DIMENSION(0:1),INTENT(IN)          :: TBOUNDS     !
INTEGER(I4B), INTENT(IN)                    :: JRCH     ! reach to process
INTEGER(I4B), INTENT(IN)                    :: NRCH
TYPE(RCHTOPO), DIMENSION(NRCH),INTENT(IN)   :: NETOPO
TYPE(KREACH),DIMENSION(NRCH),INTENT(INOUT)  :: KROUTE
TYPE(RCHPRP),DIMENSION(NRCH),INTENT(IN)     :: RPARAM
TYPE(MODFLX), DIMENSION(NRCH),INTENT(IN)    :: BASFLX
TYPE(STRFLX), INTENT(INOUT)                 :: RCHFLX
INTEGER(I4B), INTENT(IN)                    :: LAKEFLAG
INTEGER(I4B), INTENT(IN)                    :: NLAK
TYPE(LAKTOPO), DIMENSION(NLAK),INTENT(IN)   :: LKTOPO
TYPE(LKFLX),DIMENSION(NLAK),INTENT(IN)      :: LAKFLX
INTEGER(I4B),INTENT(IN)                     :: MAXQPAR
INTEGER(I4B), INTENT(IN), OPTIONAL          :: RSTEP    ! retrospective time step offset
!INTEGER(I4B)                                :: ret   ! 
! (1) extract flow from upstream reaches and append to the non-routed flow in JRCH
INTEGER(I4B)                                :: NUPS   ! number of upstream reaches
REAL(DP),DIMENSION(:),POINTER,SAVE          :: Q_JRCH ! flow in downstream reach JRCH
REAL(DP),DIMENSION(:),POINTER,SAVE          :: TENTRY ! entry time to JRCH (exit time u/s)
INTEGER(I4B)                                :: NQ1    ! # flow particles
! (2) route flow within the current [JRCH] river segment
INTEGER(I4B)                                :: ROFFSET ! retrospective offset due to rstep
REAL(DP)                                    :: T_START ! start of time step
REAL(DP)                                    :: T_END  ! end of time step
REAL(DP),DIMENSION(:),POINTER,SAVE          :: T_EXIT ! time particle expected exit JRCH
LOGICAL(LGT),DIMENSION(:),POINTER,SAVE      :: FROUTE ! routing flag .T. if particle exits
INTEGER(I4B)                                :: NQ2    ! # flow particles (<=NQ1 b/c merge)
! (3) calculate time-step averages
INTEGER(I4B)                                :: NR     ! # routed particles
INTEGER(I4B)                                :: NN     ! # non-routed particles
INTEGER(I4B)                                :: IERR   ! error code (1 = bad bounds)
REAL(DP),DIMENSION(2)                       :: TNEW   ! start/end of time step
REAL(DP),DIMENSION(1)                       :: QNEW   ! interpolated flow
! (4) housekeeping
REAL(DP)                                    :: Q_END  ! flow at the end of the timestep
REAL(DP)                                    :: TIMEI  ! entry time at the end of the timestep
TYPE(FPOINT), DIMENSION(:), POINTER, SAVE   :: NEW_WAVE  ! temporary wave
LOGICAL(LGT),SAVE                           :: INIT=.TRUE. ! used to initialize pointers
! ----------------------------------------------------------------------------------------
! (0) INITIALIZE POINTERS
! ----------------------------------------------------------------------------------------
IF(INIT) THEN
 INIT=.FALSE.
 NULLIFY(Q_JRCH,TENTRY,T_EXIT,FROUTE,NEW_WAVE)
ENDIF
RCHFLX%TAKE=0.0_DP ! initialize take from this reach
! ----------------------------------------------------------------------------------------
! (1) EXTRACT FLOW FROM UPSTREAM REACHES & APPEND TO THE NON-ROUTED FLOW PARTICLES IN JRCH
! ----------------------------------------------------------------------------------------
NUPS = SIZE(NETOPO(JRCH)%UREACHI)        ! number of upstream reaches
IF (NUPS.GT.0) THEN
 !CALL GETUSQ_RCH(IENS,JRCH,Q_JRCH,TENTRY,T_EXIT,RSTEP)
 qroute_rch = GETUSQ_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,Q_JRCH,TENTRY,T_EXIT,RSTEP)
 if(qroute_rch > 1) return
ELSE
 ! set flow in headwater reaches to modelled streamflow from time delay histogram
 RCHFLX%REACH_Q = BASFLX(JRCH)%INSTN_Q(1)
 RETURN  ! no upstream reaches (routing for sub-basins done using time-delay histogram)
ENDIF
! check for negative flow
IF (MINVAL(Q_JRCH).LT.0.0D0) THEN
 print *, 'WARNING: negative flow in kinwav_rch!  Reach = ', JRCH
 print *, Q_JRCH
 PAUSE
ENDIF
! ----------------------------------------------------------------------------------------
! (2) REMOVE FLOW PARTICLES (REDUCE MEMORY USAGE AND PROCESSING TIME)
! ----------------------------------------------------------------------------------------
IF (SIZE(Q_JRCH).GT.MAXQPAR) then 
    qroute_rch = REMOVE_RCH(Q_JRCH,TENTRY,T_EXIT,MAXQPAR)
    if(qroute_rch > 1) return
end if
NQ1 = SIZE(Q_JRCH)-1                                     ! -1 because of the zero element
! ----------------------------------------------------------------------------------------
! (3) ROUTE FLOW WITHIN THE CURRENT [JRCH] RIVER SEGMENT
! ----------------------------------------------------------------------------------------
! set the retrospective offset
IF (.NOT.PRESENT(RSTEP)) THEN
  ROFFSET = 0
ELSE
  ROFFSET = RSTEP
END IF
! set time boundaries
T_START = TBOUNDS(0) - DT*ROFFSET
T_END   = TBOUNDS(1) - DT*ROFFSET
ALLOCATE(FROUTE(0:NQ1),STAT=IERR)
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem allocating array [qroute_rch.f90]')
   PRINT *, 'problem deallocating array [qroute_rch.f90]'
   QROUTE_RCH = 1
   RETURN
END IF
FROUTE(0) = .TRUE.; FROUTE(1:NQ1)=.FALSE.  ! init. routing flags
! route flow through the current [JRCH] river segment (Q_JRCH in units of m2/s)
!CALL KINWAV_RCH(JRCH,T_START,T_END,Q_JRCH(1:NQ1),TENTRY(1:NQ1),&    ! (input)
!                                   FROUTE(1:NQ1),T_EXIT(1:NQ1),NQ2) ! (output)
qroute_rch = KINWAV_RCH(RPARAM(JRCH),JRCH,T_START,T_END,Q_JRCH(1:NQ1),TENTRY(1:NQ1),FROUTE(1:NQ1),T_EXIT(1:NQ1),NQ2)
if(qroute_rch > 0) return
! ----------------------------------------------------------------------------------------
! (4) COMPUTE TIME-STEP AVERAGES
! ----------------------------------------------------------------------------------------
NR = COUNT(FROUTE)-1   ! -1 because of the zero element (last routed)
NN = NQ2-NR            ! number of non-routed points
TNEW = (/T_START,T_END/)
! (zero position last routed; use of NR+1 instead of NR keeps next expected routed point)
!CALL INTERP_RCH(T_EXIT(0:NR+1),Q_JRCH(0:NR+1),TNEW,QNEW,IERR)
qroute_rch = INTERP_RCH(T_EXIT(0:NR+1),Q_JRCH(0:NR+1),TNEW,QNEW,IERR)
if(qroute_rch > 0) return
! m2/s --> m3/s + instantaneous runoff from basin
RCHFLX%REACH_Q = QNEW(1)*RPARAM(JRCH)%R_WIDTH + BASFLX(JRCH)%INSTN_Q(1)
! ----------------------------------------------------------------------------------------
! (5) HOUSEKEEPING
! ----------------------------------------------------------------------------------------
! compute the instantaneous flow at the end of the time step
!   (last routed point)
Q_END = Q_JRCH(NR) + &   !        (dQ/dT)                                 (dT)
         ( (Q_JRCH(NR+1)-Q_JRCH(NR)) / (T_EXIT(NR+1)-T_EXIT(NR)) ) * (T_END-T_EXIT(NR))
! compute an approximate entry time (needed for the remove routine later)
TIMEI = TENTRY(NR) + &   !        (dT/dT)                                 (dT)
         ( (TENTRY(NR+1)-TENTRY(NR)) / (T_EXIT(NR+1)-T_EXIT(NR)) ) * (T_END-T_EXIT(NR))
! allocate space for the routed data (+1 to allocate space for the interpolated point)
IF (.NOT.ASSOCIATED(KROUTE(JRCH)%KWAVE)) pause ' not associated '
!DEALLOCATE(KROUTE(IENS,JRCH)%KWAVE,STAT=IERR)
!IF (IERR.NE.0) &
!  CALL EXIT_TOPNET(1,'problem deallocating array [qroute_rch.f90]')
IF(ASSOCIATED(KROUTE(JRCH)%KWAVE)) THEN
 DEALLOCATE(KROUTE(JRCH)%KWAVE, STAT=IERR)
 IF (IERR.NE.0) THEN
  print *, '% CANNOT DEALLOCATE SPACE (FOR SOME UNKNOWN REASON)... TRY AND NULLIFY!'
  NULLIFY(KROUTE(JRCH)%KWAVE)
 ENDIF
ENDIF
ALLOCATE(KROUTE(JRCH)%KWAVE(0:NQ2+1),STAT=IERR)   ! NQ2 is number of points for kinematic routing
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem allocating array [qroute_rch.f90]')
   PRINT *, 'problem allocating array [qroute_rch.f90]'
   QROUTE_RCH = 1
   RETURN
END IF
! insert the interpolated point (TI is irrelevant, as the point is "routed")
KROUTE(JRCH)%KWAVE(NR+1)%QF=Q_END;   KROUTE(JRCH)%KWAVE(NR+1)%TI=TIMEI
KROUTE(JRCH)%KWAVE(NR+1)%TR=T_END;   KROUTE(JRCH)%KWAVE(NR+1)%RF=.TRUE.
! add the output from kinwave...         - skip NR+1
! (when JRCH becomes IR routed points will be stripped out & the structures updated again)
KROUTE(JRCH)%KWAVE(0:NR)%QF=Q_JRCH(0:NR); KROUTE(JRCH)%KWAVE(NR+2:NQ2+1)%QF=Q_JRCH(NR+1:NQ2)
KROUTE(JRCH)%KWAVE(0:NR)%TI=TENTRY(0:NR); KROUTE(JRCH)%KWAVE(NR+2:NQ2+1)%TI=TENTRY(NR+1:NQ2)
KROUTE(JRCH)%KWAVE(0:NR)%TR=T_EXIT(0:NR); KROUTE(JRCH)%KWAVE(NR+2:NQ2+1)%TR=T_EXIT(NR+1:NQ2)
KROUTE(JRCH)%KWAVE(0:NR)%RF=FROUTE(0:NR); KROUTE(JRCH)%KWAVE(NR+2:NQ2+1)%RF=FROUTE(NR+1:NQ2)
KROUTE(JRCH)%KWAVE(0:NQ2+1)%QM=-9999
!IF (NUSER.GT.0.AND.SIMDAT%UCFFLAG.GE.1) THEN
!  CALL EXTRACT_FROM_RCH(IENS,JRCH,NR,Q_JRCH,T_EXIT,T_END,TNEW)
!ENDIF
!if (iens.eq.3 .and. jrch.eq.1) print *, jrch, KROUTE(IENS,JRCH)%KWAVE(:)%QF
!if (iens.eq.3 .and. jrch.eq.1) print *, jrch, KROUTE(IENS,JRCH)%KWAVE(:)%TI
!if (iens.eq.3 .and. jrch.eq.1) print *, jrch, KROUTE(IENS,JRCH)%KWAVE(:)%TR
!if (iens.eq.3 .and. jrch.eq.1) print *, jrch, KROUTE(IENS,JRCH)%KWAVE(:)%RF
! free up space for the next reach
DEALLOCATE(Q_JRCH,TENTRY,T_EXIT,FROUTE,STAT=IERR)   ! FROUTE defined in this sub-routine
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem deallocating array [qroute_rch.f90]')
   PRINT *, 'problem deallocating array [qroute_rch.f90]'
   QROUTE_RCH = 1
   RETURN
END IF
! if the last reach or lake inlet (and lakes are enabled), remove routed elements from memory
IF (NETOPO(JRCH)%DREACHI.LT.0 .OR. &  ! if the last reach, then there is no downstream reach
    (LAKEFLAG.GT.0.AND.NETOPO(JRCH)%LAKINLT)) THEN ! if lake inlet
 ! copy data to a temporary wave
 IF (ASSOCIATED(NEW_WAVE)) THEN
   DEALLOCATE(NEW_WAVE,STAT=IERR)
   IF (IERR.NE.0) THEN
     !CALL EXIT_TOPNET(1,'problem deallocating array [qroute_rch.f90]')
       PRINT *, 'problem deallocating array [qroute_rch.f90]'
       QROUTE_RCH = 1
       RETURN
   END IF
 END IF
 ALLOCATE(NEW_WAVE(0:NN),STAT=IERR)  ! NN = number non-routed (the zero element is the last routed point)
 IF (IERR.NE.0) THEN
   !CALL EXIT_TOPNET(1,'problem allocating array [qroute_rch.f90]')
   PRINT *, 'problem allocating array [qroute_rch.f90]'
   QROUTE_RCH = 1
   RETURN
 END IF
   
 NEW_WAVE(0:NN) = KROUTE(JRCH)%KWAVE(NR+1:NQ2+1)  ! +1 because of the interpolated point
 ! re-size wave structure
 IF (ASSOCIATED(KROUTE(JRCH)%KWAVE)) THEN
   DEALLOCATE(KROUTE(JRCH)%KWAVE,STAT=IERR)
   IF (IERR.NE.0) THEN
     !CALL EXIT_TOPNET(1,'problem deallocating array [qroute_rch.f90]')
        PRINT *, 'problem deallocating array [qroute_rch.f90]'
       QROUTE_RCH = 1
       RETURN
   END IF
END IF
 ALLOCATE(KROUTE(JRCH)%KWAVE(0:NN),STAT=IERR)  ! again, the zero element for the last routed point
 IF (IERR.NE.0) THEN
   !CALL EXIT_TOPNET(1,'problem allocating array [qroute_rch.f90]')
    PRINT *, 'problem allocating array [qroute_rch.f90]'
   QROUTE_RCH = 1
   RETURN
 END IF
! copy data back to the wave structure and deallocate space for the temporary wave
 KROUTE(JRCH)%KWAVE(0:NN) = NEW_WAVE(0:NN)
 DEALLOCATE(NEW_WAVE,STAT=IERR)
 IF (IERR.NE.0) THEN
   !CALL EXIT_TOPNET(1,'problem deallocating array [qroute_rch.f90]')
   PRINT *, 'problem deallocating array [qroute_rch.f90]'
   QROUTE_RCH = 1
   RETURN
 END IF
ENDIF  ! (if JRCH is the last reach or lake inlet (and lakes enabled))
QROUTE_RCH = 0
RETURN
! --------------------------------------------------------------------------------------------
END FUNCTION QROUTE_RCH
