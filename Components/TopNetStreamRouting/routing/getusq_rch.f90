!SUBROUTINE GETUSQ_RCH(IENS,JRCH,Q_JRCH,TENTRY,T_EXIT,RSTEP)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Ross Woods, 2000; Martyn Clark, 2006
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to extract routed flow from the reaches upstream of JRCH, and append to the
!    non-routed flow in JRCH
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   Input(s):
!       IENS: Ensemble member
!       JRCH: Index of the downstream reach
!      RSTEP: Retrospective time step
!
!   Outputs:
!  Q_JRCH(:): Vector of merged flow particles in reach JRCH
!  TENTRY(:): Vector of times flow particles entered reach JRCH (exited upstream reaches)
!  T_EXIT(:): Vector of times flow particles are expected to exit reach JRCH
!
! ----------------------------------------------------------------------------------------
FUNCTION GETUSQ_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,Q_JRCH,TENTRY,T_EXIT,RSTEP)
! ----------------------------------------------------------------------------------------
USE nrtype
USE interreach, ONLY: qexmul_rch
!USE bastype
USE reachtype
USE laketype
IMPLICIT NONE
INTEGER(I4B)                                ::GETUSQ_RCH
! Input
REAL(DP),INTENT(IN)                         :: DT     !
REAL(DP),DIMENSION(0:1),INTENT(IN)          :: TBOUNDS     !
INTEGER(I4B), INTENT(IN)                    :: JRCH     ! reach to process
INTEGER(I4B), INTENT(IN)                    :: NRCH
TYPE(RCHTOPO), DIMENSION(NRCH),INTENT(IN)   :: NETOPO
TYPE(KREACH),DIMENSION(NRCH),INTENT(INOUT)  :: KROUTE
TYPE(RCHPRP),DIMENSION(NRCH),INTENT(IN)     :: RPARAM
TYPE(MODFLX), DIMENSION(NRCH),INTENT(IN)    :: BASFLX
INTEGER(I4B), INTENT(IN)                    :: LAKEFLAG
INTEGER(I4B), INTENT(IN)                    :: NLAK
TYPE(LAKTOPO), DIMENSION(NLAK),INTENT(IN)   :: LKTOPO
TYPE(LKFLX),DIMENSION(NLAK),INTENT(IN)      :: LAKFLX
INTEGER(I4B), INTENT(IN), OPTIONAL          :: RSTEP    ! retrospective time step offset
! Output
REAL(DP), DIMENSION(:), POINTER             :: Q_JRCH   ! merged (non-routed) flow in JRCH
REAL(DP), DIMENSION(:), POINTER             :: TENTRY   ! time flow particles entered JRCH
REAL(DP), DIMENSION(:), POINTER             :: T_EXIT   ! time flow is expected to exit JR
! Local variables to hold the merged inputs to the downstream reach
INTEGER(I4B)                                :: ROFFSET  ! retrospective offset due to rstep
REAL(DP), DIMENSION(:), POINTER             :: QD       ! merged downstream flow
REAL(DP), DIMENSION(:), POINTER             :: TD       ! merged downstream time
INTEGER(I4B)                                :: ND       ! # points shifted downstream
INTEGER(I4B)                                :: NJ       ! # points in the JRCH reach
INTEGER(I4B)                                :: NK       ! # points for routing (NJ+ND)
INTEGER(I4B)                                :: ILAK_O   ! lake index for outlet reach
INTEGER(I4B)                                :: IERR     ! error status
!INTEGER(I4B)                                :: ret     ! error status
! ----------------------------------------------------------------------------------------
! (1) EXTRACT (AND MERGE) FLOW FROM UPSTREAM REACHES OR LAKE
! ----------------------------------------------------------------------------------------
! set the retrospective offset
IF (.NOT.PRESENT(RSTEP)) THEN
  ROFFSET = 0
ELSE
  ROFFSET = RSTEP
END IF
IF (LAKEFLAG.GT.0) THEN               ! only if lakes enabled
 ! get lake outflow and only lake outflow if reach is a lake outlet reach, else do as normal
 IF (NETOPO(JRCH)%LAKE_IX.GT.0) THEN ! part of reach is in lake
  IF (ANY(LKTOPO(:)%DREACHI.EQ.JRCH.AND.LKTOPO(:)%LAKE_ID.GT.0)) THEN ! we are in a lake outlet reach
   ILAK_O = MINVAL(LKTOPO(:)%LAKE_IX, MASK=LKTOPO(:)%DREACHI.EQ.JRCH) ! lake index
   ND = 1
   ALLOCATE(QD(1),TD(1),STAT=IERR)
   IF (IERR.NE.0) THEN
   !  CALL EXIT_TOPNET(1,'problem allocating array [getusq_rch.f90]')
       PRINT *, 'problem allocating array [getusq_rch.f90]'
       GETUSQ_RCH = 1
       RETURN
   END IF
   QD(1) = LAKFLX(ILAK_O)%LAKE_Q / RPARAM(JRCH)%R_WIDTH  ! lake outflow per unit reach width
   TD(1) = TBOUNDS(1) - DT*ROFFSET
  ELSE
   !CALL QEXMUL_RCH(IENS,JRCH,ND,QD,TD,RSTEP)        ! do as normal for unsubmerged part of inlet reach
   GETUSQ_RCH = QEXMUL_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,ND,QD,TD,RSTEP)
   if(GETUSQ_RCH) return
  ENDIF
 ELSE
  !CALL QEXMUL_RCH(IENS,JRCH,ND,QD,TD,RSTEP)         ! not in lake; do as normal
  GETUSQ_RCH = QEXMUL_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,ND,QD,TD,RSTEP)
  if(GETUSQ_RCH) return
 ENDIF
ELSE                                         ! lakes disabled
 !CALL QEXMUL_RCH(IENS,JRCH,ND,QD,TD,RSTEP)         ! includes merging flow from different reaches
  GETUSQ_RCH = QEXMUL_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,ND,QD,TD,RSTEP)
  if(GETUSQ_RCH) return
ENDIF
! ----------------------------------------------------------------------------------------
! (2) EXTRACT NON-ROUTED FLOW FROM THE REACH JRCH & APPEND TO THE FLOW JUST ROUTED D/S
! ----------------------------------------------------------------------------------------
! check that the wave has been initialized
IF (ASSOCIATED(KROUTE(JRCH)%KWAVE).EQV..FALSE.) THEN
 ! if not initialized, then set initial flow to first flow
 ! (this will only occur for a cold start in the case of no streamflow observations)
 ALLOCATE(KROUTE(JRCH)%KWAVE(0:0),STAT=IERR)
 IF (IERR.NE.0) THEN
   !CALL EXIT_TOPNET(1,'problem allocating array [getusq_rch.f90]')
       PRINT *, 'problem allocating array [getusq_rch.f90]'
       GETUSQ_RCH = 1
       RETURN
 END IF
 KROUTE(JRCH)%KWAVE(0)%QF = QD(1)
 KROUTE(JRCH)%KWAVE(0)%TI = TBOUNDS(0) - DT - DT*ROFFSET
 KROUTE(JRCH)%KWAVE(0)%TR = TBOUNDS(0)      - DT*ROFFSET
 KROUTE(JRCH)%KWAVE(0)%RF = .TRUE.
 !IF (IO_INFO%DEBUG_FILE) WRITE(99,*) jrch, KROUTE(IENS,JRCH)%KWAVE
ENDIF
! now extract the non-routed flow
! NB: routed flows were stripped out in the previous timestep when JRCH was index of u/s reach
!  {only non-routed flows remain in the routing structure [ + zero element (last routed)]}
NJ = SIZE(KROUTE(JRCH)%KWAVE) - 1           ! number of elements not routed (-1 for 0)
NK = NJ + ND                                     ! pts still in reach + u/s pts just routed
ALLOCATE(Q_JRCH(0:NK),TENTRY(0:NK),T_EXIT(0:NK),STAT=IERR) ! include zero element for INTERP later
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem allocating array [getusq_rch.f90]')
       PRINT *, 'problem allocating array [getusq_rch.f90]'
       GETUSQ_RCH = 1
       RETURN
END IF
Q_JRCH(0:NJ) = KROUTE(JRCH)%KWAVE(0:NJ)%QF  ! extract the non-routed flow from reach JR
TENTRY(0:NJ) = KROUTE(JRCH)%KWAVE(0:NJ)%TI  ! extract the non-routed time from reach JR
T_EXIT(0:NJ) = KROUTE(JRCH)%KWAVE(0:NJ)%TR  ! extract the expected exit time
Q_JRCH(NJ+1:NJ+ND) = QD(1:ND)                    ! append u/s flow just routed downstream
TENTRY(NJ+1:NJ+ND) = TD(1:ND)                    ! append u/s time just routed downstream
T_EXIT(NJ+1:NJ+ND) = -9999.0D0                   ! set un-used T_EXIT to missing
DEALLOCATE(QD,TD,STAT=IERR)                      ! routed flow appended, no longer needed
IF (IERR.NE.0) THEN
  !CALL EXIT_TOPNET(1,'problem deallocating array [getusq_rch.f90]')
       PRINT *, 'problem deallocating array [getusq_rch.f90]'
       GETUSQ_RCH = 1
       RETURN
END IF
! ----------------------------------------------------------------------------------------
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
! ----------------------------------------------------------------------------------------
GETUSQ_RCH = 0
END FUNCTION GETUSQ_RCH
