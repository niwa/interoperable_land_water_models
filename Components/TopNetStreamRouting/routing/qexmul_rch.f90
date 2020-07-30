!SUBROUTINE QEXMUL_RCH(IENS,JRCH,ND,QD,TD,RSTEP)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Ross Woods, 2000 (two upstream reaches)
!   Martyn Clark, 2006 (generalize to multiple upstream reaches)
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to extract routed flow from a multiple reaches upstream of JRCH.  Merges flow
!    from multiple reaches into a single series.
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
!      ND   : Number of routed particles
!      QD(:): Vector of merged flow particles in reach JRCH
!      TD(:): Vector of times flow particles entered reach JRCH (exited upstream reaches)
!
! ----------------------------------------------------------------------------------------
FUNCTION QEXMUL_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,ND,QD,TD,RSTEP)
! ----------------------------------------------------------------------------------------
USE nrtype
!USE bastype
USE reachtype
IMPLICIT NONE
INTEGER(I4B)                                :: QEXMUL_RCH
! Input
REAL(DP),INTENT(IN)                         :: DT     !
REAL(DP),DIMENSION(0:1),INTENT(IN)          :: TBOUNDS     !
INTEGER(I4B), INTENT(IN)                    :: JRCH      ! reach to process
INTEGER(I4B), INTENT(IN)                    :: NRCH      ! number of reaches
TYPE(RCHTOPO), DIMENSION(NRCH),INTENT(IN)   :: NETOPO
TYPE(KREACH),DIMENSION(NRCH),INTENT(INOUT)  :: KROUTE
TYPE(RCHPRP),DIMENSION(NRCH),INTENT(IN)     :: RPARAM
TYPE(MODFLX), DIMENSION(NRCH),INTENT(IN)    :: BASFLX
INTEGER(I4B), INTENT(IN), OPTIONAL          :: RSTEP     ! retrospective time step offset
! Output
INTEGER(I4B), INTENT(OUT)                   :: ND        ! number of routed particles
REAL(DP), DIMENSION(:), POINTER             :: QD        ! flow particles just enetered JRCH
REAL(DP), DIMENSION(:), POINTER             :: TD        ! time flow particles entered JRCH

! Local variables to hold flow/time from upstream reaches
INTEGER(I4B)                                :: ROFFSET   ! retrospective offset due to rstep
INTEGER(I4B)                                :: IUPS      ! loop through u/s reaches
INTEGER(I4B)                                :: NUPB      ! number of upstream basins
INTEGER(I4B)                                :: NUPR      ! number of upstream reaches
INTEGER(I4B)                                :: INDX      ! index of the IUPS u/s reach
INTEGER(I4B)                                :: MUPR      ! # reaches u/s of IUPS u/s reach
INTEGER(I4B)                                :: NUPS      ! number of upstream elements
TYPE(KREACH), DIMENSION(:), POINTER, SAVE   :: USFLOW    ! waves for all upstream segments
REAL(DP), DIMENSION(:), ALLOCATABLE         :: UWIDTH    ! width of all upstream segments
INTEGER(I4B)                                :: IMAX      ! max number of upstream particles
INTEGER(I4B)                                :: IUPR      ! counter for reaches with particles
INTEGER(I4B)                                :: IR        ! index of the upstream reach
INTEGER(I4B)                                :: NS        ! size of  the wave
INTEGER(I4B)                                :: NR        ! # routed particles in u/s reach
INTEGER(I4B)                                :: NQ        ! NR+1, if non-routed particle exists
TYPE(FPOINT), DIMENSION(:), POINTER, SAVE   :: NEW_WAVE  ! temporary wave
LOGICAL(LGT),SAVE                           :: INIT=.TRUE. ! used to initialize pointers
! Local variables to merge flow
LOGICAL(LGT), DIMENSION(:), ALLOCATABLE     :: MFLG      ! T = all particles processed
INTEGER(I4B), DIMENSION(:), ALLOCATABLE     :: ITIM      ! processing point for all u/s segments
REAL(DP), DIMENSION(:), ALLOCATABLE         :: CTIME     ! central time for each u/s segment
INTEGER(I4B)                                :: JUPS      ! index of reach with the earliest time
REAL(DP)                                    :: Q_AGG     ! aggregarted flow at a given time
INTEGER(I4B)                                :: IWAV      ! index of particle in the IUPS reach
REAL(DP)                                    :: SCFAC     ! scale to conform to d/s reach width
REAL(DP)                                    :: SFLOW     ! scaled flow at CTIME(JUPS)
INTEGER(I4B)                                :: IBEG,IEND ! indices for particles that bracket time
REAL(DP)                                    :: SLOPE     ! slope for the interpolation
REAL(DP)                                    :: PREDV     ! value predicted by the interpolation
INTEGER(I4B)                                :: IPRT      ! counter for flow particles
INTEGER(I4B)                                :: JUPS_OLD  ! check that we don't get stuck in do-forever
INTEGER(I4B)                                :: ITIM_OLD  ! check that we don't get stuck in do-forever
INTEGER(I4B)                                :: IERR      ! error code (allocate statements)
REAL(DP)                                    :: TIME_OLD  ! previous time -- used to check for duplicates
REAL(DP), DIMENSION(:), POINTER, SAVE       :: QD_TEMP   ! flow particles just enetered JRCH
REAL(DP), DIMENSION(:), POINTER, SAVE       :: TD_TEMP   ! time flow particles entered JRCH
! Memory allocation check
INTEGER(I4B)                                :: ERR       ! error code for memory allocation
! ----------------------------------------------------------------------------------------
! (0) INITIALIZE POINTERS
! ----------------------------------------------------------------------------------------
IF(INIT) THEN
 INIT=.FALSE.
 NULLIFY(USFLOW,NEW_WAVE,QD_TEMP,TD_TEMP)
ENDIF
! set the retrospective offset
IF (.NOT.PRESENT(RSTEP)) THEN
  ROFFSET = 0
ELSE
  ROFFSET = RSTEP
END IF
! ----------------------------------------------------------------------------------------
! (1) DETERMINE THE NUMBER OF UPSTREAM REACHES
! ----------------------------------------------------------------------------------------
! Need to extract and merge the runoff from all upstream BASINS as well as the streamflow
!  from all upstream REACHES.  However, streamflow in headwater basins is undefined.  Thus
!  the number of series merged from upstream reaches is the number of upstream basins +
!  the number of upstream reaches that are not headwater basins.
NUPR = 0                               ! number of upstream reaches
NUPB = SIZE(NETOPO(JRCH)%UREACHI)      ! number of upstream basins
DO IUPS=1,NUPB
 INDX = NETOPO(JRCH)%UREACHI(IUPS)     ! index of the IUPS upstream reach
 MUPR = SIZE(NETOPO(INDX)%UREACHI)     ! # reaches upstream of the IUPS upstream reach
 IF (MUPR.GT.0) NUPR = NUPR + 1        ! reach has streamflow in it, so add that as well
END DO  ! iups
NUPS = NUPB + NUPR                     ! number of upstream elements (basins + reaches)
! if nups eq 1, then ** SPECIAL CASE ** of just one upstream basin that is a headwater
IF (NUPS.EQ.1) THEN
 ND = 1
 ALLOCATE(QD(1),TD(1),STAT=IERR)
 IF (IERR.NE.0) then
   !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
 END IF
 IR = NETOPO(JRCH)%UREACHI(1)
 QD(1) = BASFLX(IR)%INSTN_Q(1)
 TD(1) = TBOUNDS(1)
 RETURN
ENDIF
! allocate space for the upstream flow, time, and flags
ALLOCATE(USFLOW(NUPS),UWIDTH(NUPS),CTIME(NUPS),STAT=IERR)
IF (IERR.NE.0) then
  !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
END IF
  
! define the minimum size of the routed data structure (number of flow particles)
!  (IMAX is increased when looping through the reaches -- section 3 below)
IMAX = NUPB                            ! flow from basins (one particle / timestep)
! ----------------------------------------------------------------------------------------
! (2) EXTRACT FLOW FROM UPSTREAM BASINS
! ----------------------------------------------------------------------------------------
DO IUPS=1,NUPB
 ! identify the index for the IUPS upstream segment
 IR = NETOPO(JRCH)%UREACHI(IUPS)
 ! allocate space for the IUPS stream segment (flow, time, and flags)
 ALLOCATE(USFLOW(IUPS)%KWAVE(0:1),STAT=ERR)  ! basin, has flow @start and @end of the time step
 IF (ERR.GT.0) THEN
  print *, 'ALLOCATE(USFLOW(IUPS)%KWAVE(0:1),STAT=ERR): ERR = ', ERR
!  IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'ALLOCATE(USFLOW(IUPS)%KWAVE(0:1),STAT=ERR): ERR = ', ERR
!  CALL EXIT_TOPNET(1,'  [qexmul_rch.f90]')
  PRINT *, ' [qexmul_rch.f90]'
  qexmul_rch = 1
  RETURN
ENDIF
 ! place flow and time in the KWAVE array (routing done with time-delay histogram in TIMDEL_BAS.F90)
 USFLOW(IUPS)%KWAVE(0:1)%QF = BASFLX(IR)%INSTN_Q(0:1)                 ! flow
 USFLOW(IUPS)%KWAVE(0:1)%TI = TBOUNDS(0:1) - DT*ROFFSET ! entry time (not used)
 USFLOW(IUPS)%KWAVE(0:1)%TR = TBOUNDS(0:1) - DT*ROFFSET ! exit time
 USFLOW(IUPS)%KWAVE(0:1)%RF = .TRUE.                                       ! routing flag
 ! save the upstream width
 UWIDTH(IUPS) = 1.0D0                         ! basin = unit width
 ! save the the time for the first particle in each reach
 CTIME(IUPS) = USFLOW(IUPS)%KWAVE(1)%TR       ! central time
END DO    ! (loop through upstream basins)
! ----------------------------------------------------------------------------------------
! (3) EXTRACT FLOW FROM UPSTREAM REACHES
! ----------------------------------------------------------------------------------------
IUPR = 0
DO IUPS=1,NUPB
 INDX = NETOPO(JRCH)%UREACHI(IUPS)     ! index of the IUPS upstream reach
 MUPR = SIZE(NETOPO(INDX)%UREACHI)     ! # reaches upstream of the IUPS upstream reach
 IF (MUPR.GT.0) THEN                   ! reach has streamflow in it, so add that as well
  IUPR = IUPR + 1
  ! identify the index for the IUPS upstream segment
  IR = NETOPO(JRCH)%UREACHI(IUPS)
  ! identify the size of the wave
  NS = SIZE(KROUTE(IR)%KWAVE)
  ! identify number of routed flow elements in the IUPS upstream segment
  NR = COUNT(KROUTE(IR)%KWAVE(:)%RF)
  ! include a non-routed point, if it exists
  NQ = MIN(NR+1,NS)
  ! allocate space for the IUPS stream segment (flow, time, and flags)
  ALLOCATE(USFLOW(NUPB+IUPR)%KWAVE(0:NQ-1),STAT=IERR)  ! (zero position = last routed)
  IF (IERR.NE.0) then
    !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
  END IF
  ! place data in the new arrays
  USFLOW(NUPB+IUPR)%KWAVE(0:NQ-1) = KROUTE(IR)%KWAVE(0:NQ-1)
  ! here a statement where we check for a modification in the upstream reach;
  ! if flow upstream is modified, then copy KROUTE(:,:)%KWAVE(:)%QM to USFLOW(..)%KWAVE%QF
  !IF (NUSER.GT.0.AND.SIMDAT%UCFFLAG.GE.1) THEN !if the irrigation module is active and there are users
  !  IF (RCHFLX(IENS,IR)%TAKE.GT.0._DP) THEN !if take from upstream reach is greater then zero
  !    ! replace QF with modified flow (as calculated in extract_from_rch)
  !    USFLOW(NUPB+IUPR)%KWAVE(0:NQ-1)%QF = KROUTE(IR)%KWAVE(0:NQ-1)%QM
  !  ENDIF
  !ENDIF
  ! ...and REMOVE the routed particles from the upstream reach
  ! (copy the wave to a temporary wave)
  IF (ASSOCIATED(NEW_WAVE)) THEN
    DEALLOCATE(NEW_WAVE,STAT=IERR)    ! (so we can allocate)
    IF (IERR.NE.0) then
      !CALL EXIT_TOPNET(1,'problem deallocating array [qexmul_rch.f90]')
       PRINT *, 'problem deallocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
    END IF
  END IF
  ALLOCATE(NEW_WAVE(0:NS-1),STAT=IERR)                 ! get new wave
  IF (IERR.NE.0) then
    !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
  END IF   
  NEW_WAVE(0:NS-1) = KROUTE(IR)%KWAVE(0:NS-1)  ! copy
  ! (re-size wave structure)
  IF (.NOT.ASSOCIATED(KROUTE(IR)%KWAVE)) pause ' not associated. in qex '
  IF (ASSOCIATED(KROUTE(IR)%KWAVE)) THEN
    DEALLOCATE(KROUTE(IR)%KWAVE,STAT=IERR)
    IF (IERR.NE.0) then
      !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
    END IF
  END IF
  ALLOCATE(KROUTE(IR)%KWAVE(0:NS-NR),STAT=IERR)   ! reduced size
  IF (IERR.NE.0) then
    !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
  END IF
  ! (copy "last routed" and "non-routed" elements)
  KROUTE(IR)%KWAVE(0:NS-NR) = NEW_WAVE(NR-1:NS-1)
  ! (de-allocate temporary wave)
  DEALLOCATE(NEW_WAVE,STAT=IERR)
  IF (IERR.NE.0) then
    !CALL EXIT_TOPNET(1,'problem deallocating array [qexmul_rch.f90]')
       PRINT *, 'problem deallocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
  END IF
  NULLIFY(NEW_WAVE)
  ! save the upstream width
  UWIDTH(NUPB+IUPR) = RPARAM(IR)%R_WIDTH            ! reach, width = parameter
  ! save the time for the first particle in each reach
  CTIME(NUPB+IUPR) = USFLOW(NUPB+IUPR)%KWAVE(1)%TR  ! central time
  ! keep track of the total number of points that must be routed downstream
  IMAX = IMAX + (NR-1)     ! exclude zero point for the last routed
 ENDIF ! if reach has particles in it
END DO  ! iups
! ----------------------------------------------------------------------------------------
! (4) MERGE FLOW FROM MULTIPLE UPSTREAM REACHES
! ----------------------------------------------------------------------------------------
! This is a bit tricky.  Consider a given upstream reach x.  For all upstream reaches
!  *other than* x, we need to estimate (interpolate) flow for the *times* associted with
!  each of the flow particles in reach x.  Then, at a given time, we can sum the flow
!  (routed in reach x plus interpolated flow in all other reaches).  This needs to be done
!  for all upstream reaches.
! ----------------------------------------------------------------------------------------
! We accomplish this as follows.  We define a vector of indices (ITIM), where each
!  element of ITIM points to a particle in a given upstream reach still to be processed.
!  We also define a vector of times (CTIME), which is the time of the flow particles that
!  relate to the vector of indices ITIM.  We identify upstream reach with the earliest
!  time in CTIME, save the flow, and produce corresponding flow estimates for the same
!  time in all other reaches.  We then scale the flow and flow estimates in all upstream
!  reaches by the width of the downstream reach, and sum the flow over all upstream reaches.
!  We then move the index forward in ITIM (for the upstream reach just processed), get a
!  new vector CTIME, and process the next earliest particle.  We continue until all
!  flow particles are processed in all upstream reaches.
! ----------------------------------------------------------------------------------------
IPRT = 0  ! initialize counter for flow particles in the output array
! allocate space for the merged flow at the downstream reach
ALLOCATE(QD_TEMP(IMAX),TD_TEMP(IMAX),STAT=IERR)
IF (IERR.NE.0) then
  !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
END IF
! allocate positional arrays
ALLOCATE(MFLG(NUPS),ITIM(NUPS),STAT=IERR)
IF (IERR.NE.0) then
  !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
END IF
! initalize the flag that defines whether all particles in a given reach are processed
MFLG(1:NUPS)  = .FALSE.                     ! false until all particles are processed
! initialize the search vector
ITIM(1:NUPS)  = 1                           ! start with the first element of the wave
! initialize jups_old and itim_old (used to check we don't get stuck in the do-forever loop)
JUPS_OLD = HUGE(JUPS_OLD)
ITIM_OLD = HUGE(ITIM_OLD)
DO      ! loop through all the times in the upstream reaches until no more routed flows
 ! find the reach with the earliest time in all upstream reaches
 !  (NB: the time at the start of the timestep is the earliest possible time and
 !       the time at the end of the timestep is the latest possible time)
 JUPS  = MINLOC(CTIME,DIM=1)     ! JUPS = reach w/ earliest time
 ! check that we're not stuck in a continuous do loop
 IF (JUPS.EQ.JUPS_OLD .AND. ITIM(JUPS).EQ.ITIM_OLD) THEN
  print *, 'we are stuck in the continuous do-loop in qexmul_rch...'
  print *, '...something must have gone terribly wrong!'
  !CALL EXIT_TOPNET(1,'  [qexmul_rch.f90]')
  qexmul_rch = 1
  RETURN
ENDIF
 ! save jups and itim(jups) to check that we don't get stuck in a continuous do-loop
 JUPS_OLD = JUPS
 ITIM_OLD = ITIM(JUPS)
 ! check that there are still particles in the given reach that require processing
 IF (.NOT.MFLG(JUPS)) THEN
  ! check that the particle in question is a particle routed (if not, then don't process)
  IF (USFLOW(JUPS)%KWAVE(ITIM(JUPS))%RF.EQV..FALSE.) THEN
   MFLG(JUPS) = .TRUE. ! if routing flag is false, then have already processed all particles
   CTIME(JUPS) = HUGE(SFLOW)  ! largest possible number = ensure reach is not selected again
  ! the particle is in need of processing
  ELSE
   ! define previous time
   IF (IPRT.GE.1) THEN
     TIME_OLD = TD_TEMP(IPRT)
   ELSE ! (if no particles, set to largest possible negative number)
     TIME_OLD = -HUGE(SFLOW)
   END IF
   ! check that the particles are being processed in the correct order
   IF (CTIME(JUPS).LT.TIME_OLD) PAUSE ' [expect process in order of time, in qexmul_rch.f90] '
   ! don't process if time already exists
   IF (CTIME(JUPS).NE.TIME_OLD) THEN
    ! -------------------------------------------------------------------------------------
    ! compute sum of scaled flow for all reaches
    Q_AGG = 0.0D0
    DO IUPS=1,NUPS
     ! identify the element of the wave for the IUPS upstream reach
     IWAV = ITIM(IUPS)
     ! compute scale factor (scale upstream flow by width of downstream reach)
     SCFAC = UWIDTH(IUPS) / RPARAM(JRCH)%R_WIDTH
     ! case of the upstream reach with the minimum time (no interpolation required)
     IF (IUPS.EQ.JUPS) THEN
      SFLOW = USFLOW(IUPS)%KWAVE(IWAV)%QF * SCFAC  ! scaled flow
     ! case of all other upstream reaches (*** now, interpolate ***)
     ELSE
      ! identify the elements that bracket the flow particle in the reach JUPS
      ! why .GE.?  Why not .GT.??
      IBEG = IWAV; IF (USFLOW(IUPS)%KWAVE(IBEG)%TR.GE.CTIME(JUPS)) IBEG=IWAV-1
      IEND = IBEG+1  ! *** check the elements are ordered as we think ***
      ! test if we have bracketed properly
      IF (USFLOW(IUPS)%KWAVE(IEND)%TR.LT.CTIME(JUPS) .OR. &
          USFLOW(IUPS)%KWAVE(IBEG)%TR.GT.CTIME(JUPS)) THEN
           PRINT *, 'THE TIMES ARE NOT ORDERED AS WE THINK, IN QEXMUL_RCH...'
           PRINT *, IUPS, JRCH
           PRINT *, 'CTIME(JUPS) = ', CTIME(JUPS)
           PRINT *, USFLOW(IUPS)%KWAVE(:)%QF
           PRINT *, USFLOW(IUPS)%KWAVE(:)%TI
           PRINT *, USFLOW(IUPS)%KWAVE(:)%TR
           PRINT *, USFLOW(IUPS)%KWAVE(:)%RF
           !PAUSE
      ENDIF  ! test for bracketing
      ! estimate flow for the IUPS upstream reach at time CTIME(JUPS)
      SLOPE = (USFLOW(IUPS)%KWAVE(IEND)%QF - USFLOW(IUPS)%KWAVE(IBEG)%QF) / &
              (USFLOW(IUPS)%KWAVE(IEND)%TR - USFLOW(IUPS)%KWAVE(IBEG)%TR)
      PREDV =  USFLOW(IUPS)%KWAVE(IBEG)%QF + SLOPE*(CTIME(JUPS)-USFLOW(IUPS)%KWAVE(IBEG)%TR)
      SFLOW = PREDV * SCFAC  ! scaled flow
     ENDIF  ! (if interpolating)
     ! aggregate flow
     Q_AGG = Q_AGG + SFLOW
    END DO  ! looping through upstream elements
    ! -------------------------------------------------------------------------------------
    ! place Q_AGG and CTIME(JUPS) in the output arrays
    IPRT = IPRT + 1
    QD_TEMP(IPRT) = Q_AGG
    TD_TEMP(IPRT) = CTIME(JUPS)
   ENDIF  ! (check that time doesn't already exist)
   ! check if the particle just processed is the last element
   IF (ITIM(JUPS).EQ.SIZE(USFLOW(JUPS)%KWAVE)-1) THEN  ! -1 because of the zero element
    MFLG(JUPS) = .TRUE.            ! have processed all particles in a given u/s reach
    CTIME(JUPS) = HUGE(SFLOW)      ! largest possible number = ensure reach is not selected again
   ELSE
    ITIM(JUPS) = ITIM(JUPS) + 1                       ! move on to the next flow element
    CTIME(JUPS) = USFLOW(JUPS)%KWAVE(ITIM(JUPS))%TR   ! save the time
   ENDIF  ! (check if particle is the last element)
  ENDIF  ! (check if the particle is a routed element)
 ENDIF  ! (check that there are still particles to process)
 ! if processed all particles in all upstream reaches, then EXIT
 IF (COUNT(MFLG).EQ.NUPS) EXIT
END DO   ! do-forever
! free up memory
DO IUPS=1,NUPS  ! de-allocate each element of USFLOW
 DEALLOCATE(USFLOW(IUPS)%KWAVE,STAT=IERR)
 IF (IERR.NE.0) then
 !  CALL EXIT_TOPNET(1,'problem deallocating array [qexmul_rch.f90]')
       PRINT *, 'problem deallocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
 END IF
END DO          ! looping thru elements of USFLOW
DEALLOCATE(USFLOW,UWIDTH,CTIME,ITIM,MFLG,STAT=IERR)
IF (IERR.NE.0) then
  !CALL EXIT_TOPNET(1,'problem deallocating array [qexmul_rch.f90]')
       PRINT *, 'problem deallocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
END IF
! ...and, save reduced arrays in QD and TD
ND = IPRT
ALLOCATE(QD(ND),TD(ND),STAT=IERR)
IF (IERR.NE.0) then
  !CALL EXIT_TOPNET(1,'problem allocating array [qexmul_rch.f90]')
       PRINT *, 'problem allocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
END IF
QD(1:ND) = QD_TEMP(1:ND)
TD(1:ND) = TD_TEMP(1:ND)
DEALLOCATE(QD_TEMP,TD_TEMP,STAT=IERR)
IF (IERR.NE.0) then
  !CALL EXIT_TOPNET(1,'problem deallocating array [qexmul_rch.f90]')
       PRINT *, 'problem deallocating array [qexmul_rch.f90]'
       qexmul_rch = 1
       RETURN
END IF
! ----------------------------------------------------------------------------------------
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
! ----------------------------------------------------------------------------------------
QEXMUL_RCH = 0
END FUNCTION QEXMUL_RCH
