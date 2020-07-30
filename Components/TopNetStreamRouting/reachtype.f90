MODULE reachtype
 USE nrtype
 IMPLICIT NONE
 ! --------------------------------------------------------------------------------------
 ! Collection of flow particles in each reach (Kinematic wave routing)
 ! --------------------------------------------------------------------------------------
 ! Individual flow particles
 TYPE FPOINT
  REAL(DP)                             :: QF       ! Flow
  REAL(DP)                             :: QM       ! Modified flow
  REAL(DP)                             :: TI       ! initial time of point in reach
  REAL(DP)                             :: TR       ! time point expected to exit reach
  LOGICAL(LGT)                         :: RF       ! routing flag (T if point has exited)
 END TYPE FPOINT
 ! Collection of flow points within a given reach
 TYPE KREACH
  TYPE(FPOINT), DIMENSION(:), POINTER  :: KWAVE
 END TYPE KREACH
 ! type kreach for all stream segments
 !TYPE(KREACH), DIMENSION(:,:), POINTER :: KROUTE
 !TYPE(KREACH), DIMENSION(:), POINTER :: KROUTE
 ! --------------------------------------------------------------------------------------
 
  TYPE STRFLX
  REAL(DP)                             :: REACH_Q ! time-step average streamflow
  REAL(DP)                             :: TAKE    ! average take
  ENDTYPE STRFLX
! TYPE(STRFLX), DIMENSION(:), POINTER :: RCHFLX  ! Reach fluxes
! TYPE(STRFLX), DIMENSION(:,:), POINTER :: RCHFLX  ! Reach fluxes

  ! Reach Parameters
 TYPE RCHPRP
  REAL(DP)                             :: R_SLOPE
  REAL(DP)                             :: R_MAN_N
  REAL(DP)                             :: R_WIDTH
  REAL(DP)                             :: RLENGTH
  REAL(DP)                             :: TOTAREA
  REAL(DP)                             :: MINFLOW  ! minimum environmental flow
 ENDTYPE RCHPRP
! TYPE(RCHPRP), DIMENSION(:), POINTER   :: RPARAM   ! Reach Parameters
 
 ! Network topology
 TYPE RCHTOPO
  INTEGER(I4B)                         :: REACHIX   ! Reach index (0,1,2,...,nrch-1)
  INTEGER(I4B)                         :: REACHID   ! Reach ID (REC code)
  REAL(DP)                             :: RCHLAT1   ! Start latitude
  REAL(DP)                             :: RCHLAT2   ! End latitude
  REAL(DP)                             :: RCHLON1   ! Start longitude
  REAL(DP)                             :: RCHLON2   ! End longitude
  INTEGER(I4B)                         :: DREACHI   ! Downstream reach index
  INTEGER(I4B)                         :: DREACHK   ! Downstream reach ID
  INTEGER(I4B), DIMENSION(:), POINTER  :: UREACHI   ! Upstream reach indices
  INTEGER(I4B), DIMENSION(:), POINTER  :: UREACHK   ! Upstream reach IDs
  INTEGER(I4B)                         :: RHORDER   ! Processing sequence
  INTEGER(I4B), DIMENSION(:), POINTER  :: RCHLIST   ! List of reaches upstream
  INTEGER(I4B)                         :: LAKE_IX   ! Lake index (0,1,2,...,nlak-1)
  INTEGER(I4B)                         :: LAKE_ID   ! Lake ID (REC code?)
  REAL(DP)                             :: BASULAK   ! Area of basin under lake
  REAL(DP)                             :: RCHULAK   ! Length of reach under lake
  LOGICAL(LGT)                         :: LAKINLT   ! .TRUE. if reach is lake inlet, .FALSE. otherwise
  LOGICAL(LGT)                         :: USRTAKE   ! .TRUE. if user takes from reach, .FALSE. otherwise
 ENDTYPE RCHTOPO
 !TYPE(RCHTOPO), DIMENSION(:), POINTER  :: NETOPO    ! Network topology
 
 TYPE MODFLX
  REAL(DP)                             :: POTEVAP       ! Potential ET demand (canopy forcing)     
  REAL(DP),DIMENSION(0:1)              :: INSTN_Q
 END TYPE MODFLX
 !TYPE(Basinflx), DIMENSION(:), POINTER   :: BASFLX    ! hold the base flow
 ! Reach Parameter multipliers
 !TYPE MRCHPAR
 ! REAL(DP)                             :: MR_SLOPE
 ! REAL(DP)                             :: MR_MAN_N
 ! REAL(DP)                             :: MR_WIDTH
 ! REAL(DP)                             :: MRLENGTH
 ! REAL(DP)                             :: MTOTAREA
 !ENDTYPE MRCHPAR
 !TYPE(MRCHPAR), DIMENSION(:,:), POINTER  :: MURPARM  ! Reach Parameters
END MODULE reachtype
