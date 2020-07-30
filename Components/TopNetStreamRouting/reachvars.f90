MODULE reachvars
 USE nrtype
 use reachtype
 IMPLICIT NONE
 SAVE

 ! Individual flow particles
 ! type kreach for all stream segments
 TYPE(KREACH), DIMENSION(:), POINTER :: KROUTE
 TYPE(STRFLX), DIMENSION(:), POINTER :: RCHFLX  ! Reach fluxes
 TYPE(RCHPRP), DIMENSION(:), POINTER :: RPARAM   ! Reach Parameters
 TYPE(RCHTOPO), DIMENSION(:), POINTER  :: NETOPO    ! Network topology
 
 REAL(DP)                             :: DT          ! time step (seconds)
TYPE(MODFLX), DIMENSION(:), POINTER   :: BASFLX    ! hold the base flow
 INTEGER(I4B)                         :: MAXQPAR   ! max # flow particles allowed in each reach
 INTEGER(I4B)                         :: NRCH        ! Number of reaches

END MODULE reachvars
