MODULE lakevars
 USE nrtype
 use laketype
 IMPLICIT NONE
 SAVE
 INTEGER(I4B)                          :: LAKEFLAG    ! lake flag
 INTEGER(I4B)                          :: NLAK        ! number of lakes
 
 TYPE(LAKPRP), DIMENSION(:), POINTER   :: LPARAM  ! Lake Parameters
 
 TYPE(LAKTOPO), DIMENSION(:), POINTER  :: LKTOPO  ! Lake topology
 TYPE(LKSTATE), DIMENSION(:), POINTER  :: LSTATE     ! Lake state (NENS,NLAK)
 TYPE(LKFLX), DIMENSION(:), POINTER    :: LAKFLX  ! Reach fluxes
END MODULE lakevars
