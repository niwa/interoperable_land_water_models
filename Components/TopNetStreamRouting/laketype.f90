MODULE laketype
 USE nrtype
 IMPLICIT NONE
 ! Lake Parameters
 TYPE LAKPRP
  REAL(DP)                             :: AREAREF  ! lake reference area (m2)
  REAL(DP)                             :: ELEVREF  ! lake reference elevation (m amsl)
  REAL(DP)                             :: DEPHREF  ! lake reference depth (m)
  REAL(DP)                             :: SHAPE_M  ! lake shape parameter (Nilsson model)
  REAL(DP)                             :: HE2AR_C  ! water height-surface area parameter (Rupp model)
  REAL(DP)                             :: HE2AR_D  ! water height-surface area parameter (Rupp model)
  REAL(DP)                             :: HGHTLOW  ! minimum water height for discharge (m amsl)
  REAL(DP)                             :: HGHTECO  ! minimum height for ecological concerns (m amsl)
  REAL(DP)                             :: HGHTSPL  ! spillway height (m amsl)
  REAL(DP)                             :: DSCHECO  ! discharge at "ecological" height (m3 s-1)
  REAL(DP)                             :: DSCHSPL  ! discharge at spillway height (m3 s-1)
  REAL(DP)                             :: RATECVA  ! discharge rating curve parameter
  REAL(DP)                             :: RATECVB  ! discharge rating curve parameter
  LOGICAL(LGT)                         :: DSCHFUN  ! true if lake outflow is calculated using discharge function
  LOGICAL(LGT)                         :: EXIST    ! true if lake level and outflow can be modelled
 ENDTYPE LAKPRP
! TYPE(LAKPRP), DIMENSION(:), POINTER   :: LPARAM  ! Lake Parameters
 
 ! Lake topology
 TYPE LAKTOPO
  INTEGER(I4B)                         :: LAKE_IX   ! Lake index (0,1,2,...,nlak-1)
  INTEGER(I4B)                         :: LAKE_ID   ! Lake ID (REC code?)
  REAL(DP)                             :: LAKLAT1   ! Centroid latitude
  REAL(DP)                             :: LAKLAT2   ! Outlet latitude
  REAL(DP)                             :: LAKLON1   ! Centroid longitude
  REAL(DP)                             :: LAKLON2   ! Outlet longitude
  INTEGER(I4B)                         :: DREACHI   ! Downstream reach index
  INTEGER(I4B)                         :: DREACHK   ! Downstream reach ID
  INTEGER(I4B)                         :: DLAKE_I   ! Downstream lake index
  INTEGER(I4B)                         :: DLAKE_K   ! Downstream lake ID
  REAL(DP)                             :: BUTULAK   ! Outlet basin are under lake
  REAL(DP)                             :: RUTULAK   ! Outlet reach length under lake
 ENDTYPE LAKTOPO
! TYPE(LAKTOPO), DIMENSION(:), POINTER  :: LKTOPO  ! Lake topology
 ! Reach Parameter multipliers
 !TYPE MLAKPAR
 ! REAL(DP)                             :: MAREAREF
 ! REAL(DP)                             :: MELEVREF
 ! REAL(DP)                             :: MDEPHREF
 ! REAL(DP)                             :: MSHAPE_M
 ! REAL(DP)                             :: MHE2AR_C
 ! REAL(DP)                             :: MHE2AR_D
 ! REAL(DP)                             :: MHGHTLOW
 ! REAL(DP)                             :: MHGHTECO
 ! REAL(DP)                             :: MHGHTSPL
 ! REAL(DP)                             :: MDSCHECO
 ! REAL(DP)                             :: MDSCHSPL
 ! REAL(DP)                             :: MRATECVA
 ! REAL(DP)                             :: MRATECVB
 !ENDTYPE MLAKPAR
 !TYPE(MLAKPAR), DIMENSION(:), POINTER  :: MULPARM  ! Reach Parameters
 ! Model states for each lake
 TYPE LKSTATE
  REAL(DP)                              :: LAKELVL    ! water level (height) (m)
  REAL(DP)                              :: LAKEARE    ! surface area (m2)
  REAL(DP)                              :: LAKEVOL    ! lake h20 (m3)
  REAL(DP)                              :: ENSKAL     ! lake level update
  REAL(DP)                              :: LKLVLOR    ! lake level original
 ENDTYPE LKSTATE
! TYPE(LKSTATE), DIMENSION(:,:), POINTER :: LSTATE     ! Lake state (NENS,NLAK)
! TYPE(LKSTATE), DIMENSION(:), POINTER :: LSTATE     ! Lake state (NENS,NLAK)
 
 TYPE LKFLX
  REAL(DP)                             :: LAKE_Qav ! lake discharge (average over time step) (m3 s-1)
  REAL(DP)                             :: LAKE_Q ! lake discharge (instantaneous) (m3 s-1)
  REAL(DP)                             :: LAKE_P ! lake precipitation (m3)
  REAL(DP)                             :: LAKE_E ! lake evaporation (m3)
  REAL(DP)                             :: LAKE_I ! inflow to lake (m3 s-1)
 ENDTYPE LKFLX
! TYPE(LKFLX), DIMENSION(:), POINTER :: LAKFLX  ! Reach fluxes
! TYPE(LKFLX), DIMENSION(:,:), POINTER :: LAKFLX  ! Reach fluxes
END MODULE laketype
