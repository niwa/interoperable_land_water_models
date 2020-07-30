!module init
!    contains
!subroutine print_value()
!!DEC$ ATTRIBUTES DLLEXPORT::print_value
!use reachvars
!write(*,*)NETOPO(26)%UREACHI(2)
!end subroutine

!function SPLDAT_GET(ROUTFLAG,LAKEFLAG,NRCH,NLAK,NETOPO,RPARAM,LKTOPO,LPARAM)
!function SPLDAT_GET(ROUTFLAG,LAKEFLAG,NRCH,NLAK,ncSpatial)
function SPLDAT_GET(ROUTFLAG,ncSpatial)
! ----------------------------------------------------------------------------------------
! Creator(s):
!   Martyn Clark, 2006
!   Lakes added by David Rupp, 2006
!   Added RPARAM(:)%MINFLOW, GT, 2007
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Used to read spatially-distributed model parameters from NetCDF file and store
!    parameters in data structures
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   INPUT:
!      NENS: Number of ensemble members
!
!   OUTPUT:
!      NRCH: Number of stream segments in the river network (reaches)
!      NLAK: Number of lakes in river network
!
! ----------------------------------------------------------------------------------------
! Structures Populated:
!
!   (1) Model Parameters
!   --------------------
!   BPARAM(NBAS)% [PARAMETER NAME] = model parameters for each sub-basin
!
!   (2) Stream Network
!   ------------------
!   NETOPO(NRCH)% [INDICES OF STREAM SEGMENTS] = network topology
!   RPARAM(NRCH)% [PARAMETER NAME] = parameters for each stream segment
!
!   (3) Basin initial states
!   ------------------------
!   BSTATE(NBAS)% [STATE] = intial states
!   LPARAM(NLAK)% [PARAMETER NAME] = parameters for each lake
!
! ----------------------------------------------------------------------------------------
! Future revisions:
!
!   (none planned)
!
! ----------------------------------------------------------------------------------------
USE nrtype                                                ! variable types (DP, I4B, etc.)
use reachvars
use lakevars
!USE reachparam                                            ! reach parameters
!USE reachstate                                            ! reach states (flow particles)
!USE reach_flux                                            ! reach fluxes (streamflow)
!USE lake_param                                            ! lake and reservoir parameters
USE netcdf                                                ! netCDF subroutines
IMPLICIT NONE

INTEGER(I4B)                                :: SPLDAT_GET

! Output variables -- NetCDF dimensions
INTEGER(I4B), INTENT(IN)               :: ROUTFLAG            ! if routing will be done
!INTEGER(I4B), INTENT(INOUT)            :: LAKEFLAG            ! if lake will be simulated
!INTEGER(I4B), INTENT(OUT)              :: NRCH            ! number of reaches
!INTEGER(I4B), INTENT(OUT)              :: NLAK            ! number of lakes
character(len=LEN_PATH), intent(in)    :: ncSpatial
!TYPE(RCHTOPO), DIMENSION(:),POINTER, INTENT(OUT)   :: NETOPO
!TYPE(RCHPRP),DIMENSION(:),POINTER,INTENT(OUT)     :: RPARAM
!TYPE(LAKTOPO), DIMENSION(:),POINTER,INTENT(OUT)   :: LKTOPO
!TYPE(LAKPRP), DIMENSION(:), POINTER,INTENT(OUT)   :: LPARAM
!TYPE(RCHTOPO), DIMENSION(:),POINTER   :: NETOPO
!TYPE(RCHPRP),DIMENSION(:),POINTER     :: RPARAM
!TYPE(LAKTOPO), DIMENSION(:),POINTER   :: LKTOPO
!TYPE(LAKPRP), DIMENSION(:), POINTER   :: LPARAM

! Looping variables
INTEGER(I4B)                           :: IBAS            ! loop through basins
INTEGER(I4B)                           :: IRCH,JRCH,KRCH,URCH  ! loop through reaches
INTEGER(I4B)                           :: IDIS            ! loop through pts in prob disns
INTEGER(I4B)                           :: ILAK,DLAK,ULAK  ! loop through lakes
! Global NetCDF variables
!CHARACTER(LEN=LEN_FILE_PATH)           :: SPATIAL_PATH=''    ! path for spatial data file
!CHARACTER(LEN=120)           :: SPATIAL_FILE=''    ! name of spatial data file
INTEGER(I4B)                           :: IERR            ! NetCDF error code
INTEGER(I4B)                           :: NCID            ! NetCDF file ID
INTEGER(I4B)                           :: IDIMID          ! NetCDF dimension ID
INTEGER(I4B)                           :: IVARID          ! NetCDF variable ID
INTEGER(I4B),DIMENSION(1)              :: TMPINT          ! netcdf data array
INTEGER(I4B),DIMENSION(1)              :: POS1            ! 1-d array index
INTEGER(I4B),DIMENSION(2)              :: POS2            ! 2-d array index
! (1) Spatial model parameters + parameter distributions (NB: populate structure BPARAM)
!REAL(DP),DIMENSION(:),ALLOCATABLE      :: TMPBAS          ! data vector - basin parameters
!INTEGER(I4B)                           :: NUMATAN         ! num points in a/tan(b)
!INTEGER(I4B)                           :: NUMOVER         ! num points in overland q
!INTEGER(I4B)                           :: NUMELEV         ! num points in elevation distribution
!REAL(DP),DIMENSION(1)                  :: ATANVAL,ATANFRQ ! value; freq in a/tan(b)
!REAL(DP),DIMENSION(1)                  :: OVERVAL,OVERFRQ ! value; freq in overland q
!REAL(DP),DIMENSION(1)                  :: ELEVVAL,ELEVFRQ ! value; freq in elevation distribution
!REAL(DP)                               :: OLDVAL          ! previous value in cumul. prob.
!REAL(DP)                               :: OLDFRQ          ! convert cum freq->probability
REAL(DP)                               :: SFAC_VAL        ! scale factor: value of prob distn
REAL(DP)                               :: SFAC_FRQ        ! scale factor: freq in prob distn
REAL(DP),DIMENSION(:),ALLOCATABLE      :: BASAREA          ! data vector - BASIN AREA
! (2) Network topology + reach parameters (NB: populate structures "NETOPO" and "RPARAM")
INTEGER(I4B),DIMENSION(1)              :: NWRCHID         ! reach id (net topo)
REAL(DP),DIMENSION(1)                  :: RCHLAT1         ! start latitude
REAL(DP),DIMENSION(1)                  :: RCHLAT2         ! end latitude
REAL(DP),DIMENSION(1)                  :: RCHLON1         ! start longitude
REAL(DP),DIMENSION(1)                  :: RCHLON2         ! end longitude
INTEGER(I4B),DIMENSION(1)              :: NWDRCHI         ! downstream reach index
INTEGER(I4B),DIMENSION(1)              :: NWDRCHK         ! downstream reach id
INTEGER(I4B)                           :: NUMBUPS         ! number of upstream reaches
INTEGER(I4B),DIMENSION(1)              :: NWURCHI         ! upstream reach index
INTEGER(I4B),DIMENSION(1)              :: NWURCHK         ! upstream reach id
REAL(DP),DIMENSION(:),ALLOCATABLE      :: TMPRCH          ! data vector - reach parameters
! (3) Lake topology + lake parameters (NB: populate structures "LKTOPO" and "LPARAM")
INTEGER(I4B),DIMENSION(1)              :: NWLAKID         ! lake id (lake topo)
INTEGER(I4B),DIMENSION(1)              :: NWLAKIX         ! lake index (lake topo)
REAL(DP),DIMENSION(1)                  :: LAKLAT1         ! centroid latitude
REAL(DP),DIMENSION(1)                  :: LAKLAT2         ! outled latitude
REAL(DP),DIMENSION(1)                  :: LAKLON1         ! centroid longitude
REAL(DP),DIMENSION(1)                  :: LAKLON2         ! outlet longitude
REAL(DP),DIMENSION(1)                  :: BASULAK         ! basin area under lake
REAL(DP),DIMENSION(1)                  :: RCHULAK         ! reach length under lake
REAL(DP),DIMENSION(:),ALLOCATABLE      :: TMPLAK          ! data vector - lake parameters
! (4) Basin initial states (NB: populate structure "BSTATE")
!       (populate structure)
! (5) Global attributes
CHARACTER(LEN=50)                      :: TMPTEXT         ! temporary text
CHARACTER(LEN=100)                     :: DESTEXT         ! descriptive text
INTEGER(I4B)                           :: NLRCH           ! number of reaches associated with lake
INTEGER(I4B)                           :: ILEN            ! length of the basin name
! (6) Other
LOGICAL(LGT)                           :: FATAL           ! flag for fatal/non-fatal error
INTEGER(I4B)                           :: I               ! string index
INTEGER(I4B)                           :: SLEN            ! string length
INTEGER(I4B)                           :: LEN_TRIM_NULL   ! function
LOGICAL(LGT),DIMENSION(:),ALLOCATABLE  :: PMASK           ! reach processing mask
! ----------------------------------------------------------------------------------------
! OPEN FILE
! ----------------------------------------------------------------------------------------
write(*,*) 'I am right here'
!SPATIAL_FILE = '../ancillary_data/topnet/spatial_rec_13066888_strahler3.nc'
 IERR = NF90_OPEN(TRIM(ncSPATIAL),NF90_NOWRITE,NCID); !CALL HANDLE_ERR90(IERR)
 if(IERR.NE.NF90_NOERR)then
    SPLDAT_GET  = -1;
    return
 end if
 ! ---------------------------------------------------------------------------------------
 ! (1) GET DIMENSIONS
 ! ---------------------------------------------------------------------------------------
 ! get the number of reaches (stream segments) = number of basins
 IERR = NF90_INQ_DIMID(NCID,'nrch',IDIMID); !CALL HANDLE_ERR90(IERR)
 if(IERR.NE.NF90_NOERR)then
    SPLDAT_GET  = -1;
    return
 end if
 IERR = NF90_INQUIRE_DIMENSION(NCID,IDIMID,LEN=NRCH) ; !CALL HANDLE_ERR90(IERR)
 if(IERR.NE.NF90_NOERR)then
    SPLDAT_GET  = -1;
    return
 end if
 ! get the number of lakes
 IF (LAKEFLAG.GT.0) THEN
  IERR = NF90_INQ_DIMID(NCID,'nlake',IDIMID)
  IF (IERR.EQ.0) THEN
   IERR = NF90_INQUIRE_DIMENSION(NCID,IDIMID,LEN=NLAK); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1;
     return
   end if
  ! set lakes to false if there are no lakes in the ancillary data file
  ELSE
   NLAK=0
   LAKEFLAG=0
  ENDIF
 ELSE
  NLAK = 0
 ENDIF
 ! ---------------------------------------------------------------------------------------
 ! (2) INITIALIZE ARRAYS
 ! ---------------------------------------------------------------------------------------
 ! de-allocate the basin parameters
 ! deallocate the network topology
 ! deallocate the reach parameters
  ! deallocate the lake topology
 ! deallocate the lake parameters
 ! allocate arrays
 ALLOCATE(NETOPO(NRCH),STAT=IERR)      ! in MODULE reachparam
 IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
 end if
 ALLOCATE(RPARAM(NRCH),STAT=IERR)      ! in MODULE reachparam
 IF (IERR.NE.0) then
 !  CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
 end if
 ALLOCATE(LKTOPO(NLAK),STAT=IERR)      ! in MODULE reachparam
 IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
 end if
 ALLOCATE(LPARAM(NLAK),STAT=IERR)      ! in MODULE reachparam
 IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
 end if
 ! nullify structure components (Jing)
 !DO IRCH=1,NRCH
 ! NULLIFY(NETOPO(IRCH)%UREACHI)
 ! NULLIFY(NETOPO(IRCH)%UREACHK)
 ! NULLIFY(NETOPO(IRCH)%RCHLIST)
 !END DO
 ! ---------------------------------------------------------------------------------------
 ! (3) GET THE SPATIAL MODEL PARAMETERS -- POPULATE STRUCTURE "MODPARM"
 ! ---------------------------------------------------------------------------------------
 ALLOCATE(BASAREA(NRCH),STAT=IERR)  ! temporary array
 ALLOCATE(TMPRCH(NRCH),STAT=IERR)  ! temporary array
 IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
  end if
 ! ---------------------------------------------------------------------------------------
 ! (3a) get common basin parameters
 FATAL = .TRUE.
 SPLDAT_GET = NETCDF_GET(NCID,'basarea',NRCH,BASAREA,FATAL); IF(SPLDAT_GET < 0) RETURN
 SPLDAT_GET = NETCDF_GET(NCID,'uparea', NRCH,TMPRCH,FATAL); IF(SPLDAT_GET < 0) RETURN; RPARAM(1:NRCH)%TOTAREA = TMPRCH(1:NRCH)
! ---------------------------------------------------------------------------------------
 DO IRCH=1,NRCH  ! loop through reaches and get reach indices and REC IDs
  POS1(1) = IRCH
  ! NETWORK TOPOLOGY -- reach index
  IERR = NF90_INQ_VARID(NCID,'rchindex',IVARID); !CALL HANDLE_VAR_ERR90('rchindex',IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  IERR = NF90_GET_VAR(NCID,IVARID,NWRCHID,POS1); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  IF (NWRCHID(1).GE.0) THEN
    NETOPO(IRCH)%REACHIX = NWRCHID(1)+1
  ELSE
    NETOPO(IRCH)%REACHIX = NWRCHID(1)
  END IF
  ! NETWORK TOPOLOGY -- reach ID
  IERR = NF90_INQ_VARID(NCID,'rchid',IVARID); !CALL HANDLE_VAR_ERR90('rchid',IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  IERR = NF90_GET_VAR(NCID,IVARID,NWRCHID,POS1); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  NETOPO(IRCH)%REACHID = NWRCHID(1)
  ! NETWORK TOPOLOGY -- downstream reach index
  IERR = NF90_INQ_VARID(NCID,'dsrch_nrch',IVARID); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  IERR = NF90_GET_VAR(NCID,IVARID,NWDRCHI,POS1); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  IF (NWDRCHI(1).GE.0) THEN
    NETOPO(IRCH)%DREACHI = NWDRCHI(1)+1
  ELSE
    NETOPO(IRCH)%DREACHI = NWDRCHI(1)
  END IF
  ! NETWORK TOPOLOGY -- downstream reach ID ( NB: DREACHK = RCHID[DREACHI] )
  IERR = NF90_INQ_VARID(NCID,'dsrch_rchid',IVARID); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  IERR = NF90_GET_VAR(NCID,IVARID,NWDRCHK,POS1); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
  end if
  NETOPO(IRCH)%DREACHK = NWDRCHK(1)
 END DO ! (end looping through reaches
! ! save STD_LON (need to compute local time)
! MODTIM%STD_LON = SUM(BPARAM(:)%BAS_LON) / REAL(NRCH,KIND(TMPBAS))
! ! save STD_LAT (need for radiation computations)
! MODTIM%STD_LAT = SUM(BPARAM(:)%BAS_LAT) / REAL(NRCH,KIND(TMPBAS))
  ! (get elevation distribution)
 ! ---------------------------------------------------------------------------------------
 ! (3b) get canopy parameters
 ! (3c) get snow parameters
 ! (3d) get soil parameters
 ! (3d) get basin routing parameters
 ! (4) GET THE NETWORK TOPOLOGY AND REACH PARAMETERS (POPULATE STRUCTURES NETOPO + RPARAM)
 ! ---------------------------------------------------------------------------------------
 IF (ROUTFLAG.GE.1) THEN  ! (if we're computing network routing)
  DO IRCH=1,NRCH
   POS1(1) = IRCH
   ! NETWORK TOPOLOGY -- start lat
   IERR = NF90_INQ_VARID(NCID,'start_lat',IVARID); !CALL HANDLE_VAR_ERR90('start_lat',IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,RCHLAT1,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_ATT(NCID,IVARID,'scale_factor',SFAC_VAL); IF (IERR.NE.0) SFAC_VAL = 1.0D0
   NETOPO(IRCH)%RCHLAT1 = RCHLAT1(1)*SFAC_VAL
   ! NETWORK TOPOLOGY -- end lat
   IERR = NF90_INQ_VARID(NCID,'end_lat',IVARID); !CALL HANDLE_VAR_ERR90('end_lat',IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,RCHLAT2,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_ATT(NCID,IVARID,'scale_factor',SFAC_VAL); IF (IERR.NE.0) SFAC_VAL = 1.0D0
   NETOPO(IRCH)%RCHLAT2 = RCHLAT2(1)*SFAC_VAL
   ! NETWORK TOPOLOGY -- start lon
   IERR = NF90_INQ_VARID(NCID,'start_lon',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,RCHLON1,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_ATT(NCID,IVARID,'scale_factor',SFAC_VAL); IF (IERR.NE.0) SFAC_VAL = 1.0D0
   NETOPO(IRCH)%RCHLON1 = RCHLON1(1)*SFAC_VAL
   ! NETWORK TOPOLOGY -- end lon
   IERR = NF90_INQ_VARID(NCID,'end_lon',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,RCHLON2,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_ATT(NCID,IVARID,'scale_factor',SFAC_VAL); IF (IERR.NE.0) SFAC_VAL = 1.0D0
   NETOPO(IRCH)%RCHLON2 = RCHLON2(1)*SFAC_VAL
   ! NETWORK TOPOLOGY -- number of upstream reaches (just allocate space)
   IERR = NF90_INQ_VARID(NCID,'numuprch',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,TMPINT,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
   end if
   NUMBUPS = TMPINT(1)
   IF (LAKEFLAG.GT.0) THEN
    ! NETWORK TOPOLOGY WITH LAKES -- associated lake index
    IERR = NF90_INQ_VARID(NCID,'rch_nlake',IVARID); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,NWLAKID,POS1); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    IF (NWLAKID(1).GE.0) THEN
      NETOPO(IRCH)%LAKE_IX = NWLAKID(1)+1
    ELSE
      NETOPO(IRCH)%LAKE_IX = NWLAKID(1)
    END IF
    ! NETWORK TOPOLOGY WITH LAKES -- associated lake id
    IERR = NF90_INQ_VARID(NCID,'rch_lakeid',IVARID); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,NWLAKID,POS1); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    NETOPO(IRCH)%LAKE_ID = NWLAKID(1)
    ! NETWORK TOPOLOGY WITH LAKES -- basin area under lake (m2)
    IERR = NF90_INQ_VARID(NCID,'rch_lake_area',IVARID); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,BASULAK,POS1); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    NETOPO(IRCH)%BASULAK = BASULAK(1)
    ! NETWORK TOPOLOGY WITH LAKES -- reach length under lake (m)
    IERR = NF90_INQ_VARID(NCID,'rch_lake_length',IVARID); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,RCHULAK,POS1); !CALL HANDLE_ERR90_2(IERR)
    if(IERR.NE.NF90_NOERR)then
       SPLDAT_GET  = -1; return
    end if
    NETOPO(IRCH)%RCHULAK = RCHULAK(1)
   ENDIF
   ALLOCATE(NETOPO(IRCH)%UREACHI(NUMBUPS),NETOPO(IRCH)%UREACHK(NUMBUPS),STAT=IERR) !Jing
   IF (IERR.NE.0) then
!!     CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
   end if
   DO JRCH=1,NUMBUPS
    POS2 = (/JRCH,IRCH/)
    ! NETWORK TOPOLOGY -- indices of upstream reaches
    IERR = NF90_INQ_VARID(NCID,'uprch_nrch',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,NWURCHI,POS2); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
    end if
    IF (NWURCHI(1).GE.0) THEN
      NETOPO(IRCH)%UREACHI(JRCH) = NWURCHI(1)+1
    ELSE
      NETOPO(IRCH)%UREACHI(JRCH) = NWURCHI(1)
    END IF
    ! NETWORK TOPOLOGY -- reach ID for upstream reaches
    IERR = NF90_INQ_VARID(NCID,'uprch_rchid',IVARID); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
    end if
    IERR = NF90_GET_VAR(NCID,IVARID,NWURCHK,POS2); !CALL HANDLE_ERR90(IERR)
    if(IERR.NE.NF90_NOERR)then
      SPLDAT_GET  = -1; return
    end if
    NETOPO(IRCH)%UREACHK(JRCH) = NWURCHK(1)
   END DO  ! jrch
  END DO  ! irch
  SPLDAT_GET = NETCDF_GET(NCID,'rchslope',  NRCH,TMPRCH,FATAL); if(SPLDAT_GET < 0) return; RPARAM(1:NRCH)%R_SLOPE = TMPRCH(1:NRCH)
  SPLDAT_GET = NETCDF_GET(NCID,'rchman_n',  NRCH,TMPRCH,FATAL); if(SPLDAT_GET < 0) return; RPARAM(1:NRCH)%R_MAN_N = TMPRCH(1:NRCH)
  SPLDAT_GET = NETCDF_GET(NCID,'rchwidth',  NRCH,TMPRCH,FATAL); if(SPLDAT_GET < 0) return; RPARAM(1:NRCH)%R_WIDTH = TMPRCH(1:NRCH)
  SPLDAT_GET = NETCDF_GET(NCID,'rchlength', NRCH,TMPRCH,FATAL); if(SPLDAT_GET < 0) return; RPARAM(1:NRCH)%RLENGTH = TMPRCH(1:NRCH)
  ! For now not fatal, only used by user controlled flow.
  ! CALL NETCDF_GET(NCID,'rch_minflow', NRCH,TMPRCH,.FALSE.); RPARAM(1:NRCH)%MINFLOW = TMPRCH(1:NRCH)
  DEALLOCATE(TMPRCH,STAT=IERR)
  IF (IERR.NE.0) then
!    CALL EXIT_TOPNET(1,'problem deallocating array [spldat_get.f90]')
        SPLDAT_GET = -1
       return
  end if
ENDIF  ! (if we're computing network routing)
 ! ---------------------------------------------------------------------------------------
 ! (5) GET THE LAKE TOPOLOGY AND LAKE PARAMETERS (POPULATE STRUCTURES LKTOPO + LPARAM)
 ! ---------------------------------------------------------------------------------------
 IF (LAKEFLAG.GT.0) THEN
  DO ILAK=1,NLAK
   POS1(1) = ILAK
   ! LAKE TOPOLOGY -- lake index (LID code)
   IERR = NF90_INQ_VARID(NCID,'lakeindex',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,NWLAKIX,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IF (NWLAKIX(1).GE.0) THEN
     LKTOPO(ILAK)%LAKE_IX = NWLAKIX(1) + 1
   ELSE
     LKTOPO(ILAK)%LAKE_IX = NWLAKIX(1)
   END IF
   ! LAKE TOPOLOGY -- lake ID (LID code)
   IERR = NF90_INQ_VARID(NCID,'lakeid',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,NWLAKID,POS1);! CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%LAKE_ID = NWLAKID(1)
   ! LAKE TOPOLOGY -- centroid lat
   IERR = NF90_INQ_VARID(NCID,'lk_cen_lat',IVARID); !CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,LAKLAT1,POS1); !CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%LAKLAT1 = LAKLAT1(1)
   ! LAKE TOPOLOGY -- outlet lat
   IERR = NF90_INQ_VARID(NCID,'lk_out_lat',IVARID);!CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,LAKLAT2,POS1); !CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%LAKLAT2 = LAKLAT2(1)
   ! LAKE TOPOLOGY -- centroid lon
   IERR = NF90_INQ_VARID(NCID,'lk_cen_lon',IVARID); !CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,LAKLON1,POS1); !CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%LAKLON1 = LAKLON1(1)
   ! LAKE TOPOLOGY -- outlet lon
   IERR = NF90_INQ_VARID(NCID,'lk_out_lon',IVARID);! CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,LAKLON2,POS1); !CALL HANDLE_ERR90_2(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%LAKLON2 = LAKLON2(1)
   ! LAKE TOPOLOGY -- downstream reach index
   IERR = NF90_INQ_VARID(NCID,'lk_ds_nrch',IVARID);! CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,NWDRCHI,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IF (NWDRCHI(1).GE.0) THEN
     LKTOPO(ILAK)%DREACHI = NWDRCHI(1)+1
   ELSE
     LKTOPO(ILAK)%DREACHI = NWDRCHI(1)
   END IF
   ! LAKE TOPOLOGY -- downstream reach ID ( NB: DREACHK = REACHID[DREACHI] )
   IERR = NF90_INQ_VARID(NCID,'lk_ds_rchid',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,NWDRCHK,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%DREACHK = NWDRCHK(1)
   ! LAKE TOPOLOGY -- downstream lake index
   IERR = NF90_INQ_VARID(NCID,'lk_ds_nlake',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,NWDRCHI,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IF (NWDRCHI(1).GE.0) THEN
     LKTOPO(ILAK)%DLAKE_I = NWDRCHI(1)+1
   ELSE
     LKTOPO(ILAK)%DLAKE_I = NWDRCHI(1)
   END IF
   ! LAKE TOPOLOGY -- downstream lake ID ( NB: DLAKE_K = LAKE_ID[DLAKE_I] )
   IERR = NF90_INQ_VARID(NCID,'lk_ds_lakeid',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,NWDRCHK,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%DLAKE_K = NWDRCHK(1)
   ! LAKE TOPOLOGY -- outlet basin area under lake
   IERR = NF90_INQ_VARID(NCID,'lk_ds_area',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,BASULAK,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%BUTULAK = BASULAK(1)
   ! LAKE TOPOLOGY -- outlet reach length under lake
   IERR = NF90_INQ_VARID(NCID,'lk_ds_length',IVARID); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   IERR = NF90_GET_VAR(NCID,IVARID,RCHULAK,POS1); !CALL HANDLE_ERR90(IERR)
   if(IERR.NE.NF90_NOERR)then
     SPLDAT_GET  = -1; return
   end if
   LKTOPO(ILAK)%RUTULAK = RCHULAK(1)
  END DO  ! ilak
  ALLOCATE(TMPLAK(NLAK),STAT=IERR)
  IF (IERR.NE.0) then
!    CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
  end if
  FATAL = .FALSE.
  SPLDAT_GET = NETCDF_GET(NCID,'lk_refarea', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%AREAREF = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_refelev', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%ELEVREF = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_refdeph', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%DEPHREF = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_shape_m', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%SHAPE_M = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_he2ar_c', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%HE2AR_C = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_he2ar_d', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%HE2AR_D = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_hghtlow', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%HGHTLOW = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_hghteco', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%HGHTECO = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_hghtspl', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%HGHTSPL = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_dscheco', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%DSCHECO = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_dschspl', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%DSCHSPL = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_ratecva', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%RATECVA = TMPLAK(1:NLAK)
  SPLDAT_GET = NETCDF_GET(NCID,'lk_ratecvb', NLAK,TMPLAK,FATAL); if(SPLDAT_GET < 0) return; LPARAM(1:NLAK)%RATECVB = TMPLAK(1:NLAK)
  FATAL = .TRUE.
  ! check if lake parameters exist and set a flag appropriately
  DO ILAK=1,NLAK
    ! check if mandatory parameters exist for calculating lake level (and outflow):
    !   a) for Nilsson model
    !     reference area, reference elevation, reference depth and m-shape factor
    !   b) for Rupp model
    !     reference area (for initialisation), reference elevation, reference depth and
    !     c- and d-shape factors
    IF (LPARAM(ILAK)%AREAREF.LE.0._DP.OR. &
        LPARAM(ILAK)%ELEVREF.LE.-9999._DP.OR. &
        LPARAM(ILAK)%DEPHREF.LE.0._DP.OR. &
        ((LPARAM(ILAK)%SHAPE_M.LE.0._DP).AND.LAKEFLAG.EQ.1).OR. &
        ((LPARAM(ILAK)%HE2AR_C.LE.0._DP.OR.LPARAM(ILAK)%HE2AR_D.LE.0._DP).AND.LAKEFLAG.EQ.2)) THEN
      PRINT '(A,I2,A,I8,A)',' *** LAKE LEVEL AND OUTFLOW FOR LAKE NUMBER ',ILAK,' &
                     &(ID ',LKTOPO(ILAK)%LAKE_ID,') CAN NOT BE MODELLED ***'
      LPARAM(ILAK)%EXIST = .FALSE.
    ELSE
      LPARAM(ILAK)%EXIST = .TRUE.
    END IF
    ! set hghtlow to elevref if not defined (this might be changed to x*elevref, 0 < x < 1)
    IF (LPARAM(ILAK)%HGHTLOW.LE.-9999._DP) LPARAM(ILAK)%HGHTLOW = LPARAM(ILAK)%ELEVREF
    ! set hghteco to hghtlow if not defined
    IF (LPARAM(ILAK)%HGHTECO.LE.-9999._DP) LPARAM(ILAK)%HGHTECO = LPARAM(ILAK)%HGHTLOW
    ! set hghtspl to hghteco if not defined
    IF (LPARAM(ILAK)%HGHTSPL.LE.-9999._DP) LPARAM(ILAK)%HGHTSPL = LPARAM(ILAK)%HGHTECO
    ! set dscheco to zero if not defined
    IF (LPARAM(ILAK)%DSCHECO.LE.0._DP) LPARAM(ILAK)%DSCHECO = 0._DP
    ! set dschspl to dscheco if not defined
    IF (LPARAM(ILAK)%DSCHSPL.LE.0._DP) LPARAM(ILAK)%DSCHSPL = LPARAM(ILAK)%DSCHECO
    ! if eiher a or b are missing set an appropriate flag to calculating outflow based on the
    ! discharge function: q = sqrt(s)/n*w(h-hl)^a for h > hl (q = 0 for h <= hl).
    IF (LPARAM(ILAK)%RATECVA.LE.0._DP.OR.LPARAM(ILAK)%RATECVB.LE.0._DP) THEN
      LPARAM(ILAK)%DSCHFUN = .TRUE.
    ELSE
      LPARAM(ILAK)%DSCHFUN = .FALSE.
    END IF
  END DO
  DEALLOCATE(TMPLAK,STAT=IERR)
  IF (IERR.NE.0) then
!    CALL EXIT_TOPNET(1,'problem deallocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
  end if
  ! this code requres an outlet to exist
  IF (ANY(LKTOPO(:)%DREACHI.LE.-9999)) THEN
!    PRINT *,'One or more lakes are partially inside the model domain -- this is unsupported case.'
!    CALL EXIT_TOPNET(1,'Recreate the spatial file and select the model domain outlet reach carefully [spldat_get.f90]')
       SPLDAT_GET = -1
       return
  END IF
!  ! (temporary) check to assure that basin area under lake does not exceed total basin area
!  NETOPO(:)%BASULAK = MIN(NETOPO(:)%BASULAK,BPARAM(:)%BASAREA)
!  LKTOPO(:)%BUTULAK = MIN(LKTOPO(:)%BUTULAK,BPARAM(LKTOPO(:)%DREACHI)%BASAREA)
  NETOPO(:)%BASULAK = MIN(NETOPO(:)%BASULAK,BASAREA)
  LKTOPO(:)%BUTULAK = MIN(LKTOPO(:)%BUTULAK,BASAREA(LKTOPO(:)%DREACHI))
  ! (temporary) check to assure that basin area under lake is not negative
  NETOPO(:)%BASULAK = MAX(NETOPO(:)%BASULAK,0.0D0)
  LKTOPO(:)%BUTULAK = MAX(LKTOPO(:)%BUTULAK,0.0D0)
  ! set reach length under lake to zero if negative
  NETOPO(:)%RCHULAK = MAX(NETOPO(:)%RCHULAK,0.0D0)
  LKTOPO(:)%RUTULAK = MAX(LKTOPO(:)%RUTULAK,0.0D0)
  ! (temporary) check to assure that reach length under lake does not exceed total reach length
  NETOPO(:)%RCHULAK = MIN(NETOPO(:)%RCHULAK,RPARAM(:)%RLENGTH)
  LKTOPO(:)%RUTULAK = MIN(LKTOPO(:)%RUTULAK,RPARAM(LKTOPO(:)%DREACHI)%RLENGTH)
  ! Convert reach length submerged by lake (m) to fraction
  NETOPO(:)%RCHULAK = NETOPO(:)%RCHULAK / RPARAM(:)%RLENGTH
  LKTOPO(:)%RUTULAK = LKTOPO(:)%RUTULAK / RPARAM(LKTOPO(:)%DREACHI)%RLENGTH
  ! initially mark all reaches as non-lake-inlet reaches
  NETOPO(:)%LAKINLT =.FALSE.
  ! when a reach belongs to two lakes (in the case when a lake outlet is an
  ! inlet into another lake, i.e. a downstream lake exists), make sure that the
  ! reach is associated with the downstream lake (as a lake inlet). Otherwise,
  ! when lake has no downstream lake defined, let the lake outlet be associated
  ! with lake
  DO ILAK=1,NLAK
   ! outlet reach index
   KRCH = LKTOPO(ILAK)%DREACHI
   ! downstream lake index
   DLAK = LKTOPO(ILAK)%DLAKE_I
   IF (DLAK.LE.0) THEN
     ! lake ilak has no downstream lake defined so the lake outlet only belongs
     ! to lake ilak
     NETOPO(KRCH)%LAKE_IX = LKTOPO(ILAK)%LAKE_IX
     NETOPO(KRCH)%LAKE_ID = LKTOPO(ILAK)%LAKE_ID
     ! also make sure that basin area and reach length under lake are zero in
     ! this data structure for lake outlets (lktopo(:)%butulak and lktopo(:)%rutulak
     ! are used for lake outlets)
     NETOPO(KRCH)%BASULAK = 0._DP
     NETOPO(KRCH)%RCHULAK = 0._DP
   ELSE
     ! lake ilak has a downstream lake defined so the lake outlet belongs
     ! to the downstream lake as a lake inlet
     NETOPO(KRCH)%LAKE_IX = LKTOPO(DLAK)%LAKE_IX
     NETOPO(KRCH)%LAKE_ID = LKTOPO(DLAK)%LAKE_ID
   END IF
  END DO
  ! loop through the lakes and do some checks
  DO ILAK=1,NLAK
   ! outlet reach index for lake ilak
   KRCH = LKTOPO(ILAK)%DREACHI
   ! identify potential lake inlets for lake ilak as all headwater reaches that are
   ! partly in lake; and all non-headwater reaches that have:
   ! i) no upstream reaches in lake; AND
   ! ii) EITHER
   !      a) reach partly in lake
   !     OR
   !      b) a downstream reach fully in lake  !! NB require reach to be FULLY contained in lake
   !     OR
   !      c) a downstream reach that is the lake outlet
   DO IRCH=1,NRCH
     IF (IRCH.EQ.KRCH) CYCLE ! an outlet can never be an inlet of the same lake
     IF (NETOPO(IRCH)%LAKE_IX.EQ.LKTOPO(ILAK)%LAKE_IX) THEN
       ! reach irch is associated with lake ilak
       IF (SIZE(NETOPO(IRCH)%UREACHI).LE.0) THEN
         IF(NETOPO(IRCH)%BASULAK.LT.BASAREA(IRCH)) THEN
           ! reach is a headwater reach -> mark as lake inlet
           NETOPO(IRCH)%LAKINLT =.TRUE.
         ELSE
!           ! reach is a headwater reach and completely submerged in lake!
!           CALL EXIT_TOPNET(1,'Headwater basin is fully submerged within a lake! [spldat_get.f90]')
            SPLDAT_GET = -1
            RETURN
         END IF
       ELSE
         ! reach is not a headwater reach -> apply criteria for lake inlet
         IF (ALL(NETOPO(NETOPO(IRCH)%UREACHI(:))%LAKE_IX.NE.LKTOPO(ILAK)%LAKE_IX) .AND. &
             (NETOPO(IRCH)%RCHULAK.LT.1._DP &
              .OR. &
              (NETOPO(NETOPO(IRCH)%DREACHI)%LAKE_IX.EQ.LKTOPO(ILAK)%LAKE_IX .AND. &
               NETOPO(NETOPO(IRCH)%DREACHI)%RCHULAK.GE.1._DP) &
              .OR. &
              NETOPO(IRCH)%DREACHI.EQ.KRCH)) &
           NETOPO(IRCH)%LAKINLT =.TRUE.
       END IF
     END IF
   END DO
   ! loop through reaches from potential lake inlets down to the lake outlet and 
   ! i) mark downstream reaches as fully submerged in lake (except the lake outlet)
   ! ii) mark downstream reaches as non-lake-inlets
   ! iii) mark other upstream reaches (different from the upstream one just looping from)
   !      as lake inlets if they are not submerged in lake
   ! first create processing mask (only need to process each reach once in this loop)
   ALLOCATE(PMASK(NRCH),STAT=IERR)
   IF (IERR.NE.0) then
!     CALL EXIT_TOPNET(1,'problem allocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
   end if
   PMASK(:)=.FALSE.
   PMASK(KRCH)=.TRUE.
   DO IRCH=1,NRCH
     IF (PMASK(IRCH)) CYCLE
     IF (NETOPO(IRCH)%LAKE_IX.EQ.LKTOPO(ILAK)%LAKE_IX.AND.NETOPO(IRCH)%LAKINLT) THEN
       ! reach is potential inlet for lake ilak
       ! set initial reach in the loop as the next downstream reach
       JRCH = NETOPO(IRCH)%DREACHI
       DO WHILE (JRCH.NE.KRCH)
         IF (PMASK(JRCH)) EXIT
         PMASK(JRCH) = .TRUE.
         ! remove lake inlet status
         NETOPO(JRCH)%LAKINLT = .FALSE.
         IF (NETOPO(JRCH)%RCHULAK.LT.1._DP) THEN
           ! reach is not fully submerged -> make it fully submerged
           NETOPO(JRCH)%RCHULAK = 1._DP
         END IF
         IF (NETOPO(JRCH)%LAKE_IX.NE.LKTOPO(ILAK)%LAKE_IX) THEN
           ! reach is not associated with lake -> make lake association
           NETOPO(JRCH)%LAKE_IX = LKTOPO(ILAK)%LAKE_IX
           NETOPO(JRCH)%LAKE_ID = LKTOPO(ILAK)%LAKE_ID
         END IF
         ! mark non-lake upstream reaches as lake inlets
         DO I=1,SIZE(NETOPO(JRCH)%UREACHI)
           URCH = NETOPO(JRCH)%UREACHI(I)
           ULAK = NETOPO(URCH)%LAKE_IX
           IF (ULAK.NE.LKTOPO(ILAK)%LAKE_IX) THEN
             ! urch is not associated with lake -> mark as lake inlet
             NETOPO(URCH)%LAKINLT = .TRUE.
             NETOPO(URCH)%LAKE_IX = LKTOPO(ILAK)%LAKE_IX
             NETOPO(URCH)%LAKE_ID = LKTOPO(ILAK)%LAKE_ID
             ! if urch is associated with other lakes -> mark this as downstream lake
             ! with negligible basin area and reach length under lake
             IF (ANY(ULAK.EQ.LKTOPO(:)%LAKE_IX)) THEN
               ! this must be an upstream lake outlet -- so lets
               ! mark current lake ilak as downstream lake from ulak
               LKTOPO(ULAK)%DLAKE_I = LKTOPO(ILAK)%LAKE_IX
               LKTOPO(ULAK)%DLAKE_K = LKTOPO(ILAK)%LAKE_ID
               ! with negligible basin area and reach length under lake ilak
               NETOPO(URCH)%BASULAK = 0._DP
               NETOPO(URCH)%RCHULAK = 0._DP
             END IF
           ELSE IF (URCH.NE.IRCH) THEN
             ! urch is associated with lake and is different from irch
             IF (NETOPO(URCH)%RCHULAK.GE.1._DP) THEN
               ! urch is fully contained in lake so it can not be an inlet
               NETOPO(URCH)%LAKINLT = .FALSE.
             END IF
           END IF
         END DO
         ! next downstream reach in loop
         JRCH = NETOPO(JRCH)%DREACHI
       END DO
     END IF
   END DO
   DEALLOCATE(PMASK,STAT=IERR)
   IF (IERR.NE.0) then
!     CALL EXIT_TOPNET(1,'problem deallocating array [spldat_get.f90]')
       SPLDAT_GET = -1
       return
   end if
   ! ignore "small" lakes (that only have an outlet reach assigned to them),
   ! this is done because the same reach can not both be a lake outlet and
   ! a lake inlet.
   NLRCH = COUNT(NETOPO(:)%LAKE_IX.EQ.LKTOPO(ILAK)%LAKE_IX)
   IF (LKTOPO(ILAK)%DLAKE_I.GT.0) NLRCH = NLRCH + 1
   IF (NLRCH.LE.1) THEN
     ! ignore "small" lake
     PRINT '(A,I2,A,I8,A)',' *** SMALL LAKE NUMBER ',ILAK,' &
                     &(ID ',LKTOPO(ILAK)%LAKE_ID,') CAN NOT BE MODELLED (IGNORED) ***'
     NETOPO(KRCH)%LAKE_IX = -9999
     NETOPO(KRCH)%LAKE_ID = -9999
     NETOPO(KRCH)%RCHULAK = 0._DP
     NETOPO(KRCH)%BASULAK = 0._DP
     LKTOPO(ILAK)%LAKE_ID = -9999
     LKTOPO(ILAK)%BUTULAK = 0._DP
     LKTOPO(ILAK)%RUTULAK = 0._DP
   END IF
   !print *,'-----------------'
   !print *,'ilak,lakeid ', ilak, lktopo(ilak)%lake_id
   !print *,pack(netopo(:)%reachid,netopo(:)%lakinlt.and.netopo(:)%lake_ix.eq.lktopo(ilak)%lake_ix.and.netopo(:)%reachix.ne.krch)
   !print *,'-----------------'
   !flush(6)
  END DO
  ! Modify basin area and reach length to account for submergence by lake
!  BPARAM(:)%BASAREA = BPARAM(:)%BASAREA - NETOPO(:)%BASULAK
!  BPARAM(LKTOPO(:)%DREACHI)%BASAREA = BPARAM(LKTOPO(:)%DREACHI)%BASAREA - LKTOPO(:)%BUTULAK
  RPARAM(:)%RLENGTH = RPARAM(:)%RLENGTH * (1._DP - NETOPO(:)%RCHULAK)
  RPARAM(LKTOPO(:)%DREACHI)%RLENGTH = RPARAM(LKTOPO(:)%DREACHI)%RLENGTH * (1._DP - LKTOPO(:)%RUTULAK)
 ENDIF
 ! ---------------------------------------------------------------------------------------
 ! (6) GET THE GLOBAL ATTRIBUTES
 ! ---------------------------------------------------------------------------------------
 ! ---------------------------------------------------------------------------------------
 ! CLOSE THE NETCDF FILE
 ! ---------------------------------------------------------------------------------------
 deallocate(BASAREA,STAT=IERR)
IERR = NF90_CLOSE(NCID) 
if(IERR.NE.NF90_NOERR) SPLDAT_GET = -1
SPLDAT_GET = 0
return
! ----------------------------------------------------------------------------------------
CONTAINS
 function NETCDF_GET(NCID,VARSTR,NPTS,TMPDAT,FATAL)
 USE netcdf
 ! ---------------------------------------------------------------------------------------
 ! used to get data, scale it, and constrain within valid bounds
 ! ---------------------------------------------------------------------------------------
 integer(i4b)              :: NETCDF_GET
 INTEGER(I4B)              :: IERR      ! error code
 INTEGER(I4B)              :: NCID      ! NetCDF file ID
 CHARACTER(LEN=*)          :: VARSTR    ! character string of desired variable
 INTEGER(I4B)              :: NPTS      ! number of points in the vector
 REAL(DP),DIMENSION(NPTS)  :: TMPDAT    ! returned data vector
 REAL(DP)                  :: SFAC      ! scale factor
 REAL(DP)                  :: VMIN      ! valid min
 REAL(DP)                  :: VMAX      ! valid max
 LOGICAL(LGT)              :: FATAL     ! flag for fatal/non-fatal error
 INTEGER(I4B),DIMENSION(1) :: ISTART    ! start indices for data
 INTEGER(I4B),DIMENSION(1) :: ICOUNT    ! count for data
 ! ---------------------------------------------------------------------------------------
 ISTART = (/1/)
 ICOUNT = (/NPTS/)
 IF (FATAL) THEN
  IERR = NF90_INQ_VARID(NCID,TRIM(VARSTR),IVARID); !CALL HANDLE_VAR_ERR90(TRIM(VARSTR),IERR)
  if(IERR.NE.NF90_NOERR)then
     NETCDF_GET  = -1;
     return
  end if
  IERR = NF90_GET_VAR(NCID,IVARID,TMPDAT,ISTART,ICOUNT); !CALL HANDLE_ERR90(IERR)
  if(IERR.NE.NF90_NOERR)then
     NETCDF_GET  = -1;
     return
  end if
 ELSE
  IERR = NF90_INQ_VARID(NCID,TRIM(VARSTR),IVARID); !CALL HANDLE_ERR90_2(IERR)
  IF (IERR.EQ.0) THEN
    IERR = NF90_GET_VAR(NCID,IVARID,TMPDAT,ISTART,ICOUNT); !CALL HANDLE_ERR90(IERR)
      if(IERR.NE.NF90_NOERR)then
         NETCDF_GET  = -1;
         return
      end if
  ELSE
    NETCDF_GET = 0
    TMPDAT(1:NPTS) = -9999._DP
    RETURN
  END IF
 ENDIF
 ! NB according to the CF conventions the minimum and maximum should be checked
 ! before the scale_factor and/or add_offset is applied (need to change this AND
 ! the spacial file generation tool)
 ! NB need to add the "add_offset" attribute to all variables that have negative
 ! values less then -9999 (cen_lat and snowz0n)
 ! NB values in the spatial file can not have the value -9999 (which is the
 ! missing value or _FillValue in the netcdf files) because this values is
 ! ignored by Topnet when it is applying the scaling factor and min/max (see
 ! below).
 ! NB -9999 values (_FillValue) should be read from the netcdf file (not assume
 ! that -9999 will be used)
 IERR = NF90_GET_ATT(NCID,IVARID,'scale_factor',SFAC) ! get scale factor
 IF (IERR.EQ.0) WHERE(TMPDAT.NE.-9999._DP) TMPDAT = TMPDAT*SFAC  ! only scale if the scale factor exists
 IERR = NF90_GET_ATT(NCID,IVARID,'valid_min',VMIN) ! get the lower bound
 IF (IERR.EQ.0) WHERE(TMPDAT.LT.VMIN.AND.TMPDAT.NE.-9999._DP) TMPDAT=VMIN   ! ensure .ge. lower bound
 IERR = NF90_GET_ATT(NCID,IVARID,'valid_max',VMAX) ! get the upper bound
 IF (IERR.EQ.0) WHERE(TMPDAT.GT.VMAX) TMPDAT=VMAX   ! ensure .le. upper bound
 NETCDF_GET = 0
 ! ---------------------------------------------------------------------------------------
 END function NETCDF_GET
END function SPLDAT_GET
!end module
