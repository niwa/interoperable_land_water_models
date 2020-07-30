!module init
!    contains
!subroutine print_value()
!!DEC$ ATTRIBUTES DLLEXPORT::print_value
!use reachvars
!write(*,*)NETOPO(26)%UREACHI(2)
!end subroutine

!function SPLDAT_GET_NONC(ROUTFLAG,LAKEFLAG,NRCH,NLAK,NETOPO,RPARAM,LKTOPO,LPARAM)
!function SPLDAT_GET_NONC(ROUTFLAG,LAKEFLAG,NRCH,NLAK,ncSpatial)
function SPLDAT_GET_NONC(txtRiv,txtLak,nMaxUStr)
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
!USE netcdf                                                ! netCDF subroutines
IMPLICIT NONE

INTEGER(I4B)                                :: SPLDAT_GET_NONC

! Output variables -- NetCDF dimensions
!INTEGER(I4B), INTENT(IN)               :: ROUTFLAG            ! if routing will be done
!INTEGER(I4B), INTENT(INOUT)            :: LAKEFLAG            ! if lake will be simulated
!INTEGER(I4B), INTENT(OUT)              :: NRCH            ! number of reaches
!INTEGER(I4B), INTENT(OUT)              :: NLAK            ! number of lakes
character(len=LEN_PATH), intent(in)    :: txtRiv,txtLak
INTEGER(I4B), INTENT(IN)               :: nMaxUStr            !
LOGICAL(LGT)                           :: exists
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
!INTEGER(I4B)                           :: NCID            ! NetCDF file ID
!INTEGER(I4B)                           :: IDIMID          ! NetCDF dimension ID
!INTEGER(I4B)                           :: IVARID          ! NetCDF variable ID
!INTEGER(I4B),DIMENSION(1)              :: TMPINT          ! netcdf data array
!INTEGER(I4B),DIMENSION(1)              :: POS1            ! 1-d array index
!INTEGER(I4B),DIMENSION(2)              :: POS2            ! 2-d array index
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
!REAL(DP)                               :: SFAC_VAL        ! scale factor: value of prob distn
!REAL(DP)                               :: SFAC_FRQ        ! scale factor: freq in prob distn
REAL(DP),DIMENSION(:),ALLOCATABLE      :: BASAREA          ! data vector - BASIN AREA
! (2) Network topology + reach parameters (NB: populate structures "NETOPO" and "RPARAM")
INTEGER(I4B),DIMENSION(2)              :: NWRCHID         ! reach id (net topo)
REAL(DP)                               :: RCHLAT1         ! start latitude
REAL(DP)                               :: RCHLAT2         ! end latitude
REAL(DP)                               :: RCHLON1         ! start longitude
REAL(DP)                               :: RCHLON2         ! end longitude
INTEGER(I4B)                           :: NWDRCHI         ! downstream reach index
INTEGER(I4B)                           :: NWDRCHK         ! downstream reach id
INTEGER(I4B)                           :: NUMBUPS         ! number of upstream reaches
INTEGER(I4B),DIMENSION(10)             :: NWURCHI         ! upstream reach index
INTEGER(I4B),DIMENSION(10)             :: NWURCHK         ! upstream reach id
!! (3) Lake topology + lake parameters (NB: populate structures "LKTOPO" and "LPARAM")
!INTEGER(I4B),DIMENSION(1)              :: NWLAKID         ! lake id (lake topo)
!INTEGER(I4B),DIMENSION(1)              :: NWLAKIX         ! lake index (lake topo)
!REAL(DP),DIMENSION(1)                  :: LAKLAT1         ! centroid latitude
!REAL(DP),DIMENSION(1)                  :: LAKLAT2         ! outled latitude
!REAL(DP),DIMENSION(1)                  :: LAKLON1         ! centroid longitude
!REAL(DP),DIMENSION(1)                  :: LAKLON2         ! outlet longitude
!REAL(DP),DIMENSION(1)                  :: BASULAK         ! basin area under lake
!REAL(DP),DIMENSION(1)                  :: RCHULAK         ! reach length under lake
!REAL(DP),DIMENSION(:),ALLOCATABLE      :: TMPLAK          ! data vector - lake parameters
! (4) Basin initial states (NB: populate structure "BSTATE")
!       (populate structure)
!! (5) Global attributes
!CHARACTER(LEN=50)                      :: TMPTEXT         ! temporary text
!CHARACTER(LEN=100)                     :: DESTEXT         ! descriptive text
!INTEGER(I4B)                           :: NLRCH           ! number of reaches associated with lake
!INTEGER(I4B)                           :: ILEN            ! length of the basin name
! (6) Other
!LOGICAL(LGT)                           :: FATAL           ! flag for fatal/non-fatal error
!INTEGER(I4B)                           :: I               ! string index
!INTEGER(I4B)                           :: SLEN            ! string length
!INTEGER(I4B)                           :: LEN_TRIM_NULL   ! function
!LOGICAL(LGT),DIMENSION(:),ALLOCATABLE   :: PMASK          ! reach processing mask
! ----------------------------------------------------------------------------------------
! OPEN FILE
! ----------------------------------------------------------------------------------------
!write(*,*) 'I am right here'
 ! ---------------------------------------------------------------------------------------
 ! (1) GET DIMENSIONS
 ! ---------------------------------------------------------------------------------------
NRCH = 0
INQUIRE(FILE=TRIM(txtRiv), EXIST=exists)
if(.not.exists) then
   SPLDAT_GET_NONC = -1; return 
end if 
OPEN (90, file = TRIM(txtRiv))
DO
  READ(90,*,iostat=IERR)
  IF (IERR/=0) EXIT
  NRCH = NRCH + 1
END DO
NRCH = NRCH - 1
if(NRCH <= 0) then
   SPLDAT_GET_NONC = -1; return 
   CLOSE (90)
end if
rewind(90)

NLAK = 0
LAKEFLAG = 0
!INQUIRE(FILE=TRIM(txtRiv), EXIST=exists)
!if(exists)then
!    OPEN (91, file = TRIM(txtLak))
!    DO
!      READ(1,*,iostat=IERR)
!      IF (IERR/=0) EXIT
!      NLAK = NLAK + 1
!    END DO
!    NLAK = NLAK - 1
!    if(NLAK <= 0) then
!       SPLDAT_GET_NONC = -1; return 
!       CLOSE (91)
!    end if
!end if
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
!   CALL EXIT_TOPNET(1,'problem allocating array [SPLDAT_GET_NONC.f90]')
    SPLDAT_GET_NONC = -1; return
end if
ALLOCATE(RPARAM(NRCH),STAT=IERR)      ! in MODULE reachparam
IF (IERR.NE.0) then
!  CALL EXIT_TOPNET(1,'problem allocating array [SPLDAT_GET_NONC.f90]')
    SPLDAT_GET_NONC = -1; return
end if
ALLOCATE(LKTOPO(NLAK),STAT=IERR)      ! in MODULE reachparam
IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [SPLDAT_GET_NONC.f90]')
    SPLDAT_GET_NONC = -1; return
end if
ALLOCATE(LPARAM(NLAK),STAT=IERR)      ! in MODULE reachparam
IF (IERR.NE.0) then
!   CALL EXIT_TOPNET(1,'problem allocating array [SPLDAT_GET_NONC.f90]')
    SPLDAT_GET_NONC = -1; return
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
IF (IERR.NE.0) then
    SPLDAT_GET_NONC = -1;     return
end if

read(90,*)
DO IRCH=1,NRCH
  read(90, *) basarea(IRCH),RPARAM(IRCH)%TOTAREA,NWRCHID(1),NWRCHID(2),NWDRCHI,NWDRCHK,RCHLAT1,RCHLAT2,RCHLON1,RCHLON2,NUMBUPS, &
  (NWURCHI(JRCH),JRCH = 1,nMaxUStr),(NWURCHK(JRCH),JRCH = 1,nMaxUStr),RPARAM(IRCH)%R_SLOPE,RPARAM(IRCH)%R_MAN_N,RPARAM(IRCH)%R_WIDTH,RPARAM(IRCH)%RLENGTH 
  IF (NWRCHID(1).GE.0) THEN                      !rchindex(nrch) - reach index
    NETOPO(IRCH)%REACHIX = NWRCHID(1)+1
  ELSE
    NETOPO(IRCH)%REACHIX = NWRCHID(1)
  END IF
  NETOPO(IRCH)%REACHID = NWRCHID(2)              !rchid(nrch) - reach ID
  IF (NWDRCHI.GE.0) THEN                         !dsrch_nrch(nrch) - downstream reach index
    NETOPO(IRCH)%DREACHI = NWDRCHI+1
  ELSE
    NETOPO(IRCH)%DREACHI = NWDRCHI
  END IF
  NETOPO(IRCH)%DREACHK = NWDRCHK                 !dsrch_rchid(nrch) - downstream reach ID ( NB: DREACHK = RCHID[DREACHI] )
  NETOPO(IRCH)%RCHLAT1 = RCHLAT1                 !start_lat(nrch) - start lat
  NETOPO(IRCH)%RCHLAT2 = RCHLAT2                 !end_lat(nrch) - end lat
  
  NETOPO(IRCH)%RCHLON1 = RCHLON1                 !start_lon(nrch) - start lon
  NETOPO(IRCH)%RCHLON2 = RCHLON2                 !end_lon(nrch) - end lon
  !number of upstream reaches
  ALLOCATE(NETOPO(IRCH)%UREACHI(NUMBUPS),NETOPO(IRCH)%UREACHK(NUMBUPS),STAT=IERR) !Jing
  IF (IERR.NE.0) then
       SPLDAT_GET_NONC = -1;  return
  end if
  
  DO JRCH=1,NUMBUPS
      IF (NWURCHI(JRCH).GE.0) THEN
         NETOPO(IRCH)%UREACHI(JRCH) = NWURCHI(JRCH)+1
      ELSE
         NETOPO(IRCH)%UREACHI(JRCH) = NWURCHI(JRCH)
      END IF
      NETOPO(IRCH)%UREACHK(JRCH) = NWURCHK(JRCH)
  enddo  
ENDDO
close(90)
 ! ---------------------------------------------------------------------------------------
 ! (3a) get common basin parameters
 ! ---------------------------------------------------------------------------------------
 ! (5) GET THE LAKE TOPOLOGY AND LAKE PARAMETERS (POPULATE STRUCTURES LKTOPO + LPARAM)
 ! ---------------------------------------------------------------------------------------
 ! ---------------------------------------------------------------------------------------
 ! (6) GET THE GLOBAL ATTRIBUTES
 ! ---------------------------------------------------------------------------------------
 ! ---------------------------------------------------------------------------------------
 ! CLOSE THE NETCDF FILE
 ! ---------------------------------------------------------------------------------------
close(91)

deallocate(BASAREA,STAT=IERR)
SPLDAT_GET_NONC = 0
return
! ----------------------------------------------------------------------------------------

END function SPLDAT_GET_NONC
!end module
