MODULE interreach
 IMPLICIT NONE
 ! ---------------------------------------------------------------------------------------
 ! DEFINES EXPLICIT INTERFACES BETWEEN SUB-PROGRAMS
 ! ---------------------------------------------------------------------------------------
 !INTERFACE ! needed because of pointer arguments
 !  Function QTOTAL_RCH(IRCH,NRCH,NETOPO,BPARAM,BINPUT,BASFLX,RCHFLX)
 !   USE nrtype                                              ! variable types (I4B, DP, etc.)
 !   USE bastype
 !   USE reachtype
 !   IMPLICIT NONE
 !   INTEGER(I4B)                                :: QTOTAL_RCH   ! 
 !   INTEGER(I4B), INTENT(IN)                    :: IRCH     ! reach to process
 !   INTEGER(I4B), INTENT(IN)                    :: NRCH     ! reach to process
 !   TYPE(RCHTOPO),INTENT(IN)                     :: NETOPO
 !   TYPE(CPARAM), DIMENSION(NRCH), INTENT(IN)    :: BPARAM
 !   TYPE(MODFLX), DIMENSION(NRCH), INTENT(IN)    :: BASFLX
 !   TYPE(INPDAT), DIMENSION(NRCH), INTENT(OUT)    :: BINPUT
 !   TYPE(STRFLX), INTENT(OUT)                   :: RCHFLX
 ! end function QTOTAL_RCH
 !END INTERFACE
 interface
   function INIT_BASIN()
   end function
 end interface
 interface
   function REACHORDER()
   end function
 end interface
 INTERFACE ! needed because of pointer arguments
   function SPLDAT_GET(ROUTFLAG,ncSpatial)
    USE nrtype                                                ! variable types (DP, I4B, etc.)
    use reachvars
    use lakevars
    USE netcdf                                                ! netCDF subroutines
    IMPLICIT NONE
    INTEGER(I4B)                                :: SPLDAT_GET
    INTEGER(I4B), INTENT(IN)               :: ROUTFLAG            ! if routing will be done
    character(len=LEN_PATH), intent(in)    :: ncSpatial
   end function SPLDAT_GET
 END INTERFACE
 
 INTERFACE ! needed because of pointer arguments
 function RSTART_GET(RESTART_FILE,DTIME,LEXIST,FEXIST)
    USE nrtype                                               ! variable types (DP, I4B, etc.)
    USE reachvars                                           ! reach state variables (particles)
    USE lakevars                                           ! lake states (level, area, volume)
    USE netcdf                                               ! netCDF subroutines
    IMPLICIT NONE
    INTEGER(I4B)                           :: RSTART_GET
    REAL(DP), INTENT(IN)                   :: DTIME          ! Time difference between restart file and model reference time
    LOGICAL(LGT), INTENT(OUT)              :: LEXIST         ! .TRUE. if re-start file exists
    LOGICAL(LGT), INTENT(IN), OPTIONAL     :: FEXIST         ! if .TRUE., only check for file existance
    CHARACTER(LEN=LEN_PATH)           :: RESTART_FILE   ! name of re-start file
 end function RSTART_GET
 END INTERFACE

 INTERFACE ! needed because of pointer arguments
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
    TYPE(MODFLX), DIMENSION(NRCH),INTENT(IN)  :: BASFLX
    INTEGER(I4B), INTENT(IN), OPTIONAL          :: RSTEP     ! retrospective time step offset
    ! Output
    INTEGER(I4B), INTENT(OUT)                   :: ND        ! number of routed particles
    REAL(DP), DIMENSION(:), POINTER             :: QD        ! flow particles just enetered JRCH
    REAL(DP), DIMENSION(:), POINTER             :: TD        ! time flow particles entered JRCH
   END FUNCTION QEXMUL_RCH 
 END INTERFACE

 INTERFACE ! needed because of pointer arguments
   FUNCTION GETUSQ_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,Q_JRCH,TENTRY,T_EXIT,RSTEP)
! ----------------------------------------------------------------------------------------
    USE nrtype
!    USE bastype
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
   END FUNCTION GETUSQ_RCH 
 END INTERFACE
 INTERFACE ! needed because of pointer arguments
   FUNCTION REMOVE_RCH(Q_JRCH,TENTRY,T_EXIT,MAXQPAR)
    ! ----------------------------------------------------------------------------------------
    USE nrtype
    USE nrutil, ONLY : arth                                 ! Num. Recipies utilities
    IMPLICIT NONE
    INTEGER(I4B)                                ::REMOVE_RCH
    ! I/O
    REAL(DP), DIMENSION(:), POINTER             :: Q_JRCH   ! merged (non-routed) flow in JRCH
    REAL(DP), DIMENSION(:), POINTER             :: TENTRY   ! time flow particles entered JRCH
    REAL(DP), DIMENSION(:), POINTER             :: T_EXIT   ! time flow particles exited JRCH
    INTEGER(I4B),INTENT(IN)                     :: MAXQPAR
   END FUNCTION REMOVE_RCH 
 END INTERFACE

 INTERFACE ! needed because of pointer arguments
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
   END FUNCTION KINWAV_RCH 
 END INTERFACE
 INTERFACE ! needed because of pointer arguments
   FUNCTION INTERP_RCH(TOLD,QOLD,TNEW,QNEW,IERR)
    ! --------------------------------------------------------------------------------------------
    USE nrtype
    IMPLICIT NONE
    INTEGER(I4B)                                :: INTERP_RCH
    ! Input
    REAL(DP), DIMENSION(:), INTENT(IN)          :: TOLD     ! input time array
    REAL(DP), DIMENSION(:), INTENT(IN)          :: QOLD     ! input flow array
    REAL(DP), DIMENSION(:), INTENT(IN)          :: TNEW     ! desired output times
    ! Output
    REAL(DP), DIMENSION(:), INTENT(OUT)         :: QNEW     ! flow averaged for desired times
    INTEGER(I4B), INTENT(OUT)                   :: IERR     ! error, 1= bad bounds
   END FUNCTION INTERP_RCH 
 END INTERFACE

 INTERFACE ! needed because of pointer arguments
   FUNCTION QROUTE_RCH(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,LAKFLX,MAXQPAR,RSTEP)
    ! ----------------------------------------------------------------------------------------
    USE nrtype
!    USE bastype
    USE laketype
    USE reachtype
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
   END FUNCTION QROUTE_RCH 
 END INTERFACE
 
END MODULE interreach
