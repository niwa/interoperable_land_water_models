MODULE interlake
 IMPLICIT NONE
 ! ---------------------------------------------------------------------------------------
 ! DEFINES EXPLICIT INTERFACES BETWEEN SUB-PROGRAMS
 ! ---------------------------------------------------------------------------------------
 INTERFACE ! needed because of pointer arguments
  FUNCTION INIT_LAKES(NLAK,LAKFLX)
	! ----------------------------------------------------------------------------------------
	USE nrtype                                                ! variable types (DP, I4B, etc.)
	USE laketype
	IMPLICIT NONE
	INTEGER(I4B)                            :: INIT_LAKES
	! Input variables
	INTEGER(I4B), INTENT(IN)                :: NLAK            ! current ensemble member
	TYPE(LKFLX),DIMENSION(NLAK)             ::LAKFLX
  END FUNCTION INIT_LAKES
 END INTERFACE
! ----------------------------------------------------------------------------------------
 INTERFACE ! needed because of pointer arguments
   FUNCTION  Q_RCH_LAKE(DT,TBOUNDS,JRCH,NRCH,NETOPO,KROUTE,RPARAM,BASFLX,RCHFLX,LAKEFLAG,NLAK,LKTOPO,APRECIP,LPARAM,LSTATE,LAKFLX,MAXQPAR,RSTEP)
    USE nrtype
    !USE userparam
    !USE user_state
    !USE inputdat2d
    !USE ret_moddat
    USE interreach, ONLY: qroute_rch
    use reachtype
    !use bastype
    use laketype
    IMPLICIT NONE
    INTEGER(I4B)                                  :: Q_RCH_LAKE        ! 
    ! inputs
    REAL(DP), INTENT(IN)                          :: DT       !
    REAL(DP),DIMENSION(0:1),INTENT(IN)            :: TBOUNDS     !
    INTEGER(I4B), INTENT(IN)                      :: JRCH        ! loop through the stream segments
    INTEGER(I4B),INTENT(IN)                       :: NRCH
    TYPE(RCHTOPO), DIMENSION(NRCH),INTENT(IN)     :: NETOPO
    TYPE(KREACH),DIMENSION(NRCH),INTENT(INOUT)    :: KROUTE
    TYPE(RCHPRP),DIMENSION(NRCH),INTENT(IN)       :: RPARAM
    TYPE(MODFLX), DIMENSION(NRCH),INTENT(IN)      :: BASFLX
    TYPE(STRFLX), INTENT(INOUT)                   :: RCHFLX
    INTEGER(I4B), INTENT(IN)                      :: LAKEFLAG        ! 
    INTEGER(I4B), INTENT(IN)                      :: NLAK       ! 
    type(LAKTOPO), dimension(NLAK), INTENT(IN)    :: LKTOPO 
    REAL(DP), INTENT(IN)                          :: APRECIP       ! 
    TYPE(LAKPRP), DIMENSION(NLAK), INTENT(IN)     :: LPARAM  ! Lake Parameters
    TYPE(LKSTATE), DIMENSION(NLAK), INTENT(INOUT) :: LSTATE
    TYPE(LKFLX),DIMENSION(NLAK),INTENT(INOUT)     :: LAKFLX
    INTEGER(I4B),INTENT(IN)                       :: MAXQPAR
    INTEGER(I4B), INTENT(IN), OPTIONAL            :: RSTEP       ! retrospective time step offset
  END FUNCTION Q_RCH_LAKE
 END INTERFACE
END MODULE interlake
