!SUBROUTINE INIT_LAKES(IENS)
! ----------------------------------------------------------------------------------------
! Creator(s):
!    David Rupp
!    Einar Örn Hreinsson, 2009
!       -- changed so only initialised for one ensemble member
!
! ----------------------------------------------------------------------------------------
! Purpose:
!
!   Initializes lake fluxes to zero at beginning of time step
!
! ----------------------------------------------------------------------------------------
! I/O:
!
!   INPUT:
!      IENS: current ensemble member
!
! ----------------------------------------------------------------------------------------
! Structures Populated:
!
!   LAKFLX(IENS,ILAK)% [flux]  = lake discharge, inflow, precipitation and evaporation
!
! ----------------------------------------------------------------------------------------
FUNCTION INIT_LAKES(NLAK,LAKFLX)
!!DIR$ ATTRIBUTES DLLEXPORT :: INIT_LAKES
!!DIR$ ATTRIBUTES ALIAS:'_init_lakes_' :: INIT_LAKES

! ----------------------------------------------------------------------------------------
USE nrtype                                                ! variable types (DP, I4B, etc.)
USE laketype
IMPLICIT NONE
INTEGER(I4B)                            :: INIT_LAKES
! Input variables
INTEGER(I4B), INTENT(IN)                :: NLAK            ! current ensemble member
TYPE(LKFLX),DIMENSION(NLAK)             ::LAKFLX
!IF (IO_INFO%DEBUG_FILE) WRITE(99,*) 'init_lakes'
! ----------------------------------------------------------------------------------------
! (1) INITIALISE LAKE FLUXES
! ----------------------------------------------------------------------------------------
LAKFLX(:)%LAKE_Q = 0._DP  ! lake discharge
LAKFLX(:)%LAKE_I = 0._DP  ! lake inflows
LAKFLX(:)%LAKE_P = 0._DP  ! lake precipitation
LAKFLX(:)%LAKE_E = 0._DP  ! lake evaporation
LAKFLX(:)%LAKE_Qav = 0._DP  ! lake discharge
! ----------------------------------------------------------------------------------------
!IF (IO_INFO%DEBUG_FILE) FLUSH(99)
INIT_LAKES = 0
END FUNCTION INIT_LAKES
