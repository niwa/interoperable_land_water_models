module routing_model
  use iso_c_binding

  use iso_c_utils
  use logging
  USE nrtype
  use TopRoute
  
  implicit none

  !!Initialization
  
  double precision, target :: t
  double precision, target :: t_end
  double precision, target :: t_start
  !
  !character(kind=c_char) :: c_startfile(*) ! initialization from previous simulation
  character(len=LEN_PATH)     :: strRivFile       ! Riv setups
  character(len=LEN_PATH)     :: strLakFile       ! Lake setups
  character(len=LEN_PATH)     :: strStateFile   ! restart file (stored river routing state variables)
  !character(len=LEN_PATH)     :: strRestartFile2  ! restart file (stored river routing state variables)
  INTEGER(I4B)                :: nLakMod         ! type of lake model (0, 1, 2)
  double precision            :: deltat           ! time step (seconds)
  double precision            :: t_diff           ! time diff between restart file and reference
  INTEGER(I4B)                :: nRch             ! number of rches
  INTEGER(I4B)                :: nLak             ! number of lakes
  INTEGER(I4B)                :: nMaxQPars         ! number of maximum flow particles
  INTEGER(I4B)                :: nMaxUStr           !number of maximum upstream reaches
  
  integer(c_int), parameter :: MAXDIMS = 1
  
  !!Update (from another model)

  REAL(DP),DIMENSION(0:1)              :: TBOUNDS     ! time bounds (start + end of timestep)
  REAL(DP),DIMENSION(:),ALLOCATABLE    :: PRECIP     ! precipitation at time step t (in) - PRECIP(1:nRch)
  REAL(DP),DIMENSION(:),ALLOCATABLE    :: POTVAP     ! potential evaotranspiration at time t (in) - POTVAP(1:nRch)
  REAL(DP),DIMENSION(:),ALLOCATABLE    :: INSTN_Q     ! instant flow at time step t (in) - INSTN_Q(1:nRch)
  REAL(DP),DIMENSION(:),ALLOCATABLE    :: QOUT     ! routed flow at time step t (out) - QOUT(1:nRch)
  REAL(DP),DIMENSION(:),ALLOCATABLE    :: VOUT     ! routed flow velocity at time step t (out) - VOUT(1:nRch)

  !!Finalize
  
contains

  integer(c_int) function finalize() result(ierr) bind(C, name="finalize")
    !DEC$ ATTRIBUTES DLLEXPORT::finalize
    ierr = 0
    !ierr = SaveStates(strRestartFile2)
    ierr = Finalization()
    DEALLOCATE(PRECIP,POTVAP,INSTN_Q,QOUT,VOUT,STAT=IERR)    
    call log(LEVEL_INFO, 'Finalize')
  end function finalize


  integer(c_int) function initialize(c_configfile) result(ierr) bind(C, name="initialize")
    !DEC$ ATTRIBUTES DLLEXPORT::initialize
    implicit none

    ! Variables
    character(kind=c_char), intent(in) :: c_configfile(*)
    character(len=strlen(c_configfile)) :: strConfigFile
!0				!start time
!3600			!end time
!3600			!time step (deltat)
!0				!time difference between restart and reference (t_diff)
!0				!lake model (0, 1 or 2)
!25				!maximum number of flow particles (nMaxQPar / nMaxPar)
!10				!maximum number of upstream reaches (nMaxUStr)
!rivSpatial.csv	!river network
!lakSpatial.csv	!lake network
!restart.csv		!restart file 
    ! Convert c string to fortran string
    ierr = 0
    strConfigFile = char_array_to_string(c_configfile)
    write(msgbuf,*) 'Initializing with ', strConfigFile
    open(90, file = strConfigFile)
    read(90,*) TBOUNDS(0)
    read(90,*) t_end
    read(90,*) deltat
    read(90,*) t_diff
    read(90,*) nLakMod
    read(90,*) nMaxQPars
    read(90,*) nMaxUStr
    read(90,*) strRivFile
    read(90,*) strLakFile
    read(90,*) strStateFile
    close(90)
    if(nLakMod == 0) strLakFile = ''
    TBOUNDS(1) = TBOUNDS(0) + deltat
    ierr = Initialization(strRivFile,strLakFile,strStateFile,nLakMod,deltat,t_diff,nMaxQPars,nMaxUStr,nRch,nLak)
    write (*,*) ierr
    ALLOCATE(PRECIP(nRch),POTVAP(nRch),INSTN_Q(nRch),QOUT(nRch),VOUT(nRch),STAT=IERR)
    PRECIP(:) = 0
    POTVAP(:) = 0
    INSTN_Q(:) = 4.626094E-02
    QOUT(:) = 4.626094E-02
    VOUT(:) = 0.01E-02
    
    call log(LEVEL_INFO, trim(msgbuf))

  end function initialize


  !> Performs a single timestep with the current model.
  integer(c_int) function update(dt) result(ierr) bind(C,name="update")
    !DEC$ ATTRIBUTES DLLEXPORT::update

    !< Custom timestep size, use -1 to use model default.
    real(c_double), value, intent(in) :: dt

    ierr = 0
    write(msgbuf,*) 'Updating with dt: ', dt
    TBOUNDS = TBOUNDS + deltat
    ierr = Route1Step(TBOUNDS,PRECIP,POTVAP,INSTN_Q,QOUT,VOUT)
    write(msgbuf,*) 'Routed flow at outlet:', QOUT(nrch) 
    call log(LEVEL_DEBUG, trim(msgbuf))
    if (dt .eq. -1) then
       t = t + 1.0d0
    else
       t = t + dt
    end if
  end function update


  ! Void function is a subroutine
  subroutine get_var_type(c_var_name, c_type_name)  bind(C, name="get_var_type")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type

    character(kind=c_char), intent(in) :: c_var_name(*)
    character(kind=c_char), intent(out) :: c_type_name(MAXSTRINGLEN)

    character(len=strlen(c_var_name)) :: var_name
    character(len=MAXSTRINGLEN) :: type_name

    var_name = char_array_to_string(c_var_name)

    select case(var_name)
    case('RiverFlow')
       type_name = 'double'
    case('RiverFlowVelocity')
       type_name = 'double'
    case('Time')
       type_name = 'double'
    case('arr3')
       type_name = 'bool'
    case default
    end select

    c_type_name = string_to_char_array(trim(type_name))

  end subroutine get_var_type

  subroutine get_var_rank(c_var_name, rank) bind(C, name="get_var_rank")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank

    character(kind=c_char), intent(in) :: c_var_name(*)
    integer(c_int), intent(out) :: rank

    ! The fortran name of the attribute name
    character(len=strlen(c_var_name)) :: var_name
    ! Store the name
    var_name = char_array_to_string(c_var_name)

    select case(var_name)
    case("RiverFlow")
       rank = 1
    case("RiverFlowVelocity")
       rank = 1
    case("Time")
       rank = 0
    case("arr3")
       rank = 3
    case default
       rank = 0
    end select
  end subroutine get_var_rank

  subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
  
    character(kind=c_char), intent(in) :: c_var_name(*)
    integer(c_int), intent(inout) :: shape(MAXDIMS)
  
    character(len=strlen(c_var_name)) :: var_name
  
    var_name = char_array_to_string(c_var_name)
    shape = (/0/)
  
    select case(var_name)
    case("RiverFlow")
       shape(1:1) = nRch
    case("RiverFlowVelocity")
       shape(1:1) = nRch
    end select
  end subroutine get_var_shape
  
  
  subroutine get_var(c_var_name, x) bind(C, name="get_var")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var
  
    ! Return a pointer to the variable
  
    character(kind=c_char), intent(in) :: c_var_name(*)
    type(c_ptr), intent(inout) :: x
  !
    character(len=strlen(c_var_name)) :: var_name
    ! Store the name
  
    var_name = char_array_to_string(c_var_name)
  
    select case(var_name)
    case("RiverFlow")
       x = c_loc(QOUT)
    case("RiverFlowVelocity")
       x = c_loc(VOUT)
    case("Time")
       x = c_loc(t)
   ! case("arr3")
   !    x = c_loc(arr3)
    end select
  !
  end subroutine get_var
  !
  subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
    !DEC$ ATTRIBUTES DLLEXPORT :: set_var
    ! Return a pointer to the variable
    use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
  
    character(kind=c_char), intent(in) :: c_var_name(*)
    type(c_ptr), value, intent(in) :: xptr
  !
    real(c_double), pointer :: x_1d_double_ptr(:)
  !  real(c_double), pointer :: x_2d_double_ptr(:,:)
  !  real(c_double), pointer :: x_3d_double_ptr(:,:,:)
  !  integer(c_int), pointer :: x_1d_int_ptr(:)
  !  integer(c_int), pointer :: x_2d_int_ptr(:,:)
  !  integer(c_int), pointer :: x_3d_int_ptr(:,:,:)
  !  real(c_float), pointer  :: x_1d_float_ptr(:)
  !  real(c_float), pointer  :: x_2d_float_ptr(:,:)
  !  real(c_float), pointer  :: x_3d_float_ptr(:,:,:)
  !  logical(c_bool), pointer  :: x_1d_bool_ptr(:)
  !  logical(c_bool), pointer  :: x_2d_bool_ptr(:,:)
  !  logical(c_bool), pointer  :: x_3d_bool_ptr(:,:,:)
  !
    ! The fortran name of the attribute name
    character(len=strlen(c_var_name)) :: var_name
    ! Store the name
    var_name = char_array_to_string(c_var_name)
    write (*,*) "Writing variable ", var_name
    select case(var_name)
    case("INSTN_Q")
       call c_f_pointer(xptr, x_1d_double_ptr, shape(INSTN_Q))
       write(*,*) INSTN_Q(:)
       INSTN_Q(:) = x_1d_double_ptr
       write(*,*) INSTN_Q(:)
  !  case("arr2")
  !     call c_f_pointer(xptr, x_2d_int_ptr, shape(arr2))
  !     arr2(:,:) = x_2d_int_ptr
  !  case("arr3")
  !     call c_f_pointer(xptr, x_3d_bool_ptr, shape(arr3))
  !     arr3(:,:,:) = x_3d_bool_ptr
    end select  
  end subroutine set_var
  
  subroutine get_current_time(time) bind(C, name="get_current_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
  
    real(c_double) :: time
    time = t
  
  end subroutine get_current_time
  !
  subroutine get_start_time(time) bind(C, name="get_start_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
  
    real(c_double) :: time
    time = t_start
    write(msgbuf,*) 'the start time is:', time 
  
  end subroutine get_start_time
  !
  subroutine get_end_time(time) bind(C, name="get_end_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
  
    real(c_double) :: time
    time = t_end
  
  end subroutine get_end_time
  
  subroutine get_time_step(time) bind(C, name="get_time_step")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
  
    real(c_double) :: time
    time = deltat
  
  end subroutine get_time_step

end module routing_model