module set_precision
  use iso_fortran_env, only : real32, real64, int32, int64
  implicit none
  private
  public :: dp, i4, i8, r4, r8
  integer, parameter :: dp  = real64
  integer, parameter :: i4  = int32
  integer, parameter :: i8  = int64
  integer, parameter :: r4  = real32
  integer, parameter :: r8  = real64
end module set_precision

module set_constants
  use set_precision, only : dp
  implicit none
  private
  public :: zero, one, two, three, four, ten
  public :: half, third, fourth
  public :: pi, large, near_zero
  public :: max_text_line_length
  real(dp), parameter :: zero      = 0.0_dp
  real(dp), parameter :: one       = 1.0_dp
  real(dp), parameter :: two       = 2.0_dp
  real(dp), parameter :: three     = 3.0_dp
  real(dp), parameter :: four      = 4.0_dp
  real(dp), parameter :: ten       = 10.0_dp
  real(dp), parameter :: third     = one / three
  real(dp), parameter :: fourth    = 0.25_dp
  real(dp), parameter :: half      = 0.50_dp
  real(dp), parameter :: large  = huge(one)
  real(dp), parameter :: pi     = acos(-one)
  real(dp), parameter :: near_zero = epsilon(one)
  integer,  parameter :: max_text_line_length = 1024
end module set_constants

module project_inputs
  use set_constants, only : zero, one, max_text_line_length
  use set_precision, only : dp
  implicit none
  private
  public :: allocate_inputs, deallocate_inputs
  public :: job_name, verbose_level
  public :: n_dim, rec_degree, n_rec_vars, n_nodes, n_ghost, n_skip
  public :: column_scaling, local_scaling, use_tci
  public :: use_cweno, epsilon_cweno, r_cweno, lambda_0_cweno, use_cwenoz
  public :: geom_space_r, grid_perturb
  public :: out_quad_order, out_derivatives
  ! public :: space_coefs, time_coefs
  public :: space_scale, space_origin
  public :: time_scale, time_origin
  public :: rand_coefs, rand_seed, test_function

  character(max_text_line_length) :: job_name = 'test'
  integer :: verbose_level = 0
  integer :: n_dim         = 1
  integer :: rec_degree    = 2
  integer :: n_rec_vars    = 1
  integer, dimension(3) :: n_nodes       = [9,1,1]
  integer, dimension(3) :: n_ghost       = [0,0,0]
  integer, dimension(3) :: n_skip        = [1,1,1]
  logical  :: column_scaling   = .true.
  logical  :: local_scaling    = .true.
  logical  :: use_tci          = .true.
  logical  :: use_cweno        = .false.
  logical  :: use_cwenoz       = .false.
  real(dp) :: epsilon_cweno    = 1.0e-14_dp
  real(dp) :: lambda_0_cweno   = 1.0e5_dp
  integer  :: r_cweno          = 4
  real(dp) :: geom_space_r = 1.1_dp
  real(dp) :: grid_perturb = zero
  integer  :: out_quad_order=1
  logical  :: out_derivatives=.false.

  logical :: rand_coefs    = .false.
  integer :: rand_seed     = 1
  integer :: test_function=1
  ! real(dp), dimension(:,:), allocatable :: space_coefs, time_coefs
  real(dp), dimension(:,:), allocatable :: space_scale, space_origin
  real(dp), dimension(:),   allocatable :: time_scale, time_origin

contains
  subroutine allocate_inputs()
    allocate(space_scale(n_dim,n_rec_vars))
    allocate(space_origin(n_dim,n_rec_vars))
    allocate(time_scale(n_rec_vars))
    allocate(time_origin(n_rec_vars))
  end subroutine allocate_inputs

  subroutine deallocate_inputs()
    if ( allocated(space_scale)  ) deallocate(space_scale )
    if ( allocated(space_origin) ) deallocate(space_origin)
    if ( allocated(time_scale)   ) deallocate(time_scale  )
    if ( allocated(time_origin)  ) deallocate(time_origin )
  end subroutine deallocate_inputs

end module project_inputs

module string_stuff
  implicit none
  private
  public :: generate_newline_string
  public :: progress_line, iteration_line
  public :: write_integer_tuple
  public :: line_count
  interface line_count
    module procedure line_count_i4
    module procedure line_count_i8
    module procedure line_count_r4
    module procedure line_count_r8
  end interface line_count

contains

  pure subroutine line_count_i4(source,max_count,line,cnt)
    use set_precision, only : i4
    integer(i4),  intent(in)  :: source
    integer,      intent(in)  :: max_count
    character(*), intent(in)  :: line
    integer,      intent(out) :: cnt
    integer :: i, ierr
    integer(i4), dimension(max_count) :: buffer
    cnt = 0
    do i = 1,max_count
      read(line,*,iostat=ierr) buffer(1:cnt+1)
      if (ierr/=0) exit
      cnt = cnt + 1
    end do
  end subroutine line_count_i4

  pure subroutine line_count_i8(source,max_count,line,cnt)
    use set_precision, only : i8
    integer(i8),  intent(in)  :: source
    integer,      intent(in)  :: max_count
    character(*), intent(in)  :: line
    integer,      intent(out) :: cnt
    integer :: i, ierr
    integer(i8), dimension(max_count) :: buffer
    cnt = 0
    do i = 1,max_count
      read(line,*,iostat=ierr) buffer(1:cnt+1)
      if (ierr/=0) exit
      cnt = cnt + 1
    end do
  end subroutine line_count_i8

  pure subroutine line_count_r4(source,max_count,line,cnt)
    use set_precision, only : r4
    real(r4),     intent(in)  :: source
    integer,      intent(in)  :: max_count
    character(*), intent(in)  :: line
    integer,      intent(out) :: cnt
    integer :: i, ierr
    real(r4), dimension(max_count) :: buffer
    cnt = 0
    do i = 1,max_count
      read(line,*,iostat=ierr) buffer(1:cnt+1)
      if (ierr/=0) exit
      cnt = cnt + 1
    end do
  end subroutine line_count_r4

  pure subroutine line_count_r8(source,max_count,line,cnt)
    use set_precision, only : r8
    real(r8),     intent(in)  :: source
    integer,      intent(in)  :: max_count
    character(*), intent(in)  :: line
    integer,      intent(out) :: cnt
    integer :: i, ierr
    real(r8), dimension(max_count) :: buffer
    cnt = 0
    do i = 1,max_count
      read(line,*,iostat=ierr) buffer(1:cnt+1)
      if (ierr/=0) exit
      cnt = cnt + 1
    end do
  end subroutine line_count_r8

  subroutine write_integer_tuple(integer_list,out_string,plus,delim)
    integer, dimension(:),  intent(in)  :: integer_list
    character(*),           intent(out) :: out_string
    logical,      optional, intent(in)  :: plus
    character(*), optional, intent(in)  :: delim
    integer :: j, sz
    character(1) :: p
    p = ' '
    if ( present(plus) ) then
      if ( plus ) p = '+'
    end if
    sz = size(integer_list)
    out_string=''
    if (sz<1) return
    if ( integer_list(1)>0 ) then
      write(out_string,'(A,I0)') trim(out_string)//p,integer_list(1)
    elseif ( integer_list(1)==0 ) then
      write(out_string,'(A,I0)') trim(out_string)//' ',integer_list(1)
    else
      write(out_string,'(A,I0)') trim(out_string),integer_list(1)
    end if
    if ( present(delim) ) then
      do j = 2,sz
        if ( integer_list(j)>0 ) then
          write(out_string,'(A,I0)') trim(out_string)//delim//p,integer_list(j)
        elseif ( integer_list(j)==0 ) then
          write(out_string,'(A,I0)') trim(out_string)//delim//' ',integer_list(j)
        else
          write(out_string,'(A,I0)') trim(out_string)//delim,integer_list(j)
        end if
      end do
    else
      do j = 2,sz
        if ( integer_list(j)>0 ) then
          write(out_string,'(A,I0)') trim(out_string)//', '//p,integer_list(j)
        elseif ( integer_list(j)==0 ) then
          write(out_string,'(A,I0)') trim(out_string)//',  ',integer_list(j)
        else
          write(out_string,'(A,I0)') trim(out_string)//', ',integer_list(j)
        end if
      end do
    end if
  end subroutine write_integer_tuple
  
  subroutine generate_newline_string(strings,out_fmt)
    character(*), dimension(:), intent(in)  :: strings
    character(*)              , intent(out) :: out_fmt
    integer :: j, sz
    sz = size(strings)
    out_fmt = '('//trim(strings(1))
    do j=2,sz
        out_fmt=trim(out_fmt)//',/,'//trim(strings(j))
    end do
    out_fmt=trim(out_fmt)//')'
  end subroutine generate_newline_string

  subroutine update_current_line(string)
    use iso_fortran_env, only : std_out => output_unit
    character(*), intent(in) :: string
    write(std_out,'(A)',advance='no') string
    flush(std_out)
  end subroutine update_current_line

  subroutine progress_line(string,n,n_total)
    use set_precision, only : dp
    character(*), intent(in) :: string
    integer,      intent(in) :: n, n_total
    character(*), parameter :: fmt = '(A,I0,A,I0,A,F5.1,A)'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: out_string
    write(out_string,fmt) string, n,'/',n_total, ' (', real(n,dp)/real(n_total,dp)*100.0_dp, '%)'
    call update_current_line(carriage_return//trim(out_string))
  end subroutine progress_line

  subroutine iteration_line(string,n,residual)
    use set_precision, only : dp
    character(*), intent(in) :: string
    integer,      intent(in) :: n
    real(dp), dimension(:), intent(in) :: residual
    character(*), parameter :: fmt1 = '("(A,I0,",I0,"("" "",ES18.12))")'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: fmt, out_string
    write(fmt,fmt1) size(residual)
    write(out_string,fmt) string, n, residual
    call update_current_line(carriage_return//trim(out_string))
  end subroutine iteration_line
end module string_stuff

module message
  use ISO_FORTRAN_ENV, only : error_unit
  implicit none
  private
  public :: error_message, warning_message
  public :: WARN_ALWAYS, WARN_SOMETIMES, WARN_RARELY
  integer, parameter :: WARN_ALWAYS    = 0
  integer, parameter :: WARN_SOMETIMES = 1
  integer, parameter :: WARN_RARELY    = 2
contains
  function error_message( routine_name, message ) result( err )
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: err
    err = .true.
    write(error_unit,*)
    write(error_unit,*) ' ERROR: In ' // trim(routine_name)
    write(error_unit,*) '   ', trim(message)
    write(error_unit,*) ' Stopping ...'
    call abort
    stop
  end function error_message

  function warning_message( warn_level, routine_name, message ) result( warn )
    use project_inputs,  only : verbose_level
    integer,      intent(in) :: warn_level
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: warn
    warn = .true. ! Setup
    if ( warn_level <= verbose_level ) then ! Print Warning Message
      write(error_unit,*)
      write(error_unit,*) ' WARNING: In ' // trim(routine_name)
      write(error_unit,*) '   ', trim(message)
    end if
  end function warning_message

end module message

module file_routines
  implicit none
  private
  public :: open_existing_file
  public :: merge_files
contains
  function open_existing_file( filename, append ) result(fid)
    use project_inputs, only : verbose_level
    use message,        only : warning_message, error_message, WARN_RARELY
    character(*),      intent(in) :: filename
    logical,           intent(in) :: append
    logical :: exists, err
    integer :: fid
    character(*), parameter :: routine_name = 'open_existing_file'

    inquire( file=trim(filename), exist=exists )
    if ( .not. exists ) then
      err = error_message( routine_name, ' Attempting to open '//trim(filename)//'... Failed. File does not exist. Stopping.' )
    else
      if( append ) then
        open( newunit=fid, file=trim(filename), status='old', position='append' )
      else
        open( newunit=fid, file=trim(filename), status='old' )
      end if
      err = warning_message( WARN_RARELY, routine_name, ' Attempting to open '//trim(filename)//'... Success!' )
    end if
  end function open_existing_file

  subroutine merge_files( pattern, file_name )
    character(*),               intent(in) :: pattern
    character(*),               intent(in) :: file_name
    integer :: N_files, i, fid, fid_tmp
    
    ! concatenate files
    call system('cat '//trim(pattern)//' >> '//trim(file_name))

    ! archive in case you fucked something up
    call system('tar -cf '//trim(file_name)//'.backup.tar '//trim(pattern))

    ! remove tmp files
    call system('rm '//trim(pattern))
  end subroutine merge_files
end module file_routines

module namelist_helper
  use set_constants,  only : MAX_TEXT_LINE_LENGTH
  implicit none
  private
  public :: nml_warnings, check_for_input_error
contains
  subroutine nml_warnings( nml_unit, err, name, quiet, err_tot, required )
    integer,           intent(in)    :: nml_unit
    integer,           intent(in)    :: err
    character(*),      intent(in)    :: name
    logical,           intent(in)    :: quiet
    integer,           intent(inout) :: err_tot
    logical, optional, intent(in)    :: required
    logical :: flag
    character(MAX_TEXT_LINE_LENGTH) :: line
    flag = .false.
    if (present(required)) flag = required

    if ( err < 0 ) then
      if (flag) then
          write(*,'(A)') "NML "//trim(name)//" not found"
          write(*,'(A)') "This is a required namelist!"
          err_tot = err_tot+1
      else
        if ( .not. quiet ) then
          write(*,'(A)') "NML "//trim(name)//" not found, using defaults."
        end if
      end if
      
    else if ( err > 0 ) then
      write(*,'(A)') "WARNING: Error in NML "//trim(name)//" inputs!"
      backspace(nml_unit)
      backspace(nml_unit)
      read(nml_unit,fmt='(A)') line
      write(*,'(A)') "    Invalid line in namelist: "//trim(line)
      write(*,*)
      err_tot = err_tot + 1
    end if
  end subroutine nml_warnings

  subroutine check_for_input_error( var, var_default, condition, option_error, &
                                     namelist_name, message )
    use set_precision, only : dp
    real(dp),     intent(inout) :: var
    real(dp),     intent(in)    :: var_default
    logical,      intent(in)    :: condition
    integer,      intent(inout) :: option_error
    character(*), intent(in)    :: namelist_name
    character(*), intent(in)    :: message
    if ( condition ) then
      write(*,*) ""
      write(*,'(A)') "Error in NML "//trim(namelist_name)//":"
      write(*,'(A)') "  "//trim(message)
      option_error = option_error+1
      ! set to default to avoid tripping other unrelated checks
      var = var_default
    end if
  end subroutine check_for_input_error
end module namelist_helper

module nml_project
  use namelist_helper, only : nml_warnings
  use project_inputs, only : job_name, verbose_level, n_dim, rec_degree,       &
                             n_rec_vars, n_nodes, n_ghost, n_skip,             &
                             column_scaling, local_scaling, use_tci,           &
                             use_cweno, epsilon_cweno, r_cweno, lambda_0_cweno,&
                             use_cwenoz, grid_perturb, geom_space_r,           &
                             out_quad_order, out_derivatives
  implicit none
  private
  public :: read_nml_project
  public :: write_nml_project
  namelist /project/ job_name, verbose_level, n_dim, n_rec_vars, rec_degree,   &
                     n_nodes, n_ghost, n_skip, column_scaling, local_scaling,  &
                     use_tci,                                                  &
                     use_cweno, epsilon_cweno, r_cweno, lambda_0_cweno,        &
                     use_cwenoz, grid_perturb, geom_space_r,                   &
                     out_quad_order, out_derivatives
contains
  subroutine read_nml_project( nml_unit, quiet, err_tot, option_error )
    use project_inputs, only : allocate_inputs
    use set_precision,  only : dp
    use set_constants,  only : zero, one
    integer, intent(in)    :: nml_unit
    logical, intent(in)    :: quiet
    integer, intent(inout) :: err_tot
    integer, intent(inout) :: option_error
    integer :: ierr
    ! PROJECT Namelist
    job_name = 'test'
    verbose_level = 0
    n_dim         = 1
    rec_degree    = 2
    n_rec_vars    = 1
    n_nodes       = [9,1,1]
    n_ghost       = [0,0,0]
    n_skip        = [1,1,1]
    column_scaling   = .true.
    local_scaling    = .true.
    use_tci          = .true.
    use_cweno        = .false.
    use_cwenoz       = .false.
    epsilon_cweno    = 1.0e-14_dp
    r_cweno          = 4
    lambda_0_cweno   = 1.0e5_dp
    grid_perturb  = zero
    geom_space_r  = one
    out_quad_order=1
    out_derivatives=.false.
    rewind( nml_unit )
    read( nml_unit, nml = project, iostat = ierr )
    call nml_warnings( nml_unit, ierr, 'PROJECT', quiet, err_tot )
    call check_nml_project( option_error )
    call allocate_inputs()
  end subroutine read_nml_project

  subroutine write_nml_project( nml_unit )
    integer, intent(in) :: nml_unit
    integer :: ierr
    write( nml_unit, nml = project, iostat = ierr )
  end subroutine write_nml_project
  subroutine check_nml_project( option_error )
    integer, intent(inout) :: option_error
    continue
  end subroutine check_nml_project
end module nml_project

module nml_exact
  use namelist_helper, only : nml_warnings
  use project_inputs, only : n_dim, n_rec_vars
  use project_inputs, only : rand_coefs, rand_seed, space_scale, space_origin, &
                             time_scale, time_origin, test_function
  implicit none
  private
  public :: read_nml_exact
  public :: write_nml_exact
  namelist /exact/ rand_coefs, rand_seed, space_scale, space_origin,           &
                   time_scale, time_origin, test_function
contains
  subroutine read_nml_exact( nml_unit, quiet, err_tot, option_error )
    use project_inputs, only : allocate_inputs
    use set_constants,  only : zero, one
    integer, intent(in)    :: nml_unit
    logical, intent(in)    :: quiet
    integer, intent(inout) :: err_tot
    integer, intent(inout) :: option_error
    integer :: ierr
    ! EXACT Namelist
    rand_coefs    = .false.
    rand_seed     = 1
    space_scale   = one
    space_origin  = zero
    time_scale    = one
    time_origin   = zero
    rewind( nml_unit )
    read( nml_unit, nml = exact, iostat = ierr )
    call nml_warnings( nml_unit, ierr, 'EXACT', quiet, err_tot )
    call check_nml_exact( option_error )
  end subroutine read_nml_exact
  subroutine write_nml_exact( nml_unit )
    integer, intent(in) :: nml_unit
    integer :: ierr
    write( nml_unit, nml = exact, iostat = ierr )
  end subroutine write_nml_exact
  subroutine check_nml_exact( option_error )
    integer, intent(inout) :: option_error
    continue
  end subroutine check_nml_exact
end module nml_exact

module namelist
  use namelist_helper, only : nml_warnings
  implicit none
  private
  public :: read_nml, write_nml
contains
  subroutine read_nml( )
    use set_precision,        only : dp
    use project_inputs,       only : verbose_level
    use message,              only : WARN_SOMETIMES
    use file_routines,        only : open_existing_file
    use nml_project,          only : read_nml_project
    use nml_exact,            only : read_nml_exact
    logical :: quiet
    integer :: nml_unit, err_tot, option_error

    quiet = WARN_SOMETIMES > verbose_level
    err_tot = 0
    option_error = 0
    nml_unit = open_existing_file( 'project.nml', append=.false. )
    if ( .not. quiet ) then
      write(*,*) 'Reading project.nml...'
    end if
    quiet = .false.
    call read_nml_project( nml_unit, quiet, err_tot, option_error )
    quiet = WARN_SOMETIMES > verbose_level
    call read_nml_exact( nml_unit, quiet, err_tot, option_error )
    close(nml_unit)

    if ( err_tot /= 0 .or. option_error /= 0 ) then
      write(*,*)
      write(*,*) "STOPPING: Errors in project.nml!"
      write(*,*) ""
      write(*,'(a,i10)') 'Total # of Errors found: ',(err_tot + option_error)
      write(*,*) ""
      write(*,*) "Review the above WARNINGS and fix before continuing!"
      write(*,*) "See project.nml.updated for an up-to-date version."
      write(*,*) ""
      call write_nml('updated')
      stop
    end if
  end subroutine read_nml

  subroutine write_nml(str)
    use nml_project,          only : write_nml_project
    character(*), intent(in), optional :: str
    integer       :: nml_unit, err_tot
    character(16) :: today_str
    character(12), dimension(3) :: now
    call date_and_time( now(1), now(2), now(3) )
    today_str = now(1)(1:4)//'-'//now(1)(5:6)//'-'//now(1)(7:8)//'_'//         &
                now(2)(1:2)//'.'//now(2)(3:4)
    err_tot = 0
    if (present(str)) then
      open(newunit=nml_unit, file='project.nml.'//trim(str), status='unknown')
    else
      open(newunit=nml_unit, file='project.nml.'//trim(today_str),              &
        status='unknown')
    end if
    call write_nml_project(          nml_unit )
    close(nml_unit)
  end subroutine write_nml
end module namelist

module timer_derived_type

  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: basic_timer_t

  type :: basic_timer_t
    private
    real(dp)         :: time_start   = zero
    real(dp), public :: time_elapsed = zero
  contains
    private
    procedure, public, pass :: tic => timer_tick
    procedure, public, pass :: toc => timer_tock
  end type basic_timer_t

contains

  function get_time()
    integer(kind=8) :: ticks, ticks_per_sec, max_ticks
    real(dp) :: get_time
    call system_clock( count      = ticks,                                     &
                      count_rate = ticks_per_sec,                              &
                      count_max  = max_ticks )
    if ( ticks_per_sec == 0 ) then
      get_time = zero
    else
      get_time = real(ticks,dp) / real(ticks_per_sec,dp)
    end if
  end function get_time

  subroutine timer_tick( this )
    class(basic_timer_t), intent(inout) :: this
    this%time_elapsed = zero
    this%time_start   = get_time()
  end subroutine timer_tick

  function timer_tock( this )
    class(basic_timer_t), intent(in) :: this
    real(dp)                         :: timer_tock
    timer_tock = get_time() - this%time_start
  end function timer_tock

end module timer_derived_type

module quick_sort
  implicit none
  private
  public :: sort
contains
  pure subroutine sort(array,sorted,idx)
    integer, dimension(:),           intent(in) :: array
    integer, dimension(size(array)), optional, intent(out) :: sorted
    integer, dimension(:), optional, intent(inout) :: idx
    integer, dimension(size(array)) :: idx_, sorted_
    integer :: m
    m = size(array)
    sorted_ = array
    if ( present(idx) ) then
      call qsort_1D( m, sorted_, idx )
    else
      call qsort_1D( m, sorted_, idx_ )
    end if
    if ( present(sorted) ) sorted = sorted_
  end subroutine sort

  pure recursive subroutine qsort_1D( m, A, indx )
    integer,               intent(in)    :: m
    integer, dimension(m), intent(inout) :: A
    integer, dimension(m), intent(inout) :: indx
    integer :: iq
    if ( m > 1 ) then
      call partition_1D( m, A, indx, iq )
      call qsort_1D( iq-1  , A(1:iq-1), indx(1:iq-1) )
      call qsort_1D( m-iq+1, A(iq:m)  , indx(iq:m)   )
    end if
  end subroutine qsort_1D

  pure subroutine partition_1D( m, A, indx, marker )
    integer,               intent(in)    :: m
    integer, dimension(m), intent(inout) :: A
    integer, dimension(m), intent(inout) :: indx
    integer,               intent(out)   :: marker
    integer :: i, j
    integer :: temp_indx
    integer :: x, temp_A
    x = A(1)
    i = 0
    j = m+1
    do
      do; j = j-1; if ( A(j) <= x ) exit; end do
      do; i = i+1; if ( A(i) >= x ) exit; end do
      if ( i < j ) then
        temp_A    = A(i);   temp_indx = indx(i)
        A(i)      = A(j);   indx(i)   = indx(j)
        A(j)      = temp_A; indx(j)   = temp_indx
      elseif ( i == j ) then
        marker = i+1; return
      else
        marker = i; return
      end if
    end do
  end subroutine partition_1D
end module quick_sort

module index_conversion
  implicit none
  private
  public :: in_bound
  public :: global2local, global2local_bnd, global2local_ghost
  public :: local2global, local2global_bnd, local2global_ghost
  public :: global2local_face, local2global_face
  public :: cell_face_nbors, node_cell_nbors
  public :: get_face_id_from_node_list
  public :: get_face_idx_from_id
  public :: get_reshape_indices, get_dim_order
  public :: range_intersect, bound_intersect
  public :: node_to_cell_idx, get_neighbor_idx
  public :: shift_val_to_start
  public :: get_exterior_mask
  public :: remove_duplicates_unsorted
  public :: get_ghost_cell_indices

  interface cell_face_nbors
    module procedure cell_face_nbors_lin
    module procedure cell_face_nbors_sub
  end interface cell_face_nbors

  interface node_cell_nbors
    module procedure node_cell_nbors_lin
    module procedure node_cell_nbors_sub
  end interface node_cell_nbors

  interface get_face_id_from_node_list
    module procedure get_face_id_from_node_list_
    module procedure get_face_id_from_node_list_packed
  end interface get_face_id_from_node_list
  
contains

  pure function in_bound( dim, idx, bnd_min, bnd_max )
    integer,                 intent(in) :: dim
    integer, dimension(dim), intent(in) :: idx, bnd_min, bnd_max
    logical                             :: in_bound
    in_bound =     all(idx>=bnd_min).and.all(idx<=bnd_max)                       &
              .or. all(idx<=bnd_min).and.all(idx>=bnd_max)
  end function in_bound

  pure function global2local(iG,nSub) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: nSub
    integer, dimension(size(nSub)) :: iSub
    integer :: i, nDims, p, iGtmp, iTmp
    nDims = size(nSub)
    if (nDims==1) then
      iSub(1) = iG
      return
    end if
    p = product(nSub)
    iGtmp = iG
    do i = nDims,1,-1
      p = p/nSub(i)
      iTmp = mod(iGtmp-1,p) + 1
      iSub(i) = (iGtmp-iTmp)/p + 1
      iGtmp = iTmp
    end do
  end function global2local

  pure function local2global(iSub,nSub) result(iG)
    integer, dimension(:), intent(in) :: iSub, nSub
    integer :: iG
    integer :: nDims, p, i
    nDims = size(iSub)
    p = 1
    iG = 1
    do i = 1,nDims
        iG = iG + ( iSub(i) - 1 )*p
        p = p*nSub(i)
    end do
  end function local2global

  pure function global2local_ghost(iG,nSub,nGhost) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: nSub, nGhost
    integer, dimension(size(nSub)) :: iSub, nSub2
    nSub2 = nSub + 2*nGhost
    iSub = global2local(iG,nSub2)
    iSub = iSub - nGhost
  end function global2local_ghost

  pure function local2global_ghost(iSub,nSub,nGhost) result(iG)
    integer, dimension(:), intent(in) :: iSub, nSub, nGhost
    integer, dimension(size(nSub)) :: iSub2, nSub2
    integer :: iG
    iSub2 = iSub + nGhost
    nSub2 = nSub + 2*nGhost
    iG = local2global(iSub2,nSub2)
  end function local2global_ghost

  pure function global2local_bnd(iG,lo,hi) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: lo, hi
    integer, dimension(size(lo)) :: iSub, nSub
    nSub = hi - lo + 1
    iSub = global2local(iG,nSub)
    iSub = iSub + lo - 1
  end function global2local_bnd

  pure function local2global_bnd(iSub,lo,hi) result(iG)
    integer, dimension(:), intent(in) :: iSub, lo, hi
    integer, dimension(size(iSub)) :: idx, nSub
    integer :: iG
    idx  = iSub - lo + 1
    nSub = hi - lo + 1
    iG   = local2global(idx,nSub)
  end function local2global_bnd

  pure subroutine get_ghost_cell_indices(nSub,nGhost,iGg,iGi)
    integer, dimension(:), intent(in) :: nSub, nGhost
    integer, dimension(product(nSub+2*nGhost)-product(nSub)), intent(out) :: iGg
    integer, dimension(product(nSub)), intent(out) :: iGi
    integer, dimension(product(nSub+2*nGhost)) :: iAll
    logical, dimension(product(nSub+2*nGhost)) :: mask
    integer, dimension(size(nSub)) :: sz, itmp
    integer :: iG, n_Cells, ni_Cells, ng_Cells
    integer :: j

    sz       = nSub + 2*nGhost
    n_Cells  = product(sz)
    ni_Cells = product(nSub)
    ng_Cells = n_Cells - ni_Cells

    do j = 1,n_Cells
      iAll(j) = j
    end do
    mask = .true.
    do j = 1,ni_Cells
      itmp = global2local(j,nSub)
      iG   = local2global_ghost(itmp,nSub,nGhost)
      mask(iG) = .false.
    end do
    iGg = pack(iAll,     mask)
    iGi = pack(iAll,.not.mask)

  end subroutine get_ghost_cell_indices

  pure function get_face_intervals(n_dim,n_cells) result(intervals)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,  dimension(n_dim)             :: intervals
    integer, dimension(n_dim) :: tmp
    integer :: d
    tmp = n_cells
    tmp(1) = tmp(1) + 1
    intervals(1) = product(tmp)
    do d = 2,n_dim
      tmp = n_cells
      tmp(d) = tmp(d) + 1
      intervals(d) = intervals(d-1) + product(tmp)
    end do
  end function get_face_intervals

  pure subroutine local2global_face(n_dim,n_cells,dir,local_idx,lin_face_idx)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,                   intent(in)  :: dir
    integer, dimension(n_dim), intent(in)  :: local_idx
    integer,                   intent(out) :: lin_face_idx
    integer, dimension(n_dim) :: idx_extents
    integer, dimension(n_dim) :: nsub
    idx_extents = get_face_intervals(n_dim,n_cells)
    nsub = n_cells
    nsub(dir) = nsub(dir) + 1
    lin_face_idx = ( idx_extents(dir) - idx_extents(1) ) + local2global(local_idx,nsub)
  end subroutine local2global_face

  pure subroutine global2local_face(n_dim,n_cells,lin_face_idx,dir,local_idx)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,                   intent(in)  :: lin_face_idx
    integer,                   intent(out) :: dir
    integer, dimension(n_dim), intent(out) :: local_idx
    integer, dimension(n_dim) :: idx_extents
    integer, dimension(n_dim) :: nsub
    integer                   :: shifted_lin_idx
    integer, dimension(1) :: loc
    idx_extents = get_face_intervals(n_dim,n_cells)
    loc = findloc((lin_face_idx <= idx_extents),.true.)
    dir = loc(1)
    shifted_lin_idx = lin_face_idx - ( idx_extents(dir) - idx_extents(1) )
    nsub = n_cells
    nsub(dir) = nsub(dir) + 1
    local_idx = global2local( shifted_lin_idx,nsub)
  end subroutine global2local_face

  pure subroutine node_cell_nbors_sub( dim, node_idx, cell_bnd_min, cell_bnd_max, nbor_cell_idx, n_nbors )
    integer,                        intent(in)  :: dim
    integer, dimension(dim),        intent(in)  :: node_idx
    integer, dimension(dim),        intent(in)  :: cell_bnd_min, cell_bnd_max
    integer, dimension(dim,2**dim), intent(out) :: nbor_cell_idx
    integer,                        intent(out) :: n_nbors
    integer, dimension(dim) :: sz, tmp_idx, offset, offset2, offset3
    integer :: j
    integer, dimension(:,:), allocatable :: cells

    ! if on max bound, offsets in that direction are negative only
    ! if on min bound, offsets in that direction are positive only
    ! [1,1] =>   [1,1],   [2,1],   [1,2],   [2,2]
    !       => [+0,+0], [+1,+0], [+0,+1], [+1,+1]
    ! [N,1] =>   [N,1], [N-1,1],   [N,2], [N-1,2]
    !       => [+0,+0], [-1,+0], [+0,+1], [-1,+1]
    ! [1,M] =>   [1,M],   [2,M], [1,M-1], [2,M-1]
    !       => [+0,+0], [+1,+0], [+0,-1], [+1,-1]
    ! [N,M] =>   [N,M], [N-1,M], [N,M-1], [N-1,M-1]
    !       => [+0,+0], [-1,+0], [+0,-1], [-1,-1]

    ! if on a min_bound, offsets are positive in that direction
    ! otherwise offsets are negative


    sz            = 2
    where ( cell_bnd_max - cell_bnd_min == 0 ) sz = 1
    nbor_cell_idx = 0
    n_nbors       = 0
    ! offset1       = 0
    ! where ( node_idx == cell_bnd_max+1 )
    !   offset1 = -1
    ! end where
    ! offset3   = -1
    ! where ( node_idx == 1 )
    !   offset3 =  1
    ! end where

    ! allocate( cells(dim,product(sz)))
    ! do j = 1,product(sz)
    !   cells(:,j) = global2local(j,sz)-2
    !   ! where ( cell_bnd_max - cell_bnd_min /= 0 ) cells(:,j) = cells(:,j) - 1
    !   where ( node_idx == 1 ) cells(:,j) = cells(:,j) + 1
    ! end do
    

    ! deallocate( cells )


    do j = 1,product(sz)
      offset = global2local(j,sz)-2
      where ( cell_bnd_max - cell_bnd_min == 0 ) offset = 0
      ! where ( node_idx == 1 ) offset = offset + 1
      tmp_idx = node_idx + offset
      if ( in_bound( dim, tmp_idx, cell_bnd_min, cell_bnd_max ) ) then
        n_nbors = n_nbors + 1
        nbor_cell_idx(:,n_nbors) = tmp_idx
      end if
    end do
  end subroutine node_cell_nbors_sub

  pure subroutine node_cell_nbors_lin( dim, lin_node_idx, cell_bnd_min, cell_bnd_max, lin_nbor_cell_idx, n_nbors )
    integer,                    intent(in)  :: dim
    integer,                    intent(in)  :: lin_node_idx
    integer, dimension(dim),    intent(in)  :: cell_bnd_min, cell_bnd_max
    integer, dimension(2**dim), intent(out) :: lin_nbor_cell_idx
    integer,                    intent(out) :: n_nbors
    integer, dimension(dim) :: tmp_idx
    integer, dimension(dim,2**dim) :: nbor_cell_idx
    integer :: i

    tmp_idx = global2local_bnd(lin_node_idx,cell_bnd_min,cell_bnd_max+1)
    call node_cell_nbors_sub(dim,tmp_idx,cell_bnd_min,cell_bnd_max,nbor_cell_idx,n_nbors)
    lin_nbor_cell_idx = 0
    do i = 1,n_nbors
      lin_nbor_cell_idx(i) = local2global_bnd(nbor_cell_idx(:,i),cell_bnd_min,cell_bnd_max)
    end do
  end subroutine node_cell_nbors_lin

  pure subroutine cell_face_nbors_sub( dim, idx, bnd_min, bnd_max, nbor_cell_idx, nbor_face_id, n_int )
    integer,                       intent(in) :: dim
    integer, dimension(dim),       intent(in) :: idx, bnd_min, bnd_max
    integer, dimension(dim,2*dim), intent(out) :: nbor_cell_idx
    integer, dimension(2*dim),     intent(out) :: nbor_face_id
    integer,                       intent(out) :: n_int
    integer, dimension(dim,2*dim) :: nbor_cell_idx_tmp
    integer, dimension(2*dim) :: nbor_face_id_tmp
    integer, dimension(dim) :: idx_tmp
    integer :: s, j, n_ext, cnt
    cnt   = 0
    n_int = 0
    n_ext = 0
    do j = 1,dim
      do s = -1,1,2
        cnt = cnt + 1
        idx_tmp = idx
        idx_tmp(j) = idx_tmp(j) + s
        if ( in_bound(dim,idx_tmp,bnd_min,bnd_max) ) then
            n_int = n_int + 1
            nbor_cell_idx(:,n_int) = idx_tmp
            nbor_face_id(n_int) = cnt
        else
          n_ext = n_ext + 1
          nbor_cell_idx_tmp(:,n_ext) = idx_tmp
          nbor_face_id_tmp(n_ext) = cnt
        end if
      end do
    end do
    do j = 1,n_ext
      nbor_cell_idx(:,n_int+j) = nbor_cell_idx_tmp(:,j)
      nbor_face_id(n_int+j) = nbor_face_id_tmp(j)
    end do
  end subroutine cell_face_nbors_sub

  pure subroutine cell_face_nbors_lin( dim, lin_idx, bnd_min, bnd_max, &
                                       nbor_cell_idx, nbor_face_id, n_int )
    integer,                       intent(in) :: dim, lin_idx
    integer, dimension(dim),       intent(in) :: bnd_min, bnd_max
    integer, dimension(2*dim), intent(out) :: nbor_cell_idx
    integer, dimension(2*dim), intent(out) :: nbor_face_id
    integer,                       intent(out) :: n_int
    integer, dimension(dim,2*dim) :: nbor_idx
    integer, dimension(dim) :: idx
    integer :: j
    idx = global2local_bnd(lin_idx,bnd_min,bnd_max)
    call cell_face_nbors_sub( dim, idx, bnd_min, bnd_max, nbor_idx, nbor_face_id, n_int )
    do j = 1,2*dim
      nbor_cell_idx(j) = local2global_bnd(nbor_idx(:,j),bnd_min,bnd_max)
    end do
  end subroutine cell_face_nbors_lin

  pure elemental subroutine get_face_info_from_id(face_id,dir,offset)
    integer, intent(in)  :: face_id
    integer, intent(out) :: dir, offset
    dir    = (face_id-1)/2 + 1
    offset = mod(face_id+1,2)
  end subroutine get_face_info_from_id

  pure subroutine get_face_idx_from_id(idx,face_id,dir,face_idx)
    integer, dimension(:),         intent(in) :: idx
    integer,                       intent(in)  :: face_id
    integer,                       intent(out) :: dir
    integer, dimension(size(idx)), intent(out) :: face_idx

    integer, dimension(size(idx)) :: face_offset
    integer :: offset
    call get_face_info_from_id(face_id,dir,offset)
    face_offset = 0
    face_offset(dir) = offset
    face_idx = idx + face_offset
  end subroutine get_face_idx_from_id

  ! pure function get_face_id_from_out_dir(out_dir) result(face_id)
  !   integer, dimension(:), intent(in) :: out_dir
  !   integer                           :: face_id
  !   integer, dimension(1) :: dir
  !   integer :: s
  !   dir = findloc( abs(out_dir)>0, .true.)
  !   s   = merge(1,2,out_dir(dir(1))<0)
  !   face_id = 2*(dir(1)-1) + s
  ! end function get_face_id_from_out_dir

  pure subroutine get_face_id_from_node_list_( n_dim, bc_node_idx, face_id, min_bnd, max_bnd, status )
    integer,                             intent(in)  :: n_dim
    integer, dimension(:,:),             intent(in)  :: bc_node_idx
    integer,                             intent(out) :: face_id
    integer, dimension(:),     optional, intent(in)  :: min_bnd, max_bnd
    integer, optional,                   intent(out) :: status
    integer, dimension(n_dim) :: n_lo, n_hi, min_, max_
    logical, dimension(n_dim) :: varies, lo, hi, dir
    integer :: i, swap
    if ( present(status) ) status = 0

    face_id = 0
    n_lo = bc_node_idx(1,1:n_dim)
    n_hi = bc_node_idx(2,1:n_dim)
    varies = ( (n_lo/=n_hi) )

    ! not a valid face for n_dim
    if ( count(varies) /= n_dim-1 ) then
      if ( present(status) ) status = -1
      return
    end if

    ! reorder if necessary
    dir  = ( (n_hi-n_lo)>=0 )
    do i = 1,n_dim
      if ( dir(i) ) then
        swap    = n_lo(i)
        n_lo(i) = n_hi(i)
        n_hi(i) = swap
      end if
    end do

    min_ = 1
    max_ = n_hi

    if ( present(min_bnd) ) min_ = min(min_,min_bnd(1:n_dim))
    if ( present(max_bnd) ) max_ = max(max_,max_bnd(1:n_dim))

    lo = ( n_lo == min_ )
    hi = ( n_hi == max_ )

    do i = 1,n_dim
      if ( .not. varies(i) ) then
        if ( lo(i) .and. .not.(hi(i)) ) then
          face_id = 2*(i-1)
          return
        elseif ( hi(i) .and. .not.(lo(i)) ) then
          face_id = 2*(i-1) + 1
          return
        else ! interior (?)
          if ( present(status) ) status = 1
        end if
      end if
    end do

  end subroutine get_face_id_from_node_list_

  pure subroutine get_face_id_from_node_list_packed( n_dim, bc_node_idx, face_id, min_bnd, max_bnd, status )
    integer,                             intent(in)  :: n_dim
    integer, dimension(:),         intent(in)  :: bc_node_idx
    integer,                             intent(out) :: face_id
    integer, dimension(:),     optional, intent(in)  :: min_bnd, max_bnd
    integer, optional,                   intent(out) :: status
    call get_face_id_from_node_list_( n_dim, reshape(bc_node_idx,[2,size(bc_node_idx)/2]), face_id, min_bnd=min_bnd, max_bnd=max_bnd, status=status )
  end subroutine get_face_id_from_node_list_packed

  ! subroutine find_face_info( bc_node_idx, face_label, out_dir )

  !   use set_constants,  only : IMIN_FACE, IMAX_FACE, JMIN_FACE, JMAX_FACE,     &
  !                              KMIN_FACE, KMAX_FACE
  !   use project_inputs, only : twod
  !   use message,        only : error_message
  !   !use mpi
  !   !use set_inputs,     only : world_comm2, id2, ierr2

  !   integer, dimension(:), intent(in)  :: bc_node_idx
  !   integer,               intent(out) :: face_label
  !   integer, dimension(3), intent(out) :: out_dir

  !   logical :: err

  !   character(*), parameter :: routine_name = 'bc_derived_type: find_face_label'

  !   continue

  !   if( bc_node_idx(1) == bc_node_idx(2) ) then

  !     ! TODO check if interior
  !     if ( bc_node_idx(1) == 1 ) then
  !       face_label = IMIN_FACE
  !       out_dir    = [ -1, 0, 0 ]
  !     else
  !       face_label = IMAX_FACE
  !       out_dir    = [ 1, 0, 0 ]
  !     end if

  !   else if( bc_node_idx(3) == bc_node_idx(4) ) then

  !     ! TODO check if interior
  !     if ( bc_node_idx(3) == 1 ) then
  !       face_label = JMIN_FACE
  !       out_dir    = [ 0, -1, 0 ]
  !     else
  !       face_label = JMAX_FACE
  !       out_dir    = [ 0, 1, 0 ]
  !     end if

  !   else if( .not. twod ) then

  !     if( bc_node_idx(5) == bc_node_idx(6) ) then

  !       ! TODO check if interior
  !       if ( bc_node_idx(5) == 1 ) then
  !         face_label = KMIN_FACE
  !         out_dir    = [ 0, 0, -1 ]
  !       else
  !         face_label = KMAX_FACE
  !         out_dir    = [ 0, 0, 1 ]
  !       end if

  !     else

  !       err = error_message( routine_name, 'Not a valid BC index range: zeta!' )

  !     end if

  !   else

  !     err = error_message( routine_name, 'Not a valid BC index range!' )

  !   end if

  ! end subroutine find_face_info

  pure subroutine get_neighbor_idx( dim, bnd1_min, bnd1_max, bnd2_min, bnd2_max, idx, out_idx )
    integer,                 intent(in)  :: dim
    integer, dimension(dim), intent(in)  :: bnd1_min, bnd1_max, &
                                            bnd2_min, bnd2_max
    integer, dimension(dim), intent(in)  :: idx
    integer, dimension(dim), intent(out) :: out_idx
    integer, dimension(dim) :: delta1, delta2, s1, s2, s, offset
    delta1 = bnd1_max - bnd1_min
    delta2 = bnd2_max - bnd2_min
    s1 = sign(1,delta1)
    s2 = sign(1,delta2)
    s  = s1*s2
    offset = idx - bnd1_min - s1
    out_idx = bnd2_min + s*offset + s2
  end subroutine get_neighbor_idx

  pure subroutine get_reshape_indices( sz_in, loc, sz_cnt, dir, sz_out, idx_start, idx_end )
    integer, dimension(:),           intent(in)  :: sz_in
    integer, dimension(size(sz_in)), intent(in)  :: loc
    integer,                         intent(out) :: sz_cnt
    integer,                         intent(out) :: dir
    integer, dimension(size(sz_in)), intent(out) :: sz_out
    integer, dimension(size(sz_in)), intent(out) :: idx_start
    integer, dimension(size(sz_in)), intent(out) :: idx_end
    

    logical, dimension(size(sz_in)) :: lo, hi, varies

    lo     = (loc==0)
    hi     = (loc==1)
    varies = (loc==2)
    
    sz_out    = 1
    sz_cnt    = count(varies)
    dir       = 0
    if ( sz_cnt > 0 ) dir = findloc(varies,.false.,dim=1)
    sz_out(1:sz_cnt) = pack(sz_in,varies)
    idx_start = 1
    idx_end   = 1

    where ( lo .or. varies ) idx_start = 1
    where ( hi             ) idx_start = sz_in
    where ( lo             ) idx_end   = 1
    where ( hi .or. varies ) idx_end   = sz_in

  end subroutine get_reshape_indices

  pure subroutine get_dim_order( loc, dir, end_pt )
    use quick_sort, only : sort
    integer, dimension(:),         intent(in)  :: loc
    integer, dimension(size(loc)), intent(out) :: dir
    integer, dimension(size(loc)), intent(out) :: end_pt
    logical, dimension(size(loc)) :: varies
    integer, dimension(size(loc)) :: tmp
    integer :: i, n_dim, cnt
    n_dim = size(loc)
    dir    = [(i,i=1,n_dim)]
    end_pt = 0
    where (loc==0) end_pt = -1
    where (loc==1) end_pt =  1
    varies = (loc==2)
    cnt = count(varies)
    if ( cnt == 0     ) return
    if ( cnt == n_dim ) return
    tmp = dir
    dir(1:cnt)       = pack(tmp,varies)
    dir(cnt+1:n_dim) = pack(tmp,.not.varies)
    tmp = end_pt
    end_pt(1:cnt)       = pack(tmp,varies)
    end_pt(cnt+1:n_dim) = pack(tmp,.not.varies)
  end subroutine get_dim_order



  pure elemental function range_intersect( startA, endA, startB, endB )
    integer, intent(in) :: startA, endA, startB, endB
    logical             :: range_intersect
    range_intersect = ( startA <= endB ).and.( endA >= startB )
  end function range_intersect

  pure function bound_intersect( dim, bnd1_min, bnd1_max, bnd2_min, bnd2_max )
    integer,                 intent(in) :: dim
    integer, dimension(dim), intent(in) :: bnd1_min, bnd1_max, &
                                           bnd2_min, bnd2_max
    logical                             :: bound_intersect
    bound_intersect = all( range_intersect( bnd1_min, bnd1_max,                &
                                            bnd2_min, bnd2_max ) )
  end function bound_intersect

  pure elemental subroutine node_to_cell_idx(bnd_min,bnd_max)
    integer, intent(inout) :: bnd_min, bnd_max
    if ( bnd_max > bnd_min ) then
      bnd_max = bnd_max - 1
    elseif (bnd_min > bnd_max ) then
      bnd_min = bnd_min - 1
    end if
  end subroutine node_to_cell_idx

  pure subroutine shift_val_to_start(list,idx)
    integer, dimension(:), intent(inout) :: list
    integer,               intent(in)    :: idx
    integer, dimension(size(list)) :: tmp
    integer :: i
    tmp = list
    list(1) = list(idx)
    do i = 1,idx-1
      list(i+1) = tmp(i)
    end do
  end subroutine shift_val_to_start

  pure subroutine get_exterior_mask(lo,hi,lo2,hi2,mask)
    integer, dimension(:), intent(in) :: lo, hi, lo2, hi2
    logical, dimension(product(hi-lo+1)), intent(out) :: mask
    integer, dimension(size(hi-lo+1)) :: itmp
    integer :: iG, ni_Cells
    integer :: j

    ! n_Cells  = product(hi-lo+1)
    ni_Cells = product(hi2-lo2+1)
    ! ne_Cells = n_Cells - ni_Cells

    mask = .true.
    do j = 1,ni_Cells
      ! get the interior index
      itmp = global2local_bnd(j,lo2,hi2)
      ! figure out global index in the larger array
      iG   = local2global_bnd(itmp,lo,hi)
      mask(iG) = .false.
    end do
  end subroutine get_exterior_mask

  pure subroutine remove_duplicates_unsorted(input,output,n_unique)
    integer, dimension(:),           intent(in)  :: input
    integer, dimension(size(input)), intent(out) :: output
    integer,                         intent(out) :: n_unique
    integer :: i, n_max
    n_unique = 1
    output    = 0
    output(1) = input(1)
    n_max = size(input)
    do i = 2,n_max
      if (any(output==input(i))) cycle
      n_unique = n_unique + 1
      output(n_unique) = input(i)
    end do
end subroutine remove_duplicates_unsorted

end module index_conversion

module stencil_indexing

  implicit none

  private

  public :: get_linear_face_idx
  public :: cell_offsets

  public :: get_offsets
  public :: cube_mask2dir
  public :: mask_dim_split
  public :: mask_in_bounds
  
  public :: on_3d_boundary
  public :: is_ghost_cell
  public :: idx_to_offset
  public :: sort_stencil_idx
  public :: get_interior_mask
  public :: determine_interior_stencil_count

  public :: linear_map_offsets_check
  public :: inviscid_stencil_indices_3D
  public :: viscous_offsets, muscl_offsets, center_offsets
  public :: sector_offsets, identify_sector_stencils, identify_sector_stencils_alt

  public :: get_bounding_box

  public :: get_all_interior_vertex_nbors

  integer, parameter, dimension(7) :: map_idx = [5,3,1,0,2,4,6]
  integer, parameter, dimension(3,6) :: cell_offsets = reshape(                &
                              [-1,0,0,1,0,0,0,-1,0,0,1,0,0,0,-1,0,0,1],[3,6])
contains

  pure subroutine get_all_interior_vertex_nbors(n_dim,lin_idx,blk_size,n_nbor,nbor_list,status)
    use index_conversion, only : range_intersect, in_bound, global2local, local2global
    integer,                         intent(in)  :: n_dim, lin_idx
    integer, dimension(n_dim),       intent(in)  :: blk_size
    integer,                         intent(out) :: n_nbor
    integer, dimension(3**n_dim-1),  intent(out) :: nbor_list
    integer,                         intent(out) :: status
    integer, dimension(3**n_dim-1) :: linear_offset_list
    integer, dimension(n_dim) :: idx, lo, hi, idx_tmp
    integer :: i, N_max, N_total_cells

    N_total_cells = product(blk_size)

    ! check if lin_idx is in bounds
    if ( .not.( range_intersect(1,N_total_cells,lin_idx,lin_idx) ) ) then
      n_nbor = 0
      nbor_list = 0
      status    = -1
      return
    end if

    idx = global2local(lin_idx,blk_size)
    hi = max(blk_size-2,1)
    lo = min(hi,2)
    N_max = 3**n_dim-1
    linear_offset_list = get_nearest_nbor_offset_list(n_dim)
    
    if ( in_bound(n_dim,idx,lo,hi) ) then
      n_nbor = N_max
      status    = 0 ! cell is on the interior
      do i = 1,N_max
        nbor_list(i) = local2global(get_nearest_nbor_idx(n_dim,linear_offset_list(i),idx),blk_size)
      end do
    else
      status    = 1 ! cell is on the boundary
      n_nbor = 0
      hi = blk_size
      lo = 1
      do i = 1,N_max
        idx_tmp = get_nearest_nbor_idx(n_dim,linear_offset_list(i),idx)
        ! if neighbor is in bounds
        if ( in_bound(n_dim,idx_tmp,lo,hi) ) then
          n_nbor = n_nbor + 1
          nbor_list(n_nbor) = local2global(idx_tmp,blk_size)
        end if
      end do
    end if
  end subroutine get_all_interior_vertex_nbors

  pure function get_nearest_nbor_offset_list(n_dim) result(linear_offset_list)
    integer,            intent(in) :: n_dim
    integer, dimension(3**n_dim-1) :: linear_offset_list
    integer :: i, mid, N
    N = 3**n_dim
    mid = N/2 + 1
    linear_offset_list = [(i,i=1,mid-1),(i,i=mid+1,N)]
  end function get_nearest_nbor_offset_list

  pure function get_nearest_nbor_idx(n_dim,linear_offset,idx) result(nbor)
    use index_conversion, only : global2local
    integer,                   intent(in)  :: n_dim, linear_offset
    integer, dimension(n_dim), intent(in)  :: idx
    integer, dimension(n_dim)              :: nbor
    integer, dimension(n_dim) :: nsub
    nsub = 3
    nbor = idx + global2local(linear_offset,nsub)-2
  end function get_nearest_nbor_idx

  pure function get_linear_face_idx( idx )
    integer, dimension(3),   intent(in) :: idx
    integer                             :: get_linear_face_idx
    get_linear_face_idx = map_idx( idx(1) + 2*idx(2) + 3*idx(3) + 4 )
  end function get_linear_face_idx

  pure subroutine get_offsets(mask,offsets,n)
    logical, dimension(6),   intent(in)  :: mask
    integer, dimension(3,6), intent(out) :: offsets
    integer,                 intent(out) :: n
    integer :: i
    offsets = 0
    n = count(.not.mask)
    do i = 1,3
      offsets(i,:) = pack( cell_offsets(i,:), .not.mask,offsets(i,:) )
    end do
  end subroutine get_offsets

  pure function cube_mask2dir(mask) result(dir)
    logical, dimension(6), intent(in) :: mask
    integer, dimension(3)             :: dir
    integer, dimension(6), parameter :: ds = [-1,1,-2,2,-3,3]
    integer, dimension(6) :: dds
    integer :: n, i, d, s

    dds = 0
    dir = 0
    n = count(mask)
    if (n == 0) return
    
    dds(1:n) = pack( ds, mask )
    do i = 1,n
      d = abs( dds(i) )
      if (d/=0) then
        s      = sign( 1, dds(i) )
        dir(d) = dir(d) + s
      end if
    end do
  end function cube_mask2dir

  pure subroutine mask_dim_split( mask, idx, origin )
    logical, dimension(6), intent(inout) :: mask
    integer, dimension(3), intent(in)    :: idx, origin
    mask(1) = mask(1).or.( idx(1) > origin(1) )
    mask(2) = mask(2).or.( idx(1) < origin(1) )
    mask(3) = mask(3).or.( idx(2) > origin(2) )
    mask(4) = mask(4).or.( idx(2) < origin(2) )
    mask(5) = mask(5).or.( idx(3) > origin(3) )
    mask(6) = mask(6).or.( idx(3) < origin(3) )
  end subroutine mask_dim_split

  pure subroutine mask_in_bounds( mask, idx, lo, hi )
    logical, dimension(6), intent(inout) :: mask
    integer, dimension(3), intent(in)    :: idx, lo, hi

    integer, dimension(3,2) :: bnds

    bnds(:,1) = lo
    bnds(:,2) = hi

    mask = mask .or. on_3d_boundary( idx, bnds )
  end subroutine mask_in_bounds

  pure function on_3d_boundary( idx, bnds )
    use index_conversion, only : in_bound
    integer, dimension(3),   intent(in) :: idx
    integer, dimension(3,2), intent(in) :: bnds
    logical, dimension(6)               :: on_3d_boundary

    integer, dimension(3,2) :: bnds_tmp
    integer :: d, s

     do d = 1,3
      do s = 1,2
        bnds_tmp = bnds
        bnds_tmp(d,:) = bnds(d,s)
        on_3d_boundary(2*(d-1)+s) = in_bound( 3, idx, bnds_tmp(:,1), bnds_tmp(:,2) )
      end do
    end do
  end function on_3d_boundary

  pure function is_ghost_cell(idx,lo,hi,n_ghost_cells) result(mask)
    use index_conversion, only : in_bound
    integer, dimension(3), intent(in) :: idx, lo, hi, n_ghost_cells
    logical, dimension(6)             :: mask
    integer, dimension(2), parameter :: dir = [-1,1]

    integer, dimension(3,2) :: bnds, bnds2, bnds_tmp
    integer :: d, s

    bnds(:,1)  = lo
    bnds(:,2)  = hi
    bnds2(:,1) = lo - n_ghost_cells
    bnds2(:,2) = hi + n_ghost_cells
    mask = .false.
    do d = 1,3
      do s = 1,2
        bnds_tmp = bnds2
        bnds_tmp(d,:) = bnds(d,s) + dir(s)
        bnds_tmp(d,s) = bnds(d,s) + dir(s) * n_ghost_cells(d)
        mask(2*(d-1)+s) = in_bound( 3, idx, bnds_tmp(:,1), bnds_tmp(:,2) )
      end do
    end do
  end function is_ghost_cell

  pure subroutine get_interior_mask( offset_list, idx, lo_bnd, hi_bnd, interior_mask )
    use index_conversion, only : in_bound
    integer, dimension(:,:),            intent(in)  :: offset_list
    integer, dimension(3),              intent(in)  :: idx, lo_bnd, hi_bnd
    logical, dimension(:), allocatable, intent(out) :: interior_mask
    integer :: i, N_cells
    
    N_cells = size(offset_list,2)

    allocate( interior_mask(N_cells) )
    interior_mask = .false.
    do i = 1,N_cells
      interior_mask(i) = in_bound( 3, idx + offset_list(:,i), lo_bnd, hi_bnd )
    end do
  end subroutine get_interior_mask

  pure subroutine idx_to_offset( idx_list, center_idx, offset_list )
    integer, dimension(:,:),              intent(in)  :: idx_list
    integer, dimension(size(idx_list,1)), intent(in)  :: center_idx
    integer, dimension(size(idx_list,1),size(idx_list,2)), intent(out)  :: offset_list
    integer :: i

    do i = 1,size(idx_list,2)
      offset_list(:,i) = idx_list(:,i) - center_idx
    end do
  end subroutine idx_to_offset

  pure subroutine get_bounding_box( idx_list, idx, bnd_min, bnd_max )
    integer, dimension(:,:),              intent(in)  :: idx_list
    integer, dimension(size(idx_list,1)), intent(in)  :: idx
    integer, dimension(size(idx_list,1)), intent(out) :: bnd_min, bnd_max

    bnd_min = minval(idx_list,2) + idx
    bnd_max = maxval(idx_list,2) + idx

  end subroutine get_bounding_box

  pure subroutine determine_interior_stencil_count( idx_list, lo_bnd_in, hi_bnd_in,           &
                                               lo_bnd_out, hi_bnd_out,                   &
                                               offset, n_ghost, n_gc_sten )
    integer, dimension(:,:),         intent(in)  :: idx_list
    integer, dimension(3),           intent(in)  :: lo_bnd_in, hi_bnd_in
    integer, dimension(3), optional, intent(in)  :: offset, n_ghost
    integer,               optional, intent(in)  :: n_gc_sten
    integer, dimension(3),           intent(out) :: lo_bnd_out
    integer, dimension(3),           intent(out) :: hi_bnd_out
    integer, dimension(3,size(idx_list,2)) :: offset_list
    integer, dimension(3) :: idx,lo_bnd_stencil, hi_bnd_stencil, ng
    integer :: Ncells
    
    Ncells = size(idx_list,2)
    
    if ( present(offset) ) then
      call idx_to_offset( idx_list, offset, offset_list )
    else
      offset_list = idx_list
    end if

    ng  = 0
    if ( present(n_ghost) )   ng = n_ghost
    if ( present(n_gc_sten) ) ng = max(ng-n_gc_sten,0)

    idx = 0
    call get_bounding_box( offset_list, idx, lo_bnd_stencil, hi_bnd_stencil )

    lo_bnd_out = lo_bnd_in - lo_bnd_stencil - ng
    hi_bnd_out = hi_bnd_in - hi_bnd_stencil + ng

    if ( any(lo_bnd_out > hi_bnd_out) ) then
      lo_bnd_out = 1
      hi_bnd_out = 0
    end if
  end subroutine determine_interior_stencil_count

  pure subroutine sort_stencil_idx( n_stencil, stencil )
    use quick_sort, only : sort
    use index_conversion, only : local2global_bnd
    integer,                         intent(in)    :: n_stencil
    integer, dimension(:,:),         intent(inout) :: stencil
    integer, dimension(4,n_stencil) :: stencil_tmp
    integer, dimension(4) :: min_bnd, max_bnd, range
    integer, dimension(n_stencil)   :: ind1, ind2
    integer :: i
    min_bnd = minval(stencil,dim=2)
    max_bnd = maxval(stencil,dim=2)
    range   = max_bnd - min_bnd + 1
    do i = 1,n_stencil
      ind1(i) = local2global_bnd(stencil(:,i),min_bnd,max_bnd)
      ind2(i) = i
    end do
    call sort(ind1,idx=ind2)
    stencil_tmp = stencil(:,1:n_stencil)
    do i = 1,n_stencil
      stencil(:,i) = stencil_tmp(:,ind2(i))
    end do
  end subroutine sort_stencil_idx

  pure subroutine inviscid_stencil_indices_3D(len,idx)
    integer, intent(in) :: len
    integer, dimension(3,(2*len)*3+1), intent(out) :: idx
    integer, dimension(3) :: idx_tmp
    integer :: dim, off, cnt
    idx = 0
    cnt = 1
    do dim = 1,3
      do off = -len,-1
        cnt = cnt + 1
        idx_tmp = 0
        idx_tmp(dim) = idx_tmp(dim) + off
        idx(:,cnt) = idx_tmp
      end do
      do off = 1,len
        cnt = cnt + 1
        idx_tmp = 0
        idx_tmp(dim) = idx_tmp(dim) + off
        idx(:,cnt) = idx_tmp
      end do
    end do
  end subroutine inviscid_stencil_indices_3D

  pure subroutine sector_offsets(direction,n_dim,offsets)
    use index_conversion, only : shift_val_to_start
    integer,                             intent(in)  :: direction, n_dim
    integer, dimension(n_dim,2*n_dim-1), intent(out) :: offsets
    integer, dimension(n_dim) :: d
    integer :: s, dir
    integer :: i, j, k, cnt
    offsets = 0
    dir = abs(direction)
    s   = sign(1,direction)
    do i = 1,n_dim
      d(i) = i
    end do
    call shift_val_to_start(d,dir)
    offsets(dir,:) = s
    cnt = 1
    do i = 2,n_dim
      do j = -1,1,2
        cnt = cnt + 1
        offsets(d(i),cnt) = j
      end do
    end do
  end subroutine sector_offsets

  pure subroutine identify_sector_stencils(n_dim,n_stencil,stencil_idxs,n_sec,n_sec_idx,sec_idx)
    integer, intent(in) :: n_dim, n_stencil
    integer, dimension(n_dim+1,n_stencil), intent(in) :: stencil_idxs
    integer, intent(out) :: n_sec
    integer, dimension(2*n_dim), intent(out) :: n_sec_idx
    integer, dimension(2*n_dim-1,2*n_dim), intent(out) :: sec_idx
    integer :: d, s, i, j, k
    integer, dimension(n_dim,2*n_dim-1) :: sec_off
    integer, dimension(n_dim+1) :: tmp_idx

    n_sec     = 0
    n_sec_idx = 0
    sec_idx   = 0

    ! for each face
    do d = 1,n_dim
      do s = -1,1,2
        call sector_offsets(d*s,n_dim,sec_off)
        ! for each cell in the sector stencil
        do k = 1,2*n_dim-1
          ! check against all cells in stencil
          do j = 2,n_stencil
            tmp_idx = stencil_idxs(:,1)
            tmp_idx(2:) = tmp_idx(2:) + sec_off(:,k)

            if (all(tmp_idx==stencil_idxs(:,j))) then

              ! increment the counter for this sector stencil
              n_sec_idx(n_sec+1) = n_sec_idx(n_sec+1) + 1
              ! save the location in the stencil
              sec_idx( n_sec_idx(n_sec+1), n_sec+1 ) = j
            end if
          end do
        end do
        ! if there was any intersection, then increment the counter over the sector stencils
        if ( n_sec_idx(n_sec+1) > 0 ) then
          n_sec = n_sec+1
        end if
      end do
    end do
  end subroutine identify_sector_stencils

  pure subroutine identify_sector_stencils_alt(n_dim,n_stencil,stencil_idxs,n_list,list)
    integer, intent(in) :: n_dim, n_stencil
    integer, dimension(n_dim+1,n_stencil), intent(in) :: stencil_idxs
    integer,                          intent(out) :: n_list
    integer, dimension(4*n_dim**2+1), intent(out) :: list
    integer :: d, s, i, j, k
    integer, dimension(n_dim,2*n_dim-1) :: sec_off
    integer, dimension(n_dim+1) :: tmp_idx
    integer :: n_sec, offset
    integer, dimension(2*n_dim)           :: n_sec_idx
    integer, dimension(2*n_dim-1,2*n_dim) :: sec_idx

    ! integer :: n_sec2
    ! integer, dimension(2*n_dim)           :: n_sec_idx2
    ! integer, dimension(2*n_dim-1,2*n_dim) :: sec_idx2
    ! integer, dimension(4*n_dim**2+1)      :: lis

    n_sec     = 0
    n_sec_idx = 0
    sec_idx   = 0
    ! for each face
    do d = 1,n_dim
      do s = -1,1,2
        call sector_offsets(d*s,n_dim,sec_off)
        ! for each cell in the sector stencil
        do k = 1,2*n_dim-1
          ! check against all cells in stencil
          do j = 2,n_stencil
            tmp_idx = stencil_idxs(:,1)
            tmp_idx(2:) = tmp_idx(2:) + sec_off(:,k)

            if (all(tmp_idx==stencil_idxs(:,j))) then

              ! increment the counter for this sector stencil
              n_sec_idx(n_sec+1) = n_sec_idx(n_sec+1) + 1
              ! save the location in the stencil
              sec_idx( n_sec_idx(n_sec+1), n_sec+1 ) = j - 1 ! offset, since we aren't counting the central cell
            end if
          end do
        end do
        ! if there was any intersection, then increment the counter over the sector stencils
        if ( n_sec_idx(n_sec+1) > 0 ) then
          n_sec = n_sec+1
        end if
      end do
    end do

    list      = 0
    n_list    = 0
    
    list(1)         = n_sec
    list(2:n_sec+1) = n_sec_idx(1:n_sec)
    n_list = n_sec+1
    do k = 1,n_sec
      do j = 1,n_sec_idx(k)
        n_list = n_list + 1
        list(n_list) = sec_idx(j,k)
      end do
    end do

    ! lis = list

    ! ! check that you can recover the information
    ! n_sec2     = 0
    ! n_sec_idx2 = 0
    ! sec_idx2   = 0

    ! n_sec2 = lis(1)
    ! do k = 1,lis(1)
    !   n_sec_idx2(k) = lis(1+k)
    !   offset = 1+lis(1)+sum(lis(2:k))
    !   do j = 1,lis(1+k)
    !     sec_idx2(j,k) = lis(offset+j)
    !   end do
    ! end do

    ! if ( n_sec2 /= n_sec ) then
    !   tmp_idx = 0
    ! end if

    ! do k = 1,n_sec
    !   if ( n_sec_idx2(k) /= n_sec_idx(k) ) then
    !     tmp_idx = 0
    !   end if
    !   do j = 1,n_sec_idx(k)
    !     if ( sec_idx2(j,k) /= sec_idx(j,k) ) then
    !       tmp_idx = 0
    !     end if
    !   end do
    ! end do
        

    ! tmp_idx = 0

  end subroutine identify_sector_stencils_alt

  pure subroutine viscous_offsets(direction,ndim,offsets)
    use index_conversion, only : shift_val_to_start
    integer,                         intent(in)  :: direction, ndim
    integer, dimension(:,:), allocatable, intent(out) :: offsets
    integer, dimension(ndim) :: d
    integer :: s, dir
    integer :: i, j, k, cnt
    allocate( offsets(ndim,2+2**ndim) )
    offsets = 0
    dir = abs(direction)
    s   = sign(1,direction)
    do i = 1,ndim
      d(i) = i
    end do
    call shift_val_to_start(d,dir)
    cnt = 0
    do i = 1,2
      cnt = cnt + 1
      offsets(dir,cnt) = s*mod(cnt+1,2)
    end do
    do i = 2,ndim
      do j = -1,1,2
        do k = 1,2
          cnt = cnt + 1
          offsets(dir,cnt) = s*mod(cnt+1,2)
          offsets(d(i),cnt) = j
        end do
      end do
    end do
  end subroutine viscous_offsets

  pure subroutine muscl_offsets(direction,ndim,offsets)
    integer,                 intent(in)  :: direction, ndim
    integer, dimension(:,:), allocatable, intent(out) :: offsets
    integer :: i, dir, s

    allocate( offsets(ndim,4) )
    offsets = 0
    dir = abs(direction)
    s   = sign(1,direction)

    do i = 1,4
      offsets(dir,i) = i-2+(s-1)/2
    end do
  end subroutine muscl_offsets

  pure subroutine center_offsets(direction,ndim,offsets)
    integer,                 intent(in)  :: direction, ndim
    integer, dimension(:,:), allocatable, intent(out) :: offsets
    integer :: s, dir
    integer, dimension(ndim) :: start
    integer :: i, j, cnt

    allocate( offsets(ndim,1+2*ndim) )
    offsets = 0
    start = 0
    dir = abs(direction)
    s   = sign(1,direction)
    if (dir /= 0) then
      start(dir) = s
    end if

    cnt = 1
    offsets(:,cnt) = start
    do i = 1,ndim
      do j = -1,1,2
          cnt = cnt + 1
          offsets(:,cnt) = start
          offsets(i,cnt) = offsets(i,cnt) + j
      end do
    end do
  end subroutine center_offsets

  pure subroutine linear_map_offsets_check(offsets,max_bnd,min_bnd,idx,n_valid)
    use index_conversion, only : in_bound
    integer, dimension(:,:), intent(in) :: offsets
    integer, dimension(3),   intent(in) :: max_bnd, min_bnd
    integer, dimension(size(offsets,2)), intent(out) :: idx
    integer,                             intent(out) :: n_valid
    integer, dimension(3) :: range
    logical, dimension(size(offsets,2)) :: mask

    integer :: i, Ncells

    Ncells = size(offsets,2)

    range   = max_bnd - min_bnd + 1

    !! out of bounds check
    do i = 1,Ncells
      mask(i) = in_bound(3,offsets(:,i),min_bnd,max_bnd)
    end do
    n_valid = count(mask)
    do i = 1,Ncells
      idx(i) = ( offsets(1,i) - min_bnd(1) )                                   &
             + ( offsets(2,i) - min_bnd(2) ) * range(1)                        &
             + ( offsets(3,i) - min_bnd(3) ) * range(1) * range(2)
    end do

    idx(1:n_valid) = pack(idx,mask)

  end subroutine linear_map_offsets_check

end module stencil_indexing

module stencil_cell_derived_type

  use set_precision, only : dp

  implicit none

  private
  public :: stencil_cell_t, block_info
  public :: check_neighbors

  type basic_bc_info
    integer :: block_id = -1
    integer :: face_id  = -1
    integer, dimension(3) :: bnd_min = -1
    integer, dimension(3) :: bnd_max = -1
  end type basic_bc_info

  type connected_bc_info
    type(basic_bc_info) :: self, nbor
  end type connected_bc_info

  type block_info
    integer               :: block_id = -1
    integer, dimension(3) :: Ncells   = -1
    type(connected_bc_info), allocatable, dimension(:) :: connected_bc_list
  contains
    ! procedure, public, pass :: create  => create_block_info
    procedure, public, pass :: destroy => destroy_block_info
  end type block_info

  type stencil_cell_t
    type(block_info)        :: info
    integer, dimension(4)   :: idx      = 0
    integer, dimension(4,6) :: nbor_idx = 0
    logical, dimension(6)   :: free     = .false.
    logical, dimension(6)   :: on_boundary = .false.
    integer                 :: degree   = 0
  contains
    procedure, public, pass :: create  => create_stencil_cell
    procedure, public, pass :: destroy => destroy_stencil_cell
  end type stencil_cell_t

  interface block_info
    module procedure block_info_constructor
  end interface block_info

contains

  pure function block_info_constructor( block_id, Ncells, connected_bc_list ) result(this)
    integer, intent(in) :: block_id
    integer, dimension(3), intent(in) :: Ncells
    type(connected_bc_info), dimension(:), optional, intent(in) :: connected_bc_list
    type(block_info) :: this
    call this%destroy()
    this%block_id = block_id
    this%Ncells   = Ncells

    if ( present( connected_bc_list ) ) then
      allocate( this%connected_bc_list(size(connected_bc_list)) )
      this%connected_bc_list = connected_bc_list
    else
      allocate( this%connected_bc_list(0) )
    end if
    
  end function block_info_constructor

  ! subroutine create_block_info( this, gblock )
  !   use grid_derived_type, only : grid_block
  !   class(block_info), intent(inout) :: this
  !   type(grid_block),  intent(in)    :: gblock
  !   integer :: n, cnt, n_connected

  !   this%block_id = gblock%block_id
  !   this%Ncells   = gblock%Ncells

  !   n_connected = 0
  !   do n = 1,gblock%nbounds
  !     if ( gblock%boundaries(n)%bc%is_connected ) n_connected = n_connected+1
  !   end do
  !   allocate( this%connected_bc_list(n_connected) )

  !   cnt = 0
  !   do n = 1,gblock%nbounds
  !     if ( gblock%boundaries(n)%bc%is_connected ) then
  !       cnt = cnt + 1
  !       this%connected_bc_list(cnt)%self%block_id = gblock%boundaries(n)%bc%block_id
  !       this%connected_bc_list(cnt)%self%face_id  = gblock%boundaries(n)%bc%face_label
  !       ! this%connected_bc_list(cnt)%self%bnd_min  = gblock%boundaries(n)%bc%idx_min
  !       ! this%connected_bc_list(cnt)%self%bnd_max  = gblock%boundaries(n)%bc%idx_max
  !       this%connected_bc_list(cnt)%self%bnd_min  = gblock%boundaries(n)%bc%node_idx_min
  !       this%connected_bc_list(cnt)%self%bnd_max  = gblock%boundaries(n)%bc%node_idx_max

  !       call node_to_cell_idx( this%connected_bc_list(cnt)%self%bnd_min,       &
  !                              this%connected_bc_list(cnt)%self%bnd_max )

  !       this%connected_bc_list(cnt)%nbor%block_id = gblock%boundaries(n)%bc%nbor%block_id
  !       this%connected_bc_list(cnt)%nbor%face_id  = gblock%boundaries(n)%bc%nbor%face_label
  !       ! this%connected_bc_list(cnt)%nbor%bnd_min  = gblock%boundaries(n)%bc%nbor%idx_min
  !       ! this%connected_bc_list(cnt)%nbor%bnd_max  = gblock%boundaries(n)%bc%nbor%idx_max
  !       this%connected_bc_list(cnt)%nbor%bnd_min  = gblock%boundaries(n)%bc%nbor%node_idx_min
  !       this%connected_bc_list(cnt)%nbor%bnd_max  = gblock%boundaries(n)%bc%nbor%node_idx_max

  !       call node_to_cell_idx( this%connected_bc_list(cnt)%nbor%bnd_min,       &
  !                              this%connected_bc_list(cnt)%nbor%bnd_max )
  !     end if
  !   end do

  ! end subroutine create_block_info

  pure elemental subroutine destroy_block_info( this )
    class(block_info), intent(inout) :: this
    if ( allocated(this%connected_bc_list) ) deallocate(this%connected_bc_list)
  end subroutine destroy_block_info

  pure elemental subroutine destroy_stencil_cell( this )
    class(stencil_cell_t), intent(inout) :: this
    call this%info%destroy()
  end subroutine destroy_stencil_cell

  pure subroutine create_stencil_cell( this, block_id, idx, block_info_list )
    use index_conversion, only : in_bound, get_neighbor_idx
    use stencil_indexing, only : cell_offsets, on_3d_boundary
    class(stencil_cell_t),          intent(inout) :: this
    integer,                        intent(in)    :: block_id
    integer,          dimension(3), intent(in)    :: idx
    type(block_info), dimension(:), intent(in)    :: block_info_list

    logical, dimension(6)   :: mask
    integer, dimension(3) :: bnd_min, bnd_max
    integer, dimension(3,2) :: bnds

    integer :: i, j, k

    call this%destroy()

    this%idx(1)   = block_id
    this%idx(2:4) = idx
    ! get block info
    do i = 1,size(block_info_list)
      if ( block_info_list(i)%block_id == this%idx(1) ) then
        this%info = block_info_list(i)
        exit
      end if
    end do
    ! check if the current cell is on any of the block boundaries
    bnd_min = [1,1,1]
    bnd_max = this%info%Ncells
    bnds(:,1) = bnd_min
    bnds(:,2) = bnd_max
    mask = on_3d_boundary( this%idx(2:4), bnds )
    do i = 1,6
      if ( mask(i) ) then
        ! assume out-of-bounds for now
        this%free(i) = .false.
        ! find corresponding connected boundary if it exists
        do j = 1,size( this%info%connected_bc_list )
          ! try this BC ...
          if ( this%info%connected_bc_list(j)%self%face_id == i ) then
    associate( bnd_min1 => this%info%connected_bc_list(j)%self%bnd_min,        &
               bnd_max1 => this%info%connected_bc_list(j)%self%bnd_max )
            ! ... if the bounds match
            if ( in_bound( 3, this%idx(2:4), bnd_min1, bnd_max1 ) ) then
              ! get the neighbor
      associate( bnd_min2 => this%info%connected_bc_list(j)%nbor%bnd_min,      &
                 bnd_max2 => this%info%connected_bc_list(j)%nbor%bnd_max )
              this%nbor_idx(1,i) = this%info%connected_bc_list(j)%nbor%block_id
              call get_neighbor_idx( 3, bnd_min1, bnd_max1,                    &
                                        bnd_min2, bnd_max2,                    &
                                        this%idx(2:4), this%nbor_idx(2:4,i) )
              ! check if the neighbor is in bounds
              do k = 1,size( block_info_list )
                ! find the corresponding neighboring block dimensions
                if (                  block_info_list(k)%block_id ==           &
                     this%info%connected_bc_list(j)%nbor%block_id    ) then
                  this%free(i) = in_bound( 3, this%nbor_idx(2:4,i), bnd_min,   &
                                              block_info_list(k)%Ncells )
                  exit
                end if
              end do
      end associate
            end if
    end associate  
          end if
        end do
      else
        ! same block, simple offset
        this%nbor_idx(:,i) = this%idx
        this%nbor_idx(2:4,i) = this%nbor_idx(2:4,i) + cell_offsets(:,i)
        this%free(i) = in_bound( 3, this%nbor_idx(2:4,i), bnd_min, bnd_max )
      end if
    end do
    this%on_boundary = .not.this%free

  end subroutine create_stencil_cell

  pure subroutine check_neighbors( stencil, stencil_idx, n_cells )
    type(stencil_cell_t), dimension(:), intent(inout) :: stencil
    integer,                            intent(in)    :: stencil_idx, n_cells
    integer, dimension(4) :: idx_tmp
    integer :: i, j, k

    do j = 1,n_cells
      if ( j==stencil_idx ) cycle
      do i = 1,6
        if ( stencil(stencil_idx)%free(i) ) then
          idx_tmp = stencil(stencil_idx)%nbor_idx(:,i)
          if ( all( stencil(j)%idx == idx_tmp ) ) then
            ! update the mask on the current cell
            stencil(stencil_idx)%free(i) = .false.
            stencil(stencil_idx)%degree = min( stencil(stencil_idx)%degree,    &
                                               stencil(j)%degree + 1 )
            ! and on the jth cell
            do k = 1,6
              if ( all( stencil(j)%nbor_idx(:,k) ==                            &
                        stencil(stencil_idx)%idx ) ) then
                stencil(j)%free(k) = .false.
                exit
              end if
            end do
          end if
        end if
      end do
    end do
  end subroutine check_neighbors

end module stencil_cell_derived_type

module stencil_growing_routines

  use set_precision, only : dp
  use set_constants, only : zero, large

  implicit none

  private

  public :: grow_stencil_basic, get_max_degree

  character(*), parameter :: FMT = '("iter: ",I4," start: (",3(I3),"), '//     &
                                   'shift: (",3(I3),"), end: (",3(I3),")")'
  character(*), parameter :: msg_FMT = '("Warning: Could not fill stencil'//   &
                                       ' to requested size, n =",I4)'

  contains

  pure subroutine grow_stencil_basic( block_id, idx, N_cells, sz_in, sz_out, nbor_block, nbor_idx, nbor_degree, on_boundary, n_sec, sec_idx )
    use stencil_cell_derived_type, only : block_info
    use index_conversion,          only : local2global_bnd
    use stencil_indexing,          only : identify_sector_stencils, identify_sector_stencils_alt
    integer,                              intent(in)  :: block_id
    integer, dimension(3),                intent(in)  :: idx, N_cells
    integer,                              intent(in)  :: sz_in
    integer,                              intent(out) :: sz_out
    integer, dimension(6*sz_in),          intent(out) :: nbor_block, nbor_idx, nbor_degree
    logical, dimension(6,6*sz_in), optional, intent(out) :: on_boundary
    integer,                       optional, intent(out) :: n_sec
    integer, dimension(37),        optional, intent(out) :: sec_idx
    type(block_info), dimension(1) :: bi
    integer, dimension(4,6*sz_in) :: idx_list
    integer :: i

    integer                 :: n_sec_
    integer, dimension(37)  :: sec_idx_

    bi(1) = block_info(block_id,N_cells)
    call grow_stencil_new_connected_block( block_id, idx, bi, sz_in, sz_out, idx_list, nbor_degree, on_boundary=on_boundary )
    call bi%destroy()

    n_sec_     = 0
    sec_idx_   = 0
    if ( present(n_sec).or.present(sec_idx)) then
      ! call identify_sector_stencils(3,sz_out,idx_list(:,1:sz_out),n_sec_,n_sec_idx_,sec_idx_)
      call identify_sector_stencils_alt(3,sz_out,idx_list(:,1:sz_out),n_sec_,sec_idx_)
      if ( present(n_sec)     ) n_sec     = n_sec_
      if ( present(sec_idx)   ) sec_idx   = sec_idx_
    end if

    nbor_block = 0
    nbor_idx   = 0
    do i = 1,sz_out
      nbor_block(i) = idx_list(1,i)
      nbor_idx(i)   = local2global_bnd( idx_list(2:4,i), [1,1,1], N_cells )
    end do

  end subroutine grow_stencil_basic

  pure function get_max_degree( block_id, idx, N_cells, sz_in ) result(max_degree)
    use stencil_cell_derived_type, only : block_info, stencil_cell_t
    integer,                              intent(in)  :: block_id
    integer, dimension(3),                intent(in)  :: idx, N_cells
    integer,                              intent(in)  :: sz_in
    integer                                           :: max_degree
    type(stencil_cell_t), dimension(6*sz_in) :: stencil
    integer :: sz_out
    call build_stencil_connected_block( block_id, idx, sz_in, [block_info(block_id,N_cells)], &
                                        sz_out, stencil, balanced=.true. )
    max_degree = maxval(stencil%degree)
  end function get_max_degree

  pure subroutine grow_stencil_new_connected_block( block_id, idx, block_info_list, sz_in, sz_out, idx_list, degree, on_boundary )
    use stencil_indexing, only : sort_stencil_idx
    use stencil_cell_derived_type, only : block_info, stencil_cell_t
    integer,                              intent(in)  :: block_id
    integer, dimension(3),                intent(in)  :: idx
    type(block_info), dimension(:),       intent(in)  :: block_info_list
    integer,                              intent(in)  :: sz_in
    integer,                              intent(out) :: sz_out
    integer, dimension(4,6*sz_in),        intent(out) :: idx_list
    integer, dimension(6*sz_in),          intent(out) :: degree
    logical, dimension(6,6*sz_in), optional, intent(out) :: on_boundary
    type(stencil_cell_t), dimension(6*sz_in) :: stencil
    integer :: i
    logical :: balanced
    balanced = .true.
    
    call build_stencil_connected_block( block_id, idx, sz_in, block_info_list, &
                                        sz_out, stencil, balanced=.true. )
    idx_list = 0
    degree   = 0
    do i = 1,sz_out
      idx_list(:,i)    = stencil(i)%idx
      degree(i)        = stencil(i)%degree
    end do
    if (present(on_boundary)) then
      on_boundary = .false.
      do i = 1,sz_out
        on_boundary(:,i) = stencil(i)%on_boundary
      end do
    end if

  end subroutine grow_stencil_new_connected_block

  pure subroutine build_stencil_connected_block( block_id, idx, n_stencil,          &
                                            block_info_list, n_out, stencil, balanced )
    use message,         only : warning_message
    use project_inputs,  only : verbose_level
    use stencil_indexing, only : cell_offsets, get_linear_face_idx
    use stencil_cell_derived_type, only : stencil_cell_t, block_info, check_neighbors

    integer,                                      intent(in)  :: block_id
    integer,              dimension(3),           intent(in)  :: idx
    integer,                                      intent(in)  :: n_stencil
    type(block_info),     dimension(:),           intent(in)  :: block_info_list
    integer,                                      intent(out) :: n_out
    type(stencil_cell_t), dimension(6*n_stencil), intent(out) :: stencil
    logical,              optional,               intent(in)  :: balanced
    integer :: n, i, cnt, in_it, out_it, min_degree, current_min_degree
    integer :: stencil_idx, face_idx, out_it_max, in_it_max
    integer, dimension(4) :: idx1
    character(*), parameter :: name = 'build_stencil_connected_block'
    character(200) :: msg
    logical :: ierr, flag

    if ( balanced ) then
      out_it_max  = 6*n_stencil
      in_it_max   = 6*n_stencil
    else
      out_it_max  = n_stencil
      in_it_max   = 1
    end if

    ! parent cell
    n         = 1
    idx1(1)   = block_id
    idx1(2:4) = idx
    call stencil(n)%create( block_id, idx, block_info_list )

    flag = .false.
    out_it = 0
    do while ( ( n < n_stencil ).and.( out_it < out_it_max ) )
      out_it = out_it + 1
      ! update masks and degree
      do i = 1,n
        call check_neighbors( stencil, i, n )
      end do
      ! iterate through cells and calculate minimum degree
      call get_distance( stencil, n, stencil_idx, face_idx, min_degree )

      cnt = 0
      in_it = 0
      current_min_degree = min_degree
      do while ( ( min_degree == current_min_degree ).and.(in_it < in_it_max) )

        in_it = in_it + 1

        if (stencil_idx == -1) then
          write(msg,msg_FMT) n
          n_out = n
          ! ierr = warning_message(verbose_level,name,msg)
          exit
        end if
        if ( .not.any(stencil(stencil_idx)%free) ) then
          exit
        end if

        idx1 = stencil(stencil_idx)%nbor_idx(:,face_idx)

        ! increment the counter
        cnt = cnt + 1

        ! add the new cell
        call stencil(n+cnt)%create( idx1(1), idx1(2:4), block_info_list )

        ! set the degree
        stencil(n+cnt)%degree = min_degree + 1

        ! check for any edge cases
        do i = 1,n+cnt
          call check_neighbors( stencil, i, n+cnt )
        end do
        ! iterate through cells and calculate new degrees
        call get_distance( stencil, n, stencil_idx, face_idx, min_degree )
      end do
      ! increment the counter
      n = n + cnt
      if ( flag ) exit
    end do
    n_out = n
  end subroutine build_stencil_connected_block

  pure subroutine get_distance( stencil, n_cells, stencil_idx, face_idx, min_degree )
    use stencil_cell_derived_type, only : stencil_cell_t
    use stencil_indexing, only : mask_in_bounds, get_offsets, get_linear_face_idx
    type(stencil_cell_t), dimension(:), intent(inout) :: stencil
    integer,                            intent(in)    :: n_cells
    integer,                            intent(out)   :: stencil_idx, face_idx, min_degree
    
    integer,  dimension(6*n_cells) :: degree
    integer,  dimension(6*n_cells) :: stencil_indices
    integer,  dimension(6*n_cells) :: mask_indices
    logical,  dimension(6*n_cells) :: min_mask
    integer,  dimension(3)   :: idx
    integer,  dimension(1)   :: min_idx, degree_tmp
    integer :: i, j, n, cnt, block_id

    stencil_idx = -1
    face_idx    = -1
    min_degree  = 6*n_cells
    degree(:)          = 1
    stencil_indices(:) = 1

    cnt = 0
    ! iterate through cells and calculate distances
    do j = 1,n_cells
      if ( .not. any( stencil(j)%free ) ) cycle
      ! for each neighbor
      do i = 1,6
        if ( stencil(j)%free(i) ) then
          cnt = cnt + 1
          degree(cnt) = stencil(j)%degree
          min_degree  = minval( degree(1:cnt) )
          block_id    = stencil(j)%nbor_idx(1,i)
          idx         = stencil(j)%nbor_idx(2:4,i)
          stencil_indices(cnt) = j
          mask_indices(cnt)    = i
        end if
      end do
    end do

    if (cnt > 0) then
      ! find the 1st minimum index
      min_idx = minloc( degree(1:cnt) )
        
      ! Now grab any other indices that match this distance
      degree_tmp = degree( min_idx(1) )
      min_mask = ( degree == degree_tmp(1) )
      
      n = count(min_mask)
      stencil_indices(1:n) = pack(stencil_indices,min_mask)
      mask_indices(1:n)    = pack(mask_indices,   min_mask)

      face_idx    = mask_indices( min_idx(1) )
      stencil_idx = stencil_indices( min_idx(1) )
      min_degree  = degree( min_idx(1) )
    end if

  end subroutine get_distance

end module stencil_growing_routines

module combinatorics
  implicit none
  private
  public :: nchoosek
  public :: get_exponents
  public :: n_polytopes
  public :: get_hypercube_vertex_coord, get_hypercube_face_vertex_coord
contains

  pure function nchoosek( n, k ) result( c )
    integer, intent(in) :: n, k
    integer             :: c
    integer :: i
    c = 0
    if (k>n) return

    c = 1
    do i = 1, min(n-k,k)
      c = c * ( n - (i-1) )
      c = c / i
    end do
  end function nchoosek

  pure subroutine get_exponents(n_dim,degree,n_terms,exponents,idx,diff_idx)
    use index_conversion, only : global2local
    integer, intent(in) :: n_dim, degree, n_terms
    integer, dimension(n_dim,n_terms), intent(out) :: exponents
    integer, dimension(0:degree),      intent(out) :: idx
    integer, dimension(n_dim,n_terms), optional, intent(out) :: diff_idx
    integer :: curr_total_degree, i, j, cnt, N_full_terms
    integer, dimension(n_dim) :: tmp_exp, nsub
    cnt = 0
    do curr_total_degree = 0,degree
      ! idx(curr_total_degree+1) = cnt + 1
      nSub = curr_total_degree + 1
      N_full_terms = (curr_total_degree+1) ** n_dim
      do j = 0,N_full_terms
        tmp_exp = global2local(j+1,nsub)-1
        if ( sum(tmp_exp) == curr_total_degree ) then
          cnt = cnt + 1
          exponents(:,cnt) = tmp_exp
        end if
      end do
      idx(curr_total_degree) = cnt
      tmp_exp = 0
    end do

    ! determine corresponding gradient terms of a given term
    if (present(diff_idx)) then
      diff_idx = -1 ! last terms (idx(degree)+1:idx(degree+1)) are not defined
      if ( degree==0) return
      do j = 1,idx(degree-1)
        tmp_exp = exponents(:,j)
        curr_total_degree = sum(tmp_exp)
        cnt = 0
        do i = idx(curr_total_degree)+1,idx(curr_total_degree+1)
          if ( sum( abs( exponents(:,i) - tmp_exp ) )==1 ) then
            cnt = cnt + 1
            diff_idx(cnt,j) = i
          end if
        end do
      end do
    end if
  end subroutine get_exponents

  pure function n_polytopes(n_dim,i)
    integer, intent(in) :: n_dim, i
    integer             :: n_polytopes
    n_polytopes = 2**(n_dim-i)*nchoosek(n_dim,i)
  end function n_polytopes

  pure elemental function get_hypercube_vertex_coord( sz_i, dim_i, n ) result(v)
    integer, intent(in) :: sz_i, dim_i, n
    integer             :: v
    v = ibits(n-1,dim_i-1,1) * (sz_i-1) + 1
  end function get_hypercube_vertex_coord

  pure elemental function get_hypercube_face_vertex_coord(sz_i,dim_i,dir,n,lo) result(v)
    integer,               intent(in) :: sz_i, dim_i, dir, n
    logical,               intent(in) :: lo
    integer                           :: v
    v = merge( merge( 1, sz_i, lo ), ibits(n-1, merge( max(dim_i-1,0),         &
                                                       max(dim_i-2,0),         &
                                                       dim_i < dir ),1) *      &
                                                       ( sz_i - 1 ) + 1,       &
                                                       dim_i==dir )
  end function get_hypercube_face_vertex_coord

end module combinatorics

module math
  use set_precision, only : dp
  implicit none
  private
  public :: cross_product, vector_norm
  public :: LUdecomp, LUsolve, det_2x2, det_3x3
  public :: mat_inv_2x2, mat_inv_3x3
  public :: linear_solve, mat_inv
  public :: LegendrePolynomialAndDerivative, LegendreGaussNodesAndWeights
  public :: maximal_extents
  public :: rand_int_in_range, rand_coord_in_range
  public :: compute_pseudo_inverse
  public :: careful_divide
  public :: reflect_through_hyperplane, closest_point_on_hyperplane
  public :: dist_to_2_pt_line, dist_to_3_pt_plane
  public :: is_colinear, pt_in_line, pt_in_triangle

  interface LUsolve
    module procedure LUsolve_single_rhs
    module procedure LUsolve_multiple_rhs
  end interface
contains

  pure elemental function careful_divide(num,den) result(q)
    use set_constants, only : near_zero
    real(dp), intent(in) :: num, den
    real(dp)             :: q
    q = num / merge( sign(near_zero,den), den, abs(den)<near_zero )
  end function careful_divide


  impure elemental function rand_int_in_range(lo,hi) result(num)
    integer, intent(in) :: lo, hi
    integer             :: num
    real(dp) :: harvest
    call random_number(harvest)
    num = nint( harvest*real(hi-lo,dp) + real(lo,dp) )
  end function rand_int_in_range

  function rand_coord_in_range(dim,lo,hi) result(coord)
    integer,                  intent(in) :: dim
    real(dp), dimension(dim), intent(in) :: lo, hi
    real(dp), dimension(dim)             :: coord
    real(dp), dimension(dim) :: harvest
    call random_number(harvest)
    coord = harvest*(hi-lo) + lo
  end function rand_coord_in_range

  pure function cross_product( vec1, vec2 )
    real(dp), dimension(3), intent(in) :: vec1, vec2
    real(dp), dimension(3)             :: cross_product
    cross_product(1) =  ( vec1(2)*vec2(3) - vec1(3)*vec2(2) )
    cross_product(2) = -( vec1(1)*vec2(3) - vec1(3)*vec2(1) )
    cross_product(3) =  ( vec1(1)*vec2(2) - vec1(2)*vec2(1) )
  end function cross_product

  pure function det_3x3( mat )
    real(dp), dimension(3,3), intent(in) :: mat
    real(dp)                             :: det_3x3
    det_3x3 = mat(1,1)*(mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)) &
            - mat(1,2)*(mat(2,1)*mat(3,3)-mat(2,3)*mat(3,1)) &
            + mat(1,3)*(mat(2,1)*mat(3,2)-mat(2,2)*mat(3,1))
  end function det_3x3

  pure function det_2x2( mat )
    real(dp), dimension(2,2), intent(in) :: mat
    real(dp)                             :: det_2x2
    continue
    det_2x2 = mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1)
  end function det_2x2

  pure subroutine mat_inv_1x1( mat, inv, status )
    use set_constants, only : zero, near_zero
    real(dp), dimension(1,1), intent(in)  :: mat
    real(dp), dimension(1,1), intent(out) :: inv
    integer,  optional,       intent(out) :: status
    real(dp) :: det

    if (present(status)) status = 0
    inv(1,1) =  mat(1,1)

    det = mat(1,1)

    if ( abs(det) > zero ) then
      inv = inv/det
    end if

    if ( present(status) ) then
      if (abs(det) <= near_zero ) status = -1
    end if

  end subroutine mat_inv_1x1

  pure subroutine mat_inv_2x2( mat, inv, status )
    use set_constants, only : zero, near_zero
    real(dp), dimension(2,2), intent(in)  :: mat
    real(dp), dimension(2,2), intent(out) :: inv
    integer,  optional,       intent(out) :: status
    real(dp) :: det

    if (present(status)) status = 0
    inv      = zero
    inv(1,1) =  mat(2,2)
    inv(2,1) = -mat(2,1)
    inv(1,2) = -mat(1,2)
    inv(2,2) =  mat(1,1)

    det = ( mat(1,1) * mat(2,2) - mat(1,2)*mat(2,1) )

    if ( abs(det) > zero ) then
      inv = inv/det
    end if

    if ( present(status) ) then
      if (abs(det) <= near_zero ) status = -1
    end if

  end subroutine mat_inv_2x2

  pure subroutine mat_inv_3x3( mat, inv, status )

    use set_constants, only : zero, near_zero

    real(dp), dimension(3,3), intent(in)  :: mat
    real(dp), dimension(3,3), intent(out) :: inv
    integer,  optional,       intent(out) :: status
    real(dp) :: det
    if (present(status)) status = 0

    inv(1,1) = mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)
    inv(2,1) = mat(2,3)*mat(3,1)-mat(2,1)*mat(3,3)
    inv(3,1) = mat(2,1)*mat(3,2)-mat(2,2)*mat(3,1)

    inv(1,2) = mat(1,3)*mat(3,2)-mat(1,2)*mat(3,3)
    inv(2,2) = mat(1,1)*mat(3,3)-mat(1,3)*mat(3,1)
    inv(3,2) = mat(1,2)*mat(3,1)-mat(1,1)*mat(3,2)

    inv(1,3) = mat(1,2)*mat(2,3)-mat(1,3)*mat(2,2)
    inv(2,3) = mat(1,3)*mat(2,1)-mat(1,1)*mat(2,3)
    inv(3,3) = mat(1,1)*mat(2,2)-mat(1,2)*mat(2,1)

    det = det_3x3(mat)

    if ( abs(det) > zero ) then
      inv = inv/det
    end if

    if ( present(status) ) then
      if (abs(det) <= near_zero ) status = -1
    end if

  end subroutine mat_inv_3x3

  pure subroutine mat_inv_LU( m, mat, inv, status )
    use set_constants, only : zero, one
    integer,                  intent(in) :: m
    real(dp), dimension(m,m), intent(in) :: mat
    real(dp), dimension(m,m), intent(out) :: inv
    integer,  optional,       intent(out) :: status
    real(dp), dimension(m,m) :: LU, P, bin
    integer :: i, stat
    if (present(status)) status = 0
    call LUdecomp(LU, P, mat, m, status=stat )
    if (present(status)) status = stat
    inv = zero
    if ( stat /= 0 ) return
    bin = zero
    do i = 1,m
      bin(i,i) = one
    end do
    call LUsolve_multiple_rhs( inv, LU, P, bin, m, m )
  end subroutine mat_inv_LU

  pure subroutine mat_inv(mat,inv,status)
    use set_constants, only : zero
    real(dp), dimension(:,:),                     intent(in)  :: mat
    real(dp), dimension(size(mat,1),size(mat,2)), intent(out) :: inv
    integer,  optional,                           intent(out) :: status
    integer :: m, n
    inv = zero
    if (present(status)) status = 0
    
    m = size(mat,1)
    n = size(mat,2)

    if ( m /= n ) then
      if (present(status)) status = -1
      return
    end if

    select case(m)
    case(0)
      if (present(status)) status = -2
    case(1)
      call mat_inv_1x1(mat,inv,status=status)
    case(2)
      call mat_inv_2x2(mat,inv,status=status)
    case(3)
      call mat_inv_3x3(mat,inv,status=status)
    case default
      call mat_inv_LU(m,mat,inv,status=status)
    end select
  end subroutine mat_inv

  pure subroutine linear_solve_LU( m, A, b, x, status )
    use set_constants, only : zero, one
    integer,                  intent(in)  :: m
    real(dp), dimension(m,m), intent(in)  :: A
    real(dp), dimension(m),   intent(in)  :: b
    real(dp), dimension(m),   intent(out) :: x
    integer,  optional,       intent(out) :: status
    real(dp), dimension(m,m) :: LU, P
    integer :: stat
    if (present(status)) status = 0
    call LUdecomp(LU, P, A, m, status=stat )
    if (present(status)) status = stat
    x = zero
    if ( stat /= 0 ) return
    call LUsolve_single_rhs( x, LU, P, b, m )
  end subroutine linear_solve_LU

  pure subroutine linear_solve_1x1(A,b,x,status)
    use set_constants, only : zero, near_zero
    real(dp), dimension(1,1), intent(in)  :: A
    real(dp), dimension(1),   intent(in)  :: b
    real(dp), dimension(1),   intent(out) :: x
    integer,  optional,       intent(out) :: status
    real(dp) :: det
    if ( present(status) ) status = 0
    x = b
    det = A(1,1)
    if ( abs(det) > zero ) then
      x = b/det
    else
      if ( present(status) ) status = -1
    end if

    if ( present(status) ) then
      if (abs(det) <= near_zero ) status = -1
    end if
  end subroutine linear_solve_1x1

  pure subroutine linear_solve_2x2(A,b,x,status)
    use set_constants, only : zero, near_zero
    real(dp), dimension(2,2), intent(in)  :: A
    real(dp), dimension(2),   intent(in)  :: b
    real(dp), dimension(2),   intent(out) :: x
    integer,  optional,       intent(out) :: status
    real(dp) :: det
    if ( present(status) ) status = 0
    x(1) = A(2,2)*b(1) - A(1,2)*b(2)
    x(2) = A(1,1)*b(2) - A(2,1)*b(1)
    det = det_2x2(A)
    if ( abs(det) > zero ) then
      x = x/det
    else
      if ( present(status) ) status = -1
    end if
    if ( present(status) ) then
      if (abs(det) <= near_zero ) status = -1
    end if
  end subroutine linear_solve_2x2

  pure subroutine linear_solve(A,b,x,status)
    use set_constants, only : zero
    real(dp), dimension(:,:),         intent(in)  :: A
    real(dp), dimension(size(A,1)), intent(in)  :: b
    real(dp), dimension(size(A,1)), intent(out) :: x
    integer,  optional,               intent(out) :: status
    integer :: m, n
    x = zero
    if (present(status)) status = 0
    
    m = size(A,1)
    n = size(A,2)

    if ( m /= n ) then
      if (present(status)) status = -2
      return
    end if

    select case(m)
    case(1)
      ! x(1) = b(1)/A(1,1)
      call linear_solve_1x1(A,b,x,status=status)
    case(2)
      call linear_solve_2x2(A,b,x,status=status)
    case default
      call linear_solve_LU(m,A,b,x,status=status)
    end select
  end subroutine linear_solve


  pure function vector_norm( vector )
    use set_precision, only : dp
    use set_constants, only : zero
    real(dp), dimension(:), intent(in) :: vector
    real(dp)                           :: vector_norm
    integer :: i
    vector_norm = zero
    do i = 1, size(vector)
      vector_norm = vector_norm + vector(i)**2
    end do
    vector_norm = sqrt( vector_norm )
  end function vector_norm

  pure subroutine LUdecomp( LU, P, A, m, status )
    use set_precision, only : dp
    use set_constants, only : zero, one, near_zero
    real(dp), dimension(m,m), intent(out) :: LU,P
    real(dp), dimension(m,m), intent(in)  :: A
    integer,                  intent(in)  :: m
    integer,  optional,       intent(out) :: status
    real(dp), dimension(m) :: ctemp1, LUtemp
    integer  :: col, row, maxi, ipr
    real(dp) :: factor
    if (present(status)) status = 0
    LU = A
    P = zero
    do col = 1,m
      P(col,col) = one
    end do
    do col = 1,m-1
      maxi=maxloc(abs(LU(col:m,col)),1) !row pivot
      ipr=maxi+col-1
      if (ipr.ne.col) then
        ctemp1 = LU(ipr,:)
        LU(ipr,:) = LU(col,:)
        LU(col,:) = ctemp1
        ctemp1 = P(ipr,:)
        P(ipr,:) = P(col,:)
        P(col,:) = ctemp1
      end if
      if ( abs(LU(col,col)) > zero ) then
        do row = col+1,m
          factor = LU(row,col)/LU(col,col)
          LUtemp(col+1:m) = LU(row,col+1:m) - factor*LU(col,col+1:m)
          LU(row,col+1:m) = LUtemp(col+1:m)
          LU(row,col) = factor
        end do
      else
        if (present(status)) status = -1
      end if
      if ( present(status) ) then
        if (abs(LU(col,col)) <= near_zero ) status = -1
      end if
    end do
  end subroutine LUdecomp

  pure subroutine LUsolve_single_rhs( x, LU, P, bin, m )
    use set_precision, only : dp
    real(dp), dimension(m),   intent(out) :: x
    real(dp), dimension(m,m), intent(in)  :: LU, P
    real(dp), dimension(m),   intent(in)  :: bin
    integer,                  intent(in)  :: m 
    integer :: i, row
    real(dp), dimension(m) :: b, d
    b = matmul(P,bin) ! Permute b matrix
    d(1) = b(1) ! Forward substitution
    do row = 2,m
      d(row) = b(row) - sum( LU(row,1:row-1)*d(1:row-1) )
    end do
    x(m) = d(m)/LU(m,m) ! Backward substitution
    do i = 1,m-1
      row = m-i
      x(row) = ( d(row) - sum( LU(row,row+1:m)*x(row+1:m) ) ) / LU(row,row)
    end do
  end subroutine LUsolve_single_rhs

  pure subroutine LUsolve_multiple_rhs(x,LU,P,bin,m,n_rhs)
    real(dp), dimension(m,n_rhs), intent(out) :: x
    real(dp), dimension(m,m),     intent(in)  :: LU, P
    real(dp), dimension(m,n_rhs), intent(in)  :: bin
    integer,                      intent(in)  :: m, n_rhs
    integer :: n
    do n = 1, n_rhs
      call LUsolve_single_rhs(x(:,n),LU,P,bin(:,n),m)
    end do
  end subroutine LUsolve_multiple_rhs

  elemental subroutine LegendrePolynomialAndDerivative(N,x,LN,dLN)
    use set_constants, only : zero, one, two
    integer, intent(in) :: N
    real(dp), intent(in) :: x
    real(dp), intent(out) :: LN, dLN
    real(dp) :: LNm2, LNm1, dLNm2, dLNm1
    integer :: j
    if (N == 0) then
      LN = one
      dLN = zero
    elseif (N == 1) then
      LN = x
      dLN = one
    else
      LNm2 = one
      LNm1 = x
      dLNm2 = zero
      dLNm1 = one
      do j = 2,N
        LN = real(2*j-1,dp)/real(j,dp) * x * LNm1 &
          - real(j-1,dp)/real(j,dp) * LNm2
        dLN = dLNm2 + real(2*j-1,dp) * LNm1
        LNm2 = LNm1
        LNm1 = LN
        dLNm2 = dLNm1
        dLNm1 = dLN
      end do
    end if
  end subroutine LegendrePolynomialAndDerivative

  pure subroutine LegendreGaussNodesAndWeights(N,x,w)
    use set_constants, only : zero, one, two, four, third, pi
    integer,                  intent(in)  :: N
    real(dp), dimension(N+1), intent(out) :: x, w
    integer :: j, k
    real(dp) :: eps4, delta, LNp1, dLNp1
    integer, parameter :: quad_n_iter = 10
    eps4 = four*epsilon(one)
    x = zero
    w = zero

    if (N == 0) then
      x(1) = zero
      w(1) = two
    elseif (N == 1) then
      x(1) = -sqrt(third)
      w(1) = one
      x(2) = -x(1)
      w(2) = w(1)
    else
      do j = 0,(N+1)/2 - 1
        x(j+1) = -cos( ( real(2*j+1,dp)/real(2*N+2,dp) )*pi )
        do k = 1,quad_n_iter
          call LegendrePolynomialAndDerivative(N+1,x(j+1),LNp1,dLNp1)
          delta = -LNp1/dLNp1
          x(j+1) = x(j+1) + delta
          if ( abs(delta) <= eps4*abs(x(j+1)) ) then
            exit
          end if
        end do
        call LegendrePolynomialAndDerivative(N+1,x(j+1),LNp1,dLNp1)
        x(N+1-j) = -x(j+1)
        w(j+1) = two/( (one-x(j+1)**2)*dLNp1**2)
        w(N+1-j) = w(j+1)
      end do
      if (mod(N,2) == 0) then
        call LegendrePolynomialAndDerivative(N+1,zero,LNp1,dLNp1)
        x(N/2+1) = zero
        w(N/2+1) = two/dLNp1**2
      end if
    end if
  end subroutine LegendreGaussNodesAndWeights

  pure function maximal_extents(dim,n_points,points) result(d)
    use set_constants, only : half
    integer, intent(in) :: dim, n_points
    real(dp), dimension(dim,n_points), intent(in) :: points
    real(dp), dimension(dim) :: d
    d = half*(maxval(points,dim=2) - minval(points,dim=2))
  end function maximal_extents

!=========================== compute_pseudo_inverse ==========================80
!>
!! Description: Computes the pseudo-inverse of the reconstruction LHS.
!!
!! Inputs:      LHS_m: Extent of reconstruction LHS
!!              LHS_n: Extent of reconstruction LHS
!!              LHS:   Reconstruction LHS
!!
!! Outputs:     LHS_plus: Pseudo-inverse of reconstruction LHS
!<
!=============================================================================80
  subroutine compute_pseudo_inverse( LHS_m, LHS_n, LHS, LHS_plus, cond )

    use set_precision, only : dp
    use set_constants, only : zero, one, ten

    external dgesvd

    integer,                          intent(in)  :: LHS_m, LHS_n
    ! real(dp), dimension(LHS_m,LHS_n), intent(in)  :: LHS
    real(dp), dimension(:,:),         intent(in)  :: LHS
    real(dp), dimension(LHS_n,LHS_m), intent(out) :: LHS_plus
    real(dp), optional,               intent(out) :: cond

    integer :: LDA, LDU, LDVT
    integer :: max_LHS_extents, min_LHS_extents
    integer :: LWORK
    integer :: INFO
    integer :: i, j
    integer :: min_s_loc

    real(dp) :: machine_precision
    real(dp) :: abs_tolerance
    real(dp) :: rel_tolerance

    real(dp), dimension(LHS_m,LHS_n)      :: LHS_cpy
    real(dp), dimension(:),   allocatable :: S
    real(dp), dimension(:,:), allocatable :: U
    real(dp), dimension(:,:), allocatable :: VT
    real(dp), dimension(:),   allocatable :: WORK
    real(dp), dimension(:),   allocatable :: Sinv
    real(dp), dimension(:),   allocatable :: V_Sinv

    continue

    ! Setup
    LHS_plus = zero

    LHS_cpy = LHS(1:LHS_m,1:LHS_n)

    ! Define SVD Parameters
    LDA   = LHS_m
    LDU   = LHS_m
    LDVT  = LHS_n

    max_LHS_extents = max(LHS_m,LHS_n)
    min_LHS_extents = min(LHS_m,LHS_n)

    LWORK = max( 3*min_LHS_extents + max_LHS_extents, 5*min_LHS_extents )

    ! Allocate Storage for the Singular Value Decomposition
    allocate( S(min_LHS_extents) )
    allocate( U(LHS_m,LHS_m) )
    allocate( VT(LHS_n,LHS_n) )
    allocate( WORK(LWORK) )
    allocate( Sinv( min_LHS_extents ) )
    allocate( V_Sinv( LHS_m ) )

    ! Compute the Singular Value Decomposition of the Reconstruction LHS
    ! Note: dgesvd = LAPACK routine
    ! Note: dgesdd makes assumptions about floating point arithmetic.
    call dgesvd( 'A', 'A', LHS_m, LHS_n, LHS_cpy, LDA, S, U, LDU, VT, LDVT,        &
                 WORK, LWORK, INFO )
    !call sgesvd( 'A', 'A', LHS_m, LHS_n, LHS, LDA, S, U, LDU, VT, LDVT,        &
    !             WORK, LWORK, INFO )


    ! Condition Number
    if ( present(cond) ) then
      min_s_loc = max(min_LHS_extents,count(S>zero))
      cond = S(1)/S(min_s_loc)
    end if
    
    ! Determine Truncation Tolerance
    machine_precision = (ten)**(-precision(one))
    abs_tolerance     = sqrt(machine_precision)
    rel_tolerance     = S(1)*abs_tolerance

    ! Compute Inverse of Singular Values
    Sinv = zero

    do i = 1, min_LHS_extents
      if ( S(i) <= rel_tolerance ) then
        ! Truncate Singular Value
        Sinv(i) = zero
      else
        Sinv(i) = one/S(i)
      end if
    end do

    ! Compute Pseudo-Inverse
    do i = 1, LHS_n
      V_Sinv = zero
      do j = 1, LHS_n
        V_Sinv(j) = VT(j,i)*Sinv(j)
      end do

      do j = 1, LHS_m
        LHS_plus(i,j) = dot_product( V_Sinv, U(j,:) )
      end do
    end do

    ! Deallocate Storage
    deallocate( S      )
    deallocate( U      )
    deallocate( VT     )
    deallocate( WORK   )
    deallocate( Sinv   )
    deallocate( V_Sinv )

  end subroutine compute_pseudo_inverse

  pure function reflect_through_hyperplane(p1, a, o) result(p2)
    use set_constants, only : two
    real(dp), dimension(:), intent(in) :: p1, a, o
    real(dp), dimension(size(p1))      :: p2
    p2  = p1 - two*dot_product(p1-o,a)*a/dot_product(a,a)
  end function reflect_through_hyperplane

  pure function closest_point_on_hyperplane(p1, a, o) result(p2)
    real(dp), dimension(:), intent(in) :: p1, a, o
    real(dp), dimension(size(p1))      :: p2
    p2  = p1 - dot_product(p1-o,a)*a/dot_product(a,a)
  end function closest_point_on_hyperplane

  pure subroutine dist_to_2_pt_line(P,A,B,dist,tol,intersect,pt)
    real(dp), dimension(3),           intent(in)  :: P, A, B
    real(dp),                         intent(out) :: dist
    real(dp),               optional, intent(in)  :: tol
    logical,                optional, intent(out) :: intersect
    real(dp), dimension(3), optional, intent(out) :: pt
    dist = norm2( cross_product( P-A, P-B ) ) / norm2( B - A )
    if (present(intersect)) then
      call pt_in_line(P,A,B,intersect,tol=tol)
      if ( .not. intersect ) then
        dist = min(dist,norm2(P-A),norm2(P-B))
      end if
    end if
  end subroutine dist_to_2_pt_line

  pure subroutine dist_to_3_pt_plane(P,A,B,C,dist,tol,intersect,pt)
    use set_constants, only : zero
    real(dp), dimension(3),           intent(in)  :: P, A, B, C
    real(dp),                         intent(out) :: dist
    real(dp),               optional, intent(in)  :: tol
    logical,                optional, intent(out) :: intersect
    real(dp), dimension(3), optional, intent(out) :: pt
    real(dp), dimension(3) :: n
    real(dp) :: tol_, den, tmp
    logical :: intersect_
    tol_ = zero
    if ( present(tol) ) tol_ = tol
    n = cross_product(A-B,A-C)
    den = norm2(n)
    if ( den > tol_) then
      if ( present(pt) ) pt = closest_point_on_hyperplane(P,n,n*zero)
      dist = abs( dot_product( n, P-A ) ) / den
      if ( present(intersect) ) then
        call pt_in_triangle(P,A,B,C,intersect,tol=tol)
        if ( .not. intersect ) then
          dist = min(dist,norm2(P-A),norm2(P-B),norm2(P-C))
        end if
      end if
    else ! collinear
      if ( present(pt) ) pt = A + (B-A)*dot_product(A-P,B-A)/dot_product(B-A,B-A)
      call dist_to_2_pt_line(P,A,B,tmp,tol=tol,intersect=intersect_)
      dist = tmp
      if ( present(intersect) ) intersect = intersect_
      if (intersect_) return
      call dist_to_2_pt_line(P,A,C,tmp,tol=tol,intersect=intersect_)
      dist = min(dist,tmp)
      if ( present(intersect) ) intersect = intersect_
      if (intersect_) return
      call dist_to_2_pt_line(P,B,C,tmp,tol=tol,intersect=intersect_)
      dist = min(dist,tmp)
      if ( present(intersect) ) intersect = intersect_
      if (intersect_) return

    end if
  end subroutine dist_to_3_pt_plane

  pure function is_colinear(A,B,C,tol)
    use set_constants, only : zero
    real(dp), dimension(3), intent(in) :: A,B,C
    real(dp), optional,     intent(in) :: tol
    logical                            :: is_colinear
    real(dp) :: tol_
    tol_ = zero
    if ( present(tol) ) tol_ = tol
    is_colinear = norm2(cross_product(A-B,A-C)) > tol_
  end function is_colinear

  pure subroutine pt_in_triangle(P,A,B,C,in_triangle,tol)
    use set_constants, only : near_zero, zero, one
    real(dp), dimension(3), intent(in)  :: P,A,B,C
    logical,                intent(out) :: in_triangle
    real(dp),     optional, intent(in)  :: tol
    real(dp), dimension(3) :: v0, v1, v2
    real(dp) :: d00, d01, d02, d11, d12, den, u, v, tol_
    tol_ = zero
    if ( present(tol) ) tol_ = tol
    v0 = C - A
    v1 = B - A
    v2 = P - A
    d00 = dot_product(v0, v0)
    d01 = dot_product(v0, v1)
    d02 = dot_product(v0, v2)
    d11 = dot_product(v1, v1)
    d12 = dot_product(v1, v2)
    den = (d00 * d11 - d01 * d01)
    ! if ( den<near_zero ) then ! collinear
    !   if ( norm2(cross_product(v1,v2)) > )
    u = (d11 * d02 - d01 * d12) / den
    v = (d00 * d12 - d01 * d02) / den

    
    in_triangle = (u >= zero - tol_) &
            .and. (v >= zero - tol_) &
            .and. ( (u + v) < one + tol_)
  end subroutine pt_in_triangle

  pure subroutine pt_in_line(P,A,B,in_line,tol)
    use set_constants, only : near_zero, zero, one
    real(dp), dimension(3), intent(in)  :: P,A,B
    logical,                intent(out) :: in_line
    real(dp),     optional, intent(in)  :: tol
    real(dp), dimension(3) :: v1, v2
    real(dp) :: tol_, t
    tol_ = zero
    if ( present(tol) ) tol_ = tol
    v1 = B - A
    v2 = P - A
    t = dot_product(v1,v2) / sum(v1**2)
    in_line = ( t >= zero - tol_ ).and.( t < one + tol_ )
  end subroutine pt_in_line

end module math

module vector_derived_type
  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: vec_t
  public :: vec_ptr, vec_ptr_3D
  type vec_t
    integer :: n
    real(dp), allocatable, dimension(:,:) :: v
  contains
    private
    procedure, public, pass :: create  => allocate_vec
    procedure, public, pass :: destroy => deallocate_vec
  end type vec_t

  type vec_ptr
    type(vec_t), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_vec_ptr
  end type vec_ptr

  type vec_ptr_3D
    type(vec_t), dimension(:,:,:), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_vec_ptr_3D
  end type vec_ptr_3D

contains

  pure elemental subroutine allocate_vec( this, n )
    class(vec_t), intent(inout) :: this
    integer,      intent(in)      :: n
    continue
    this%n = n
    allocate( this%v(3,n) )
    this%v = zero
  end subroutine allocate_vec

  pure elemental subroutine deallocate_vec( this )
    class(vec_t), intent(inout) :: this
    continue
    this%n = 0
    if( allocated( this%v  ) ) deallocate( this%v )
  end subroutine deallocate_vec

  pure elemental subroutine destroy_vec_ptr( this )
    class(vec_ptr), intent(inout) :: this
    this%p => null()
  end subroutine destroy_vec_ptr

  pure elemental subroutine destroy_vec_ptr_3D( this )
    class(vec_ptr_3D), intent(inout) :: this
    this%p => null()
  end subroutine destroy_vec_ptr_3D
  
end module vector_derived_type

module pointers
  use set_precision, only : dp
  implicit none
  private
  public :: array_ptr_3D_real, array_ptr_4D_real

  type array_ptr_3D_real
    real(dp), dimension(:,:,:),     pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_real_3D
  end type array_ptr_3D_real

  type array_ptr_4D_real
    real(dp), dimension(:,:,:,:),   pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_real_4D
  end type array_ptr_4D_real

contains

  pure elemental subroutine destroy_real_3D( this )
    class(array_ptr_3D_real), intent(inout) :: this
    this%p => null()
  end subroutine destroy_real_3D

  pure elemental subroutine destroy_real_4D( this )
    class(array_ptr_4D_real), intent(inout) :: this
    this%p => null()
  end subroutine destroy_real_4D
end module pointers

module linspace_helper
  use set_precision, only : dp
  implicit none
  private
  public :: linspace, meshgrid2, meshgrid3
  public :: map_1D_fun, perturb_mesh
  public :: cartesian_mesh, annulus_mesh, sphere_mesh

  interface
    pure function map_1D_fun(x_in) result(x_out)
      use set_precision, only : dp
      real(dp), dimension(:), intent(in) :: x_in
      real(dp), dimension(size(x_in)) :: x_out
    end function map_1D_fun
  end interface
contains

  pure function cartesian_mesh_coords(x_vec,y_vec,z_vec) result(xyz)
    real(dp), dimension(:), intent(in) :: x_vec, y_vec, z_vec
    real(dp), dimension(3,size(x_vec),size(y_vec),size(z_vec)) :: xyz
    call meshgrid3( x_vec, y_vec, z_vec, xyz(1,:,:,:),xyz(2,:,:,:),xyz(3,:,:,:) )
  end function cartesian_mesh_coords

  pure function cartesian_mesh(nx,ny,nz,end_pts,x_fun,y_fun,z_fun) result(xyz)
    use set_constants, only : zero, one
    integer, intent(in) :: nx, ny, nz
    real(dp), dimension(3,2), optional, intent(in) :: end_pts
    procedure(map_1D_fun), optional :: x_fun, y_fun, z_fun
    real(dp), dimension(3,nx,ny,nz) :: xyz
    real(dp), dimension(nx) :: x_vec1, x_vec2
    real(dp), dimension(ny) :: y_vec1, y_vec2
    real(dp), dimension(nz) :: z_vec1, z_vec2

    if ( present(end_pts) ) then
      x_vec1 = linspace(nx,end_pts(1,1),end_pts(1,2))
      y_vec1 = linspace(ny,end_pts(2,1),end_pts(2,2))
      z_vec1 = linspace(nz,end_pts(3,1),end_pts(3,2))
    else
      x_vec1 = linspace(nx,zero,one)
      y_vec1 = linspace(ny,zero,one)
      z_vec1 = linspace(nz,zero,one)
    end if

    if ( present(x_fun) ) then
      x_vec2 = x_vec1
      x_vec1 = x_fun(x_vec2)
    end if

    if ( present(y_fun) ) then
      y_vec2 = y_vec1
      y_vec1 = y_fun(y_vec2)
    end if

    if ( present(z_fun) ) then
      z_vec2 = z_vec1
      z_vec1 = z_fun(z_vec2)
    end if

    xyz = cartesian_mesh_coords(x_vec1,y_vec1,z_vec1)
  end function cartesian_mesh

  pure function annulus_mesh(nr,nt,nz,end_pts,r_fun,theta_fun,z_fun) result(xyz)
    use set_constants, only : zero, one, two, pi, half
    integer, intent(in) :: nr, nt, nz
    real(dp), dimension(3,2), optional, intent(in) :: end_pts
    procedure(map_1D_fun), optional :: r_fun, theta_fun, z_fun
    real(dp), dimension(3,nr,nt,nz) :: r_theta_z, xyz
    real(dp), dimension(nr) :: r_vec1, r_vec2
    real(dp), dimension(nt) :: theta_vec1, theta_vec2
    real(dp), dimension(nz) :: z_vec1, z_vec2

    if ( present(end_pts) ) then
      r_vec1     = linspace(nr,end_pts(1,1),end_pts(1,2))
      theta_vec1 = linspace(nt,end_pts(2,1),end_pts(2,2))
      z_vec1     = linspace(nz,end_pts(3,1),end_pts(3,2))
    else
      r_vec1     = linspace(nr,one,two)
      theta_vec1 = linspace(nt,zero,half*pi)
      z_vec1     = linspace(nz,zero,one)
    end if

    if ( present(r_fun) ) then
      r_vec2 = r_vec1
      r_vec1 = r_fun(r_vec2)
    end if

    if ( present(theta_fun) ) then
      theta_vec2 = theta_vec1
      theta_vec1 = theta_fun(theta_vec2)
    end if

    if ( present(z_fun) ) then
      z_vec2 = z_vec1
      z_vec1 = z_fun(z_vec2)
    end if
    r_theta_z = cartesian_mesh_coords(r_vec1,theta_vec1,z_vec1)
    xyz(1,:,:,:) = r_theta_z(1,:,:,:) * cos( r_theta_z(2,:,:,:) )
    xyz(2,:,:,:) = r_theta_z(1,:,:,:) * sin( r_theta_z(2,:,:,:) )
    xyz(3,:,:,:) = r_theta_z(3,:,:,:)
  end function annulus_mesh

  pure function sphere_mesh(nr,nt,np,end_pts,r_fun,theta_fun,phi_fun) result(xyz)
    use set_constants, only : zero, one, two, pi, fourth, half, three
    integer, intent(in) :: nr, nt, np
    real(dp), dimension(3,2), optional, intent(in) :: end_pts
    procedure(map_1D_fun), optional :: r_fun, theta_fun, phi_fun
    real(dp), dimension(3,nr,nt,np) :: r_theta_phi, xyz
    real(dp), dimension(nr) :: r_vec1, r_vec2
    real(dp), dimension(nt) :: theta_vec1, theta_vec2
    real(dp), dimension(np) :: phi_vec1, phi_vec2

    if ( present(end_pts) ) then
      r_vec1     = linspace(nr,end_pts(1,1),end_pts(1,2))
      theta_vec1 = linspace(nt,end_pts(2,1),end_pts(2,2))
      phi_vec1     = linspace(np,end_pts(3,1),end_pts(3,2))
    else
      r_vec1     = linspace(nr,one,two)
      theta_vec1 = linspace(nt,zero,half*pi)
      phi_vec1   = linspace(np,fourth*pi,three*fourth*pi)
    end if

    if ( present(r_fun) ) then
      r_vec2 = r_vec1
      r_vec1 = r_fun(r_vec2)
    end if

    if ( present(theta_fun) ) then
      theta_vec2 = theta_vec1
      theta_vec1 = theta_fun(theta_vec2)
    end if

    if ( present(phi_fun) ) then
      phi_vec2 = phi_vec1
      phi_vec1 = phi_fun(phi_vec2)
    end if
    r_theta_phi = cartesian_mesh_coords(r_vec1,theta_vec1,phi_vec1)
    xyz(1,:,:,:) = r_theta_phi(1,:,:,:) * cos( r_theta_phi(2,:,:,:) ) * sin( r_theta_phi(3,:,:,:) )
    xyz(2,:,:,:) = r_theta_phi(1,:,:,:) * sin( r_theta_phi(2,:,:,:) ) * sin( r_theta_phi(3,:,:,:) )
    xyz(3,:,:,:) = r_theta_phi(1,:,:,:)                               * cos( r_theta_phi(3,:,:,:) )
  end function sphere_mesh

  subroutine perturb_mesh(xyz,delta,include_boundaries)
    use index_conversion, only : in_bound
    use set_constants, only : zero, one, two, large
    real(dp), dimension(:,:,:,:), intent(inout) :: xyz
    real(dp),                     intent(in)    :: delta
    logical, optional,            intent(in)    :: include_boundaries
    integer, dimension(4) :: sz_tmp
    integer, dimension(3) :: n_nodes, o, min_bnd, max_bnd
    real(dp), dimension(3) :: mult
    real(dp), allocatable, dimension(:,:,:,:) :: xyz_tmp
    real(dp) :: diff, h0
    integer :: i, j, k, d, s, n_dim
    sz_tmp = shape(xyz)
    n_dim   = sz_tmp(1)
    n_nodes = sz_tmp(2:4)

    mult = zero
    where ( n_nodes > 2 ) mult = delta

    allocate( xyz_tmp(n_dim,0:n_nodes(1)+1,0:n_nodes(2)+1,0:n_nodes(3)+1) )
    xyz_tmp = large
    xyz_tmp(:,1:n_nodes(1),1:n_nodes(2),1:n_nodes(3)) = xyz

    max_bnd = max(n_nodes-1,1)
    min_bnd = min([2,2,2],max_bnd)

    if (present(include_boundaries) ) then
      if ( include_boundaries ) then
        min_bnd = [1,1,1]
        max_bnd = n_nodes
      end if
    end if

    do k = 1,n_nodes(3)
      do j = 1,n_nodes(2)
        do i = 1,n_nodes(1)
          if ( in_bound(3,[i,j,k],min_bnd,max_bnd) ) then
            do d = 1,n_dim
              o = 0
              h0 = large
              do s = -1,1,2
                o(d) = s
                h0 = min( h0, abs( xyz_tmp(d,i,j,k) - xyz_tmp(d,i+o(1),j+o(2),k+o(3)) ) )
              end do
              call random_number(diff); diff = two*diff - one
              xyz(d,i,j,k) = xyz(d,i,j,k) + mult(d)*diff*h0
            end do
          end if
        end do
      end do
    end do

    deallocate( xyz_tmp )

  end subroutine perturb_mesh

  pure function linspace(N,x1,x2) result(array)
    integer,  intent(in)   :: N
    real(dp), intent(in)   :: x1, x2
    real(dp), dimension(N) :: array
    real(dp) :: range_den
    integer :: i
    if (N==0) return
    if (N==1) then
      array(1) = x1
      return
    end if
    range_den = (x2-x1)/real(N-1,dp)
    do i = 1,N
      array(i) = x1 + range_den*real(i-1,dp)
    end do
  end function linspace

  pure subroutine meshgrid2(x1,x2,x1_array,x2_array)
    real(dp), dimension(:),   intent(in)  :: x1, x2
    real(dp), dimension(:,:), intent(out) :: x1_array, x2_array
    integer :: N1, N2
    N1 = size(x1)
    N2 = size(x2)
    x1_array = spread(x1,2,N2)
    x2_array = spread(x2,1,N1)
  end subroutine meshgrid2

  pure subroutine meshgrid3(x1,x2,x3,x1_array,x2_array,x3_array)
    real(dp), dimension(:),     intent(in)  :: x1, x2, x3
    real(dp), dimension(:,:,:), intent(out) :: x1_array, x2_array, x3_array
    real(dp), dimension(size(x1),size(x2)) :: x1_tmp
    real(dp), dimension(size(x2),size(x3)) :: x2_tmp
    real(dp), dimension(size(x2),size(x3),size(x1)) :: x2_tmp2
    real(dp), dimension(size(x3),size(x1)) :: x3_tmp
    real(dp), dimension(size(x3),size(x1),size(x2)) :: x3_tmp2
    integer, parameter, dimension(3) :: o2 = [2,3,1], o3 = [3,1,2]
    integer :: N1, N2, N3
    N1 = size(x1)
    N2 = size(x2)
    N3 = size(x3)

    x1_tmp   = spread(x1,2,N2)
    x2_tmp   = spread(x2,2,N3)
    x3_tmp   = spread(x3,2,N1)
    x1_array = spread(x1_tmp,3,N3)
    x2_tmp2  = spread(x2_tmp,3,N1)
    x3_tmp2  = spread(x3_tmp,3,N2)
    x2_array = reshape(x2_tmp2,shape(x2_array),order=o2)
    x3_array = reshape(x3_tmp2,shape(x3_array),order=o3)
  end subroutine meshgrid3

end module linspace_helper

module tecplot_output
  use set_precision, only : dp
  use set_constants, only : zero
  use message,       only : error_message
  implicit none
  private
  public :: write_tecplot_ordered_zone_header
  public :: write_tecplot_ordered_zone_block_packed
  character(*), dimension(6), parameter :: data_types=['DOUBLE  ','SINGLE  ',  &
                                                       'LONGINT ','SHORTINT',  &
                                                       'BYTE    ','BIT     ' ]
  character(*), dimension(6), parameter :: formats = ['(ES23.15)','(ES16.7) ', &
                                                      '(I11)    ','(I6)     ', &
                                                      '(I4)     ','(I1)     ']

  interface write_tecplot_size_fmt
    ! module procedure :: write_tecplot_size_fmt_fe
    module procedure :: write_tecplot_size_fmt_ordered
  end interface write_tecplot_size_fmt
contains

! subroutine write_tecplot_zone_header( fid, n_dim, n_nodes, n_cells,            &
!                                       n_node_vars, n_cell_vars, zone_name,     &
!                                       data_packing, data_format, var_share,    &
!                                       strand_id, solution_time )
!     integer,                                intent(in) :: fid, n_dim, n_vars
!     integer,      dimension(:),             intent(in) :: n_nodes
!     character(*), dimension(:),             intent(in) :: var_names
!     integer,                      optional, intent(in) :: n_cells
!     integer,                      optional, intent(in) :: n_cell_vars
!     character(*),                 optional, intent(in) :: zone_name
!     integer,                      optional, intent(in) :: data_packing
!     integer, dimension(:),        optional, intent(in) :: data_format
!     integer, dimension(:),        optional, intent(in) :: var_share
!     integer,                      optional, intent(in) :: strand_id
!     real(dp),                     optional, intent(in) :: solution_time
!     integer,                            intent(in) :: fid, n_dim
!     integer,                            intent(in) :: n_nodes, n_cells
!     integer,                            intent(in) :: n_node_var, n_cell_var
!     character(*),                       intent(in) :: zone_name
    
!     integer, dimension(:),    optional, intent(in) :: data_format
!     integer,                  optional, intent(in) :: strand_id
!     real(dp),                 optional, intent(in) :: solution_time
!     if ( present(n_cells) ) then
!       call write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes, n_cells,  &
!                                                  n_node_vars, n_cell_vars,        &
!                                                  zone_name, var_name,           &
!                                                  data_format,                   &
!                                                  strand_id, solution_time )
! end subroutine write_tecplot_zone_header
! subroutine write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,           &
!                                                 n_node_vars, n_cell_vars,      &
!                                                 zone_name,                     &
!                                                 data_packing,                  &
!                                                 data_format,                   &
!                                                 var_share,                     &
!                                                 strand_id,                     &
!                                                 solution_time)!,               &
! subroutine write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes, n_cells,  &
!                                                  n_node_vars, n_cell_vars,        &
!                                                  zone_name, var_name,           &
!                                                  data_format,                   &
!                                                  strand_id, solution_time )

  ! subroutine write_tecplot_size_fmt_fe( n_dim, n_nodes, n_cells, fmt )
  !   integer,      intent(in)  :: n_dim, n_nodes, n_cells
  !   character(*), intent(out) :: fmt
  !   logical :: err
  !   character(*), parameter :: routine_name = 'write_tecplot_size_fmt_fe'
  !   character(*), parameter :: fmt_2D = "ZONETYPE=FEQUADRILATERAL, "
  !   character(*), parameter :: fmt_3D = "ZONETYPE=FEBRICK, "
  !   character(*), parameter :: fmt_ne = "(A,', NODES=',I0,', ELEMENTS=',I0,',')"
  !   select case(n_dim)
  !   case(2)
  !     write(fmt,fmt_ne) fmt_2D, n_nodes, n_cells 
  !   case(3)
  !     write(fmt,fmt_ne) fmt_3D, n_nodes, n_cells
  !   case default
  !     err = error_message(routine_name,"Only n_dim=2 or 3 supported")
  !   end select
  ! end subroutine write_tecplot_size_fmt_fe

  subroutine write_tecplot_size_fmt_ordered( n_dim, n_nodes, fmt )
    integer,               intent(in)  :: n_dim
    integer, dimension(:), intent(in)  :: n_nodes
    character(*),          intent(out) :: fmt
    character(*), dimension(3), parameter :: IJK = ['I','J','K']
    character(*), parameter :: zone_type_fmt = 'ZONETYPE=ORDERED'
    character(*), parameter :: sz_fmt1 = "(""('"",(A,""=',I0,', ""),"
    character(*), parameter :: sz_fmt2 = "(A,""=',I0,', ""),""')"")"
    if (n_dim == 1) then
      write( fmt, '("I=",I0,",")') n_nodes(1)
    else
      write( fmt, '(A,I0,A)') sz_fmt1, n_dim-1, sz_fmt2
      write( fmt,trim(fmt)) IJK(1:n_dim)
      write( fmt,trim(fmt)) n_nodes(1:n_dim)
    end if
    fmt = zone_type_fmt//", "//trim(fmt)
  end subroutine write_tecplot_size_fmt_ordered

  subroutine write_tecplot_var_fmt( var_name, fmt )
    character(*), dimension(:),  intent(in)  :: var_name
    character(*),                intent(out) :: fmt
    integer               :: n_vars, i
    n_vars = size(var_name)
    write(fmt,'((A),I0,(A))') "('VARIABLES = ',", n_vars-1,                  &
                  "('""',(A),'""',', '),'""',(A),'""')"
    write(fmt,trim(fmt)) (trim(var_name(i)),i=1,n_vars)
  end subroutine write_tecplot_var_fmt

  subroutine write_tecplot_loc_fmt( n_node_vars, n_cell_vars, fmt )
    integer,      intent(in)  :: n_node_vars, n_cell_vars
    character(*), intent(out) :: fmt
    integer                   :: loc_ind, i
    integer, dimension(4)     :: loc_range
    character(100)            :: loc_cell, loc_nodal
    loc_ind   = 1
    loc_range = 1
    if ( n_node_vars > 0 ) then
      write(loc_nodal,'(A)') "('[',I0,'-',I0,']=NODAL'"
      if (n_cell_vars>0) then; loc_nodal = trim(loc_nodal)//",',')"
      else;                    loc_nodal = trim(loc_nodal)//")"
      end if
      loc_range(2) = n_node_vars
      loc_ind = 2
    else
      write(loc_nodal,'(A)') ''
    end if

    if ( n_cell_vars > 0 ) then
      write(loc_cell,'(A)') "'[',I0,'-',I0,']=CELLCENTERED')"
      if (n_node_vars>0) then; loc_cell = ",("//trim(loc_cell)
      else;                    loc_cell = "("//trim(loc_cell)
      end if
      loc_range(3) = n_node_vars + 1
      loc_range(4) = n_node_vars + n_cell_vars
      loc_ind = 4
    else
      write(loc_cell,'(A)') ''
    end if
    write(fmt,'(A)') "('VARLOCATION=(',"//trim(loc_nodal)//                  &
                                          trim(loc_cell)//"')')"
    write(fmt,trim(fmt)) (loc_range(i),i=1,loc_ind)
  end subroutine write_tecplot_loc_fmt


  subroutine write_tecplot_pack_fmt(n_cell_vars,data_packing,fmt)
    integer, intent(in) :: n_cell_vars
    character(*), intent(in) :: data_packing
    character(*), intent(out) :: fmt
    character(*), parameter :: routine_name = 'write_tecplot_pack_fmt'
    logical :: err
    select case(data_packing)
      case('point')
        if (n_cell_vars > 0) then
          err = error_message(routine_name, 'data_packing must be "block" '//  &
                                            'for cell-centered data' )
        end if
        write( fmt, '(A)' ) 'DATAPACKING=POINT'
      case('block')
        write( fmt, '(A)' ) 'DATAPACKING=BLOCK'
      case default
        err = error_message(routine_name, 'unrecognized data_packing! must '// &
                                          'be "point" or "block")' )
    end select
  end subroutine write_tecplot_pack_fmt

  subroutine write_tecplot_data_type_fmt( n_vars, fmt, data_format )
    integer,                              intent(in)  :: n_vars
    character(*),                         intent(out) :: fmt
    integer, dimension(n_vars), optional, intent(in)  :: data_format
    character(*), parameter :: routine_name = 'write_tecplot_data_type_fmt'
    integer :: i
    logical :: err
    write(fmt,'((A),I0,(A))') "('DT=(',(A),", n_vars-1,"(',',(A))')')"
    if ( present(data_format) ) then
      if ( any(data_format<1).or.any(data_format>6) ) then
        err = error_message(routine_name,'data_format must be in range [1,6]')
      end if
      write(fmt,trim(fmt)) ( trim( data_types( data_format(i) ) ),i=1,n_vars)
    else
      write(fmt,trim(fmt)) (trim(data_types(1)),i=1,n_vars)
    end if
  end subroutine write_tecplot_data_type_fmt

  subroutine write_tecplot_var_share_fmt( var_info, fmt )
  ! assume donor zone is first integer in var_info
    integer, dimension(:),       intent(in)  :: var_info
    character(*),                intent(out) :: fmt
    character(*), parameter :: routine_name = 'write_tecplot_var_share_fmt'
    integer :: n_vars, zone_num, i
    logical :: err
    n_vars = size(var_info) - 1
    if (n_vars < 1) then
      err = error_message(routine_name,'No variables were specified to be shared')
    end if
    zone_num = var_info(1)
    write(fmt,'((A),I0,(A),I0,(A))') "('VARSHARELIST=([',I0,", n_vars-1,       &
                                                 "(',',I0),']=", zone_num, ")')"
    write(fmt,trim(fmt)) ( var_info(i+1),i=1,n_vars)
  end subroutine write_tecplot_var_share_fmt

  !======================== write_tecplot_file_header ==========================80
  !>
  !! Generic routine for writing header info in Tecplot ASCII file
  !<
  !=============================================================================80
  ! subroutine write_tecplot_file_header( fid, var_names, title, filetype )

  !   integer,                            intent(in) :: fid
  !   character(*), dimension(:),         intent(in) :: var_names
  !   character(*), optional,             intent(in) :: title
  !   integer,      optional,             intent(in) :: filetype ! 0, 1, or 2

  !   logical               :: err
  !   character(1024)       :: var_fmt, title_fmt

  !   character(*), parameter :: routine_name = 'write_tecplot_file_header'

  !   call write_tecplot_var_fmt(var_names,var_fmt)

  !   write( fid, * )
  !   if ( present(title) ) then
  !     write(title_fmt,"('TITLE=','""',(A),'""')") trim(title)
  !     write( fid, '(A)' ) trim( title_fmt )
  !   end if
  !   if ( present(filetype) ) then
  !     select case(filetype)
  !     case(0)
  !       write( fid, '(A)' ) 'FILETYPE=FULL'
  !     case(1)
  !       write( fid, '(A)' ) 'FILETYPE=GRID'
  !     case(2)
  !       write( fid, '(A)' ) 'FILETYPE=SOLUTION'
  !     case default
  !       err = error_message(routine_name, 'unrecognized filetype! must be '//  &
  !                               '0 ("FULL"), 1 ("GRID"), or 2 ("SOLUTION")' )
  !     end select
  !   end if
  !   write( fid, '(A)' ) trim( var_fmt )

  ! end subroutine write_tecplot_file_header

!===================== write_tecplot_ordered_zone_header =====================80
!>
!! Generic routine for writing ordered zone header info in Tecplot ASCII file
!<
!=============================================================================80
  subroutine write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,           &
                                                n_node_vars,                   &
                                                n_cell_vars,                   &
                                                zone_name,                     &
                                                var_names,                     &
                                                var_share,                     &
                                                data_packing,                  &
                                                data_format,                   &
                                                strand_id,                     &
                                                solution_time )!,              &
                                                ! aux_data )
    use set_constants, only : max_text_line_length
    integer,                                intent(in) :: fid, n_dim
    integer, dimension(:),                  intent(in) :: n_nodes
    integer,                                intent(in) :: n_node_vars
    integer,                                intent(in) :: n_cell_vars
    character(*),                 optional, intent(in) :: zone_name
    character(*), dimension(:),   optional, intent(in) :: var_names
    integer,      dimension(:),   optional, intent(in) :: var_share
    character(*),                 optional, intent(in) :: data_packing
    integer,      dimension(:),   optional, intent(in) :: data_format
    integer,                      optional, intent(in) :: strand_id
    real(dp),                     optional, intent(in) :: solution_time
    ! character(*), dimension(:,:), optional, intent(in) :: aux_data
    integer :: n_vars
    logical :: err
    character(max_text_line_length) :: pack_fmt, loc_fmt, type_fmt
    character(max_text_line_length) :: var_fmt, var_share_fmt
    character(max_text_line_length) :: zone_fmt, size_fmt
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_header'
    
    if ( n_dim < 1 .or. n_dim > 3 ) then
      err = error_message( routine_name, 'Tecplot ordered file-format '//      &
                                         'supports only 1-3 dim.' )
    end if

    if ( present(zone_name) ) then
      write(zone_fmt,"('ZONE T=','""',(A),'""')") trim(zone_name)
    else
      write(zone_fmt,"(A)") 'ZONE'
    end if

    call write_tecplot_size_fmt_ordered(n_dim,n_nodes,size_fmt)

    if ( present(data_packing) ) then
      call write_tecplot_pack_fmt(n_cell_vars,data_packing,pack_fmt)
    end if

    if ( present(var_names) ) then
      call write_tecplot_var_fmt(var_names,var_fmt)
    end if

    if ( present(var_share) )  then
      call write_tecplot_var_share_fmt( var_share, var_share_fmt )
    end if

    call write_tecplot_loc_fmt( n_node_vars, n_cell_vars, loc_fmt )

    n_vars = n_node_vars + n_cell_vars
    if ( present(data_format) )  then
      if (size(data_format)/=n_vars) then
        err = error_message(routine_name,"Size of optional argument "//        &
                                        "'data_format' does not match "//      &
                                        "number of variables.")
      end if
      call write_tecplot_data_type_fmt( n_vars, type_fmt,                      &
                                        data_format=data_format )
    else
      call write_tecplot_data_type_fmt( n_vars, type_fmt )
    end if

    write( fid, *     )
    if ( present(var_names) ) write( fid, '(A)' ) trim( var_fmt )
    write( fid, '(A)' ) trim( zone_fmt )
    if ( present(var_share) ) write( fid, '(A)' ) trim( var_share_fmt )
    write( fid, '(A)' ) trim( size_fmt )
    if ( present(data_packing) ) write(fid,'(A)' ) trim( pack_fmt )
    write( fid, '(A)' ) trim( loc_fmt   )
    write( fid, '(A)' ) trim( type_fmt  )

    if ( present(strand_id) )  then
      write( fid, '((A),I0)' ) 'STRANDID=',strand_id
    end if

    if ( present(solution_time) )  then
      write( fid, '((A),ES23.15)' ) 'SOLUTIONTIME=',solution_time
    end if

  end subroutine write_tecplot_ordered_zone_header

  ! subroutine write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes,          &
  !                                                n_cells,                      &
  !                                                n_node_vars,                  &
  !                                                n_cell_vars,                  &
  !                                                zone_name,                    &
  !                                                var_names,                    &
  !                                                data_format,                  &
  !                                                strand_id,                    &
  !                                                solution_time )
  !   use set_constants, only : max_text_line_length
  !   integer,                              intent(in) :: fid, n_dim
  !   integer,                              intent(in) :: n_nodes, n_cells
  !   integer,                              intent(in) :: n_node_vars, n_cell_vars
  !   character(*),               optional, intent(in) :: zone_name
  !   character(*), dimension(:), optional, intent(in) :: var_names
  !   integer,      dimension(:), optional, intent(in) :: data_format
  !   integer,                    optional, intent(in) :: strand_id
  !   real(dp),                   optional, intent(in) :: solution_time
  !   integer               :: n_vars
  !   logical               :: err
  !   character(max_text_line_length) :: var_fmt, loc_fmt, type_fmt
  !   character(max_text_line_length) :: zone_fmt, size_fmt
  !   character(*), parameter :: routine_name = 'write_tecplot_fe_brick_zone_header'

  !   if ( present(zone_name) ) then
  !     write(zone_fmt,"('ZONE T=','""',(A),'""')") trim(zone_name)
  !   else
  !     write(zone_fmt,"(A)") 'ZONE'
  !   end if

  !   if ( present(var_names) ) then
  !     call write_tecplot_var_fmt(var_names,var_fmt)
  !   end if
  !   call write_tecplot_size_fmt_fe(n_dim,n_nodes,n_cells,size_fmt)
  !   call write_tecplot_loc_fmt( n_node_vars, n_cell_vars, loc_fmt )

  !   n_vars = n_node_vars + n_cell_vars
  !   if ( present(data_format) ) then
  !     if (size(data_format)/=n_vars) then
  !       err = error_message(routine_name,"Size of optional argument "//        &
  !                                        "'data_format' does not match "//     &
  !                                        "number of variables.")
  !     end if
  !     call write_tecplot_data_type_fmt( n_vars, type_fmt,                      &
  !                                       data_format=data_format )
  !   else
  !     call write_tecplot_data_type_fmt( n_vars, type_fmt )
  !   end if

  !   write( fid, *     )
  !   write( fid, '(A)' ) trim( zone_fmt )
  !   write( fid, '(A)' ) trim( size_fmt )
  !   if ( present(var_names) ) write( fid, '(A)' ) trim( var_fmt )
  !   write( fid, '(A)' ) trim( loc_fmt   )
  !   write( fid, '(A)' ) trim( type_fmt  )

  !   if ( present(strand_id) )  then
  !     write( fid, '((A),I0)' ) 'STRANDID=',strand_id
  !   end if

  !   if ( present(solution_time) )  then
  !     write( fid, '((A),ES23.15)' ) 'SOLUTIONTIME=',solution_time
  !   end if

  ! end subroutine write_tecplot_fe_brick_zone_header

  subroutine write_tecplot_ordered_zone_block_packed( fid, ijk_size,       &
                                                      n_node_vars, n_cell_vars,&
                                                      NODE_DATA, CELL_DATA,    &
                                                      data_format )
    integer,                  intent(in) :: fid
    integer, dimension(:),    intent(in) :: ijk_size
    integer,                  intent(in) :: n_node_vars, n_cell_vars
    real(dp), dimension(:,:), intent(in) :: NODE_DATA
    real(dp), dimension(:,:), intent(in) :: CELL_DATA
    integer,  dimension(:), optional, intent(in) :: data_format
    integer :: n_nodes, n_cells, cnt
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_block_packed'
    n_nodes = product(ijk_size)
    n_cells = product(max(ijk_size-1,1))
    if ( present(data_format) ) then
      cnt = 1
      call formatted_write(fid,n_nodes,n_node_vars,cnt,NODE_DATA,.false.,data_format=data_format)
      cnt = cnt + n_node_vars
      call formatted_write(fid,n_cells,n_cell_vars,cnt,CELL_DATA,.false.,data_format=data_format)
    else
      call formatted_write(fid,n_nodes,n_node_vars,1,NODE_DATA,.false.)
      call formatted_write(fid,n_cells,n_cell_vars,1,CELL_DATA,.false.)
    end if
  end subroutine write_tecplot_ordered_zone_block_packed

  ! subroutine write_tecplot_ordered_zone_point_packed( fid, n_nodes,       &
  !                                                     n_vars, NODE_DATA,       &
  !                                                     data_format )
  !   integer,                  intent(in) :: fid, n_nodes, n_vars
  !   real(dp), dimension(:,:), intent(in) :: NODE_DATA
  !   integer,  dimension(:), optional, intent(in) :: data_format
  !   character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_point_packed'
  !   if ( present(data_format) ) then
  !     call formatted_write(fid,n_nodes,n_vars,1,NODE_DATA,.true.,data_format=data_format)
  !   else
  !     call formatted_write(fid,n_nodes,n_vars,1,NODE_DATA,.true.)
  !   end if
  ! end subroutine write_tecplot_ordered_zone_point_packed

  ! subroutine write_tecplot_fe_brick_zone( fid, n_nodes, n_cells, n_node_vars,   &
  !                                         n_cell_vars, NODE_DATA, CELL_DATA,    &
  !                                         conn_idx, data_format )
  !   integer,                  intent(in) :: fid
  !   integer,                  intent(in) :: n_nodes,    n_cells
  !   integer,                  intent(in) :: n_node_vars, n_cell_vars
  !   real(dp), dimension(:,:), intent(in) :: NODE_DATA
  !   real(dp), dimension(:,:), intent(in) :: CELL_DATA
  !   integer,  dimension(:,:), intent(in) :: conn_idx
  !   integer,  dimension(:), optional, intent(in) :: data_format
  !   integer :: v, t, n_brick, cnt
  !   character(len=100) :: conn_fmt
  !   n_brick = size(conn_idx,1)
  !   if ( present(data_format) ) then
  !     cnt = 1
  !     call formatted_write(fid,n_nodes,n_node_vars,cnt,NODE_DATA,.false.,data_format=data_format)
  !     cnt = cnt + n_node_vars
  !     call formatted_write(fid,n_cells,n_cell_vars,cnt,CELL_DATA,.false.,data_format=data_format)
  !   else
  !     call formatted_write(fid,n_nodes,n_node_vars,1,NODE_DATA,.false.)
  !     call formatted_write(fid,n_cells,n_cell_vars,1,CELL_DATA,.false.)
  !   end if

  !   write(conn_fmt,'((A),I0,(A))') "((I0),", n_brick-1,"(' ',(I0)))"
  !   do t = 1,merge(n_cells,n_nodes,n_cell_vars>0)
  !     write(fid,trim(conn_fmt)) ( conn_idx(v,t), v=1,n_brick )
  !   end do

  ! end subroutine write_tecplot_fe_brick_zone

  subroutine formatted_write(fid,n_data,n_vars,fmt_idx,DATA,point_zone,data_format)
    use set_constants, only : max_text_line_length
    integer,                          intent(in) :: fid,n_data, n_vars, fmt_idx
    real(dp), dimension(:,:),         intent(in) :: DATA
    logical,                          intent(in) :: point_zone
    integer,  dimension(:), optional, intent(in) :: data_format
    
    integer :: v, i, cnt
    character(max_text_line_length) :: tmp_fmt

    if ( present(data_format) ) then
      if ( point_zone ) then
        do i = 1,n_data
          cnt = fmt_idx - 1
          do v = 1,n_vars
            cnt = cnt + 1
            write(tmp_fmt,'(A,A)') trim(formats(data_format(cnt))), " "
            select case(data_format(cnt))
            case(1,2)
              write(fid,trim(tmp_fmt),advance='no') DATA(v,i)
            case default
              write(fid,trim(tmp_fmt),advance='no') int(DATA(v,i))
            end select
          end do
          write(fid,*)
        end do
      else
        cnt = fmt_idx - 1
        do v = 1,n_vars
          cnt = cnt + 1
          select case(data_format(cnt))
          case(1,2)
            write(fid,trim(formats(data_format(cnt)))) ( DATA(v,i), i=1,n_data )
          case default
            write(fid,trim(formats(data_format(cnt)))) ( int(DATA(v,i)), i=1,n_data )
          end select
        end do
      end if
    else
      if (point_zone) then
        tmp_fmt = trim(formats(1))
        do v = 2,n_vars
          tmp_fmt = trim(tmp_fmt)//' '//trim(formats(1))
        end do
        do i = 1,n_data
          write(fid,trim(tmp_fmt)) ( DATA(v,i), v=1,n_vars )
        end do
      else
        write(fid,trim(formats(1))) ( ( DATA(v,i), i=1,n_data ), v=1,n_vars )
      end if
    end if
  end subroutine formatted_write
end module tecplot_output

module interpolant_derived_type
  use set_precision, only : dp
  use set_constants, only : zero, one, two, half
  implicit none
  private
  public :: interpolant_t
  public :: interpolant_w_3D_data_t

  type :: interpolant_t
    integer :: Nmax
    real(dp), dimension(:,:),     allocatable :: xb, wb
    real(dp), dimension(:,:,:,:), allocatable :: Dmat
  contains
    private
    procedure, public, nopass :: constructor
    procedure, public, pass   :: destroy       => destroy_interpolant
    procedure,         pass   :: lagbary, lagbary_wderiv, lagbary_wderiv2
    procedure,         pass   :: lagbary_2D, lagbary_2D_wgrad, lagbary_2D_whess
    procedure,         pass   :: lagbary_3D, lagbary_3D_wgrad, lagbary_3D_whess
    procedure, public, pass   :: calc_grid_metrics_2D, calc_grid_metrics_3D, calc_grid_metrics_alt
    procedure, public, pass   :: normal_vectors_2D, normal_vectors_3D
    ! procedure, public, pass   :: map_point_3D
    procedure, public, pass   :: map_point_3D_curve, map_point_3D_surface, map_point_3D_volume
  end type interpolant_t

  type, extends(interpolant_t) :: interpolant_w_3D_data_t
    integer                                 :: n_dim
    integer,  dimension(3)                  :: Npts
    real(dp), dimension(:,:,:), allocatable :: X1, X2, X3
  contains
    private
    procedure, public, pass :: create  => create_interpolant_w_3D_data
    procedure, public, pass :: destroy => destroy_interpolant_w_3D_data
    procedure, public, pass :: pt_interp
    procedure, public, pass :: pt_dist_fun, nearest_pt
    procedure, public, pass :: min_distance
  end type interpolant_w_3D_data_t

  interface interpolant_t
    procedure constructor
  end interface interpolant_t

  ! interface pt_interp
  !   module procedure curv_pt_interp
  !   module procedure surf_pt_interp
  !   module procedure volm_pt_interp
  ! end interface pt_interp

  ! interface pt_diff
  !   module procedure curv_pt_diff
  !   module procedure surf_pt_diff
  !   module procedure volm_pt_diff
  ! end interface pt_diff

  ! interface min_dist_pt
  !   module procedure min_dist_pt_curv
  !   module procedure min_dist_pt_surf
  !   module procedure min_dist_pt_volm
  ! end interface min_dist_pt
contains

  pure elemental subroutine destroy_interpolant(this)
    class(interpolant_t), intent(inout) :: this
    if ( allocated(this%Dmat) ) deallocate(this%Dmat)
    if ( allocated(this%xb) )   deallocate(this%xb)
    if ( allocated(this%wb) )   deallocate(this%wb)
    this%Nmax = 0
  end subroutine destroy_interpolant

  pure elemental subroutine destroy_interpolant_w_3D_data(this)
    class(interpolant_w_3D_data_t), intent(inout) :: this
    if ( allocated(this%X1) ) deallocate(this%X1)
    if ( allocated(this%X2) ) deallocate(this%X2)
    if ( allocated(this%X3) ) deallocate(this%X3)
    this%n_dim = 0
    this%Npts  = 0
    call this%interpolant_t%destroy()
  end subroutine destroy_interpolant_w_3D_data

  pure elemental function constructor(N) result(this)
    use linspace_helper, only : linspace
    use set_constants,   only : zero, one
    integer, optional, intent(in) :: N
    type(interpolant_t)           :: this
    integer :: j
    call this%destroy()
    if ( present(N) ) this%Nmax = max(N,2)
    allocate( this%Dmat(this%Nmax,this%Nmax,this%Nmax,2) )
    allocate(   this%xb(this%Nmax,this%Nmax), this%wb(this%Nmax,this%Nmax) )
    this%Dmat = zero; this%xb = zero; this%wb = zero
    this%wb(1,1) = one
    do j = 2,this%Nmax
      this%xb(1:j,j) = linspace(j,-one,one)
      this%wb(1:j,j) = barycentric_weights( this%xb(1:j,j) )
      this%Dmat(1:j,1:j,j,:) = mth_order_polynomial_derivative_matrix( this%xb(1:j,j), this%wb(1:j,j), 2 )
    end do
  end function constructor

  pure subroutine create_interpolant_w_3D_data( this, X1, X2, X3, shp )
    class(interpolant_w_3D_data_t), intent(inout) :: this
    real(dp), dimension(:),         intent(in) :: X1, X2, X3
    integer,  dimension(:),         intent(in) :: shp
    integer,  dimension(3) :: shp_
    call this%destroy()
    ! this%n_dim = size(shp)
    this%n_dim = count(shp>1)
    shp_ = 1
    shp_(1:this%n_dim) = shp(1:this%n_dim)
    this%Npts = shp_
    this%interpolant_t = interpolant_t(n=maxval(shp_))
    
    allocate(this%X1(shp_(1),shp_(2),shp_(3)))
    allocate(this%X2(shp_(1),shp_(2),shp_(3)))
    allocate(this%X3(shp_(1),shp_(2),shp_(3)))
    this%X1 = reshape(X1,shp_)
    this%X2 = reshape(X2,shp_)
    this%X3 = reshape(X3,shp_)
  end subroutine create_interpolant_w_3D_data

  pure elemental logical function almost_equal(a,b)
    real(dp), intent(in) :: a, b
    logical :: test1, test2, test3
    test1 = ( (a==zero) .or. (b==zero) )
    test2 = ( abs(a-b) <= two*epsilon(one) )
    test3 = ( ( abs(a-b) <= epsilon(abs(a)) ) .and. &
              ( abs(a-b) <= epsilon(abs(b)) ) )
    almost_equal = ( ( test1 .and. test2 ) .or. ( (.not. test1) .and. test3 ) )
  end function almost_equal

  pure function barycentric_weights(x) result(w)
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(size(x))       :: w
    integer :: j, k, N
    N = size(x)
    w = one
    do j = 2,N
      do k = 1,j-1
        w(k) = w(k) * ( x(k) - x(j) )
        w(j) = w(j) * ( x(j) - x(k) )
      end do
    end do
    w = one/w
  end function barycentric_weights

  pure function polynomial_derivative_matrix(x,w) result(D)
    real(dp), dimension(:), intent(in) :: x, w
    real(dp), dimension(size(x),size(x)) :: D
    integer :: i, j, N
    D = zero
    N = size(x)
    do i = 1,N
      do j = 1,N
        if (j/=i) then
          D(i,j) = w(j)/w(i) * one / ( x(i) - x(j) )
          D(i,i) = D(i,i) - D(i,j)
        end if
      end do
    end do
  end function polynomial_derivative_matrix

  pure function mth_order_polynomial_derivative_matrix(x,w,M) result(D)
    real(dp), dimension(:), intent(in) :: x, w
    integer,                intent(in) :: M
    real(dp), dimension(size(x),size(x),M) :: D
    integer :: i, j, k, N
    D = zero
    N = size(x)
    D(:,:,1) = polynomial_derivative_matrix(x,w)
    do k = 2,M
      do i = 1,N
        D(i,i,k) = zero
        do j = 1,N
          if (j/=i) then
            D(i,j,k) = ( real(k,dp) / (x(i) - x(j)) )        &
                     * ( w(j)/w(i)*D(i,i,k-1) - D(i,j,k-1) )
            D(i,i,k) = D(i,i,k) - D(i,j,k)
          end if
        end do
      end do
    end do
  end function mth_order_polynomial_derivative_matrix

  pure subroutine lagbary(this,x,dir,fval,Npts,val)
    class(interpolant_t),   intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val
    real(dp) :: A, F
    real(dp) :: x1, t1
    integer :: j, N
    A = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val = fval(j)
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
    end do
    val = A/F
  end subroutine lagbary

  pure subroutine lagbary_wderiv(this,x,dir,fval,Npts,val,dval)
    class(interpolant_t),   intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val, dval
    real(dp) :: A, B, C, F
    real(dp) :: x1, t1, t2, FF, AC
    integer :: j, N
    A = zero; B = zero; C = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val = fval(j)
        dval = dot_product( this%Dmat(j,1:N,N,1), fval )
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
      t2 = t1/x1
      B = B + t2 * fval(j)
      C = C + t2
    end do
    val = A/F
    FF = F*F
    AC = A*C
    dval = (B * F - AC)/FF
  end subroutine lagbary_wderiv

  pure subroutine lagbary_wderiv2(this,x,dir,fval,Npts,val,dval,d2val)
    class(interpolant_t),   intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val, dval, d2val
    real(dp) :: A, B, C, D, E, F
    real(dp) :: x1, t1, t2, t3, FF, AC
    integer :: j, N
    A = zero; B = zero; C = zero; D = zero; E = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val   = fval(j)
        dval  = dot_product( this%Dmat(j,1:N,N,1), fval )
        d2val = dot_product( this%Dmat(j,1:N,N,2), fval )
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
      t2 = t1/x1
      B = B + t2 * fval(j)
      C = C + t2
      t3 = t2/x1
      D = D + t3 * fval(j)
      E = E + t3
    end do
    val = A/F
    FF = F*F
    AC = A*C
    dval = (B * F - AC)/FF
    d2val = ( two * D      ) / F          &
          - ( two * E * A  ) / FF         &
          - ( two * B * C  ) / FF         &
          + ( two * C * AC ) / ( FF * F )
  end subroutine lagbary_wderiv2

  pure subroutine lagbary_2D(this,x,fval,Npts,val)
    class(interpolant_t),     intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(size(fval,2)) :: tmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary( x(1), 1, fval(:,j), Npts, tmp(j) )
    end do
    call this%lagbary( x(2), 2, tmp, Npts, val )
  end subroutine lagbary_2D

  pure subroutine lagbary_2D_wgrad(this,x,fval,Npts,val,grad)
    class(interpolant_t),     intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(2),   intent(out) :: grad
    real(dp), dimension(size(fval,2)) :: tmp, gtmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary_wderiv( x(1), 1, fval(:,j), Npts, tmp(j), gtmp(j) )
    end do
    call this%lagbary_wderiv( x(2), 2,  tmp, Npts, val, grad(2) )
    call this%lagbary(        x(2), 2, gtmp, Npts,      grad(1) )
  end subroutine lagbary_2D_wgrad

  pure subroutine lagbary_2D_whess(this,x,fval,Npts,val,grad,hess)
    class(interpolant_t),     intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(2),   intent(out) :: grad
    real(dp), dimension(3),   intent(out) :: hess
    real(dp), dimension(size(fval,2)) :: tmp, gtmp, htmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary_wderiv2( x(1), 1, fval(:,j), Npts, tmp(j), gtmp(j), htmp(j) )
    end do
    call this%lagbary_wderiv2( x(2), 2,  tmp, Npts, val, grad(2), hess(3) )
    call this%lagbary_wderiv(  x(2), 2, gtmp, Npts,      grad(1), hess(2) )
    call this%lagbary(         x(2), 2, htmp, Npts,               hess(1) )
  end subroutine lagbary_2D_whess

  pure subroutine lagbary_3D(this,x,fval,Npts,val)
    class(interpolant_t),       intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp
    real(dp), dimension(size(fval,3)) :: tmp2
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary( x(1), 1, fval(:,j,k), Npts, tmp(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary( x(2), 2, tmp(:,k), Npts, tmp2(k) )
    end do
    call this%lagbary( x(3), 3, tmp2, Npts, val )
  end subroutine lagbary_3D

  pure subroutine lagbary_3D_wgrad(this,x,fval,Npts,val,grad)
    class(interpolant_t),       intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(3),     intent(out) :: grad
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp, gtmp0
    real(dp), dimension(size(fval,3)) :: tmp2, gtmp1, gtmp2
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary_wderiv( x(1), 1, fval(:,j,k), Npts, tmp(j,k), gtmp0(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary_wderiv( x(2), 2,   tmp(:,k), Npts, tmp2(k), gtmp2(k) )
      call this%lagbary(        x(2), 2, gtmp0(:,k), Npts, gtmp1(k) )
    end do
    call this%lagbary_wderiv( x(3), 3,  tmp2, Npts, val, grad(3) )
    call this%lagbary(        x(3), 3, gtmp2, Npts,      grad(2) )
    call this%lagbary(        x(3), 3, gtmp1, Npts,      grad(1) )
  end subroutine lagbary_3D_wgrad

  pure subroutine lagbary_3D_whess(this,x,fval,Npts,val,grad,hess)
    class(interpolant_t),       intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(3),     intent(out) :: grad
    real(dp), dimension(6),     intent(out) :: hess
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp, gtmp, htmp
    real(dp), dimension(size(fval,3)) :: tmp1, gtmp1, gtmp2, htmp1, htmp2, htmp3
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary_wderiv2( x(1), 1, fval(:,j,k), Npts, tmp(j,k), gtmp(j,k), htmp(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary_wderiv2( x(2), 2,  tmp(:,k), Npts, tmp1(k), gtmp2(k), htmp3(k) )
      call this%lagbary_wderiv(  x(2), 2, gtmp(:,k), Npts,          gtmp1(k), htmp2(k) )
      call this%lagbary(         x(2), 2, htmp(:,k), Npts,                    htmp1(k) )
    end do
    call this%lagbary_wderiv2( x(3), 3,  tmp1, Npts, val, grad(3), hess(6) )
    call this%lagbary_wderiv(  x(3), 3, gtmp2, Npts,      grad(2), hess(5) )
    call this%lagbary(         x(3), 3, htmp3, Npts,               hess(4) )
    call this%lagbary_wderiv(  x(3), 3, gtmp1, Npts,      grad(1), hess(3) )
    call this%lagbary(         x(3), 3, htmp2, Npts,               hess(2) )
    call this%lagbary(         x(3), 3, htmp1, Npts,               hess(1) )
  end subroutine lagbary_3D_whess

  pure function calc_grid_metrics_2D(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    use math, only : cross_product
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:),   intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    real(dp), dimension(3) :: a1, a2, a3
    real(dp), dimension(2) :: tmp
    real(dp) :: junk
    integer, dimension(2) :: Npts
    Ja = zero
    a1 = zero
    a2 = zero
    a3 = [zero,zero,one]
    Npts = shape(X1)
    call this%lagbary_2D_wgrad( point, X1, Npts, junk, tmp )
    a1(1) = tmp(1)
    a2(1) = tmp(2)
    call this%lagbary_2D_wgrad( point, X2, Npts, junk, tmp )
    a1(2) = tmp(1)
    a2(2) = tmp(2)
    Ja(:,1) = cross_product(a2,a3)
    Ja(:,2) = cross_product(a3,a1)
    Ja(:,3) = [zero,zero,one]
  end function calc_grid_metrics_2D

  pure function calc_grid_metrics_3D(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3))   :: tmp
    real(dp), dimension(3) :: dX_l, dX_m, dd1, dd2, dd3
    real(dp) :: junk
    integer, dimension(3) :: Npts
    Ja = zero
    Npts = shape(X1)
    call this%lagbary_3D_wgrad( point, X3, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X2, Npts, junk, dX_m )
    tmp = X3*dX_m(1) - X2*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X3*dX_m(2) - X2*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X3*dX_m(3) - X2*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(1,1) = -half*( dd3(2) - dd2(3) );
    Ja(1,2) = -half*( dd1(3) - dd3(1) );
    Ja(1,3) = -half*( dd2(1) - dd1(2) );

    call this%lagbary_3D_wgrad( point, X1, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X3, Npts, junk, dX_m )
    tmp = X1*dX_m(1) - X3*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X1*dX_m(2) - X3*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X1*dX_m(3) - X3*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(2,1) = -half*( dd3(2) - dd2(3) );
    Ja(2,2) = -half*( dd1(3) - dd3(1) );
    Ja(2,3) = -half*( dd2(1) - dd1(2) );

    call this%lagbary_3D_wgrad( point, X2, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X1, Npts, junk, dX_m )
    tmp = X2*dX_m(1) - X1*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X2*dX_m(2) - X1*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X2*dX_m(3) - X1*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(3,1) = -half*( dd3(2) - dd2(3) );
    Ja(3,2) = -half*( dd1(3) - dd3(1) );
    Ja(3,3) = -half*( dd2(1) - dd1(2) );
  end function calc_grid_metrics_3D

  pure function calc_grid_metrics_alt(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3),3) :: X
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3))   :: tmp
    real(dp), dimension(3) :: dX_l, dX_m, dd1, dd2, dd3
    real(dp) :: junk
    integer, dimension(3), parameter :: ijk = [1,2,3]
    integer, dimension(3), parameter :: kij = cshift(ijk,1)
    integer, dimension(3), parameter :: jki = cshift(kij,1)
    integer, dimension(3) :: Npts
    integer :: i
    Ja = zero
    X(:,:,:,1) = X1
    X(:,:,:,2) = X2
    X(:,:,:,3) = X3
    Npts = shape(X1)
    do i = 1,3
      associate( l => kij(i), m => jki(i) )
        associate( X_l => X(:,:,:,l), X_m => X(:,:,:,m) )
          call this%lagbary_3D_wgrad( point, X_l, Npts, junk, dX_l )
          call this%lagbary_3D_wgrad( point, X_m, Npts, junk, dX_m )
          tmp = X_l*dX_m(1) - X_m*dX_l(1)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
          tmp = X_l*dX_m(2) - X_m*dX_l(2)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
          tmp = X_l*dX_m(3) - X_m*dX_l(3)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
          Ja(i,1) = -half*( dd3(2) - dd2(3) );
          Ja(i,2) = -half*( dd1(3) - dd3(1) );
          Ja(i,3) = -half*( dd2(1) - dd1(2) );
        end associate
      end associate
    end do
  end function calc_grid_metrics_alt

  pure function normal_vectors_3D(this,point,X1,X2,X3) result(Nvec)
    use set_constants, only : one
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Nvec
    Nvec = this%calc_grid_metrics_3D(point,X1,X2,X3)
    Nvec(:,1) = Nvec(:,1)/norm2(Nvec(:,1))
    Nvec(:,2) = Nvec(:,2)/norm2(Nvec(:,2))
    Nvec(:,3) = Nvec(:,3)/norm2(Nvec(:,3))
  end function normal_vectors_3D

  pure function normal_vectors_2D(this,point,X1,X2,X3) result(Nvec)
    use set_constants, only : one
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Nvec
    Nvec = this%calc_grid_metrics_2D(point,X1(:,:,1),X2(:,:,1),X3(:,:,1))
    Nvec(:,1) = Nvec(:,1)/norm2(Nvec(:,1))
    Nvec(:,2) = Nvec(:,2)/norm2(Nvec(:,2))
    Nvec(:,3) = Nvec(:,3)/norm2(Nvec(:,3))
  end function normal_vectors_2D

  ! pure function normal_vectors(this,point,X1,X2,X3) result(Nvec)
  !   use set_constants, only : one
  !   class(interpolant_t),       intent(in) :: this
  !   real(dp), dimension(3),     intent(in) :: point
  !   real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
  !   real(dp), dimension(3,3) :: Nvec
  !   real(dp), dimension(:,:,:), allocatable :: X1_tmp, X2_tmp, X3_tmp
  !   integer, dimension(3) :: sz

  !   sz = shape(X1)
  !   if ( all( sz > 1 ) ) then
  !     Nvec = this%calc_grid_metrics(point,X1,X2,X3)
  !   else
  !     if ( all( sz == 1 ) ) then
  !       allocate( X1_tmp(2,2,2), &
  !                 X2_tmp(2,2,2), &
  !                 X3_tmp(2,2,2) )
  !       X1_tmp = spread(spread(spread(X1(1,1,1),1,2),2,2),3,2)
  !       X2_tmp = spread(spread(spread(X2(1,1,1),1,2),2,2),3,2)
  !       X3_tmp = spread(spread(spread(X3(1,1,1),1,2),2,2),3,2)
  !       X1_tmp(2,:,:) = X1_tmp(2,:,:) + one
  !       X2_tmp(:,2,:) = X2_tmp(:,2,:) + one
  !       X3_tmp(:,:,2) = X3_tmp(:,:,2) + one
  !     elseif ( all(sz([2,3]) == 1) ) then
  !       allocate( X1_tmp(sz(1),2,2), &
  !                 X2_tmp(sz(1),2,2), &
  !                 X3_tmp(sz(1),2,2) )
  !       X1_tmp = spread(spread(X1(:,1,1),2,2),3,2)
  !       X2_tmp = spread(spread(X2(:,1,1),2,2),3,2)
  !       X3_tmp = spread(spread(X3(:,1,1),2,2),3,2)
  !       X2_tmp(:,2,:) = X2_tmp(:,2,:) + one
  !       X3_tmp(:,:,2) = X3_tmp(:,:,2) + one
  !     elseif( sz(3) == 1 ) then
  !       allocate( X1_tmp(sz(1),sz(2),2), &
  !                 X2_tmp(sz(1),sz(2),2), &
  !                 X3_tmp(sz(1),sz(2),2) )
  !       X1_tmp = spread(X1(:,:,1),3,2)
  !       X2_tmp = spread(X2(:,:,1),3,2)
  !       X3_tmp = spread(X3(:,:,1),3,2)
  !       X3_tmp(:,:,2) = X3_tmp(:,:,2) + one
  !     end if
  !     Nvec = this%calc_grid_metrics(point,X1_tmp,X2_tmp,X3_tmp)
  !   end if
    
  !   Nvec(:,1) = Nvec(:,1)/norm2(Nvec(:,1))
  !   Nvec(:,2) = Nvec(:,2)/norm2(Nvec(:,2))
  !   Nvec(:,3) = Nvec(:,3)/norm2(Nvec(:,3))

  !   if ( allocated(X1_tmp) ) deallocate(X1_tmp)
  !   if ( allocated(X2_tmp) ) deallocate(X2_tmp)
  !   if ( allocated(X3_tmp) ) deallocate(X3_tmp)
  ! end function normal_vectors

  pure subroutine get_nearest_node_1D(this,xyz,X1,X2,X3,node,dist,loc1,loc2,loc3)
    use set_constants, only : zero, large
    type(interpolant_t),              intent(in)  :: this
    real(dp), dimension(3),           intent(in)  :: xyz
    real(dp), dimension(:),           intent(in)  :: X1, X2, X3
    real(dp), dimension(1),           intent(out) :: node
    real(dp),               optional, intent(out) :: dist
    integer,  dimension(1), optional, intent(out) :: loc1, loc2, loc3
    integer,  dimension(1) :: Npts, loc
    logical,  dimension(size(X1)) :: mask
    real(dp), dimension(size(X1)) :: tmp_dist
    Npts = shape(X1)
    mask     = .true.
    tmp_dist = sqrt( (X1-xyz(1))**2 + (X2-xyz(2))**2 + (X3-xyz(3))**2 )
    loc = minloc( tmp_dist )
    node(1) = this%xb(loc(1),Npts(1))

    if ( present(dist) ) dist = tmp_dist(loc(1))
    if ( present(loc1) ) loc1 = loc

    mask(loc(1)) = .false.
    loc = minloc( tmp_dist, mask=mask )
    if ( present(loc2) ) loc2 = loc

    mask(loc(1)) = .false.
    loc = minloc( tmp_dist, mask=mask )
    if ( present(loc3) ) loc3 = loc
    
  end subroutine get_nearest_node_1D

  pure subroutine get_nearest_node_2D(this,xyz,X1,X2,X3,node,dist,loc1,loc2,loc3)
    use set_constants, only : zero, large
    type(interpolant_t),              intent(in)  :: this
    real(dp), dimension(3),           intent(in)  :: xyz
    real(dp), dimension(:,:),         intent(in)  :: X1, X2, X3
    real(dp), dimension(2),           intent(out) :: node
    real(dp),               optional, intent(out) :: dist
    integer,  dimension(2), optional, intent(out) :: loc1, loc2, loc3
    integer,  dimension(2) :: Npts, loc
    logical,  dimension(size(X1,1),size(X1,2)) :: mask
    real(dp), dimension(size(X1,1),size(X1,2)) :: tmp_dist
    Npts = shape(X1)
    mask     = .true.
    tmp_dist = sqrt( (X1-xyz(1))**2 + (X2-xyz(2))**2 + (X3-xyz(3))**2 )
    loc = minloc( tmp_dist )
    node(1) = this%xb(loc(1),Npts(1))
    node(2) = this%xb(loc(2),Npts(2))

    if ( present(dist) ) dist = tmp_dist(loc(1),loc(2))

    if ( present(loc1) ) loc1 = loc

    mask(loc(1),loc(2)) = .false.
    loc = minloc( tmp_dist, mask=mask )
    if ( present(loc2) ) loc2 = loc

    mask(loc(1),loc(2)) = .false.
    loc = minloc( tmp_dist, mask=mask )
    if ( present(loc3) ) loc3 = loc
    
  end subroutine get_nearest_node_2D

  pure subroutine get_nearest_node_3D(this,xyz,X1,X2,X3,node,dist,loc1,loc2,loc3)
    use set_constants, only : zero, large
    type(interpolant_t),              intent(in)  :: this
    real(dp), dimension(3),           intent(in)  :: xyz
    real(dp), dimension(:,:,:),       intent(in)  :: X1, X2, X3
    real(dp), dimension(3),           intent(out) :: node
    real(dp),               optional, intent(out) :: dist
    integer,  dimension(3), optional, intent(out) :: loc1, loc2, loc3
    integer,  dimension(3) :: Npts, loc
    logical,  dimension(size(X1,1),size(X1,2),size(X1,3)) :: mask
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3)) :: tmp_dist
    Npts = shape(X1)
    mask     = .true.
    tmp_dist = sqrt( (X1-xyz(1))**2 + (X2-xyz(2))**2 + (X3-xyz(3))**2 )
    loc = minloc( tmp_dist )
    node(1) = this%xb(loc(1),Npts(1))
    node(2) = this%xb(loc(2),Npts(2))
    node(3) = this%xb(loc(3),Npts(3))

    if ( present(dist) ) dist = tmp_dist(loc(1),loc(2),loc(3))

    if ( present(loc1) ) loc1 = loc

    mask(loc(1),loc(2),loc(3)) = .false.
    loc = minloc( tmp_dist, mask=mask )
    if ( present(loc2) ) loc2 = loc

    mask(loc(1),loc(2),loc(3)) = .false.
    loc = minloc( tmp_dist, mask=mask )
    if ( present(loc3) ) loc3 = loc
  end subroutine get_nearest_node_3D

  pure subroutine map_point_3D_curve(this,point,X1,X2,X3,xyz,dS)
    class(interpolant_t),   intent(in)  :: this
    real(dp), dimension(1), intent(in)  :: point ! [t]
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3), intent(out) :: xyz
    real(dp),               intent(out) :: dS
    integer,  dimension(1) :: Npts
    real(dp), dimension(3) :: dval
    Npts = shape(X1)
    call this%lagbary_wderiv(point(1),1,X1,Npts,xyz(1),dval(1))
    call this%lagbary_wderiv(point(1),1,X2,Npts,xyz(2),dval(2))
    call this%lagbary_wderiv(point(1),1,X3,Npts,xyz(3),dval(3))
    dS = norm2(dval)
  end subroutine map_point_3D_curve

  pure subroutine map_point_3D_surface(this,point,X1,X2,X3,xyz,dS)
    use math, only : cross_product
    class(interpolant_t),     intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: point ! [u,v]
    real(dp), dimension(:,:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3),   intent(out) :: xyz
    real(dp),                 intent(out) :: dS
    integer,  dimension(2) :: Npts
    real(dp), dimension(2) :: tmp
    real(dp), dimension(3) :: drdu, drdv
    Npts = shape(X1)
    call this%lagbary_2D_wgrad(point,X1,Npts,xyz(1),tmp)
    drdu(1) = tmp(1); drdv(1) = tmp(2)
    call this%lagbary_2D_wgrad(point,X2,Npts,xyz(2),tmp)
    drdu(2) = tmp(1); drdv(2) = tmp(2)
    call this%lagbary_2D_wgrad(point,X3,Npts,xyz(3),tmp)
    drdu(3) = tmp(1); drdv(3) = tmp(2)
    dS = norm2( cross_product(drdu,drdv) )
  end subroutine map_point_3D_surface

  pure subroutine map_point_3D_volume(this,point,X1,X2,X3,xyz,dS)
    use math, only : det_3x3
    class(interpolant_t),       intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: point ! [xi,eta,zeta]
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3),     intent(out) :: xyz
    real(dp),                   intent(out) :: dS
    integer, dimension(3) :: Npts
    real(dp), dimension(3,3) :: A
    Npts = shape(X1)
    call this%lagbary_3D_wgrad(point,X1,Npts,xyz(1),A(:,1))
    call this%lagbary_3D_wgrad(point,X2,Npts,xyz(2),A(:,2))
    call this%lagbary_3D_wgrad(point,X3,Npts,xyz(3),A(:,3))
    dS = det_3x3(A)
  end subroutine map_point_3D_volume

  pure function curv_pt_interp(this,X1,X2,X3,pt) result(xyz)
    type(interpolant_t),    intent(in) :: this
    real(dp), dimension(:), intent(in) :: X1, X2, X3
    real(dp),               intent(in) :: pt
    real(dp), dimension(3)             :: xyz
    integer,  dimension(1) :: Npts
    Npts = shape(X1)
    call this%lagbary(pt,1,X1,Npts,xyz(1))
    call this%lagbary(pt,1,X2,Npts,xyz(2))
    call this%lagbary(pt,1,X3,Npts,xyz(3))
  end function curv_pt_interp

  pure function surf_pt_interp(this,X1,X2,X3,pt) result(xyz)
    type(interpolant_t),      intent(in) :: this
    real(dp), dimension(:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(2),   intent(in) :: pt
    real(dp), dimension(3)               :: xyz
    integer,  dimension(2) :: Npts
    Npts = shape(X1)
    call this%lagbary_2D(pt,X1,Npts,xyz(1))
    call this%lagbary_2D(pt,X2,Npts,xyz(2))
    call this%lagbary_2D(pt,X3,Npts,xyz(3))
  end function surf_pt_interp

  pure function volm_pt_interp(this,X1,X2,X3,pt) result(xyz)
    type(interpolant_t),        intent(in) :: this
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3),     intent(in) :: pt
    real(dp), dimension(3)                 :: xyz
    integer, dimension(3) :: Npts
    Npts = shape(X1)
    call this%lagbary_3D(pt,X1,Npts,xyz(1))
    call this%lagbary_3D(pt,X2,Npts,xyz(2))
    call this%lagbary_3D(pt,X3,Npts,xyz(3))
  end function volm_pt_interp

  pure subroutine curv_pt_diff(this,X1,X2,X3,pt,xyz,grad,hess)
    type(interpolant_t),              intent(in)  :: this 
    real(dp), dimension(:),           intent(in)  :: X1, X2, X3
    real(dp),                         intent(in)  :: pt
    real(dp), dimension(3),           intent(out) :: xyz
    real(dp), dimension(3), optional, intent(out) :: grad
    real(dp), dimension(3), optional, intent(out) :: hess
    real(dp), dimension(3) :: gtmp
    integer,  dimension(1) :: Npts
    Npts = shape(X1)
    if ( present(hess) ) then
      call this%lagbary_wderiv2(pt,1,X1,Npts,xyz(1),gtmp(1),hess(1) )
      call this%lagbary_wderiv2(pt,1,X2,Npts,xyz(2),gtmp(2),hess(2) )
      call this%lagbary_wderiv2(pt,1,X3,Npts,xyz(3),gtmp(3),hess(3) )
      if ( present(grad) ) grad = gtmp
    elseif ( present(grad) ) then
      call this%lagbary_wderiv(pt,1,X1,Npts,xyz(1),grad(1) )
      call this%lagbary_wderiv(pt,1,X2,Npts,xyz(2),grad(2) )
      call this%lagbary_wderiv(pt,1,X3,Npts,xyz(3),grad(3) )
    else
      call this%lagbary(pt,1,X1,Npts,xyz(1) )
      call this%lagbary(pt,1,X2,Npts,xyz(2) )
      call this%lagbary(pt,1,X3,Npts,xyz(3) )
    end if
  end subroutine curv_pt_diff

  pure subroutine surf_pt_diff(this,X1,X2,X3,pt,xyz,grad,hess)
    type(interpolant_t),                intent(in)  :: this 
    real(dp), dimension(:,:),           intent(in)  :: X1, X2, X3
    real(dp), dimension(2),             intent(in)  :: pt
    real(dp), dimension(3),             intent(out) :: xyz
    real(dp), dimension(2,3), optional, intent(out) :: grad
    real(dp), dimension(3,3), optional, intent(out) :: hess
    real(dp), dimension(2,3) :: gtmp
    integer,  dimension(2) :: Npts
    Npts = shape(X1)
    if ( present(hess) ) then
      call this%lagbary_2D_whess(pt,X1,Npts,xyz(1),gtmp(:,1),hess(:,1) )
      call this%lagbary_2D_whess(pt,X2,Npts,xyz(2),gtmp(:,2),hess(:,2) )
      call this%lagbary_2D_whess(pt,X3,Npts,xyz(3),gtmp(:,3),hess(:,3) )
      if ( present(grad) ) grad = gtmp
    elseif ( present(grad) ) then
      call this%lagbary_2D_wgrad(pt,X1,Npts,xyz(1),grad(:,1) )
      call this%lagbary_2D_wgrad(pt,X2,Npts,xyz(2),grad(:,2) )
      call this%lagbary_2D_wgrad(pt,X3,Npts,xyz(3),grad(:,3) )
    else
      call this%lagbary_2D(pt,X1,Npts,xyz(1) )
      call this%lagbary_2D(pt,X2,Npts,xyz(2) )
      call this%lagbary_2D(pt,X3,Npts,xyz(3) )
    end if
  end subroutine surf_pt_diff

  pure subroutine volm_pt_diff(this,X1,X2,X3,pt,xyz,grad,hess)
    type(interpolant_t),                intent(in)  :: this 
    real(dp), dimension(:,:,:),         intent(in)  :: X1, X2, X3
    real(dp), dimension(3),             intent(in)  :: pt
    real(dp), dimension(3),             intent(out) :: xyz
    real(dp), dimension(3,3), optional, intent(out) :: grad
    real(dp), dimension(6,3), optional, intent(out) :: hess
    real(dp), dimension(3,3) :: gtmp
    integer,  dimension(3) :: Npts
    Npts = shape(X1)
    if ( present(hess) ) then
      call this%lagbary_3D_whess(pt,X1,Npts,xyz(1),gtmp(:,1),hess(:,1) )
      call this%lagbary_3D_whess(pt,X2,Npts,xyz(2),gtmp(:,2),hess(:,2) )
      call this%lagbary_3D_whess(pt,X3,Npts,xyz(3),gtmp(:,3),hess(:,3) )
      if ( present(grad) ) grad = gtmp
    elseif ( present(grad) ) then
      call this%lagbary_3D_wgrad(pt,X1,Npts,xyz(1),grad(:,1) )
      call this%lagbary_3D_wgrad(pt,X2,Npts,xyz(2),grad(:,2) )
      call this%lagbary_3D_wgrad(pt,X3,Npts,xyz(3),grad(:,3) )
    else
      call this%lagbary_3D(pt,X1,Npts,xyz(1) )
      call this%lagbary_3D(pt,X2,Npts,xyz(2) )
      call this%lagbary_3D(pt,X3,Npts,xyz(3) )
    end if

  end subroutine volm_pt_diff

  pure subroutine curv_pt_dist_fun(this,X1,X2,X3,pt,xyz_pt,fval,dfval,d2fval)
    use set_constants, only : two
    type(interpolant_t),      intent(in)  :: this
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    real(dp),               intent(in)  :: pt
    real(dp), dimension(3), intent(in)  :: xyz_pt
    real(dp),               intent(out) :: fval
    real(dp), dimension(1),   optional, intent(out) :: dfval
    real(dp), dimension(1,1), optional, intent(out) :: d2fval
    real(dp), dimension(3) :: grad
    real(dp), dimension(3) :: hess
    real(dp) :: A
    real(dp), dimension(3) :: x_

    if ( present(d2fval) ) then
      call curv_pt_diff(this,X1,X2,X3,pt,x_,grad=grad,hess=hess)
    elseif ( present(dfval) ) then
      call curv_pt_diff(this,X1,X2,X3,pt,x_,grad=grad)
    else
      call curv_pt_diff(this,X1,X2,X3,pt,x_)
    end if
    x_ = x_ - xyz_pt
    fval = dot_product(x_,x_)

    if ( present(d2fval).or.present(dfval) ) then
      A    = x_(1) * grad(1) + x_(2) * grad(2) + x_(3) * grad(3)
      if ( present(dfval)  ) dfval = two*A
      if ( present(d2fval) ) then
        d2fval  = x_(1) * hess(1) + grad(1) * grad(1) &
                + x_(2) * hess(2) + grad(2) * grad(2) &
                + x_(3) * hess(3) + grad(3) * grad(3)
        d2fval = two * d2fval
      end if
    end if

  end subroutine curv_pt_dist_fun

  pure subroutine surf_pt_dist_fun(this,X1,X2,X3,pt,xyz_pt,fval,dfval,d2fval)
    use set_constants, only : two
    type(interpolant_t),                intent(in)  :: this
    real(dp), dimension(:,:),           intent(in)  :: X1, X2, X3
    real(dp), dimension(2),             intent(in)  :: pt
    real(dp), dimension(3),             intent(in)  :: xyz_pt
    real(dp),                           intent(out) :: fval
    real(dp), dimension(2),   optional, intent(out) :: dfval
    real(dp), dimension(2,2), optional, intent(out) :: d2fval
    real(dp), dimension(2,3) :: grad
    real(dp), dimension(3,3) :: hess
    real(dp) :: A11, A22, A12
    real(dp), dimension(3) :: x_
    real(dp), dimension(2) :: A
    
    if ( present(d2fval) ) then
      call surf_pt_diff(this,X1,X2,X3,pt,x_,grad=grad,hess=hess)
    elseif ( present(dfval) ) then
      call surf_pt_diff(this,X1,X2,X3,pt,x_,grad=grad)
    else
      call surf_pt_diff(this,X1,X2,X3,pt,x_)
    end if

    x_ = x_ - xyz_pt
    fval = dot_product(x_,x_)
    
    if ( present(d2fval).or.present(dfval) ) then
      A(1) = x_(1) * grad(1,1) + x_(2) * grad(1,2) + x_(3) * grad(1,3)
      A(2) = x_(1) * grad(2,1) + x_(2) * grad(2,2) + x_(3) * grad(2,3)
      if ( present(dfval)  ) dfval = two * A
      if ( present(d2fval) ) then
        A11  = x_(1) * hess(1,1) + grad(1,1) * grad(1,1) &
             + x_(2) * hess(1,2) + grad(1,2) * grad(1,2) &
             + x_(3) * hess(1,3) + grad(1,3) * grad(1,3)
        A22  = x_(1) * hess(3,1) + grad(2,1) * grad(2,1) &
             + x_(2) * hess(3,2) + grad(2,2) * grad(2,2) &
             + x_(3) * hess(3,3) + grad(2,3) * grad(2,3)
        A12  = x_(1) * hess(2,1) + grad(1,1) * grad(2,1) &
             + x_(2) * hess(2,2) + grad(1,2) * grad(2,2) &
             + x_(3) * hess(2,3) + grad(1,3) * grad(2,3)
        d2fval(1,1) = A11
        d2fval(2,1) = A12
        d2fval(1,2) = d2fval(2,1)
        d2fval(2,2) = A22
        d2fval = two * d2fval
      end if
    end if

  end subroutine surf_pt_dist_fun


  pure subroutine volm_pt_dist_fun(this,X1,X2,X3,pt,xyz_pt,fval,dfval,d2fval)
    use set_constants, only : two
    type(interpolant_t),                intent(in)  :: this
    real(dp), dimension(:,:,:),         intent(in)  :: X1, X2, X3
    real(dp), dimension(3),             intent(in)  :: pt
    real(dp), dimension(3),             intent(in)  :: xyz_pt
    real(dp),                           intent(out) :: fval
    real(dp), dimension(3),   optional, intent(out) :: dfval
    real(dp), dimension(3,3), optional, intent(out) :: d2fval
    real(dp), dimension(3,3) :: grad
    real(dp), dimension(6,3) :: hess
    real(dp) :: L, L2, A11, A22, A33, A12, A13, A23
    real(dp), dimension(3) :: x_, A

    if ( present(d2fval) ) then
      call volm_pt_diff(this,X1,X2,X3,pt,x_,grad=grad,hess=hess)
    elseif ( present(dfval) ) then
      call volm_pt_diff(this,X1,X2,X3,pt,x_,grad=grad)
    else
      call volm_pt_diff(this,X1,X2,X3,pt,x_)
    end if

    x_ = x_ - xyz_pt
    fval = dot_product(x_,x_)

    if ( present(d2fval).or.present(dfval) ) then
      A(1) = x_(1) * grad(1,1) + x_(2) * grad(1,2) + x_(3) * grad(1,3)
      A(2) = x_(1) * grad(2,1) + x_(2) * grad(2,2) + x_(3) * grad(2,3)
      A(3) = x_(1) * grad(3,1) + x_(2) * grad(3,2) + x_(3) * grad(3,3)
      if ( present(dfval)  ) dfval = two * A
      if ( present(d2fval) ) then
        A11  = x_(1) * hess(1,1) + grad(1,1) * grad(1,1) &
             + x_(2) * hess(1,2) + grad(1,2) * grad(1,2) &
             + x_(3) * hess(1,3) + grad(1,3) * grad(1,3)
        A22  = x_(1) * hess(2,1) + grad(2,1) * grad(2,1) &
             + x_(2) * hess(2,2) + grad(2,2) * grad(2,2) &
             + x_(3) * hess(2,3) + grad(2,3) * grad(2,3)
        A33  = x_(1) * hess(3,1) + grad(3,1) * grad(3,1) &
             + x_(2) * hess(3,2) + grad(3,2) * grad(3,2) &
             + x_(3) * hess(3,3) + grad(3,3) * grad(3,3)
        A12  = x_(1) * hess(4,1) + grad(1,1) * grad(2,1) &
             + x_(2) * hess(4,2) + grad(1,2) * grad(2,2) &
             + x_(3) * hess(4,3) + grad(1,3) * grad(2,3)
        A13  = x_(1) * hess(5,1) + grad(1,1) * grad(3,1) &
             + x_(2) * hess(5,2) + grad(1,2) * grad(3,2) &
             + x_(3) * hess(5,3) + grad(1,3) * grad(3,3)
        A23  = x_(1) * hess(6,1) + grad(2,1) * grad(3,1) &
             + x_(2) * hess(6,2) + grad(2,2) * grad(3,2) &
             + x_(3) * hess(6,3) + grad(2,3) * grad(3,3)
        d2fval(1,1) = A11
        d2fval(2,1) = A12
        d2fval(3,1) = A13
        d2fval(1,2) = d2fval(2,1)
        d2fval(2,2) = A22
        d2fval(3,2) = A23
        d2fval(1,3) = d2fval(3,1)
        d2fval(2,3) = d2fval(3,2)
        d2fval(3,3) = A33
        d2fval = two * d2fval
      end if
    end if

  end subroutine volm_pt_dist_fun

  pure subroutine curv_pt_dist_fun_old(this,X1,X2,X3,pt,xyz_pt,fval,dfval,d2fval)
    type(interpolant_t),      intent(in)  :: this
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    real(dp),               intent(in)  :: pt
    real(dp), dimension(3), intent(in)  :: xyz_pt
    real(dp),               intent(out) :: fval
    real(dp), dimension(1),   optional, intent(out) :: dfval
    real(dp), dimension(1,1), optional, intent(out) :: d2fval
    real(dp), dimension(3) :: grad
    real(dp), dimension(3) :: hess
    real(dp) :: L, A, A11
    real(dp), dimension(3) :: x_

    if ( present(d2fval) ) then
      call curv_pt_diff(this,X1,X2,X3,pt,x_,grad=grad,hess=hess)
    elseif ( present(dfval) ) then
      call curv_pt_diff(this,X1,X2,X3,pt,x_,grad=grad)
    else
      call curv_pt_diff(this,X1,X2,X3,pt,x_)
    end if
    x_ = x_ - xyz_pt
    fval = norm2(x_)

    if ( present(d2fval).or.present(dfval) ) then
      L    = fval
      A    = x_(1) * grad(1) + x_(2) * grad(2) + x_(3) * grad(3)
      if ( present(dfval)  ) dfval = A / L
      if ( present(d2fval) ) then
        A11  = x_(1) * hess(1) + grad(1) * grad(1) &
            + x_(2) * hess(2) + grad(2) * grad(2) &
            + x_(3) * hess(3) + grad(3) * grad(3)
        d2fval = A11*L*L - A*A
        d2fval = d2fval/(L*L*L)
      end if
    end if

  end subroutine curv_pt_dist_fun_old

  pure subroutine surf_pt_dist_fun_old(this,X1,X2,X3,pt,xyz_pt,fval,dfval,d2fval)
    type(interpolant_t),                intent(in)  :: this
    real(dp), dimension(:,:),           intent(in)  :: X1, X2, X3
    real(dp), dimension(2),             intent(in)  :: pt
    real(dp), dimension(3),             intent(in)  :: xyz_pt
    real(dp),                           intent(out) :: fval
    real(dp), dimension(2),   optional, intent(out) :: dfval
    real(dp), dimension(2,2), optional, intent(out) :: d2fval
    real(dp), dimension(2,3) :: grad
    real(dp), dimension(3,3) :: hess
    real(dp) :: L, A11, A22, A12
    real(dp), dimension(3) :: x_
    real(dp), dimension(2) :: A
    
    if ( present(d2fval) ) then
      call surf_pt_diff(this,X1,X2,X3,pt,x_,grad=grad,hess=hess)
    elseif ( present(dfval) ) then
      call surf_pt_diff(this,X1,X2,X3,pt,x_,grad=grad)
    else
      call surf_pt_diff(this,X1,X2,X3,pt,x_)
    end if

    x_ = x_ - xyz_pt
    fval = norm2(x_)
    
    if ( present(d2fval).or.present(dfval) ) then
      L    = fval
      A(1) = x_(1) * grad(1,1) + x_(2) * grad(1,2) + x_(3) * grad(1,3)
      A(2) = x_(1) * grad(2,1) + x_(2) * grad(2,2) + x_(3) * grad(2,3)
      if ( present(dfval)  ) dfval = A / L
      if ( present(d2fval) ) then
        A11  = x_(1) * hess(1,1) + grad(1,1) * grad(1,1) &
             + x_(2) * hess(1,2) + grad(1,2) * grad(1,2) &
             + x_(3) * hess(1,3) + grad(1,3) * grad(1,3)
        A22  = x_(1) * hess(3,1) + grad(2,1) * grad(2,1) &
             + x_(2) * hess(3,2) + grad(2,2) * grad(2,2) &
             + x_(3) * hess(3,3) + grad(2,3) * grad(2,3)
        A12  = x_(1) * hess(2,1) + grad(1,1) * grad(2,1) &
             + x_(2) * hess(2,2) + grad(1,2) * grad(2,2) &
             + x_(3) * hess(2,3) + grad(1,3) * grad(2,3)
        d2fval(1,1) = A11*L*L - A(1)*A(1)
        d2fval(2,1) = A12*L*L - A(1)*A(2)
        d2fval(1,2) = d2fval(2,1)
        d2fval(2,2) = A22*L*L - A(2)*A(2)
        d2fval = d2fval/(L*L*L)
      end if
    end if

  end subroutine surf_pt_dist_fun_old


  pure subroutine volm_pt_dist_fun_old(this,X1,X2,X3,pt,xyz_pt,fval,dfval,d2fval)
    type(interpolant_t),                intent(in)  :: this
    real(dp), dimension(:,:,:),         intent(in)  :: X1, X2, X3
    real(dp), dimension(3),             intent(in)  :: pt
    real(dp), dimension(3),             intent(in)  :: xyz_pt
    real(dp),                           intent(out) :: fval
    real(dp), dimension(3),   optional, intent(out) :: dfval
    real(dp), dimension(3,3), optional, intent(out) :: d2fval
    real(dp), dimension(3,3) :: grad
    real(dp), dimension(6,3) :: hess
    real(dp) :: L, L2, A11, A22, A33, A12, A13, A23
    real(dp), dimension(3) :: x_, A

    if ( present(d2fval) ) then
      call volm_pt_diff(this,X1,X2,X3,pt,x_,grad=grad,hess=hess)
    elseif ( present(dfval) ) then
      call volm_pt_diff(this,X1,X2,X3,pt,x_,grad=grad)
    else
      call volm_pt_diff(this,X1,X2,X3,pt,x_)
    end if

    x_ = x_ - xyz_pt
    fval = norm2(x_)

    if ( present(d2fval).or.present(dfval) ) then
      L    = fval
      L2   = L*L
      A(1) = x_(1) * grad(1,1) + x_(2) * grad(1,2) + x_(3) * grad(1,3)
      A(2) = x_(1) * grad(2,1) + x_(2) * grad(2,2) + x_(3) * grad(2,3)
      A(3) = x_(1) * grad(3,1) + x_(2) * grad(3,2) + x_(3) * grad(3,3)
      if ( present(dfval)  ) dfval = A / L
      if ( present(d2fval) ) then
        A11  = x_(1) * hess(1,1) + grad(1,1) * grad(1,1) &
             + x_(2) * hess(1,2) + grad(1,2) * grad(1,2) &
             + x_(3) * hess(1,3) + grad(1,3) * grad(1,3)
        A22  = x_(1) * hess(2,1) + grad(2,1) * grad(2,1) &
             + x_(2) * hess(2,2) + grad(2,2) * grad(2,2) &
             + x_(3) * hess(2,3) + grad(2,3) * grad(2,3)
        A33  = x_(1) * hess(3,1) + grad(3,1) * grad(3,1) &
             + x_(2) * hess(3,2) + grad(3,2) * grad(3,2) &
             + x_(3) * hess(3,3) + grad(3,3) * grad(3,3)
        A12  = x_(1) * hess(4,1) + grad(1,1) * grad(2,1) &
             + x_(2) * hess(4,2) + grad(1,2) * grad(2,2) &
             + x_(3) * hess(4,3) + grad(1,3) * grad(2,3)
        A13  = x_(1) * hess(5,1) + grad(1,1) * grad(3,1) &
             + x_(2) * hess(5,2) + grad(1,2) * grad(3,2) &
             + x_(3) * hess(5,3) + grad(1,3) * grad(3,3)
        A23  = x_(1) * hess(6,1) + grad(2,1) * grad(3,1) &
             + x_(2) * hess(6,2) + grad(2,2) * grad(3,2) &
             + x_(3) * hess(6,3) + grad(2,3) * grad(3,3)
        d2fval(1,1) = A11*L2 - A(1)*A(1)
        d2fval(2,1) = A12*L2 - A(1)*A(2)
        d2fval(3,1) = A13*L2 - A(1)*A(3)
        d2fval(1,2) = d2fval(2,1)
        d2fval(2,2) = A22*L2 - A(2)*A(2)
        d2fval(3,2) = A23*L2 - A(2)*A(3)
        d2fval(1,3) = d2fval(3,1)
        d2fval(2,3) = d2fval(3,2)
        d2fval(3,3) = A33*L2 - A(3)*A(3)
        d2fval = d2fval/(L**3)
      end if
    end if

  end subroutine volm_pt_dist_fun_old

  pure function pt_interp(this,pt) result(xyz_eval)
    use set_constants, only : zero
    class(interpolant_w_3D_data_t), intent(in) :: this
    real(dp), dimension(:),         intent(in) :: pt
    real(dp), dimension(3)              :: xyz_eval
    select case(size(pt))
    case(1)
      xyz_eval = curv_pt_interp(this%interpolant_t,this%X1(:,1,1),this%X2(:,1,1),this%X3(:,1,1),pt(1))
    case(2)
      xyz_eval = surf_pt_interp(this%interpolant_t,this%X1(:,:,1),this%X2(:,:,1),this%X3(:,:,1),pt)
    case(3)
      xyz_eval = volm_pt_interp(this%interpolant_t,this%X1(:,:,:),this%X2(:,:,:),this%X3(:,:,:),pt)
    end select
  end function pt_interp

  pure subroutine nearest_pt(this,dim,xyz_pt,pt,tol,dist,dist_est,xyz_out)
    use math, only : dist_to_2_pt_line, dist_to_3_pt_plane
    class(interpolant_w_3D_data_t),   intent(in)  :: this
    integer,                          intent(in)  :: dim
    real(dp), dimension(3),           intent(in)  :: xyz_pt
    real(dp), dimension(dim),         intent(out) :: pt
    real(dp),               optional, intent(in)  :: tol
    real(dp),               optional, intent(out) :: dist
    real(dp),               optional, intent(out) :: dist_est
    real(dp), dimension(3), optional, intent(out) :: xyz_out
    integer,  dimension(dim) :: loc1, loc2, loc3
    real(dp), dimension(3) :: A, B, C
    logical :: intersect
    if ( present(dist_est) ) then
      select case(dim)
      case(1)
        call get_nearest_node_1D( this%interpolant_t, xyz_pt, this%X1(:,1,1), this%X2(:,1,1), this%X3(:,1,1), pt, dist=dist, loc1=loc1, loc2=loc2 )
        A = [this%X1(loc1(1),1,1),this%X2(loc1(1),1,1),this%X3(loc1(1),1,1)]
        B = [this%X1(loc2(1),1,1),this%X2(loc2(1),1,1),this%X3(loc2(1),1,1)]
        call dist_to_2_pt_line( xyz_pt, A, B, dist_est, tol=tol, intersect=intersect, pt=xyz_out )
      case(2)
        call get_nearest_node_2D( this%interpolant_t, xyz_pt, this%X1(:,:,1), this%X2(:,:,1), this%X3(:,:,1), pt, dist=dist, loc1=loc1, loc2=loc2, loc3=loc3 )
        A = [this%X1(loc1(1),loc1(2),1),this%X2(loc1(1),loc1(2),1),this%X3(loc1(1),loc1(2),1)]
        B = [this%X1(loc2(1),loc2(2),1),this%X2(loc2(1),loc2(2),1),this%X3(loc2(1),loc2(2),1)]
        C = [this%X1(loc3(1),loc3(2),1),this%X2(loc3(1),loc3(2),1),this%X3(loc3(1),loc3(2),1)]
        call dist_to_3_pt_plane( xyz_pt, A, B, C, dist_est, tol=tol, intersect=intersect, pt=xyz_out )
      case(3)
        ! could check for intersection with tetrahedron...
        call get_nearest_node_3D( this%interpolant_t, xyz_pt, this%X1(:,:,:), this%X2(:,:,:), this%X3(:,:,:), pt, dist=dist )
        dist_est = dist
      end select
    else
      select case(dim)
      case(1)
        call get_nearest_node_1D( this%interpolant_t, xyz_pt, this%X1(:,1,1), this%X2(:,1,1), this%X3(:,1,1), pt, dist=dist )
      case(2)
        call get_nearest_node_2D( this%interpolant_t, xyz_pt, this%X1(:,:,1), this%X2(:,:,1), this%X3(:,:,1), pt, dist=dist )
      case(3)
        call get_nearest_node_3D( this%interpolant_t, xyz_pt, this%X1(:,:,:), this%X2(:,:,:), this%X3(:,:,:), pt, dist=dist )
      end select
      if ( present(xyz_out) ) xyz_out = this%pt_interp(pt)
    end if
    end subroutine nearest_pt

  pure subroutine pt_dist_fun(this,xyz_pt,pt,fval,dfval,d2fval)
    use set_constants, only : zero
    class(interpolant_w_3D_data_t), intent(in) :: this
    real(dp), dimension(3), intent(in)    :: xyz_pt
    real(dp), dimension(:), intent(in)    :: pt
    real(dp),               intent(out)   :: fval
    real(dp), dimension(this%n_dim),            optional, intent(out) :: dfval
    real(dp), dimension(this%n_dim,this%n_dim), optional, intent(out) :: d2fval
    real(dp) :: tmp1, tmp2

    select case(size(pt))
    case(1)
      call curv_pt_dist_fun( this%interpolant_t, this%X1(:,1,1),this%X2(:,1,1),this%X3(:,1,1),pt(1),xyz_pt,fval,dfval=dfval,d2fval=d2fval )
    case(2)
      call surf_pt_dist_fun( this%interpolant_t, this%X1(:,:,1),this%X2(:,:,1),this%X3(:,:,1),pt,xyz_pt,fval,dfval=dfval,d2fval=d2fval )
    case(3)
      call volm_pt_dist_fun( this%interpolant_t, this%X1(:,:,:),this%X2(:,:,:),this%X3(:,:,:),pt,xyz_pt,fval,dfval=dfval,d2fval=d2fval )
    end select
  end subroutine pt_dist_fun


  pure subroutine min_distance( this, xyz_point, pt, dist, xyz_eval, iter,     &
                                max_iter, gamma, c1, c2, optim_tol_abs,        &
                                optim_tol_rel, step_tol_abs, step_tol_rel,     &
                                fun_tol_abs, fun_tol_rel, clip, status )
    use math,          only : linear_solve
    use set_constants, only : near_zero, zero, one, two, four, half
    class(interpolant_w_3D_data_t),   intent(in)    :: this
    real(dp), dimension(3),           intent(in)    :: xyz_point
    real(dp), dimension(:),           intent(inout) :: pt
    real(dp),                         intent(out)   :: dist
    real(dp), dimension(3), optional, intent(out)   :: xyz_eval
    integer,                optional, intent(out)   :: iter
    integer,                optional, intent(in)    :: max_iter
    real(dp),               optional, intent(in)    :: gamma, c1, c2
    real(dp),               optional, intent(in)    :: optim_tol_abs, optim_tol_rel
    real(dp),               optional, intent(in)    :: step_tol_abs, step_tol_rel
    real(dp),               optional, intent(in)    :: fun_tol_abs, fun_tol_rel
    logical,                optional, intent(in)    :: clip
    integer,                optional, intent(out)   :: status
    real(dp), dimension(this%n_dim) :: dk, xk, xkp1, dfk, dfkp1
    real(dp), dimension(this%n_dim,this%n_dim) :: d2fk
    real(dp) :: dfk0norm, dk0norm, fk0, fk, fkp1, eta, gamma_, c1_, c2_
    real(dp) :: opttola, opttolr, stola, stolr, ftola, ftolr
    real(dp) :: opta,    optr,    esa,   esr,   efa,   efr, dk_mag, m1, m2
    integer :: j, k, max_iter_
    logical  :: wc1, wc2, converged, clip_
    real(dp), parameter :: h = 1.0e-6_dp
    ! real(dp) :: a_i, a_lo, a_hi, a_im1, f_0, f_i, f_im1, df_0, df_i, df_im1
    ! logical  :: zoom, line_conv
    ! integer  :: iter
    ! real(dp), dimension(this%n_dim) :: dfk_alt
    ! real(dp), dimension(this%n_dim,this%n_dim) :: d2fk_alt
    ! real(dp) :: f__, fp_, fm_, f_p, f_m, fpp, fpm, fmp, fmm

    if ( present(status) ) status = 0

    max_iter_ = 10000
    if ( present(max_iter) ) max_iter_ = max_iter

    gamma_ = 0.5_dp
    if ( present(gamma) ) gamma_ = gamma

    c1_ = 1.0e-6_dp
    if ( present(c1) ) c1_ = c1

    c2_ = 0.1_dp
    if ( present(c2) ) c2_ = c2

    opttola = 1.0e-15_dp
    if ( present(optim_tol_abs) ) opttola = optim_tol_abs

    opttolr = 1.0e-10_dp
    if ( present(optim_tol_rel) ) opttolr = optim_tol_rel

    stola = 1.0e-15_dp
    if ( present(step_tol_abs) ) stola = step_tol_abs

    stolr = 1.0e-15_dp
    if ( present(step_tol_rel) ) stolr = step_tol_rel

    ftola = 1.0e-15_dp
    if ( present(fun_tol_abs) ) ftola = fun_tol_abs

    ftolr = 1.0e-10_dp
    if ( present(fun_tol_rel) ) ftolr = fun_tol_rel

    clip_ = .false.
    if (present(clip)) clip_ = clip

    xk = pt(1:this%n_dim)

    eta = one
    
    
    ! f__ = norm2( this%pt_interp(xk + [0.0_dp,0.0_dp]) - xyz_point )
    ! fp_ = norm2( this%pt_interp(xk + [     h,0.0_dp]) - xyz_point )
    ! fm_ = norm2( this%pt_interp(xk + [    -h,0.0_dp]) - xyz_point )
    ! f_p = norm2( this%pt_interp(xk + [0.0_dp,     h]) - xyz_point )
    ! f_m = norm2( this%pt_interp(xk + [0.0_dp,    -h]) - xyz_point )
    ! fpp = norm2( this%pt_interp(xk + [     h,     h]) - xyz_point )
    ! fpm = norm2( this%pt_interp(xk + [     h,    -h]) - xyz_point )
    ! fmp = norm2( this%pt_interp(xk + [    -h,     h]) - xyz_point )
    ! fmm = norm2( this%pt_interp(xk + [    -h,    -h]) - xyz_point )

    ! dfk_alt(1) = (fp_ - fm_)/(two*h)
    ! dfk_alt(2) = (f_p - f_m)/(two*h)

    ! d2fk_alt(1,1) = (fp_ - two*f__ + fm_)/(h*h)
    ! d2fk_alt(2,2) = (f_p - two*f__ + f_m)/(h*h)
    ! d2fk_alt(1,2) = (fpp - fpm - fmp + fmm)/(four*h*h)
    ! d2fk_alt(2,1) = d2fk_alt(1,2)

    call this%pt_dist_fun(xyz_point,xk,fk)
    dist = sqrt( fk )
    k = 1
    if ( dist > near_zero) then
      call this%pt_dist_fun(xyz_point,xk,fk,dfval=dfk,d2fval=d2fk)
      call linear_solve(-d2fk,dfk,dk,status=status)
      ! if ( all( [(abs(d2fk(j,j))>h,j=1,this%n_dim)] ) ) then
      !   call linear_solve(-d2fk,dfk,dk,status=status)
      !   m1 = dot_product(dfk,dk)
      !   if (m1>zero) then
      !     dk = -dfk
      !   end if
      ! else
      !   dk = -dfk
      ! end if
      m1 = dot_product(dfk,dk)

      fk0      = fk
      dk0norm  = norm2(dk)
      dfk0norm = maxval(abs(dfk))

      do k = 1,max_iter_

        ! if ( all( [(abs(d2fk(j,j))>h,j=1,this%n_dim)] ) ) then
          call linear_solve(-d2fk,dfk,dk,status=status)
        !   m1 = dot_product(dfk,dk)
        !   if (m1>zero) then
        !     dk = -dfk
        !   end if
        ! else
        !   dk = -dfk
        ! end if
        m1 = dot_product(dfk,dk)

        eta = min(one,eta/gamma_)
        ! backtracking loop
        do
          xkp1 = xk + eta*dk
          esa = norm2(xkp1-xk) ! calculate change in step size
          if ((esa<stola).or.(eta<h)) then
            exit
          end if
          
          call this%pt_dist_fun(xyz_point,xkp1,fkp1,dfval=dfkp1)

          ! Wolfe conditions
          wc1 = fkp1 <= fk + c1_*eta*dot_product(dfk,dk)
          ! wc2 = abs( dot_product(dfkp1,dk) ) <= c2_*abs( dot_product(dfk,dk) )
          wc2 = dot_product(dfkp1,dk) >= c2_*dot_product(dfk,dk)
          if ( wc1 .and. wc2 ) then
            exit
          end if
          eta = gamma_*eta ! calculate new eta
        end do

        if ( clip_ ) xkp1 = min( max( xkp1,-one-h), one+h)
        
        ! 1st order optimality measure
        opta = maxval(abs(dfk))
        optr = opta / (dfk0norm + near_zero)

        ! step length measure 
        esa = norm2(xkp1-xk)         ! calculate change in step size
        esr = esa/(one + norm2(xk))

        ! function value measure
        efa = abs(fkp1 - fk)    ! calculate change in function
        efr = efa/(one+abs(fk))
        converged = ((optr<opttolr).and.(efr<ftolr)).or.(esr<stolr)
        converged = converged.or.( ((opta<opttola).and.(efa<ftola)).or.(esa<stola) )

        ! save the last step
        xk = xkp1

        if ( converged ) then
          exit
        end if

        ! take the next step
        call this%pt_dist_fun(xyz_point,xk,fk,dfval=dfk,d2fval=d2fk)
        
      end do

      if ( any( abs(xk)>one+h ) ) then
        if ( clip_ ) then
          xk = min(max(xk,-one-h),one+h)
          call this%pt_dist_fun(xyz_point,xk,fk)
        else
          if ( present(status) ) status = 1
        end if
      end if

      ! output
      pt = 1
      pt(1:this%n_dim) = xk
      dist = sqrt( fk )
      if (present(xyz_eval) ) xyz_eval = this%pt_interp(xk)
    else
      if (present(xyz_eval) ) xyz_eval = xyz_point
    end if

    if ( present(iter) ) iter = k

  end subroutine min_distance

  pure elemental function interp_step_length(a_jm1,a_jm2,f_jm1,f_jm2,df_jm1,df_jm2) result(a_j)
    use set_constants, only : one, two, three
    real(dp), intent(in) :: a_jm1, a_jm2, f_jm1, f_jm2, df_jm1, df_jm2
    real(dp)             :: a_j
    real(dp) :: d1, d2
    d1 = df_jm2 + df_jm1 - three * ( f_jm2 - f_jm1 )/( a_jm2 - a_jm1)
    d2 = sign(one,a_jm1-a_jm2) * sqrt( d1**2 - df_jm2 * df_jm1 )
    a_j = a_jm1 - ( a_jm1 - a_jm2 ) * ( df_jm1 + d2 - d1 )/( df_jm1 - df_jm2 +two*d2 )
  end function interp_step_length

  pure elemental subroutine zoom_step_length( zoom, a_lo, a_hi, a_j, f_0, f_lo, f_j, df_0, df_j, c1, c2 )
    use set_constants, only : zero
    logical,  intent(inout) :: zoom
    real(dp), intent(inout) :: a_lo, a_hi, a_j
    real(dp), intent(in)    :: f_0, f_lo, f_j, df_0, df_j, c1, c2
    
    if (zoom) then
      if ( (f_j > f_0 + c1*a_j*df_0).or.(f_j>=f_lo) ) then
        a_hi = a_j
      else
        if ( abs(df_j) <= -c2*df_0 ) then
          zoom      = .false.
          return
        end if
        if ( df_j*(a_hi-a_lo) >= zero) then
          a_hi = a_lo
        end if
        a_lo = a_j
      end if
    end if
  end subroutine zoom_step_length

  pure elemental subroutine line_search( iter, a_i, a_lo, a_hi, a_im1, f_0, f_i, f_im1, df_0, df_i, df_im1, c1, c2, zoom, converged )
    use set_constants, only : zero, one
    integer,  intent(in)    :: iter
    real(dp), intent(inout) :: a_i, a_lo, a_hi
    real(dp), intent(in)    :: a_im1, f_0, f_i, f_im1, df_0, df_i, df_im1, c1, c2
    logical,  intent(inout) :: zoom
    logical,  intent(out)   :: converged
    real(dp), parameter :: a_max = one

    converged = .false.

    if ( zoom ) return

    if ( ( f_i > f_0 + c1 * a_i * df_0 ).or.((iter==1).and.(f_i >= f_im1)) ) then
      a_lo = a_im1
      a_hi = a_i
      zoom = .true.
      return
    end if

    if ( abs(df_i) <= -c2*df_0 ) then
      zoom      = .false.
      converged = .true.
      return
    end if

    if ( df_i >= zero ) then
      a_lo = a_i
      a_hi = a_im1
      zoom = .true.
      return
    end if

    a_i = min( max( interp_step_length(a_i,a_im1,f_i,f_im1,df_i,df_im1), a_i ), a_max )
  end subroutine line_search



end module interpolant_derived_type

module quadrature_derived_type

  use set_precision,       only : dp
  use set_constants,       only : zero
  implicit none
  private
  public :: quad_t
  public :: quad_ptr, quad_ptr_3D
  public :: create_quad_ref_1D, create_quad_ref_2D, create_quad_ref_3D
  public :: map_quad_ref_to_physical
  public :: num_quad_pts
  public :: reference_quad_t
  type quad_t
    integer :: n_quad = 0
    real(dp), allocatable, dimension(:,:) :: quad_pts
    real(dp), allocatable, dimension(:)   :: quad_wts
  contains
    private
    procedure, public, pass :: create  => allocate_quad
    procedure, public, pass :: destroy => deallocate_quad
    generic,   public       :: integrate => integrate_scalar, integrate_vector
    procedure :: integrate_scalar
    procedure :: integrate_vector
  end type quad_t

  type reference_quad_t
    integer :: n_dim, n_quad, n_face, n_edge
    type(quad_t)                            :: v
    type(quad_t), dimension(:), allocatable :: f
    type(quad_t), dimension(:), allocatable :: e
  contains
      private
      procedure, public :: create  => create_reference_quad
      procedure, public :: destroy => destroy_reference_quad
  end type reference_quad_t
  type quad_ptr
    type(quad_t), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_quad_ptr
  end type quad_ptr

  type quad_ptr_3D
    type(quad_t), dimension(:,:,:), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_quad_ptr_3D
  end type quad_ptr_3D

contains

  pure function get_end_pts(n_dim,i) result(b)
    use set_precision, only : dp
    use combinatorics, only : get_hypercube_vertex_coord
    integer,  intent(in)     :: n_dim, i
    real(dp), dimension(n_dim) :: b
    integer k
    b = real( 2* ( get_hypercube_vertex_coord(spread(2,1,n_dim),[(k,k=1,n_dim)],i) - 1 ) - 1, dp )
  end function get_end_pts
  
  pure elemental subroutine create_reference_quad( this, n_dim, n_quad, include_ends, status )
    use set_constants, only : zero, one
    class(reference_quad_t), intent(inout) :: this
    integer,                 intent(in)    :: n_dim, n_quad
    logical, optional,       intent(in)    :: include_ends
    integer, optional,       intent(out)   :: status
    integer :: i
    this%n_dim = n_dim
    this%n_quad = n_quad

    select case(this%n_dim)
    case(0)
      this%n_quad = 1
      this%n_face = 0
      this%n_edge = 0
      allocate( this%f(0) )
      allocate( this%e(0) )
      call this%v%create(1)
      this%v%quad_pts = zero
      this%v%quad_wts = one
    case(1)
      this%n_face = 2
      this%n_edge = 0
      allocate( this%f(2) )
      allocate( this%e(0) )
      call create_quad_ref_1D( 0, this%v, include_ends=include_ends, nq=n_quad )
      call this%f(1:2)%create(1)
      do i = 1,2
        this%f(i)%quad_wts = one
        this%f(i)%quad_pts = zero
        this%f(i)%quad_pts(1:1,1) = get_end_pts(n_dim,i)
      end do
    case(2)
      this%n_face = 4
      this%n_edge = 4
      allocate( this%f(4) )
      allocate( this%e(4) )
      ! call this%v%create(n_quad**2)
      call create_quad_ref_2D( 0, this%v, include_ends=include_ends, nq=n_quad )
      do i = 1,4
        ! call this%f(i)%create(n_quad)
        call create_quad_ref_1D( 0, this%f(i), include_ends=include_ends, nq=n_quad )
      end do
      do i = 1,4
        call this%e(i)%create(1)
        this%e(i)%quad_wts = one
        this%e(i)%quad_pts = zero
        this%e(i)%quad_pts(1:2,1) = get_end_pts(n_dim,i)
      end do
    case(3)
      this%n_face = 6
      this%n_edge = 12
      allocate( this%f(6) )
      allocate( this%e(12) )
      ! call this%v%create(n_quad**3)
      call create_quad_ref_3D( 0, this%v, include_ends=include_ends, nq=n_quad )
      
      do i = 1,6
        ! call this%f(1:6)%create(n_quad**2)
        call create_quad_ref_2D( 0, this%f(i), include_ends=include_ends, nq=n_quad )
      end do
      do i = 1,12
        call this%e(i)%create(n_quad)
        call create_quad_ref_1D( 0, this%e(i), include_ends=include_ends, nq=n_quad )
      end do
    case default
      this%n_dim  = n_dim
      this%n_quad = n_quad
      this%n_face = 0
      this%n_edge = 0
      allocate( this%f(0) )
      allocate( this%e(0) )
      call this%v%create(0)
      status = -1
    end select

  end subroutine create_reference_quad

  pure elemental subroutine destroy_reference_quad( this )
    class(reference_quad_t), intent(inout) :: this
    this%n_quad = 0
    this%n_face = 0
    this%n_edge = 0
    call this%v%destroy()
    if ( allocated(this%f) ) then
      call this%f%destroy()
      deallocate( this%f )
    end if
    if ( allocated(this%e) ) then
      call this%e%destroy()
      deallocate( this%e )
    end if
  end subroutine destroy_reference_quad

    

  pure elemental subroutine destroy_quad_ptr_3D( this )
    class(quad_ptr_3D), intent(inout) :: this
    this%p => null()
  end subroutine destroy_quad_ptr_3D

  pure elemental subroutine destroy_quad_ptr( this )
    class(quad_ptr), intent(inout) :: this
    this%p => null()
  end subroutine destroy_quad_ptr

  pure elemental subroutine allocate_quad( this, n_quad )
    use set_constants, only : zero
    class(quad_t), intent(inout) :: this
    integer,       intent(in)    :: n_quad
    this%n_quad = n_quad
    allocate( this%quad_pts(3,n_quad) )
    this%quad_pts = zero
    allocate( this%quad_wts(n_quad) )
    this%quad_wts = zero
  end subroutine allocate_quad

  pure elemental subroutine deallocate_quad( this )
    class(quad_t), intent(inout) :: this
    this%n_quad = 0
    if( allocated( this%quad_wts  ) ) deallocate( this%quad_wts  )
    if( allocated( this%quad_pts  ) ) deallocate( this%quad_pts  )
  end subroutine deallocate_quad

  pure function integrate_scalar( this, f ) result( integral )
    use set_precision, only : dp
    class(quad_t),                    intent(in) :: this
    real(dp), dimension(this%n_quad), intent(in) :: f
    real(dp)                                     :: integral
    integral = dot_product(f,this%quad_wts)
  end function integrate_scalar

  pure function integrate_vector( this, neq, f ) result( integral )
    use set_precision, only : dp
    class(quad_t),                        intent(in) :: this
    integer,                              intent(in) :: neq
    real(dp), dimension(neq,this%n_quad), intent(in) :: f
    real(dp), dimension(neq)                         :: integral
    integer :: n
    do n = 1, neq
      integral(n) = dot_product(f(n,:),this%quad_wts)
    end do
  end function integrate_vector

  pure function num_quad_pts(quad_order,n_dim,include_ends) result(n_pts)
    integer, intent(in) :: quad_order, n_dim
    logical, intent(in) :: include_ends
    integer, dimension(n_dim) :: n_pts
    n_pts(1:n_dim) = gauss_1D_size(quad_order) + merge(2,0,include_ends)
  end function num_quad_pts

  pure function gauss_1D_size( polynomial_order ) result( N_quad )
    use set_constants, only : half
    integer, intent(in) :: polynomial_order
    integer             :: N_quad
    N_quad = ceiling( half*(polynomial_order + 1) )
  end function gauss_1D_size

  pure subroutine gauss_1D( n_quad, pts_1D, wts_1D )
    use math, only : LegendreGaussNodesAndWeights
    integer,                       intent(in)  :: n_quad
    real(dp), dimension( n_quad ), intent(out) :: pts_1D
    real(dp), dimension( n_quad ), intent(out) :: wts_1D
    call LegendreGaussNodesAndWeights(n_quad-1, pts_1D, wts_1D)
  end subroutine gauss_1D

  pure subroutine gauss_1D_w_ends(n_quad,pts_1D,wts_1D,include_ends,n_out,s,e,o)
    use set_constants, only : one
    integer,                         intent(in)  :: n_quad
    real(dp), dimension(0:n_quad+1), intent(out) :: pts_1D
    real(dp), dimension(0:n_quad+1), intent(out) :: wts_1D
    logical,  optional,              intent(in)  :: include_ends
    integer,  optional,              intent(out) :: n_out, s, e, o
    logical :: include_ends_
    include_ends_ = .false.
    if ( present(include_ends) ) include_ends_ = include_ends
    pts_1D = zero; pts_1D(0) = -one; pts_1D(n_quad+1) = one
    wts_1D = zero
    call gauss_1D(n_quad,pts_1D(1:n_quad),wts_1D(1:n_quad))
    if ( include_ends_ ) then
      if ( present(n_out) ) n_out = n_quad+2
      if ( present(s)     ) s     = 0
      if ( present(e)     ) e     = n_quad+1
      if ( present(o)     ) o     = 1
    else
      if ( present(n_out) ) n_out = n_quad
      if ( present(s)     ) s     = 1
      if ( present(e)     ) e     = n_quad
      if ( present(o)     ) o     = 0
    end if
  end subroutine gauss_1D_w_ends

  pure subroutine create_quad_ref_1D( quad_order, quad_ref, include_ends, nq, loc )
    use index_conversion, only : get_dim_order
    integer,           intent(in)  :: quad_order
    type(quad_t),      intent(out) :: quad_ref
    logical, optional, intent(in)  :: include_ends
    integer, optional, intent(in)  :: nq
    integer, dimension(3), optional, intent(in) :: loc
    real(dp), dimension(:), allocatable :: pts_1D
    real(dp), dimension(:), allocatable :: wts_1D
    integer, dimension(3) :: dir, end_pt
    integer :: n_quad, n_gauss
    integer :: start_idx, end_idx
    integer :: i
    dir     = [(i,i=1,3)]
    end_pt  = 0
    if ( present(loc) ) call get_dim_order(loc,dir,end_pt)
    n_gauss = gauss_1D_size( quad_order )
    if ( present(nq) ) n_gauss = nq
    allocate( pts_1D(0:n_gauss+1), wts_1D(0:n_gauss+1) )
    call gauss_1D_w_ends( n_gauss, pts_1D, wts_1D, include_ends=include_ends, n_out=n_quad, s=start_idx, e=end_idx )
    call quad_ref%destroy()
    call quad_ref%create( n_quad )
    quad_ref%quad_wts           = wts_1D(start_idx:end_idx)
    quad_ref%quad_pts(dir(1),:) = pts_1D(start_idx:end_idx)
    quad_ref%quad_pts(dir(2),:) = real( end_pt(2), dp )
    quad_ref%quad_pts(dir(3),:) = real( end_pt(3), dp )
    deallocate( pts_1D, wts_1D )
  end subroutine create_quad_ref_1D

  pure subroutine create_quad_ref_2D( quad_order, quad_ref, include_ends, nq, loc )
    use index_conversion, only : get_dim_order
    integer,                          intent(in)  :: quad_order
    type(quad_t),                     intent(out) :: quad_ref
    logical,                optional, intent(in)  :: include_ends
    integer,                optional, intent(in)  :: nq
    integer,  dimension(3), optional, intent(in)  :: loc
    real(dp), dimension(:), allocatable :: pts_1D
    real(dp), dimension(:), allocatable :: wts_1D
    integer, dimension(3) :: dir, end_pt
    integer :: n_quad, n_gauss
    integer :: offset
    integer :: i, j, cnt
    dir     = [(i,i=1,3)]
    end_pt  = 0
    if ( present(loc) ) call get_dim_order(loc,dir,end_pt)
    n_gauss = gauss_1D_size( quad_order )
    if ( present(nq) ) n_gauss = nq
    allocate( pts_1D(0:n_gauss+1), wts_1D(0:n_gauss+1) )
    call gauss_1D_w_ends( n_gauss, pts_1D, wts_1D, include_ends=include_ends, n_out=n_quad, o=offset )
    call quad_ref%destroy()
    call quad_ref%create( n_quad**2 )
    cnt = 0
    do j = 1-offset, n_gauss+offset
      do i = 1-offset, n_gauss+offset
        cnt = cnt + 1
        quad_ref%quad_wts(cnt)        = wts_1D(i)*wts_1D(j)
        quad_ref%quad_pts(dir(1),cnt) = pts_1D(i)
        quad_ref%quad_pts(dir(2),cnt) = pts_1D(j)
        quad_ref%quad_pts(dir(3),cnt) = real( end_pt(3), dp )
      end do
    end do
    deallocate( pts_1D, wts_1D )
  end subroutine create_quad_ref_2D

  pure subroutine create_quad_ref_3D( quad_order, quad_ref, include_ends, nq, loc )
    use index_conversion, only : get_dim_order
    integer,      intent(in)  :: quad_order
    type(quad_t), intent(out) :: quad_ref
    logical, optional, intent(in) :: include_ends
    integer, optional, intent(in)  :: nq
    integer,  dimension(3), optional, intent(in)  :: loc
    real(dp), dimension(:), allocatable :: pts_1D
    real(dp), dimension(:), allocatable :: wts_1D
    integer, dimension(3) :: dir, end_pt
    integer :: n_quad, n_gauss
    integer :: start_idx, end_idx, offset
    integer :: i, j, k, cnt
    dir     = [(i,i=1,3)]
    end_pt  = 0
    if ( present(loc) ) call get_dim_order(loc,dir,end_pt)
    n_gauss = gauss_1D_size( quad_order )
    if ( present(nq) ) n_gauss = nq
    allocate( pts_1D(0:n_gauss+1), wts_1D(0:n_gauss+1) )
    call gauss_1D_w_ends( n_gauss, pts_1D, wts_1D, include_ends=include_ends, n_out=n_quad, o=offset )
    call quad_ref%destroy()
    call quad_ref%create( n_quad**3 )
    cnt = 0
    do k = 1-offset, n_gauss+offset
      do j = 1-offset, n_gauss+offset
        do i = 1-offset, n_gauss+offset
          cnt = cnt + 1
          quad_ref%quad_pts(dir(1),cnt) = pts_1D(i)
          quad_ref%quad_pts(dir(2),cnt) = pts_1D(j)
          quad_ref%quad_pts(dir(3),cnt) = pts_1D(k)
          quad_ref%quad_wts(cnt) = wts_1D(i)*wts_1D(j)*wts_1D(k)
        end do
      end do
    end do
    deallocate( pts_1D, wts_1D )
  end subroutine create_quad_ref_3D

  pure subroutine map_quad_ref_to_physical_0D( X1, X2, X3, interpolant, quad_ref, quad_physical, vol, centroid )
    use interpolant_derived_type, only : interpolant_t
    use set_constants,            only : zero, one
    real(dp),               intent(in)  :: X1, X2, X3
    type(interpolant_t),    intent(in)  :: interpolant
    type(quad_t),           intent(in)  :: quad_ref
    type(quad_t),           intent(out) :: quad_physical
    real(dp),     optional, intent(out) :: vol
    real(dp), dimension(3), optional, intent(out) :: centroid
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      quad_physical%quad_pts(1,n) = X1
      quad_physical%quad_pts(2,n) = X2
      quad_physical%quad_pts(3,n) = X3
      quad_physical%quad_wts(n) = one
    end do
    if ( present(vol) ) vol = zero
    if ( present(centroid) ) centroid = [X1,X2,X3]
  end subroutine map_quad_ref_to_physical_0D

  pure subroutine map_quad_ref_to_physical_1D( X1, X2, X3, interpolant, quad_ref, quad_physical, vol, centroid )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    type(interpolant_t),    intent(in)  :: interpolant
    type(quad_t),           intent(in)  :: quad_ref
    type(quad_t),           intent(out) :: quad_physical
    real(dp),     optional, intent(out) :: vol
    real(dp), dimension(3), optional, intent(out) :: centroid
    real(dp) :: dS
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_curve( [quad_ref%quad_pts(1,n)], X1, X2, X3, quad_physical%quad_pts(:,n), dS )
      quad_physical%quad_wts(n) = dS * quad_ref%quad_wts(n)
    end do
    dS = sum(quad_physical%quad_wts)
    if ( present(vol) ) vol = dS
    if ( present(centroid) ) centroid = quad_physical%integrate(3,quad_physical%quad_pts) /dS
  end subroutine map_quad_ref_to_physical_1D

  pure subroutine map_quad_ref_to_physical_2D( X1, X2, X3, interpolant, quad_ref, quad_physical, vol, centroid )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:), intent(in)  :: X1, X2, X3
    type(interpolant_t),      intent(in)  :: interpolant
    type(quad_t),             intent(in)  :: quad_ref
    type(quad_t),             intent(out) :: quad_physical
    real(dp),       optional, intent(out) :: vol
    real(dp), dimension(3), optional, intent(out) :: centroid
    real(dp) :: dA
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_surface( quad_ref%quad_pts(1:2,n), X1, X2, X3, quad_physical%quad_pts(:,n), dA )
      quad_physical%quad_wts(n) = dA * quad_ref%quad_wts(n)
    end do
    dA = sum(quad_physical%quad_wts)
    if ( present(vol) ) vol = dA
    if ( present(centroid) ) centroid = quad_physical%integrate(3,quad_physical%quad_pts) /dA
  end subroutine map_quad_ref_to_physical_2D

  pure subroutine map_quad_ref_to_physical_3D( X1, X2, X3, interpolant, quad_ref, quad_physical, vol, centroid )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    type(interpolant_t),        intent(in)  :: interpolant
    type(quad_t),               intent(in)  :: quad_ref
    type(quad_t),               intent(out) :: quad_physical
    real(dp),         optional, intent(out) :: vol
    real(dp), dimension(3), optional, intent(out) :: centroid
    real(dp) :: dV
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_volume( quad_ref%quad_pts(1:3,n), X1, X2, X3, quad_physical%quad_pts(:,n), dV )
      quad_physical%quad_wts(n) = dV * quad_ref%quad_wts(n)
    end do
    dV = sum(quad_physical%quad_wts)
    if ( present(vol) ) vol = dV
    if ( present(centroid) ) centroid = quad_physical%integrate(3,quad_physical%quad_pts) /dV
  end subroutine map_quad_ref_to_physical_3D

  pure subroutine map_quad_ref_to_physical( X1, X2, X3, loc, interpolant, quad_ref, quad_physical, face_vec, normal, centroid, vol, status )
    use set_constants,            only : zero
    use index_conversion,         only : get_reshape_indices
    use interpolant_derived_type, only : interpolant_t
    use vector_derived_type,      only : vec_t
    real(dp), dimension(:,:,:),       intent(in)  :: X1, X2, X3
    integer,  dimension(3),           intent(in)  :: loc
    type(interpolant_t),              intent(in)  :: interpolant
    type(quad_t),                     intent(in)  :: quad_ref
    type(quad_t),                     intent(out) :: quad_physical
    type(vec_t),         optional, intent(out) :: face_vec
    real(dp), dimension(3), optional, intent(out) :: normal
    real(dp), dimension(3), optional, intent(out) :: centroid
    real(dp),               optional, intent(out) :: vol
    integer,                optional, intent(out) :: status
    type(quad_t)   :: quad_ref_tmp
    type(vec_t) :: face_vec_tmp
    real(dp), allocatable, dimension(:,:) :: X1_tmp, X2_tmp, X3_tmp
    integer, dimension(3) :: idx_start, idx_end, sz_in, sz_out
    integer  :: sz_cnt, dir
    if (present(status)) status = 1
    sz_in = shape(X1)
    call get_reshape_indices(sz_in, loc, sz_cnt, dir, sz_out, idx_start, idx_end )
    select case(sz_cnt)
    case(0)
      call map_quad_ref_to_physical_0D( X1(idx_end(1),idx_end(2),idx_end(3)),  &
                                        X2(idx_end(1),idx_end(2),idx_end(3)),  &
                                        X3(idx_end(1),idx_end(2),idx_end(3)),  &
                                        interpolant, quad_ref, quad_physical, vol=vol, centroid=centroid )
    case(1)
      allocate( X1_tmp(sz_out(1),1), X2_tmp(sz_out(1),1), X3_tmp(sz_out(1),1) )
      X1_tmp(:,1) = reshape( X1( idx_start(1):idx_end(1), &
                                 idx_start(2):idx_end(2), &
                                 idx_start(3):idx_end(3)), [sz_out(1)] )
      X2_tmp(:,1) = reshape( X2( idx_start(1):idx_end(1), &
                                 idx_start(2):idx_end(2), &
                                 idx_start(3):idx_end(3)), [sz_out(1)] )
      X3_tmp(:,1) = reshape( X3( idx_start(1):idx_end(1), &
                                 idx_start(2):idx_end(2), &
                                 idx_start(3):idx_end(3)), [sz_out(1)] )
      call map_quad_ref_to_physical_1D( X1_tmp(:,1), X2_tmp(:,1), X3_tmp(:,1), &
                                        interpolant, quad_ref, quad_physical, vol=vol, centroid=centroid )
    case(2)
      allocate( X1_tmp(sz_out(1),sz_out(2)) )
      allocate( X2_tmp(sz_out(1),sz_out(2)) )
      allocate( X3_tmp(sz_out(1),sz_out(2)) )
      X1_tmp = reshape( X1( idx_start(1):idx_end(1), &
                            idx_start(2):idx_end(2), &
                            idx_start(3):idx_end(3)), sz_out(1:2) )
      X2_tmp = reshape( X2( idx_start(1):idx_end(1), &
                            idx_start(2):idx_end(2), &
                            idx_start(3):idx_end(3)), sz_out(1:2) )
      X3_tmp = reshape( X3( idx_start(1):idx_end(1), &
                            idx_start(2):idx_end(2), &
                            idx_start(3):idx_end(3)), sz_out(1:2) )
      call map_quad_ref_to_physical_2D(X1_tmp,X2_tmp,X3_tmp,interpolant,quad_ref,quad_physical,vol=vol, centroid=centroid)
    case(3)
      call map_quad_ref_to_physical_3D(X1,X2,X3,interpolant,quad_ref,quad_physical,vol=vol, centroid=centroid)
    case default
      if (present(status)) status = -1
    end select

    if ( present(face_vec) .or. present(normal) ) then
      select case(sz_cnt)
      case(0)
        call set_face_normals_1D( X1, X2, X3, dir, interpolant, quad_ref, face_vec=face_vec, normal=normal )
      case(1)
        call create_quad_ref_1D(0,quad_ref_tmp,nq=quad_ref%n_quad,loc=loc)
        call set_face_normals_2D( X1, X2, X3, dir, interpolant, quad_ref_tmp, face_vec=face_vec, normal=normal )
      case(2)
        call create_quad_ref_2D(0,quad_ref_tmp,nq=int(sqrt(real(quad_ref%n_quad))),loc=loc)
        call set_face_normals_3D( X1, X2, X3, dir, interpolant, quad_ref_tmp, face_vec=face_vec, normal=normal )
      case default
        if (present(status)) status = -1
      end select
    end if

    call quad_ref_tmp%destroy()
    call face_vec_tmp%destroy()
    if ( allocated(X1_tmp) ) deallocate( X1_tmp )
    if ( allocated(X2_tmp) ) deallocate( X2_tmp )
    if ( allocated(X3_tmp) ) deallocate( X3_tmp )

  end subroutine map_quad_ref_to_physical

  pure subroutine set_face_normals_1D( X1, X2, X3, dir, interpolant, quad_ref, face_vec, normal )
    use set_constants,            only : zero, one
    use interpolant_derived_type, only : interpolant_t
    use vector_derived_type,      only : vec_t
    
    real(dp), dimension(:,:,:),       intent(in)  :: X1, X2, X3
    integer,                          intent(in)  :: dir
    type(interpolant_t),              intent(in)  :: interpolant
    type(quad_t),                     intent(in)  :: quad_ref
    type(vec_t),         optional, intent(out) :: face_vec
    real(dp), dimension(3), optional, intent(out) :: normal

    if ( present(face_vec) ) then
      call face_vec%create(quad_ref%n_quad)
        face_vec%v(1,:) = one
    end if
    if ( present(normal) ) then
      normal    = zero
      normal(1) = one
    end if
  end subroutine set_face_normals_1D

  pure subroutine set_face_normals_2D( X1, X2, X3, dir, interpolant, quad_ref, face_vec, normal )
    use interpolant_derived_type, only : interpolant_t
    use vector_derived_type,      only : vec_t

    real(dp), dimension(:,:,:),       intent(in)  :: X1, X2, X3
    integer,                          intent(in)  :: dir
    type(interpolant_t),              intent(in)  :: interpolant
    type(quad_t),                     intent(in)  :: quad_ref
    type(vec_t),         optional, intent(out) :: face_vec
    real(dp), dimension(3), optional, intent(out) :: normal
    integer  :: n
    real(dp), dimension(3,3) :: Nvec
    real(dp), dimension(3)   :: tmp_vec
    real(dp), dimension(3,quad_ref%n_quad) :: tmp_array

    do n = 1,quad_ref%n_quad
      Nvec = interpolant%normal_vectors_2D( quad_ref%quad_pts(1:3,n), X1, X2, X3 )
      tmp_array(:,n) = Nvec(:,dir)
    end do

    tmp_vec = quad_ref%integrate(3,tmp_array)
    tmp_vec = tmp_vec/norm2(tmp_vec)

    if ( present(face_vec) ) then
      call face_vec%create(quad_ref%n_quad)
      if ( ( size(X1,1) == 2 ) .and. ( size(X1,2) == 2 ) ) then
        do n = 1,quad_ref%n_quad
          face_vec%v(:,n) = tmp_vec
        end do
      else
        face_vec%v = tmp_array
      end if
    end if

    if ( present(normal) ) normal = tmp_vec

  end subroutine set_face_normals_2D

  pure subroutine set_face_normals_3D( X1, X2, X3, dir, interpolant, quad_ref, face_vec, normal )
    use set_constants,           only : zero, one
    use math,                    only : vector_norm
    use interpolant_derived_type, only : interpolant_t
    use vector_derived_type,      only : vec_t
    

    real(dp), dimension(:,:,:),       intent(in)  :: X1, X2, X3
    integer,                          intent(in)  :: dir
    type(interpolant_t),              intent(in)  :: interpolant
    type(quad_t),                     intent(in)  :: quad_ref
    type(vec_t),         optional, intent(out) :: face_vec
    real(dp), dimension(3), optional, intent(out) :: normal
    integer  :: n
    real(dp), dimension(3,3) :: Nvec
    real(dp), dimension(3)   :: tmp_vec
    real(dp), dimension(3,quad_ref%n_quad) :: tmp_array

    do n = 1,quad_ref%n_quad
      Nvec = interpolant%normal_vectors_2D( quad_ref%quad_pts(1:3,n), X1, X2, X3 )
      tmp_array(:,n) = Nvec(:,dir)
    end do

    tmp_vec = quad_ref%integrate(3,tmp_array)
    tmp_vec = tmp_vec/norm2(tmp_vec)

    if ( present(face_vec) ) then
      call face_vec%create(quad_ref%n_quad)
      face_vec%v = tmp_array
    end if

    if ( present(normal) ) normal = tmp_vec

  end subroutine set_face_normals_3D

end module quadrature_derived_type

module face_info_type
    use set_precision, only : dp
  use set_constants, only : zero
  use interpolant_derived_type, only : interpolant_w_3D_data_t
  implicit none
  private
  public :: face_info_t
  ! collect all wall info here prior to wall distance search
  type face_info_t
    integer                             :: block_id
    integer                             :: face_label
    real(dp), dimension(:), allocatable :: out_dir
    type(interpolant_w_3D_data_t)       :: interp
  contains
    private
    procedure, public :: create  => create_face_info_t
    procedure, public :: destroy => destroy_face_info_t
  end type face_info_t

  interface constructor
    module procedure :: face_info_t
  end interface

contains

  pure elemental subroutine destroy_face_info_t( this )
    class(face_info_t), intent(inout) :: this
    this%face_label = 0
    this%out_dir    = zero
    call this%interp%destroy()
  end subroutine destroy_face_info_t

  pure subroutine create_face_info_t( this, n_dim, block_id, face_label, cell_nodes )
    class(face_info_t),           intent(inout) :: this
    integer,                      intent(in)    :: n_dim, block_id, face_label
    real(dp), dimension(:,:,:,:), intent(in)    :: cell_nodes
    real(dp), dimension(:,:,:), allocatable :: face_nodes
    integer, dimension(4) :: tmp
    integer, dimension(3) :: skip, stride
    integer, dimension(2) :: shp
    integer :: dir
    integer :: status

    call this%destroy()

    tmp  = shape(cell_nodes)
    skip = tmp(2:4) - 1
    stride = 1
    ! dir    = face_label
    dir    = merge(-1,1,mod(face_label,2)==0) * face_label/2
    call get_face_coords_from_cell( shp, skip, stride, dir, cell_nodes, status=status )
    allocate( face_nodes(shp(1),shp(2),3) )
    call this%interp%create( pack(face_nodes(:,:,1),.true.), &
                             pack(face_nodes(:,:,2),.true.), &
                             pack(face_nodes(:,:,3),.true.), shape(face_nodes(:,:,1)) )
    deallocate( face_nodes )

    this%block_id   = block_id
    this%face_label = face_label
    this%out_dir    = zero
  end subroutine create_face_info_t

  pure function constructor( n_dim, block_id, face_label, cell_nodes ) result(this)
    integer,                      intent(in) :: n_dim, block_id, face_label
    real(dp), dimension(:,:,:,:), intent(in) :: cell_nodes
    type(face_info_t)                        :: this
    real(dp), dimension(:,:,:), allocatable :: face_nodes
    integer, dimension(4) :: tmp
    integer, dimension(3) :: skip, stride
    integer, dimension(2) :: shp
    integer :: dir
    integer :: status
    call this%destroy()
    tmp  = shape(cell_nodes)
    skip = tmp(2:4) - 1
    stride = 1
    ! dir    = face_label
    dir    = merge(-1,1,mod(face_label,2)==0) * face_label/2
    call get_face_coords_from_cell( shp, skip, stride, dir, cell_nodes, status=status )
    allocate( face_nodes(shp(1),shp(2),3) )
    call this%interp%create( pack(face_nodes(:,:,1),.true.), &
                             pack(face_nodes(:,:,2),.true.), &
                             pack(face_nodes(:,:,3),.true.), shape(face_nodes(:,:,1)) )
    deallocate( face_nodes )

    this%block_id   = block_id
    this%face_label = face_label
    this%out_dir    = zero
  end function constructor

  pure subroutine get_face_coords_from_cell( shp, skip, stride, dir, coords_in, coords_out, status )
    integer, dimension(2),                              intent(inout) :: shp
    integer, dimension(3),                              intent(in)    :: skip
    integer, dimension(3),                              intent(in)    :: stride
    integer,                                            intent(in)    :: dir
    real(dp), dimension( 3, skip(1),skip(2),skip(3) ),  intent(in)    :: coords_in
    real(dp), dimension(shp(1),shp(2),3), optional,     intent(out)   :: coords_out
    integer,                              optional,     intent(out)   :: status
    integer, dimension(3) :: lo, hi

    if ( present(status) ) status = 0
    lo = 1
    hi = 1 + skip

    select case (dir)
      case(-1)
        hi(1) = lo(1)
        shp   = [skip(2),skip(3)] + 1
      case( 1)
        lo(1) = hi(1)
        shp   = [skip(2),skip(3)] + 1
      case(-2)
        hi(2) = lo(2)
        shp   = [skip(1),skip(3)] + 1
      case( 2)
        lo(2) = hi(2)
        shp   = [skip(1),skip(3)] + 1
      case(-3)
        hi(3) = lo(3)
        shp   = [skip(1),skip(2)] + 1
      case( 3)
        lo(3) = hi(3)
        shp   = [skip(1),skip(2)] + 1
      case default
        if ( present(status) ) status = -1
        return
    end select

    if ( present(coords_out) ) then
      if ( any( shape(coords_out) /= [shp(1),shp(2),3] ) ) then
        if ( present(status) ) status = -2
        return
      else
        coords_out = reshape( pack( coords_in(:,lo(1):hi(1):stride(1),           &
                                                lo(2):hi(2):stride(2),           &
                                                lo(3):hi(3):stride(3) ),.true.), &
                                                [shp(1),shp(2),3],     &
                                                order=[3,1,2] )
      end if
    end if

  end subroutine get_face_coords_from_cell

end module face_info_type

module boundary_info_type
  use set_precision, only : dp
  use set_constants, only : zero
  use index_conversion, only : get_face_id_from_node_list, get_face_idx_from_id, node_to_cell_idx
  implicit none
  private
  public :: bc_t, bc_holder_t
  ! collect all wall info here prior to wall distance search
  type bc_t
    ! Indicies of the interior cells that are along this boundary
    integer, dimension(3) :: idx_min = -1
    integer, dimension(3) :: idx_max = -1

    ! Indicies of the interior nodes that are along this boundary
    integer, dimension(3) :: node_idx_min = -1
    integer, dimension(3) :: node_idx_max = -1

    integer :: bound_id   = -1  ! boundary id
    integer :: block_id   = -1  ! What parent block this is on
    integer :: bc_label   = -1  ! What bc type this is
    integer :: face_label = -1  ! What face this is on
  contains
    private
    procedure, public :: create  => create_bc_t
    procedure, public :: destroy => destroy_bc_t
  end type bc_t

  type bc_holder_t
    integer :: n_bounds
    type(bc_t), dimension(:), allocatable :: bc
  contains
    private
    procedure, public :: create  => create_bc_holder_t
    procedure, public :: destroy => destroy_bc_holder_t
  end type bc_holder_t

contains

  pure elemental subroutine destroy_bc_holder_t( this )
    class(bc_holder_t), intent(inout) :: this
    if ( allocated(this%bc) ) then
      call this%bc%destroy
      deallocate( this%bc )
    end if
    this%n_bounds = 0
  end subroutine destroy_bc_holder_t

  pure elemental subroutine destroy_bc_t( this )
    class(bc_t), intent(inout) :: this
    this%idx_min      = -1
    this%idx_min      = -1
    this%node_idx_min = -1
    this%node_idx_max = -1
    this%bound_id     = -1
    this%block_id     = -1
    this%bc_label     = -1
    this%face_label   = -1
  end subroutine destroy_bc_t


  pure subroutine create_bc_t( this, n_dim, block_id, bound_id, bc_label, node_list, node_bnd_min, node_bnd_max, n_skip )
    class(bc_t),                         intent(inout) :: this
    integer,                             intent(in)    :: n_dim, block_id, bound_id, bc_label
    integer, dimension(:),               intent(in)    :: node_list
    integer, dimension(:),     optional, intent(in)    :: node_bnd_min, node_bnd_max, n_skip
    
    integer, dimension(2,n_dim) :: nodes
    call this%destroy()
    this%block_id = block_id
    this%bound_id = bound_id
    this%bc_label = bc_label
    call get_face_id_from_node_list( n_dim, node_list, this%face_label, min_bnd=node_bnd_min, max_bnd=node_bnd_max )
    nodes = reshape( node_list, [2,n_dim] )
    this%node_idx_min          = 1
    this%node_idx_min(1:n_dim) = nodes(1,:)
    this%node_idx_max          = 1
    this%node_idx_max(1:n_dim) = nodes(2,:)
    this%idx_min = this%node_idx_min
    this%idx_max = this%node_idx_max
    call node_to_cell_idx(this%idx_min,this%idx_max)
    if ( present(n_skip) ) then
      this%node_idx_max(1:n_dim) = ( this%node_idx_max(1:n_dim) - 1 )/n_skip(1:n_dim) + 1
      where( this%idx_max(1:n_dim)>1) this%idx_max(1:n_dim) = ( this%idx_max(1:n_dim) )/n_skip(1:n_dim)
    end if
  end subroutine create_bc_t

  pure subroutine create_bc_holder_t( this, n_bounds )
    class(bc_holder_t), intent(inout) :: this
    integer,            intent(in)    :: n_bounds
    call this%destroy()
    allocate( this%bc(n_bounds) )
    this%n_bounds = n_bounds
  end subroutine create_bc_holder_t

end module boundary_info_type
module wall_info_type
  use set_precision, only : dp
  use set_constants, only : zero
  use face_info_type, only : face_info_t
  implicit none
  private
  public :: wall_info_t
  ! collect all wall info here prior to wall distance search
  type wall_info_t
    integer :: n_dim, n_faces
    type(face_info_t), dimension(:), allocatable :: faces
  contains
    private
    procedure, public :: create  => create_wall_info_t
    procedure, public :: destroy => destroy_wall_info_t
  end type wall_info_t

contains

  pure elemental subroutine destroy_wall_info_t( this )
    class(wall_info_t), intent(inout) :: this
    if ( allocated(this%faces) ) then
      call this%faces%destroy()
      deallocate( this%faces )
    end if
    this%n_faces = 0
    this%n_dim   = 0
  end subroutine destroy_wall_info_t

  pure subroutine create_wall_info_t( this, n_dim, n_faces )
    class(wall_info_t), intent(inout) :: this
    integer,            intent(in)    :: n_dim, n_faces

    call this%destroy()
    this%n_dim   = n_dim
    this%n_faces = n_faces
    allocate( this%faces(n_faces) )
  end subroutine create_wall_info_t

  pure subroutine add_faces( this, block_id, fidx, face_label, lo, hi, skip, stride, node_coords, w_lo, w_hi )
    type(wall_info_t), intent(inout) :: this
    integer,           intent(in)    :: block_id, fidx, face_label
    integer, dimension(3), intent(in) :: lo, hi, skip, stride
    real(dp), dimension(3,lo(1):hi(1),lo(2):hi(2),lo(3):hi(3)), intent(in) :: node_coords
    integer, dimension(3), intent(in) :: w_lo, w_hi

    integer, dimension(3) :: s, o, l,h
    integer :: i,j,k, cnt

    ! get cell nodes from cell adjacent to face
    
    s = stride
    o = 0
    ! do something with face_label to get the right indices

    cnt = fidx
    do k = w_lo(3),w_hi(3),stride(3)
      do j = w_lo(2),w_hi(2),stride(2)
        do i = w_lo(1),w_hi(1),stride(1)
          cnt = cnt + 1
          call this%faces(cnt)%create( this%n_dim, block_id, face_label, node_coords(:,l(1):h(1):s(1),l(2):h(2):s(2),l(3):h(3):s(3)) )
        end do
      end do
    end do
  end subroutine add_faces

  pure subroutine get_face_coords_from_cell( shp, skip, stride, dir, coords_in, coords_out, status )
    integer, dimension(2),                              intent(inout) :: shp
    integer, dimension(3),                              intent(in)    :: skip
    integer, dimension(3),                              intent(in)    :: stride
    integer,                                            intent(in)    :: dir
    real(dp), dimension( 3, skip(1),skip(2),skip(3) ),  intent(in)    :: coords_in
    real(dp), dimension(shp(1),shp(2),3), optional,     intent(out)   :: coords_out
    integer,                              optional,     intent(out)   :: status
    integer, dimension(3) :: lo, hi

    if ( present(status) ) status = 0
    lo = 1
    hi = 1 + skip

    select case (dir)
      case(-1)
        hi(1) = lo(1)
        shp   = [skip(2),skip(3)] + 1
      case( 1)
        lo(1) = hi(1)
        shp   = [skip(2),skip(3)] + 1
      case(-2)
        hi(2) = lo(2)
        shp   = [skip(1),skip(3)] + 1
      case( 2)
        lo(2) = hi(2)
        shp   = [skip(1),skip(3)] + 1
      case(-3)
        hi(3) = lo(3)
        shp   = [skip(1),skip(2)] + 1
      case( 3)
        lo(3) = hi(3)
        shp   = [skip(1),skip(2)] + 1
      case default
        if ( present(status) ) status = -1
        return
    end select

    if ( present(coords_out) ) then
      if ( any( shape(coords_out) /= [shp(1),shp(2),3] ) ) then
        if ( present(status) ) status = -2
        return
      else
        coords_out = reshape( pack( coords_in(:,lo(1):hi(1):stride(1),           &
                                                lo(2):hi(2):stride(2),           &
                                                lo(3):hi(3):stride(3) ),.true.), &
                                                [shp(1),shp(2),3],     &
                                                order=[3,1,2] )
      end if
    end if

  end subroutine get_face_coords_from_cell

  ! pure subroutine get_candidate_faces(gblock,bnd_num,xyz_point,node_idx,n_faces,cell_idxs,min_dist)
  !   use set_constants, only : large
  !   use index_conversion, only : node_cell_nbors
  !   class(grid_block),        intent(in)  :: gblock
  !   integer,                  intent(in)  :: bnd_num
  !   real(dp), dimension(3),   intent(in)  :: xyz_point
  !   integer,  dimension(3),   intent(out) :: node_idx
  !   integer,                  intent(out) :: n_faces
  !   integer,  dimension(3,8), intent(out) :: cell_idxs
  !   real(dp),                 intent(out) :: min_dist
  !   integer, dimension(3) :: lo, hi, stride, offset, idx, tmp
  !   integer :: n_dim
  !   integer :: i,j,k,n
  !   real(dp), dimension(3) :: coord
  !   real(dp) :: dist

  !   n_dim = gblock%n_dim

  !   ! loop over all face nodes on face specified by bnd_num, and find closest node
  !   lo    = 1
  !   hi    =  gblock%n_nodes
  !   stride = 1; stride(1:n_dim) = gblock%n_skip(1:n_dim)
  !   offset = 0; offset(1:n_dim) = 1
  !   select case(bnd_num)
  !   case(-1) ! xi_min  
  !     hi(1)     = 1
  !     stride(1) = 1
  !     offset(1) = 0
  !   case(1) ! xi_max
  !     lo(1)     = hi(1)
  !     stride(1) = 1
  !     offset(1) = 0
  !   case(-2) ! eta_min
  !     hi(2)     = 1
  !     stride(2) = 1
  !     offset(2) = 0
  !   case(2) ! eta_max
  !     lo(2)     = hi(2)
  !     stride(2) = 1
  !     offset(2) = 0
  !   case(-3) ! zeta_min
  !     hi(3)     = 1
  !     stride(3) = 1
  !     offset(3) = 0
  !   case(3) ! zeta_max
  !     lo(3)     = hi(3)
  !     stride(3) = 1
  !     offset(3) = 0
  !   end select
  !   min_dist = large
  !   node_idx  = 1
  !   do k = lo(3),hi(3),stride(3)
  !     do j = lo(2),hi(2),stride(2)
  !       do i = lo(1),hi(1),stride(1)
  !         coord = gblock%node_coords(:,i,j,k)
  !         dist  = norm2(xyz_point-coord)
  !         if ( dist < min_dist ) then
  !           min_dist = dist
  !           node_idx = [i,j,k]
  !         end if
  !       end do
  !     end do
  !   end do

  !   ! change to cell indexing
  !   lo    = 1
  !   hi    = 1
  !   hi(1:n_dim) = (gblock%n_nodes(1:n_dim)-1)/gblock%n_skip(1:n_dim)
  !   idx   = 1
  !   idx(1:n_dim) = (node_idx(1:n_dim) - 1)/gblock%n_skip(1:n_dim) + 1
  !   call node_cell_nbors( 3, idx, lo, hi, cell_idxs, n_faces )
  !   lo = 1
  ! end subroutine get_candidate_faces


  ! pure subroutine get_min_distance(gblock,bnd_num,xyz_point,min_dist,clip,max_iter,xyz_eval,out_idx)
  !   use set_constants,            only : zero, one
  !   use interpolant_derived_type, only : interpolant_w_3D_data_t
  !   class(grid_block),        intent(in)  :: gblock
  !   integer,                  intent(in)  :: bnd_num
  !   real(dp), dimension(3),   intent(in)  :: xyz_point
  !   real(dp),                 intent(out) :: min_dist
  !   logical,                optional, intent(in)   :: clip
  !   integer,                optional, intent(in)   :: max_iter
  !   real(dp), dimension(3), optional, intent(out)  :: xyz_eval
  !   integer,  dimension(3), optional, intent(out)  :: out_idx
    

  !   integer                  :: n_faces
  !   integer,  dimension(3)   :: node_idx
  !   integer,  dimension(3,8) :: cell_idxs
  !   integer, dimension(2) :: shp
  !   real(dp), dimension(:,:,:), allocatable :: face_nodes
  !   real(dp), dimension(3) :: xyz_out
  !   real(dp), dimension(2) :: uv
  !   real(dp) :: dist, dist_est, dist_tmp
  !   integer :: i, dir, status, face_num
  !   type(interpolant_w_3D_data_t) :: interp
  !   real(dp), dimension(3) :: xyz00, xyz10, xyz01, xyz11
  !   real(dp), parameter :: h = 0.5_dp

  !   call gblock%get_candidate_faces(bnd_num,xyz_point,node_idx,n_faces,cell_idxs,min_dist)
  !   if (present(xyz_eval)) xyz_out = gblock%node_coords(:,node_idx(1),node_idx(2),node_idx(3))
  !   dir = bnd_num
  !   face_num = 1
  !   do i = 1,n_faces
  !     call gblock%get_fg_face_nodes(cell_idxs(:,i),shp,dir)
  !     allocate( face_nodes(shp(1),shp(2),3) )
  !     call gblock%get_fg_face_nodes(cell_idxs(:,i),shp,dir,face_nodes=face_nodes)
  !     call interp%create( pack(face_nodes(:,:,1),.true.), &
  !                         pack(face_nodes(:,:,2),.true.), &
  !                         pack(face_nodes(:,:,3),.true.), shape(face_nodes(:,:,1)) )
  !     uv = zero
  !     call interp%min_distance(xyz_point,uv,dist,xyz_eval=xyz_eval,max_iter=max_iter,clip=clip,status=status)
  !     call interp%destroy()
  !     deallocate( face_nodes )
  !     ! out of bounds in parametric space
  !     ! if (any(abs(uv) > one) ) continue
  !     if ( dist < min_dist ) then
  !       min_dist = dist
  !       face_num = i
  !       if (present(xyz_eval)) xyz_out = xyz_eval
  !     end if
  !   end do
  !   if (present(xyz_eval)) xyz_eval = xyz_out
  !   if (present(out_idx))  out_idx  = cell_idxs(:,face_num)
  ! end subroutine get_min_distance

end module wall_info_type
! #endif

module grid_derived_type
  use set_precision,           only : dp
  use quadrature_derived_type, only : quad_t, quad_ptr_3D
  use vector_derived_type,     only : vec_t, vec_ptr_3D
  use pointers,                only : array_ptr_3D_real, array_ptr_4D_real
  use interpolant_derived_type, only : interpolant_t
  implicit none
  private
  public :: derived_grid_vars
  public :: grid_block
  public :: grid_type
  public :: deallocate_grid
  public :: allocate_grid_block, deallocate_grid_block
  public :: allocate_derived_grid, deallocate_derived_grid

  public :: pack_cell_node_coords

  type derived_grid_vars
    real(dp),     allocatable, dimension(:,:,:,:) :: cell_c
    real(dp),     allocatable, dimension(:,:,:)   :: volume
    real(dp),     allocatable, dimension(:,:,:)   :: xi_area
    real(dp),     allocatable, dimension(:,:,:)   :: eta_area
    real(dp),     allocatable, dimension(:,:,:)   :: zeta_area
    real(dp),     allocatable, dimension(:,:,:)   :: wall_distance
    type(quad_t), allocatable, dimension(:,:,:)   :: quad
    type(quad_t), allocatable, dimension(:,:,:)   :: xi_face_quad
    type(quad_t), allocatable, dimension(:,:,:)   :: eta_face_quad
    type(quad_t), allocatable, dimension(:,:,:)   :: zeta_face_quad
    real(dp),     allocatable, dimension(:,:,:,:) :: xi_n
    real(dp),     allocatable, dimension(:,:,:,:) :: eta_n
    real(dp),     allocatable, dimension(:,:,:,:) :: zeta_n
    type(quad_ptr_3D),         dimension(3)       :: face_quads
    type(array_ptr_4D_real),   dimension(3)       :: normals

    real(dp),     allocatable, dimension(:,:,:,:) :: cell_c_curv
    real(dp),     allocatable, dimension(:,:,:)   :: volume_curv
    real(dp),     allocatable, dimension(:,:,:)   :: xi_area_curv
    real(dp),     allocatable, dimension(:,:,:)   :: eta_area_curv
    real(dp),     allocatable, dimension(:,:,:)   :: zeta_area_curv
    real(dp),     allocatable, dimension(:,:,:)   :: wall_distance_curv
    type(quad_t), allocatable, dimension(:,:,:)   :: quad_curv
    type(quad_t), allocatable, dimension(:,:,:)   :: xi_face_quad_curv
    type(quad_t), allocatable, dimension(:,:,:)   :: eta_face_quad_curv
    type(quad_t), allocatable, dimension(:,:,:)   :: zeta_face_quad_curv
    type(vec_t),  allocatable, dimension(:,:,:)   :: xi_nv
    type(vec_t),  allocatable, dimension(:,:,:)   :: eta_nv
    type(vec_t),  allocatable, dimension(:,:,:)   :: zeta_nv
    type(quad_ptr_3D),         dimension(3)       :: face_quads_curv
    type(vec_ptr_3D),          dimension(3)       :: normals_curv

    integer, dimension(:), pointer :: n_cells, n_ghost
    integer,               pointer :: n_dim
    type(interpolant_t) :: interp
  contains
    private
    procedure, public, pass :: setup   =>   allocate_derived_grid
    procedure, public, pass :: destroy => deallocate_derived_grid
  end type derived_grid_vars

  ! type :: grid_sz_info
  !   integer  :: n_dim
  !   integer  :: n_blocks
  !   integer  :: total_interior_cells
  ! end type grid_sz_info

  ! type :: gblock_sz_info
  !   integer  :: n_dim
  !   integer  :: total_interior_cells
  !   integer, dimension(3) :: n_nodes
  !   integer, dimension(3) :: n_cells
  !   integer, dimension(3) :: n_ghost
  !   integer, dimension(3) :: n_skip
  !   integer, dimension(3) :: lo_cg, hi_cg
  !   integer, dimension(3) :: lo_fg, hi_fg
  ! end type gblock_sz_info

  type :: grid_block
    integer, dimension(3) :: n_nodes
    integer, dimension(3) :: n_cells
    integer, dimension(3) :: n_ghost
    integer, dimension(3) :: n_skip
    integer  :: block_id
    integer  :: n_dim
    integer  :: total_cells
    real(dp) :: total_volume
    real(dp) :: total_volume_curv
    real(dp), allocatable, dimension(:,:,:,:) ::  node_coords
    type(derived_grid_vars) :: grid_vars
  contains
    private
    procedure, public, pass :: setup     => allocate_grid_block
    procedure, public, pass :: set_nodes => set_grid_block_nodes
    procedure, public, pass :: get_cell_nodes
    procedure, public, pass :: get_fg_volume_nodes, get_fg_face_nodes, get_wall_distance_vec
    procedure, public, pass :: get_candidate_faces, get_min_distance
    procedure, public, pass :: destroy   => deallocate_grid_block
  end type grid_block

  type grid_type
    integer  :: n_dim
    integer  :: n_blocks
    integer  :: total_int_cells
    real(dp) :: total_int_volume
    type(grid_block), allocatable, dimension(:) :: gblock
  contains
    private
    procedure, public, pass   :: setup => init_grid_type
    procedure, public, pass :: destroy => deallocate_grid
  end type grid_type

contains

  pure function pack_cell_node_coords( idx, bnd_min, bnd_max, n_skip,          &
                                       coords_in ) result(coords_out)
    integer, dimension(3),                            intent(in)  :: idx, bnd_min, bnd_max, n_skip
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ), intent(in)  :: coords_in
    real(dp), dimension(3,product(n_skip+1))                     :: coords_out
    integer :: i,j,k,cnt
    cnt = 0
    do k = idx(3),idx(3)+n_skip(3)
      do j = idx(2),idx(2)+n_skip(2)
        do i = idx(1),idx(1)+n_skip(1)
          cnt = cnt + 1
          coords_out(:,cnt) = coords_in(:,i,j,k)
        end do
      end do
    end do
  end function pack_cell_node_coords

  pure subroutine get_fg_volume_nodes(gblock,idx,volume_nodes)
    class(grid_block),     intent(in) :: gblock
    integer, dimension(3), intent(in) :: idx
    real(dp), dimension(gblock%n_skip(1)+1,gblock%n_skip(2)+1,gblock%n_skip(3)+1,3), intent(out) :: volume_nodes
    integer, dimension(4) :: tmp
    integer, dimension(3) :: bnd_min, bnd_max, sz, skip, stride, c_idx
    tmp = lbound(gblock%node_coords)
    bnd_min = tmp(2:4)
    tmp = ubound(gblock%node_coords)
    bnd_max = tmp(2:4)

    sz     = gblock%n_skip+1
    skip   = gblock%n_skip
    stride = 1
    c_idx  = (idx - 1)*skip + 1
    volume_nodes = get_cell_node_coords_vol( c_idx, sz, skip, stride, bnd_min, bnd_max, gblock%node_coords )
  end subroutine get_fg_volume_nodes

  pure subroutine get_fg_face_nodes(gblock,idx,shp,dir,face_nodes)
    class(grid_block),     intent(in)    :: gblock
    integer, dimension(3), intent(in)    :: idx
    integer, dimension(2), intent(inout) :: shp
    integer,               intent(in)    :: dir
    real(dp), dimension(shp(1),shp(2),3), optional, intent(out) :: face_nodes
    integer, dimension(4) :: tmp
    integer, dimension(3) :: bnd_min, bnd_max, sz, skip, stride, c_idx
    integer, dimension(2) :: shp2
    tmp = lbound(gblock%node_coords)
    bnd_min = tmp(2:4)
    tmp = ubound(gblock%node_coords)
    bnd_max = tmp(2:4)

    sz     = gblock%n_skip+1
    skip   = gblock%n_skip
    stride = 1
    c_idx  = (idx - 1)*skip + 1
    call get_cell_node_coords_face( c_idx, shp, skip, stride, dir, bnd_min, bnd_max, gblock%node_coords, coords_out=face_nodes )
  end subroutine get_fg_face_nodes

  pure function get_cell_node_coords_vol( idx, shp, skip, stride, bnd_min, bnd_max, coords_in )   &
                                                             result(coords_out)
    integer, dimension(3),                            intent(in)  :: idx
    integer, dimension(3),                            intent(in)  :: shp
    integer, dimension(3),                            intent(in)  :: skip
    integer, dimension(3),                            intent(in)  :: stride
    integer, dimension(3),                            intent(in)  :: bnd_min
    integer, dimension(3),                            intent(in)  :: bnd_max
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ), intent(in)  :: coords_in
    real(dp), dimension(shp(1),shp(2),shp(3),3)   :: coords_out
    real(dp), dimension(shp(1),shp(2),shp(3),3)   :: coords_out_cmp
    integer, dimension(3) :: lo, hi
    integer :: i,j,k,ii,jj,kk
    kk = 0
    do k = idx(3),idx(3)+skip(3),stride(3)
      kk = kk + 1
      jj = 0
      do j = idx(2),idx(2)+skip(2),stride(2)
        jj = jj + 1
        ii = 0
        do i = idx(1),idx(1)+skip(1),stride(1)
          ii = ii + 1
          coords_out_cmp(ii,jj,kk,1) = coords_in(1,i,j,k)
          coords_out_cmp(ii,jj,kk,2) = coords_in(2,i,j,k)
          coords_out_cmp(ii,jj,kk,3) = coords_in(3,i,j,k)
        end do
      end do
    end do

    lo = idx
    hi = idx + skip

    coords_out = reshape( coords_in(:,lo(1):hi(1):stride(1),           &
                                      lo(2):hi(2):stride(2),           &
                                      lo(3):hi(3):stride(3) ),         &
                                      shape(coords_out),     &
                                      order=[4,1,2,3] )

                                      
    kk = 1
    if ( any( abs( coords_out_cmp-coords_out )>epsilon(1.0_dp) ) ) then
      kk = 1
    end if
  end function get_cell_node_coords_vol

  pure subroutine get_cell_node_coords_face( idx, shp, skip, stride, dir, bnd_min, bnd_max, coords_in, coords_out, status )
    integer, dimension(3),                            intent(in)  :: idx
    integer, dimension(2),                            intent(inout) :: shp
    integer, dimension(3),                            intent(in)  :: skip
    integer, dimension(3),                            intent(in)  :: stride
    integer,                                          intent(in)  :: dir
    integer, dimension(3),                            intent(in)  :: bnd_min
    integer, dimension(3),                            intent(in)  :: bnd_max
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ),  intent(in)  :: coords_in
    real(dp), dimension(shp(1),shp(2),3), optional,   intent(out) :: coords_out
    integer,                              optional,   intent(out) :: status
    integer, dimension(3) :: lo, hi

    if ( present(status) ) status = 0
    lo = idx
    hi = idx + skip

    select case (dir)
      case(-1)
        hi(1) = lo(1)
        shp   = [skip(2),skip(3)] + 1
      case( 1)
        lo(1) = hi(1)
        shp   = [skip(2),skip(3)] + 1
      case(-2)
        hi(2) = lo(2)
        shp   = [skip(1),skip(3)] + 1
      case( 2)
        lo(2) = hi(2)
        shp   = [skip(1),skip(3)] + 1
      case(-3)
        hi(3) = lo(3)
        shp   = [skip(1),skip(2)] + 1
      case( 3)
        lo(3) = hi(3)
        shp   = [skip(1),skip(2)] + 1
      case default
        if ( present(status) ) status = -1
        return
    end select

    if ( present(coords_out) ) then
      if ( any( shape(coords_out) /= [shp(1),shp(2),3] ) ) then
        if ( present(status) ) status = -2
        return
      else
        coords_out = reshape( pack( coords_in(:,lo(1):hi(1):stride(1),           &
                                                lo(2):hi(2):stride(2),           &
                                                lo(3):hi(3):stride(3) ),.true.), &
                                                [shp(1),shp(2),3],     &
                                                order=[3,1,2] )
      end if
    end if

  end subroutine get_cell_node_coords_face

  pure function get_wall_distance_vec( gblock, idx, dir, xyz_point, n_iter ) result(out_vec)
    use set_constants, only : one, two
    use interpolant_derived_type, only : interpolant_w_3D_data_t
    class(grid_block),                                intent(in)  :: gblock
    integer, dimension(3),                            intent(in)  :: idx
    integer,                                          intent(in)  :: dir
    real(dp), dimension(3),                           intent(in)  :: xyz_point
    integer,                                          intent(in)  :: n_iter
    real(dp), dimension(3,n_iter+3) :: out_vec
    real(dp), dimension(3) :: xyz_eval
    integer, dimension(2) :: shp
    real(dp), dimension(:,:,:), allocatable :: face_nodes
    real(dp), dimension(2) :: uv
    real(dp) :: dist, dist_cmp
    integer :: i, status
    type(interpolant_w_3D_data_t) :: interp

    call gblock%get_fg_face_nodes(idx,shp,dir)
    allocate( face_nodes(shp(1),shp(2),3) )
    call gblock%get_fg_face_nodes(idx,shp,dir,face_nodes=face_nodes)
    
    out_vec(:,1) = xyz_point
    call interp%create( pack(face_nodes(:,:,1),.true.), &
                        pack(face_nodes(:,:,2),.true.), &
                        pack(face_nodes(:,:,3),.true.), shape(face_nodes(:,:,1)) )
    uv = [-one,-one]
    out_vec(:,2) = interp%pt_interp(uv)
    do i = 1, n_iter
      ! uv = [-one,-one] + two*real((i-1),dp)/real(n_iter,dp)
      call interp%min_distance(xyz_point,uv,dist_cmp,xyz_eval=xyz_eval,max_iter=1,status=status)
      out_vec(:,i+2) = xyz_eval
    end do
    out_vec(:,n_iter+3) = xyz_point

    call interp%destroy()
    deallocate( face_nodes )
  end function get_wall_distance_vec

  pure subroutine get_candidate_faces(gblock,bnd_num,xyz_point,node_idx,n_faces,cell_idxs,min_dist)
    use set_constants, only : large
    use index_conversion, only : node_cell_nbors
    class(grid_block),        intent(in)  :: gblock
    integer,                  intent(in)  :: bnd_num
    real(dp), dimension(3),   intent(in)  :: xyz_point
    integer,  dimension(3),   intent(out) :: node_idx
    integer,                  intent(out) :: n_faces
    integer,  dimension(3,8), intent(out) :: cell_idxs
    real(dp),                 intent(out) :: min_dist
    integer, dimension(3) :: lo, hi, stride, offset, idx, tmp
    integer :: n_dim
    integer :: i,j,k,n
    real(dp), dimension(3) :: coord
    real(dp) :: dist

    n_dim = gblock%n_dim

    ! loop over all face nodes on face specified by bnd_num, and find closest node
    lo    = 1
    hi    =  gblock%n_nodes
    stride = 1; stride(1:n_dim) = gblock%n_skip(1:n_dim)
    offset = 0; offset(1:n_dim) = 1
    select case(bnd_num)
    case(-1) ! xi_min  
      hi(1)     = 1
      stride(1) = 1
      offset(1) = 0
    case(1) ! xi_max
      lo(1)     = hi(1)
      stride(1) = 1
      offset(1) = 0
    case(-2) ! eta_min
      hi(2)     = 1
      stride(2) = 1
      offset(2) = 0
    case(2) ! eta_max
      lo(2)     = hi(2)
      stride(2) = 1
      offset(2) = 0
    case(-3) ! zeta_min
      hi(3)     = 1
      stride(3) = 1
      offset(3) = 0
    case(3) ! zeta_max
      lo(3)     = hi(3)
      stride(3) = 1
      offset(3) = 0
    end select
    min_dist = large
    node_idx  = 1
    do k = lo(3),hi(3),stride(3)
      do j = lo(2),hi(2),stride(2)
        do i = lo(1),hi(1),stride(1)
          coord = gblock%node_coords(:,i,j,k)
          dist  = norm2(xyz_point-coord)
          if ( dist < min_dist ) then
            min_dist = dist
            node_idx = [i,j,k]
          end if
        end do
      end do
    end do

    ! change to cell indexing
    lo    = 1
    hi    = 1
    hi(1:n_dim) = (gblock%n_nodes(1:n_dim)-1)/gblock%n_skip(1:n_dim)
    idx   = 1
    idx(1:n_dim) = (node_idx(1:n_dim) - 1)/gblock%n_skip(1:n_dim) + 1
    call node_cell_nbors( 3, idx, lo, hi, cell_idxs, n_faces )
    lo = 1
    ! cell_idxs = 1
    ! ! now get the indices of the cells adjacent to this node:
    ! n_faces = 0
    ! do k = -offset(3),0
    !   do j = -offset(2),0
    !     do i = -offset(1),0
    !       tmp = [i,j,k]
    !       idx = 1
    !       do n = 1,gblock%n_dim
    !         idx(n) = (node_idx(n)-1)/gblock%n_skip(n) + 1  + tmp(n)
    !       end do
    !       if ( in_bound(3,idx,lo,hi) ) then
    !         n_faces = n_faces + 1
    !         cell_idxs(:,n_faces) = idx
    !       end if
    !     end do
    !   end do
    ! end do
  end subroutine get_candidate_faces


  pure subroutine get_min_distance(gblock,bnd_num,xyz_point,min_dist,clip,max_iter,xyz_eval,out_idx)
    use set_constants,            only : zero, one
    use interpolant_derived_type, only : interpolant_w_3D_data_t
    class(grid_block),        intent(in)  :: gblock
    integer,                  intent(in)  :: bnd_num
    real(dp), dimension(3),   intent(in)  :: xyz_point
    real(dp),                 intent(out) :: min_dist
    logical,                optional, intent(in)   :: clip
    integer,                optional, intent(in)   :: max_iter
    real(dp), dimension(3), optional, intent(out)  :: xyz_eval
    integer,  dimension(3), optional, intent(out)  :: out_idx
    

    integer                  :: n_faces
    integer,  dimension(3)   :: node_idx
    integer,  dimension(3,8) :: cell_idxs
    integer, dimension(2) :: shp
    real(dp), dimension(:,:,:), allocatable :: face_nodes
    real(dp), dimension(3) :: xyz_out
    real(dp), dimension(2) :: uv
    real(dp) :: dist, dist_est, dist_tmp
    integer :: i, dir, status, face_num
    type(interpolant_w_3D_data_t) :: interp
    real(dp), dimension(3) :: xyz00, xyz10, xyz01, xyz11
    real(dp), parameter :: h = 0.5_dp

    call gblock%get_candidate_faces(bnd_num,xyz_point,node_idx,n_faces,cell_idxs,min_dist)
    if (present(xyz_eval)) xyz_out = gblock%node_coords(:,node_idx(1),node_idx(2),node_idx(3))
    dir = bnd_num
    face_num = 1
    do i = 1,n_faces
      call gblock%get_fg_face_nodes(cell_idxs(:,i),shp,dir)
      allocate( face_nodes(shp(1),shp(2),3) )
      call gblock%get_fg_face_nodes(cell_idxs(:,i),shp,dir,face_nodes=face_nodes)
      call interp%create( pack(face_nodes(:,:,1),.true.), &
                          pack(face_nodes(:,:,2),.true.), &
                          pack(face_nodes(:,:,3),.true.), shape(face_nodes(:,:,1)) )
      uv = zero
      ! call interp%nearest_pt(2,xyz_point,uv,dist=dist, dist_est=dist_est, xyz_out=xyz_out)
      ! call interp%nearest_pt(2,xyz_point,uv,dist=dist)

      ! if ( dist < min_dist ) then
      !   min_dist = dist
      !   face_num = i
      ! end if
      call interp%min_distance(xyz_point,uv,dist,xyz_eval=xyz_eval,max_iter=max_iter,clip=clip,status=status)
      call interp%destroy()
      deallocate( face_nodes )
      ! out of bounds in parametric space
      ! if (any(abs(uv) > one) ) continue
      if ( dist < min_dist ) then
        min_dist = dist
        face_num = i
        if (present(xyz_eval)) xyz_out = xyz_eval
      end if
    end do
    if (present(xyz_eval)) xyz_eval = xyz_out
    if (present(out_idx))  out_idx  = cell_idxs(:,face_num)
  end subroutine get_min_distance

  ! subroutine get_wall_distance_linear(grid)
  !   class(grid_type), intent(inout)  :: grid
  ! end subroutine get_wall_distance_linear

    
  ! subroutine coarsen_grid(n_skip,grid,grid_coarse)
  !   integer, dimension(3), intent(in)  :: n_skip
  !   type(grid_type),       intent(in)  :: grid
  !   type(grid_type),       intent(out) :: grid_coarse
  !   integer :: n, n_blocks, n_dim
  !   integer, dimension(3) :: n_nodes, n_ghost, lo1, hi1, lo2, hi2
  !   n_blocks = grid%n_blocks
  !   n_nodes  = grid
  !   call grid_coarse%setup( n_blocks )
  !   do n = 1,n_blocks
  !     n_nodes = grid%gblock(n)%n_nodes
  !     n_nodes = grid%gblock(n)%n_ghost

  !     call grid_coarse%gblock(n)%setup(n)
  ! end subroutine coarsen_grid

  ! pure subroutine coarsen_node_coords( lo1, hi1, lo2, hi2, n_skip, coords_in, coords_out )
  !   integer, dimension(3), intent(in) :: lo1, hi1, lo2, hi2, n_skip
  !   real(dp), dimension(3,lo1(1):hi1(1),lo1(2):hi1(2),lo1(3):hi1(3)), intent(in)  :: coords_in
  !   real(dp), dimension(3,lo2(1):hi2(1),lo2(2):hi2(2),lo2(3):hi2(3)), intent(out) :: coords_out
  !   coords_out = coords_in(:,lo1(1):hi1(1):n_skip(1),lo1(2):hi1(2):n_skip(2),lo1(3):hi1(3):n_skip(3))
  ! end subroutine coarsen_node_coords

  pure function get_cell_nodes(gblock,start_idx,shp,skip,stride) result(coords_out)
    class(grid_block),               intent(in) :: gblock
    integer, dimension(3),           intent(in) :: start_idx
    integer, dimension(3),           intent(in) :: shp, skip, stride
    real(dp), dimension(shp(1),shp(2),shp(3),3) :: coords_out
    integer, dimension(4) :: bnd_tmp
    integer, dimension(3) :: bnd_min, bnd_max
    bnd_tmp = lbound(gblock%node_coords)
    bnd_min = bnd_tmp(2:4)
    bnd_tmp = ubound(gblock%node_coords)
    bnd_max = bnd_tmp(2:4)
    coords_out = cell_node_coords( start_idx, shp, skip, stride, bnd_min, bnd_max,        &
                                   gblock%node_coords )
  end function get_cell_nodes

  pure function cell_node_coords( idx, shp, skip, stride, bnd_min, bnd_max, coords_in )   &
                                                             result(coords_out)
    integer, dimension(3),                            intent(in) :: idx
    integer, dimension(3),                            intent(in) :: shp, skip, stride
    integer, dimension(3),                            intent(in) :: bnd_min
    integer, dimension(3),                            intent(in) :: bnd_max
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ),  intent(in) :: coords_in
    real(dp), dimension(shp(1),shp(2),shp(3),3)                  :: coords_out
    integer :: i,j,k,ii,jj,kk
    kk = 0
    do k = idx(3),idx(3)+skip(3),stride(3)
      kk = kk + 1
      jj = 0
      do j = idx(2),idx(2)+skip(2),stride(2)
        jj = jj + 1
        ii = 0
        do i = idx(1),idx(1)+skip(1),stride(1)
          ii = ii + 1
          coords_out(ii,jj,kk,1) = coords_in(1,i,j,k)
          coords_out(ii,jj,kk,2) = coords_in(2,i,j,k)
          coords_out(ii,jj,kk,3) = coords_in(3,i,j,k)
        end do
      end do
    end do
  end function cell_node_coords

  pure subroutine init_grid_type( this, n_dim, n_blocks )
    use set_constants, only : zero
    class(grid_type), intent(inout) :: this
    integer, intent(in) :: n_dim, n_blocks

    this%n_dim    = n_dim
    this%n_blocks = n_blocks
    this%total_int_cells = 0
    this%total_int_volume = zero
    allocate( this%gblock(n_blocks) )
  end subroutine init_grid_type

  pure subroutine allocate_grid_block( this, block_id, n_dim, n_nodes, n_ghost, n_skip )
    use set_constants, only : zero
    class(grid_block),     intent(inout) :: this
    integer,               intent(in)  :: block_id, n_dim
    integer, dimension(3), intent(in)  :: n_nodes, n_ghost
    integer, dimension(3), optional, intent(in) :: n_skip
    integer, dimension(3) :: lo, hi
    this%n_dim   = n_dim
    this%n_nodes = 1; this%n_nodes(1:n_dim) = n_nodes(1:n_dim)
    this%n_ghost = n_ghost
    this%n_cells = 1; this%n_cells(1:n_dim) = n_nodes(1:n_dim) - 1
    this%n_skip  = 0
    if ( present(n_skip) ) this%n_skip(1:n_dim) = n_skip(1:n_dim)
    lo = 1; lo(1:n_dim) = 1 - n_ghost(1:n_dim)
    hi = 1; hi(1:n_dim) = n_nodes(1:n_dim) + n_ghost(1:n_dim)
    allocate( this%node_coords( 3, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    this%node_coords = zero
    this%total_volume = zero
    this%total_cells  = product(this%n_cells)
  end subroutine allocate_grid_block

  pure subroutine set_grid_block_nodes( this, node_coords )
    use set_constants, only : zero
    class(grid_block),                         intent(inout) :: this
    real(dp), dimension( 3, this%n_nodes(1),                                   &
                            this%n_nodes(2),                                   &
                            this%n_nodes(3) ), intent(in) :: node_coords
    integer, dimension(3) :: lo, hi
    lo = 1
    hi = 1; hi(1:this%n_dim) = this%n_nodes(1:this%n_dim)
    this%node_coords( :, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) =             &
          node_coords(:, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) )
  end subroutine set_grid_block_nodes




  subroutine allocate_derived_grid( this, gblock )
    use set_constants,  only : zero
    use interpolant_derived_type, only : interpolant_t
    class(derived_grid_vars), target, intent(inout)   :: this
    class(grid_block),       target, intent(inout) :: gblock
    integer, dimension(3) :: lo, hi, n_skip
    this%n_cells => gblock%n_cells
    this%n_ghost => gblock%n_ghost
    this%n_dim   => gblock%n_dim
    lo = 1
    lo(1:this%n_dim) = 1 - this%n_ghost(1:this%n_dim)
    hi = 1
    hi(1:this%n_dim) = this%n_cells(1:this%n_dim) + this%n_ghost(1:this%n_dim)
    allocate( this%volume(    lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%cell_c( 3, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )

    allocate( this%volume_curv(    lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%cell_c_curv( 3, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )

    ! not including ghost cells
    ! allocate( this%quad(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    this%volume = zero
    this%cell_c = zero

    lo = 1
    hi = 1
    hi(1:this%n_dim) = this%n_cells(1:this%n_dim)
    allocate( this%quad(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%xi_area(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_area(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_area( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_face_quad(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_face_quad(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_face_quad( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_n(   3, lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_n(  3, lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_n( 3, lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    this%xi_area   = zero
    this%eta_area  = zero
    this%zeta_area = zero

    allocate( this%quad_curv(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%xi_area_curv(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_area_curv(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_area_curv( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_face_quad_curv(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_face_quad_curv(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_face_quad_curv( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_nv(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_nv(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_nv( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    this%xi_area_curv   = zero
    this%eta_area_curv  = zero
    this%zeta_area_curv = zero

    this%face_quads(1)%p => this%xi_face_quad
    this%face_quads(2)%p => this%eta_face_quad
    this%face_quads(3)%p => this%zeta_face_quad

    this%normals(1)%p         => this%xi_n
    this%normals(2)%p         => this%eta_n
    this%normals(3)%p         => this%zeta_n

    this%face_quads_curv(1)%p => this%xi_face_quad_curv
    this%face_quads_curv(2)%p => this%eta_face_quad_curv
    this%face_quads_curv(3)%p => this%zeta_face_quad_curv

    this%normals_curv(1)%p    => this%xi_nv
    this%normals_curv(2)%p    => this%eta_nv
    this%normals_curv(3)%p    => this%zeta_nv

    this%interp = interpolant_t(N=10)

    n_skip = 0; n_skip(1:this%n_dim) = 1
    ! call compute_quadrature_points( gblock, gblock, [1,1,1], 4 )
    call compute_quadrature_points( gblock, gblock, n_skip, 4 )

  end subroutine allocate_derived_grid

  subroutine compute_quadrature_points( gblock1, gblock, n_skip, quad_order )
    use set_precision,             only : dp
    use set_constants,             only : zero
    use project_inputs,            only : ng => n_ghost
    use index_conversion,          only : get_ghost_cell_indices,              &
                                          global2local_ghost
    use quadrature_derived_type,   only : quad_t
    use interpolant_derived_type,  only : interpolant_t
    use quadrature_derived_type,   only : create_quad_ref_1D,              &
                                          create_quad_ref_2D,              &
                                          create_quad_ref_3D,              &
                                          map_quad_ref_to_physical
    class(grid_block),      intent(in)    :: gblock1
    class(grid_block),      intent(inout) :: gblock
    integer, dimension(3),  intent(in)    :: n_skip
    integer,                intent(in)    :: quad_order
    type(quad_t), dimension(0:3) :: ref_quads
    type(quad_t) :: tmp_quad0, tmp_quad1, tmp_quad2
    real(dp), dimension(:,:,:,:), allocatable :: coords_tmp, coords_tmp_g
    ! real(dp), dimension(n_skip(1)+1,n_skip(2)+1,n_skip(3)+1,3) :: coords_tmp
    ! real(dp), dimension(2,2,2,3) :: coords_tmp_g
    integer,  dimension(:), allocatable :: ig_g, ig_i
    integer :: i, j, k
    integer :: status, szint, szgst
    integer, dimension(3) :: loc1, loc2, idx, skip, shp_c, shp_f, stride_c, stride_f
    type(interpolant_t) :: interp
    real(dp) :: volume1, volume2, diff

    ! n_dim = merge(2,3,twod)

    call create_quad_ref_1D(          1, ref_quads(0) )
    call create_quad_ref_1D( quad_order, ref_quads(1) )
    call create_quad_ref_2D( quad_order, ref_quads(2) )
    call create_quad_ref_3D( quad_order, ref_quads(3) )

    ! first the volume quads
    loc1 = 2
    loc1(gblock%n_dim+1:) = 0

    call tmp_quad0%create( ref_quads(gblock%n_dim-1)%n_quad )
    call tmp_quad1%create( ref_quads(gblock%n_dim-1)%n_quad )
    call tmp_quad2%create( ref_quads(gblock%n_dim-1)%n_quad )
    
    gblock%total_volume      = zero
    gblock%total_volume_curv = zero


    skip = 0;   skip(1:gblock%n_dim) = n_skip(1:gblock%n_dim)
    stride_c = 1
    stride_f = 1; stride_f(1:gblock%n_dim) = skip(1:gblock%n_dim)
    shp_c = (skip / stride_c) + 1
    shp_f = (skip / stride_f) + 1

    ! interior
    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc1,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim),            &
                                           gv%quad(i,j,k),                     &
                                           vol= gv%volume(i,j,k),              &
                                           centroid= gv%cell_c(:,i,j,k),       &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim),            &
                                           gv%quad_curv(i,j,k),                &
                                           vol= gv%volume_curv(i,j,k),         &
                                           centroid= gv%cell_c_curv(:,i,j,k),  &
                                           status=status )
            gblock%total_volume      = gblock%total_volume + gv%volume(i,j,k)
            gblock%total_volume_curv = gblock%total_volume_curv + gv%volume_curv(i,j,k)
          end associate
        end do
      end do
    end do

    ! ghost cells
    szint = product( gblock%n_cells )
    szgst = product( gblock%n_cells + 2*ng) - szint
    allocate( ig_g(szgst) )
    allocate( ig_i(szint) )
    call get_ghost_cell_indices( gblock%n_cells, ng, ig_g, ig_i )
    do i = 1,szgst
      idx = global2local_ghost( ig_g(i), gblock%n_cells, ng )
      coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
      associate( X1 => coords_tmp_g(:,:,:,1), &
                 X2 => coords_tmp_g(:,:,:,2), &
                 X3 => coords_tmp_g(:,:,:,3), &
                 ii => idx(1),                &
                 jj => idx(2),                &
                 kk => idx(3),                &
                 gv => gblock%grid_vars )
        call map_quad_ref_to_physical( X1, X2, X3, loc1,                     &
                                           gblock%grid_vars%interp,                    &
                                           ref_quads(gblock%n_dim),          &
                                           gv%quad(ii,jj,kk),                &
                                           vol= gv%volume(ii,jj,kk),         &
                                           centroid= gv%cell_c(:,ii,jj,kk),  &
                                           status=status )
        call map_quad_ref_to_physical( X1, X2, X3, loc1,                     &
                                           gblock%grid_vars%interp,                    &
                                           ref_quads(gblock%n_dim),          &
                                           gv%quad_curv(ii,jj,kk),                &
                                           vol= gv%volume_curv(ii,jj,kk),         &
                                           centroid= gv%cell_c_curv(:,ii,jj,kk),  &
                                           status=status )
      end associate
    end do
    deallocate( ig_g, ig_i )

    ! now the face quads

    ! return

    ! xi-faces
    loc1 = 2
    ! if ( gblock%n_dim /= 1)
    loc1(gblock%n_dim+1:) = 0
    loc1(1) = 0

    loc2 = 2
    ! if ( gblock%n_dim /= 1)
    loc2(gblock%n_dim+1:) = 0
    loc2(1) = 1

    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,1
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc1,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%xi_face_quad(i,j,k),             &
                                           normal=gv%xi_n(:,i,j,k),            &
                                           vol=gv%xi_area(i,j,k),              &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%xi_face_quad_curv(i,j,k),        &
                                           face_vec=   gv%xi_nv(i,j,k),        &
                                           vol= gv%xi_area_curv(i,j,k),        &
                                           status=status )
          end associate
        end do
        do i = 1,gblock%n_cells(1)
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc2,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%xi_face_quad( i+1,j,k),          &
                                           normal=gv%xi_n(:,i+1,j,k),          &
                                           vol=  gv%xi_area(i+1,j,k),          &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc2,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%xi_face_quad_curv(i+1,j,k),      &
                                           face_vec=   gv%xi_nv(i+1,j,k),      &
                                           vol= gv%xi_area_curv(i+1,j,k),      &
                                           status=status )
          end associate
        end do
      end do
    end do

    if ( gblock%n_dim == 1) then
      if (allocated(coords_tmp))   deallocate(coords_tmp)
      if (allocated(coords_tmp_g)) deallocate(coords_tmp_g)
      return
    end if

    ! eta-faces
    loc1 = 2
    loc1(gblock%n_dim+1:) = 0
    loc1(2) = 0

    loc2 = 2
    loc2(gblock%n_dim+1:) = 0
    loc2(2) = 1

    do k = 1,gblock%n_cells(3)
      do j = 1,1
        do i = 1,gblock%n_cells(1)
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc1,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%eta_face_quad( i,j,k),           &
                                           normal=gv%eta_n(:,i,j,k),           &
                                           vol=  gv%eta_area(i,j,k),           &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%eta_face_quad_curv(i,j,k),       &
                                           face_vec=   gv%eta_nv(i,j,k),       &
                                           vol= gv%eta_area_curv(i,j,k),       &
                                           status=status )
          end associate
        end do
      end do
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc2,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%eta_face_quad( i,j+1,k),         &
                                           normal=gv%eta_n(:,i,j+1,k),         &
                                           vol=  gv%eta_area(i,j+1,k),         &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc2,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%eta_face_quad_curv(i,j+1,k),     &
                                           face_vec=   gv%eta_nv(i,j+1,k),     &
                                           vol= gv%eta_area_curv(i,j+1,k),     &
                                           status=status )
          end associate
        end do
      end do
    end do

    if ( gblock%n_dim == 2) then
      if (allocated(coords_tmp))   deallocate(coords_tmp)
      if (allocated(coords_tmp_g)) deallocate(coords_tmp_g)
      return
    end if

    ! zeta-faces
    loc1 = 2
    loc1(3) = 0

    loc2 = 2
    loc2(3) = 1

    do k = 1,1
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc1,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%zeta_face_quad(  i,j,k),         &
                                           normal=gv%zeta_n( :,i,j,k),         &
                                           vol=   gv%zeta_area(i,j,k),         &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%zeta_face_quad_curv(i,j,k),      &
                                           face_vec=   gv%zeta_nv(i,j,k),      &
                                           vol= gv%zeta_area_curv(i,j,k),      &
                                           status=status )
          end associate
        end do
      end do
    end do
    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          coords_tmp = gblock1%get_cell_nodes([i,j,k],shp_c,skip,stride_c)
          coords_tmp_g = gblock%get_cell_nodes([i,j,k],shp_f,skip,stride_f)
          associate( X1  => coords_tmp(:,:,:,1), &
                     X2  => coords_tmp(:,:,:,2), &
                     X3  => coords_tmp(:,:,:,3), &
                     X1g => coords_tmp_g(:,:,:,1), &
                     X2g => coords_tmp_g(:,:,:,2), &
                     X3g => coords_tmp_g(:,:,:,3), &
                     gv  => gblock%grid_vars )
            call map_quad_ref_to_physical( X1g, X2g, X3g, loc2,                &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%zeta_face_quad( i,j,k+1),        &
                                           normal=gv%zeta_n(:,i,j,k+1),        &
                                           vol  =gv%zeta_area(i,j,k+1),        &
                                           status=status )
            call map_quad_ref_to_physical( X1, X2, X3, loc2,                   &
                                           gblock%grid_vars%interp,            &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%zeta_face_quad_curv(i,j,k+1),    &
                                           face_vec=     gv%zeta_nv(i,j,k+1),  &
                                           vol= gv%zeta_area_curv(i,j,k+1),    &
                                           status=status )
          end associate
        end do
      end do
    end do

    call ref_quads%destroy()
    if (allocated(coords_tmp))   deallocate(coords_tmp)
    if (allocated(coords_tmp_g)) deallocate(coords_tmp_g)

    ! min_dist = large
    ! do i = 1,n_bnds
    !   call grid%gblock(1)%get_min_distance(bnd_nums(i),pt,dist,max_iter=n_iter,xyz_eval=xyz_tmp,clip=.true.,out_idx=cell_idx)
    !   if ( dist < min_dist ) then
    !     min_dist = dist
    !     xyz_eval = xyz_tmp
    !   end if
    ! end do

  end subroutine compute_quadrature_points

  pure elemental subroutine deallocate_grid(this)
    use set_constants, only : zero
    class(grid_type), intent(inout) :: this
    if ( allocated(this%gblock) ) then
      call this%gblock%destroy()
      deallocate( this%gblock)
    end if
    this%n_dim    = 0
    this%n_blocks = 0
    this%total_int_cells = 0
    this%total_int_volume = zero
  end subroutine deallocate_grid

  pure elemental subroutine deallocate_grid_block( this )
    use set_constants, only : zero
    class(grid_block), intent(inout) :: this
    call this%grid_vars%destroy()
    if (allocated( this%node_coords ) ) deallocate( this%node_coords )
    this%total_volume      = zero
    this%total_volume_curv = zero
    this%total_cells  = 0
    this%n_dim        = 0
    this%n_nodes      = 0
    this%n_cells      = 0
  end subroutine deallocate_grid_block

  pure elemental subroutine deallocate_derived_grid( this )
    class(derived_grid_vars), intent(inout) :: this
    call this%interp%destroy()
    call this%face_quads(1)%destroy()
    call this%face_quads(2)%destroy()
    call this%face_quads(3)%destroy()
    call this%normals(1)%destroy()
    call this%normals(2)%destroy()
    call this%normals(3)%destroy()

    if ( allocated(this%cell_c)    ) deallocate( this%cell_c    )
    if ( allocated(this%volume)    ) deallocate( this%volume    )
    if ( allocated(this%xi_n)      ) deallocate( this%xi_n      )    
    if ( allocated(this%eta_n)     ) deallocate( this%eta_n     )
    if ( allocated(this%zeta_n)    ) deallocate( this%zeta_n    )
    if ( allocated(this%xi_area)   ) deallocate( this%xi_area   )
    if ( allocated(this%eta_area)  ) deallocate( this%eta_area  )
    if ( allocated(this%zeta_area) ) deallocate( this%zeta_area )
    if ( allocated(this%quad) ) then
      call this%quad%destroy()
      deallocate( this%quad )
    end if
    if ( allocated(this%xi_face_quad) ) then
      call this%xi_face_quad%destroy()
      deallocate( this%xi_face_quad )
    end if
    if ( allocated(this%eta_face_quad) ) then
      call this%eta_face_quad%destroy()
      deallocate( this%eta_face_quad )
    end if
    if ( allocated(this%zeta_face_quad) ) then
      call this%zeta_face_quad%destroy()
      deallocate( this%zeta_face_quad )
    end if

    call this%face_quads_curv(1)%destroy()
    call this%face_quads_curv(2)%destroy()
    call this%face_quads_curv(3)%destroy()

    call this%normals_curv(1)%destroy()
    call this%normals_curv(2)%destroy()
    call this%normals_curv(3)%destroy()

    if ( allocated(this%cell_c_curv)    ) deallocate( this%cell_c_curv    )
    if ( allocated(this%volume_curv)    ) deallocate( this%volume_curv    )
    if ( allocated(this%xi_area_curv)   ) deallocate( this%xi_area_curv   )
    if ( allocated(this%eta_area_curv)  ) deallocate( this%eta_area_curv  )
    if ( allocated(this%zeta_area_curv) ) deallocate( this%zeta_area_curv )
    if ( allocated(this%quad_curv) ) then
      call this%quad_curv%destroy()
      deallocate( this%quad_curv )
    end if
    if ( allocated(this%xi_face_quad_curv) ) then
      call this%xi_face_quad_curv%destroy()
      deallocate( this%xi_face_quad_curv )
    end if
    if ( allocated(this%eta_face_quad_curv) ) then
      call this%eta_face_quad_curv%destroy()
      deallocate( this%eta_face_quad_curv )
    end if
    if ( allocated(this%zeta_face_quad_curv) ) then
      call this%zeta_face_quad_curv%destroy()
      deallocate( this%zeta_face_quad_curv )
    end if
    if ( allocated(this%xi_nv) ) then
      call this%xi_nv%destroy()
      deallocate(this%xi_nv)
    end if
    if ( allocated(this%eta_nv) ) then
      call this%eta_nv%destroy()
      deallocate(this%eta_nv)
    end if
    if ( allocated(this%zeta_nv) ) then
      call this%zeta_nv%destroy()
      deallocate(this%zeta_nv)
    end if

  end subroutine deallocate_derived_grid


  ! pure subroutine add_faces_from_bc_to_wall_info_t( info, idx, bound, )
  !   use wall_info_type, only : wall_info_t
  ! end subroutine add_faces_from_bc

end module grid_derived_type

module grid_local
  use grid_derived_type, only : grid_type

  implicit none
  private
  public :: grid

  type(grid_type) :: grid

end module grid_local

module plot_3D_grid_io
  use set_precision, only : dp
  implicit none
  private
  public :: read_grid
contains

  subroutine read_grid( file_name, n_dim, n_ghost, n_skip, grid )
    use grid_derived_type,    only : grid_type
    use file_routines,        only : open_existing_file
    character(*),          intent(in)  :: file_name 
    integer,               intent(in)  :: n_dim
    integer, dimension(3), intent(in)  :: n_ghost, n_skip
    type(grid_type),       intent(out) :: grid
    integer :: data_start, n_blocks, dim, total_size
    logical :: query
    integer :: fid, status
    integer :: b
    integer, dimension(:,:), allocatable :: blk_sizes
    real(dp), dimension(:),  allocatable :: coord_data

    real(dp), dimension(:,:,:,:),  allocatable :: coords_tmp

    fid = open_existing_file( trim(file_name), append=.false.)

    query = .true.
    call read_plot3D(fid,query,data_start,n_blocks,dim,total_size)

    allocate( blk_sizes(3,n_blocks)   )
    allocate( coord_data(total_size ) )

    query = .false.
    call read_plot3D(fid,query,data_start,n_blocks,dim,total_size,blk_sizes=blk_sizes,coord_data=coord_data,status=status)

    close(fid)

    call grid%setup(n_dim,n_blocks)
    do b = 1,n_blocks
      call grid%gblock(b)%setup( b, n_dim, blk_sizes(:,b), n_ghost, n_skip=n_skip )
      allocate( coords_tmp(3,blk_sizes(1,b),blk_sizes(2,b),blk_sizes(3,b)) )
      call access_block_data( b, n_blocks, dim, blk_sizes, total_size, coord_data, coords_tmp )
      call grid%gblock(b)%set_nodes( coords_tmp )
      deallocate( coords_tmp )
    end do

    deallocate( blk_sizes, coord_data )

    grid%total_int_cells  = sum( grid%gblock%total_cells )
    grid%total_int_volume = sum( grid%gblock%total_volume )
  end subroutine read_grid

  pure subroutine access_block_data( b, n_blocks, dim, blk_sizes, total_size, coord_data, coords )
    use set_constants, only : zero
    integer,                         intent(in) :: b, n_blocks, dim
    integer,  dimension(3,n_blocks), intent(in) :: blk_sizes
    integer,                         intent(in) :: total_size
    real(dp), dimension(total_size), intent(in) :: coord_data
    real(dp), dimension(3,blk_sizes(1,b),blk_sizes(2,b),blk_sizes(3,b)), intent(out) :: coords
    integer :: n, i, j, k, cnt, lo

    cnt = 0
    do n = 1,b-1
      cnt = cnt + product(blk_sizes(:,n)) * dim
    end do

    coords = zero
    do n = 1,dim
      do k = 1,blk_sizes(3,b)
        do j = 1,blk_sizes(2,b)
          do i = 1,blk_sizes(1,b)
            cnt = cnt + 1
            coords(n,i,j,k) = coord_data(cnt)
          end do
        end do
      end do
    end do
  end subroutine access_block_data

  subroutine read_plot3D( fid, query, data_start, n_blocks, dim, total_size, blk_sizes, coord_data, status )
    use set_constants, only : max_text_line_length
    integer,                                   intent(in)    :: fid
    logical,                                   intent(in)    :: query
    integer,                                   intent(inout) :: data_start, n_blocks, dim, total_size
    integer,  dimension(3,n_blocks), optional, intent(out)   :: blk_sizes
    real(dp), dimension(total_size), optional, intent(out)   :: coord_data
    integer,                         optional, intent(out)   :: status
    character(max_text_line_length) :: line
    integer :: n
    integer :: ierr
    logical :: single_block, plot2D

    if ( present(status) ) status = 0
    ierr = 0
    if (query) then
      call query_plot3D(fid,plot2D,single_block,data_start,n_blocks,dim,total_size,status=ierr)
      if (ierr==0) then
        if ( present(blk_sizes) ) then
          call read_plot3D_size( fid, single_block, n_blocks, dim, blk_sizes, status=ierr )
        end if
      end if
    else
      if ( present(blk_sizes) ) then
        call read_plot3D_size( fid, single_block, n_blocks, dim, blk_sizes, status=ierr )
      end if
      do n = 1,data_start-1
        read(fid,'(A)') line
      end do
      read (fid,*) ( coord_data(n), n=1,total_size )
    end if

    if ( ierr /= 0 ) then
      if ( present(status) ) status = ierr
    end if

    rewind(fid)

  end subroutine read_plot3D

  subroutine read_plot3D_size( fid, single_block, n_blocks, dim, blk_sizes, status )
    use set_constants, only : max_text_line_length
    integer,                         intent(in)  :: fid
    logical,                         intent(in)  :: single_block
    integer,                         intent(in)  :: n_blocks, dim
    integer,  dimension(3,n_blocks), intent(out) :: blk_sizes
    integer,               optional, intent(out) :: status
    integer :: n, i, j, k
    integer :: ierr, opt
    character(max_text_line_length) :: line

    if ( present(status) ) status = 0
    ierr      = 0
    blk_sizes = 1
    if (dim==2) then
      opt = 1
      if (.not.single_block) opt = 2
    else
      opt = 3
      if (.not.single_block) opt = 4
    end if
    select case(opt)
    case(1)
      read(fid,'(A)') line
      read(line,*,iostat=ierr) i, j
      if ( ierr == 0 ) blk_sizes(1:2,1) = [i,j]
    case(2)
      read(fid,'(A)') line
      read(line,*,iostat=ierr) n
      if ( ierr == 0 ) then
        do n = 1,n_blocks
          read(fid,'(A)') line
          read(line,*,iostat=ierr) i, j
          if ( ierr /= 0 ) exit
          blk_sizes(1:2,n) = [i,j]
        end do
      end if
    case(3)
      read(fid,'(A)') line
      read(line,*,iostat=ierr) i, j, k
      if ( ierr == 0 ) blk_sizes(1:3,1) = [i,j,k]
    case(4)
      read(fid,'(A)') line
      read(line,*,iostat=ierr) n
      if ( ierr == 0 ) then
        do n = 1,n_blocks
          read(fid,'(A)') line
          read(line,*,iostat=ierr) i, j, k
          if ( ierr /= 0 ) exit
          blk_sizes(1:3,n) = [i,j,k]
        end do
      end if
    case default
      ierr = 1
    end select

    if ( ierr /= 0 ) then
      if ( present(status) ) status = ierr
    end if

    rewind(fid)

  end subroutine read_plot3D_size

  subroutine query_plot3D( fid, plot2d, single_block, data_start, n_blocks, dim, total_size, status )
    use set_constants, only : max_text_line_length
    use string_stuff,  only : line_count
    integer,           intent(in)    :: fid
    logical,           intent(out)   :: plot2d, single_block
    integer,           intent(out)   :: data_start, n_blocks, dim, total_size
    integer, optional, intent(out)   :: status
    integer, parameter :: max_dim_cnt = 10
    integer :: n, i, j, k, cnt, opt
    integer :: ierr
    character(max_text_line_length) :: line

    if ( present(status) ) status = 0
    ierr = 0
    data_start = 0
    n_blocks   = 0
    dim        = 0
    total_size = 0
    single_block = .false.
    plot2d       = .false.
    read(fid,'(A)') line
    call line_count(i,max_dim_cnt,line,cnt)
    select case(cnt)
    case(1)
      single_block = .false.
    case(2)
      single_block = .true.
      plot2d       = .true.
    case(3)
      single_block = .true.
      plot2d       = .false.
      read(line,*,iostat=ierr) i, j, k
    case default
      ierr = -cnt
    end select

    if ( ierr == 0 .and.(.not. single_block)) then
      read(fid,'(A)') line
      call line_count(i,max_dim_cnt,line,cnt)
      select case(cnt)
      case(2)
        plot2d = .true.
      case(3)
        plot2d = .false.
        read(line,*,iostat=ierr) i, j, k
      case default
        ierr = -cnt
      end select
    end if

    if ( ierr == 0 ) then
      rewind(fid)
      if (plot2d) then
        opt = 1
        if (.not.single_block) opt = 2
      else
        opt = 3
        if (.not.single_block) opt = 4
      end if

      data_start = data_start + 1

      select case(opt)
      case(1)
        dim      = 2
        n_blocks = 1
        read(fid,'(A)') line
        data_start = data_start + 1
        read(line,*,iostat=ierr) i, j
        if ( ierr == 0 ) total_size = dim * i * j
      case(2)
        dim  = 2
        read(fid,'(A)') line
        data_start = data_start + 1
        read(line,*,iostat=ierr) n_blocks
        if ( ierr /= 0 ) n_blocks = 0
        do n = 1,n_blocks
          read(fid,'(A)') line
          data_start = data_start + 1
          read(line,*,iostat=ierr) i, j
          if ( ierr /= 0 ) exit
          total_size = total_size + dim * i * j
        end do
      case(3)
        dim      = 3
        n_blocks = 1
        read(fid,'(A)') line
        data_start = data_start + 1
        read(line,*,iostat=ierr) i, j, k
        if ( ierr == 0 ) total_size = dim * i * j * k
      case(4)
        dim = 3
        read(fid,'(A)') line
        data_start = data_start + 1
        read(line,*,iostat=ierr) n_blocks
        if ( ierr /= 0 ) n_blocks = 0
        do n = 1,n_blocks
          read(fid,'(A)') line
          data_start = data_start + 1
          read(line,*,iostat=ierr) i, j, k
          if ( ierr /= 0 ) exit
          total_size = total_size + dim * i * j * k
        end do
      case default
        ierr = 1
      end select
    end if

    if ( ierr /= 0 ) then
      if ( present(status) ) status = ierr
    end if

    rewind(fid)
  end subroutine query_plot3D

end module plot_3D_grid_io

module monomial_basis_derived_type
  implicit none
  private
  public :: monomial_basis_t
  type :: monomial_basis_t
    private
    integer, public :: total_degree
    integer, public :: n_dim
    integer, public :: n_terms
    integer, public, allocatable, dimension(:)   :: idx
    integer, public, allocatable, dimension(:,:) :: exponents
    integer, public, allocatable, dimension(:,:) :: diff_idx
  contains
    private
    procedure, public, pass :: eval_m  => evaluate_monomial
    procedure, public, pass :: deval_m => evaluate_monomial_derivative
    procedure, public, pass :: destroy => destroy_monomial_basis_t
    procedure, public, pass :: check_gradient_indexing
  end type monomial_basis_t

  interface monomial_basis_t
    procedure constructor
  end interface monomial_basis_t

contains

  pure function constructor( total_degree, n_dim ) result(this)
    use combinatorics, only : nchoosek, get_exponents
    integer, intent(in) :: total_degree, n_dim
    type(monomial_basis_t) :: this

    call this%destroy()

    this%total_degree  = total_degree
    this%n_dim   = n_dim
    this%n_terms = nchoosek( n_dim + total_degree, total_degree )
    allocate( this%exponents( this%n_dim, this%n_terms ) )
    allocate( this%idx(0:this%total_degree) )
    allocate( this%diff_idx( this%n_dim, this%n_terms ) )
    call get_exponents( this%n_dim, this%total_degree, this%n_terms,           &
                        this%exponents, this%idx, diff_idx=this%diff_idx )
  end function constructor
  
  pure subroutine destroy_monomial_basis_t(this)
    class(monomial_basis_t), intent(inout) :: this
    if ( allocated(this%exponents) ) deallocate( this%exponents )
    if ( allocated(this%idx) )       deallocate( this%idx )
    if ( allocated(this%diff_idx) )  deallocate( this%diff_idx )
  end subroutine destroy_monomial_basis_t

  pure subroutine evaluate_monomial(this,term,x,val,coef)
    use set_precision, only : dp
    use set_constants, only : one
    class(monomial_basis_t), intent(in)  :: this
    integer,                 intent(in)  :: term
    real(dp), dimension(:),  intent(in)  :: x
    real(dp),                        intent(out) :: val
    integer,                         intent(out) :: coef
    integer :: d, i
    val  = one ! << x^[a] >>
    coef = 1   ! << [a]! >>
    do d = 1,this%n_dim
      do i = this%exponents(d,term),1,-1
        val  = val * x(d)
        coef = coef * i
      end do
    end do
  end subroutine evaluate_monomial

  pure subroutine evaluate_monomial_derivative( this, term, x, order,          &
                                                dval, dcoef, coef )
    use set_precision, only : dp
    use set_constants, only : zero, one
    class(monomial_basis_t),         intent(in)  :: this
    integer,                         intent(in)  :: term
    real(dp), dimension(:),          intent(in)  :: x
    integer,  dimension(:),          intent(in)  :: order
    real(dp),                        intent(out) :: dval
    integer,                         intent(out) :: dcoef, coef
    integer :: d, i
    
    dcoef = 1 ! D^[b](x^[a]) = ([a]!)/([a]-[b])! x^[a-b] => << ([a]!)/([a]-[b])! >>
    coef  = 1 ! << ([a]-[b])! >>
    dval  = zero ! << D^[b](x^[a]) >>
    if ( any( this%exponents(:,term)-order(1:this%n_dim) < 0 ) ) return

    dval  = one
    do d = 1,this%n_dim
      do i = this%exponents(d,term),this%exponents(d,term)-order(d)+1,-1
        dcoef = dcoef * i
      end do
      do i = this%exponents(d,term)-order(d),1,-1
        dval  = dval * x(d)
        coef = coef * i
      end do
    end do
  end subroutine evaluate_monomial_derivative

  subroutine check_gradient_indexing( this )
    use set_constants, only : max_text_line_length
    use string_stuff, only : write_integer_tuple
    class(monomial_basis_t), intent(in)  :: this
    integer :: d, term, a, b
    integer, dimension(this%n_dim) :: grad_idx
    character(max_text_line_length) :: tmp_string, out_string
    ! do d = this%total_degree-1,0,-1
    !   ! for each term with this total degree:
    !   do term = this%idx(d),this%idx(d+1)-1
    write(*,*) this%idx
    do d = this%total_degree-1,1,-1
      ! for each term with this total degree:
      ! do term = this%idx(d-1)+1,this%idx(d-1) + 1 + (this%idx(d)-this%idx(d-1)-1)
      do term = this%idx(d-1)+1,this%idx(d)
        grad_idx = this%diff_idx(:,term) ! indices to extract gradient information
        out_string=''
        call write_integer_tuple([d,term],tmp_string)
        write(out_string,'(A,A)') trim(out_string),trim(tmp_string)//' : '
        call write_integer_tuple(this%exponents(:,term),tmp_string)
        write(out_string,'(A,A)') trim(out_string),'['//trim(tmp_string)//'] : '
        call write_integer_tuple(grad_idx,tmp_string)
        write(out_string,'(A,A)') trim(out_string),'('//trim(tmp_string)//')'
        write(*,'(A)') trim(out_string)
      end do
    end do
    term = 1
    grad_idx = this%diff_idx(:,term) ! indices to extract gradient information
    out_string=''
    call write_integer_tuple([d,term],tmp_string)
    write(out_string,'(A,A)') trim(out_string),trim(tmp_string)//' : '
    call write_integer_tuple(this%exponents(:,term),tmp_string)
    write(out_string,'(A,A)') trim(out_string),'['//trim(tmp_string)//'] : '
    call write_integer_tuple(grad_idx,tmp_string)
    write(out_string,'(A,A)') trim(out_string),'('//trim(tmp_string)//')'
    write(*,'(A)') trim(out_string)

    ! constant term
  end subroutine check_gradient_indexing

end module monomial_basis_derived_type

module function_holder_type
  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: func_h_t

  type, abstract :: func_h_t
    integer :: n_eq
    integer :: n_dim
    logical :: derivatives_implemented
  contains
    procedure :: initialize_super
    procedure, pass :: test_eval
    procedure, pass :: dx_eval   => not_implemented_dx_eval
    procedure, pass :: dt_eval   => not_implemented_dt_eval
    procedure, pass :: dtx_eval  => not_implemented_dtx_eval
    procedure, pass :: grad_eval => not_implemented_grad_eval
    procedure, pass :: hess_eval => not_implemented_hess_eval
    procedure(eval_i),      public, deferred :: eval
    ! procedure(dx_eval_i),   public, deferred :: dx_eval
    ! procedure(dt_eval_i),   public, deferred :: dt_eval
    ! procedure(dtx_eval_i),  public, deferred :: dtx_eval
    ! procedure(grad_eval_i), public, deferred :: grad_eval
    ! procedure(hess_eval_i), public, deferred :: hess_eval
    procedure(destroy_i),   public, deferred :: destroy
  end type func_h_t

  abstract interface
    pure function eval_i( this, x, t ) result(q)
      use set_precision,  only : dp
      import func_h_t
      class(func_h_t),        intent(in) :: this
      real(dp), dimension(:), intent(in) :: x
      real(dp), optional,     intent(in) :: t
      real(dp), dimension(this%n_eq)     :: q
    end function eval_i

    ! pure function dx_eval_i( this, x, t, order ) result(dqdx)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   integer, dimension(this%n_dim), optional, intent(in) :: order
    !   real(dp), dimension(this%n_eq)                       :: dqdx
    ! end function dx_eval_i

    ! pure function dt_eval_i( this, x, t ) result(dqdt)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   real(dp), dimension(this%n_eq)                       :: dqdt
    ! end function dt_eval_i

    ! pure function dtx_eval_i( this, x, t, order, t_order ) result(dqdtdx)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   integer, dimension(this%n_dim), optional, intent(in) :: order
    !   integer,                        optional, intent(in) :: t_order
    !   real(dp), dimension(this%n_eq)                       :: dqdtdx
    ! end function dtx_eval_i

    ! pure function grad_eval_i( this, x, t ) result(gradq)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    ! end function grad_eval_i

    ! pure function hess_eval_i( this, x, t ) result(hessq)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   real(dp), dimension(this%n_dim,this%n_dim,this%n_eq) :: hessq
    ! end function hess_eval_i

    pure elemental subroutine destroy_i(this)
      import func_h_t
      class(func_h_t), intent(inout) :: this
    end subroutine destroy_i
  end interface

contains
  subroutine initialize_super( this, n_eq, n_dim )
    class(func_h_t),  intent(inout) :: this
    integer,      intent(in)    :: n_eq, n_dim
    this%n_eq     = n_eq
    this%n_dim    = n_dim
    this%derivatives_implemented = .false.
  end subroutine initialize_super

  pure function test_eval( this, n_dim, n_var, x ) result(val)
    class(func_h_t),        intent(in) :: this
    integer,                intent(in) :: n_dim, n_var
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(n_var)         :: val
    real(dp), dimension(this%n_eq)     :: tmp_val
    integer :: sz, i
    sz = min(n_var,this%n_eq)
    tmp_val = this%eval(x)
    val = zero
    do i = 1,sz
      val(i) = tmp_val(i)
    end do
  end function test_eval

  pure function not_implemented_dx_eval( this, x, t, order ) result(dqdx)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    integer, dimension(this%n_dim), optional, intent(in) :: order
    real(dp), dimension(this%n_eq)                       :: dqdx
    dqdx = zero
  end function not_implemented_dx_eval

  pure function not_implemented_dt_eval( this, x, t ) result(dqdt)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_eq)                       :: dqdt
    dqdt = zero
  end function not_implemented_dt_eval

  pure function not_implemented_dtx_eval( this, x, t, order, t_order ) result(dqdtdx)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    integer, dimension(this%n_dim), optional, intent(in) :: order
    integer,                        optional, intent(in) :: t_order
    real(dp), dimension(this%n_eq)                       :: dqdtdx
    dqdtdx = zero
  end function not_implemented_dtx_eval

  pure function not_implemented_grad_eval( this, x, t ) result(gradq)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    gradq = zero
  end function not_implemented_grad_eval

  pure function not_implemented_hess_eval( this, x, t ) result(hessq)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_dim,this%n_eq) :: hessq
    hessq = zero
  end function not_implemented_hess_eval

end module function_holder_type

module test_function_1
  use function_holder_type, only : func_h_t
  use set_precision,        only : dp
  implicit none
  private
  public :: test_fun1_t

  real(dp), dimension(3), parameter :: coefs = [999.0_dp, 888.0_dp, 777.0_dp]

  type, extends(func_h_t) :: test_fun1_t
  contains
    procedure :: eval    => eval_test_fun1
    procedure :: destroy   => destroy_test_fun1
    procedure :: grad_eval => grad_eval_test_fun1
  end type test_fun1_t

  interface test_fun1_t
    procedure constructor
  end interface test_fun1_t
contains
  function constructor(n_dim,n_eq) result(this)
    integer, intent(in) :: n_dim, n_eq
    type(test_fun1_t)   :: this
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
  end function constructor

  pure elemental subroutine destroy_test_fun1(this)
    class(test_fun1_t), intent(inout) :: this
    continue
  end subroutine destroy_test_fun1

  pure function eval_test_fun1( this, x, t ) result(q)
    class(test_fun1_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    
    ! q = 999.0_dp * x(1) - 888.0_dp * x(2) + 777.0_dp * x(3) - 666.0_dp
    q = dot_product(x,coefs(1:size(x))) - 666.0_dp
  end function eval_test_fun1

  pure function grad_eval_test_fun1( this, x, t ) result(gradq)
    class(test_fun1_t),        intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    integer :: i
    do i = 1,this%n_eq
      gradq(:,i) = coefs(1:size(x))
    end do
  end function grad_eval_test_fun1

end module test_function_1

module test_function_2
  use function_holder_type, only : func_h_t
  use set_precision,        only : dp
  implicit none
  private
  public :: test_fun2_t

  type, extends(func_h_t) :: test_fun2_t
  contains
    procedure :: eval      => eval_test_fun2
    procedure :: destroy   => destroy_test_fun2
    procedure :: grad_eval => grad_eval_test_fun2
  end type test_fun2_t

  interface test_fun2_t
    procedure constructor
  end interface test_fun2_t
contains
  function constructor(n_dim,n_eq) result(this)
    integer, intent(in) :: n_dim, n_eq
    type(test_fun2_t)   :: this
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
  end function constructor

  pure elemental subroutine destroy_test_fun2(this)
    class(test_fun2_t), intent(inout) :: this
    continue
  end subroutine destroy_test_fun2

  pure function eval_test_fun2( this, x, t ) result(q)
    use set_constants, only : pi
    class(test_fun2_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    integer :: i

    q = sin(pi*x(1))
    do i = 2,this%n_dim
      q = q*sin(pi*x(i))
    end do
  end function eval_test_fun2

  pure function grad_eval_test_fun2( this, x, t ) result(gradq)
    use set_constants, only : pi
    class(test_fun2_t),        intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    integer :: n, i
    real(dp), dimension(this%n_dim) :: tmpq

    do n = 1,this%n_dim
      tmpq(n) = pi*cos(pi*x(n))
      do i = 1,n-1
        tmpq(n) = tmpq(n)*sin(pi*x(i))
      end do
      do i = n+1,this%n_dim
        tmpq(n) = tmpq(n)*sin(pi*x(i))
      end do
    end do

    do i = 1,this%n_eq
      gradq(:,i) = tmpq
    end do
  end function grad_eval_test_fun2

end module test_function_2

module test_problem
  use set_precision, only : dp
  implicit none
  private
  public :: setup_grid
  public :: geom_space_wrapper
  public :: make_eval_function, make_grid
  public :: output_grid
  public :: output_volume_subzone
  public :: output_face_subzone
  public :: output_line_subzone
  interface setup_grid
    module procedure setup_grid_generate
    module procedure setup_grid_read
  end interface setup_grid
contains

  pure function geom_space_wrapper(x_in) result(x_out)
    use project_inputs, only : geom_space_r
    real(dp), dimension(:), intent(in) :: x_in
    real(dp), dimension(size(x_in))    :: x_out
    x_out = geom_space(x_in,geom_space_r)
  end function geom_space_wrapper

  pure function geom_space(x_in,r) result(x_out)
    use set_constants, only : zero, one, near_zero
    use linspace_helper, only : linspace
    real(dp), dimension(:), intent(in) :: x_in
    real(dp),               intent(in) :: r
    real(dp), dimension(size(x_in))    :: x_out
    real(dp) :: dx0, dx
    integer :: i, j, N
    N = size(x_in)
    x_out = zero
    if ( abs(r-one)<near_zero) then
      x_out = linspace(N,x_in(1),x_in(N))
    else
      x_out(1) = x_in(1)
      x_out(N) = x_in(N)
      dx0 = one
      do i = 1,N-2
        dx0 = dx0 + r**i
      end do
      dx0 = ( x_in(N) - x_in(1) ) / dx0
      do i = 2,N-1
        dx = dx0
        do j = 1,i-2
          dx = dx * r
        end do
        x_out(i) = x_out(i-1) + dx
      end do
    end if
  end function geom_space

  subroutine setup_grid_read( file_name, n_dim, n_ghost, n_skip, grid )
    use grid_derived_type,    only : grid_type
    use plot_3D_grid_io,      only : read_grid
    character(*),          intent(in)  :: file_name
    integer,               intent(in)  :: n_dim
    integer, dimension(3), intent(in)  :: n_ghost, n_skip
    type(grid_type),       intent(out) :: grid
    integer :: b
    call read_grid(file_name,n_dim,n_ghost,n_skip,grid)
    do b = 1,grid%n_blocks
      call grid%gblock(b)%grid_vars%setup( grid%gblock(b) )
    end do
    grid%total_int_cells  = sum( grid%gblock%total_cells )
    grid%total_int_volume = sum( grid%gblock%total_volume )
  end subroutine setup_grid_read

  subroutine setup_grid_generate( n_dim, n_nodes, n_ghost, n_skip, grid, name, delta, &
                                    end_pts, x1_map, x2_map, x3_map )
    use grid_derived_type,    only : grid_type
    use linspace_helper,      only : cartesian_mesh, annulus_mesh, sphere_mesh, perturb_mesh, map_1D_fun
    use message,              only : warning_message, WARN_ALWAYS
    use project_inputs,       only : verbose_level
    integer, intent(in) :: n_dim
    integer, dimension(3),  intent(in)             :: n_nodes, n_ghost, n_skip
    type(grid_type),        intent(out)            :: grid
    character(*), optional, intent(in)             :: name
    real(dp), optional,     intent(in)             :: delta
    real(dp), dimension(3,2), optional, intent(in) :: end_pts
    procedure(map_1D_fun), optional                :: x1_map, x2_map, x3_map
    logical :: err

    call grid%setup(n_dim,1)
    call grid%gblock(1)%setup(1,n_dim,n_nodes,n_ghost,n_skip=n_skip)

    if ( present(name) ) then
      select case(name)
      case('sphere')
        call grid%gblock(1)%set_nodes( sphere_mesh(   n_nodes(1),                 &
                                                      n_nodes(2),                 &
                                                      n_nodes(3),                 &
                                                      end_pts=end_pts,            &
                                                      r_fun=x1_map,               &
                                                      theta_fun=x2_map,           &
                                                      phi_fun=x3_map ) )
      case('annulus')
        call grid%gblock(1)%set_nodes( annulus_mesh(  n_nodes(1),                 &
                                                      n_nodes(2),                 &
                                                      n_nodes(3),                 &
                                                      end_pts=end_pts,            &
                                                      r_fun=x1_map,               &
                                                      theta_fun=x2_map,           &
                                                      z_fun=x3_map ) )
      case('cartesian')
        call grid%gblock(1)%set_nodes( cartesian_mesh(n_nodes(1),                 &
                                                      n_nodes(2),                 &
                                                      n_nodes(3),                 &
                                                      end_pts=end_pts,            &
                                                      x_fun=x1_map,               &
                                                      y_fun=x2_map,               &
                                                      z_fun=x3_map ) )
      case default
        err = warning_message(WARN_ALWAYS,'setup_grid_generate','unrecognized grid name; using cartesian')
        call grid%gblock(1)%set_nodes( cartesian_mesh(n_nodes(1),                 &
                                                      n_nodes(2),                 &
                                                      n_nodes(3),                 &
                                                      end_pts=end_pts,            &
                                                      x_fun=x1_map,               &
                                                      y_fun=x2_map,               &
                                                      z_fun=x3_map ) )
      end select
    else
      call grid%gblock(1)%set_nodes( cartesian_mesh(n_nodes(1),                 &
                                                    n_nodes(2),                 &
                                                    n_nodes(3),                 &
                                                    end_pts=end_pts,            &
                                                    x_fun=x1_map,               &
                                                    y_fun=x2_map,               &
                                                    z_fun=x3_map ) )
    end if
    if (present(delta) ) then
      call perturb_mesh( grid%gblock(1)%node_coords, delta )
    end if
    call grid%gblock(1)%grid_vars%setup( grid%gblock(1) )
    grid%total_int_cells  = sum( grid%gblock%total_cells )
    grid%total_int_volume = sum( grid%gblock%total_volume )
  end subroutine setup_grid_generate

  subroutine make_eval_function(eval_fun)
    use project_inputs,       only : test_function, space_origin, space_scale, &
                                     time_origin, time_scale, rand_coefs,      &
                                     rand_seed, n_dim, n_rec_vars
    use function_holder_type, only : func_h_t
    use test_function_1,      only : test_fun1_t
    use test_function_2,      only : test_fun2_t
    use message,              only : error_message
    class(func_h_t), allocatable, intent(out) :: eval_fun
    character(*), parameter :: routine_name = 'set_eval_function'
    logical :: err
    select case(test_function)
    case(1)
      allocate( eval_fun, source=test_fun1_t( n_dim, n_rec_vars ) )
    case(2)
      allocate( eval_fun, source=test_fun2_t( n_dim, n_rec_vars ) )
    case default
      err = error_message(routine_name,'test_function must be 1-2')
    end select
  end subroutine make_eval_function

  subroutine make_grid( grid )
    use project_inputs,       only : n_dim, n_nodes, n_ghost, n_skip, grid_perturb
    use grid_derived_type,    only : grid_type
    use grid_local,           only : grid_l => grid

    use quadrature_derived_type, only : reference_quad_t
    use project_inputs,          only : out_quad_order

    type(grid_type), intent(inout) :: grid

    type(reference_quad_t) :: Q
    integer :: status
    call Q%create(n_dim,out_quad_order,include_ends=.true.,status=status)

    call setup_grid( n_dim, n_nodes, n_ghost, n_skip, grid, delta=grid_perturb, x1_map=geom_space_wrapper )

    ! copy of grid saved in a module to help with debugging
    call setup_grid( n_dim, n_nodes, n_ghost, n_skip, grid_l, delta=grid_perturb, x1_map=geom_space_wrapper )
  end subroutine make_grid

  subroutine output_gblock( grid, blk, file_name, old, strand_id, solution_time )
    use tecplot_output, only : write_tecplot_ordered_zone_header
    use tecplot_output, only : write_tecplot_ordered_zone_block_packed
    use grid_derived_type,          only : grid_type
    type(grid_type),                 intent(in)    :: grid
    integer,                         intent(in)    :: blk
    character(*),                    intent(in)    :: file_name
    logical,                         intent(inout) :: old
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    integer,  dimension(grid%n_dim) :: n_nodes
    integer :: lin_idx
    real(dp), dimension(0,0) :: CELL_DATA
    real(dp), dimension(:,:), allocatable :: NODE_DATA
    character(*), dimension(3), parameter :: xyz        = ['x','y','z']
    character(100), dimension(:), allocatable :: var_names
    character(100) :: zone_name, tmp_name
    integer, dimension(3) :: lo,hi,stride

    integer :: n_dim, n_vars, n_node_vars, n_cell_vars
    integer :: fid, cnt, i, j, k
    logical :: file_exists

    n_dim = grid%n_dim

    n_node_vars = n_dim
    n_cell_vars = 0
    n_vars      = n_node_vars + n_cell_vars

    n_nodes     = ( grid%gblock(blk)%n_nodes(1:grid%n_dim) - 1 )/grid%gblock(1)%n_skip(1:grid%n_dim) + 1

    allocate( var_names( n_vars ) )
    allocate( NODE_DATA( n_node_vars, product(n_nodes) ) )

    write(zone_name,'(A,I0)') "'BLOCK:'",blk

    cnt = 0
    do i = 1,n_dim
      cnt = cnt + 1
      var_names(cnt) = xyz(i)
    end do

    lo(1) = lbound( grid%gblock(blk)%node_coords,dim=2)
    lo(2) = lbound( grid%gblock(blk)%node_coords,dim=3)
    lo(3) = lbound( grid%gblock(blk)%node_coords,dim=4)

    hi(1) = ubound( grid%gblock(blk)%node_coords,dim=2)
    hi(2) = ubound( grid%gblock(blk)%node_coords,dim=3)
    hi(3) = ubound( grid%gblock(blk)%node_coords,dim=4)
    
    cnt = 0
    stride = 1; stride(1:grid%n_dim) = grid%gblock(blk)%n_skip(1:grid%n_dim)
    do k = lo(3),hi(3),stride(3)
      do j = lo(2),hi(2),stride(2)
        do i = lo(1),hi(1),stride(1)
          cnt = cnt + 1
          NODE_DATA( 1:n_dim, cnt ) = grid%gblock(blk)%node_coords(1:n_dim,i,j,k)
        end do
      end do
    end do
    
    inquire( file=trim(file_name), exist=file_exists )
    if ( file_exists ) then
      if (.not.old) then
        open( newunit=fid, file=trim(file_name), status='unknown')
      else
        open( newunit=fid, file=trim(file_name), status='old',                 &
                                                 position='append' )
      end if
    else
      open( newunit=fid, file=trim(file_name), status='unknown')
    end if
    old = .true.

    call write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,               &
                                           n_node_vars, n_cell_vars,           &
                                           zone_name=zone_name,                &
                                           var_names=var_names,                &
                                           data_packing='block',               &
                                           solution_time=solution_time,        &
                                           strand_id=strand_id )
    call write_tecplot_ordered_zone_block_packed( fid, n_nodes,                &
                                                  n_node_vars, n_cell_vars,    &
                                                  NODE_DATA, CELL_DATA )
    close(fid)
    deallocate( var_names, NODE_DATA )
  end subroutine output_gblock

  subroutine output_volume_subzone( n_dim, volume_nodes, file_name, old, zone_name, strand_id, solution_time )
    use tecplot_output, only : write_tecplot_ordered_zone_header
    use tecplot_output, only : write_tecplot_ordered_zone_block_packed
    integer,                         intent(in)    :: n_dim
    real(dp), dimension(:,:,:,:),    intent(in)    :: volume_nodes
    character(*),                    intent(in)    :: file_name
    logical,                         intent(inout) :: old
    character(*),          optional, intent(in)    :: zone_name
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    integer,  dimension(n_dim) :: n_nodes
    integer :: lin_idx
    real(dp), dimension(0,0) :: CELL_DATA
    real(dp), dimension(:,:), allocatable :: NODE_DATA
    character(*), dimension(3), parameter :: xyz        = ['x','y','z']
    character(100), dimension(:), allocatable :: var_names
    integer, dimension(4) :: tmp
    integer, dimension(3) :: lo,hi

    integer :: n_vars, n_node_vars, n_cell_vars
    integer :: fid, cnt, i, j, k
    logical :: file_exists

    n_node_vars = n_dim
    n_cell_vars = 0
    n_vars      = n_node_vars + n_cell_vars

    tmp = lbound(volume_nodes)
    lo  = tmp(1:3)
    tmp = ubound(volume_nodes)
    hi  = tmp(1:3)
    n_nodes = hi - lo + 1

    allocate( var_names( n_vars ) )
    allocate( NODE_DATA( n_node_vars, product(n_nodes) ) )

    ! if (n_dim==1) then
    !   write(zone_name,'(A)') "('CELL:(',I0,',[',I0,'])')"
    ! else
    !   write(zone_name,'(A,I0,A)') "('CELL:(',I0,',[',I0,",n_dim-1,"(',',I0),'])')"
    ! end if
    ! write(zone_name,trim(zone_name)) blk, cell_idx(1:n_dim)

    cnt = 0
    do i = 1,n_dim
      cnt = cnt + 1
      var_names(cnt) = xyz(i)
    end do
    
    cnt = 0
    do k = lo(3),hi(3)
      do j = lo(2),hi(2)
        do i = lo(1),hi(1)
          cnt = cnt + 1
          NODE_DATA( 1:n_dim, cnt ) = volume_nodes(i,j,k,1:n_dim)
        end do
      end do
    end do
    
    inquire( file=trim(file_name), exist=file_exists )
    if ( file_exists ) then
      if (.not.old) then
        open( newunit=fid, file=trim(file_name), status='unknown')
      else
        open( newunit=fid, file=trim(file_name), status='old',                 &
                                                 position='append' )
      end if
    else
      open( newunit=fid, file=trim(file_name), status='unknown')
    end if
    old = .true.

    call write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,               &
                                           n_node_vars, n_cell_vars,           &
                                           zone_name=zone_name,                &
                                           var_names=var_names,                &
                                           data_packing='block',               &
                                           solution_time=solution_time,        &
                                           strand_id=strand_id )
    call write_tecplot_ordered_zone_block_packed( fid, n_nodes,                &
                                                  n_node_vars, n_cell_vars,    &
                                                  NODE_DATA, CELL_DATA )
    close(fid)
    deallocate( var_names, NODE_DATA )
  end subroutine output_volume_subzone

  subroutine output_face_subzone( n_dim, face_nodes, file_name, old, zone_name, strand_id, solution_time )
    use tecplot_output, only : write_tecplot_ordered_zone_header
    use tecplot_output, only : write_tecplot_ordered_zone_block_packed
    integer,                         intent(in)    :: n_dim
    real(dp), dimension(:,:,:),      intent(in)    :: face_nodes
    character(*),                    intent(in)    :: file_name
    logical,                         intent(inout) :: old
    character(*),          optional, intent(in)    :: zone_name
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    integer,  dimension(n_dim-1) :: n_nodes
    integer :: lin_idx
    real(dp), dimension(0,0) :: CELL_DATA
    real(dp), dimension(:,:), allocatable :: NODE_DATA
    character(*), dimension(3), parameter :: xyz        = ['x','y','z']
    character(100), dimension(:), allocatable :: var_names
    integer, dimension(3) :: tmp
    integer, dimension(2) :: lo,hi

    integer :: n_vars, n_node_vars, n_cell_vars
    integer :: fid, cnt, i, j, k
    logical :: file_exists

    n_node_vars = n_dim
    n_cell_vars = 0
    n_vars      = n_node_vars + n_cell_vars

    tmp = lbound(face_nodes)
    lo  = tmp(1:2)
    tmp = ubound(face_nodes)
    hi  = tmp(1:2)
    n_nodes = hi - lo + 1

    allocate( var_names( n_vars ) )
    allocate( NODE_DATA( n_node_vars, product(n_nodes) ) )

    cnt = 0
    do i = 1,n_dim
      cnt = cnt + 1
      var_names(cnt) = xyz(i)
    end do
    
    cnt = 0
    do j = lo(2),hi(2)
      do i = lo(1),hi(1)
        cnt = cnt + 1
        NODE_DATA( 1:n_dim, cnt ) = face_nodes(i,j,1:n_dim)
      end do
    end do
    
    inquire( file=trim(file_name), exist=file_exists )
    if ( file_exists ) then
      if (.not.old) then
        open( newunit=fid, file=trim(file_name), status='unknown')
      else
        open( newunit=fid, file=trim(file_name), status='old',                 &
                                                 position='append' )
      end if
    else
      open( newunit=fid, file=trim(file_name), status='unknown')
    end if
    old = .true.

    call write_tecplot_ordered_zone_header( fid, n_dim-1, n_nodes,               &
                                           n_node_vars, n_cell_vars,           &
                                           zone_name=zone_name,                &
                                           var_names=var_names,                &
                                           data_packing='block',               &
                                           solution_time=solution_time,        &
                                           strand_id=strand_id )
    call write_tecplot_ordered_zone_block_packed( fid, n_nodes,                &
                                                  n_node_vars, n_cell_vars,    &
                                                  NODE_DATA, CELL_DATA )
    close(fid)
    deallocate( var_names, NODE_DATA )
  end subroutine output_face_subzone

  subroutine output_line_subzone( n_dim, nodes, file_name, old, zone_name, strand_id, solution_time )
    use tecplot_output, only : write_tecplot_ordered_zone_header
    use tecplot_output, only : write_tecplot_ordered_zone_block_packed
    integer,                         intent(in)    :: n_dim
    real(dp), dimension(:,:),        intent(in)    :: nodes
    character(*),                    intent(in)    :: file_name
    logical,                         intent(inout) :: old
    character(*),          optional, intent(in)    :: zone_name
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    integer,  dimension(1) :: n_nodes
    integer :: lin_idx
    real(dp), dimension(0,0) :: CELL_DATA
    real(dp), dimension(:,:), allocatable :: NODE_DATA
    character(*), dimension(3), parameter :: xyz        = ['x','y','z']
    character(100), dimension(:), allocatable :: var_names
    integer :: lo,hi

    integer :: n_vars, n_node_vars, n_cell_vars
    integer :: fid, cnt, i, j, k
    logical :: file_exists

    n_node_vars = n_dim
    n_cell_vars = 0
    n_vars      = n_node_vars + n_cell_vars

    lo = lbound(nodes,2)
    hi = ubound(nodes,2)
    n_nodes(1) = hi - lo + 1

    allocate( var_names( n_vars ) )
    allocate( NODE_DATA( n_node_vars, product(n_nodes) ) )

    cnt = 0
    do i = 1,n_dim
      cnt = cnt + 1
      var_names(cnt) = xyz(i)
    end do
    
    cnt = 0
    do i = lo,hi
      cnt = cnt + 1
      ! NODE_DATA( 1:n_dim, cnt ) = nodes(i,1:n_dim)
      NODE_DATA( 1:n_dim, cnt ) = nodes(1:n_dim,i)
    end do
    
    inquire( file=trim(file_name), exist=file_exists )
    if ( file_exists ) then
      if (.not.old) then
        open( newunit=fid, file=trim(file_name), status='unknown')
      else
        open( newunit=fid, file=trim(file_name), status='old',                 &
                                                 position='append' )
      end if
    else
      open( newunit=fid, file=trim(file_name), status='unknown')
    end if
    old = .true.

    call write_tecplot_ordered_zone_header( fid, 1, n_nodes,               &
                                           n_node_vars, n_cell_vars,           &
                                           zone_name=zone_name,                &
                                           var_names=var_names,                &
                                           data_packing='block',               &
                                           solution_time=solution_time,        &
                                           strand_id=strand_id )
    call write_tecplot_ordered_zone_block_packed( fid, n_nodes,                &
                                                  n_node_vars, n_cell_vars,    &
                                                  NODE_DATA, CELL_DATA )
    close(fid)
    deallocate( var_names, NODE_DATA )
  end subroutine output_line_subzone

  subroutine output_grid( grid, file_name, strand_id, solution_time )
    use grid_derived_type,          only : grid_type
    type(grid_type),                 intent(in)    :: grid
    character(*),                    intent(in)    :: file_name
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    logical :: old
    integer :: blk

    old = .false.

    do blk = 1,grid%n_blocks
      call output_gblock(grid,blk,file_name,old,strand_id=strand_id,solution_time=solution_time)
    end do
  end subroutine output_grid


  ! call getarg(1, refine_str)

  !   read(refine_str, *) refine_factor

  !   print *, 'refine factor: ', refine_factor

end module test_problem

program main
  use set_precision, only : dp
  use set_constants, only : zero, one, two, three, pi, half, fourth, large
  use math,          only : rand_coord_in_range
  use test_problem,  only : setup_grid, geom_space_wrapper, output_grid, output_volume_subzone, output_face_subzone, output_line_subzone
  use grid_derived_type, only : grid_type
  use timer_derived_type, only : basic_timer_t
  use project_inputs, only : n_dim, n_nodes, n_ghost, n_skip
  use linspace_helper, only : sphere_mesh, annulus_mesh
  use boundary_info_type, only : bc_t, bc_holder_t

  implicit none

  type(bc_holder_t)   :: bounds
  type(grid_type)     :: grid
  type(basic_timer_t) :: timer
  real(dp), dimension(:,:,:,:), allocatable :: volume_nodes
  real(dp), dimension(:,:,:),   allocatable :: face_nodes
  real(dp), dimension(:,:),     allocatable :: out_vec
  real(dp), dimension(:,:),     allocatable :: pts
  real(dp), dimension(3) :: pt, xyz_eval, xyz_tmp
  integer, dimension(3) :: cell_idx
  integer, dimension(2) :: shp
  integer, dimension(:), allocatable :: bnd_nums
  real(dp) :: dist, min_dist, ex, err
  integer :: n_iter, n_pts, n_t_pts, n_bnds
  integer :: i, j, cnt
  logical :: old
  character(100) :: zone_name
  character(*), parameter :: file_name='TEST_GRID.dat'
  ! n_dim   = 3
  ! n_nodes = [9,17,17]
  ! n_ghost = [0,0,0]
  ! n_skip  = [2,2,2]

  n_dim   = 2
  n_nodes = [9,17,1]
  n_ghost = [0,0,0]
  n_skip  = [2,2,0]

  n_iter   = 100
  n_pts = 100
  n_t_pts = n_pts**(n_dim-1)
  
  n_bnds = 2*n_dim
  allocate( bnd_nums(n_bnds) )
  cnt = 0
  do j = 1,n_dim
    do i = -1,1,2
      cnt = cnt + 1
      bnd_nums(cnt) = i*j
    end do
  end do

  ! call setup_grid( n_dim, n_nodes, n_ghost, n_skip, grid )
  ! setup_grid_read(file_name, n_dim, n_ghost, n_skip, grid)
  ! call setup_grid('/mnt/c/Users/wajordan/Desktop/_kt0257x0065/kt.grd',n_dim,n_ghost,n_skip,grid)
  call setup_grid('/mnt/c/Users/Will/Desktop/MY_CASES/_kt0513x0129/kt.grd',n_dim,n_ghost,n_skip,grid)
  n_nodes = grid%gblock(1)%n_nodes

  ! call bounds%create(2)
  ! call bounds%bc(1)%create(n_dim, 1, 1, 202, [49,81,1,1], node_bnd_min=[1,1,1], node_bnd_max=n_nodes)
  ! call bounds%bc(1)%create(n_dim, 1, 1, 202, [129,385,1,1], node_bnd_min=[1,1,1], node_bnd_max=n_nodes, n_skip=n_skip )
  ! call bounds%destroy()

  allocate( pts(3,n_t_pts) )
  ! allocate( pts(3,n_pts) )

  ! pts = reshape(sphere_mesh(1,n_pts,n_pts,end_pts=reshape([1.5_dp,zero,fourth*pi,1.5_dp,half*pi,three*fourth*pi],[3,2])),[3,n_t_pts])
  ! pts = reshape(sphere_mesh(n_pts,1,n_pts,end_pts=reshape([1.0_dp,half*pi,fourth*pi,2.0_dp,half*pi,three*fourth*pi],[3,2])),[3,n_t_pts])

  pts = reshape(annulus_mesh(1,n_pts,1,end_pts=reshape([1.5_dp,zero,zero,three,half*pi,zero],[3,2])),[3,n_t_pts])

  call output_grid( grid, file_name )
  old = .true.

  
  allocate(out_vec(3,2))
  ! do j = 495,495 !1,n_t_pts
  do j = 1,n_t_pts
    ! pt = rand_coord_in_range(3,[two,two,two],[three,three,three])
    pt = pts(:,j)
    ! pt = [2.3749641296722843_dp,1.8223740721795063_dp,-0.19620938769042878_dp]
    xyz_eval = zero
    min_dist = large
    do i = 1,n_bnds
      call grid%gblock(1)%get_min_distance(bnd_nums(i),pt,dist,max_iter=n_iter,xyz_eval=xyz_tmp,clip=.true.,out_idx=cell_idx)
      if ( dist < min_dist ) then
        min_dist = dist
        xyz_eval = xyz_tmp
      end if
    end do
        
    ex = abs(norm2(pt)-one)
    err = ex - min_dist
    write(*,'(A,3(ES16.7),A,ES16.7,A,ES16.7)') 'pt = ',pt,' Min. distance = ', min_dist, ' Error: ', err

    ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
    ! allocate( face_nodes(shp(1),shp(2),3) )
    ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
    ! write(zone_name,'(A,I0)') 'FACE:',n
    ! call output_face_subzone(n_dim,face_nodes,file_name,old,zone_name=trim(zone_name))
    ! deallocate( face_nodes )
  
    out_vec(:,1) = pt
    out_vec(:,2) = xyz_eval
    write(zone_name,'(A,I0)') 'PT:',j
    call output_line_subzone(n_dim,out_vec,file_name,old,zone_name=trim(zone_name))
  end do
  deallocate( out_vec )
  deallocate( pts )
  deallocate( bnd_nums )
  ! cell_idx = [1,1,4]
  ! allocate( volume_nodes( grid%gblock(1)%n_skip(1)+1, &
  !                         grid%gblock(1)%n_skip(2)+1, &
  !                         grid%gblock(1)%n_skip(3)+1, &
  !                         3 ) )
  
  ! call grid%gblock(1)%get_fg_volume_nodes(cell_idx,volume_nodes)


  ! call output_volume_subzone(n_dim,volume_nodes,'TEST_GRID',old)

  ! deallocate( volume_nodes )

  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
  ! allocate( face_nodes(shp(1),shp(2),3) )
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
  ! call output_face_subzone(n_dim,face_nodes,'TEST_GRID',old)
  ! deallocate( face_nodes )

  ! pt = [three*cos(pi/32.0_dp),three*sin(pi/32.0_dp),one]
  ! allocate(out_vec(3,n_iter+3))
  ! out_vec = grid%gblock(1)%get_wall_distance_vec(cell_idx,bnd_num,pt,n_iter)
  ! call output_line_subzone(n_dim,out_vec,'TEST_GRID',old)
  ! deallocate( out_vec )

  ! bnd_num = 1
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
  ! allocate( face_nodes(shp(1),shp(2),3) )
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
  ! call output_face_subzone(n_dim,face_nodes,'TEST_GRID',old)
  ! deallocate( face_nodes )

  ! bnd_num = -2
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
  ! allocate( face_nodes(shp(1),shp(2),3) )
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
  ! call output_face_subzone(n_dim,face_nodes,'TEST_GRID',old)
  ! deallocate( face_nodes )

  ! bnd_num = 2
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
  ! allocate( face_nodes(shp(1),shp(2),3) )
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
  ! call output_face_subzone(n_dim,face_nodes,'TEST_GRID',old)
  ! deallocate( face_nodes )

  ! bnd_num = -3
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
  ! allocate( face_nodes(shp(1),shp(2),3) )
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
  ! call output_face_subzone(n_dim,face_nodes,'TEST_GRID',old)
  ! deallocate( face_nodes )

  ! bnd_num = 3
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num)
  ! allocate( face_nodes(shp(1),shp(2),3) )
  ! call grid%gblock(1)%get_fg_face_nodes(cell_idx,shp,bnd_num,face_nodes=face_nodes)
  ! call output_face_subzone(n_dim,face_nodes,'TEST_GRID',old)
  ! deallocate( face_nodes )

  

  call grid%destroy()
end program main