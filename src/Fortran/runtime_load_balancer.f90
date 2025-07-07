module SLB4MPI_runtime_load_balancer_m
  use SLB4MPI_abstract_load_balancer_m
  use SLB4MPI_static_load_balancer_m
  use SLB4MPI_local_static_load_balancer_m
  use SLB4MPI_dynamic_load_balancer_m
  use SLB4MPI_guided_load_balancer_m
  use SLB4MPI_work_stealing_load_balancer_m
  implicit none
  private

  integer, parameter :: ENV_LOAD_BALANCER = 0
  integer, parameter :: STATIC_LOAD_BALANCER = 1
  integer, parameter :: LOCAL_STATIC_LOAD_BALANCER = 2
  integer, parameter :: DYNAMIC_LOAD_BALANCER = 3
  integer, parameter :: GUIDED_LOAD_BALANCER = 4
  integer, parameter :: WORK_STEALING_LOAD_BALANCER = 5
  integer :: load_balancer_type = ENV_LOAD_BALANCER

  type, extends(load_balancer_t) :: runtime_load_balancer_t
    class(load_balancer_t), allocatable :: balancer
  contains
    procedure :: initialize
    procedure :: get_range
    procedure :: clean
  end type

  interface

  end interface

  public :: runtime_load_balancer_t
  public :: SLB4MPI_set_schedule

contains
  !>
  !> @brief initialize runtime load balancer
  !>
  !> @param[in] communicator   - MPI communicator on which load balancer will be used
  !> @param[in] lower_bound    - lower bound of range
  !> @param[in] upper_bound    - upper bound of range, upper_bound >= lower_bound
  !> @param[in] min_chunk_size - minimal size of chank that can be associated with job, default: 1
  !> @param[in] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
  !>
  subroutine initialize(lb, communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
    class(runtime_load_balancer_t), intent(inout) :: lb
    integer(MPI_INTEGER_KIND),      intent(in)    :: communicator
    integer(8),                     intent(in)    :: lower_bound, upper_bound
    integer(8),           optional, intent(in)    :: min_chunk_size, max_chunk_size

    if (load_balancer_type == ENV_LOAD_BALANCER) then
    block
      use, intrinsic :: iso_fortran_env, only: error_unit
      character(len=80) :: envval
      logical :: ok
      call get_environment_variable("SLB4MPI_LOAD_BALANCER", envval)
      call SLB4MPI_set_schedule(trim(envval), ok)
      if (len_trim(envval) /= 0 .and. (.not.ok .or. trim(envval) == 'env')) then
        write(error_unit, '(A)') "SLB4MPI_LOAD_BALANCER environmental variable is not set properly!"
        write(error_unit, '(A)') "Actual value is '" // trim(envval) // "'"
        write(error_unit, '(A)') "Possible values are: static, local_static, dynamic, guided, work_stealing"
        write(error_unit, '(A)') "static load balancer will be used!"
        call SLB4MPI_set_schedule("static")
      else if (len_trim(envval) == 0) then
        call SLB4MPI_set_schedule("static")
      end if
    end block
    end if

    select case (load_balancer_type)
      case(STATIC_LOAD_BALANCER)
        allocate(static_load_balancer_t :: lb%balancer)
      case(LOCAL_STATIC_LOAD_BALANCER)
        allocate(local_static_load_balancer_t :: lb%balancer)
      case(DYNAMIC_LOAD_BALANCER)
        allocate(dynamic_load_balancer_t :: lb%balancer)
      case(GUIDED_LOAD_BALANCER)
        allocate(guided_load_balancer_t :: lb%balancer)
      case(WORK_STEALING_LOAD_BALANCER)
        allocate(work_stealing_load_balancer_t :: lb%balancer)
      case default
        error stop "Unknown load balancer"
    end select

    call lb%balancer%initialize(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)

  end subroutine initialize

  !>
  !> @brief get range to compute
  !>
  !> @note `upper_bound` can be only less than or equal `runtime_load_balancer_t%upper_bound`.
  !>       So, for last elements a batch with size less than `min_chunk_size` can be returned
  !>
  !> @param[out] lower_bound - lower bound of range to compute
  !> @param[out] upper_bound - upper bound of range to compute
  !> @return                 - true if there is something to compute
  !>
  logical function get_range(lb, lower_bound, upper_bound) result(to_compute)
    class(runtime_load_balancer_t), intent(inout) :: lb
    integer(8), intent(out) :: lower_bound, upper_bound

    to_compute = lb%balancer%get_range(lower_bound, upper_bound)

  end function get_range

  !>
  !> @brief runtime load balancer destructor
  !>
  subroutine clean(lb)
    class(runtime_load_balancer_t), intent(inout) :: lb
    call lb%balancer%clean()
    deallocate(lb%balancer)
  end subroutine clean

  !>
  !> @brief set runtime load balancer type
  !>
  !> @param[in]           lbtype - name of load balancer: static, dynamic, guided
  !> @param[out,optional] ok     - was it set?
  subroutine SLB4MPI_set_schedule(lbtype, ok)
    character(len=*), intent(in) :: lbtype
    logical, optional, intent(out) :: ok
    logical :: ok_
    ok_ = .true.
    select case(lbtype)
      case ("env")
        load_balancer_type = ENV_LOAD_BALANCER
      case ("static")
        load_balancer_type = STATIC_LOAD_BALANCER
      case ("local_static")
        load_balancer_type = LOCAL_STATIC_LOAD_BALANCER
      case ("dynamic")
        load_balancer_type = DYNAMIC_LOAD_BALANCER
      case ("guided")
        load_balancer_type = GUIDED_LOAD_BALANCER
      case ("work_stealing")
        load_balancer_type = WORK_STEALING_LOAD_BALANCER
      case default
        ok_ = .false.
    end select
    if (present(ok)) ok = ok_
  end subroutine SLB4MPI_set_schedule
end module SLB4MPI_runtime_load_balancer_m
