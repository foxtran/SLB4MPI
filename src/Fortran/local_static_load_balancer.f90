module local_static_load_balancer_m
  use abstract_load_balancer_m
  implicit none
  private

  type, extends(load_balancer_t) :: local_static_load_balancer_t
    private
    integer(8) :: counter
  contains
    procedure :: initialize
    procedure :: get_range
    procedure :: clean
  end type

  interface

  end interface

  public :: local_static_load_balancer_t

contains
  !>
  !> @brief initialize local static load balancer
  !>
  !> @param[in]           communicator   - MPI communicator on which load balancer will be used
  !> @param[in]           lower_bound    - lower bound of range
  !> @param[in]           upper_bound    - upper bound of range, upper_bound >= lower_bound
  !> @param[in, optional] min_chunk_size - minimal size of chank that can be associated with job, default: 1
  !> @param[in, optional] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
  !>
  subroutine initialize(lb, communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)
    class(local_static_load_balancer_t), intent(inout) :: lb
    integer(MPI_INTEGER_KIND),           intent(in)    :: communicator
    integer(8),                          intent(in)    :: lower_bound, upper_bound
    integer(8),                optional, intent(in)    :: min_chunk_size, max_chunk_size
    !
    integer(8) :: n_tasks, extra_tasks

    call lb%default_initialize(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)

    n_tasks = (upper_bound - lower_bound + 1) / lb%nprocs
    extra_tasks = mod(upper_bound - lower_bound + 1, lb%nprocs)
    if (n_tasks < min_chunk_size) then
        n_tasks = min_chunk_size
        extra_tasks = 0
    end if

    if (lb%rank < extra_tasks) then
        lb%lower_bound = lb%rank * (n_tasks + 1) + lower_bound
        lb%upper_bound = (lb%rank + 1) * (n_tasks + 1) + lower_bound - 1
    else
        lb%lower_bound = lb%rank * n_tasks + extra_tasks + lower_bound
        lb%upper_bound = (lb%rank + 1) * n_tasks + extra_tasks + lower_bound - 1
    end if

    lb%lower_bound = min(lb%lower_bound, upper_bound + 1)
    lb%upper_bound = min(lb%upper_bound, upper_bound)

    lb%counter = lb%lower_bound

  end subroutine initialize

  !>
  !> @brief get range to compute
  !>
  !> @note upper_bound can be only less than or equal load_balancer_t%upper_bound
  !>       so, for last elements a batch with size less than min_chunk_size can be returned
  !>
  !> @param[out] lower_bound - lower bound of range to compute
  !> @param[out] upper_bound - upper bound of range to compute
  !> @return                 - is there something to compute?
  !>
  logical function get_range(lb, lower_bound, upper_bound) result(to_compute)
    class(local_static_load_balancer_t), intent(inout) :: lb
    integer(8),                          intent(out)   :: lower_bound, upper_bound

    to_compute = .true.

    lower_bound = lb%counter
    upper_bound = min(lower_bound + lb%max_chunk_size - 1, lb%upper_bound)
    lb%counter = upper_bound + 1

    if (lower_bound > lb%upper_bound) to_compute = .false.

  end function get_range

  !>
  !> @brief local static load balancer destructor
  !>
  subroutine clean(lb)
    class(local_static_load_balancer_t), intent(inout) :: lb
  end subroutine clean
end module local_static_load_balancer_m
