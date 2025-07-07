module SLB4MPI_dynamic_load_balancer_m
#ifdef SLB4MPI_WITH_MPI
  use mpi
#endif
  use SLB4MPI_abstract_load_balancer_m
  implicit none
  private

  type, extends(load_balancer_t) :: dynamic_load_balancer_t
#ifdef SLB4MPI_WITH_MPI
    integer(MPI_INTEGER_KIND) :: window
#else
    integer(MPI_INTEGER_KIND) :: counter
#endif
  contains
    procedure :: initialize
    procedure :: get_range
    procedure :: clean
  end type

  interface

  end interface

  public :: dynamic_load_balancer_t

contains
  !>
  !> @brief initialize dynamic load balancer
  !>
  !> @param[in,out] lb         - load balancer object
  !> @param[in] communicator   - MPI communicator on which load balancer will be used
  !> @param[in] lower_bound    - lower bound of range
  !> @param[in] upper_bound    - upper bound of range, upper_bound >= lower_bound
  !> @param[in] min_chunk_size - minimal size of chank that can be associated with job, default: 1
  !> @param[in] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
  !>
  subroutine initialize(lb, communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
    class(dynamic_load_balancer_t), intent(inout) :: lb
    integer(MPI_INTEGER_KIND),      intent(in)    :: communicator
    integer(8),                     intent(in)    :: lower_bound, upper_bound
    integer(8),           optional, intent(in)    :: min_chunk_size, max_chunk_size

    integer(MPI_ADDRESS_KIND) :: size
    integer(MPI_INTEGER_KIND) :: disp_unit, ierr
    type(c_ptr) :: baseaddr
    integer(8), pointer :: value

    size = storage_size(lower_bound) / BITS_IN_BYTE
    disp_unit = size

    call lb%default_initialize(communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)

#ifdef SLB4MPI_WITH_MPI
    call MPI_Win_allocate(size, disp_unit, MPI_INFO_NULL, lb%communicator, baseaddr, lb%window, ierr)

    if (lb%rank == lb%root) then
      call c_f_pointer(baseaddr, value)
      value = lower_bound
    end if

    call MPI_Barrier(lb%communicator, ierr)
#else
    lb%counter = lb%lower_bound
#endif

  end subroutine initialize

  !>
  !> @brief get range to compute
  !>
  !> @note `upper_bound` can be only less than or equal `dynamic_load_balancer_t%upper_bound`.
  !>       So, for last elements a batch with size less than `min_chunk_size` can be returned
  !>
  !> @param[in,out] lb       - load balancer object
  !> @param[out] lower_bound - lower bound of range to compute
  !> @param[out] upper_bound - upper bound of range to compute
  !> @return                 - true if there is something to compute
  !>
  logical function get_range(lb, lower_bound, upper_bound) result(to_compute)
    class(dynamic_load_balancer_t), intent(inout) :: lb
    integer(8), intent(out) :: lower_bound, upper_bound

    integer(MPI_INTEGER_KIND) :: ierr

    to_compute = .true.

#ifdef SLB4MPI_WITH_MPI
    call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, lb%root, 0_MPI_INTEGER_KIND, lb%window, ierr)
    call MPI_Fetch_and_op(lb%min_chunk_size, lower_bound, MPI_INTEGER8, lb%root, 0_MPI_ADDRESS_KIND, MPI_SUM, lb%window, ierr)
    call MPI_Win_unlock(lb%root, lb%window, ierr)
#else
    lower_bound = lb%counter
    lb%counter = lb%counter + lb%min_chunk_size
#endif

    upper_bound = min(lower_bound + lb%min_chunk_size - 1, lb%upper_bound)

    if (lower_bound > lb%upper_bound) to_compute = .false.

  end function get_range

  !>
  !> @brief dynamic load balancer destructor
  !>
  !> @param[in,out] lb - load balancer object
  !>
  subroutine clean(lb)
    class(dynamic_load_balancer_t), intent(inout) :: lb
    integer(MPI_INTEGER_KIND) :: ierr

#ifdef SLB4MPI_WITH_MPI
    if (lb%window /= MPI_WIN_NULL) then
      call MPI_Win_free(lb%window, ierr)
    end if
#endif
  end subroutine clean
end module SLB4MPI_dynamic_load_balancer_m
