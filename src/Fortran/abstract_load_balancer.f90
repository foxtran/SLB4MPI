module SLB4MPI_abstract_load_balancer_m
#ifdef SLB4MPI_WITH_MPI
  use mpi
#endif
  implicit none
  private

#ifndef SLB4MPI_WITH_MPI
  integer, parameter :: MPI_INTEGER_KIND = 4
  integer, parameter :: MPI_ADDRESS_KIND = 8
#endif
  public :: MPI_INTEGER_KIND
  public :: MPI_ADDRESS_KIND
  integer, parameter, public :: BITS_IN_BYTE = 8

  type, abstract :: load_balancer_t
    integer(MPI_INTEGER_KIND) :: communicator !< MPI communicator
    integer(MPI_INTEGER_KIND) :: rank         !< Rank of the process
    integer(MPI_INTEGER_KIND) :: root = 0     !< Rank of the root process
    integer(MPI_INTEGER_KIND) :: nranks       !< Number of processes (group size)
    integer(8) :: lower_bound                 !< Lower bound of range
    integer(8) :: upper_bound                 !< Upper bound of range
    integer(8) :: min_chunk_size              !< Minimal chunk size for job
    integer(8) :: max_chunk_size              !< Maximal chunk size for job
  contains
    procedure(initialize), deferred, public :: initialize
    procedure(get_range),  deferred, public :: get_range
    procedure(clean),      deferred, public :: clean

    procedure :: default_initialize
  end type

  abstract interface

    !>
    !> @brief initialize load balancer
    !>
    !> @param[in,out] lb         - load balancer object
    !> @param[in] communicator   - MPI communicator on which load balancer will be used
    !> @param[in] lower_bound    - lower bound of range
    !> @param[in] upper_bound    - upper bound of range, upper_bound >= lower_bound
    !> @param[in] min_chunk_size - minimal size of chank that can be associated with job, default: 1
    !> @param[in] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
    !>
    subroutine initialize(lb, communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)
      import load_balancer_t, MPI_INTEGER_KIND
      class(load_balancer_t),    intent(inout) :: lb
      integer(MPI_INTEGER_KIND), intent(in)    :: communicator
      integer(8),                intent(in)    :: lower_bound, upper_bound
      integer(8),      optional, intent(in)    :: min_chunk_size, max_chunk_size
    end subroutine initialize

    !>
    !> @brief get range to compute
    !>
    !> @note `upper_bound` can be only less than or equal `load_balancer_t%upper_bound`.
    !>       So, for last elements a batch with size less than `min_chunk_size` can be returned
    !>
    !> @param[in,out] lb       - load balancer object
    !> @param[out] lower_bound - lower bound of range to compute
    !> @param[out] upper_bound - upper bound of range to compute
    !> @return                 - true if there is something to compute
    !>
    logical function get_range(lb, lower_bound, upper_bound)
      import load_balancer_t
      class(load_balancer_t), intent(inout) :: lb
      integer(8),             intent(out)   :: lower_bound, upper_bound
    end function get_range

    !>
    !> @brief load balancer destructor
    !>
    !> @param[in,out] lb - load balancer object
    !>
    subroutine clean(lb)
      import load_balancer_t
      class(load_balancer_t), intent(inout) :: lb
    end subroutine clean

  end interface

  public :: load_balancer_t

contains

  !>
  !> @brief initialize variables of load balancer with default values
  !>
  !> @param[in,out] lb         - load balancer object
  !> @param[in] communicator   - MPI communicator on which load balancer will be used
  !> @param[in] lower_bound    - lower bound of range
  !> @param[in] upper_bound    - upper bound of range, upper_bound >= lower_bound
  !> @param[in] min_chunk_size - minimal size of chank that can be associated with job, default: 1
  !> @param[in] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
  !>
  subroutine default_initialize(lb, communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)
    class(load_balancer_t),    intent(inout) :: lb
    integer(MPI_INTEGER_KIND), intent(in)    :: communicator
    integer(8),                intent(in)    :: lower_bound, upper_bound
    integer(8),      optional, intent(in)    :: min_chunk_size, max_chunk_size

    integer(MPI_INTEGER_KIND) :: ierr

#ifdef SLB4MPI_WITH_MPI
    call MPI_Comm_rank(communicator, lb%rank, ierr)
    call MPI_Comm_size(communicator, lb%nranks, ierr)
#else
    lb%rank = 0
    lb%nranks = 1
#endif

    lb%communicator = communicator
    lb%lower_bound = lower_bound
    lb%upper_bound = upper_bound
    lb%min_chunk_size = 1
    if (present(min_chunk_size)) lb%min_chunk_size = min_chunk_size
    lb%max_chunk_size = max((lb%upper_bound - lb%lower_bound + 1) / lb%nranks, lb%min_chunk_size)
    if (present(max_chunk_size)) lb%max_chunk_size = max_chunk_size

    if (lb%min_chunk_size <= 0) error stop "Min chunk size must be greater than zero!"

    if (lb%lower_bound > lb%upper_bound) error stop "Upper bound is less than lower bound!"
    if (lb%min_chunk_size > lb%max_chunk_size) error stop "Max chunk size is less than min chunk size!"

  end subroutine default_initialize
end module SLB4MPI_abstract_load_balancer_m
