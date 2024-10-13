module work_stealing_load_balancer_m
#ifdef WITH_MPI
  use mpi
#endif
  use abstract_load_balancer_m
  implicit none
  private

  type, extends(load_balancer_t) :: work_stealing_load_balancer_t
#ifdef WITH_MPI
    integer(MPI_INTEGER_KIND) :: window_num_active  !< number of active threads
    integer(MPI_INTEGER_KIND) :: window_bounds      !< lower and upper bounds of rank
    integer(MPI_INTEGER_KIND) :: window_actual_rank !< actual rank to compute (for fast look up)
    integer(MPI_INTEGER_KIND) :: actual_rank
    logical :: done = .false.
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

  public :: work_stealing_load_balancer_t

contains
  !>
  !> @brief initialize work_stealing load balancer
  !>
  !> @param[in]           communicator   - MPI communicator on which load balancer will be used
  !> @param[in]           lower_bound    - lower bound of range
  !> @param[in]           upper_bound    - upper bound of range, upper_bound >= lower_bound
  !> @param[in, optional] min_chunk_size - minimal size of chank that can be associated with job, default: 1
  !> @param[in, optional] max_chunk_size - maximal size of chank that can be associated with job, default: upper_bound - lower_bound + 1
  !>
  subroutine initialize(lb, communicator, lower_bound, upper_bound, min_chunk_size, max_chunk_size)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
    class(work_stealing_load_balancer_t), intent(inout) :: lb
    integer(MPI_INTEGER_KIND),      intent(in)    :: communicator
    integer(8),                     intent(in)    :: lower_bound, upper_bound
    integer(8),           optional, intent(in)    :: min_chunk_size, max_chunk_size

    integer(MPI_INTEGER_KIND) :: ierr
    integer(8) :: n_tasks, extra_tasks
    ! num active
    integer(MPI_ADDRESS_KIND) :: size_num_active
    integer(MPI_INTEGER_KIND) :: disp_unit_num_active
    type(c_ptr) :: baseaddr_num_active
    integer(MPI_INTEGER_KIND), pointer :: num_active
    ! actual rank
    integer(MPI_ADDRESS_KIND) :: size_actual_rank
    integer(MPI_INTEGER_KIND) :: disp_unit_actual_rank
    type(c_ptr) :: baseaddr_actual_rank
    integer(MPI_INTEGER_KIND), pointer :: actual_rank
    ! bounds
    integer(MPI_ADDRESS_KIND) :: size_bounds
    integer(MPI_INTEGER_KIND) :: disp_unit_bounds
    type(c_ptr) :: baseaddr_bounds
    integer(8), pointer :: bounds(:)
    integer(8), parameter :: bounds_2 = 0

    size_num_active = storage_size(lb%nprocs) / BITS_IN_BYTE
    disp_unit_num_active = size_num_active
    size_actual_rank = storage_size(lb%actual_rank) / BITS_IN_BYTE
    disp_unit_actual_rank = size_actual_rank
    size_bounds = 400 ! storage_size(bounds) / BITS_IN_BYTE * 2
    disp_unit_bounds = 1 ! size_bounds / 2

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

#ifdef WITH_MPI
    lb%actual_rank = lb%rank

    call MPI_Win_allocate(size_num_active, disp_unit_num_active, MPI_INFO_NULL, lb%communicator, baseaddr_num_active, lb%window_num_active, ierr)
    call MPI_Win_create(lb%actual_rank, size_actual_rank, disp_unit_actual_rank, MPI_INFO_NULL, lb%communicator, lb%window_actual_rank, ierr)
    call MPI_Win_allocate(size_bounds, disp_unit_bounds, MPI_INFO_NULL, lb%communicator, baseaddr_bounds, lb%window_bounds, ierr)

    if (lb%rank == lb%root) then
      call c_f_pointer(baseaddr_num_active, num_active)
      num_active = lb%nprocs
    end if
    call c_f_pointer(baseaddr_actual_rank, actual_rank)
    actual_rank = lb%rank
    call c_f_pointer(baseaddr_bounds, bounds, [2])
    bounds = [ lb%lower_bound, lb%upper_bound ]

    call MPI_Barrier(lb%communicator, ierr)
#else
    lb%counter = lb%lower_bound
#endif

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
    class(work_stealing_load_balancer_t), intent(inout) :: lb
    integer(8), intent(out) :: lower_bound, upper_bound

    integer(MPI_INTEGER_KIND) :: ierr
    integer(MPI_INTEGER_KIND) :: num_active
    integer(8) :: bounds(2), hop_count, nohop_count
#ifdef DEBUG_LOCKS
    integer(8) :: t1, t2, cr
#endif

    call system_clock(count_rate=cr)

    to_compute = .false.

#ifndef WITH_MPI

    lower_bound = lb%counter
    upper_bound = min(lower_bound + lb%max_chunk_size - 1, lb%upper_bound)
    lb%counter = upper_bound + 1
    if (lower_bound <= lb%upper_bound) to_compute = .true.

#else

    if (lb%actual_rank == lb%rank) then
        to_compute = .true.
        !> select range for computing
        call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, lb%rank, 0_MPI_INTEGER_KIND, lb%window_bounds, ierr)
        call MPI_Get(bounds, 2_MPI_INTEGER_KIND, MPI_INTEGER8, lb%rank, 0_MPI_ADDRESS_KIND, 2_MPI_INTEGER_KIND, MPI_INTEGER8, lb%window_bounds, ierr)
        call MPI_Win_flush(lb%rank, lb%window_bounds, ierr)
        lb%lower_bound = bounds(1)
        lb%upper_bound = bounds(2)
        lower_bound = lb%lower_bound
        upper_bound = min(lower_bound + lb%max_chunk_size - 1, lb%upper_bound)
        bounds = [ upper_bound + 1, lb%upper_bound ]
#ifdef DEBUG_RANGES
        print '(A,I0,A,I0,A,I0)', 'Thr ', lb%rank, ' computes range from ', lower_bound, ' to ', upper_bound
#endif
        !> update lower bound
        call MPI_Accumulate(bounds, 2_MPI_INTEGER_KIND, MPI_INTEGER8, lb%rank, 0_MPI_ADDRESS_KIND, 2_MPI_INTEGER_KIND, MPI_INTEGER8, MPI_REPLACE, lb%window_bounds, ierr)
        call MPI_Win_unlock(lb%rank, lb%window_bounds, ierr)
        !> if it the last block, give that information in advance
        if (upper_bound + 1 > lb%upper_bound .and. .not.lb%done) then
            call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, lb%root, 0_MPI_INTEGER_KIND, lb%window_num_active, ierr)
            call MPI_Fetch_and_op(-1_MPI_INTEGER_KIND, num_active, MPI_INTEGER4, lb%root, 0_MPI_ADDRESS_KIND, MPI_SUM, lb%window_num_active, ierr)
            call MPI_Win_unlock(lb%root, lb%window_num_active, ierr)
            lb%actual_rank = mod(lb%actual_rank + 1, lb%nprocs)
            lb%done = .true.
        end if
        if (lower_bound > upper_bound) to_compute = .false.
    end if

    !> switch to other ranks
    hop_count = 0
    nohop_count = 0
    do while (.not.to_compute)
        block
            integer(MPI_INTEGER_KIND) :: compute_rank
            !> get rank which computes lb%actual_rank
#ifdef DEBUG_LOCKS
            call system_clock(count=t1)
#endif
            call MPI_Win_lock(MPI_LOCK_SHARED, lb%actual_rank, MPI_MODE_NOCHECK, lb%window_actual_rank, ierr)
#ifdef DEBUG_LOCKS
            call system_clock(count=t2)
            print '(A,I0,A,I0)', 'lock0, MPI: ', lb%rank, ' delay: ', t2-t1
#endif
            call MPI_Get(compute_rank, 1_MPI_INTEGER_KIND, MPI_INTEGER4, lb%actual_rank, 0_MPI_ADDRESS_KIND, 1_MPI_INTEGER_KIND, MPI_INTEGER4, lb%window_actual_rank, ierr)
            call MPI_Win_unlock(lb%actual_rank, lb%window_actual_rank, ierr)
            !> check that compute_rank computes itself
            if (lb%actual_rank == compute_rank) then
                !> try to steal min_chunk_size jobs from compute_rank
#ifdef DEBUG_LOCKS
                call system_clock(count=t1)
#endif
                call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, compute_rank, 0_MPI_INTEGER_KIND, lb%window_bounds, ierr)
#ifdef DEBUG_LOCKS
                call system_clock(count=t2)
                print '(A,I0,A,I0)', 'lock1, MPI: ', lb%rank, ' delay: ', t2-t1
#endif
                call MPI_Get(bounds, 2_MPI_INTEGER_KIND, MPI_INTEGER8, compute_rank, 0_MPI_ADDRESS_KIND, 2_MPI_INTEGER_KIND, MPI_INTEGER8, lb%window_bounds, ierr)
                call MPI_Win_flush(compute_rank, lb%window_bounds, ierr)
                upper_bound = bounds(2)
                lower_bound = max(bounds(1), bounds(2) - lb%min_chunk_size + 1)
                bounds = [ bounds(1), lower_bound - 1 ]
                if (lower_bound <= upper_bound) to_compute = .true.
#ifdef DEBUG_RANGES
                if (to_compute) then
                    print '(A,I0,A,I0,A,I0,A,I0)', 'Thr ', lb%rank, ' steals range from ', lower_bound, ' to ', upper_bound, ' from ', compute_rank
                    print '(A,2I8)', 'new bounds: ', bounds
                end if
#endif
                if (to_compute) then
                    !> update upper bound
                    call MPI_Accumulate(bounds, 2_MPI_INTEGER_KIND, MPI_INTEGER8, compute_rank, 0_MPI_ADDRESS_KIND, 2_MPI_INTEGER_KIND, MPI_INTEGER8, MPI_REPLACE, lb%window_bounds, ierr)
                end if
                call MPI_Win_unlock(compute_rank, lb%window_bounds, ierr)
                if (to_compute) return
            else
                !> switch to lb%actual_rank of compute_rank
#ifdef DEBUG_RANGES
                print '(A,I0)', 'hop MPI: ', lb%rank
#endif
                lb%actual_rank = compute_rank
                if (lb%actual_rank == lb%rank) exit
                !> but... to avoid infinity cycle, sometimes we will check that there is some jobs
                hop_count = hop_count + 1
                block
                    real(4) :: randval
                    call random_number(randval)
                    ! log(Nprocs) <- how often to check cond (each 1, 2, 3, ..)
                    ! 1 / log(Nprocs) <- probability
                    if (randval > 1._4 / log(real(lb%nprocs, kind=4)) .and. hop_count < 20) cycle
                end block
            end if
#ifdef DEBUG_RANGES
            print '(A,I0)', 'no-hop MPI: ', lb%rank
#endif
            !> if could not steal job, check how many threads finished job
#ifdef DEBUG_LOCKS
            call system_clock(count=t1)
#endif
            call MPI_Win_lock(MPI_LOCK_SHARED, lb%root, 0_MPI_INTEGER_KIND, lb%window_num_active, ierr)
#ifdef DEBUG_LOCKS
            call system_clock(count=t2)
            print '(A,I0,A,I0)', 'lock2, MPI: ', lb%rank, ' delay: ', t2-t1
#endif
            call MPI_Get(num_active, 1_MPI_INTEGER_KIND, MPI_INTEGER4, lb%root, 0_MPI_ADDRESS_KIND, 1_MPI_INTEGER_KIND, MPI_INTEGER4, lb%window_num_active, ierr)
            call MPI_Win_unlock(lb%root, lb%window_num_active, ierr)
            if (num_active == 0) return
            lb%actual_rank = mod(lb%actual_rank + 1, lb%nprocs)
            hop_count = 0
            nohop_count = nohop_count + 1
            if (nohop_count > lb%nprocs) return
        end block
    end do
#endif

  end function get_range

  !>
  !> @brief work stealing load balancer destructor
  !>
  subroutine clean(lb)
    class(work_stealing_load_balancer_t), intent(inout) :: lb
    integer(MPI_INTEGER_KIND) :: ierr

#ifdef WITH_MPI
    if (lb%window_num_active /= MPI_WIN_NULL) then
      call MPI_Win_free(lb%window_num_active, ierr)
    end if
    if (lb%window_actual_rank /= MPI_WIN_NULL) then
      call MPI_Win_free(lb%window_actual_rank, ierr)
    end if
    if (lb%window_bounds /= MPI_WIN_NULL) then
      call MPI_Win_free(lb%window_bounds, ierr)
    end if
#endif
  end subroutine clean
end module work_stealing_load_balancer_m
