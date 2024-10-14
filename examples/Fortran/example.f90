program main
#ifdef WITH_MPI
  use MPI
#endif
  use SLB4MPI
  implicit none
  type(runtime_load_balancer_t) :: lb
  integer(8) :: bot, top, i, j
  integer(4) :: mpierr
#ifndef WITH_MPI
  integer(4), parameter :: MPI_COMM_WORLD = 0
#endif

#ifdef WITH_MPI
  call MPI_init(mpierr)
#endif
  call LBMPI_set_schedule("env")
  call lb%initialize(MPI_COMM_WORLD, 1_8, 100_8, 2_8, 4_8)
  do
    if (.not.lb%get_range(bot, top)) exit
    do j = bot, top
    block
      real(8), allocatable :: a(:,:), b(:,:), c(:,:)
      i = 3 * (100 - j) + 200
      allocate(a(i,i),b(i,i),c(i,i))
      call random_number(a)
      call random_number(b)
      c = matmul(a,b)
      write(7,'(I6,ES15.8)') j, sum(c)
      deallocate(a,b,c)
    end block
    end do
  end do
  print '(A)', "Done!"
  call lb%clean()
#ifdef WITH_MPI
  call MPI_Finalize(mpierr)
#endif
end program main
