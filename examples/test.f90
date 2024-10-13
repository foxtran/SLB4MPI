program main
  use MPI
  use MPI_load_balancers
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
  call LBMPI_set_schedule("work_stealing")
  call lb%initialize(MPI_COMM_WORLD, 1_8, 1000_8, 4_8, 8_8)
  do
    if (.not.lb%get_range(bot, top)) exit
    do j = bot, top
    block
      real(8), allocatable :: a(:,:), b(:,:), c(:,:)
      i = 3 * (1000 - j) + 600
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
