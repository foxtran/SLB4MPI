#include <SLB4MPI.hpp>

#include <iostream>
#include <thread>

int main() {
  using namespace std::chrono_literals;
  int mpi_rank = 0;
#ifdef WITH_MPI
  MPI_Init(nullptr, nullptr);
  MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#else
  const int MPI_COMM_WORLD = 0;
#endif
  {
    int64_t bot, top;
    SLB4MPI::RuntimeLoadBalancer lb("env", MPI_COMM_WORLD, 1, 100, 2, 4);
    while (lb.get_range(bot, top)) {
      std::cout << "Range [ " << bot << ", " << top << " ] @ " << mpi_rank << std::endl;
      for (int64_t i = bot; i <= top; i++) {
        // do stuff
        std::this_thread::sleep_for(i * 2ms);
      }
    }
    std::cout << "Done!" << std::endl;
    // call descructor of RuntimeLoadBalancer here
  }
#ifdef WITH_MPI
  MPI_Finalize();
#endif
  return 0;
}
