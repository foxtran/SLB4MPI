#include <SLB4MPI.hpp>

#include <iostream>

int main() {
#ifdef WITH_MPI
  MPI_Init(nullptr, nullptr);
#else
  const int MPI_COMM_WORLD = 0;
#endif
  {
    int64_t bot, top;
    SLB4MPI::RuntimeLoadBalancer lb("env", MPI_COMM_WORLD, 1, 100, 2, 4);
    while (lb.get_range(bot, top)) {
      std::cout << "Range [ " << bot << ", " << top << " ]" << std::endl;
      for (int64_t i = bot; i <= top; i++) {
        // do stuff
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
