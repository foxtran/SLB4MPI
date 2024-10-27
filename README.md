# SLB4MPI: Simple load balancers for MPI

A collection of simple load balancers for MPI made in OpenMP manner.


## API

### С++

All load balancers are available via `SLB4MPI.hpp` header which provides `<Load-Balancer-Type>LoadBalancer` types.
Each `<Load-Balancer-Type>LoadBalancer` implements abstract interface defined in [abstract_load_balancer.hpp](src/CXX/SLB4MPI/SLB4MPI/abstract_load_balancer.hpp).
See possible `Load-Balancer-Type`s in the next section.

Initialization can be done in the following manner, for example (`StaticLoadBalancer` will be used):
```cpp
#include <SLB4MPI.h>
...
StaticLoadBalancer slb(MPI_COMM_WORLD, 1, 100, 2, 4);
```
That will initialize load balancer `slb`  with range from lower bound (`1`) to upper bound (`100`) and with min (`2`) and max (`4`) chunk sizes which are optional which will work on communicator `comm`.
Simpler initialization is also possible with default min and max chuck sizes:
```cpp
StaticLoadBalancer slb(MPI_COMM_WORLD, 1, 100);
```
This is a barrier for the whole communicator.

Then, one can ask range for computing with `get_range` method which returns range's start and end by arguments, and the method returns logical value that signals is there something to compute.
So, the pattern of usage this routine is:
```cpp
if (slb.get_range(range_start, range_end)) { // returns [ range_start, range_end ]
...
}
```
Single call may return only a part of whole range, so, proper call should be inside of `while`-loop.

After finishing loop, load balancer `slb` must be destroyed.
Internally, it uses RAII, so it most of cases you do not think about destructing it.

For `Runtime` load balancer, a proper load balancer must be specified via first argument during initialization:
```cpp
std::string rlbtype = std::string("dynamic");
RuntimeLoadBalancer rlb(rlbtype, MPI_COMM_WORLD, 1, 100, 2, 4);
```
See possible values in the next section.

### Fortran

All load balancers are available in `SLB4MPI` module which provides `<load-balancer-type>_load_balancer_t` types.
Each `<load-balancer-type>_load_balancer_t` implements abstract interface defined in [abstract_load_balancer.f90](src/Fortran/abstract_load_balancer.f90).
See possible `load-balancer-type`s in the next section.

Type definition can be done in the following manner, for example:
```fortran
use SLB4MPI
...
type(static_load_balancer_t) :: lb
```
Here, static load balancer will be used.

Now, before usage load balancer `lb`, one must initialize the load balancer `lb` with range from lower bound to upper bound and with min and max chunk sizes which are optional which will work on communicator `comm`.
For range from 1 to 1000 with min chunk size equals 6 and max chunk size equals 12, the initialization will look as follows:
```fortran
call lb%initialize(comm, 1_8, 1000_8, 6_8, 12_8)
```
The simpler call is also possible, where min and max chunk sizes will be determined automatically:
```fortran
call lb%initialize(comm, 1_8, 1000_8)
```
This call is a barrier for the whole communicator.

Then, one can ask range for computing with `get_range` call which returns range's start and end by arguments, and the call returns logical value that signals is there something to compute.
So, the pattern of usage this routine is:
```fortran
if (lb%get_range(range_start, range_end)) then
...
endif
```
Single call may return only a part of whole range, so, proper call should be inside of `do`-loop.

After finishing loop, load balancer `lb` must be destroyed before next usage via `clear` call:
```fortran
call lb%clean()
```

For `runtime` load balancer, default load balancer can be changed with `SLB4MPI_set_schedule` call in the following way before its initialization:
```fortran
call SLB4MPI_set_schedule("guided")
```


## Supported balancers

- `static`
- `local_static`
- `dynamic`
- `guided`
- `work_stealing`
- `runtime`

Typenames of different balancers in diffent languages:

| load balancer   | Typename in Fortran             | Typename in C++            |
|-----------------|:--------------------------------|:---------------------------|
| `static`        | `static_load_balancer_t`        | `StaticLoadBalancer`       |
| `local_static`  | `local_static_load_balancer_t`  | `LocalStaticLoadBalancer`  |
| `dynamic`       | `dynamic_load_balancer_t`       | `DynamicLoadBalancer`      |
| `guided`        | `guided_load_balancer_t`        | `GuidedLoadBalancer`       |
| `work_stealing` | `work_stealing_load_balancer_t` | `WorkStealingLoadBalancer` |
| `runtime`       | `runtime_load_balancer_t`       | `RuntimeLoadBalancer`      |

### `static`

Iterations are divided into chunks of size `max_chunk_size`, and the chunks are assigned to the MPI ranks in the communicator in a round-robin fashion in the order of the MPI rank.
Each chunk contains `max_chunk_size` iterations, except for the chunk that contains the sequentially last iteration, which may have fewer iterations.
If `min_chunk_size` is not specified, it defaults to `1`.
If `max_chunk_size` is not specified, the iteration range is divided into chunks that are approximately equal in size, but no less than `min_chunk_size`.

### `local_static`

Iterations are divided into chunks of size `max_chunk_size`, and the chunks are assigned to the MPI ranks in the communicator in a continuous fashion in the order of the MPI rank.
Each chunk contains `max_chunk_size` iterations, except for the chunk that contains the sequentially last iteration, which may have fewer iterations.
If `min_chunk_size` is not specified, it defaults to `1`.
If `max_chunk_size` is not specified, the iteration range is divided into chunks that are approximately equal in size, but no less than `min_chunk_size`.

### `dynamic`

Each MPI rank executes a chunk, then requests another chunk, until no chunks remain to be assigned.
Each chunk contains `min_chunk_size` iterations, except for the chunk that contains the sequentially last iteration, which may have fewer iterations.
If `min_chunk_size` is not specified, it defaults to `1`.

### `guided`

Each MPI rank executes a chunk, then requests another chunk, until no chunks remain to be assigned.
For a `min_chunk_size` of `1`, the size of each chunk is proportional to the number of unassigned iterations divided by the number of threads in the team, decreasing to `1` but no more than `max_chunk_size`  iterations.
For a `min_chunk_size` with value `k` > `1`, the size of each chunk is determined in the same way, with the restriction that the chunks do not contain fewer than `k` iterations but no more than `max_chunk_size` iterations
  (except for the chunk that contains the sequentially last iteration, which may have fewer than `k` iterations).
If `min_chunk_size` is not specified, it defaults to `1`.
If `max_chunk_size` is not specified, the value is determined as average number of elements per MPI rank, but no less than `min_chunk_size`.

### `work_stealing`

Iterations are divided into chunks of size `max_chunk_size`, and the chunks are assigned to the MPI ranks in the communicator in a continuous fashion in the order of the MPI rank.
Each chunk contains `max_chunk_size` iterations, except for the chunk that contains the sequentially last iteration, which may have fewer iterations.
If MPI rank finished its chunks, it starts to steal tasks from other MPI ranks in round-robin fashion in the order of MPI ranks.
These chunks contains `min_chunk_size` iterations, except for the chunk that contains the sequentially last iteration, which may have fewer iterations for victim MPI rank.
If `min_chunk_size` is not specified, it defaults to `1`.
If `max_chunk_size` is not specified, the iteration range is divided into chunks that are approximately equal in size, but no less than `min_chunk_size`.

### `runtime`

The load balancer is determing by `SLB4MPI_set_schedule` procedure.
The list of possible values passed as string:
- `env` (default) selects slice algorithm by `SLB4MPI_LOAD_BALANCER` environment variable;
- `static` selects `static_load_balancer`;
- `local_static` selects `local_static_load_balancer`;
- `dynamic` selects `dynamic_load_balancer`;
- `guided` selects `guided_load_balancer`;
- `work_stealing` selects `work_stealing_load_balancer`.

`SLB4MPI_LOAD_BALANCER` environment variable accepts the following values:
- not set: runtime load balancer will use `static_load_balancer`
- empty string: runtime load balancer will use `static_load_balancer`
- `static`: runtime load balancer will use `static_load_balancer`
- `local_static`: runtime load balancer will use `local_static_load_balancer`
- `dynamic`: runtime load balancer will use `dynamic_load_balancer`
- `guided`: runtime load balancer will use `guided_load_balancer`
- `work_stealing`: runtime load balancer will use `work_stealing_load_balancer`
- other values: runtime load balancer will use `static_load_balancer`


## Compilation process

The collection is assumed to be compiled in source tree of parent project with passing all flags from it.
Additional flags, which are required for compiling of collection, are kept inside.
Currently, only [CMake build system](https://cmake.org) is supported.

To use SLB4MPI from your project, add the following lines into your `CMakeLists.txt` for fetching the library:
```cmake
include(FetchContent)
FetchContent_Declare(SLB4MPI
    GIT_REPOSITORY https://github.com/foxtran/SLB4MPI.git
    GIT_TAG        0.0.1
)
FetchContent_MakeAvailable(SLB4MPI)
FetchContent_GetProperties(SLB4MPI SOURCE_DIR SLB4MPI_SOURCE_DIR)

set(SLB4MPI_ENABLE_CXX ON) # or OFF
set(SLB4MPI_ENABLE_Fortran ON) # or OFF
set(SLB4MPI_WITH_MPI ON) # or OFF

add_subdirectory(${SLB4MPI_SOURCE_DIR} ${SLB4MPI_SOURCE_DIR}-binary)
```

After this, you can link the library with your Fortran application or library (`SLB4MPI_ENABLE_Fortran` must be `ON`):
```cmake
target_link_libraries(<TARGET> PUBLIC SLB4MPI::SLB4MPI_Fortran)
```
or with C++ application or library (`SLB4MPI_ENABLE_CXX` must be `ON`):
```cmake
target_link_libraries(<TARGET> PUBLIC SLB4MPI::SLB4MPI_CXX)
```


Useful flags that changes behaviour of library:
- `SLB4MPI_ENABLE_CXX` enables/disables MPI load balancers for C++ language (requires C++14)
- `SLB4MPI_ENABLE_Fortran` enables/disables MPI load balancers for Fortran language (requires Fortran 2003)
- `SLB4MPI_WITH_MPI` enables/disables support of MPI for MPI/non-MPI builds

See example for [Fortran](examples/Fortran/CMakeLists.txt) and [C++](examples/CXX/CMakeLists.txt).


**NOTE:** The library does not provide ILP64 support.


## Notes about MPI implementations

### OpenMPI

- In Fortran, `clean` call is synchonization point. In C++, it is an end of scope containing load balancer(s).

### IntelMPI

- Load balancers `dynamic`, `guided`, `work_stealing` as well as `runtime` may have significant performance issues. [Report on Intel Forum](https://community.intel.com/t5/Intel-MPI-Library/MPI-Win-lock-extremely-long-time-for-waiting/m-p/1637153/highlight/true#M11939)
- `MPI_Accumulate` is used instead of `MPI_Put`. [Report on Intel Forum](https://community.intel.com/t5/Intel-MPI-Library/MPI-Put-RDMA-WRITE-error-which-MPI-Accumulate-works-fine/m-p/1637148/highlight/true#M11938)
- `I_MPI_ASYNC_PROGRESS=1` leads to runtime fails. [Report on Intel Forum](https://community.intel.com/t5/Intel-MPI-Library/MPI-Win-lock-extremely-long-time-for-waiting/m-p/1637153/highlight/true#M11939)
- **NOTE:** no ILP64 support.

### MPICH

Not tested yet.
