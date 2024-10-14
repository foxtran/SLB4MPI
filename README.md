# MPI-lb

OpenMP-like load balancers for MPI


## Compilation process

Manual compilation only in the following sequence:
```text
abstract_load_balancer.f90 dynamic_load_balancer.f90 guided_load_balancer.f90 local_static_load_balancer.f90 static_load_balancer.f90 work_stealing_load_balancer.f90 runtime_load_balancer.f90 MPI_load_balancers.f90
```
Library requires C preprocessing of Fortran files as well as support of lines up to 200 characters.
For GFortran, the additional flags are `-cpp -ffree-line-length-none`.
To enable MPI support, use `-DWITH_MPI`.

**NOTE:** no ILP64 support.


## API

### Fortran

All load balancers are available with `MPI_load_balancers` which provides `<load-balancer-type>_load_balancer_t` types.
Each `<load-balancer-type>_load_balancer_t` implements abstract interface defined in [abstract_load_balancer.f90](src/Fortran/abstract_load_balancer.f90).
See possible `load-balancer-type`s in the next section.

Type definition can be done in the following manner, for example:
```fortran
use MPI_load_balancers
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

For `runtime` load balancer, default load balancer can be changed with `LBMPI_set_schedule` call in the following way before its initialization:
```fortran
call LBMPI_set_schedule("guided")
```


## Supported balancers

- `static`
- `local_static`
- `dynamic`
- `guided`
- `work_stealing`
- `runtime`

### `static`

Iterations are divided into chunks of size `max_chunk_size`, and the chunks are assigned to the MPI ranks in the communicator in a round-robin fashion in the order of the MPI thread rank.
Each chunk contains `max_chunk_size` iterations, except for the chunk that contains the sequentially last iteration, which may have fewer iterations.
If `min_chunk_size` is not specified, it has value `1`.
If `max_chunk_size` is not specified, the iteration range is divided into chunks that are approximately equal in size, but no less than `min_chunk_size`.

### `local_static`

### `dynamic`

### `guided`

### `work_stealing`

### `runtime`


## Notes about MPI implementations

### OpenMPI

- `clean` call is synchonization point.

### IntelMPI

- Load balancers `dynamic`, `guided`, `work_stealing` as well as `runtime` may have significant performance issues.
- `MPI_Accumulate` is used instead of `MPI_Put`
- `I_MPI_ASYNC_PROGRESS=1` leads to runtime fails.

### MPICH

Not tested yet.
