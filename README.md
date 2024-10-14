# MPI-lb

A collection of simple load balancers for MPI made in OpenMP manner.


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

The load balancer is determing by `LBMPI_set_schedule` procedure.
The list of possible values passed as string:
- `env` (default) selects slice algorithm by `MPIlb_LOAD_BALANCER` environment variable;
- `static` selects `static_load_balancer`;
- `local_static` selects `local_static_load_balancer`;
- `dynamic` selects `dynamic_load_balancer`;
- `guided` selects `guided_load_balancer`;
- `work_stealing` selects `work_stealing_load_balancer`.

`MPIlb_LOAD_BALANCER` environment variable accepts the following values:
- not set: runtime load balancer will use `static_load_balancer`
- empty string: runtime load balancer will use `static_load_balancer`
- `static`: runtime load balancer will use `static_load_balancer`
- `local_static`: runtime load balancer will use `local_static_load_balancer`
- `dynamic`: runtime load balancer will use `dynamic_load_balancer`
- `guided`: runtime load balancer will use `guided_load_balancer`
- `work_stealing`: runtime load balancer will use `work_stealling_load_balancer`
- other values: runtime load balancer will use `static_load_balancer`

## Notes about MPI implementations

### OpenMPI

- `clean` call is synchonization point.

### IntelMPI

- Load balancers `dynamic`, `guided`, `work_stealing` as well as `runtime` may have significant performance issues.
- `MPI_Accumulate` is used instead of `MPI_Put`
- `I_MPI_ASYNC_PROGRESS=1` leads to runtime fails.

### MPICH

Not tested yet.
