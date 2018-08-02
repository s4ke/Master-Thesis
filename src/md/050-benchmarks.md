# Performance results and discussion

\label{sec:benchmarks}

The preceding sections have shown that PArrows are expressive.
This section evaluates the performance overhead of this compositional abstraction
in comparison to  GpH and the `Par` Monad on shared memory architectures and Eden on
a distributed memory cluster.
We describe our measurement platform, the benchmark results -- the shared-memory
variants (GpH, `Par` Monad and Eden CP) followed by Eden in a distributed-memory
setting, and conclude that PArrows hold up in terms of performance when compared
to the original parallel Haskells.

## Measurement platform

We start by explaining the hardware and software stack and outline the
benchmark programs and motivation for choosing them. We also shortly address
hyper-threading and why we do not use it in our benchmarks.

### Hardware and software

The benchmarks are executed  both in a shared and in a distributed memory
setting using the Glasgow GPG Beowulf cluster, consisting of
16 machines with 2 Intel® Xeon® E5-2640 v2 and 64 GB of DDR3 RAM each.
Each processor has 8 cores and 16 (hyper-threaded) threads with a base
frequency of 2 GHz and a turbo frequency of 2.50 GHz. This results in a total
of 256 cores and 512 threads for the whole cluster. The operating system was
Ubuntu 14.04 LTS with Kernel 3.19.0-33. Non-surprisingly, we found that
hyper-threaded 32 cores do not behave in the same manner as real 16 cores
(numbers here for a single machine). We disregarded the hyper-threading
ability in most of the cases.

Apart from Eden, all benchmarks and libraries were compiled with
Stack's^[see \url{https://www.haskellstack.org/}]
lts-7.1 GHC compiler which is equivalent to a standard GHC 8.0.1 with
the base package in version 4.9.0.0. Stack itself was used in version 1.3.2.
For GpH in its Multicore variant we used the parallel package in version
3.2.1.0^[see \url{https://hackage.haskell.org/package/parallel-3.2.1.0}],
while for the `Par` Monad we used monad-par in version
0.3.4.8^[see \url{https://hackage.haskell.org/package/monad-par-0.3.4.8}].
For all Eden tests, we used its GHC-Eden compiler in version
7.8.2^[see \url{http://www.mathematik.uni-marburg.de/~eden/?content=build_eden_7_&navi=build}]
together with OpenMPI 1.6.5^[see \url{https://www.open-mpi.org/software/ompi/v1.6/}].

Furthermore, all benchmarks were done with help of the
bench^[see \url{https://hackage.haskell.org/package/bench}] tool in version
1.0.2 which uses criterion
(>=1.1.1.0 \&\& < 1.2)^[see \url{https://hackage.haskell.org/package/criterion-1.1.1.0}]
internally. All runtime data (mean runtime, max stddev, etc.) was collected with
this tool.

We used a single node with 16 real cores as a shared memory test-bed
and the whole grid with 256 real cores as a device to test our
distributed memory software.

### Benchmarks

We measure four benchmarks from different
sources. Most of them are parallel mathematical computations, initially
implemented in Eden. Table @tbl:benches summarises.

| Name               |     Area    |          Type         |    Origin   | Source                      |
|--------------------|:-----------:|:---------------------:|:-----------:|-----------------------------|
| Rabin--Miller test | Mathematics | `parMap` + `reduce`   | Eden        | @Lobachev2012               |
| Jacobi sum test    | Mathematics | `workpool` + `reduce` | Eden        | @Lobachev2012               |
| Gentleman          | Mathematics | `torus`               | Eden        | @Eden:SkeletonBookChapter02 |
| Sudoku             | Puzzle      | `parMap`              | `Par` Monad | @par-monad[^tblnote1]       |

Table: The benchmarks we use in this paper. {#tbl:benches}

[^tblnote1]: actual code from: \url{http://community.haskell.org/\~simonmar/par-tutorial.pdf} and \url{https://github.com/simonmar/parconc-examples}}

Rabin--Miller test is a probabilistic primality test that iterates multiple
(here: 32--256) \enquote{subtests}. Should a subtest fail, the input is
definitely not a prime. If all $n$ subtest pass, the input is composite with the
probability of $1/4^{n}$.

Jacobi sum test or APRCL is also a primality test, that however,
guarantees the correctness of the result. It is probabilistic in the
sense that its run time is not certain. Unlike Rabin--Miller test, the subtests
of Jacobi sum test have very different durations. @lobachev-phd
discusses some optimisations of parallel APRCL. Generic parallel
implementations of Rabin--Miller test and APRCL were presented in @Lobachev2012.

\enquote{Gentleman} is a standard Eden test program, developed
for their `torus` skeleton. It implements a Gentleman's algorithm for parallel matrix
multiplication [@Gentleman1978]. We ported an Eden-based version
[@Eden:SkeletonBookChapter02] to PArrows.

A parallel Sudoku solver was used by @par-monad to compare `Par` Monad
to GpH, we ported it to PArrows.

### What parallel Haskells run where

The `Par` monad and GpH -- in its multicore version [@Marlow2009] -- 
can be executed on shared memory machines only.
Although GpH is available on distributed memory
clusters, and newer distributed memory Haskells such as HdpH exist,
current support of distributed memory in PArrows is limited to
Eden. We used the MPI backend of Eden in a distributed memory
setting. However, for shared memory Eden features a \enquote{CP} backend
that merely copies the memory blocks between disjoint heaps. In
this mode, Eden still operates in the \enquote{nothing shared} setting, but
is adapted better to multicore machines. We call this version of Eden
\enquote{Eden CP}.

## Benchmark results

\label{sec:benchmarkResults}

We compare the PArrow performance with direct implementations of the
benchmarks in Eden, GpH and the `Par` Monad.
We start with the definition of mean overhead to compare both
PArrows-enabled and standard benchmark implementations. We continue comparing
speedups and overheads for the shared memory implementations and then study
OpenMPI variants of the Eden-enabled PArrows as a representative of a
distributed memory backend. We plot all speedup curves and all overhead values
in the supplementary materials.

### Defining overhead

We compare the mean overhead, i.e. the mean of relative wall-clock run time
differences between the PArrow and direct benchmark implementations executed
multiple times with the same settings.
The error margins of the time measurements, supplied by criterion
package^[\url{https://hackage.haskell.org/package/criterion-1.1.1.0}],
yield the error margin of the mean overhead. 

Quite often the zero value lies in the error margin of the mean overhead.
This means that even though we have measured some difference
(against or even in favour of PArrows), it could be merely the error margin
of the measurement and the difference might not be existent. We are mostly
interested in the cases where above issue does not persist, we call
them *significant*. We often denote the error margin with $\pm$ after
the mean overhead value.

### Shared memory

#### Speedup

The Rabin--Miller benchmark showed almost linear speedup for both 32 and 64 tasks,
the performance is slightly better in the latter case: 13.7 at 16 cores for input
$2^{11213}-1$ and 64 tasks in the best case scenario with Eden CP. The
performance of the Sudoku benchmark merely reaches a speedup of 9.19 (GpH),
8.78 (`Par` Monad), 8.14 (Eden CP) for 16 cores and 1000 Sudokus. In contrast to
Rabin--Miller, here the `GpH` seems to be the best of all, while Rabin--Miller
profited most from Eden CP (i.e. Eden with direct memory copy) implementation of
PArrows. Gentleman on shared memory has a plummeting speedup curve with GpH and
`Par` Monad and logarithmically increasing speedup for the Eden-based version.
The latter reached a speedup of 6.56 at 16 cores.

#### Overhead

For the shared memory Rabin--Miller benchmark, implemented with PArrows using
Eden CP, GpH, and `Par` Monad, the overhead values are within single percents
range, but also negative overhead (i.e. PArrows are better) and larger error
margins happen. To give a few examples, the overhead for Eden CP
with input value $2^{11213}-1$, 32 tasks, and 16 cores is $1.5\%$, but the
error margin is around $5.2\%$! Same implementation in the same setting with
64 tasks reaches $-0.2\%$ overhead, PArrows apparently fare better than Eden --
but the error margin of $1.9\%$ disallows this interpretation.
We focus now on significant overhead values.
To name a few: $0.41\%\; \pm 7\cdot 10^{-2}\%$ for Eden~CP and 64 tasks at 4 cores;
$4.7\% \; \pm 0.72\%$ for GpH, 32 tasks, 8 cores; $0.34\% \; \pm 0.31\%$ for `Par`
Monad at 4 cores with 64 tasks. The worst significant overhead was in case of GpH 
with $8\% \; \pm 6.9\%$ at 16 cores with 32 tasks and input value $2^{11213}-1$.
In other words, we notice no major slow-down through PArrows here.

For Sudoku the situation is slightly different.
There is a minimal significant ($-1.4\% \; \pm 1.2\%$ at 8 cores) speed
*improvement* with PArrows Eden CP version when compared with the base Eden CP
benchmark. However, with increasing number of cores the error margin reaches
zero again: $-1.6\% \; \pm 5.0\%$ at 16 cores. The `Par` Monad shows a similar
development, e.g. with $-1.95\% \; \pm 0.64\%$ at 8 cores. The GpH version shows
both a significant speed improvement of $-4.2\% \; \pm 0.26\%$ (for 16 cores) with
PArrows and a minor overhead of $0.87\% \; \pm 0.70\%$ (4 cores).

The Gentleman multiplication with Eden CP shows a minor significant overhead of
$2.6\% \; \pm 1.0\%$ at 8 cores and an insignificant improvement at 16 cores.
Summarising, we observe a low (if significant at all) overhead,
induced by PArrows in the shared memory setting.

### Distributed memory

#### Speedup

The speedup of distributed memory Rabin--Miller benchmark with PArrows and Eden
showed an almost linear speedup excepting around 192 cores where an unfortunate
task distribution reduces performance.
As seen in Fig. \ref{fig:rabinMillerDistSpeedup}, we reached a speedup of 213.4
with PArrrows at 256 cores (vs. 207.7 with pure Eden). Because of memory limitations,
the speedup of Jacobi sum test for large inputs (such as $2^{4253}-1$) could be
measured only in a massively distributed setting. PArrows improved there
from $9193s$ (at 128 cores) to $1649s$ (at 256 cores).
A scaled-down version with input $2^{3217}-1$ stagnates the speedup at
about 11 for both PArrows and Eden for more than 64 cores.
There is apparently not enough work for that many cores.
The Gentleman test with input 4096 had an almost linear speedup first,
then plummeted between 128 and 224 cores, and recovered at 256 cores with speedup
of 129.

![Speedup of the distributed Rabin--Miller benchmark using PArrows with Eden.](src/img/rabinMillerDistSpeedup.pdf){#fig:rabinMillerDistSpeedup}

#### Overhead

We use our mean overhead quality measure and the notion of significance
also for distributed memory benchmarks. The mean overhead of Rabin--Miller test
in the distributed memory setting ranges from $0.29\%$ to $-2.8\%$
(last value in favour of PArrows), but these values are not
significant with error margins $\pm 0.8\%$ and $\pm 2.9\%$ correspondingly.
A sole significant (by a very low margin) overhead is $0.35\% \; \pm 0.33\%$ at 64 cores.
We measured the mean overhead for Jacobi benchmark for an input of $2^{3217}-1$ for
up to 256 cores. We reach the flattering value $-3.8\% \; \pm 0.93\%$ at 16 cores
in favour of PArrows, it was the sole significant overhead value. The value for 256
cores was $0.31\% \; \pm 0.39\%$.
Mean overhead for distributed Gentleman multiplication was also low.
Significant values include $1.23\% \; \pm 1.20\%$ at 64 cores and $2.4\% \; \pm 0.97\%$
at 256 cores. It took PArrows 64.2 seconds at 256 cores to complete the benchmark.

Similar to the shared memory setting, PArrows only imply a very low penalty with
distributed memory that lies in lower single-percent digits at most.

## Discussion

| ![](src/img/bestAndWorstBenchmarks1.pdf) |
|------------------------------------------|

Table: Overhead in the shared memory benchmarks. Bold marks values in favour of PArrows. {#tbl:meanOverheadSharedMemory}

| ![](src/img/bestAndWorstBenchmarks2.pdf) |
|------------------------------------------|

Table: Overhead in the distributed memory benchmarks. Bold marks values in favour of PArrows. {#tbl:meanOverHeadDistributedMemory}

PArrows performed in our benchmarks with little to no overhead.
Tables \ref{tbl:meanOverheadSharedMemory} and \ref{tbl:meanOverHeadDistributedMemory}
clarify this once more: The PArrows-enabled versions trade blows with their vanilla
counterparts when comparing the means over all cores of the mean overheads.
If we combine these findings with the usability of our DSL,
the minor overhead induced by PArrows is outweighed by their convenience and
usefulness to the user.

PArrows are an extendable formalism, they can be easily ported to further
parallel Haskells while still maintaining interchangeability.
It is straightforward to provide further implementations of algorithmic skeletons
in PArrows.
