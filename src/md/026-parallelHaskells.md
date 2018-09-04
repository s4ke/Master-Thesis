## Introduction to *parallel* Haskells

\label{sec:parallelHaskells}
\label{sec:parEvalNIntro}

In Chapter \ref{sec:fuproHaskell}, we cited @Hughes:1990:WFP:119830.119832 saying that in functional
programming, the order of evaluation is irrelevant. In parallel programs this is not the case,
as at least some kind of structure of evaluation is required to have actual speedup in programs.
Now, one might think that the idea of us wanting side effects
(parallel evaluation is a side-effect)
and having to think about order of evaluation in a pure functional program seems a bit odd.
The fact of the matter is that functional programs
only aim to avoid *unnecessary* side-effects and in the case of parallelism it
is obvious that some amount of side-effects are required.
Also, parallel Haskells generally aim to encapsulate all the necessary and
complicated code in a way such that the room for code-breaking errors is almost impossible.
If some parallel evaluation code is written in a sub-optimal way, only the performance 
is affected, but not the result, which will always be tractable no matter the order of evaluation.
^[Some exceptions using unsafe and non-deterministic operations exist, though. These situations 
can however only be achieved if the programmer actively chooses to use these kinds of operations.]
In the following, we will take a look at how parallelism can be achieved in Haskell programs in general.

In its purest form, parallel computation (on functions) can be looked at
as the execution of some functions `[a -> b]` in parallel or 
`parEvalN :: [a -> b] -> [a] -> [b]`, as also Figure \ref{fig:parEvalN} 
symbolically shows. In this chapter, we will implement this non-Arrow version which
will later be adapted for usage in our Arrow-based parallel Haskell.

![Schematic illustration of `parEvalN`. A list of inputs is transformed by different functions in parallel.](src/img/parEvalN.pdf){#fig:parEvalN}

There exist several different parallel Haskells, already.
As stated in the Introduction,
this is the reason why we base our efforts on existing work which we wrap
as backends behind a common interface so that we do not re-implement
yet another parallel runtime. As described earlier, we here focus on
three of the most important ones -- GpH [@Trinder1996; @Trinder1998a, based on `par` and `pseq` \enquote{hints}],
the `Par` Monad [@par-monad; @Foltzer:2012:MPC:2398856.2364562, a Monad for deterministic parallelism] and
Eden [@eden; @Loogen2012, a parallel Haskell for distributed memory].

We will now go into some detail on these parallel Haskells, and also
give their respective implementations of the non-Arrow version of `parEvalN`.
Chapter \ref{sec:GpHIntro} covers GpH, while Chapters \ref{sec:ParIntro}
and Chapter \ref{sec:EdenIntro} explain the `Par` Monad and Eden, respectively. 

### Glasgow parallel Haskell -- GpH

\label{sec:GpHIntro}

GpH [@Marlow2009; @Trinder1998a] is one of the simplest ways to do parallel
processing that is found in standard GHC.^[The Multicore implementation of GpH is available on Hackage under \url{https://hackage.haskell.org/package/parallel-3.2.1.0}, compiler support is integrated in the stock GHC.]
Besides some basic primitives
^[`par :: a -> b -> b` to evaluate `a` and `b` in parallel and `pseq :: a -> b -> b`, a special
variant of `seq` that allows enforcing of parallel evaluation.],
it already ships with parallel
evaluation strategies for several types which can be applied with
`using :: a -> Strategy a -> a`. This is exactly what is required for an
implementation of `parEvalN`:

~~~~ {.haskell}
parEvalN :: (NFData b) => [a -> b] -> [a] -> [b]
parEvalN fs as = let bs = zipWith ($) fs as 
                 in bs `using` parList rdeepseq
~~~~

In the above definition of `parEvalN`, we just apply the list of functions `[a -> b]`
to the list of inputs `[a]` by zipping them with the application operator `$`.
We then evaluate this lazy list `[b]` according to a `Strategy [b]` with
`using :: a -> Strategy a -> a`.
We construct such a strategy with `parList :: Strategy a -> Strategy [a]` and
`rdeepseq :: NFData a => Strategy a`, where the latter is a strategy
which evaluates to normal form. Other strategies like e.g. evaluation
to weak head normal form are available as well.
GpH also allows for custom `Strategy` implementations to be used.
Figure \ref{fig:parEvalNMulticoreImg} shows a visual representation of this code.

![`parEvalN` (GpH).](src/img/parEvalNMulticoreImg.pdf){#fig:parEvalNMulticoreImg}

### `Par` Monad

\label{sec:ParIntro}

The `Par` Monad^[The `Par` Monad can be found in the `monad-par` package on Hackage
under \url{https://hackage.haskell.org/package/monad-par-0.3.4.8/}.],
introduced by [@par-monad], is a Monad designed for the composition of
parallel programs:

~~~~{.haskell}
myComp :: Par (a, b)
myComp = do
     fx <- spawn $ return (f x)  -- start evaluating (f x)
     gx <- spawnP $ return (g x)  -- start evaluating (g x)
     a  <- get fx        -- wait for fx
     b  <- get gx        -- wait for gx
     return (a,b)        -- return results
~~~~

We, however, do not need its composition features and only use
its parallel backend in our definition of `parEvalN`:

~~~~ {.haskell}
parEvalN :: (NFData b) => [a -> b] -> [a] -> [b]
parEvalN fs as = runPar $ 
	(sequenceA (map (return . spawn) (zipWith ($) fs as))) >>= mapM get
~~~~

This `Par` Monad version of our parallel evaluation function `parEvalN` is
defined by zipping the list of `[a -> b]` with the list of inputs `[a]` with the
application operator `$` just like with GpH.
Then, we map over this not yet evaluated lazy list of results `[b]` with
`spawn :: NFData a => Par a -> Par (IVar a)` to transform them to a list
of not yet evaluated forked away computations `[Par (IVar b)]`,
which we convert to `Par [IVar b]` with `sequenceA`.
Next, we wait for the computations to finish by mapping over the `IVar b`
values inside the `Par` Monad with `get`, which results in `Par [b]`.
We execute this process with `runPar` to finally get the fully
evaluated list of results`[b]`.
While we used `spawn` in the definition above, a head-strict variant
can easily be defined by replacing `spawn` with `spawn_ :: Par a -> Par (IVar a)`.
Figure \ref{fig:parEvalNParMonadImg} shows a graphical representation.

![`parEvalN` (`Par` Monad).](src/img/parEvalNParMonadImg.pdf){#fig:parEvalNParMonadImg}

### Eden

\label{sec:EdenIntro}

Eden [@eden; @Loogen2012] is a parallel Haskell for distributed memory
and allows for MPI and PVM as
distributed backends.^[The projects homepage can be found at \url{http://www.mathematik.uni-marburg.de/~eden/}. The Hackage page is at \url{https://hackage.haskell.org/package/edenmodules-1.2.0.0/}.]
It is targeted towards clusters, but also works well in a shared-memory
setting with a further simple backend. However, in contrast to many other
parallel Haskells, in Eden each process has its own heap. This seems to
be a waste of memory, but with the distributed programming paradigm and
individual GC per process, Eden yields good performance results on multicores,
as well [@arcs-dc; @aswad2009low].

While Eden comes with a Monad `PA` for parallel evaluation, it also ships
with a completely functional interface that includes
a `spawnF :: (Trans a, Trans b) => [a -> b] -> [a] -> [b]`^[The type class `Trans`
stands for Transmissible.
The Eden library already has instances for most common types and allows for easy construction
of further instances.]
function that allows us to define `parEvalN` directly:

~~~~ {.haskell}
parEvalN :: (Trans a, Trans b) => [a -> b] -> [a] -> [b]
parEvalN = spawnF 
~~~~

#### Eden TraceViewer

\label{sec:edentv}

To comprehend the efficiency -- or the lack thereof, in a parallel program,
an inspection of its execution is extremely helpful. While some large-scale
solutions exist [@Geimer2010], the parallel Haskell community mainly utilises
the tools Threadscope [@Wheeler2009] and Eden TraceViewer
^[See \url{http://hackage.haskell.org/package/edentv} on Hackage for
the last available version of Eden TraceViewer.] [@Berthold2007a].
In the next chapters, we will present some \emph{trace visualisations},
the post-mortem process diagrams of Eden processes and their activity.

The trace visualisations are colour-coded.
In such a visualisation (Figure \ref{fig:withoutFutures}),
the `x` axis shows the time, the `y` axis enumerates the machines and processes.
The visualisation shows a running process in green, a blocked process is red.
If the process is \enquote{runnable}, i.e. it may run, but does not,
it is yellow. The typical reason for this is garbage collection (GC).
An inactive machine, where no processes are started yet,
or all are already terminated, shows as a blue bar.
A communication from one process to another is represented with a black arrow.
A stream of communications, e.g. a transmitted list is shows as a dark shading
between sender and receiver processes.
