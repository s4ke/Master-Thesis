## Futures

\label{sec:futures}

Consider the following outline parallel Arrow combinator:

~~~~ {.haskell}
outlineCombinator :: (
    ArrowParallel arr a b (),
	ArrowParallel arr b c ()) =>
	[arr a b] -> [arr b c] -> arr [a] [c]
outlineCombinator fs1 fs2 =
	parEvalN () fs1 >>>
	rightRotate >>>
	parEvalN () fs2
~~~~

In a distributed environment this first evaluates all `[arr a b]` in parallel,
sends the results back to the master node, rotates the input once
and then evaluates the
`[arr b c]` in parallel to then gather the input once again on the master node.
Such situations arise, e.g. in scientific computations when data
distributed across the nodes needs to be transposed.
A concrete example is 2D FFT computation [@Gorlatch; @Berthold2009-fft].

While the example could be rewritten into a single `parEvalN` call by
directly wiring the Arrows together before spawning, it illustrates an
important problem. When using a `ArrowParallel` backend that resides on
multiple computers, all communication between the nodes is done via
the master node, as shown in the Eden trace in Figure \ref{fig:withoutFutures}.
This can become a serious bottleneck for a larger amount of data and
number of processes as e.g. @Berthold2009-fft showcases.

![Communication between 4 Eden processes without Futures.
All communication goes through the master node.
Each bar represents one process. Black lines represent communication.
Colours: blue $\hat{=}$ idle, green $\hat{=}$ running, red  $\hat{=}$ blocked,
yellow $\hat{=}$ suspended.](src/img/withoutFutures.pdf){#fig:withoutFutures}

This is usually only a problem in distributed memory
and we should allow nodes to communicate directly with each other. Eden already provides
\enquote{remote data} that enable this [@AlGo03a; @Dieterle2010].
But as we want code using our DSL to be agnostic in terms of which backend is used,
we have to wrap this concept. We do this with the `Future` type class to abstract
the idea of handles on data that can be passed between nodes:

~~~~ {.haskell}
class Future fut a conf | a conf -> fut where
    put :: (Arrow arr) => conf -> arr a (fut a)
    get :: (Arrow arr) => conf -> arr (fut a) a
~~~~

A `conf` parameter is required here as well, but only
so that Haskells type system allows us to have multiple Future implementations
imported at once without breaking any dependencies similar to what we did with
the `ArrowParallel` type class earlier. However, we
can obviously yet again define default
utility instances `Future fut a ()`
for each backend similar to how `ArrowParallel arr a b ()` was defined
in Chapter \ref{sec:parallel-arrows} as we will shortly see in the implementations
for the backends.

Maybe even more interestingly, we use a functional dependency
`a conf -> fut` in the definition. This means that the type of `fut`
can always fully be determined from the actual types of `a` and `conf`. We need this
because we do not want users of our DSL to have to rely on a
specific type of Future in their code.
They only have to declare that they require a compatible Future type and do not
need to worry about any specifics. This can be seen in the Future version of
`outlineCombinator` we will define soon.

In order to implement this type class for Eden and since `RD` is only a
type synonym for a communication type that is used
internally in their library, we have to use some wrapper classes to fit the type class,
as the following code showcases^[Instances of type classes can not be declared on type synonyms]:

~~~~ {.haskell}
data RemoteData a = RD { rd :: RD a }

put' :: (Arrow arr) => arr a (BasicFuture a)
put' = arr BF

get' :: (Arrow arr) => arr (BasicFuture a) a
get' = arr (\(~(BF a)) -> a)

instance NFData (RemoteData a) where
    rnf = rnf . rd
instance Trans (RemoteData a)

instance (Trans a) => Future RemoteData a Conf where
    put _ = put'
    get _ = get'

instance (Trans a) => Future RemoteData a () where
    put _ = put'
    get _ = get'
~~~~

For GpH and `Par` Monad, we can simply use `BasicFuture`s,
which are just simple wrappers around the actual data with boiler-plate logic
so that the type class is satisfied. This is because the concept of a `Future`
does not change anything for shared-memory execution as there are no
communication problems to fix. Nevertheless, we require a common interface
so the parallel Arrows are portable across backends. Here, the implementation is:

~~~~ {.haskell}
data BasicFuture a = BF a

put' :: (Arrow arr) => arr a (BasicFuture a)
put' = arr BF

get' :: (Arrow arr) => arr (BasicFuture a) a
get' = arr (\(~(BF a)) -> a)

instance NFData a => NFData (BasicFuture a) where
    rnf (BF a) = rnf a

instance Future BasicFuture a (Conf a) where
    put _ = put'
    get _ = get'

instance Future BasicFuture a () where
    put _ = put'
    get _ = get'
~~~~

Now, we can use this `Future` concept in our communication example for direct
communication between nodes:

~~~~ {.haskell}
outlineCombinator :: (
    ArrowParallel arr a (fut b) (), 
	ArrowParallel arr (fut b) c (),
	Future fut b ()) =>
	[arr a b] -> [arr b c] -> arr [a] [c]
outlineCombinator fs1 fs2 =
	parEvalN () (map (>>> put ()) fs1) >>>
	rightRotate >>>
	parEvalN () (map (get () >>>) fs2)
~~~~

In a distributed environment, this gives us a communication scheme with
messages going through the master node only if it is needed -- similar to what
is shown in the trace visualisation in Figure \ref{fig:withFutures}. This is
because only the handles to the data that are passed through the master
node, while all communication of actual data can happen between the actual nodes.
We will build upon this concept in more complicated combinators in the next chapter.

![Communication between 4 Eden processes with Futures.
Unlike in Figure \ref{fig:withoutFutures}, processes communicate directly
(one example message is highlighted) instead of always going through the
master node (bottom bar).](src/img/withFutures.pdf){#fig:withFutures}
