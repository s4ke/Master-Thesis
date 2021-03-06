## Advanced topological skeletons

\label{sec:topology-skeletons}

Even though many algorithms can be expressed by `parMap`s,
some problems require more sophisticated skeletons.
The Eden library resolves this problem and already comes with
more predefined skeletons^[Available on Hackage:
\url{https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/Control-Parallel-Eden-Topology.html}.],
among them a `pipe`, a `ring`, and a `torus` implementation
[@Eden:SkeletonBookChapter02]. These seem like reasonable candidates to be
ported to our Arrow-based parallel Haskell. By doing so, we aim to showcase that we
can express more sophisticated skeletons with parallel Arrows as well.

If we were to use the original definition of `parEvalN`, however, these skeletons
would produce an infinite loop with the GpH and `Par` Monad which during runtime
would result in the program crashing.
This materialises with the usage of `loop` of the `ArrowLoop` type class and we
think that this is due to difference of how evaluation is done in these
backends compared to Eden.
An investigation of why this difference exists is beyond the scope of this work
-- the results of the experimental Cloud Haskell backend in Chapter \ref{sec:CloudHaskellArrowParallelLimitsMitigation}
touch on the likely root cause of this problem, though.
We only provide a workaround for these types of skeletons as such they
probably are not of much importance outside of a distributed memory environment.
Nevertheless, our workaround enables users of the DSL to test their code within a
shared memory setting.

The idea of the fix is to provide a `ArrowLoopParallel` type class that has two
functions -- `loopParEvalN` and `postLoopParEvalN`. The first is to be
used inside an `loop` construct while the latter will be used right outside of
the `loop`. This way, we can delegate to the actual `parEvalN` in the spot where
the backend supports it.

~~~~ {.haskell}
class ArrowParallel arr a b conf =>
	ArrowLoopParallel arr a b conf where
    loopParEvalN :: conf -> [arr a b] -> arr [a] [b]
    postLoopParEvalN :: conf -> [arr a b] -> arr [a] [b]
~~~~

Because Eden has no problems with the looping skeletons, we use this instance:

~~~~ {.haskell}
instance (ArrowChoice arr, ArrowParallel arr a b Conf) =>
	ArrowLoopParallel arr a b Conf where
    loopParEvalN = parEvalN
    postLoopParEvalN _ = evalN
~~~~

The `Par` Monad and GpH implementations of `parEvalN` have problems inside of `loop`.
Their respective instances for `ArrowLoopParallel` look like this:

~~~~ {.haskell}
instance (ArrowChoice arr, ArrowParallel arr a b (Conf b)) =>
	ArrowLoopParallel arr a b (Conf b) where
    loopParEvalN _ = evalN
    postLoopParEvalN = parEvalN
~~~~

Chapter \ref{sec:pipe} explains how to achieve a parallel `pipe` with our DSL. Then,
Chapter \ref{sec:ring} goes into detail on how to achieve a `ring` skeleton, which
we then extend to achieve a `torus` in Chapter \ref{sec:torus}.

### Parallel pipe

\label{sec:pipe}

We start with the parallel `pipe` skeleton, which is semantically equivalent to folding
over a list `[arr a a]` of Arrows with `>>>`, but in parallel,
meaning that the Arrows do not have to reside on the same thread/machine.
We implement this skeleton using the `ArrowLoop` type class which provides us with
the `loop :: arr (a, b) (c, b) -> arr a c` combinator allowing us to express
recursive fix-point computations in which output values are fed back as input.
For example

~~~~ {.haskell}
loop (arr (\(a, b) -> (b, a:b)))
~~~~

which is the same as

~~~~ {.haskell}
loop (arr snd &&& arr (uncurry (:)))
~~~~

defines an Arrow that takes its input `a` and converts it into an infinite
stream `[a]` of it. Using `loop` to our advantage and adapting it to 
apply our list of functions in sequence gives us a first draft of
a pipe implementation (Figure \ref{fig:pipeSimple}): We plug the
parallel evaluation call `loopParEvalN conf fs` inside the second argument of `&&&`
and then only pick the first element of the resulting list with `arr last` outside
of the `loop`. Here, the length of the input list of Arrows determines when
the loop stops. For finite input lengths this means that the computation will
definitely finish.

~~~~ {#fig:pipeSimple
    .haskell
    .figure
    caption="Simple |pipe| skeleton. The use of |lazy| (Figure \ref{fig:edenlazyrightrotate}) is essential as without it programs using this definition would never halt. We need to ensure that the evaluation of the input |[a]| is not forced fully before passing it into |loopParEvalN|."
    options=t
    }
pipeSimple :: (ArrowLoop arr, ArrowLoopParallel arr a a conf) =>
	conf -> [arr a a] -> arr a a
pipeSimple conf fs =
    loop (arr snd &&&
        (arr (uncurry (:) >>> lazy) >>> loopParEvalN conf fs)) >>>
    arr last
~~~~

However, using this definition directly will make the master node a
potential bottleneck in distributed environments just like the first version
 of the outline combinator in Chapter \ref{sec:futures}.
Therefore, we introduce a more sophisticated version
that internally uses Futures and obtain the final definition of `pipe` in
Figure \ref{fig:pipe}.

~~~~ {#fig:pipe
    .haskell
    .figure
    caption="|pipe| skeleton definition with Futures."
    options=t
    }
pipe :: (ArrowLoop arr,
    ArrowLoopParallel arr (fut a) (fut a) conf,
	Future fut a conf) =>
	conf -> [arr a a] -> arr a a
pipe conf fs = unliftFut conf (pipeSimple conf (map (liftFut conf) fs))

liftFut :: (Arrow arr, Future fut a conf, Future fut b conf) =>
	conf -> arr a b -> arr (fut a) (fut b)
liftFut conf f = get conf >>> f >>> put conf

unliftFut :: (Arrow arr, Future fut a conf, Future fut b conf) =>
	conf -> arr (fut a) (fut b) -> arr a b
unliftFut conf f = put conf >>> f >>> get conf
~~~~

Sometimes, this `pipe` definition can be a bit inconvenient,
especially if we want to pipe Arrows of mixed types together,
i.e. `arr a b` and `arr b c`. By wrapping these two Arrows inside a bigger
Arrow `arr (([a], [b]), [c]) (([a], [b]), [c])` suitable for `pipe`, we can define
`pipe2` as in Figure \ref{fig:pipe2}.

~~~~ {#fig:pipe2
    .haskell
    .figure
    caption="Definition of |pipe2| and |(parcomp)|, a parallel |>>>|."
    options=t
    }
pipe2 :: (ArrowLoop arr, ArrowChoice arr,
    ArrowLoopParallel arr (fut (([a], [b]), [c])) (fut (([a], [b]), [c])) conf,
    Future fut (([a], [b]), [c]) conf) =>
	conf -> arr a b -> arr b c -> arr a c
pipe2 conf f g =
    (arr return &&& arr (const [])) &&& arr (const []) >>>
    pipe conf (replicate 2 (unify f g)) >>>
    arr snd >>>
    arr head
    where
        unify :: (ArrowChoice arr) => 
			arr a b -> arr b c -> arr (([a], [b]), [c]) (([a], [b]), [c])
        unify f' g' =
			(mapArr f' *** mapArr g') *** arr (const []) >>>
			arr (\((b, c), a) -> ((a, b), c))

(parcomp) :: (ArrowLoop arr, ArrowChoice arr,
    ArrowLoopParallel arr (fut (([a], [b]), [c])) (fut (([a], [b]), [c])) (),
    Future fut (([a], [b]), [c]) ()) =>
    arr a b -> arr b c -> arr a c
(parcomp) = pipe2 ()
~~~~

Extensive use of `pipe2` over `pipe` with a hand-written combination data type
will probably result in worse performance because of more communication overhead
from the many calls to `parEvalN` inside of `evalN`. Nonetheless, we can define
a parallel piping operator `parcomp`, which is semantically equivalent to
`>>>` similarly to other parallel syntactic sugar from Appendix \ref{syntacticSugar}.

### Ring skeleton 

\label{sec:ring}

![`ring` skeleton depiction.](src/img/ringImg.pdf){#fig:ringImg}

Eden comes with a ring skeleton^[Available on Hackage:
\url{https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/Control-Parallel-Eden-Topology.html}.]
(Figure \ref{fig:ringImg}) implementation that allows the computation of a
function `[i] -> [o]` with a ring of nodes that communicate with each other.
Its input is a node function `i -> r -> (o, r)` in which `r` serves as the
intermediary output that is sent to the neighbour of each node.
It uses the direct \enquote{remote data} communication channels that were already
mentioned in Chapter \ref{sec:futures}. 
We depict it in the Appendix, Figure \ref{fig:ringEden}.

We can rewrite this functionality easily with the use of `loop` as the
definition of the node function, `arr (i, r) (o, r)`, after being transformed
into an Arrow, already fits quite neatly into `loop`'s signature: 
`arr (a, b) (c, b) -> arr a c`. In each iteration we start by rotating the
intermediary input from the nodes `[fut r]` with `second (rightRotate >>> lazy)`
(Figure \ref{fig:edenlazyrightrotate}). Similarly to the `pipe` from
Chapter \ref{sec:pipe} (Figure \ref{fig:pipeSimple}), we have to feed the 
intermediary input into our `lazy` (Figure \ref{fig:edenlazyrightrotate}) Arrow here,
or the evaluation would fail to terminate. The reasoning is explained by
@Loogen2012 as a demand problem.

Next, we zip the resulting `([i], [fut r])` to `[(i, fut r)]` with
`arr (uncurry zip)`. We then feed this into our parallel Arrow
`arr [(i, fut r)] [(o, fut r)]` obtained by transforming our input Arrow
`f :: arr (i, r) (o, r)` into `arr (i, fut r) (o, fut r)` before `repeat`ing and
lifting it with `loopParEvalN`. Finally, we unzip the output list
`[(o, fut r)]` into `([o], [fut r])`.

Plugging this Arrow `arr ([i], [fut r]) ([o], fut r)` into the definition of
`loop` from earlier gives us `arr [i] [o]`, our ring Arrow (Figure \ref{fig:ringFinal}).
To make sure this algorithm has speedup on shared-memory machines as well, we pass
the result of this Arrow to `postLoopParEvalN conf (repeat (arr id))`.
This combinator can, for example, be used to calculate the shortest paths in a
graph using Warshall's algorithm.

~~~~ {#fig:ringFinal
    .haskell
    .figure
    caption="|ring| skeleton definition."
    options=t
    }
ring :: (Future fut r conf,
    ArrowLoop arr,
    ArrowLoopParallel arr (i, fut r) (o, fut r) conf,
    ArrowLoopParallel arr o o conf) =>
    conf -> arr (i, r) (o, r) -> arr [i] [o]
ring conf f =
    loop (second (rightRotate >>> lazy) >>>
        arr (uncurry zip) >>>
        loopParEvalN conf 
            (repeat (second (get conf) >>> f >>> second (put conf))) >>>
        arr unzip) >>>
    postLoopParEvalN conf (repeat (arr id))
~~~~

### Torus skeleton

\label{sec:torus}

![`torus` skeleton depiction.](src/img/ringTorusImg.pdf){#fig:ringTorusImg}

If we take the concept of a `ring` from Chapter \ref{sec:ring} one dimension
further, we obtain a `torus` skeleton (Figure \ref{fig:ringTorusImg}, \ref{fig:torus}).
In a `torus`, every node sends and receives data from horizontal and vertical neighbours
in each communication round. With our Parallel Arrows, we re-implement this
combinator
from Eden^[Available on Hackage: \url{https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/Control-Parallel-Eden-Topology.html}.] --
yet again with the help of the `ArrowLoop` type class.

Similar to the `ring`, we start by rotating the input
(Figure \ref{fig:edenlazyrightrotate}), but this time not only in one direction,
but in two. This means that the intermediary input from the neighbour nodes has to
be stored in a tuple `([[fut a]], [[fut b]])` in the second argument
(`loop` only allows for two arguments) of our looped Arrow of type

~~~~{.haskell}
arr ([[c]], ([[fut a]], [[fut b]])) ([[d]], ([[fut a]], [[fut b]])) 
~~~~

and our rotation Arrow becomes

~~~~ {.haskell}
second ((mapArr rightRotate >>> lazy) *** (arr rightRotate >>> lazy))
~~~~

instead of the singular rotation in the ring as we rotate `[[fut a]]` horizontally
and `[[fut b]]` vertically. Then, we zip the inputs for the input Arrow with

~~~~ {.haskell}
arr (uncurry3 zipWith3 lazyzip3)
~~~~

from `([[c]], ([[fut a]], [[fut b]]))` to `[[(c, fut a, fut b)]]`, which we then
evaluate in parallel.

This, however, is more complicated than in the `ring` case as we have one more
dimension of inputs that needs to be transformed. We first have to `shuffle` all
the inputs and then pass them into `loopParEvalN conf (repeat (ptorus conf f))` to
get an output of `[(d, fut a, fut b)]`. We then `unshuffle` this list back to
its original ordering by feeding it into `arr (uncurry unshuffle)` which
takes the input length we saved one step earlier as additional input to get a
result matrix `[[[(d, fut a, fut b)]]`. Finally, we unpack this matrix
 with `arr (map unzip3) >>> arr unzip3 >>> threetotwo` to get
 `([[d]], ([[fut a]], [[fut b]]))`.

This internal looping computation is once again fed into `loop` and we
also compose a final `postLoopParEvalN conf (repeat (arr id))` due to the same
problem with `loop` as explained for the `ring` skeleton. 

~~~~ {#fig:torus
    .haskell
    .figure
    caption="|torus| skeleton definition. |lazyzip3|, |uncurry3| and |threetotwo| definitions are in Figure \ref{fig:lazyzip3etc}."
    options=t
    }
torus :: (Future fut a conf, Future fut b conf,
      ArrowLoop arr, ArrowChoice arr,
      ArrowLoopParallel arr (c, fut a, fut b) (d, fut a, fut b) conf,
      ArrowLoopParallel arr [d] [d] conf) =>
      conf -> arr (c, a, b) (d, a, b) -> arr [[c]] [[d]]
torus conf f =
    loop (second ((mapArr rightRotate >>> lazy) 
                    *** (arr rightRotate >>> lazy)) >>>
        arr (uncurry3 (zipWith3 lazyzip3)) >>>
        arr length &&& (shuffle >>> 
                        loopParEvalN conf (repeat (ptorus conf f))) >>>
        arr (uncurry unshuffle) >>>
        arr (map unzip3) >>> arr unzip3 >>> threetotwo) >>>
    postLoopParEvalN conf (repeat (arr id))

ptorus :: (Arrow arr, Future fut a conf, Future fut b conf) =>
          conf ->
          arr (c, a, b) (d, a, b) ->
          arr (c, fut a, fut b) (d, fut a, fut b)
ptorus conf f =
	arr (\ ~(c, a, b) -> (c, get conf a, get conf b)) >>>
	f >>>
	arr (\ ~(d, a, b) -> (d, put conf a, put conf b))
~~~~

As an example of using this skeleton, @Eden:SkeletonBookChapter02 showed the
matrix multiplication using Gentleman's algorithm [@Gentleman1978].
An adapted version can be found in Figure \ref{fig:torusMatMult}.

~~~~ {#fig:torusMatMult
    .haskell
    .figure
    caption="Adapted matrix multiplication in Eden using the |torus| skeleton. |prMM_torus| is the parallel matrix multiplication. |mult| is the function performed by each worker. |prMM| is the sequential matrix multiplication in the chunks. |splitMatrix| splits the Matrix into chunks. |staggerHorizontally| and |staggerVertically| pre-rotate the matrices. |matAdd| calculates $A + B$. Omitted definitions can be found in \ref{fig:torus_example_rest}."
    options=t
    }
type Matrix = [[Int]]

prMM_torus :: Int -> Int -> Matrix -> Matrix -> Matrix
prMM_torus numCores problemSizeVal m1 m2 = 
	combine $ torus () (mult torusSize) $ zipWith zip (split1 m1) (split2 m2)
    where   torusSize = (floor . sqrt) $ fromIntegral $ numCoreCalc numCores
            combine x = concat (map ((map (concat)) . transpose) x)
            split1 x = staggerHorizontally
                (splitMatrix (problemSizeVal `div` torusSize) x)
            split2 x = staggerVertically
                (splitMatrix (problemSizeVal `div` torusSize) x)

-- Function performed by each worker
mult :: Int ->
    ((Matrix,Matrix),[Matrix],[Matrix]) ->
    (Matrix,[Matrix],[Matrix])
mult size ((sm1,sm2),sm1s,sm2s) = (result,toRight,toBottom)
    where 	toRight = take (size-1) (sm1:sm1s)
            toBottom = take (size-1) (sm2:sm2s)
            sms = zipWith prMM (sm1:sm1s) (sm2:sm2s)
            result = foldl1' matAdd sms
~~~~

If we compare the trace from a call using our Arrow definition of the
`torus` (Figure \ref{fig:torus_parrows_trace}) with the Eden version
(Figure \ref{fig:torus_eden_trace}) we can see that the behaviour of the Arrow version
and execution times are comparable -- our port was successful.
We discuss further benchmarks on larger
clusters in more detail in Chapter \ref{sec:benchmarks}.

![Communication trace of a matrix multiplication with `torus` (PArrows).](src/img/torus_matrix_parrows_trace.pdf){#fig:torus_parrows_trace}

![Communication trace of a matrix multiplication with `torus` (Eden).](src/img/torus_matrix_parrows_trace.pdf){#fig:torus_eden_trace}
