# Parallel Arrows

\label{sec:parallel-arrows}

Having discussed the idea of Arrows as well as the basics of the APIs we wish to use
 as backends, we can now discuss the design and implementation of the actual
PArrows DSL.
We present the `ArrowParallel` type class
and explain the reasoning behind it in Chapter \ref{sec:parallel-arrows-type-class}
before discussing its implementations in GpH, the `Par` Monad and Eden in Chapter
\ref{sec:arrowparallelimpl}.
Finally, we give first basic extensions in Chapter 
\ref{sec:extending-interface}.

## The `ArrowParallel` type class

\label{sec:parallel-arrows-type-class}

A parallel computation (on functions) can be seen as the execution of some functions
`a -> b` in parallel, as our `parEvalN` prototype shows
(Chapter \ref{sec:parEvalNIntro}).
Translating this into Arrow terms gives us a new operator `parEvalN` that lifts
a list of Arrows `[arr a b]` to a parallel Arrow `arr [a] [b]`.
This combinator is similar to the evaluation combinator `evalN :: [arr a b] -> arr [a] [b]`
from Chapter \ref{utilfns}, but does parallel instead of serial evaluation:

~~~~ {.haskell}
parEvalN :: (Arrow arr) => [arr a b] -> arr [a] [b]
~~~~

With such a definition of `parEvalN`, parallel execution is yet another Arrow
combinator. But since the implementation may differ depending on the actual
type of the Arrow `arr` -- or even the input `a` and output `b` -- and we
want this to be an interface for different backends that we should be able to 
switch between, we introduce a new type class `ArrowParallel arr a b`.

~~~~ {.haskell}
class Arrow arr => ArrowParallel arr a b where
	parEvalN :: [arr a b] -> arr [a] [b]
~~~~

Sometimes parallel Haskells require or allow additional configuration
parameters, e.g. information about the execution environment or the level
of evaluation (WHNF vs. NF, see the section on laziness in Chapter \ref{sec:shortIntroHaskell}).
For this reason we
introduce an additional `conf` parameter as we do not want `conf` to be a fixed type,
as the configuration parameters can differ for different instances of
`ArrowParallel`.

~~~~ {.haskell}
class Arrow arr => ArrowParallel arr a b conf where
	parEvalN :: conf -> [arr a b] -> arr [a] [b]
~~~~

By restricting the implementations of our backends to a specific `conf` type,
we also get interoperability between backends for free as it serves as a discriminator
for which backend has to be used. We can therefore parallelise one
part of a program using one backend, and do the same for the next but with another one by
just passing a different configuration type.

## `ArrowParallel` instances

\label{sec:arrowparallelimpl}

With the `ArrowParallel` type class defined, we will now give implementations of it with GpH,
the `Par` Monad and Eden.

### Glasgow parallel Haskell 

\label{sec:parrows:multicore}

The GpH instance of `ArrowParallel` is implemented in a straightforward
manner in Figure \ref{fig:ArrowParallelMulticore}, but a bit different compared
to the variant from Chapter \ref{sec:GpHIntro}.
We use `evalN :: [arr a b] -> arr [a] [b]`
(definition in Appendix \ref{utilfns}, think `zipWith ($)` on Arrows) combined
with `withStrategy :: Strategy a -> a -> a` from GpH, where `withStrategy` is
the same as `using :: a -> Strategy a -> a`, but with flipped parameters.
Our `Conf a` datatype simply wraps a `Strategy a`, but could be extended
in future versions of our DSL.

~~~~ {#fig:ArrowParallelMulticore
    .haskell
    .figure
    caption="GpH |ArrowParallel| instance."
    options=h
    }
data Conf a = Conf (Strategy a)

instance (ArrowChoice arr) =>
  ArrowParallel arr a b (Conf b) where
    parEvalN (Conf strat) fs =
        evalN fs >>>
        arr (withStrategy (parList strat))
~~~~

### `Par` Monad

As for GpH we can easily lift the definition of `parEvalN` for the
`Par` Monad to Arrows in Figure \ref{fig:ArrowParallelParMonad}.
To start off, we define the `Strategy a` and `Conf a` type so we can have a
configurable instance of ArrowParallel:

~~~~ {.haskell}
type Strategy a = a -> Par (IVar a)
data Conf a = Conf (Strategy a)
~~~~

Now we can once again define our `ArrowParallel` instance as follows:
First, we convert our Arrows `[arr a b]` with `evalN (map (>>> arr strat) fs)`
into an Arrow `arr [a] [(Par (IVar b))]` that yields composable
computations in the `Par` Monad. By combining the result of this Arrow with
`arr sequenceA`, we get an Arrow `arr [a] (Par [IVar b])`.
Then, in order to fetch the results of the different threads,
we map over the `IVar`s inside the `Par` Monad with `arr (>>= mapM get)` -- our
intermediary Arrow is of type `arr [a] (Par [b])`.
Finally, we execute the computation `Par [b]` by composing with
`arr runPar` and get the final Arrow `arr [a] [b]`.

~~~~ {#fig:ArrowParallelParMonad
    .haskell
    .figure
    caption="|Par| Monad |ArrowParallel| instance."
    options=h
    }
type Strategy a = a -> Par (IVar a)
data Conf a = Conf (Strategy a)
    
instance (ArrowChoice arr) => ArrowParallel arr a b (Conf b) where
    parEvalN (Conf strat) fs =
        evalN (map (>>> arr strat) fs) >>>
        arr sequenceA >>>
        arr (>>= mapM Control.Monad.Par.get) >>>
        arr runPar
~~~~

### Eden

\label{sec:parrows-Eden}

For both the GpH Haskell and `Par` Monad implementations we could use
general instances of `ArrowParallel` that just require the `ArrowChoice` type class.
With Eden this is not the case as we can only spawn a list of functions, which
we cannot extract from general Arrows. While we could still manage to
have only one instance in the module by introducing a type class

~~~~ {.haskell}
class (Arrow arr) => ArrowUnwrap arr where
	unwrap :: arr a b -> (a -> b)
~~~~

we avoid doing so for aesthetic reasons.
For now, we just implement `ArrowParallel` for normal functions and the
Kleisli type in Figure \ref{fig:ArrowParallelEden}, where
`Conf` is simply defined as `data Conf = Nil` since Eden does not
have a configurable `spawnF` variant.

~~~~ {#fig:ArrowParallelEden
    .haskell
    .figure
    caption="Eden |ArrowParallel| instance."
    options=h
    }
instance (Trans a, Trans b) => ArrowParallel (->) a b Conf where
    parEvalN _ = spawnF

instance (ArrowParallel (->) a (m b) Conf,
  Monad m, Trans a, Trans (m b)) =>
  ArrowParallel (Kleisli m) a b Conf where
    parEvalN conf fs = 
      arr (parEvalN conf (map (\(Kleisli f) -> f) fs)) >>>
      Kleisli sequence
~~~~

*Note that while writing this thesis, we found
another solution that could be feasible: We could write the
instance `parEvalN` as*

~~~~{.haskell}
instance (Trans b, ArrowChoice arr) => ArrowParallel arr a b Conf where
  parEvalN _ fs = evalN fs >>> arr (spawnF (repeat id))
~~~~

*We were however, not able to prove that this behaves exactly the same as
the variant presented above as this would have required re-running the whole
test-suite. First tests suggest correct behaviour, though.*

### Default configuration instances

While the configurability of the `ArrowParallel`
instances above is nice, users probably would like to have proper
default configurations for many parallel programs as well.
These can also easily be defined as we can see by the example
of the default implementation of `ArrowParallel` for GpH:

~~~~ {.haskell}
instance (NFData b, ArrowChoice arr, ArrowParallel arr a b (Conf b)) =>
  ArrowParallel arr a b () where
    parEvalN _ fs = parEvalN (defaultConf fs) fs

defaultConf :: (NFData b) => [arr a b] -> Conf b
defaultConf fs = stratToConf fs rdeepseq

stratToConf :: [arr a b] -> Strategy b -> Conf b
stratToConf _ strat = Conf strat
~~~~

The other backends have similarly structured implementations which we
do not discuss here for the sake of brevity. We can, however, only have one
instance of `ArrowParallel arr a b ()` present at a time,
which should not be a problem, anyways.

Up until now we discussed Arrow operations more in detail,
but in the following sections we focus more on the data-flow
between the Arrows, now that we have seen that Arrows are capable
of expressing parallelism. We nevertheless do explain new concepts in greater detail
if required for better understanding.
