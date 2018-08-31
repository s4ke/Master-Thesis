### Arrows

\label{sec:arrows}

Arrows were introduced by @HughesArrows as a general interface for computation
and a less restrictive generalisation of Monads.
@HughesArrows motivates the broader interface of Arrows with the example
of a parser with added static meta-information that can not satisfy the
monadic bind operator `(>>=) :: Monad m => m a -> (a -> m b) -> m b`
^[In the example a parser of the type `Parser s a` with static meta
information `s` and result `a` is shown to not be able to use the static
`s` without applying the monadic function `a -> m b`. With Arrows this is possible.].

An Arrow `arr a b` represents a computation that converts an input `a` to an output
`b`. The general concept is defined in the `Arrow` type class shown in Figure \ref{fig:ArrowDefinition}.
To lift an ordinary function to an Arrow, `arr` is used, analogous to the monadic `return`. Similarly,
the composition operator `>>>` is analogous to the monadic composition `>>=` and combines two Arrows `arr a b`
and `arr b c` by \enquote{wiring} the outputs of the first to the inputs to the second to get a new Arrow `arr a c`.
Lastly, the `first` operator takes the input Arrow `arr a b` and converts it into an Arrow on pairs `arr (a, c) (b, c)`
that leaves the second argument untouched. It allows us to to save input across Arrows. Figure \ref{fig:arrows-viz} shows a
graphical representation of these basic Arrow combinators.
The most prominent instances of this interface (Figure \ref{fig:ArrowDefinition}) are regular functions `(->)`
and the Kleisli type, which wraps monadic functions,
e.g. `a -> m b`.^[In \ref{sec:relWorkArrows} we referenced further relevant 
Arrow types, especially ones that can be used for Arrow-based functional
reactive programming.]

~~~~ {#fig:ArrowDefinition
    .haskell
    .figure
    caption="The |Arrow| type class and its two most typical instances."
    options=t
    }
class Arrow arr where
  arr :: (a -> b) -> arr a b
  (>>>) :: arr a b -> arr b c -> arr a c
  first :: arr a b -> arr (a,c) (b,c)

instance Arrow (->) where
	arr f = f
	f >>> g = g . f
	first f = \(a, c) -> (f a, c) 

data Kleisli m a b = Kleisli { run :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
	arr f = Kleisli (return . f)
	f >>> g = Kleisli (\a -> f a >>= g)
	first f = Kleisli (\(a,c) -> f a >>= \b -> return (b,c))
~~~~

![Schematic depiction of  an Arrow (left) and its basic
  combinators `arr`, `>>>` and `first` (right).](src/img/arrows-viz.pdf){#fig:arrows-viz}
  
![Visual depiction of syntactic sugar for Arrows.](src/img/syntacticSugarArrows.pdf){#fig:syntacticSugarArrows}

Hughes also defined some syntactic sugar (Figure \ref{fig:syntacticSugarArrows}): `second`, `***` and `&&&`. 
`second` is the mirrored version of `first`:

~~~~ {.haskell}
second :: Arrow arr => arr a b -> arr (c, a) (c, b)
second f = arr swap >>> first f >>> arr swap
	where swap (x, y) = (y, x)
~~~~

The `***` function combines `first` and `second` to handle two inputs in one Arrow, and is defined as follows:

~~~~ {.haskell}
(***) :: Arrow arr => arr a b -> arr c d -> arr (a, c) (b, d)
f *** g = first f >>> second g
~~~~

The `&&&` combinator, which constructs an Arrow that outputs two different values like `***`, but takes only one input, is:

~~~~ {.haskell}
(&&&) :: Arrow arr => arr a b -> arr a c -> arr a (b, c)
f &&& g = arr (\a -> (a, a)) >>> f *** g
~~~~

A first short example given by Hughes on how to use the Arrow
interface is the addition of results of two generic Arrows to get a new
Arrow:

~~~~ {.haskell}
add :: Arrow arr => arr a Int -> arr a Int -> arr a Int
add f g = f &&& g >>> arr (\(u, v) -> u + v)
~~~~

As we can rewrite the monadic bind operation `(>>=)` with only the Kleisli type
into `m a -> Kleisli m a b -> m b`, but not with a general Arrow `arr a b`,
we can intuitively get an idea of why Arrows must be a generalisation of Monads.
While this also means that a general Arrow can not express everything a Monad can,
@HughesArrows shows in his parser example that this trade-off is worth it
in some cases.

#### Utility Combinators

\label{utilfns}

In order to ease the use of Arrows, we will now define some utility Arrow combinators, namely
`evalN` as well as `mapArr`. `evalN`, which turns a list of Arrows `[arr a b]` 
into a new Arrow `arr [a] [b]` evaluating a list of inputs `[a]`
against these Arrows is defined in Figure \ref{fig:evalN}

~~~~ {#fig:evalN
    .haskell
    .figure
    caption="The definition of |evalN|."
    options=h
    }
evalN :: (ArrowChoice arr) => [arr a b] -> arr [a] [b]
evalN (f:fs) = arr listcase >>>
         arr (const []) ||| (f *** evalN fs >>> arr (uncurry (:)))
         where listcase []     = Left ()
               listcase (x:xs) = Right (x,xs)
evalN [] = arr (const [])
~~~~

Next, we have the `mapArr` combinator (Figure \ref{fig:mapArr}). It
lifts any Arrow `arr a b` to
an Arrow `arr [a] [b]`. The original inspiration was from @Hughes2005,
but the definition was then unified with `evalN`. 

~~~~ {#fig:mapArr
    .haskell
    .figure
    caption="The definition of |map| over Arrows."
    options=h
    }
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr = evalN . repeat
~~~~

These combinators make use of the `ArrowChoice` type class providing
the `pipepipepipe` combinator. This combinator takes two Arrows `arr a c` and `arr b c`
and combines them into a new Arrow `arr (Either a b) c` which pipes all
`Left a`'s to the first Arrow and all `Right b`'s to the second Arrow:

~~~~ {.haskell}
(pipepipepipe) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c
~~~~

One thing we can see from these utility Arrows is how easily we can define
generic Arrow code that will work on any sufficient specific Arrow. This
is exactly why we aim to allow for parallelisation of programs via Arrows.
We want to empower such generic code to be easily parallelised so these generic
types of APIs are better to use.

#### Notes

In this thesis we will show that parallel computations can be expressed with this
more general interface of Arrows without requiring Monads (we will see an example of
monadic parallelism in Chapter \ref{sec:parallelHaskells}). We also do not restrict
the compatible Arrows to ones which have `ArrowApply` instances -- as every
Arrow that has a `ArrowApply` instance gives rise to a Monad -- but instead
only require instances for `ArrowChoice` (for the if-then-else construction in 
`evalN` (Figure \ref{fig:evalN}))
and `ArrowLoop` (for the looping used in the topological skeletons in Chapter \ref{sec:topology-skeletons}).
Because of this, we have a truly more general
interface when compared to a monadic one or a purely function `(->)` based one.

While we could have based our DSL on Profunctors^[See \url{http://hackage.haskell.org/package/profunctors-5.3/docs/Data-Profunctor.html}
for more information as well as the Haskell interface.] as well,
we chose Arrows in this thesis since they allow for a more direct way of
thinking about parallelism than general Profunctors because of their
composability. However, they are a promising candidate for future improvements
of our DSL. Some Profunctors, especially ones supporting a composition operation,
choice, and looping, can already be adapted to our interface as shown in
Appendix \ref{app:profunctorArrows}.
