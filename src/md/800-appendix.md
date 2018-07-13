# Appendix

## Utility Arrows

\label{utilfns}

Following are definitions of some utility Arrows used in this paper that have been
left out for brevity. We start with the `second` combinator from
@HughesArrows, which is a mirrored version of `first`, which is for example
used in the definition of `***`:

~~~~ {.haskell}
second :: Arrow arr => arr a b -> arr (c, a) (c, b)
second f = arr swap >>> first f >>> arr swap
	where swap (x, y) = (y, x)
~~~~

Next, we give the definition of |evalN| which also helps us to define `map`, and
`zipWith` on Arrows. The `evalN` combinator in Fig. \ref{fig:evalN} converts a
list of Arrows `[arr a b]` into an Arrow `arr [a] [b]`.

~~~~ {#fig:evalN
    .haskell
    .figure
    caption="The definition of |evalN|."
    options=t
    }
evalN :: (ArrowChoice arr) => [arr a b] -> arr [a] [b]
evalN (f:fs) = arr listcase >>>
         arr (const []) ||| (f *** evalN fs >>> arr (uncurry (:)))
         where listcase []     = Left ()
               listcase (x:xs) = Right (x,xs)
evalN [] = arr (const [])
~~~~

The `mapArr` combinator (Fig. \ref{fig:mapArr}) lifts any Arrow `arr a b` to
an Arrow `arr [a] [b]`. The original inspiration was from @Hughes2005,
but the definition was then unified with `evalN`. 

~~~~ {#fig:mapArr
    .haskell
    .figure
    caption="The definition of |map| over Arrows."
    options=t
    }
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr = evalN . repeat
~~~~

Finally, with the help of `mapArr` (Fig. \ref{fig:mapArr}), we can define
`zipWithArr` (Fig. \ref{fig:zipWithArr}) that lifts any Arrow
`arr (a, b) c` to an Arrow `arr ([a], [b]) [c]`.

~~~~ {#fig:zipWithArr
    .haskell
    .figure
    caption="|zipWith| over Arrows."
    options=t
    }
zipWithArr :: ArrowChoice arr => arr (a, b) c -> arr ([a], [b]) [c]
zipWithArr f = (arr (\(as, bs) -> zipWith (,) as bs)) >>> mapArr f
~~~~

These combinators make use of the `ArrowChoice` type class which provides
the `pipepipepipe` combinator. It takes two Arrows `arr a c` and `arr b c`
and combines them into a new Arrow `arr (Either a b) c` which pipes all
`Left a`'s to the first Arrow and all `Right b`'s to the second Arrow:

~~~~ {.haskell}
(pipepipepipe) :: ArrowChoice arr a c -> arr b c -> arr (Either a b) c
~~~~

## Profunctor Arrows

\label{app:profunctorArrows}

In Fig. \ref{fig:profunctorArrow} we show how specific Profunctors can be
turned into Arrows. This works because Arrows are strong Monads in the bicategory
`Prof` of Profunctors as shown by @Asada:2010:ASM:1863597.1863607.
In Standard GHC `(>>>)` has the type
`(>>>) :: Category cat => cat a b -> cat b c -> cat a c` and is therefore not
part of the `Arrow` type class like presented in this paper.^[For additional information on the type classes used, see: \url{https://hackage.haskell.org/package/profunctors-5.2.1/docs/Data-Profunctor.html} and \url{https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Category.html}.]

~~~~ {#fig:profunctorArrow
    .haskell
    .figure
    caption="Profunctors as Arrows."
    options=t
    }
instance (Category p, Strong p) => Arrow p where
  arr f = dimap id f id
  first = first'

instance (Category p, Strong p, Costrong p) => ArrowLoop p where
  loop = loop'

instance (Category p, Strong p, Choice p) => ArrowChoice p where
  left = left'
~~~~

## Additional function definitions

\label{app:omitted}

We have omitted some function definitions in the main text for
brevity, and redeem this here.
We begin with warping Eden's build-in `RemoteData` to `Future` in
Figure \ref{fig:RDFuture}.

~~~~ {#fig:RDFuture
    .haskell
    .figure
    caption="|RD|-based |RemoteData| version of |Future| for the Eden backend."
    options=t
    }
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

Next, we have the definition of `BasicFuture` in Fig. \ref{fig:BasicFuture} and
the corresponding `Future` instances.

~~~~ {#fig:BasicFuture
    .haskell
    .figure
    caption="|BasicFuture| type and its |Future| instance for the |Par| Monad and GpH."
    options=t
    }
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

Figures \ref{fig:parMapImg}--\ref{fig:parMapStream} show the definitions and a
visualisations of two parallel `map` variants, defined using `parEvalN` and its
lazy counterpart.

![`parMap` depiction.](src/img/parMap.pdf){#fig:parMapImg}

~~~~ {#fig:parMap
    .haskell
    .figure
    caption="|parMap| definition."
    options=t
    }
parMap :: (ArrowParallel arr a b conf) => conf -> (arr a b) -> (arr [a] [b])
parMap conf f = parEvalN conf (repeat f)
~~~~

![`parMapStream` depiction.](src/img/parMapStream.pdf){#fig:parMapStreamImg}

~~~~ {#fig:parMapStream
    .haskell
    .figure
    caption="|parMapStream| definition."
    options=t
    }
parMapStream :: (ArrowParallel arr a b conf, ArrowChoice arr, ArrowApply arr) =>
	conf -> ChunkSize -> arr a b -> arr [a] [b]
parMapStream conf chunkSize f = parEvalNLazy conf chunkSize (repeat f)
~~~~

Arrow versions of Eden's `shuffle`, `unshuffle` and the definition of
`takeEach` are in Fig. \ref{fig:edenshuffleetc}. Similarly,
Fig. \ref{fig:edenlazyrightrotate} contains the definition of Arrow
versions of Eden's `lazy` and `rightRotate` utility functions.
Fig. \ref{fig:lazyzip3etc} contains Eden's definition of `lazyzip3` together
with the utility functions `uncurry3` and `threetotwo`.
The full definition of `farmChunk` is in Fig. \ref{fig:farmChunk}.
Eden definition of |ring| skeleton is in Fig. \ref{fig:ringEden}. It
follows @Loogen2012.

~~~~ {#fig:edenshuffleetc
    .haskell
    .figure
    caption="|shuffle|, |unshuffle|, |takeEach| definition."
    options=t
    }
shuffle :: (Arrow arr) => arr [[a]] [a]
shuffle = arr (concat . transpose)

unshuffle :: (Arrow arr) => Int -> arr [a] [[a]]
unshuffle n = arr (\xs -> [takeEach n (drop i xs) | i <- [0..n-1]])

takeEach :: Int -> [a] -> [a]
takeEach n [] = []
takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)
~~~~

~~~~ {#fig:edenlazyrightrotate
    .haskell
    .figure
    caption="|lazy| and |rightRotate| definitions."
    options=t
    }
lazy :: (Arrow arr) => arr [a] [a]
lazy = arr (\ ~(x:xs) -> x : lazy xs)

rightRotate :: (Arrow arr) => arr [a] [a]
rightRotate = arr $ \list -> case list of
  [] -> []
  xs -> last xs : init xs
~~~~

~~~~ {#fig:lazyzip3etc
    .haskell
    .figure
    caption="|lazyzip3|, |uncurry3| and |threetotwo| definitions."
    options=t
    }
lazyzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
lazyzip3 as bs cs = zip3 as (lazy bs) (lazy cs)

uncurry3 :: (a -> b -> c -> d) -> (a, (b, c)) -> d
uncurry3 f (a, (b, c)) = f a b c

threetotwo :: (Arrow arr) => arr (a, b, c) (a, (b, c))
threetotwo = arr $ \ ~(a, b, c) -> (a, (b, c))
~~~~

~~~~ {#fig:farmChunk
    .haskell
    .figure
    caption="|farmChunk| definition."
    options=t
    }
farmChunk :: (ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, 
             ArrowChoice arr, ArrowApply arr) =>
	conf -> ChunkSize -> NumCores -> arr a b -> arr [a] [b]
farmChunk conf chunkSize numCores f =
	unshuffle numCores >>>
	parEvalNLazy conf chunkSize (repeat (mapArr f)) >>>
	shuffle
~~~~

~~~~ {#fig:ringEden
    .haskell
    .figure
    caption="Eden's definition of the |ring| skeleton."
    options=t
    }
ringSimple :: (Trans i, Trans o, Trans r) => (i -> r -> (o,r)) -> [i] -> [o]
ringSimple f is =  os
  where (os,ringOuts) = unzip (parMap (toRD $ uncurry f) (zip is $ lazy ringIns))
        ringIns = rightRotate ringOuts

toRD :: (Trans i, Trans o, Trans r) => ((i,r) -> (o,r)) -> ((i, RD r) -> (o, RD r))
toRD  f (i, ringIn)  = (o, release ringOut)
  where (o, ringOut) = f (i, fetch ringIn)

rightRotate    :: [a] -> [a]
rightRotate [] =  []
rightRotate xs =  last xs : init xs

lazy :: [a] -> [a]
lazy ~(x:xs) = x : lazy xs
~~~~

The `parEval2` skeleton is defined in Figure~\ref{fig:parEval2}. 
We start by transforming the `(a, c)` input into a two-element list
`[Either a c]` by first tagging the two inputs with `Left` and `Right` and wrapping
the right element in a singleton list with `return` so that we can combine them
with `arr (uncurry (:))`. Next, we feed this list into a parallel Arrow running
on two instances of `f +++ g` as described in the paper. After the calculation
is finished, we convert the resulting `[Either b d]` into `([b], [d])` with
`arr partitionEithers`. The two lists in this tuple contain only one element
each by construction, so we can finally just convert the tuple to `(b, d)` in
the last step.

~~~~ {#fig:parEval2
    .haskell
    .figure
    caption="|parEval2| definition."
    options=t
    }
parEval2 :: (ArrowChoice arr,
	ArrowParallel arr (Either a c) (Either b d) conf) =>
	conf -> arr a b -> arr c d -> arr (a, c) (b, d)
parEval2 conf f g =
	arr Left *** (arr Right >>> arr return) >>>
	arr (uncurry (:)) >>>
	parEvalN conf (replicate 2 (f +++ g)) >>>
	arr partitionEithers >>>
	arr head *** arr head
~~~~

Furthermore, Fig. \ref{fig:torus_example_rest} contains the omitted definitions
of `prMM` (sequential matrix multiplication), `splitMatrix`
(which splits the a matrix into chunks), `staggerHorizontally` and
`staggerVertically` (to pre-rotate the matrices), and lastly `matAdd`,
that calculates $A + B$ for two matrices $A$ and $B$.

~~~~ {#fig:torus_example_rest
    .haskell
    .figure
    caption="|prMMTr|, |splitMatrix|, |staggerHorizontally|, |staggerVertically| and |matAdd| definition."
    options=t
    }
prMM :: Matrix -> Matrix -> Matrix
prMM m1 m2 = prMMTr m1 (transpose m2)
  where
    prMMTr m1' m2' = [[sum (zipWith (*) row col) | col <- m2' ] | row <- m1']

splitMatrix :: Int -> Matrix -> [[Matrix]]
splitMatrix size matrix = map (transpose . map (chunksOf size)) $ chunksOf size $ matrix

staggerHorizontally :: [[a]] -> [[a]]
staggerHorizontally matrix = zipWith leftRotate [0..] matrix

staggerVertically :: [[a]] -> [[a]]
staggerVertically matrix = transpose $ zipWith leftRotate [0..] (transpose matrix)

leftRotate :: Int -> [a] -> [a]
leftRotate i xs = xs2 ++ xs1 where
    (xs1,xs2) = splitAt i xs

matAdd = chunksOf (dimX x) $ zipWith (+) (concat x) (concat y)
~~~~

## Syntactic sugar

\label{syntacticSugar}

Finally, we also give the definitions for some syntactic sugar for PArrows,
namely `parstar` and `parand`. For basic Arrows, we have the `***` combinator
(Fig. \ref{fig:syntacticSugarArrows}) which allows us to combine two Arrows
`arr a b` and `arr c d` into an Arrow `arr (a, c) (b, d)` which does both
computations at once. This can easily be translated into a parallel version
`parstar` with the use of `parEval2`, but for this we require a backend which
has an implementation that does not require any configuration
(hence the `()` as the `conf` parameter% in Fig. \ref{fig:par***}):

~~~~ {.haskell}
(|***|) :: (ArrowChoice arr, ArrowParallel arr (Either a c) (Either b d) ())) =>
	arr a b -> arr c d -> arr (a, c) (b, d)
(|***|) = parEval2 ()
~~~~

We define the parallel `parand` in a similar manner to its sequential
pendant `&&&` (Fig. \ref{fig:syntacticSugarArrows}):

~~~~ {.haskell}
(|&&&|) :: (ArrowChoice arr, ArrowParallel arr (Either a a) (Either b c) ()) =>
	arr a b -> arr a c -> arr a (b, c)
(|&&&|) f g = (arr $ \a -> (a, a)) >>> f |***| g
~~~~