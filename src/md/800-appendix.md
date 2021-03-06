\appendix

# Appendix

Following are additional chapters with supplementary information for this thesis. Next, Chapter \ref{app:profunctorArrows}
explains how specific Profunctors fit the Arrow type class. Chapter \ref{app:omitted}
covers omitted function definitions. Then, Chapter \ref{syntacticSugar} explains syntactic
sugar for PArrows. We give additional definitions for the experimental Cloud Haskell
backend in Chapter \ref{sec:appendixCloudHaskell} and finish with the plots
for the shared memory backends and distributed memory backends in Chapters \ref{sec:benchmarkSharedPlots}
and \ref{sec:benchmarkDistPlots}, respectively.

## Profunctor Arrows

\label{app:profunctorArrows}

In Figure \ref{fig:profunctorArrow} we show how specific Profunctors can fit into the
Arrow type class. This works because Arrows are strong Monads in the bicategory
`Prof` of Profunctors as shown by @Asada:2010:ASM:1863597.1863607.
In Standard GHC `(>>>)` has the type
`(>>>) :: Category cat => cat a b -> cat b c -> cat a c` and is therefore not
part of the `Arrow` type class like presented in this thesis.^[For additional information on the type classes used, see: \url{https://hackage.haskell.org/package/profunctors-5.2.1/docs/Data-Profunctor.html} and \url{https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Category.html}.]

~~~~ {#fig:profunctorArrow
    .haskell
    .figure
    caption="Profunctors as Arrows."
    options=h
    }
instance (Category p,
    Strong p) =>
    Arrow p where
  arr f = dimap id f id
  first = first'

instance (Category p,
    Strong p,
    Costrong p) =>
    ArrowLoop p where
  loop = loop'

instance (Category p,
    Strong p,
    Choice p) =>
    ArrowChoice p where
  left = left'
~~~~

## Additional function definitions

\label{app:omitted}

We have omitted some function definitions in the main text for
brevity, and redeem this here.

We begin with Arrow versions of Eden's `shuffle`, `unshuffle` and the definition of
`takeEach` can be found in Figure \ref{fig:edenshuffleetc}. Similarly,
Figure \ref{fig:edenlazyrightrotate} contains the definition of Arrow
versions of Eden's `lazy` and `rightRotate` utility functions.
Figure \ref{fig:lazyzip3etc} contains Eden's definition of `lazyzip3` together
with the utility functions `uncurry3` and `threetotwo`.
The full definition of `farmChunk` is in Figure \ref{fig:farmChunk}.
Eden definition of `ring` skeleton is in Figure \ref{fig:ringEden}. It
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

~~~~ {#fig:ringEden
    .haskell
    .figure
    caption="Eden's definition of the |ring| skeleton."
    options=t
    }
ringSimple :: (Trans i, Trans o, Trans r) => 
    (i -> r -> (o,r)) -> [i] -> [o]
ringSimple f is =  os
  where (os,ringOuts) = unzip (parMap 
            (toRD $ uncurry f) (zip is $ lazy ringIns))
        ringIns = rightRotate ringOuts

toRD :: (Trans i, Trans o, Trans r) => 
    ((i,r) -> (o,r)) -> ((i, RD r) -> (o, RD r))
toRD  f (i, ringIn)  = (o, release ringOut)
  where (o, ringOut) = f (i, fetch ringIn)

rightRotate    :: [a] -> [a]
rightRotate [] =  []
rightRotate xs =  last xs : init xs

lazy :: [a] -> [a]
lazy ~(x:xs) = x : lazy xs
~~~~

Furthermore, Figure \ref{fig:torus_example_rest} contains the omitted definitions
required for parallel matrix multiplication with the `torus` skeleton.
They are: `prMM` (sequential matrix multiplication), `splitMatrix`
(which splits a matrix into chunks), `staggerHorizontally` and
`staggerVertically` (to pre-rotate the matrices), and lastly `matAdd`,
which calculates $A + B$ for two matrices $A$ and $B$.

~~~~ {#fig:torus_example_rest
    .haskell
    .figure
    caption="|prMMTr|, |splitMatrix|, |staggerHorizontally|, |staggerVertically| and |matAdd| definition."
    options=t
    }
prMM :: Matrix -> Matrix -> Matrix
prMM m1 m2 = prMMTr m1 (transpose m2)
  where
    prMMTr m1' m2' = 
        [[sum (zipWith (*) row col) | col <- m2' ] | row <- m1']

splitMatrix :: Int -> Matrix -> [[Matrix]]
splitMatrix size matrix = 
    map (transpose . map (chunksOf size)) $ chunksOf size $ matrix

staggerHorizontally :: [[a]] -> [[a]]
staggerHorizontally matrix = zipWith leftRotate [0..] matrix

staggerVertically :: [[a]] -> [[a]]
staggerVertically matrix = 
    transpose $ zipWith leftRotate [0..] (transpose matrix)

leftRotate :: Int -> [a] -> [a]
leftRotate i xs = xs2 ++ xs1 where
    (xs1,xs2) = splitAt i xs

matAdd = chunksOf (dimX x) $ zipWith (+) (concat x) (concat y)
~~~~

## Syntactic sugar

\label{syntacticSugar}

Next, we also give the definitions for some syntactic sugar for PArrows,
namely `parstar` and `parand`.

For basic Arrows, we have the `***` combinator.
(Figure \ref{fig:syntacticSugarArrows}) It allows us to combine two Arrows
`arr a b` and `arr c d` into an Arrow `arr (a, c) (b, d)` which does both
computations at once. This can easily be translated into a parallel version
`parstar` with the use of `parEval2`, but for this we require a backend which
has an implementation that does not require any configuration
^[Hence the `()` as the `conf` parameter.]:

~~~~ {.haskell}
(|***|) :: (ArrowChoice arr, ArrowParallel arr (Either a c) (Either b d) ())) =>
	arr a b -> arr c d -> arr (a, c) (b, d)
(|***|) = parEval2 ()
~~~~

We define the parallel `parand` in a similar manner to its sequential
pendant `&&&` (Figure \ref{fig:syntacticSugarArrows}):

~~~~ {.haskell}
(|&&&|) :: (ArrowChoice arr, ArrowParallel arr (Either a a) (Either b c) ()) =>
	arr a b -> arr a c -> arr a (b, c)
(|&&&|) f g = (arr $ \a -> (a, a)) >>> f |***| g
~~~~
