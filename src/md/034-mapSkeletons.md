## Basic `map`-based skeletons

\label{sec:skeletons}
\label{sec:map-skeletons}

Now we have developed Parallel Arrows far enough to define some useful
algorithmic skeletons that abstract typical parallel computations.
We start with some basic `map`-based skeletons.
The essential differences between these skeletons presented here 
are in terms of order of evaluation and work distribution. They nevertheless
still provide the same semantics as a sequential `map`.

### Parallel `map` and laziness

The `parMap` skeleton (Figs. \ref{fig:parMap}, \ref{fig:parMapImg})
is probably the most common skeleton for parallel programs.
We can implement it with `ArrowParallel` by repeating an Arrow `arr a b` and
then passing it into `parEvalN` to obtain an Arrow `arr [a] [b]`.

~~~~ {#fig:parMap
    .haskell
    .figure
    caption="|parMap| definition."
    options=h
    }
parMap :: (ArrowParallel arr a b conf) =>
    conf -> (arr a b) -> (arr [a] [b])
parMap conf f = parEvalN conf (repeat f)
~~~~

Just like `parEvalN`, `parMap` traverses all input Arrows as well as the inputs.
Because of this, it has the same restrictions as `parEvalN` as compared to
`parEvalNLazy`. So it makes sense to also have a `parMapStream`
(Figs. \ref{fig:parMapStream}, \ref{fig:parMapStreamImg}) which behaves like
`parMap`, but uses `parEvalNLazy` instead of `parEvalN`.

~~~~ {#fig:parMapStream
    .haskell
    .figure
    caption="|parMapStream| definition."
    options=h
    }
type ChunkSize = Int

parMapStream :: (ArrowParallel arr a b conf,
    ArrowChoice arr,
    ArrowApply arr) =>
	conf -> ChunkSize -> arr a b -> arr [a] [b]
parMapStream conf chunkSize f = parEvalNLazy conf chunkSize (repeat f)
~~~~

![`parMap` depiction.](src/img/parMap.pdf){#fig:parMapImg}

![`parMapStream` depiction.](src/img/parMapStream.pdf){#fig:parMapStreamImg}

### Statically load-balancing parallel `map`

Our `parMap` spawns every single computation in a new thread
(at least for the instances of `ArrowParallel` we presented in this thesis).
This can be quite wasteful and a statically load-balancing `farm`
(Figs. \ref{fig:farm}, \ref{fig:farmImg}) that equally distributes the
workload over `numCores` workers seems useful.
The definitions of the helper functions `unshuffle`, `takeEach`,
`shuffle` (Fig. \ref{fig:edenshuffleetc}) originate from an Eden
skeleton^[Available on Hackage under \url{https://hackage.haskell.org/package/edenskel-2.1.0.0/docs/src/Control-Parallel-Eden-Map.html}.].

~~~~ {#fig:farm
    .haskell
    .figure
    caption="|farm| definition."
    options=h
    }
type NumCores = Int

farm :: (ArrowParallel arr a b conf,
	ArrowParallel arr [a] [b] conf,
	ArrowChoice arr) =>
	conf -> NumCores -> arr a b -> arr [a] [b]
farm conf numCores f =
	unshuffle numCores >>>
	parEvalN conf (repeat (mapArr f)) >>>
	shuffle
~~~~

Since a `farm` is basically just `parMap` with a different work distribution,
it has the same restrictions as `parEvalN` and `parMap`.
We can, however, define `farmChunk` (Figs. \ref{fig:farmChunk}, \ref{fig:farmChunkImg})
which uses `parEvalNLazy` instead of `parEvalN`.
Its definition is identical to the one for `farm` apart from the use of
`parEvalNLazy` instead of `parEvalN`.

~~~~ {#fig:farmChunk
    .haskell
    .figure
    caption="|farmChunk| definition."
    options=h
    }
type ChunkSize = Int
type NumCores = Int

farmChunk :: (ArrowParallel arr a b conf,
    ArrowParallel arr [a] [b] conf, 
    ArrowChoice arr,
    ArrowApply arr) =>
	conf -> ChunkSize -> NumCores -> arr a b -> arr [a] [b]
farmChunk conf chunkSize numCores f =
	unshuffle numCores >>>
	parEvalNLazy conf chunkSize (repeat (mapArr f)) >>>
	shuffle
~~~~

![`farm` depiction.](src/img/farmImg.pdf){#fig:farmImg}

![`farmChunk` depiction.](src/img/farmChunkImg.pdf){#fig:farmChunkImg}
