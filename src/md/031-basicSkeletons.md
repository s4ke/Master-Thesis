## Extending the interface

\label{sec:extending-interface}

With the `ArrowParallel` type class in place, we
can now define other parallel interface functions. These are basic
algorithmic skeletons that are used to define more sophisticated ones
later in this thesis.

### Lazy `parEvalN`

![`parEvalNLazy` depiction.](src/img/parEvalNLazy.pdf){#fig:parEvalNLazyImg}

The resulting Arrow of `parEvalN` fully traverses the list of input Arrows as
well as their inputs. Sometimes this might not be feasible, as
it will not work on infinite lists of Arrows/functions like e.g. `map (arr . (+)) [1..]`
or just because in case we need the Arrows evaluated in chunks. `parEvalNLazy`
(Figs. \ref{fig:parEvalNLazyImg}, \ref{fig:parEvalNLazy}) fixes this.
It works by first chunking the input from `[a]` to `[[a]]` with the given
`chunkSize` in `arr (chunksOf chunkSize)`.
These chunks are then fed into a list `[arr [a] [b]]` of chunk-wise parallel
Arrows with the help of our lazy and sequential `evalN`. The resulting `[[b]]`
is lastly converted into `[b]` with `arr concat`.

~~~~ {#fig:parEvalNLazy
    .haskell
    .figure
    caption="Definition of |parEvalNLazy|."
    options=t
    }
type ChunkSize = Int

parEvalNLazy :: (ArrowParallel arr a b conf, ArrowChoice arr) =>
	conf -> ChunkSize -> [arr a b] -> (arr [a] [b])
parEvalNLazy conf chunkSize fs =
	arr (chunksOf chunkSize) >>>
    evalN fchunks >>>
    arr concat
    where
      fchunks = map (parEvalN conf) (chunksOf chunkSize fs)
~~~~

### Heterogeneous tasks

![`parEval2` depiction.](src/img/parEval2Img.pdf){#fig:parEval2Img}

We have only talked about the parallelization of Arrows of the
same input and output types until now. But sometimes we
want to parallelise heterogeneous types as well.
We can implement such a `parEval2` combinator
(Figs. \ref{fig:parEval2Img}, \ref{fig:parEval2}) which combines two Arrows
`arr a b` and `arr c d` into a new parallel Arrow `arr (a, c) (b, d)`
quite easily with the help of the `ArrowChoice` type class.
Here, the general idea is to use the `+++` combinator which combines
two Arrows `arr a b` and `arr c d` and transforms them into
`arr (Either a c) (Either b d)` to get a common Arrow type that we can
then feed into `parEvalN`.
 
We can implement this idea as follows:
Starting off, we transform the `(a, c)` input into a two-element list
`[Either a c]` by first tagging the two inputs with `Left` and `Right` and wrapping
the right element in a singleton list with `return` so that we can combine them
with `arr (uncurry (:))`. Next, we feed this list into a parallel Arrow running
on two instances of `f +++ g`. After the calculation
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
