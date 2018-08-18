# Conclusion

\label{sec:conclusion}

Arrows are a generic concept that allows for powerful composition
combinators. To our knowledge we are first to represent
*parallel* computation with Arrows, and hence to show their usefulness for
composing parallel programs.

In this thesis, we proposed an Arrow-based encoding for parallelism based on 
a new Arrow combinator `parEvalN :: [arr a b] -> arr [a] [b]`.
A parallel Arrow is still an Arrow, hence the resulting parallel
Arrow can still be used in the same way as a potential sequential version.
We evaluated the expressive power of such a formalism
in the context of parallel programming.

We introduced a parallel evaluation formalism using Arrows.
One big advantage of this specific approach is that we do not
have to introduce any new types, facilitating composability
(Chapter \ref{sec:parallel-arrows}).
These PArrow programs can readily exploit multiple parallel
language implementations. We demonstrated the use of GpH,
a `Par` Monad, and Eden. We did not re-implement all the parallel internals,
as this functionality is hosted in the `ArrowParallel` type class,
which abstracts all parallel implementation logic.
The implementations can easily be swapped, so we are not bound to any specific one.

This has many practical advantages.
For example, during development we can run the program in a
simple GHC-compiled variant using GpH and afterwards deploy it on a
cluster by converting it into an Eden program, by just replacing the
`ArrowParallel` instance and compiling with Eden's GHC variant
(Chapter \ref{sec:parallel-arrows}).

Next, we extended the PArrows formalism with `Future`s to enable direct
communication of data between nodes in a distributed memory setting
similar to Eden's Remote Data [@Dieterle2010]. 
Direct communication is useful in a distributed memory setting because
it allows for inter-node communication without blocking the master-node.
(Chapter \ref{sec:futures})

Subsequently, we demonstrated the expressiveness of PArrows by using them to define
common algorithmic skeletons (Chapters \ref{sec:skeletons}, \ref{sec:topology-skeletons}),
and by using these skeletons to implement four benchmarks
(Chapter \ref{sec:benchmarks}).

We also developed an experimental Cloud Haskell backend in Chapter
\ref{sec:cloudHaskellExperiment} as a possible PArrows backend with support
for the recent trends in cloud computing. This in an early proof of concept stage
at the moment.

Then, we practically demonstrated that Arrow parallelism has a low performance
overhead compared with existing approaches, with only some negligible performance hits
(Chapter \ref{sec:benchmarks}).

Finally, we discussed in Chapter \ref{sec:discussion} how we accomplished in fulfilling the requirements
we deemed important in the introduction, namely
that we wanted a DSL that allows us to parallelize arbitrary Arrow types while
taming the zoo of parallel Haskells and having a low performance penalty all while
being general and allowing to switch implementations at will.

## Future work

\label{sec:future-work}



Our PArrows DSL can be expanded to other task parallel Haskells, and a
specific target is HdpH [@Maier:2014:HDS:2775050.2633363].
Further Future-aware versions of Arrow combinators can be defined.
Existing combinators could also be improved, for example more specialised
versions of `>>>` and `***` combinators are viable.

In ongoing work we are expanding 
both our skeleton library and the number of skeleton-based parallel programs that
use our DSL. It would also be interesting to see a hybrid of PArrows and
Accelerate [@McDonell:2015:TRC:2887747.2804313].
Ports of our approach to other languages such as Frege, Eta, or Java directly
are at an early development stage.
