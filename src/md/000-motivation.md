# Introduction
\label{sec:introduction}

In recent years, functional programming has been on the rise as can be seen in the
growing popularity of languages like Haskell, Scala, or Lisp.
Even imperative languages have been seen adopting features
stemming from functional Languages. So, even Java, which is
generally not associated with adopting trends fast, has officially
embraced at least some functional concepts such as Lambdas or the functional
interfaces in the standard library. Other concepts like for example the
the Streaming API rely heavily on these new-to-Java concepts.
Languages like C++, C# or Python show an even greater influence of functional
paradigms as they all improve support for a functional style of programming, even
if they can never be considered pure functional languages by any means.

This rise in popularity does not come from nowhere. The core benefit
of functional programming, its modularizability allows programmers
to write concise programs in a clear and structured way.

Functional languages coming out of the environment
of academics, historically also have a long history of being used for 
experimenting with novel programming paradigms. Among them is
the use of functional languages for parallel programming.

In Haskell, which we focus on in this thesis, there already exist several
ways to write parallel programs. In this thesis, we regard
in-depth Glasgow parallel Haskell or short GpH
(its Multicore SMP implementation, in particular), the
`Par` Monad, and Eden, a distributed memory parallel Haskell targeting
clusters running MPI or PVM. These languages represent orthogonal approaches.
Some use a Monad, even if
only for the internal representation. Some introduce additional
language constructs.

One approach that has not been explored in depth yet, however, is to represent
parallel computations with Arrows. They seem a natural fit as they can be
thought of as a more general function arrow
(`->`) and serve as general interface to computations while not being as
restrictive as Monads [@HughesArrows].

This is why in this thesis
we will explain how a Arrow based parallel Haskell can be achieved.
We do however not want to re-invent parallelism, though.
We only provide a Arrow based
type class which we use as an interface to wrap around the three above mentioned
parallel Haskells instead of introducing yet another new low-level parallel backend.

With such a shallow-embedded DSL for Arrows we do not only aim to define a parallel programming
interface in a novel manner -- we also aim to tame the zoo of parallel Haskells.
Additionally, the aim is to provide a common, very low-penalty programming interface that allows
to switch the parallel implementations at will.

#### Structure

This thesis is structured as follows. In Chapter \ref{sec:related-work}, we discuss
related work. Chapter \ref{sec:background} covers the background of this thesis
including an introduction to functional programming, a monad tutorial to finally
introduce the concepts of Arrows.
In Chapter \ref{sec:parallel-arrows} we define the shallow-embedded DSL based on Arrows (PArrows)
together with some first basic extensions and `map`-based skeletons.
Chapter \ref{sec:further-development} develops the PArrows API further
by introducing the concept of `Future`s and by giving the definitions of some topology
skeletons. We experiment with a Cloud Haskell based backend in \ref{sec:cloudHaskellExperiment}.
Chapter \ref{sec:benchmarks}, evaluates the performance of the PArrows DSL.
Chapter \ref{sec:discussion} discusses our results and Chapter \ref{sec:conclusion} concludes.



