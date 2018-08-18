# Introduction
\label{sec:introduction}

In recent years, functional programming has been on the rise as functional
languages like Haskell, Scala or Lisp have seen an increase in popularity.
Furthermore, even imperative languages are adopting features
originally coming from functional languages. So, even Java, which is
generally not associated with adopting new features quickly, has officially
embraced at least some functional concepts such as Lambdas or the functional
interfaces in the standard library. Many new concepts concepts like for example
the streaming API rely heavily on these new-to-Java concepts.
Other languages such as C++, C# or Python show an even greater influence of functional
paradigms as they all improve support for a functional style of programming, even
if they can never be considered pure functional languages by any means.

This rise in popularity does not come from nowhere. The core benefit
of functional programming, its modularity, allows programmers
to write concise programs in a clear and structured way.

Functional languages coming out of the environment
of academics, historically also have a long history of being used for 
experimenting with novel programming paradigms. Among them is
the use of functional languages for parallel programming.

In Haskell, which we focus on in this thesis, there already exist several
ways to write parallel programs.
One approach that has not been explored in depth yet, however, is to represent
parallel computations with Arrows. They seem a natural fit as they can be
thought of as a more general function arrow
(`->`) and serve as general interface to computations while not being as
restrictive as Monads [@HughesArrows].

This is why in this thesis
we will explain how an Arrow based parallel Haskell can be achieved.
We do however not want to re-invent parallelism, though as we only provide an
Arrow based and use it an interface to wrap around existing
parallel Haskells instead of introducing yet another new low-level parallel backend.
In this thesis we use this approach to wrap around three parallel Haskells:
Glasgow parallel Haskell or short GpH
(its Multicore SMP implementation, in particular), the
`Par` Monad, and Eden, a distributed memory parallel Haskell targeting
clusters running MPI or PVM. These languages represent orthogonal approaches.
Some use a Monad, even if only for the internal representation.
Some introduce additional language constructs.

With such a shallow-embedded DSL for Arrows we do not only aim to define a parallel programming
interface in a novel manner that allows for arbitrary Arrow types to be parallelised --
we also aim to tame the zoo of parallel Haskells.
With our interface we also aim to provide a common,
very low-penalty programming interface that is general by allowing
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
skeletons. We describe an experimental Cloud Haskell based backend in \ref{sec:cloudHaskellExperiment}.
Chapter \ref{sec:benchmarks} evaluates the performance of the PArrows DSL.
Chapter \ref{sec:discussion} discusses our results and Chapter \ref{sec:conclusion} concludes.
