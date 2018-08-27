# Introduction
\label{sec:introduction}

In recent years, functional programming has been on the rise as functional
languages like Haskell, Scala, Lisp or their derivatives have seen an increase in popularity.
Furthermore, imperative languages are adopting features
originally coming from functional languages. So, even Java, which is
generally not associated with introducing new features quickly, has officially
embraced at least some functional concepts such as Lambdas and the functional
interfaces in the standard library. Many new concepts such as
the streaming API rely heavily on these new-to-Java ideas.
Other languages such as C++, C# or Python show an even greater influence of functional
paradigms as they all have been seen improving their support for a functional style of programming,
even though they can not be considered pure functional languages by any means.

This rise in popularity does not come from nowhere. The core benefit
of functional programming, its modularity, allows programmers
to write concise programs in a clear and structured way.

Functional languages, coming from an academic environment,
historically also have a long history of being used for 
experimenting with novel programming paradigms. Among these is
the use of functional languages for parallel programming.

In Haskell, which we focus on in this thesis, there already exist several
ways to write parallel programs.
One approach that has not been explored in depth yet, however, is to represent
parallel computations with Arrows. Nonetheless, they seem a natural fit since they can be
thought of as a more general function arrow
(`->`) and serve as a general interface to computations while not being as
restrictive as Monads [@HughesArrows].

This is why in this thesis
we will explain how a parallel Haskell based on parallel Arrows (PArrows) can be achieved.
We do however not want to re-invent parallelism, though, as we only provide an
Arrow based type class hosting an Arrow combinator
`parEvalN :: [arr a b] -> arr [a] [b]` that turns a list of Arrows into a
singular parallel Arrow. We use this class as an interface to wrap around existing
parallel Haskells instead of introducing yet another new low-level parallel
backend.

For this thesis we have chosen three of the most important parallel Haskells:
GpH, or more specifically -- its Multicore SMP implementation, [@Trinder1996; @Trinder1998a] for its simplicity,
the classic `Par` Monad [@par-monad; @Foltzer:2012:MPC:2398856.2364562]
to represent a monadic DSL, and Eden [@eden; @Loogen2012] as a
distributed memory parallel Haskell targeting clusters running MPI or PVM.
These languages represent orthogonal approaches.
Some use a Monad, even if only for the internal representation
while others introduce additional language constructs.

While alternatives such as HdpH [@Maier:2014:HDS:2775050.2633363; @stewart_maier_trinder_2016, a Template Haskell-based parallel Haskell for distributed memory]
and LVish [@Kuper:2014:TPE:2666356.2594312, a `Par` extension with focus on communication] exist,
these were not chosen. The former does not differ from the original
`Par` Monad with regard to how we would have used it in this thesis,
while the latter (at least in its current form) does not suit a wrapping approach
such as the one presented here due to its heavy reliance on Template Haskell^[a Haskell extension that allows to generate code]. We will
experiment with a potential Cloud Haskell [@Epstein:2011:THC:2096148.2034690] backend
to someday enable the DSL to work in modern cloud based clusters, though.

With the shallow--embedded DSL based on Arrows we define in this thesis
we do however not only aim to define a novel parallel programming
interface that allows for arbitrary Arrow types to be parallelised --
we also aim to tame the zoo of parallel Haskells.
Even more so, we want to provide a common,
low-penalty programming interface that is general by allowing
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
skeletons. We describe an experimental Cloud Haskell based backend in Chapter \ref{sec:cloudHaskellExperiment}.
Chapter \ref{sec:benchmarks} evaluates the performance of the PArrows DSL.
Chapter \ref{sec:discussion} discusses our results and Chapter \ref{sec:conclusion} concludes.
