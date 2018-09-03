# Outlook

\label{sec:outlook}

Finally, in this chapter we finish up this work on the PArrows DSL.
In Chapter \ref{sec:outlookContribs} we point our contributions.
In Chapter \ref{sec:outlookEval} we evaluate these on the basis of
the goals we have set ourselves in the Introduction. Chapter 
\ref{sec:outlookConclusion} concludes the work and also
points out future work.

## Contributions

\label{sec:outlookContribs}

In this thesis, we proposed an Arrow-based encoding for parallelism based on 
a new Arrow combinator `parEvalN :: [arr a b] -> arr [a] [b]`.
A parallel Arrow is still an Arrow, hence the resulting parallel
Arrow can still be used in the same way as a potential sequential version.
We evaluated the expressive power of such a formalism
in the context of parallel programming.

One big advantage of this specific approach is that we do not
have to introduce any new types, facilitating composability
(Chapter \ref{sec:parallel-arrows}).
These PArrow programs can readily exploit multiple parallel
language implementations. We demonstrated the use of GpH,
a `Par` Monad, and Eden. We did not re-implement all the parallel internals,
as this functionality is hosted in the `ArrowParallel` type class,
which abstracts all parallel implementation logic.
The implementations can easily be swapped, so we are not bound to any specific one.

Next, we extended the PArrows formalism even further
(Chapter \ref{sec:further-development}). We used
`Future`s to enable direct
communication of data between nodes in a distributed memory setting
similar to Eden's Remote Data [@Dieterle2010]. 
Direct communication is useful in a distributed memory setting because
it allows for inter-node communication without blocking the master-node
(Chapter \ref{sec:futures}).
Subsequently, we demonstrated the expressiveness of PArrows by using them to define
common algorithmic skeletons (Chapters \ref{sec:skeletons}, \ref{sec:topology-skeletons}),
and by using these skeletons to implement four benchmarks
(Chapter \ref{sec:benchmarks}).

We also developed an experimental Cloud Haskell backend (Chapter
\ref{sec:cloudHaskellExperiment}) as a possible PArrows backend with support
for the recent trends in cloud computing. This in an early proof of concept stage
at the moment.

Finally, we practically demonstrated that Arrow parallelism has a low performance
overhead compared with existing approaches, with only some negligible performance hits
(Chapter \ref{sec:benchmarks}).

## Evaluation of results

\label{sec:outlookEval}

In the Introduction, we have set ourselves goals for our DSL. We wanted

- a DSL that allows us to parallelise arbitrary Arrow types
- to tame the zoo of parallel Haskells.
- low performance penalty
- generality by being able to switch implementations at will

We will now go into detail on whether we have met these requirements.

#### Parallelizing arbitrary Arrow Types

Our PArrows API generally succeeded in providing parallelism for arbitrary Arrow
types, but only those which fulfill certain type classes. There are two restrictions
to that, though:

- Most of our API
as well as the actual instances for our type class `ArrowParallel` in the
case of GpH and the `Par` Monad require an `ArrowChoice` instance. Additionally,
we require `ArrowLoop` for the looping skeletons. While this indeed restricts the
set of suitable Arrows, it is not a restriction in which Arrows we can parallelize
specifically, but more of a restriction on what type of Arrow can be parallelized.
Furthermore, both type classes are an integral part in the code that uses them.
For example, without `ArrowChoice` we could not express the recursion with Arrows as
without it we can not express the recursion anchor in e.g. the definition
of `evalN` (Figure \ref{fig:evalN}). The same goes for `ArrowLoop` as it
is required in order to have the know-tying fix-point semantics we require for
our topology skeletons. In fact, @Hughes2005 mentions this as well in writing
\enquote{there is little interesting that can be done without
more operations on Arrows than just composition}.

- The Eden backend currently has no general implementation for `ArrowParallel`.
As explained earlier in Chapter \ref{sec:parrows-Eden}, the reason for this is that Eden's spawn function `spawnF`
only works on function (`(->)`) and therefore resorted to having manual implementations
of `ArrowParallel` for every type. As noted in the same Chapter however, this
seems to be no real issue as a possible general implementation is possible. This
has to be evaluated with more involved tests in the future, though.

Summarizing we can say that we mostly succeeded in our goal to provide support for parallelizing
arbitrary Arrows as these two restrictions are no big issues at all as we have explained here. 

#### Taming the zoo of parallel Haskells

In this thesis, we showed how we can tame at least the three parallel Haskells
we used as backends -- GpH, the `Par` Monad and Eden. We even included
the blue print for a new backend based upon Cloud Haskell. Therefore, we are confident that
other parallel Haskells can be used as backends in our DSL even if they require
some special care.^[Like we have talked about in the case of HdpH, which heavily relied
on Template Haskell to work. This Template Haskell code was however incompatible
with PArrows and would need replacing were HdpH used as a backend] With the PArrows DSL we are therefore able to
tame the zoo of parallel Haskells.

#### Low Penalty interface

As our benchmarks in Chapter \ref{sec:benchmarks} show, our PArrows DSL did only
induce very low overhead -- i.e. never more than $8\% \; \pm 6.9\%$ and typically
under $2\%$. Typically, the mean over all
cores of relative mean overhead was less than $3.5\%$ and less than $0.8\%$
for all benchmarks with GpH and Eden, respectively. As for the `Par` Monad,
the mean of mean overheads was in favour of PArrows in all benchmarks
(Chapter \ref{sec:benchmarks}).
The PArrows DSL is therefore a very low penalty interface when compared
to the native backends.

#### Generality -- Switching Implementations at will

Because of the way we designed our central `ArrowParallel` type class
in Chapter \ref{sec:parallel-arrows-type-class}, we can truly switch between
backend implementations at will and therefore programs are portable
between parallel Haskell implementations. The only thing that has to be done
when switching between implementations, 
is changing the import statement.
Implementation specifics such as different config types are well hidden
away from the user with the help of default configuration instances, but
can be accessed if required. The only thing that is not covered
by our interface is the need for specific transport logic for
distributed backends for non standard
data types (like Eden or in the experimental Cloud Haskell backend).
However, these
are easily implemented with default instances (`Trans` in Eden) or
Template Haskell (Cloud Haskell).

The only problem we currently have in terms of PArrows' generality is that the
implementations do not behave the same when it comes to the exact behaviour 
of `parEvalN`. So, the GpH, `Par` Monad and the experimental Cloud Haskell
implementations of `parEvalN` do not work as required for at least the topological
skeletons we showed in this thesis.
Even though we provided a work-around for the sake of compatibility 
with the `ArrowLoopParallel` type class, this can only be seen as temporary.
This problem has to be investigated further and fixed by standardizing
the way `parEvalN` behaves.

#### Summary

Summarizing we can say that we have fulfilled the requirements set in the
Introduction of this thesis. PArrows is a DSL
that allows us to parallelize arbitrary Arrow types that allows us
to tame the zoo of parallel Haskells while having a low performance penalty
and is general by allowing to switch implementations at will.
As described, there are some problems, however, these will have to
be amended in the future.

## Conclusion

\label{sec:outlookConclusion}

Arrows are a generic concept that allows for powerful composition
combinators. To our knowledge we are first to represent
*parallel* computation with Arrows, and hence to show their usefulness for
composing parallel programs.

In our opinion, a way to parallelise Arrows directly with a DSL like ours was
overdue. They serve
as a general interface to computation, so why not also to *parallel* computation?
Equally importantly, the DSL can serve as a interface for 
parallelism in general. For years, the maintainers of the
different APIs
have been developing their own brand of parallelism without caring about portability,
which is a big concern if an application has to be ported to
another eco-system.
This is definitely not only a theoretical argument.
In the real world, projects can become
deprecated or cause other incompatibilities -- the same goes for parallel APIs.
Also, sometimes parallel programs that
were written to be run on a single machine have to be ported
to a distributed environment. Now, if a program that is to be ported were
based on our *shallow* DSL, these
problems would basically be non-existent.
Programmers could simply change the backend and
continue their work in other areas that matter.

Even for programmers that do not care for 
the portability between APIs in their programs the PArrows DSL
can be of benefit because of the generality of our approach.
Because we use Arrows to build our DSL, we achieve a common ways of parallelising
computations whether they are simple (`->`) or monadic
functions (`a -> m b`) -- or yet another different computation type.
We can even say that PArrows is not only a general way to paralellise Arrow-based
computations, but also a general way to parallelise *computations* in general.

#### Future work

\label{sec:future-work}

Our PArrows DSL can be expanded to other task parallel Haskells as we have seen in
the Cloud Haskell experiment. It is a primary focus of further development as it will
also help investigating the biggest problem of our DSL -- the difference in behaviour
of `parEvalN` across the backends. In Chapter \ref{sec:CloudHaskellArrowParallelLimitsMitigation}
we already proposed a possible fix for the Cloud Haskell backend. Fixing this
specifically for Cloud Haskell will help us understand the problem and enable
us to amend the GpH and `Par` Monad backends as well.

In other future work, we see a big potential in getting HdpH [@Maier:2014:HDS:2775050.2633363]
to work with our interface.
Furthermore, additional Future-aware versions of Arrow combinators can be defined.
Existing combinators could also be improved, for example more specialised
versions of `>>>` and `***` combinators are viable.

Another area of interest is the expansion expanding of 
both our skeleton library as well as the number of skeleton-based parallel
programs that
use our DSL. It would also be interesting to see a hybrid of PArrows and
Accelerate [@McDonell:2015:TRC:2887747.2804313].


