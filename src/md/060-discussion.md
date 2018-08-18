# Discussion

\label{sec:discussion}

We will now discuss whether we have achieved the goals 
for our Arrow based parallel Haskells that we have set ourselves in the introduction
in Chapter \ref{sec:introduction}. We there described that we wanted

- a DSL that allows us to parallelize arbitrary Arrow types
- to tame the zoo of parallel Haskells.
- low performance penalty
- generality by being able to switch implementations at will

We will now discuss whether we have met these requirements in the results presented by
this thesis. 

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
of `evalN` (Fig. \ref{fig:evalN}). The same goes for `ArrowLoop` as it
is required in order to have the know-tying fix-point semantics we require for
our topology skeletons. In fact, @Hughes2005 mentions this as well in writing
\enquote{there is little interesting that can be done without
more operations on arrows than just composition}.

- The Eden backend currently has no general implementation for `ArrowParallel`.
As explained earlier in Chapter \ref{sec:parrows-Eden}, the reason for this is that Eden's spawn function `spawnF`
only works on function (`(->)`) and therefore resorted to having manual implementations
of `ArrowParallel` for every type. As noted in the same Chapter however, this
seems to be no real issue as a possible general implementation is possible. This
has to be evaluated with more involved tests in the future, though.

Summarizing we can say that we succeeded in our goal to provide support for parallelizing
arbitrary Arrows as these two restrictions are no big issues at all as we have explained here. 

#### Taming the zoo of parallel Haskells

In this thesis, we at least showed how we can tame at least the three parallel Haskells
we used as backends -- GpH, the `Par` Monad and Eden. We even included
the blue print for a new backend based upon Cloud Haskell. Therefore, we are confident that
other parallel Haskells can be used as backends in our DSL even if they require
some special care.^[like we have talked about in the case of HdpH, which heavily relied
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
by our implementations is the need for specific transport logic for
distributed backends for non standard
data types (like Eden or in the experimental Cloud Haskell backend). These
are easily implemented with default instances (`Trans` in Eden) or
Template Haskell (Cloud Haskell).

#### Summary

Summarizing we can say that we have fulfilled the requirements set in the
Introduction of this thesis. PArrows is a DSL
that allows us to parallelize arbitrary Arrow types that allows us
to tame the zoo of parallel Haskells while having a low performance penalty
and is general by allowing to switch implementations at will.

By proving these properties,
we have shown that for a generic and extensible
parallel Haskell, we do not have to restrict ourselves to a monadic interface.

We believe that 
Arrows are a better fit to parallelise pure code than a monadic solution as
regular functions are already Arrows and can be used with our DSL in a more natural
way while retaining all the composability. The benefit of being able to parallelize
arbitrary Arrows that have the proper functionality (`ArrowChoice`/`ArrowLoop`)
in a similar manner is also obvious.
Additionally, the DSL still allows for a direct parallelisation of
monadic code via the Kleisli type.
