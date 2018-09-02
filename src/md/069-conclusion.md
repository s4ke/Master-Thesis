# Outlook

\label{sec:conclusion}

## Evaluation of results

We will now discuss whether we have achieved the goals 
for our Arrow based parallel Haskells that we have set ourselves in the introduction
in Chapter \ref{sec:introduction}. We there described that we wanted

- a DSL that allows us to parallelise arbitrary Arrow types
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

Summarizing we can say that we succeeded in our goal to provide support for parallelizing
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
parallel Haskell we do not have to restrict ourselves to a monadic interface.

We believe that 
Arrows are a better fit to parallelise pure code than a monadic solution as
regular functions are already Arrows and can be used with our DSL in a more natural
way while retaining all the composability. The benefit of being able to parallelize
arbitrary Arrows that have the proper functionality (`ArrowChoice`/`ArrowLoop`)
in a similar manner is also obvious.
Additionally, the DSL still allows for a direct parallelisation of
monadic code via the Kleisli type.

## Contributions

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

Next, we extended the PArrows formalism with `Future`s to enable direct
communication of data between nodes in a distributed memory setting
similar to Eden's Remote Data [@Dieterle2010]. 
Direct communication is useful in a distributed memory setting because
it allows for inter-node communication without blocking the master-node
(Chapter \ref{sec:futures}).

Subsequently, we demonstrated the expressiveness of PArrows by using them to define
common algorithmic skeletons (Chapters \ref{sec:skeletons}, \ref{sec:topology-skeletons}),
and by using these skeletons to implement four benchmarks
(Chapter \ref{sec:benchmarks}).

We also developed an experimental Cloud Haskell backend in Chapter
\ref{sec:cloudHaskellExperiment} as a possible PArrows backend with support
for the recent trends in cloud computing. This in an early proof of concept stage
at the moment.

Finally, we practically demonstrated that Arrow parallelism has a low performance
overhead compared with existing approaches, with only some negligible performance hits
(Chapter \ref{sec:benchmarks}).

## Conclusion

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
the portability between APIs in their programs our DSL
can be of benefit because of the generality of our approach.
Because we use Arrows to build our DSL, we achieve a common ways of parallelising
computations whether they are simple (`->`) or monadic
functions (`a -> m b`) -- or yet another different computation type.
We can even say that PArrows is not only a general way to paralellise Arrow-based
computations, but also a general way to parallelise *computations* in general.

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

TODO: Alles was in der Evaluation of Results noch aufgekommen ist.
