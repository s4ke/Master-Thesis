# Related Work

\label{sec:related-work}

In this Chapter we will discuss related work to this thesis. We start with
work on parallel Haskells in Chapter \ref{sec:relWorkParallelHaskells}. Next,
we discuss research on algorithmic skeletons in Chapter \ref{sec:relWorkAlgorithmicSkels}.
Then, we go over previous work in the field of Arrows in Chapter \ref{sec:relWorkArrows}.
Finally, we explain how this thesis is related to previous work done on PArrows
during the exploration of the subject.

## Parallel Haskells

\label{sec:relWorkParallelHaskells}

The non-strict semantics of Haskell, and the fact that reduction
encapsulates computations as closures, makes it relatively easy to
define alternate execution strategies. A range of approaches have been explored,
including data parallelism [@Chakravarty2007; @Keller:2010:RSP:1932681.1863582],
GPU-based approaches [@Mainland:2010:NEC:2088456.1863533; @obsidian-phd] and
software transactional memory
[@Harris:2005:CMT:1065944.1065952; @Perfumo:2008:LST:1366230.1366241].
The Haskell--GPU bridge Accelerate
[@Chakravarty:2011:AHA:1926354.1926358; @CMCK14; @McDonell:2015:TRC:2887747.2804313]
is completely orthogonal to our approach as it is by nature focused on
parallel execution of singular functions on \enquote{Arrays} of data instead of
parallel execution of multiple functions with their respective inputs. 
A good survey of parallel Haskells can be found in @marlow2013parallel.

Our PArrow implementation uses three task parallel languages as backends:
the GpH [@Trinder1996; @Trinder1998a] parallel Haskell dialect
and its multicore version [@Marlow2009], the `Par` Monad
[@par-monad; @Foltzer:2012:MPC:2398856.2364562], and Eden [@eden; @Loogen2012].
These languages are under active development, for example a combined shared
and distributed memory implementation of GpH is available
[@Aljabri:2013:DIG:2620678.2620682; @Aljabri2015].
Research on Eden includes low-level implementation
[@JostThesis; @berthold_loidl_hammond_2016], skeleton composition
[@dieterle_horstmeyer_loogen_berthold_2016], communication [@Dieterle2010],
and generation of process networks [@Horstmeyer2013].
The definitions of new Eden skeletons is a specific focus
[@doi:10.1142/S0129626403001380; @Eden:PARCO05; @Berthold2009-mr; @Berthold2009-fft; @dieterle2010skeleton; @delaEncina2011; @Dieterle2013; @janjic2013space].

Other task parallel Haskells related to Eden, GpH, and the `Par`
Monad include: HdpH [@Maier:2014:HDS:2775050.2633363; @stewart_maier_trinder_2016] is an extension
of `Par` Monad to heterogeneous clusters. LVish [@Kuper:2014:TPE:2666356.2594312] is a
communication-centred extension of the `Par` Monad and is based on monotonically growing data structures.

Cloud Haskell [@Epstein:2011:THC:2096148.2034690]
which we use for an experimental backend is a domain specific language for
cloud computing but can also be used for parallel computation.

## Algorithmic skeletons

\label{sec:relWorkAlgorithmicSkels}

Algorithmic skeletons as a concept to abstract the general idea of different classes of
algorithms were introduced by @Cole1989.
Early publications on this topic include @DANELUTTO1992205, @darlington1993parallel, @botorog1996efficient, @Lengauer1997
and @Gorlatch1998. 
@SkeletonBook consolidated early reports on high-level programming approaches.
Types of algorithmic skeletons include `map`-, `fold`-, and `scan`-based parallel
programming patterns, special applications such as divide-and-conquer or
topological skeletons.

The `farm` skeleton [@Hey1990185; @Eden:PPDP01; @Kuchen05] is a statically 
task-balanced parallel `map`. When tasks' durations cannot be foreseen,
a dynamic load balancing (`workpool`) brings a lot of improvement
[@Rudolph:1991:SLB:113379.113401; @doi:10.1142/S0129626403001380; @Hippold2006; @PADL08HMWS; Marlow2009].
For special tasks `workpool` skeletons can be extended with dynamic task
creation [@WPEuropar06; @Dinan:2009:SWS:1654059.1654113; @brown2010ever].
Efficient load-balancing schemes for `workpool`s are subject of research
[@Blumofe:1999:SMC:324133.324234; @Acar:2000:DLW:341800.341801; @vanNieuwpoort:2001:ELB:568014.379563; @Chase:2005:DCW:1073970.1073974; @4625841; @Michael:2009:IWS:1594835.1504186].

The `fold` (or `reduce`) skeleton was implemented in various skeleton libraries
[@Kuchen2002; @5361825; @BUONO20102095; @Dastgeer:2011:ASM:1984693.1984697],
as also its inverse, `scan` [@Bischof2002; @harris2007parallel].
Google `map`--`reduce` [@Dean:2008:MSD:1327452.1327492; @Dean:2010:MFD:1629175.1629198]
is more special than just a composition of the two skeletons [@LAMMEL20081; @Berthold2009-mr].

The effort is ongoing, including topological skeletons [@Eden:PARCO05],
special-purpose skeletons for computer algebra
[@Berthold2009-fft; @lobachev-phd; @Lobachev2012; @janjic2013space],
and iteration skeletons [@Dieterle2013].
The idea of @scscp is to use a parallel Haskell to orchestrate further
software systems to run in parallel. @dieterle_horstmeyer_loogen_berthold_2016 
compare the composition of skeletons to stable process networks.

We implement some of these algorithmic skeletons
in Chapters \ref{sec:extending-interface} as well as \ref{sec:topology-skeletons}, namely
various parallel `map` variants as well as the topological skeletons `pipe`, `ring` and `torus`.

## Arrows

\label{sec:relWorkArrows}

Arrows were introduced by @HughesArrows as a less restrictive alternative to Monads,
in essence they are a generalised function arrow `->`. @Hughes2005 presents a
tutorial on Arrows. @jacobs_heunen_hasuo_2009, @LINDLEY201197, @ATKEY201119 develop
theoretical background of Arrows. [@Paterson:2001:NNA:507669.507664] introduced a
new notation for Arrows. Arrows have applications in information flow research
[@1648705; @LI20101974; @Russo:2008:LLI:1411286.1411289],
invertible programming [@Alimarine:2005:BAA:1088348.1088357],
and quantum computer simulation [@vizzotto_altenkirch_sabry_2006].
But probably most prominent application of Arrows is Arrow-based functional
reactive programming, AFRP [@Nilsson:2002:FRP:581690.581695; @Hudak2003; @Czaplicki:2013:AFR:2499370.2462161].
[@Liu:2009:CCA:1631687.1596559] formally define a more special kind of
Arrows that encapsulate the computation more than regular Arrows do and thus
enable optimisations. Their approach would allow parallel composition,
as their special Arrows would not interfere with each other in concurrent execution.
In contrast, we capture a whole parallel computation as a single entity: our main
instantiation function `parEvalN` creates a single (parallel) Arrow out of a list of Arrows.
@Huang2007 utilise Arrows for parallelism, but strikingly different from our approach.
They use Arrows to orchestrate several tasks in robotics.
We, however, propose a general interface for parallel programming,
while remaining completely in Haskell.

#### Arrows in other languages

Although this work is centred on Haskell implementation of Arrows,
it is applicable to any functional programming language where parallel
evaluation and Arrows can be defined. Basic definitions of PArrows are
possible in the Frege language^[GitHub project page at \url{https://github.com/Frege/frege}]
-- a Haskell-like language that compiles to Java code to then be compiled natively on the Java Virtual Machine (JVM).
However, they are beyond the scope of this work,
as are similar experiments with the Eta language^[Eta project page at \url{http://eta-lang.org}], 
a new approach to Haskell on the JVM that compiles directly to JVM bytecode.

Arrows have also been shown to be useful for better handling of typical
 GUI tasks in Clean [@achten2004arrows; @achten2007arrow].
@Dagand:2009:ORD:1481861.1481870 used Arrows in OCaml in the implementation
of a distributed system.

## Earlier work

This thesis is based on the paper \enquote{Arrows for Parallel Computation} [@PArrowsPaper]
written by me, Martin Braun, together with Oleg Lobachev and Phil Trinder. The original idea for this paper
came up during a Masters Research Project done under the supervision of Oleg Lobachev.

While based on the ideas of the Master's research project,
the work presented here is dramatically different. In this thesis, the results
are a lot more sophisticated and detailed and only Chapters \ref{sec:parallelHaskells} -- \ref{sec:map-skeletons}
are somewhat based on work done during the original project.

Similarities to the paper however, are not
coincidental. Chapters \ref{sec:related-work}, \ref{sec:arrows} -- \ref{sec:further-development} as well as Chapter \ref{sec:benchmarks}
were taken from the paper and embedded into the rest of the text. The introduction in Chapter
\ref{sec:introduction} as well as the conclusion in \ref{sec:conclusion}
are also inspired by the paper. But as I, Martin Braun, am the primary author of the paper,
this still means that the following work was done independently as required by studying regulations.



