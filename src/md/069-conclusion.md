# Conclusion

\label{sec:conclusion}

Arrows are a generic concept that allows for powerful composition
combinators. To our knowledge we are first to represent
*parallel* computation with Arrows, and hence to show their usefulness for
composing parallel programs. We have shown that for a generic and extensible
parallel Haskell, we do not have to restrict ourselves to a monadic interface.
We argue that Arrows are better suited to parallelise
pure functions than Monads, as the functions are already Arrows and can be used
directly in our DSL.
Arrows are a better fit to parallelise pure code than a monadic solution as
regular functions are already Arrows and can be used with our DSL in a more natural
way.
We use a non-monadic interface (similar to Eden or GpH) and retain composability.
The DSL allows for a direct parallelisation of monadic code via the Kleisli type
and additionally allows to parallelise any Arrow type that has an instance for
`ArrowChoice`. (Some skeletons require an additional `ArrowLoop` instance.)

We have demonstrated the generality of the approach by exhibiting PArrow
implementations for Eden, GpH, and the `Par` Monad. Hence, parallel programs can
be ported between task parallel Haskell implementations with little or no effort.
We are confident that it will be straightforward to add other task-parallel Haskells.
Our measurements of four benchmarks on both shared and distributed memory
platforms shows that the generality and portability of PArrows has very low
performance overheads, i.e. never more than $8\% \; \pm 6.9\%$ and typically
under $2\%$.

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
