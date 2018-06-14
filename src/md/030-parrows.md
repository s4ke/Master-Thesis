# Parallel Arrows

\label{sec:parallel-arrows}

While Arrows are a general interface to computation, we introduce here specialised
Arrows as a general interface to *parallel computations*.
We present the `ArrowParallel` type class and explain the reasoning
behind it before discussing some parallel Haskell implementations and basic extensions.

## The `ArrowParallel` type class

A parallel computation (on functions) can be seen as execution of some functions
`a -> b` in parallel, as our `parEvalN` prototype shows
(Section \ref{sec:parEvalNIntro}).
Translating this into Arrow terms gives us a new operator `parEvalN` that lifts
a list of Arrows `[arr a b]` to a parallel Arrow `arr [a] [b]`.
This combinator is similar to `evalN` from Appendix \ref{utilfns}, but does
parallel instead of serial evaluation.
