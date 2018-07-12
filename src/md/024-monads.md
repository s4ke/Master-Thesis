### Monads

Functional programmers try to avoid mutable state at all cost, but programs
that do not only just compute some function usually involve some sort of it.
So, doesn't this make Haskell useless being be a
pure functional language without *any* mutable state?
No. Functional Programs generally just avoid *unnecessary* mutable state at all cost.
The fact of the matter is that in functional programming, we can represent
mutable state as well, but we do so in a meaningful and controlled manner.

While in most computations, we could represent state,
by passing it into every function that can possibly change it
and returning it alongside of the actual returned value like

~~~~ {.haskell
    }
comp :: MyState -> Int -> (Int, MyState)
comp curState x = (x + 3, nextState) 
    where nextState = changeState curState
~~~~

this can become unnecessarily complicated to handle by hand. A better
alternative is the use of monads, which are the main concept
generally used in computations involving some sort of mutable state.
The typeclass for the `Monad` typeclass can be defined as

~~~~ {.haskell
    }
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
~~~~

Thinking of Monads as computations, we can come up with the following
explanation: `return` is used to create a computation `m a` just returning
some given value `a`. 
Next, `(>>=)` is used to compose some monadic computation `m a`
returning some `a` with a monadic function `a -> m b` to return
some computation `m b` returning some `b`. Finally, `(>>)` is used to
define the order of two monadic computations `m a` and `m b` so that
`m a` is computed before `m b` \enquote{discarding} the result of the first one.

Given this definition of a Monad, we can now take a look at
the well known example for a Monad - the `State` Monad
It is defined as [@learnyouahaskell] as

~~~~ {.haskell
    }
newtype State s a = State { runState :: s -> (a,s) }  
~~~~

where a `State s a` encapsulates a stateful computation on some state type
`s` yielding some value of type `a`.

computations

https://wiki.haskell.org/Monad
