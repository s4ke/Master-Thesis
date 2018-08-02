### Monads

\label{sec:monads}

Functional programmers try to avoid mutable state at all cost, but programs
that do not only just compute some function usually involve some sort of it.
So, doesn't this make Haskell useless being be a
pure functional language without *any* mutable state?
No. Functional Programs generally just avoid *unnecessary* mutable state at all cost.
The fact of the matter is that in functional programming, we can represent
mutable state as well, but we do so in a meaningful and controlled manner.

While in most computations, we could represent state
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
    m >> k = m >>= \_ -> k
    return :: a -> m a
~~~~

Thinking of Monads as computations, we can come up with the following
explanation: `return` is used to create a computation `m a` just returning
some given value `a`. 
Next, `(>>=)` is used to compose some monadic computation `m a`
returning some `a` with a monadic function `a -> m b` to return
some computation `m b` returning some `b`. Finally, `(>>)` is used to
define the order of two monadic computations `m a` and `m b` so that
`m a` is computed before `m b` while discarding the result of the first one as can
also be seen in its default implementation above.

Given this definition of a Monad, we can now take a look at how we would implement
a `State` Monad.
It is defined as [@learnyouahaskell]

~~~~ {.haskell
    }
newtype State s a = State { runState :: s -> (a, s) }  
~~~~

where a `State s a` encapsulates a stateful computation
on some state type `s` yielding some value of type `a`. For easier understanding
it is often useful to think of `State s a` just as a usability wrapper around a
function `s -> (a, s)` that returns some `a` and the final state `s`
if we pass it some starting state `s`. The State 
monad therefore only contains the \enquote{blueprint} of the computation that can
only be run if we start it by passing a state.
The instance for the Monad type class can then be defined as

~~~~ {.haskell
    }
instance Monad (State s) where
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState
    return x = State { runState = \s -> (x, s) }   
~~~~

where we declare the Monad deliberately on top of `State s` meaning that `State` itself
is not a monad, but it is a monad together with some state representation `s` ^[We can't
declare `State` a monad anyways since the Monad is a type class with just one type parameter].
Note how the operations are defined here: `return` encapsulates the given value `x :: a`
inside the internal function and therefore is equal to the identity `id :: a -> a` function on tuples
with one parameter already applied.^[With the help of `curry :: ((a, b) -> c) -> a -> b -> c`,
we could have therefore also written `return x = State { runState = (curry id) x }`]
In the composition operator `>>=`, the monadic computation 
`State h :: State s a` is composed with the function `f :: a -> State s b` into
a new monadic computation of type `State s b`. The internal function of the state
is essentially taken out of the first argument and composed with the second
argument inside the returned Monad.

Additionally, we have helper operations to use
this construct with. The first is `put :: s -> State s ()` which overwrites the
current state returning a unit `()` as result:

~~~~ {.haskell
    }
put :: s -> State s ()
put newState = State { runState = \s -> ((), newState) }
~~~~

The second one is `get :: State s s` which returns the current state, but
does not change it:

~~~~ {.haskell
    }
get :: State s s
get = State { runState = \s -> (s, s) }
~~~~

With these operations, we can easily write stateful programs like this one
^[inspired and adapted from
\url{https://gist.github.com/sdiehl/8d991a718f7a9c80f54b}]:

~~~~ {.haskell
    }
type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = get >>= (\(x:xs) -> put xs >> return x)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

peek :: State Stack Int
peek = get >>= \(x:xs) -> return x

computeStateful :: State Stack Int
computeStateful = push 10 >>
                  push 20 >>
                  pop >>= \a ->
                    (pop >>= \b -> push (a + b)) >>
                  peek 

-- main program inside the IO monad          
main :: IO ()
main = print (evalState computeStateful empty)      
~~~~

Here, `computeStateful` first pushes some values on top of a stack represented by a list
`[Int]` (the actual state inside of the `State` monad) and then `pop`s these values and `push`es their sum
back on the stack to finally `peek` the actual value which
then is the result of the computation.
To make writing such code easier, Haskell has syntactic sugar called `do` notation.
With it we can write the above method `computeStateful` in a way that resembles
imperative-style code (but with side-effects clearly encapsulated) as:

~~~~ {.haskell}
computeStateful :: State Stack Int
computeStateful = do
    push 10
    push 20
    a <- pop
    b <- pop
    push (a + b)
    peek 
~~~~

In this example, we can also see the direct relationship between `(>>)` and simple new lines and
the `(>>=)` operator and the special `<-` operator in do notation which facilitates the 
binding to a variable.^[`(>>=)` is also often called `bind` in languages which do not support
operator overloading.]

Other often used Monads in the Haskell eco-system include the `Writer` monad, which is
useful for e.g. logging or the `IO` monad, which is
used to encapsulate I/O computations as well as low level internal operations
such as modifiable variables `IORef` or `MVar` among others.
Furthermore, as one of many other applications,
Monads are used in some parallel Haskells as we will see later in this thesis.
