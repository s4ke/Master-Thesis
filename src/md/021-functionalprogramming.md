## Functional Programming

\label{sec:fuproHaskell}

### Why Functional Programming?

@Hughes:1990:WFP:119830.119832 describes the fundamental idea of 
functional programming like this:

> Functional programming is so called because its fundamental operation is
the application of functions to arguments. A main program itself is written as
a function that receives the program’s input as its argument and delivers the
program’s output as its result. Typically the main function is defined in terms of
other functions, which in turn are defined in terms of still more functions, until
at the bottom level the functions are language primitives.

Functional programming is also often - wrongly - only defined by what it does not allow programmers 
to do. @Hughes:1990:WFP:119830.119832 furthermore describes this aspect elegantly while 
naming the usual advantages of functional programs:

> The special characteristics and advantages of functional programming are
often summed up more or less as follows. Functional programs contain no
assignment statements, so variables, once given a value, never change. More
generally, functional programs contain no side-effects at all. A function call
can have no effect other than to compute its result. This eliminates a major
source of bugs, and also makes the order of execution irrelevant — since no side-
effect can change an expression’s value, it can be evaluated at any time. This
relieves the programmer of the burden of prescribing the flow of control. Since
expressions can be evaluated at any time, one can freely replace variables by
their values and vice versa — that is, programs are “referentially transparent”.
This freedom helps make functional programs more tractable mathematically
than their conventional counterparts. 
> 
> [...]
> 
> Even a functional programmer should be dissatisfied with these so-called
advantages, because they give no help in exploiting the power of functional languages.
One cannot write a program that is particularly lacking in assignment
statements, or particularly referentially transparent. There is no yardstick of
program quality here, and therefore no ideal to aim at.

To argue that there is merit in functional programming besides having fewer error-prone features
@Hughes:1990:WFP:119830.119832 goes also goes into detail about one of the actual aspects why 
functional programming matters - composability. He does this by showing how higher
order functions help in expressing programs in a modular way.

Functions are not the 

### A Short introduction to Haskell

In the following section, we will give a short introduction to functional programming
with Haskell. While this will give a good idea of how programming in Haskell works,
this is not aimed to be a complete tutorial on Haskell, but merely a quick
overview over the most relevant features of the language used in this thesis.
The following is loosely based on the book \enquote{Learn you a haskell for great good!}
[@learnyouahaskell].


Hier eine Überleitung in Richtung, dass erst mal jetzt erklärt wird, was basic Haskell 
so ausmacht 

- Functional vs Imperative (Was vs. Wie? Stateful computation, Absteiger nach Java Land)
- Basic Functions (x) / Lambdas (x)
- Typesafety
- Lambdas (x)
- Typklassen (vs Interfaces, Traits?) (x)
- Lazy Evaluation
- Monaden (wegen stateful)
- Arrows


https://docs.microsoft.com/en-us/dotnet/visual-basic/programming-guide/concepts/linq/functional-programming-vs-imperative-programming

#### Functional Programming vs Imperative Programming

In this section we will give a short introduction
to functional programming in Haskell by comparing the general style of imperative
C code to functional Haskell using the example of the Fibonacci sequence.

Since functional programs do not contain any
assignment statements, the state of a given variable can not be mutated. 
This however means that in pure^[Pure code is code without side-effects. Assignments are side-effects]
functional code we can not express
classic loops like in Fig. \ref{fig:fibonacciCIterative}.
^[It is however possible to introduce monadic DSLs in Haskell that mimic C style behaviour, see \url{https://hackage.haskell.org/package/ImperativeHaskell-2.0.0.1}.]  

~~~~ {#fig:fibonacciCIterative
    .c
    .figure
    caption="Iterative Fiboncacci in C"
    options=ht
    }
int fib( int n ) {
    int pre = 0;
    int cur = 1;
    int res = 0;
    for ( int i = 0; i < n; ++i ) {
        res = pre + cur;
        pre = cur;
        cur = res;
    }
    return cur;
}
~~~~

If we translate this Fibonacci example into a recursive definition
(Fig. \ref{fig:fibonacciCRecursive}), we get pure functional C code,
that resembles the Haskell variant in Fig. \ref{fig:fibonacciHaskell}.

~~~~ {#fig:fibonacciCRecursive
    .c
    .figure
    caption="Recursive Fiboncacci in C"
    options=ht
    }
int fib( int n ) {
	if ( n <= 0 )
		return 0;
	else if ( n == 1 )
		return 1;
	else
		return fib( n - 2 ) + fib( n - 1 );
}
~~~~

In fact, in functional languages like Haskell we only express computations by 
composition of functions (recursion is also in essence a composition of a function with itself).
The fact that pure functional languages do not have assignment statements, function
application is the only way to compute anything and since we can not change
the state of any associated variables, we generally also do not have to worry
about the order of execution in functional programs.
In general, we can say that in functional programming we primarily focus on what
information is required and by which transformations to compute it 
instead of how we perform them and how we track the changes in state
^[from \url{https://docs.microsoft.com/en-us/dotnet/visual-basic/programming-guide/concepts/linq/functional-programming-vs-imperative-programming}].

~~~~ {#fig:fibonacciHaskell
    .haskell
    .figure
    caption="Standard Fibonacci in Haskell."
    options=ht
    }
fib :: Int -> Int
fib n
	| n <= 0 = 0
	| n == 1 = 0
	| otherwise = 
		(fib (n - 2))
			+ (fib (n - 1))
~~~~

Haskell being a functional language does not mean, that we do not have
the usual problem of a too small call-stack size encountered when programming with recursion.
While Haskell programs can naturally handle much bigger call-stacks without overflowing, 
at some point the limit will be reached and the program will crash.
But since the class of tail-recursive programs
is equivalent to the class of all recursive programs (which is in turn equivalent to
all imperative programs), this is no big problem: We can just translate our `fib` definition
into a tail-recursive variant (Fig. \ref{fig:fibonacciHaskellTailRecursive})
which Haskell's compiler is capable of automatically translating into looping
machine code.

~~~~{#fig:fibonacciHaskellTailRecursive
    .haskell
    .figure
    caption="Tail Recursive Fibonacci in Haskell."
    options=ht
    }
fib :: Int -> Int
fib n
    | n <= 0 = 0
    | otherwise = fib' n 0 1
    where
        fib' :: Int -> Int -> Int -> Int
        fib' n prev res
            | n == 0 = res
            | otherwise = fib' (n - 1) res (res + prev)
~~~~

#### Functions

The basic building blocks of a Haskell program are functions:

~~~~{.haskell
    }
f :: Int -> Int -> Int
f x y = multiply x y
~~~~

where `multiply` would be defined as

~~~~{.haskell
    }
multiply :: Int -> Int -> Int
multiply x y = x * y
~~~~

Since `f` and `multiply` seem to be the same, we can even write this relationship directly:

~~~~{.haskell
    }
f :: Int -> Int -> Int
f = multiply
~~~~

We can do so because in Haskell functions can be treated just like any other data-type.
For example, if we wanted to have another function `g` which applied `f` on two lists of
integers, we can write

~~~~{.haskell
    }
g :: [Int] -> [Int] -> [Int]
g = zipWith f
~~~~

where zipWith would be of type `(Int -> Int -> Int) -> [Int] -> [Int] -> [Int]`.

Now, it does not make sense to be so restrictive in terms of which type to allow in
such a function since all it does is apply some function to zip two lists. Thankfully,
in Haskell we can define functions in a completely generic way such that
we can write the actual type of zipWith as `(a -> b -> c) -> [a] -> [b] -> [c]` as in
it can zip a list containing some `a`s with a list containing a list of `b`s with a function
`a -> b -> c` to get a list of `c`s. If we use this function in the context of
our function `g` it is then specialized into the `Int` form.

The fact of the matter is that we can even define `g` without writing down the type
definition and let the compiler determine the actual type of `g`.

~~~~{.haskell
    }
g = zipWith f
~~~~

While this is possible, it is generally encouraged to always specify the
type of top-level functions for better readability, but sometimes this is useful
for some nested helper functions.

#### Type safety and inference

Haskell is a statically typed language.

#### Function composition, higher-order functions, and function application

Functions in Haskell can be handled similar to other datatypes. This way,
we can for example define a function that computes a number to the power of four as

~~~~{.haskell
    }
toThePowerOfFour :: Int -> Int
toThePowerOfFour = toThePowerOfTwo . toThePowerOfTwo
~~~~

with `.` being the functional composition operator with type
`(.) :: (a -> b) -> (b -> c) -> (a -> b -> c)`
and where square is defined simply as `toThePowerOfTwo x = multiply x x`.
If we were to implement some function `toThePowerOfEight` as

~~~~{.haskell
    }
toThePowerOfEight :: Int -> Int
toThePowerOfEight = toThePowerOfFour . toThePowerOfFour
~~~~

we start to see a pattern. We could introduce a `squareF` function
which would take the function we would want to compose with itself that could
look like

~~~~{.haskell
    }
squareF :: (Int -> Int) -> (Int -> Int)
squareF f = f . f
~~~~

With this higher-order function we could then define `toThePowerOfFour` as

~~~~{.haskell
    }
toThePowerOfTwo :: Int -> Int
toThePowerOfTwo = squareF toThePowerOfTwo
~~~~

~~~~{.haskell
    }
toThePowerOfFour :: Int -> Int
toThePowerOfFour = squareF toThePowerOfTwo
~~~~

and `toThePowerOfEight` would become

~~~~{.haskell
    }
toThePowerOfEight :: Int -> Int
toThePowerOfEight = squareF toThePowerOfFour
~~~~

With such 

TODO: square function... square x = multiply x x

TODO: ^4 function = square . square

TODO: Function application ($)

#### Conditional Computation

Haskell has different styles of dealing with conditional evaluation. We will
now show the three the most common variants to express conditional statements.

The most obvious one in terms of functionality is the `if ... then ... else`
construct:

~~~~{.haskell
    }
myFunc :: Int -> Int
myFunc x = if x < 10 then x * 2 else x * 4
~~~~

While having the same well-known semantics of any `if ... then ... else` like
they could be found in imperative languages like e.g. C, in Haskell, being a functional
language, the `else` is non-optional as expressions are required to be total
^[total in terms of computation, unsuccessful calculations can still be expressed
with constructs like `Maybe a`.].

An alternative to this are guards, which make expressions easier to read
if many alternatives are involved in a function:

~~~~{.haskell
    }
myFunc :: Int -> Int
myFunc x 
    | x < 10 = x * 2
    | x < 12 = x * 3
    | x < 14 = x
    | x > 18 && x < 20 = 42
    | otherwise = x * 4
~~~~

Yet another technique for conditional computation is using pattern matching. For conditional
statements we can use it by writing definitions of the function for specific values, like

~~~~{.haskell
    }
myFunc :: Int -> Int
myFunc 5 = 10
myFunc x ATSIGN {10} = x * 10
myFunc x = x * 2
~~~~

, where the first matching definition is chosen during computation.
Alternatively, we can do pattern matching with the help of `case` expressions:

~~~~{.haskell
    }
myFunc :: Int -> Int
myFunc x = case x of
    5 -> 10
    x ATSIGN {10} = x * 10
    x = x * 2
~~~~

These can be used just like ordinary expressions.

We can not, however, express boolean statements in this way. This is because
pattern matching is done on the structure of the value that is being pattern matched.
Later in this section we will see what other powerful things we can do with
this technique.

#### `where`, `let`

While Haskell does not have variables, it still allows the programmer to
name sub-expressions so that either the code becomes more clear or that it
can be reused more easily. Here, two different variants are available: `where` 
and `let`.

With the help of `where` we can write code like the following:

~~~~{.haskell
    }
whereReuse :: Double -> Double -> String
whereReuse a b
    | divided > 10 = "a is more than 10 times b"
    | divided == 10 = "a is 10 times b"
    | otherwise = "a is less than 10 times b"
    where divided = a / b
~~~~

`where` is just syntactic sugar, though, and can not used in expressions like
`f (a * 2 where a = 3)`. This is possible with `let` where we can write
`f (let a = 3 in a * 2)` or `let a = 3 in f (a * 2)`. `let` can, however, not
be used in conjunction with guards.

#### Typeclasses

The example function `multiply` from above seems a bit restrictive as it only allows for the usage
of `Int`s. `Int`s are obviously not the only type which can be multiplied.

With the help of a typeclass `Multiplicable a` we can encapsulate the contract
of `multiply` on some type `a` as

~~~~{.haskell
    }
class Multiplicable a where
    multiply :: a -> a -> a
~~~~

where we can then define the `Multiplicable Int` instance as

~~~~{.haskell
    }
instance Multiplicable Int where
    multiply x y = x * y
~~~~

If we want to write a generic function `f` with the help of
`multiply`, we require a `Multiplicable` instance for every type that
we want to multiply inside the function:

~~~~{.haskell
    }
f :: Multiplicable a => a -> a -> a
f = multiply
~~~~

While this example might seem a bit odd, because we are just delegating `f`
to `multiply`, it shows how we can encapsulate contracts on functions 
("multiplication" is a contract) without losing generality with the
help of type-classes.

In Haskell we can also write
typeclasses with more than one type parameter. This allows for 
encapsulation of contracts of arbitrary complexity. Furthermore 
typeclasses can itself have constraints placed on what types are allowed:

~~~~{.haskell
    }
class (SomeClass a, SomeOtherClass b) => MyClass a b c where
    ...
~~~~

#### Lazy Evaluation

Haskell is a lazy language. This means that values are only evaluated when required.
This has one major benefit: We get a Producer/Consumer pattern behaviour for
free. For example if we have the lazy function `producer :: Int -> [Int]` producing some list
of integers and some consumer consuming `consumer :: [Int] -> Int` this list. Then,
in a program `consumer . producer`, `producer` generates
the elements of the result-list as they are consumed.
This also means that, if `consumer` only requires the first few elements of the list to compute
the result, `consumer` does not produce unneeded results.

Laziness even allows us to express infinite streams, which can be helpful in some cases
. As an example, an infinite list of ones is defined as

~~~~{.haskell
    }
ones :: [Int]
ones = 1 : ones
~~~~

Another good example where Laziness simplifies things is when branching is involved:

~~~~{.haskell
    }
calculateStuff :: [Int] -> Int
calculateStuff = if <someCondition>
                 then doStuff list1 list2
                 else doSomeOtherStuff list1
                    where
                        list1 = ...
                        list2 = ...
~~~~

Here, `list2` is not required in both branches of the `if` statement, which means it is only
evaluated when required. While such a behaviour is obviously possible in non-lazy languages
the elegance of the above definition is apparent.
We can define as many variables in the same clear way without having
unnecessary computations or code dealing with conditional computation.

Usually laziness is beneficial to programs, but sometimes we require more control about
when something is evaluated. This can be done for example with

#### Custom types

As in any good programming language, in Haskell programmers obviously do not have
to represent everything with only some base-set of types. Types are usually defined
in three different ways. For starters, we can give types aliases with the `type` keyword like

~~~~{.haskell
    }
-- Tuple of a and b
type Tuple a b = (a, b)

-- Tuple of ints
type IntTuple = (Int, Int)
~~~~

, which are treated just like original `(a, b)` or `Int, Int` would. This means, we
can use such types loosely and pass e.b. a `Tuple Int Int` into a
function `f :: (Int, Int) -> ...`. The same also holds for typeclasses.

The second way to declare types, 
`data` however declares new-types as in actual new types in the type system like

~~~~{.haskell
    }
data Direction = 
      North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest
~~~~

, where `North` - `NorthWest` are called constructors.

`data` types are not limited to enum-style types though, they can also hold values, like
the `Maybe a` type from Haskell. This type - which *may* hold a value `a` internally -
is usually used as a return type for functions which not always return an actual result.
We can define it as follows:

~~~~{.haskell
    }
-- unnamed field
data Maybe a = Just a | Nothing
~~~~

where values are created by calling the constructors with the appropriate
parameters (if any), i.e. when passed into a function: `f (Just 1)`.
Furthermore, `data` constructors can have named fields defined like

~~~~{.haskell
    }
-- named field
data Maybe a =
      Just { theThing :: a }
    | Nothing
~~~~

where values are created by calling the constructor and passing the appropriate
parameters to the properties, i.e. `f (Maybe { theThing = 1 })`
The final way to define custom types is via `newtype`:

~~~~{.haskell
    }
-- unnamed field
newtype MyNewType a = Constructor a

-- named field
newtype MyOtherNewType a = Constructor { myOnlyThing :: a }
~~~~
 
Types declared this way are similar to `data` types, but can only contain a single constructor with just
a single field. Also, unlike `data`, constructors declared with `newtype` are
strict, meaning the compiler can optimize away the surrounding declaration. Everything
else is handled exactly like with `data` types. `newtype` types are also a useful
tool if we were to write a wrapper for a type while not wanting
to inherit all instances of typeclasses, but are also often used when declaring
more complicated types.

#### Pattern Matching

While we have seen pattern matching as an alternative to 
`if ... then ... else` and guard statements, it is can do even more things.

For example, if we have a datatype `MyType a` type defined as

~~~~{.haskell
    }
data MyType a = SomeConstructor a | SomeOtherConstructor a
~~~~

and we want to write a function `unwrap :: MyType a -> a` to unwrap the `a` value,
we use pattern matching like this:

~~~~{.haskell
    }
unwrap :: MyType a -> a
unwrap (SomeConstructor x) = x
unwrap (SomeOtherConstructor x) = x
~~~~

This type of unwrapping can also be done with the help of `case` statements
so that we do not require a new function definition:

~~~~{.haskell
    }
someFunc :: MyType a -> a
someFunc t = case t of
    (SomeConstructor a) -> ...
    (SomeOtherConstructor a) -> ...
~~~~

In Haskell programs we can also write unwrapping code with the help of the
`let` notation for single constructor types like
`let (x, y) = vec2d in sqrt (x * x + y * y)`. Predictably, we can do this
with `where` as well with constructs like `where (x, y) = vec2d`.

Sometimes we only care about some part of the value. For example, in a definition
of `maybeHead :: [a] -> Maybe a`, which should return the first element of the list
or `Nothing` if it is an empty list, we can write this with the help of wildcards (`_`) as:

~~~~{.haskell
    }
maybeHead :: Maybe a -> a
maybeHead (x : _) = x
maybeHead [] = Nothing
~~~~

We could even write 

~~~~{.haskell
    }
maybeHead :: Maybe a -> a
maybeHead (x : _) = x
maybeHead _ = Nothing
~~~~

as the second equation will only ever match when the list is empty.^[A single element list
has two forms in Haskell, `a:[]` and `[a]` of which the latter is just syntactic sugar of the former.]
Furthermore, in Haskell, functions `isJust :: Maybe a -> Bool` when they
only care about the structure of the type, can be written with only with wildcards:

~~~~{.haskell
    }
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False
~~~~

Additionally, if we want to preserver laziness and we are 100% sure that a match will
work (e.g. if we have called `isJust`), we can use irrefutable patterns like
`~(Just a) = someMaybe`.

#### Lambdas & Partial application

As Functions are just another type that can be passed into higher-order functions
it makes sense to have a short-hand to write anonymous functions - lambdas.
In Haskell they look like this:

~~~~{.haskell
    }    
\(a, b) -> a + b
~~~~

This can easily be passed into functions, like zipWith^
[While `(\(a, b) -> a + b)` is obviously the same as (+), we just write it as a lambda 
here for demonstration purposes]:

~~~~{.haskell
    } 
someFunc :: [Int] -> [Int] -> [Int]  
someFunc xs ys = zipWith (\(a, b) -> a + b) xs ys
~~~~

Here, we notice the reason for yet another feature in Haskell that 
is commonly used: Partial application. While the definition of `someFunc` is
definitely not wrong, we could have written it more elegantly as

~~~~{.haskell
    } 
someFunc :: [Int] -> [Int] -> [Int]  
someFunc = zipWith (\(a, b) -> a + b)
~~~~

where we this means that `someFunc` is defined as `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c])` partially applied with
the passed lambda to get a function with type `[a] -> [b] -> [c]` which the compiler then
automatically binds to the type of `someFunc :: [Int] -> [Int] -> [Int]`. 

#### WEG DAMIT



Das sollte weiter oben hin wahrscheinlich
This type of type inference.
allows Haskell to be generic in terms of function definitions without losing type-safety.


If we 

We define 

Jetzt, Beispiel: Higher-Order Functions, Erklären warum es direkt einfacher ist, das
in Haskell zu machen, als in C oder anderen Sprachen.
