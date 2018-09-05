## Functional programming

\label{sec:fuproHaskell}

This chapter covers the basics of functional programming. We start
by citing @Hughes:1990:WFP:119830.119832 on why functional programming matters
including a characterisation of the concept in general (Chapter \ref{sec:whyfupro}).
Then, we give a short introduction to functional programming with Haskell
(Chapter \ref{sec:shortIntroHaskell}) and also explain the concept of Monads
(Chapter \ref{sec:monads}) which some parallel Haskells use. Finally, we
introduce Arrows and explain their type class in Haskell (Chapter \ref{sec:arrows}).

### Why functional programming matters

\label{sec:whyfupro}

@Hughes:1990:WFP:119830.119832 describes the fundamental idea of 
functional programming like this:

>Functional programming is so called because its fundamental operation is
the application of functions to arguments. A main program itself is written as
a function that receives the program’s input as its argument and delivers the
program’s output as its result. Typically the main function is defined in terms of
other functions, which in turn are defined in terms of still more functions, until
at the bottom level the functions are language primitives.

Basically, functional programs only contain logic described in terms of
functions and their compositions. Additionally, functional programming is also often characterized
as follows [@Hughes:1990:WFP:119830.119832]:

>Functional programs contain no
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

While all these are all good arguments in favour of functional programming -- because
of the elimination of programming bottlenecks -- these arguments only
describe functional programming by means of what it can not do. 
@Hughes:1990:WFP:119830.119832 describes his dissatisfaction with this
argument as follows:

>Even a functional programmer should be dissatisfied with these so-called
advantages, because they give no help in exploiting the power of functional languages.
One cannot write a program that is particularly lacking in assignment
statements, or particularly referentially transparent. There is no yardstick of
program quality here, and therefore no ideal to aim at.

To argue that there is merit in functional programming besides having fewer error-prone features
@Hughes:1990:WFP:119830.119832 then goes into detail about one of the areas where functional
programming shines and why it therefore matters -- modularity. He argues that
modularity is only possible with good glue code. This is where he sees functional
programming to be better suited because of two powerful tools:
higher-order functions and laziness.

Higher-order functions are functions that take other functions as arguments. They
usually generalize a concept (e.g. mapping over a list, zipping two lists, etc.) and
take the passed function(s) as their internal worker function. They provide the skeleton
of the program. Laziness here means that values are only evaluated when required.
This allows for programs to work in a producer/consumer pattern without having to
write manual interweaving code. We will explain both concepts in greater details
later in this chapter.

The focus on modularity through composability that functional languages have,
can be seen in all the definitions of Haskell functions in the following chapters of 
this thesis. Also, the main functional concept this thesis uses, Arrows,
are by nature a result of the desire to generalize modularity. We will show how to
enable programs based on this concept to benefit from parallelism by using
our combinator `parEvalN`.

### A short introduction to Haskell

\label{sec:shortIntroHaskell}

Even though this thesis is called \enquote{Concepts in Parallel Programming} and focuses
on Arrows for parallel (functional) computation, we have to first define the basic building blocks
of our programming language and show how to use them in regular programs before
we can explore *parallel* programming.
Therefore, in the following chapter, we will give a short introduction to functional programming
with Haskell. While this will give a good idea of how programming in Haskell works,
this is not supposed to be a complete tutorial on Haskell, but merely a quick
overview over the most relevant features of the language which are used in this thesis.
The following is loosely based on the book \enquote{Learn you a haskell for great good!}
[@learnyouahaskell].

#### From Imperative Programming to Functional Programming

In order to ease the introduction to functional programming,
we will give a short introduction
to functional programming in Haskell in this chapter
by comparing the general style of imperative
C code to functional Haskell using the example of the Fibonacci sequence.

To start off, we take a look at the following iterative implementation of the Fibonacci
sequence: 

~~~~ {.c}
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

It contains
assignments and a loop, which in pure^[Pure code is code without side-effects. Assignments are side-effects.]
functional programming we
can not use^[It is however possible to introduce monadic DSLs in Haskell that mimic C style behaviour, see \url{https://hackage.haskell.org/package/ImperativeHaskell-2.0.0.1}.].
If we translate this Fibonacci example into a recursive definition,
however, we get pure functional C code
without any assignment statements:

~~~~ {.c}
int fib( int n ) {
	if ( n <= 0 )
		return 0;
	else if ( n == 1 )
		return 1;
	else
		return fib( n - 2 ) + fib( n - 1 );
}
~~~~

This resembles the Haskell variant in Figure \ref{fig:fibonacciHaskell}.
We can see how the flow of the programming works even without requiring any modifiable state.
Furthermore, the Haskell implementation uses guards (e.g. `n <= 0 = 0`) which 
are equivalent to the conditional statements in the C variant. They will be explained
in more detail later in this chapter.

~~~~ {#fig:fibonacciHaskell
    .haskell
    .figure
    caption="Standard Fibonacci in Haskell."
    options=h
    }
fib :: Int -> Int
fib n
	| n <= 0 = 0
	| n == 1 = 1
	| otherwise = 
		(fib (n - 2))
			+ (fib (n - 1))
~~~~

In functional languages like Haskell we only express computations in this matter by 
composition of functions (recursion in essence is also just a composition of a function with itself).
Because of this and since we can not change the state of any associated variables,
we usually also do not have to worry
about the order of execution in functional programs and let the compiler decide how
to resolve the recursive formula.
In general, we can say that in functional programming we primarily focus on what
information is required and by which transformations to compute it 
instead of how we perform them and how we track the changes in state
^[From \url{https://docs.microsoft.com/en-us/dotnet/visual-basic/programming-guide/concepts/linq/functional-programming-vs-imperative-programming}].

Haskell being a functional language does not mean that we do not have
the usual problem of a too small call-stack size encountered when programming with recursion.
While Haskell programs can naturally handle much bigger call-stacks without overflowing, 
at some point the limit will be reached and the program will crash.
But since the class of tail-recursive programs which all have the form

~~~~{.haskell}
f x = if <end>
      then s x
      else f (r x)

-- s and r arbitrary, but not depending on f
s = ...
r = ...
~~~~

is equivalent to the class of all recursive programs (which is in turn equivalent to
all imperative programs), this is no big problem. We can just translate our `fib` definition
into a tail-recursive variant (Figure \ref{fig:fibonacciHaskellTailRecursive})
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

As already mentioned above, the basic building blocks of a
Haskell program are functions. We define them like this:

~~~~{.haskell
    }
f :: Int -> Int -> Int
f x y = multiply x y
~~~~

Here, we declared a function `f` which takes two arguments
of type `Int` and returns yet another `Int`. In the definition,
we say that `f` is the function `multiply` applied to both its arguments
`x` and `y`. We define `multiply` as

~~~~{.haskell
    }
multiply :: Int -> Int -> Int
multiply x y = x * y
~~~~

In Haskell, since `f` and `multiply` seem to be the same,
we can even write this relationship directly:

~~~~{.haskell
    }
f :: Int -> Int -> Int
f = multiply
~~~~

We can do so, because, in Haskell, functions can be treated just like any other type.
For example, if we want to have another function `g` which applies `f` on two lists of
integers, we can write

~~~~{.haskell
    }
g :: [Int] -> [Int] -> [Int]
g = zipWith f
~~~~

where `zipWith` would be of type `(Int -> Int -> Int) -> [Int] -> [Int] -> [Int]`.
It is common to express calculations in such a way using higher-order functions.
We will see more of this later in this chapter.

#### Type inference

Taking the same example function `g` from above,
it does not make sense to be so restrictive in terms of which type to allow in
such a function since all it does is to apply some function to zip two lists. Thankfully,
in Haskell we can define functions in a completely generic way so that
we can write the actual type of zipWith as `(a -> b -> c) -> [a] -> [b] -> [c]`.
This means that we zip a list containing some `a`s with a list containing a list of `b`s with a function
`a -> b -> c` to get a list of `c`s. Only because we use this function in the context of
our function `g` it is specialized to the `Int` form.

Type inference even allows us to define `g` without writing down the type
definition and let the compiler determine the actual type of `g`.

~~~~{.haskell
    }
g = zipWith f
~~~~

While this is possible, it is generally encouraged to always specify the
type of top-level functions for better readability. Leaving out the type specification
can, however, be useful when defining nested helper functions.

#### Function composition, higher-order functions, and function application

As we have seen, functions can be handled similar to data types in Haskell.
This way, we can for example define a function that computes a number to the power
of four as

~~~~{.haskell
    }
toThePowerOfFour :: Int -> Int
toThePowerOfFour = toThePowerOfTwo . toThePowerOfTwo
~~~~

with `.` being the functional composition operator with type
`(.) :: (b -> c) -> (a -> b) -> (a -> c)`
^[Note the order of the arguments, `g . f` means to first apply `f` and then `g` and not
the other way around]
and where `toThePowerOfTwo` is defined simply as 

~~~~{.haskell}
toThePowerOfTwo :: Int -> Int
toThePowerOfTwo x = multiply x x
~~~~

Another aspect of functions being similar to data types is that, in functional programming,
we frequently use higher order functions to express calculations. We have seen this
earlier with the use of `zipWith`. Other often used higher-order functions include
mapping (`map :: (a -> b) -> [a] -> [b]`, i.e. convert a list of `a`s into a list
of `b`s with the given function `a -> b`) and folding
(e.g. `foldLeft :: (b -> a -> b) -> b -> [a] -> b`, i.e. reduce the list with
the given function `b -> a -> b` into a singular value given a starting value of type `b`).
These are often used in some kind of composition like

~~~~{.haskell
    }
euclidDistance :: [Int] -> [Int] -> Int
euclidDistance = 
    sqrt . foldLeft (+) 0 . map (toThePowerOfTwo) . zipWith (-)
~~~~

Note that while this could have easily been written with fewer higher-order
 functions as something along
the lines of `sqrt (foldLeft (+) 0 (zipWith (\a b -> toThePowerOfTwo (a - b))))`,
it is easy to see that the above declaration is easier to understand because of the simple steps the
computation takes. We first zip the list of inputs with element-wise subtraction and 
then square this difference, sum these results up and finally take the square root.
This is something we see a lot in Haskell code: Complex computations can be expressed
with the help of higher-order functions instead of having to write it
manually. This is not only often much shorter,
but also easier to understand for other programmers if they have to read-up on the
implementation for some reason.

Something which is also quite useful in Haskell is the function application operator
`($) :: (a -> b) -> a -> b` allowing for the application of a
function `a -> b` to a given argument `a`. It is simply defined as:

~~~~{.haskell
    }
($) :: (a -> b) -> a -> b
f $ x = f x
~~~~

While the use-case for such an operator might not be immediately clear, it will be,
if we take a look at the following function `listApp :: [a -> b] -> [a] -> [b]`
where we take a list of functions `[a -> b]` and apply them one-by-one with their respective
input values from the input list `[a]` to generate a list of results `b`:

~~~~{.haskell
    }
listApp :: [a -> b] -> [a] -> [b]
listApp = zipWith ($)
~~~~

Here, if we had not used the `($)` operator, we would have to write
`zipWith (\f a -> f a)` which certainly seems a bit redundant.

Something the `($)` operator is also used quite often is to write shorter code.
For example, code snippets like

~~~~{.haskell
    }
someFunc = f1 (f2 param1 (f3 param2 (f4 param3))
~~~~

can also be written without the braces as

~~~~{.haskell
    }
someFunc = f1 $ f2 param1 $ f3 param2 $ f4 param3
~~~~

which is sometimes preferred to the brace-style, but is semantically
identical.

#### Conditional Computation

Haskell has different styles of dealing with conditional evaluation. We will
now show the most common variants.

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
^[Total in terms of computation, unsuccessful calculations can still be expressed
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

Yet another technique is to use pattern matching.
For conditional statements, we can use it by writing definitions of the function for specific values, like

~~~~{.haskell
    }
myFunc :: Int -> Int
myFunc 5 = 10
myFunc x ATSIGN 10 = x * 10
myFunc x = x * 2
~~~~

where the first matching definition is chosen during computation.^[Here, the `ATSIGN` allows
us to bind the value to a variable while also pattern matching it.]
Alternatively, we can do pattern matching with the help of `case` expressions:

~~~~{.haskell
    }
myFunc :: Int -> Int
myFunc x = case x of
    5 -> 10
    10 -> x * 10
    _ -> x * 2
~~~~

These can be used just like ordinary expressions.

We can not, however, express boolean statements in this way. This is because
pattern matching is done on the structure of the value that is being pattern matched.
Later in this chapter, we will see what other powerful things we can do with
this technique.

#### `where`, `let`

While Haskell does not have variables and only works on 
values instead, it still allows the programmer to
name sub-expressions so that either the code becomes more clear or that it
can be reused more easily. Here, two different variants are available: `where` 
and `let`.

With the help of `where` we can write code like

~~~~{.haskell
    }
whereReuse :: Double -> Double -> String
whereReuse a b
    | divided > 10 = "a is more than 10 times b"
    | divided == 10 = "a is 10 times b"
    | otherwise = "a is less than 10 times b"
    where divided = a / b
~~~~

It is important to note that `where` is just syntactic sugar, though,
and can not used in expressions like
`f (a * 2 where a = 3)`. This is in turn possible with `let`, which allows us to write
expressions like `f (let a = 3 in a * 2)` or `let a = 3 in f (a * 2)`.
`let` can, however, not
be used in conjunction with guards.

#### Type safety

Haskell is a statically typed functional language. This means that during compilation
all types are checked for compatibility and type declarations are not just treated as optional
\enquote{hints} to the type-checker. Pairing this with the pure aspect of the language
means that Haskell programs seem to be correct more often in practice if the program compiles
than in imperative languages. The compiler essentially helps the programmer
to write *semantically correct* instead of just syntactically correct code.
It should be noted that this does not mean that testing
can be omitted. It is still extremely important, but becomes less cumbersome
because state is mostly a non-issue.

#### Type classes

The example function `multiply` which we have seen earlier
seems a bit restrictive as it only allows for the usage
of `Int`s. `Int`s are obviously not the only type which can be multiplied. Haskell
has a way to express this fact: type classes. We can express a type class 
`Multiplicable a` that encapsulates the contract of `multiply` on some type `a` as

~~~~{.haskell
    }
class Multiplicable a where
    multiply :: a -> a -> a
~~~~

With this definition, we can then introduce instances -- implementations of the
contract -- for specific types. For example, the instance for `Int`,
`Multiplicable Int`, can be written as

~~~~{.haskell
    }
instance Multiplicable Int where
    multiply x y = x * y
~~~~

Now if we want to use this new contract on a generic function `f`,
we require a `Multiplicable` instance for every type that
we want to use `multiply` on inside the function:

~~~~{.haskell
    }
f :: Multiplicable a => a -> a -> a
f x y = multiply (multiply x y) x
~~~~

Such a function `f` does work with the contract of
`Multiplicable` instead of requiring some specific type.
Using this technique allows many definitions in Haskell to be
reused even though it is a statically typed language. 

We can also write
type classes with more than one type parameter. This allows for 
encapsulation of contracts of arbitrary complexity. Furthermore, 
type classes can themselves have constraints placed on what types are allowed.
Both can be seen here:

~~~~{.haskell
    }
class (SomeClass a, SomeOtherClass b) => MyClass a b c where
    ...
~~~~

#### Custom types

As in any mature programming language, Haskell programmers obviously do not have
to represent everything with only some base-set of types and allows
to introduce custom types. They are usually defined
in three different ways. For starters, we can give types aliases with the `type` keyword like

~~~~{.haskell
    }
-- Tuple of a and b
type Tuple a b = (a, b)

-- Tuple of ints
type IntTuple = (Int, Int)
~~~~

which are treated just like the original `(a, b)` or `(Int, Int)` would. This means
that we can use such types loosely and pass e.b. a `Tuple Int Int` into a
function `f :: (Int, Int) -> ...`. The same also holds true for instances of
type classes. Because aliases do not count as new types,
`type` declarations are often used to make function types easier to read.

The second way to declare types, `data`, declares new-types as in actual new
types in the type system:

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

The words starting with uppercase letters, e.g. `North` - `NorthWest`,
are called constructors.

`data` types are not limited to enum-style types, though. They can also hold values, like
the `Maybe a` type from Haskell. This type, which *may* hold a value `a` internally,
is usually used as a return type for functions which not always return an actual result
(e.g. in case of a failure).
We can define this type as follows:

~~~~{.haskell
    }
-- unnamed field
data Maybe a = Just a | Nothing
~~~~

Values are created by calling the constructors with the appropriate
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
strict instead of lazy, meaning the compiler can optimize away the surrounding declaration. Everything
else is handled exactly like with `data` types. The specifics of what
laziness or strictness means will be explained in the next section of this chapter.
`newtype` types are also a useful
tool if we were to write a wrapper for a type while not wanting
to inherit all instances of type classes, but are also often used when declaring
more complicated types.

#### Lazy Evaluation

One thing that is not obvious when looking at the definitions from this chapter
is that Haskell is a lazy language^[Haskell is actually defined as a non-strict language,
meaning that only as much as required is evaluated, not when it is done. Laziness is just a way to
 achieve non-strictness. The same could be achieved with an eager, but non-strict evaluation mechanism.
 But as Haskell's main compilers all implement non-strictness via lazy evaluation, it is okay
 to call Haskell a lazy language here. See \url{https://wiki.haskell.org/Lazy_vs._non-strict}]. This means that values are only evaluated when required.
This has one major benefit: We get a Producer/Consumer pattern for
free. For example if we have the lazy function `producer :: Int -> [Int]` producing some list
of integers and some consumer consuming `consumer :: [Int] -> Int` this list. Then,
in a program `consumer . producer`, `producer` generates
the elements of the result-list as they are consumed.
This also means that, if `consumer` only requires the first few elements of the list to compute
the result, `producer` does not produce unneeded results.

Laziness even allows us to express infinite streams, which can be helpful.
As an example, an infinite list of ones is defined with the help of the list
constructor `(:) :: a -> [a] -> [a]`, which prepends a value to a list, as

~~~~{.haskell
    }
ones :: [Int]
ones = 1 : ones
~~~~

or, if we require a list of some value at least $n$ times so that it can be consumed
with some list of length $n$, we can just use an infinite list instead of computing
the actual required amount (which would take $n$ steps for a linked list).
The helper function for this is called `repeat` and can be written as

~~~~{.haskell
    }
repeat :: a -> [a]
repeat a = a : (repeat a)
~~~~

Another good example where laziness simplifies things is when branching is involved:

~~~~{.haskell}
calculateStuff :: [Int] -> Int
calculateStuff = if <someCondition>
                 then doStuff list1 list2
                 else doSomeOtherStuff list1
                    where
                        list1 = ...
                        list2 = ...
~~~~

Here, `list2` is not required in both branches of the `if` statement.
Thanks to laziness, it is therefore only evaluated upon a successful if-check.
While such a behaviour is obviously possible in non-lazy languages,
the elegance of the above definition is apparent.
We can define as many variables in the same clear way without having
unnecessary computations or code dealing with conditional computation
like nested `where`s.

Usually laziness is beneficial to programs and programmers as it
allows for easy composition and better structure in code,
but sometimes we require more control about
when something is evaluated. Haskell therefore has several ways to control
*when* and *how* values are evaluated.

The most basic primitive to force values to be *strict*
instead of lazy is `seq :: a -> b -> b`, which
is by nature part of the compiler and can not be expressed in Haskell directly.
It's semantics however, are as follows: We tell the compiler that the first
argument (of type `a`) is to be evaluated before the second argument.
For example, in an expression like

~~~~{.haskell}
myFun :: Int -> (Int, Int)
myFun x = let y = f x in y `seq` g y
    where
        f = ...
        g = ...
~~~~

we can then at the compiler that we want `y = f x` evaluated before
returning the (still non-evaluated) result of `g y`. This trick is usually used
if during profiling a big chunk of non-evaluated values are noticed to
aggregate before or in the process of evaluation of `f x`.
As this is a common pattern seen in Haskell programs, there exists the
strict function application operator `($!) :: (a -> b) -> a -> b`
to encapsulate it. It is straightforwardly defined as:

~~~~{.haskell}
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
~~~~

With it, we can write our example function as

~~~~{.haskell}
myFun :: Int -> (Int, Int)
myFun x = g $! f x
    where
        f = ...
        g = ...
~~~~

Note that the strictness implied by these two operations does not equal
*complete* evaluation.
They only force to weak-head-normal-form (WHNF) meaning that evaluation
is only forced until the outermost constructor in contrast to normal-form (NF) which stands
for full evaluation. This means that if we were
to evaluate some calculation `f (g (h (i x)))` embedded in some lazy tuple `(y, z)` to WHNF,
`y` and `z` would not be touched as the evaluation stops at the tuple constructor (for more
about constructors see the section on custom types). All the computations
that lead to this constructor however, would be forced to be evaluated. Therefore, if we
had wanted
to make the insides of a tuple strict, we would have to write something along the lines of

~~~~ {.haskell}
let tup ATSIGN (y, z) = f (g (h (i x))) in y `seq` z `seq` tup
~~~~

instead of just

~~~~ {.haskell}
let tup = f (g (h (i x))) in y `seq` y
~~~~

But as `seq` and `$!` both only evaluate to WHNF, `y` and `z` might still not be completely
evaluated, since they could be of some more complex type than just `Int` or any other primitive.
This is the reason why in the Haskell eco system, there exists the
library `deepseq`^[See \url{haskell.org/package/deepseq-1.4.3.0/docs/Control-DeepSeq.html}.]
which comes with the type class `NFData` defined as

~~~~{.haskell}
class NFData a where
    rnf :: a -> ()
~~~~

Instances of this type class
for some type `a` are required to provide an appropriate implementation
of `rnf` for *full* evaluation to normal-form, where `rnf` stands for
\enquote{reduce-to-normal-form}. With this, we can then implement the
NF equivalent to `seq`, `deepseq`, as

~~~~{.haskell}
deepseq :: NFData a => a -> b -> b
deepseq a = rnf a `seq` a
~~~~

A deep analogue to `$!` is then easily definable as well as

~~~~{.haskell}
($!!) :: NFData a => (a -> b) -> a -> b
f $!! x = x `deepseq` f x
~~~~

When dealing with WHNF and NF, note that in all computations annotated with some
forcing construct, be it `seq` or `deepseq`, laziness does not go away entirely.
All forced values, even the ones forced to NF, can still be considered somewhat
lazy as they are only forced when they are requested. However, in practice,
this is usually a desired property.

#### Pattern Matching

While we have seen pattern matching as an alternative to 
`if ... then ... else` and guard statements, it can do more things.
For example, if we have a datatype `MyType a` defined as

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

This kind of unwrapping can also be done with the help of `case` statements
so that we do not require a new function definition:

~~~~{.haskell
    }
someFunc :: MyType a -> a
someFunc t = case t of
    (SomeConstructor a) -> ...
    (SomeOtherConstructor a) -> ...
~~~~

In Haskell programs, we can also write unwrapping code with the help of the
`let` notation for single constructor types like^[It is possible to do this
for types with more than one constructor as well, but this will lead
to errors at runtime unless we can ensure that the matched constructor is the correct
one]:

~~~~{.haskell}
f :: (Double, Double) -> Double
f vec2d = let (x, y) = vec2d in sqrt (x * x + y * y)
~~~~

Predictably, we can do this with `where` as well:

~~~~{.haskell}
f :: (Double, Double) -> Double
f vec2d = sqrt (x * x + y * y)
    where (x, y) = vec2d
~~~~

Sometimes, we only care about some part of the value. For example, in a definition
of `maybeHead :: [a] -> Maybe a`, which should return the first element of the list
or `Nothing` if it is an empty list `[]`, we can write this with the help of wildcards (`_`) as:

~~~~{.haskell
    }
maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead [] = Nothing
~~~~

We could even write 

~~~~{.haskell
    }
maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead _ = Nothing
~~~~

as the second equation will only ever match when the list is empty.^[A single element list
has two forms in Haskell, `a:[]` and `[a]` of which the latter is just syntactic sugar of the former.]
Furthermore, functions `isJust :: Maybe a -> Bool`, if they
only care about the structure of the type, can be written with only wildcards:

~~~~{.haskell
    }
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False
~~~~

Additionally, if we want to preserve laziness and we are sure that a match will
work (e.g. if we have called `isJust`), we can use irrefutable patterns like
`~(Just a) = someMaybe`.

#### Lambdas and Partial application

As Functions are just another type that can be passed into other (higher-order) functions,
it makes sense to have a short-hand to write anonymous functions -- lambdas.
In Haskell, they look like this:

~~~~{.haskell
    }    
\(a, b) -> a + b
~~~~

Lambdas can easily be passed into functions, like `zipWith`^[
While `(\(a, b) -> a + b)` is obviously the same as (+), we just write it as a lambda 
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

where this means that `someFunc` is defined as `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c])` partially applied with
the passed lambda to get a function with type `[a] -> [b] -> [c]` which the compiler then
automatically binds to the type of `someFunc :: [Int] -> [Int] -> [Int]`.
