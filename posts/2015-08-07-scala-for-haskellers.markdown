----
title: Scala for Haskellers
language: english
description: A quick primer on syntax and basic differences between the languages
tags: programming
----

It's only half an hour till ICFPC 2015 and your team suddenly decides to switch
to Scala for this year? Don't panic — you've got just enough time to bring
yourself up to speed.

Let's kick off with…

# Random facts about Scala

* [Predef][predef] is Scala's analog of Prelude.

* Scala has subtyping.

* The language doesn't separate pure and impure code on the type level.

* Functions can't be declared on the top level; everything should reside within
  classes and objects.

* Operators are actually methods; `a + b` translates into `a.+(b)`. Operators
  that end in colon are special; `a :: b` translates into `b.::(a)`.

* One of the design goals of Scala is seamless Java interop; half of your "why"
  questions can be answered with "because Java" ☺

# Basic types

There's a whole set of integer types (just like in Java):
```
Haskell:   Int8  Int16  Int32  Int64   -- from Data.Int
Scala:     Byte  Short  Int    Long
```

Floating-point types are the same two, `Float` and `Double`; no surprise
there.

Character types, `Char` and `String`, are spelled the same, but Scala's `Char`
is actually a 16-bit unsigned Unicode character, and thus `String` is in UTF-16.

Boolean type is spelled a bit differently and, most importantly, its values are
all lowercase:
```
Haskell:   Bool      -- True and False
Scala:     Boolean   // true and false
```

`()` is spelled `Unit`.

Save for `String`, all of the above are *value types*. They have a nice
property: they can't be `null`. *Reference types*, on the other hand, can. This
distinction is mirrored in the class hierarchy of Scala: value types are
subtypes of `AnyVal`, while reference types are subtypes of `AnyRef`. Finally,
all the types are subtypes of `Any`.

`Nothing` is a subtype of every other type.

`Null` is a type inhabited only by `null`.

# Algebraic data types

Being an object-oriented language, Scala uses class hierarchies to build ADTs.

```Haskell
data Tree a = Node { left  :: Tree a
                   , elem  :: a
                   , right :: Tree a }
            | Leaf
```
```Scala
sealed abstract class Tree[A]
case class Node[A](left: Tree[A], elem: A, right: Tree[A])
    extends Tree
object Leaf extends Tree
```

Note the `sealed` keyword — that makes `Tree` a *closed* ADT, i.e. one that
can't be extended with new value constructors (just like in Haskell). You can
omit the keyword if you need an *open* ADT, of course.

`case` keyword before the class definition enables the following:

* constructor function is automatically defined: instead of `new Node(…)`, you
  can simply write `Node(…)`;
* methods `toString`, `equals` and `hashCode` are automatically implemented.
  Their values depend on the structure of the class (i.e. the arguments you pass
  into the constructor);
* accessor methods are automatically defined (they're methods, so no risk of
  name collisions here);
* we can pattern-match on `case` classes (more on that in a minute).

`Leaf` could've been defined as `class`, but since it has no constructor
parameters, all the instances will be the same. Thus an object definition.

# Pattern matching

```Scala
smth match {
    case 1 => 0;
    case x => x-1;
    case _ => 42;
}
```

# Anonymous functions

```Scala
(x:Int, y:Int) => x + y
```

# Function definitions

Return type annotations are optional, argument type annotations aren't:
```Scala
def fib(n: Int): Int = …
def fib2(n: Int) = …
```

Unlike Haskell, Scala functions aren't curried by default — it's your
responsibility:
```Scala
def greet(greeting: String)(name: String): Unit =
    println(greeting + ", " + name + "!")

greet("Hello")("dear reader")
```

# Partial application

```Scala
val welcome = greet("Welcome") _
val cheer = greet("You can do it") _
```

(In case you're curious — no, it's always single `_`, no matter how many
arguments we omit.)

# Polymorphism and type constraints

Scala doesn't rely on hierarchy of typeclasses to overload common functions and
operators — here, you can just implement method `+` and be done with it. Thus,
type constraints are used only to limit subtyping.

For example, if we have class `C2` that extends class `C1` which, in turn, extends
`AnyVal`, the following code will work on both `C1` and `C2`, but not on `AnyVal`: 

```Scala
def f[A <: C1](x: A) = …
```

Conversely, the following will work on `C1` and `AnyVal` (and `Any`, which is
a superclass of `AnyVal`), but not on `C2`:

```Scala
def f2[A >: C1](x: A) = …
```

(These margins are too narrow to contain meaningful examples, sorry.)

# Traits

Traits are like Haskell's typeclasses — they define an interface that a type
should implement.

```Scala
trait PrettyPrintable {
    def PrettyPrint(): Unit
}
```

# Implicit parameters

Okay, so if traits and type constraints are not used to write contracts, what
is? Meet implicit parameters.

```Scala
def adder[A](x: A, y: A)(implicit a: Numeric[A]): A = {
    a.plus(x, y)
    /* Alternatively:
    
    import a._
    x + y
    */
}
```

# Variance annotations

Another thing that you never had to deal with in Haskell is [co- and
contravariance][co-and-contravariance-wikipedia] (because Haskell doesn't have
subtyping). If you want your trees to be covariant, just add plus before the
type variable:

```Scala
class Tree[+A](…)
```
To make it contravariant, use `-` instead.

# Lists

[Lists][list-docs] are eager. All the usual functions on lists — `head`, `tail`,
`map` etc. — are methods in Scala.

```Haskell
colours :: [String]
colours = [ "red", "green", "blue" ]
bw = "black" : "white" : []
more_colours = colours ++ bw
h, t = (head bw, tail bw)

empty :: Bool
empty = null bw
```

```Scala
val colours: List[String] = List("red", "green", "blue")
val bw = "black" :: "white" :: Nil
val more_colours = colours ::: bw
val (h, t) = (bw.head, bw.tail)
val empty: Boolean = bw.isEmpty
```

`forM_` is spelled `foreach`, `all` and `any` become `forall` and `exists`.

`foldLeft` in Scala (also spelled `/:`) is the same as `flip $ foldl'` in
Haskell, i.e. it takes the base value as the first argument and the function as
the second. There's also `reduceLeft` which is like `foldl1`, only strict;
`foldRight` a.k.a. `:\` (arguments are flipped, too); and `reduceRight`.

# Streams

[Streams][stream-docs] are effectively lazy lists.

```Haskell
a = "one" : "two" : []
b = a ++ ["three", "four"]
```

```Scala
val a: Stream[String] = Stream.cons("one",
                        Stream.cons("two",
                        Stream.empty))
val b = a.append(Stream("three", "four"))
```

# Iterators

…are imperative counterparts of streams. `Iterator[+A]` is a trait specifying two
methods, `hasNext: Boolean` and `next: A`. There's also `BufferedIterator[+A]`
specifying method `head: A`, which is just like `next`, but doesn't advance the
iterator.

Iterators implement all of the usual HOFs.

You can turn your own class into iterator by extending the trait, or you can
create it ad-hoc:
```Scala
val fib = new Iterator[Int] {
    private var current = 1;
    private var previous = 1;
    def hasNext = true
    def next = {
        val result = current + previous
        previous = current
        current = result
        result
    }
}
```

# For-comprehensions

```Haskell
[ x * y | x <- [1..10], y <- [1..x], y `mod` 2 == 0 ]

import Control.Monad (forM_)
forM_ [1..4] $ \x ->
    print $ x * x
```

```Scala
for { x <- List.range(1, 11)
      y <- List.range(1, x+1)
      if y % 2 == 0 }
yield x * y

for (x <- List.range(1, 5)) {
    println(x * x)
}
```

# Mutability

To create a mutable variable instead of immutable value, use `var` instead of
`val`:

```Scala
var a:Int = _
a = 42
```

`_` is a wildcard that will initialize the variable with the default value (0
for numbers, `false` for `Boolean`s, `null` for reference types).

# Laziness

You can defer initialization of any `val` by prefixing its declaration with
`lazy` keyword. This can also be used to avoid initialization order headaches.

To make functions accept their arguments by name (default is by value), prefix
them with `=>`:
```Scala
def myWhile(condition: => Boolean)(action: => Unit): Unit =
    if(condition) {
        action
        myWhile(condition)(action)
    }
```

# Imperative programming

Scala has all the traditional imperative control structures: `while` and
`do-while` loops (`for` loops are present as for-comprehensions), `if`s with
optional `else` branches, `return` and so on. Note, though, that there's no
`break` and `continue` for loops.

# Minimal working program

You're done with theory, now on to writing some real code! Oh wait, you don't
know how to compile stuff yet. Well, just head over to tpolecat's blog and read
[“Setting up to write some Scala”][setting-up-to-write-some-scala] — it'll get
you all set up in no time. He provides example program, too, so my job here is
done.

# Note of thanks

I'd like to thank kind people on scala@conference.jabber.ru for answering my
confused questions and explaining how things should be done. The same goes for
attendees of #scala and #scalaz on Freenode — you people rock!

 

Till next time! Minoru out.

[predef]: http://www.scala-lang.org/api/current/#scala.Predef$
[setting-up-to-write-some-scala]:
    https://tpolecat.github.io/2014/12/12/getting-started.html
    "Setting up to write some Scala"
[co-and-contravariance-wikipedia]:
    https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)
    "Covariance and contravariance (computer science)"
[list-docs]:
    http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.List
    "abstract class List[+A]"
[stream-docs]:
    http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Stream
    "abstract class Stream[+A]"
