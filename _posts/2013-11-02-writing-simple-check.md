---
layout: post
title: Writing simple-check
---

For the past several months I've been working on a QuickCheck library for
Clojure, called simple-check. In this post I will compare simple-check with
some other QuickCheck implementations, as well as describe some of the
challenges I ran into porting QuickCheck from Haskell to Clojure. This will not
act as an introduction to QuickCheck, or property-based testing.

One of the first major differences between writing a QuickCheck in a
statically-typed language and a dynamically-typed one is that with
static-types, we get to use that information to inform QuickCheck of what
generators to use to test our function. For example, if our function under test
has the type `Int -> [Int] -> Bool`, Haskell QuickCheck will use this
information to use generators that generate `Int` and `[Int]`, respectively.
Furthermore, this takes advantage of the fact the we can have polymorphic
_values_ in Haskell. For example, the `Arbitrary` typeclass in Haskell has a
function, `arbitrary`, whose signature is `Gen a`. This allows the compiler to
fill in the specialized version of `Gen a` for us, without us having to provide
a dummy value just to get type-based dispatch. In dynamically-typed languages,
we resort to explicitly specifying the generators to use for our test (which
you can also choose to do in Haskell). For the power we get from our tests, I
think it turns out not to be too onerous to have to explicitly provide the
generators to use, but no doubt more succinct in Haskell. Let's take a look a
little more concretely how this looks.

In Haskell:

{% highlight haskell %}
import Data.List

sortIdempotent :: [Int] -> Bool
sortIdempotent xs = (sort xs) == (sort (sort xs))

quickCheck sortIdempotent
-- +++ OK, passed 100 tests.
{% endhighlight haskell %}
