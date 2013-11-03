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
sortIdempotent :: [Int] -> Bool
sortIdempotent xs = (sort xs) == (sort (sort xs))

quickCheck sortIdempotent
-- +++ OK, passed 100 tests.
{% endhighlight haskell %}

In Clojure:

{% highlight clojure %}
(defn sort-idempotent
  [coll]
  (= (sort coll) (sort (sort coll))))

(sc/quick-check 100
  (prop/for-all [coll (gen/vector gen/int)]
    (sort-idempotent coll)))
;; {:result true, :num-tests 100, :seed 1383433754854}
{% endhighlight clojure %}

In Erlang (also dynamically typed):
{% highlight erlang %}
prop_sort_idempotent() ->
    ?FORALL(Xs, list(int()),
            lists:sort(Xs) =:=
            lists:sort(lists:sort(Xs))).

%% eqc:quickcheck(prop_sort_idempotent()).
%% OK, passed 100 tests
{% endhighlight erlang %}

As you can see, we have to explicitly provide the generator to use for our
function, because in a dynamically-typed language we can't simply extrapolate
that.

## Shrinking

Some QuickCheck implementations have a feature called shrinking. This allows
failing tests to be shrunk to 'smaller', failing cases, where 'smaller' is a
data-type specific thing, generally something that'd be easier for the
programmer to debug. For example, a list with a few elements is going to be
easier for the programmer to debug than a list with one-thousand elements. In
Haskell QuickCheck, random element generation and shrinking are treated
separately. That is to say, if you want your type to shrink, you have to
implement that separately from generating random values of your type. Let's
take a look at the arbitrary typeclass.

{% highlight haskell %}
class Arbitrary a where
  arbitrary :: Gen a

  shrink :: a -> [a]
  -- default implementation
  shrink _ = []
{% endhighlight haskell %}

Most (all?) of the standard Prelude types have an `Arbitrary` instance already
written for this, but you'll need to write one for your own types. You'll
almost certainly write your implementation of `arbitrary` based on the provided
generator-combinators, like `choose`, `elements` and `oneof`. If we want our
type to be able to shrink, we'll have to implement this on our own. This is due
to the fact that value generation, and value shrinking are treated separately.
_simple-check_ and EQC take a different approach. When you write a generator,
using similar generator-combinators as in Haskell, you get shrinking 'for
free'. That's because the notion of generating values and shrinking are tied
together in these implementations. This is really handy, because it saves us
from having to write boilerplate code to implement shrinking. Further because
it's not nearly as common to create our own types in Clojure and Erlang (it's
not even possible in Erlang), we don't want to have to create our own new type
solely to implement some shrink protocol. Further, even implicit constraints in
our generator are respected during shrinking. For example, suppose we write a
new generator which multiplies randomly generated integers by two. This will
always result in an even number being generated, and this will remain true
during shrinking. This works because in simple-check, instead of the arbitrary
function generating random values, we generate random values, along with the
shrink tree for that value. Erlang QuickCheck is proprietary, but I imagine it
works similarly. Let's imagine how this might look using Haskell's types:

{% highlight haskell %}
-- a RoseTree is just an n-ary tree
data RoseTree a = RoseTree a [RoseTree a]

class Arbitrary a where
  arbitrary :: Gen (RoseTree a)
{% endhighlight haskell %}

The top of the tree is a randomly generated value, and its children are the
first level of shrinking the value. Generator combinators can then manipulate
this shrink tree. As a result of this, we now have two nested monads in our
generators, `Gen` and `RoseTree`. A concrete type might be `Gen (RoseTree
Int))`. This means that before in Haskell QuickCheck, were we'd previously
`fmap`'d over a generator, we'd likely now want to `fmap` over both the
`Gen`, and the `RoseTree`. This is simple, since we just need `fmap` with
`fmap` as our function. Specifically: `mapGen f gen = fmap (fmap f) gen`.
Mimicing `>>=` is a little more complex, as we need a function of type: `Gen
(RoseTree a) -> (a -> Gen (RoseTree b)) -> Gen (RoseTree b)`. This is, in fact
no longer `>>=`, but provided similar functionality to the `>>=` for our old
`Gen`. This is a little trickier to implement, but `Data.Traversable.sequence`
gets us there.
