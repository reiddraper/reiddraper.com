---
title: Writing simple-check
published: 2013-11-03 00:00:00
---

For the past several months I've been working on a
[QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) (QC) library for Clojure:
[simple-check](https://github.com/reiddraper/simple-check). In this post, we'll
look at three issues I ran into porting QC from Haskell to Clojure: typing,
shrinking, and laziness. This will not act as an introduction to QC, or
property-based testing. Further, this post assumes some familiarity with
Haskell and Clojure.

## Typing

One of the major differences between writing a QC in a statically-typed
language and a dynamically-typed language is that with static-types, we get to
use that information to inform QC of the generators to use to test our
function. For example, if our function has the type `[Int] -> Bool`, Haskell QC
will use this information to generate `[Int]`s. Furthermore, this takes
advantage of the fact the we can be polymorphic on _return_ type in Haskell.
The `Arbitrary` type class in Haskell has a function, `arbitrary`, whose
signature is `Gen a`. This allows the compiler to fill in the specialized
version of `Gen a` for us, depending on context. In Clojure, we can only use
type-based dispatch on an _argument_, not the return value. So, in
dynamically-typed languages, we resort to explicitly specifying the generators
to use for our test. Let's see a concrete example:

In Haskell:
```haskell
sortIdempotent :: [Int] -> Bool
sortIdempotent xs = (sort xs) == (sort (sort xs))

quickCheck sortIdempotent
-- +++ OK, passed 100 tests.
```

In Clojure:
```clojure
(defn sort-idempotent?
  [coll]
  (= (sort coll) (sort (sort coll))))

(sc/quick-check 100
  (prop/for-all [coll (gen/vector gen/int)]
    (sort-idempotent? coll)))
;; {:result true, :num-tests 100, :seed 1383433754854}
```

In Erlang (also dynamically typed), using [Erlang QuickCheck (EQC)](http://www.quviq.com/index.html):
```erlang
sort_idempotent(Xs) ->
  lists:sort(Xs) =:= lists:sort(lists:sort(Xs)).

prop_sort_idempotent() ->
    ?FORALL(Xs, list(int()),
            sort_idempotent(Xs)).

eqc:quickcheck(prop_sort_idempotent()).
%% OK, passed 100 tests
```

As you can see, with **simple-check** and Erlang QuickCheck, we have to
explicitly provide the generator to use to test our function.

## Shrinking

Some QC implementations have a feature called shrinking. This allows failing
tests to be shrunk to 'smaller' failing cases, where 'smaller' is data-type
specific, something that'd be easier for the programmer to debug. For example,
if your function fails with a randomly-generated 100-element list, QC will try
and remove elements, as long as the test continues to fail. In Haskell
QuickCheck, random element generation and shrinking are treated separately.
That is, if you want your type to shrink, you have to implement that separately
from generating random values of your type. Let's see the type class where
these two functions live, `Arbitrary`:

```haskell
class Arbitrary a where
  arbitrary :: Gen a

  -- the returned list is the first-level of the shrink tree
  shrink :: a -> [a]
  -- default implementation
  shrink _ = []
```

Most (all?) of the standard Prelude types have an `Arbitrary` instance already
written, but you'll need to write one for your own types. Generally you'll
write your implementation of `arbitrary` based on the provided
generator-combinators, like `choose`, `elements` and `oneof`. If you want your
type to shrink, you'll have to implement this on your own. Again, this is due
to the fact that value generation and shrinking are treated separately.
_simple-check_ and Erlang QuickCheck take a different approach. When you write
a generator, using generator-combinators, you get shrinking 'for free'. That's
because the notion of generating values and shrinking are tied together in
these implementations. This is handy because it saves us from having to write
boilerplate code to implement shrinking. Further, because it's not nearly as
common to create our own types in Clojure, let along possible in Erlang, we
don't want to have to create our own new type solely to implement some shrink
protocol. As a result, even implicit constraints in our generator are respected
during shrinking. For example, suppose we write a new generator which
multiplies randomly generated integers by two. This will always result in an
even number being generated, and this will remain true during shrinking.  This
works because in simple-check, instead of the arbitrary function generating
random values, we generate random values, along with the shrink tree for that
value. Erlang QuickCheck is proprietary, but I imagine it works similarly.
Let's imagine how this might look using Haskell's types:

```haskell
-- a RoseTree is just an n-ary tree
data RoseTree a = RoseTree a [RoseTree a]

class Arbitrary a where
  -- instead of generating an `a`, we generate a shrink tree of `a`
  arbitrary :: Gen (RoseTree a)
```

The top of the tree is a randomly generated value, and its children are the
first level of shrinking. Generator-combinators can then manipulate
this shrink tree. Because we now act on these shrink trees, we simply create
larger trees as we create more complex generators. To give a concrete example,
the expression `(fmap (partial * 2) gen/int)` will create a new generator based
on `gen/int`, which multiplies the randomly generated elements by two. But
since this function is also applied to the children in the shrink tree, every
element in the shrink tree will be multiplied by two. We can also now write
generator-combinators like `elements`, which creates a generator by choosing a
random element from a list. This generator will shrink toward choosing earlier
elements in the list. Were we to use `elements` in our `arbitrary` function in
Haskell QC, we'd have to write the shrinking logic ourselves. It's
important to note, however, that this is specific to Haskell QC, and
not the language itself, we could've implemented Haskell's QC as
described here.

## Laziness

Haskell QuickCheck takes advantage of whole-program laziness. For example, when
shrinking, instead of traversing a tree of arguments to the function under
test, and applying to values to the function the tree is traversed, we're able
to use `fmap` to lazily apply to function to the entire tree. We then need only
traverse a tree of booleans (representing test success or failure). This allows
for a higher-level of abstraction. Fortunately, Clojure lets us mimic this, as
long as our types are represented as lazy sequences. To represent a large tree,
we use a two-element vector, where the first element is the top value in the
tree, and the second element is a lazy sequence, representing the children.
Using Clojure's lazy functions like `map`, `filter` and `concat`, we're able to
retain this laziness as we process the tree. However, as this tree can become
large when fully-evaluated, finding bugs can be difficult. In Haskell, we're
able to find type-mistakes during compilation, whereas in Clojure we need to
run our program, potentially sifting through a large tree to find our bugs,
which may have been introduced several call-sites away from where we're
looking. In order to combat this, I specifically debugged with values I knew
had small shrink trees, and could be easily printed at the REPL.
