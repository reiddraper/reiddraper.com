---
title: Introducing Knockbox
---

For the past few weeks I've been working on a Clojure
library called [knockbox](https://github.com/reiddraper/knockbox).
It's a library meant to make dealing with conflict-resolution
in eventually-consistent databases easier. If you're not familiar
with eventual-consistency, I'd suggest
[this](http://www.allthingsdistributed.com/2008/12/eventually_consistent.html) article
by Amazon CTO Werner Vogels.

Distributed databases like [Riak](https://github.com/basho/riak) let you trade
consistency for availability. This means that at any given moment,
all of the replicas of your data might not be synchronized.
In exchange for this, your database cluster can still operate when
all but one replica of your data is unavailable. Amazon's shopping-cart
session state has been the iconic example. In their case, a write to add an
item to your cart may go to a replica that is not up to date. At some point,
the database notices that the replicas are in conflict, and you must resolve them.
But how do you do this? If a coffee maker is in one replica and not the other, what happened?
Was the coffee maker recently added and that just hasn't been reflected in the other replica yet?
Or was the coffee maker recently deleted? It turns out that you often have to change the
way you represent your data in order to preserve the original intentions.

Developers who wanted to implement data-types with conflict-resolution semantics
have had to figure it out themselves, or read academic papers like
[A comprehensive study of Convergent and Commutative Replicated Data Types](http://hal.archives-ouvertes.fr/inria-00555588/).
[statebox](https://github.com/mochi/statebox) was the first popular open source
project to help ease the burden for developers wanting to take advantage of
eventual-consistency. As I've been learning Clojure recently, I thought
I'd try my hand at putting together a similar library.

The main goal has been to have the types conform to all appropriate
Clojure Protocols and Java interfaces. This means my last-write-wins
set should quack like a normal Clojure set. This lets you reuse existing
code that expects normal Clojure data types. Next, I've defined
a `Resolvable` Protocol for all of these types to implement. There's
only a single method, which looks like:

```clojure
(resolve [a b])
```

This function should take two conflicing objects and return a new,
resolved object.

Resolving a list of replicas (often called siblings when they're in conflict)
is as simple as providing the `resolve` function to `reduce`. This is, however,
provided for you, as `knockbox.core/resolve`. Note that this function is in
a different namespace than the `resolve` that you implement as part of
the `Resolvable` Protocol (this lives in `knockbox.resolvable`).

There are currently two data-types implemented, sets and registers.
A register is simply a container for another type. I also intend to
implement counters, but have yet to come up with an implementation
that has space-efficiency and pruning characteristics that I like.

Let's now create some conflicting replicas, and see see how they
get resolved. Here we'll use a last-write-wins (`lww`) set. The resolution
semantics used here are to use timestamps to resolve an add/delete
conflict for a particular item. This is not the same as using
timestamps for the whole set, because we're doing it per
item. To get a REPL with the correct classpath, you
can either add `[knockbox "0.0.1-SNAPSHOT"]` to your `project.clj`,
or clone the knockbox repository and type `lein repl`.

```clojure
(require 'knockbox.core)
(require '[knockbox.sets :as kbsets])

(def original (into (kbsets/lww) #{:mug :kettle}))

(def a (disj original :kettle))
(def b (conj original :coffee))

(def c (conj original :coffee-roaster))
;; this one wins because its
;; timestamp is later
(def d (disj original :coffee-roaster))

(println (knockbox.core/resolve [a b c d]))
; => #{:coffee :mug}

;; notice that this is different
;; than simply taking the union of
;; the four sets
(println (clojure.set/union a b c d))
; => #{:coffee :coffee-roaster :kettle :mug}
```

Using timestamps is fine for some domains, but what if our update-rate is high
enough that we can't trust our clocks to be synchronized enough? The
`observed-remove` set works by assigning a UUID to each addition. Deletes
will then override any UUIDs they have seen for a particular item in the set.
This means that when add/delete conflicts happen, addition will win because
the delete action couldn't have seen the UUID created by the addition. Let's
see this in action.

```clojure
(require 'knockbox.core)
(require '[knockbox.sets :as kbsets])

(def original (into (kbsets/observed-remove) #{:gin :rum}))

(def a (conj original :vodka))
(def b (conj original :vodka))

;; we've only seen the addition
;; of :vodka from a, not b
(def c (disj a :vodka))

;; don't include a in here because
;; vector clocks will take care of
;; figuring out that c supersedes it
(println (knockbox.core/resolve [b c]))
; => #{:vodka :gin :rum}
```

That's all for this first post, so go ahead and take a look
at [knockbox on github](https://github.com/reiddraper/knockbox).
