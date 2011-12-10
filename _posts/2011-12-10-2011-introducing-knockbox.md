---
layout: post
title: Introducing Knockbox
---

For the past few weeks I've been working on a Clojure
library called [Knockbox](https://github.com/reiddraper/knockbox).
It's a library meant to make dealing with conflict-resolution
in eventually-consistent databases easier. While an introduction
to eventual-consistency is outside the scope of this post, I'll
explain enough to motivate the project.

Distributed databases like [Riak](TODO: add URL) let you trade
consistency for availability. This means that at any given moment,
all of the replicas of your data might not be synchronized.
In exchange for this, your database cluster can still operate when
all but one replica of your data is unavailable.
