---
title: Client-server coupling
published: 2014-01-23 00:00:00
---

# Outline

1. Intro
    1. Describe current state
        * more complicated applications
        * more compile-to-js languages
        * single-page applications
    1. Introduce RPC as appealing solution. It's bad.
    1. You're building a distributed system
1. RPC
    1. Problems (relate each to poor abstraction)
        1. the 'streaming problem'
        1. the 'continuation problem'
        1. versioning
            * ad hoc solutions
            * can your distributed database do a rolling upgrade?
        1. security
        1. cost
        1. failure
    1. Known problems:

1. Solutions?

## Questions

1. How to deal with wide image on mobile?
1. Make sure this doesn't come across too negative or preachy.
1. Title? Relate to coupling? Or just poor abstraction.

---

Web applications are quickly becoming more complex, and are more and more being
written in languages that compile to Javascript. With this, there is a
proliferation of libraries and code that are intended to be run on both a
browser and server. The rationale is that this is code that would be duplicated
otherwise. Taking things further, if the browser and server are running the
same code, it's common to wonder if we can abstract over the network. To treat
remote calls as if they were local. After all, the browser and server are
not only written in the same langauge, but are running the same code. This,
however, turns out to be fraught with issues. This post argues that not only
does this approach present major technical issues, it's a path we've been down
before. The flaws of RPC (remote procedure calls) are known, and have been
discussed for years. This is just a reframing of hte same issues that were
faced years ago with server-server communicaton, but now with server-browser.
We'll now dig in to specific problems with RPC.

Let's start by addressing several of the most visible problems with RPC.
Remember, the core ethos of RPC is to pretend that two machines are actually
one, using the same language semantics for cross-machine communication, as
local. Namely, functions, methods and objects. This ends up being a (very)
leaky abstraction for several reasons:

* The two machines are _not_ always running the same code. Imagine if all of
  your users had to refresh their browser at _exactly_ the same time every time
  you pushed new code to the server. Your server-code needs to be able to
  handle old versions of the javascript running on the browser, even if it's
  just (annoyingly) forcing them to refresh).

* We have complete visiblity into why a local funciton fails. We even get nice
  stack traces that show us just how much of hte function was executed before
  things went awry. This is not true of communication over the network. We
  might never hear back. We might have executed the code one machine, which
  caused another RPC call to be made, and that one never completed. What do we
  do then? In distributed systems, we care whether our funcitons are
  idemptoent. We might have to re-executed them if we're not sure if htey've
  succeeded. By and large, we don't have this issue with our normal, local
  programming model.

* If normal language semantics don't even adequitely manage communcation from
  machine A->B->A, they have no chance in modelling more complicated
  communicaton graphs, like A->B->C.

* Local and remote functions have a compeltely different cost model. Local
  functions can be measured, and their runtime predicted. Netowkr failures are
  unpredictable, and can make remote communication suddenly take hours. Or
  never complete. Separate machines work in a completely different failure
  domains, whereas typically, a single program runs only in one failure domain
  (ignoring some complications with multi-threaded programs).

Since these issues are inevitable, and yet people continue to ignore them. What
is going to happen. ONe thing is that solutions to some of these issues become
ad hoc, and poorly thought out. FOr example, if you change a data-structure on
the server, and the browsers havent' all started to use the new structure yet,
you might still get requests using this old format. An ad hoc soltuion is more
likely to band-aid the issue thoruhgout the code-base, 'upgrading' the data
when its necessariy. This isparitcularly scary in a dynamically-typed langauge,
where this can proliferate and have no cost seen at compile-time.

This sentence ends with a references [^1], and actually has another [^2].

![sync](/images/coupling.png)

# Notes

# References

## RPC

1. [Implementing Distributed Systems Using Linear Naming](http://dspace.mit.edu/handle/1721.1/7085)
    * The "streaming problem" (p. 16)
    * The "continuation problem" (p. 16-17)
1. [Implementing Remote Procedure Calls. 1984](http://www.cs.princeton.edu/courses/archive/fall03/cs518/papers/rpc.pdf)
1. [Remote Procedure Call - Nelson, 1981](http://bitsavers.trailing-edge.com/pdf/xerox/parc/techReports/CSL-81-9_Remote_Procedure_Call.pdf)

### Bad
1. http://steve.vinoski.net/pdf/IEEE-Serendipitous_Reuse.pdf
1. http://steve.vinoski.net/pdf/IEEE-Demystifying_RESTful_Data_Coupling.pdf
1. http://steve.vinoski.net/pdf/IEEE-Convenience_Over_Correctness.pdf
1. http://steve.vinoski.net/pdf/IEEE-RPC_Under_Fire.pdf
1. http://www.win.tue.nl/~johanl/educ/2II45/2010/Lit/Tanenbaum%20RPC%2088.pdf

## Distributed system
1. http://queue.acm.org/detail.cfm?id=2482856

## Code sharing examples

1. [Meteor.js with Matt Debergalis](https://www.youtube.com/watch?v=vpp-8gkPWVE)
1. [yesod-fay](https://hackage.haskell.org/package/yesod-fay)
1. [fay happstack](http://happstack.com/page/view-page-slug/15/happstack-fay-acid-state-shared-datatypes-are-awesome)
1. [clojurescript - shoreleave](https://github.com/ohpauleez/shoreleave)
1. [clojurescript - hoplon](http://hoplon.io/#/home/)
1. [node.js rpc](https://nodejsmodules.org/tags/rpc)
1. [dnode - node.js](https://github.com/substack/dnode)
1. [now.js](https://github.com/Flotype/now) (nowjs might be dead?)

# References 2

[^1]: [pandoc](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
[^2]: [google](https://google.com)
