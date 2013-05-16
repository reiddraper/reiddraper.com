---
layout: post
title: Data Traceability
---

This text appears as Chapter 17 in O'Reilly's [Bad Data
Handbook](http://shop.oreilly.com/product/0636920024422.do) (ISBN-13:
978-1449321888). It is released under the [CC
BY-SA](http://creativecommons.org/licenses/by-sa/3.0/) license.


___

Your software consistently provides impressive music recommendations
by combining cultural and audio data. Customers are happy.
However, things aren't always perfect. Sometimes that Beyoncé track is
attributed to Beyonce. The artist for the Béla Fleck solo album shows up as
Béla Fleck and the Flecktones. Worse, the ボリス biography
has the artist name listed as ???. Where did things go wrong?
Did one of your customers provide you with data in an incorrect
character encoding? Did one of the web-crawlers have a bug? Perhaps
the name resolution code was incorrectly combining a solo artist
with his band?

How do we solve this problem?
We'd like to be able to trace data back to it's origin, following
each transformation. This is reified as _data provenenace_.
In this chapter, we'll explore ways of keeping track of
the source of our data, techniques for backing out
bad data, and the business value of adopting such
ability.

## Why?

The ability to trace a datum back to its origin is important
for several reasons. It helps us to back-out or reprocess bad data,
and conversely, it allows us to reward and boost good data
sources and processing techniques. Furthermore, local privacy
laws can mandate things like auditability, data transfer
restrictions and more. For example, California's Shine the Light
Law requires businesses disclose the personal information that has
been shared with third-parties, should a resident request. Europe's
Data Protection Directive provides even more stringent regulation
to businesses collecting data about residents.

We'll also later see how data traceability can provide further
business value by allowing us to provide stronger measurements
on the worth of a particular source, realize where to
focus our development effort, and even manage blame.

## Personal Experience

I previously worked in the data ingestion team at a music data
company. We provided artist and song recommendations, artist
biographies, news, and detailed audio analysis of digital music.
We exposed those data feeds via web services and raw dumps.
Behind the scenes, these feeds were composed of many sources of
data, which which were in turn cleaned, transformed, and put
through machine learning algorithms.

One of the first issues we ran into was learning how to trace a
particular result back to its constituent parts. If a given
artist recommendation was poor, was it because of our machine
learning algorithm? Did we simply not have enough data for that
artist? Was there some obviously wrong data from one of our
sources? Being able to debug our product became a business
necessity.

We developed several mechanisms for being able to debug our data
woes, some of which I'll explore here.

### Snapshotting

Many of the data sources were updated frequently. At the same
time, the web pages we crawled for news, reviews, biography
information and similarity, were updated inconsistently. This
meant that even if we were able to trace a particular datum back
to its source, that source may have been drastically different
than the time we had previously crawled or processed the data. In
turn, we needed to not only capture the source of our data, but
the time, and exact copy of the source. Our database columns or
keys would then have an extra field for a timestamp.

Keeping track of the time and the original data also allows you
to track changes from that source. You get closer to answering
the question, "why were my recommendations for The Sea and Cake
great last week, but terrible today?"

This process of writing data once and never changing it is called
_immutability,_ and it plays a key role in data traceability.
I'll return to it later, when I walk through an example.

### Saving the source

Our data was stored in several different types of databases, including
relational and key-value stores. However, nearly every schema had
a _source_ field. This field would contain one or more values.
For original sources there would be a single source listed.
As data was processed and transformed into roll-ups or
learned-data, we would preserve the list of sources that went
into creating that new piece of data. This allowed us to trace
the final data product back to its constituent parts.

### Weighting sources

One of the most important reason we collected data was to learn
about new artists, albums and songs. That said, we didn't always
want to create a new entity that would end up in our final data
product. Certain data sources were more likely to have errors,
misspellings and other inaccuracies, so we wanted them to be
vetted before they would progress through our system.

Furthermore, we wanted to be able to give priority processing to
certain sources that either had higher information value or were
for a particular customer. For applications like learning about
new artists, we'd assign a trust-score to each source that would,
among other things, determine whether a new artist was created.

If the artist wasn't created based solely on this source, it
would add weight to that artist being created if we ever heard of
them again. In this way, the combined strength of several
lower-weighted sources could lead the artist being created in our
application.

### Backing out data

Sometimes we identified that data was simply incorrect or otherwise
bad. In such cases, we had to both remove the data from our
production offering.

Recall, our data would pass through several stages of transformation
on its way to the production offering. A backout, then, required that
we first identify potential sources of the bad data, remove it,
then reprocess the product without that source. (Sometimes the data
transformations were so complex that it was easier to generate all
permutations of source data, to spot the offender.) This is only
possible since we had kept track of the sources that went into the
final product.

Because of this observation, we had to make it easy to redo any
stage of the data transformation with an altered source list. We
designed our data processing pipeline to use parameterized source
lists, so that it was easy to exclude a particular source, or
explicitly declare the sources that were allowed to affect this
particular processing stage.

### Separating phases (and keeping them pure)

Often we would divide our data processing into several stages. It's
important to identify the state barriers in your application, as doing
this allowed us to both write better code, and create more efficient
infrastructure.

From a code perspective, keeping each of our stages separate allowed us to
reduce side effects (such as I/O). In turn, this made code easier to
test, because we didn't have to set up mocks for half of our side-effecting
infrastructure.

From an infrastructure perspective, keeping things separate allowed us to make
isolated decisions about each stage of the process, ranging from compute power,
to parallelism, to memory constraints.

### Identifying the root cause

Identifying the root cause of data issues is important to being able
to fix them, and control customer relationships. For instance, if a
particular customer is having a data quality issue, it is helpful to
know whether the origin of the issue was from data they gave you, or
from your processing of the data they gave you. In the former case,
there is real business value in being able to show the customer the
exact source of the issue, as well as your solution.


### Finding areas for improvement

Related to blame is the ability to find sources of improvement in your
own processing pipeline and infrastructure. This means the steps in
your processing pipeline become data sources in their own right.

It's useful to know, for instance, when and how you derived a certain
piece of data. Should an issue arise, you can immediately focus on
the place it was created. Conversely, if a particular processing stage
tends to produce excellent results, it is helpful to be able to
understand why that is so. Ideally you can then replicate this into
other parts of your system.

Organizationally, this type of knowledge also allows you to determine
where to focus your teams' effort, and even to reorganize your team
structure. For example, you might want to place a new member of the
team on one of the infrastructure pieces that is doing well, and
should be a model for other pieces, as to give them a good starting
place for learning the system. A more senior team member may be more
effective on pieces of the infrastructure that are struggling.

## Immutability: borrowing an idea from functional programming

Considering the examples above, a core element of our strategy was
_immutability_: even though our processing pipeline transformed our
data several times over, we never changed (overwrote) the original
data.

This is an idea we borrowed from functional programming. Consider
imperative languages like C, Java and Python, in which data tends to
be mutable. For example, if we want to sort a list, we might call
`myList.sort()`. This will sort the list in-place. Consequently, all
references to `myList` will be changed. If we now want review
`myList`'s original state, we're out of luck: we should have made a
copy before calling `sort()`.

By comparison, functional languages like Haskell, Clojure and Erlang
tend to treat data as immutable. Our list sorting example becomes
something closer to `myNewSortedList = sort(myList)`. This retains the
unsorted list `myList`. One of the advantages of this immutability is
that many functions become simply the result of processing the values
passed in. Given a stack trace, we can often reproduce bugs
immediately.

With mutable data, there is no guarantee that the value of a
particular variable remains the same throughout the execution of the
function. Because of this, we can't necessarily rely on a stack trace
to reproduce bugs.

Concerning our data processing pipeline, we could save each step of
transformation and debug it later. For example, consider this
workflow:

    rawData = downloadFrom(someSite)
    cleanData = cleanup(rawData)
    newArtistData = extractNewArtists(cleanData)

Let's say we've uncovered a problem in the `cleanup()` function. We
would only have to correct the code and rerun that stage of the
pipeline. We never replaced `rawData` and hence it would be
available for any such debugging later.

To take further advantage of immutability, we persisted our data under a
compound key of identifier and
timestamp. This helped us find the exact inputs to any of our data
processing steps, which saved time when we had to debug an issue.

## An Example

As an example, let me walk you through creating a news aggregation
site. Along the way, I'll apply the lessons I describe above to
demonstrate how data traceability affects the various aspects of the
application.

Let's say that our plan is to display the top stories of the day, with
the ability to drill down by topic. Each story will also have a link
to display coverage of the same event from other sources.

We'll need to be able to do several things:

1. Crawl the web for news stories.
1. Determine a story's popularity and timeliness based on social media
activity, and perhaps its source. (For example, we assume a story on
the New York Times homepage is important and/or popular).
1. Cluster stories about the same event together.
1. Determine event popularity. (Maybe this will be aggregate popularity
of the individual stories?)

### Crawlers

We'll seed our crawlers with a number of known news sites. Every so
often we'll download the contents of the page and store it under a
composite key with URL, source and timestamp, or a relational database
row with these attributes. (Let's say we crawl frequently-updated
pages several times a day, and just once a day for other pages.)

From each of these home pages we crawl, we'll download the individual
linked stories. The stories will also be saved with URL, source and
timestamp attributes. Additionally, we'll store the composite ID of
the homepage where we were linked to this story. That way if, for
example, later we suspect we have a bug with the way we assign story
popularity based on home page placement, we can review the home page
as it was retrieved at a particular point in time. Ideally we should
be able to trace data from our own homepage all the way back to the
original HTML that our crawler downloaded.

In order to help determine popularity, and to further feed
our news crawlers, we'll also crawl social media
sites. Just like with the news crawlers, we'll want
to keep a timestamped record of the HTML and other assets
we crawl. Again, this will let us go back later and debug
our code. One example of why this would be useful is if
we suspect we are incorrectly
counting links from shares of a particular article.

### Change

Keeping previous versions of the sites we crawl allows for some
interesting analytics. Historically, how many articles does the Boston
Globe usually link to on their home page? Is there a larger variety of
news articles in the summer? Another useful byproduct of this is that
we can run new analytics on past data. Because immutability can give
us a basis from the past, we're not confined to just the data we've
collected since we turned on our new analytics.

### Clustering

Clustering data is a difficult problem. Outlying or mislabeled data
can completely change our clusters. For this reason, it is important to
be able to cheaply (in human and compute time) be able to experiment with
rerunning our clustering with altered inputs. The inputs we alter may
remove data from a particular source, or add a new topic modelling
stage between crawling and clustering. In order to achieve this, our
infrastructure must be loosely coupled such that we can just as easily
provide inputs to our clustering system for testing as we do in production.

### Popularity

Calculating story popularity shares many of the same issues as
clustering stories. As we experiment, or debug an issue, we want to
quickly test our changes and see the result. We also want to see the
most popular story on our own page and trace all the way through our
own processing steps, back to the origin site we crawled. If we find
out we've ranked a story as more popular that we would've liked, we
can trace it back to our origin crawl to see if, perhaps, we had put
too much weight in its position on its source site.

## Conclusion

You will need to debug data processing code and infrastructure just
like normal code. By taking advantage of techniques like immutability,
you can dramatically improve your ability to reason about your system.
Furthermore, we can draw from decades of experience in software design
to influence our data processing and infrastructure decisions.
