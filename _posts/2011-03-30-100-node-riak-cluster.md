---
layout: post
title: $2, 100-Node Riak Cluster
---

One of the most powerful parts of [Riak](http://wiki.basho.com/ "Riak") is the ability
to add new nodes to your cluster with one command. I thought it would be cool to take
advantage of this using [Amazon EC2](http://aws.amazon.com/ec2/ "EC2"), and see if I could
launch a 100-node cluster for one hour. I decided to use 
EC2 [micro instances](http://aws.amazon.com/about-aws/whats-new/2010/09/09/announcing-micro-instances-for-amazon-ec2/ "micro instances"),
which cost $0.02 per hour.

Riak is a distributed key-value store, data is replicated and partitioned
across your cluster. Increasing the cluster size allows you to scale both performance and
fault-tolerance. Adding a new node to the cluster is as simple as:

  
    riak-admin join riak@example.com


Riak works by splitting a 160-bit hash-space into a certain number of 
[vnodes](http://wiki.basho.com/How-Things-Work.html#The-Ring "vnode"), say 1024.
Each node is then responsible for `1024 / N` vnodes, where `N` is the number of physical
nodes in the cluster. When a node join the cluster, vnodes are redistributed. You can check
on the status of this redistribution by running:

    riak-admin ringready


I've written a simple Python script to launch and configure 100 nodes, and have them join
into a cluster. The script works by launching a master node, which has some known IP address.
The other 99 nodes use this address to join the cluster. Riak doesn't currently have provisions
to deal with many nodes trying to join the cluster at once, so to avoid the herd effect, I
simply have each node sleep for a random time, such that nodes are joining, on average,
one every 15 seconds. [This enhancement](https://issues.basho.com/show_bug.cgi?id=869 "bug 869"), along with some sort of cluster-joining queue system would eliminate the need for nodes
to stagger their cluster-join requests. [Here is](https://gist.github.com/891586) a snippet
from the Riak IRC about this, as noted in the 
[Riak recap](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-March/003654.html).

After getting my script working with a 20-node cluster, I went to launch 100, only to learn
that AWS accounts are, by default limited, to 20 nodes. Fortunately, the 
[spot instance](http://aws.amazon.com/ec2/spot-instances/ "EC2 spot instance") limit is
100 nodes. So, as it turns out, the cluster will likely run you under $2 for one hour.

Approximately 35 minutes after running the script, I had a 95 machine cluster. `riak-admin ringready` told me that two nodes were down. After restarting them, I had a 97 machine cluster.
In the time that remained of my one hour, I wasn't able to diagnose the problem with the
remaining three nodes. That being said, a 3% failure rate, using a simple sleep statement 
to stagger the nodes, is pretty impressive!

The recent trend toward operations-as-code is continuing to make it easy for small teams
to build truly massive, automated infrastructure in a short period of time. Databases
like Riak fit perfectly with this, as their administrative cost is low, and remains simple,
regardless of how many nodes you have in your system.

If you'd like to run your own $2, 100 node cluster, check out this github repository.

For those of you considering writing something similar, I highly recommend trying
[Vagrant](http://vagrantup.com/ "Vagrant") for testing virtual machine setups, before
spending a dime on EC2.
