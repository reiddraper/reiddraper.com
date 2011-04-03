---
layout: post
title: $2, 100-Node Riak Cluster
---

One of the most powerful parts of [Riak](http://wiki.basho.com/ "Riak") is the ability
to add new nodes to your cluster with one command. I thought it would be cool to take
advantage of this using [Amazon EC2](http://aws.amazon.com/ec2/ "EC2"), and see if I could
launch a 100-node cluster for one hour. I decided to use 
ec2 [micro instances](http://aws.amazon.com/about-aws/whats-new/2010/09/09/announcing-micro-instances-for-amazon-ec2/ "micro instances"),
which cost $0.02 per hour.

Riak is a distributed key-value store. Data is replicated and partitioned
across your cluster. Increasing the cluster size allows you to scale both performance and
fault-tolerance. Adding a new node to the cluster is as simple as running:

  
    riak-admin join riak@example.com


Riak works by splitting a 160-bit hash-space into a certain number of 
[vnodes](http://wiki.basho.com/How-Things-Work.html#The-Ring "vnode"), say 1024.
Each node is then responsible for `1024 / N` vnodes, where N is the number of physical
nodes in the cluster. When a node join the cluster, vnodes are redistributed. You can check
on the status of this redistribution by running:

    riak-admin ringready


I wrote a simple Python script to launch and configure 100 nodes, and have them join
into a cluster.
