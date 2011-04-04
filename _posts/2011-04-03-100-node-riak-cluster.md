---
layout: post
title: 100-Node Riak Cluster for $2
---

Riak is a distributed key-value store; data is replicated and partitioned
across your cluster. Increasing the cluster size allows you to scale both performance and
fault-tolerance. One of the most powerful parts of [Riak](http://wiki.basho.com/ "Riak") is the ability to add a new node to your cluster with one command:

    riak-admin join riak@example.com

With the recent trend toward [operations-as-code](http://en.wikipedia.org/wiki/DevOps "DevOps"),
I thought I would challenge myself to write a script to set up a 100-node Riak cluster with
one command. Using [Amazon EC2 micro-instances](http://aws.amazon.com/about-aws/whats-new/2010/09/09/announcing-micro-instances-for-amazon-ec2/ "micro instances"), the cluster costs $2 to run for an hour.

Riak works by splitting a 160-bit hash-space into a certain number of  
[virtual nodes (vnodes)](http://wiki.basho.com/How-Things-Work.html#The-Ring "vnode"), say 1024.
Each physical node is then responsible for `1024 / N` vnodes, where `N` is the number of physical
nodes in the cluster. As a new node joins, it takes some vnodes from the rest of
the cluster.

I've written a simple Python script to launch a 100-node cluster.
The script launches a master node, and notes its IP address. 
The other 99 nodes are launched and told to join the master. Riak doesn't currently have provisions
to deal with many nodes trying to join the cluster at once. To avoid the 
[thundering-herd problem](http://en.wikipedia.org/wiki/Thundering_herd_problem "thundering herd problem")
I simply have each node sleep for a random time, such that nodes are joining, on average,
one every 15 seconds. Some sort of queueing system, and
[this bugfix](https://issues.basho.com/show_bug.cgi?id=869 "bug 869"), would eliminate the
need for nodes to stagger their join requests. [Here is](https://gist.github.com/891586) a snippet
from the Riak IRC about this.
I didn't get a chance to try it, but using Chef-server, there's also a
[Riak cluster recipe](https://github.com/opscode/cookbooks/blob/master/riak/providers/cluster.rb "cluster recipe").

After getting my script working with a 20-node cluster, I tried to launch 100, only to learn
that AWS accounts are, by default, limited to 20 instances. Fortunately, the 
[spot instance](http://aws.amazon.com/ec2/spot-instances/ "EC2 spot instance") limit is
100, so I was able to use those.


The script is simple, and usage looks like:

    ./launch.py keypair ~/.ssh/keypair.pem user_data.sh 100

Approximately 35 minutes after running the script, I had a 95-node cluster. The command
`riak-admin ringready` told me that two nodes were down. After starting them, 
I had a 97-node cluster. I wasn't able to
diagnose the problem with the other three nodes.
I was impressed with how easy it was to automate Riak, and it's clear that
[Basho](http://www.basho.com/ "Basho") has plans to make things even easier.

Now is a good time to note that the script doesn't launch a truly production-ready cluster.
For starters, it probably isn't a good idea to use spot instances for a database.
You would also be wise to have a smaller number of more powerful machines, rather than
100 micro instances. Next, I would recommend something like 
[Chef](http://www.opscode.com/chef/ "Chef"), for more complicated infrastructure automation.

If you'd like to run your own 100-node cluster, check out
[this github repository](https://github.com/reiddraper/riak-ec2-cluster-launcher).
If you decide to keep your cluster up for more than an hour,
[here's some data](http://wiki.basho.com/Sample-Data.html "Riak sample data")
to play with.

It's exciting to see how infrastructure automation is making it easy for small teams
to build massive systems in short periods of time. Databases like Riak fit perfectly
with this, as their administrative cost is low, and configuration remains simple 
regardless of how many nodes are in the cluster. 

For those of you considering writing something similar, I highly recommend trying
[Vagrant](http://vagrantup.com/ "Vagrant") for testing virtual machine setups before
spending a dime on EC2.

