---
layout: post
title: Writing Your First Chef Recipe
---

[Chef](http://www.opscode.com/chef/ "Opscode Chef") is an infrastructure automation tool
that lets you write Ruby code to describe how your machines should be set up.
Applications for Chef vary from configuring complicated multi-node applications, to
[setting up your personal workstation](http://jtimberman.posterous.com/managing-my-workstations-with-chef).

As great as Chef is, getting started can be a bit daunting. It's worse if you're not
sure exactly what Chef provides, and you've never written a lick of Ruby. This was
me a few days ago, so I thought I'd write a quick Chef introduction with
that perspective in mind. In this tutorial, we'll be creating a Chef recipe for the
popular database [Redis](http://redis.io/ "Redis").

Before we get started, there are two terms we need to define, cookbooks and recipes.
In Chef, a cookbook is a collection of related recipes. For example, the MySQL
cookbook might include two recipes, `mysql::client` and `mysql::server`.
Our Redis cookbook will contain just one recipe.

## Getting Set Up

The first thing you'll want to do is:

    $ git clone https://github.com/opscode/chef-repo.git

This gives us the skeleton of our cookbook repository. Next, we'll create an empty
cookbook:

    $ cd chef-repo 
    $ rake new_cookbook COOKBOOK=redis

Our `rake` task created some folders we won't need for this simple recipe, we'll remove them:

    $ cd cookbooks/redis/
    $ rm -rf definitions/ files/ libraries/ providers/ resources/
    $ cd ../..

The folders we'll be looking at are:

    cookbooks/redis
    cookbooks/redis/attributes
    cookbooks/redis/templates/default
    cookbooks/redis/recipes

Next we'll create the files we'll be editing to create our recipe:

    $ touch cookbooks/redis/attributes/default.rb
    $ touch cookbooks/redis/recipes/default.rb
    $ touch cookbooks/redis/templates/default/redis.conf.erb
    $ touch cookbooks/redis/templates/default/redis.upstart.conf.erb

To run and test our cookbook, we'll be using [Vagrant](http://vagrantup.com/),
a tool for managing local virtual machines.
Instructions for installing Vagrant can be found
[here](http://vagrantup.com/docs/getting-started/index.html "installation").
Create and edit `Vagrantfile` to look like this:

{% highlight ruby %}
Vagrant::Config.run do |config|
  config.vm.box = "lucid32"
   config.vm.provision :chef_solo do |chef|
     chef.cookbooks_path = "cookbooks"
     chef.add_recipe "redis"
     chef.log_level = :debug
  end 
end
{% endhighlight %}

The two most important things to note here are that we're telling our
VM to use Chef to install Redis, and that we want the log level set
to debug.

Now run this to download the Ubuntu 10.04 VM we'll be using:

    $ vagrant box add lucid32 http://files.vagrantup.com/lucid32.box

## Writing Our Recipe

Now we are set up and ready to start writing our first recipe.
We'll start by looking at `cookbooks/redis/metadata.rb`. It records
metadata about our cookbook, including other cookbooks it depends
on, and supported OS's. In our case, we don't need to edit it.

### Attributes

Next we'll look at `cookbooks/redis/attributes/default.rb`,
which is where we'll be defining the variable options for installing
and running Redis. Edit it to look like:

{% highlight ruby %}
default[:redis][:dir]       = "/etc/redis"
default[:redis][:data_dir]  = "/var/lib/redis"
default[:redis][:log_dir]   = "/var/log/redis"
# one of: debug, verbose, notice, warning
default[:redis][:loglevel]  = "notice"
default[:redis][:user]      = "redis"
default[:redis][:port]      = 6379
default[:redis][:bind]      = "127.0.0.1"
{% endhighlight %}

This file gives default values for configuration options that
can be overridden by specific nodes. Since it's just Ruby code,
we can also use control statements to change these defaults
based on things like the host OS. One of the most powerful
parts of Chef is that the attributes we're defining here
will be available to all of our configuration file templates.
This means we only have to declare the `user` variable once,
and it will be used to create a new user, and start Redis running
as that same user. We're programming our config files.

A quick note for the non-Ruby programmers out there, when you see
`:redis`, this is called a symbol. The short story is that it's
a string just like `"redis"`, but is more memory efficient if
used more than once. In Python, one of the above lines might
look like:

    default["redis"]["dir"] = "/etc/redis"

### Templates

In Chef we use [ERB](http://en.wikipedia.org/wiki/ERuby)
templates to write our config files.
In this recipe we're using two templates, one for the configuration to
`redis-server` and the other for `upstart`.
[Upstart](http://upstart.ubuntu.com/) is a replacement for
`etc/init.d/` scripts.
Edit `cookbooks/redis/templates/default/redis.conf.erb` to look like:

{% highlight erb %}
port <%= node[:redis][:port] %>
bind <%= node[:redis][:bind] %>
loglevel <%= node[:redis][:loglevel] %>
dir <%= node[:redis][:data_dir] %>

daemonize no
logfile stdout
databases 16
save 900 1
save 300 10
save 60 10000
rdbcompression yes
dbfilename dump.rdb
{% endhighlight %}

and `cookbooks/redis/templates/default/redis.upstart.conf.erb` like:

{% highlight erb %}
#!upstart
description "Redis Server"

env USER=<%= node[:redis][:user] %>

start on startup
stop on shutdown

respawn

exec sudo -u $USER sh -c "/usr/local/bin/redis-server \
  /etc/redis/redis.conf 2>&1 >> \
  <%= node[:redis][:log_dir] %>/redis.log"
{% endhighlight %}


### The Recipe

Now it's time to write the actual recipe, edit `cookbooks/redis/recipes/default.rb`
to look like:
{% highlight ruby %}
package "build-essential" do
  action :install
end

user node[:redis][:user] do
  action :create
  system true
  shell "/bin/false"
end

directory node[:redis][:data_dir] do
  owner "redis"
  mode "0755"
  action :create
end

directory node[:redis][:dir] do
  owner "root"
  mode "0755"
  action :create
end

remote_file "#{Chef::Config[:file_cache_path]}/redis.tar.gz" do
  source "https://github.com/antirez/redis/tarball/v2.0.4-stable"
  action :create_if_missing
end

bash "compile_redis_source" do
  cwd Chef::Config[:file_cache_path]
  code <<-EOH
    tar zxf redis.tar.gz
    cd antirez-redis-55479a7
    make && make install
  EOH
  creates "/usr/local/bin/redis-server"
end

service "redis" do
  provider Chef::Provider::Service::Upstart
  subscribes :restart, resources(:bash => "compile_redis_source")
  supports :restart => true, :start => true, :stop => true
end

template "redis.conf" do
  path "#{node[:redis][:dir]}/redis.conf"
  source "redis.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  notifies :restart, resources(:service => "redis")
end

template "redis.upstart.conf" do
  path "/etc/init/redis.conf"
  source "redis.upstart.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  notifies :restart, resources(:service => "redis")
end

directory node[:redis][:log_dir] do
  mode 0755
  owner node[:redis][:user]
  action :create
end


service "redis" do
  action [:enable, :start]
end
{% endhighlight %}

## Testing Our Recipe
