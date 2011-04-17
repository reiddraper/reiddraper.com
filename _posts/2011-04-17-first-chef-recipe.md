---
layout: post
title: Writing Your First Chef Recipe
---

The first thing you'll want to do is:

    $ git clone https://github.com/opscode/chef-repo.git
    $ cd chef-repo 
    $ rake new_cookbook COOKBOOK=redis

The folders we'll be looking at are:

    cookbooks/redis
    cookbooks/redis/attributes
    cookbooks/redis/templates/default
    cookbooks/redis/recipes

Our `rake` task created some folders we won't need for this simple recipe, so
to keep things clean, we'll remove them.

    $ cd cookbooks/redis/
    $ rm -rf definitions/ files/ libraries/ providers/ resources/
    $ cd ../..

Next we'll create the files we'll be editing to create our recipe:

    $ touch cookbooks/redis/attributes/default.rb
    $ touch cookbooks/redis/recipes/default.rb
    $ touch cookbooks/redis/templates/default/redis.conf.erb
    $ touch cookbooks/redis/templates/default/redis.upstart.conf.erb

To run and test our cookbook, we'll be using `Vagrant`, so we need to create a
`Vagrantfile`. Edit `Vagrantfile` to look like this:

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

And this to download the VM:

    $ vagrant box add lucid32 http://files.vagrantup.com/lucid32.box

Now we are all set up and ready to start writing our first recipe.
We'll start be editing `cookbooks/redis/attributes/default.rb`,
which is where we'll be defining the variable options for installing
and running redis. Edit it to look like:

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

This file shows blah blah blah

We'll be using two configuration templates, one for the configuration to
`redis-server` and the other for `upstart`.
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


