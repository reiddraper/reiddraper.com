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
