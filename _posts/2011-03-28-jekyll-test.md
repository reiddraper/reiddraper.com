---
layout: post
title: Jekyll test
---

# Here is an h1 heading

And now some regular text is coming in and it
will span multiple lines and all that stuff.
And it just keeps going and going and going and now
here is even more. Wow, does this post seem to just
keep going on forever?


## Now for a second section

Some thoughts on this code right here...

{% highlight ruby %}
def hello()
  puts "hello"
end

{% endhighlight %}

## A Third section might be nice



and now for some line numbers

{% highlight ruby linenos %}
Vagrant::Config.run do |config|
  # Use the Opscode box that we downloaded.
  config.vm.box = "chef-ubuntu"

  # Let's give the box an IP that is accessible by the other nodes we
  # will spin up.
  config.vm.network "33.33.33.11"

  # Forward the Riak HTTP and Protobufs ports so we can access them
  # locally.
  config.vm.forward_port("riak http", 8098, 8091)
  config.vm.forward_port("riak pbc", 8087, 8081)

  # The default box has a really tiny memory size, which might prevent
  # Riak from starting. Let's give it 1GB to start. If you have less
  # RAM, give it 512MB.
  config.vm.customize do |vm|
    vm.memory_size = 1024
  end

  # We're using Opscode Platform, so the provisioning method is chef_server.
  config.vm.provision :chef_server do |chef|
    # Substitute "ORGNAME" at the end of the URL with your organization.
    chef.chef_server_url = "https://api.opscode.com/organizations/ORGNAME"

    # This is the validation key you should be able to download from
    # the Opscode Platform. Again, substitute "ORGNAME" with your
    # organization name.
    chef.validation_key_path = "../.chef/ORGNAME-validator.pem"

    # Again, substitute "ORGNAME" with your organization name.
    chef.validation_client_name = "ORGNAME-validator"

    # Using /etc/chef will allow you to run chef-client without
    # specifying the path to the config.
    chef.provisioning_path = "/etc/chef"

    # Let's set the node name for Chef so we can keep our local nodes
    # separate from one another.
    chef.node_name = "riak-fast-track-1"

    # Here's where we set the role that we created earlier so the node
    # will automatically install and start Riak.
    chef.add_role "riak-vagrant"

    # We need to set the Riak node name to use the host-only IP
    # address we set above.
    chef.json.merge!(:riak => {:erlang => {:node_name => "riak@33.33.33.11"}})
  end
end

{% endhighlight %}

And now we get to the end of the page
