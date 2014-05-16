# -*- mode: ruby -*-
# vi: set ft=ruby :

NODE_COUNT = 2

Vagrant.configure('2') do |config|
  config.vm.box = "ubuntu1204-solver2"
  config.vm.network "private_network", type: "dhcp"
  config.vm.synced_folder ".", "/home/vagrant/dcbc"

  NODE_COUNT.times do |i|
    node_id = "node#{i}"
    primary = false
    if i == 0
      primary = true
    end
    config.vm.define node_id, primary: primary do |h|
      h.vm.hostname = "#{node_id}"
    end
  end

  config.vm.provider "virtualbox" do |v|
    v.memory = 512
    v.cpus = 1
  end
end
