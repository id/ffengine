# -*- mode: ruby -*-
# -*- coding: utf-8 -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty64"
  config.vm.network "forwarded_port", guest: 5432, host: 5432
  config.ssh.forward_agent = true

  config.vm.provider "virtualbox" do |v|
    v.gui = false
    v.cpus = 4
    v.memory = 1024
  end

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "playbooks/all.yml"
    ansible.verbose = 'vv'
    ansible.raw_arguments = ENV['ANSIBLE_ARGS']
  end
end
