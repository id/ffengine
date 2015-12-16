# -*- mode: ruby -*-
# -*- coding: utf-8 -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty64"
  config.vm.network "forwarded_port", guest: 5432, host: 5432
  config.ssh.forward_agent = true

  config.vm.provider "virtualbox" do |provider, override|
    provider.gui = false
    provider.cpus = 4
    provider.memory = 1024

    override.vm.provision "ansible" do |ansible|
      ansible.playbook = "playbooks/all.yml"
      ansible.verbose = 'vv'
      ansible.raw_arguments = ENV['ANSIBLE_ARGS']
    end
  end

  config.vm.provider :digital_ocean do |provider, override|
    provider.image = 'ubuntu-14-04-x64'
    provider.region = 'ams2'
    provider.size = '1gb'

    override.ssh.private_key_path = '~/.ssh/id_rsa'
    override.vm.hostname = 'ffengine'
    override.vm.box = 'digital_ocean'
    override.vm.box_url = "https://github.com/smdahlen/vagrant-digitalocean/raw/master/box/digital_ocean.box"

    override.vm.provision "ansible" do |ansible|
      ansible.playbook = "playbooks/all.yml"
      ansible.verbose = 'vv'
      ansible.raw_arguments = ENV['ANSIBLE_ARGS']
    end
  end
end
