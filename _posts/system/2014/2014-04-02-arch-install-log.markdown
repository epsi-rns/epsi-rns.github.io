---
layout: post
title:  "Arch Linux - Install/ Post Install Log"
date      : 2014-04-02 08:46:15 +0700
categories: system
tags      : [arch, distro, package manager]
keywords  : [cli, install, post install]
author: epsi

excerpt:
  Many beginners asked for guidance to install Arch. 
  So I put this two logs.
  There is actualy two steps.
  First step is Install Vanilla Arch itself. Until you get your first login after boot.
  Then second step is Post Install. e.g. setup Wireless, or setup Desktop Environment.

related_link_ids: 
  - 14010246  # Debian Install
  - 14050934  # My Mageia Experiment
  - 14031331  # Linux Multiboot
  - 14031332  # Wireless in Command Line
  - 16020803  # Update Arch no Bloated
  - 14122608  # Unbundling AUR
  - 14042750  # BlackArch as Repository
  - 14122758  # Selectively BlackArch Tools"

---

Arch philosophy is about **knowing your system**. It doesn't give fancy installer.
In fact you have to do it all manually with command line without installer.
Before you step in, you have to be brave to walk with command line.
And be sure you have read the manual.

Reading

* [Arch Wiki: Beginners's Guide: Installation][link-archwiki]

Video

* <http://www.youtube.com/watch?v=BMgGUBDxCjo>

It is worth the risk. Your few days of installing Arch
give you more knowledge than a year with Kali.
After you get the knowledge for a while,
you can install it again in just a few minutes.

Many beginners asking for guidance to install Arch. 
There is actualy two steps.
First step is Install Vanilla Arch itself. Until you get your first root login after boot.
The second step is Post Install. e.g. setup Wireless, or setup WM/DE (Desktop Environment).

If you feel that you are not ready for Arch,
you can still improve your skill with other distro.

* [Learning Stages][local-learning-stages]

<br/>
This article contain two parts.

* Install Log

* Post Install Log

![Arch Install Wifi Menu][image-ss-arch-install]{: .img-responsive }
<br/><br/>

-- -- -- 

## Arch :: Install Log

Prepare Partition<br/>

For more details, about partitioning guidance you can read.

* [Multiboot][local-multiboot]

I'm using <code class="code-file">/dev/sda9</code>. Im sure that you might use diferent partition.

{% highlight bash %}
># cfdisk
># mkswap /dev/sda9
># mkfs.ext4 /dev/sda10
># swapon /dev/sda9
># mount /dev/sda10 /mnt
{% endhighlight %}

Wireless Connexion

{% highlight bash %}
># iw dev
># # ip link set wlp0s3f3u2 up
># # iw dev wlp0s3f3u2 myessid
># wifi-menu
># ping google.com
{% endhighlight %}

Base Install

{% highlight bash %}
># pacstrap -i /mnt base base-devel
># genfstab -p /mnt >> /mnt/etc/fstab
># arch-chroot /mnt
{% endhighlight %}

Basic Setting

{% highlight bash %}
># nano /etc/locale.gen
># locale-gen
># echo LANG=en_US.UTF-8 > /etc/locale.conf
># export LANG=en_US.UTF-8

># ln -s /usr/share/zoneinfo/Asia/Jakarta /etc/localtime
># hwclock --systohc --utc
{% endhighlight %}

Prepare system

{% highlight bash %}
># mkinitcpio -p linux
># passwd
{% endhighlight %}

Prepare pacman

{% highlight bash %}
># systemctl enable dhcpd.service
># ip link
># nano /etc/pacman.conf
{% endhighlight %}

Setup Grub

{% highlight bash %}
># pacman -Syy
># pacman -S grub-bios
># pacman -S os-prober
># grub-install /dev/sda
># grub-mkconfig -o /boot/grub.cfg
{% endhighlight %}

Prepare user

{% highlight bash %}
># useradd -m -g users -G wheel -s /bin/bash epsi
># passwd epsi
># pacman -S sudo
># nano /etc/sudoers
{% endhighlight %}

Additional Wireless Package

{% highlight bash %}
># pacman -S iw rfkill
{% endhighlight %}

Done

{% highlight bash %}
># exit
># cd ..
># umount /dev/sda10
># reboot
{% endhighlight %}


Reading

* <https://wiki.archlinux.org/index.php/Wireless_network_configuration#Installing_driver.2Ffirmware>

-- -- --

## Arch :: First Boot/ Post Install

Wireless Connexion

You can find a more comprehensive topic about [wireless command line here][local-wireless]

{% highlight bash %}
># iw dev
># # ip link set wlp0s3f3u2 up
># iw dev wlp0s3f3u2 connect myessid
># iw dev wlp0s3f3u2 link
># ping google.com
{% endhighlight %}

Desktop Environment

{% highlight bash %}
># pacman -S xorg-server xorg-xinit xorg-twm slim
># pacman -S xf86-video-sisimedia
># pacman -S gdm gnome-shell gnome
># systemctl enable gdm.service

># pacman -S gnome-keyring

{% endhighlight %}

Optional

{% highlight bash %}
?# pacman -S mc fish 
{% endhighlight %}

Additional Wireless Package

{% highlight bash %}
># pacman -S dialog wpa_supplicant wireless_tools 
># pacman -S networkmanager network-manager-applet
># systemctl enable NetworkManager.service
{% endhighlight %}

Command for Wireless Checking

{% highlight bash %}
 $ lspci | grep -i net
 $ lsusb 
 $ ip link
{% endhighlight %}

Avoid Conflict

{% highlight bash %}
># systemctl disable dhcpcd.service
># systemctl disable dhcpcd@.service
># systemctl stop dhcpcd.service
># systemctl stop dhcpcd@.service
{% endhighlight %}

-- -- -- 

Reading

* <https://bbs.archlinux.org/viewtopic.php?pid=1385771>

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-arch-install]: {{ site.url }}/assets/posts/system/2014/04/arch-wifi-menu.png

[local-learning-stages]: {{ site.url }}/opensource/2016/03/17/learning-stages.html
[local-multiboot]: {{ site.url }}/opensource/2014/03/13/linux-multiboot.html
[local-wireless]: {{ site.url }}/opensource/2014/03/13/wireless-command-line.html

[link-archwiki]: https://wiki.archlinux.org/index.php/Beginners%27_guide/Installation
