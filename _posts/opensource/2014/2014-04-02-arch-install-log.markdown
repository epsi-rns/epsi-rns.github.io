---
layout: post
title:  "Arch - Install/ Post Install Log"
date:   2014-04-02 08:46:15 +0700
categories: opensource
tags: [arch, pacman, package manager]
author: epsi
excerpt:
  Many beginners asked for guidance to install Arch. 
  So I put this two logs.
  There is actualy two steps.
  First step is Install Vanilla Arch itself. Until you get your first login after boot.
  Then second step is Post Install. e.g. setup Wireless, or setup esktop Environment.
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

![Arch Install][image-ss-arch-install]
<br/><br/>

-- -- -- 

## Arch :: Install Log

Prepare Partition<br/>

For more details, about partitioning guidance you can read.

* [Multiboot][local-multiboot]

I'm using /dev/sda9. Im sure that you might use diferent partition.

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

-- -- -- -- -- -- -- --
Reading

* <https://bbs.archlinux.org/viewtopic.php?pid=1385771>

-- -- --

**Related Links**

* [Debian Wheezy - Post Install Log][related-debian-install]

* [My Mageia Experiment][related-mageia-experiment]

* [Linux Multiboot][related-linux-multiboot]

* [Wireless in Command Line][related-wireless-cli]

* [Update Arch Linux without Unnecessary Bloated Package][related-arch-no-bloated]

* [Unbundling AUR Helper Process][related-unbundling-aur]

* [Install BlackArch as Repository][related-blackarch-repository]

* [Selectively Install BlackArch Tools][related-blackarch-selectively]


[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-arch-install]: {{ site.url }}/assets/posts/opensource/2014/04/arch-wifi-menu.png

[local-learning-stages]: {{ site.url }}/opensource/2016/03/17/learning-stages.html
[local-multiboot]: {{ site.url }}/opensource/2014/03/13/linux-multiboot.html
[local-wireless]: {{ site.url }}/opensource/2014/03/13/wireless-command-line.html

[link-archwiki]: https://wiki.archlinux.org/index.php/Beginners%27_guide/Installation

[related-debian-install]: {{ site.url }}/opensource/2014/01/02/debian-post-install-log.html
[related-mageia-experiment]: {{ site.url }}/opensource/2014/05/09/mageia-experiment.html
[related-wireless-cli]: {{ site.url }}/opensource/2014/03/13/wireless-command-line.html 
[related-linux-multiboot]: {{ site.url }}/opensource/2014/03/13/linux-multiboot.html

[related-arch-no-bloated]: {{ site.url }}/opensource/2016/02/08/pacman-ignorepkg.html
[related-unbundling-aur]: {{ site.url }}/opensource/2014/12/26/unbundling-aur-helper-process.html
[related-blackarch-repository]: {{ site.url }}/opensource/2014/04/27/install-blackarch-as-repository.html
[related-blackarch-selectively]: {{ site.url }}/opensource/2014/12/27/selectively-install-blackarch-tools.html
