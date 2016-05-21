---
layout: post
title:  "Learning Stages, from Beginner to Enthusiast"
date:   2016-03-17 23:00:15 +0700
categories: opensource
tags: [thought]
author: epsi
excerpt: 
  This is a most common issue from n00b in a community group.
  Some even don't know what to ask. Don't know where to start.
  So I make a guidance. 
---

There are stages. From beginner to enthusiast.

Tell us, what you have experienced with!
Tell me if there is more thing to be done!

* * *

<br/>

# 1. Choose Ubuntu/Mint/eOS for first time install,

"*Try everything*"

* Basic Terminal Command

	+ ls, cat, grep, ps, top, man, info, su, sudo

	+ always use '$ man' for documentation before google.

* Also learn about basic linux briefly

	+ FHS, fstab,

	+ [ubuntu-manual][link-ubuntu-manual] (GUI)

	+ [debian-handbook][link-debian-handbook] (hertzog)

* Package Manager

	+ apt-get, aptitude, dpkg, synaptic, sources.list

* Install any application you like.

	+ try only official application,

	+ gimp, blender, inkscape, libreoffice, calligra, gparted

	+ and play with PPA, but consider

	+ ! PPA is not always widely used in linux

* Desktop Environment

	+ Unity, KDE, gnome-shell, xfce4, lxde, razor, e18, cinnamon

	+ and each cusomization, show your sreenshot to group

* Window Manager & Compositor Effects

	+ Compiz, Kwin, Openbox

* Try different init

	+ open-rc, systemd, upstart, sysvinit

* Well I forget what to do, sorrry...

* More command line tools

	+ nmcli, ncdu, ntop, cfdisk, fish
	
	+ lspci, lsusb, lsmod, dmesg, ip ,iw
	
	+ ascii art: screenfetch, archey, figlet, ansi color.

* Scripting Language

	+ bash/awk, perl, python

<br/>
Most importantly, have fun with all

<br/>

* * *

<br/>

Note: Go with one distro that matches your desired path: Casual user, Enthusiast, or Professional.

* Casual user may consider:

	+ Linux Mint (with Cinnamon), eOS (with Pantheon)

* Professional user should consider:

	+ openSUSE, Fedora, CentOS, or Debian.

* Enthusiast:

	+ LFS, Arch, Gentoo, Slackware

<br/>

* * *

<br/>

# 2. Leverage yourself to Arch

"*Know your system*"

* [learn arch install log][local-arch-log].

* [multiboot][local-multiboot] form previous linux

	+ prepare your partition with cfdisk

* Install with command line only

	+ add driver manually

	+ add username manually

	+ add DE manually

	+ use pacman

* read arch wiki

	+ there is a lot of good material here

* use yaourt, packer, cower, or other AUR Helper

	+ automatic compilation

	+ try any unofficial application from AUR

	+ also try: pacmind, yaourt-gui

* Dare to use minimalis WM, rather than eyecandy DE

	+ e.g. openbox, awesome, i3, xmonad

* Penetration Testing Application

	+ add and install repo: BlackArch or maybe ArchAssault

* read more about other system

	+ hurd/redox, xfs/btrfs, pulseaudio, wayland/mir, llvm.

<br/>

* * *

<br/>

I think one month is not enough for both.

[//]: <> ( -- -- -- links below -- -- -- )

[link-ubuntu-manual]: https://ubuntu-manual.org/
[link-debian-handbook]: https://debian-handbook.info/browse/stable/
[local-multiboot]: {{ site.url }}/opensource/2014/03/13/linux-multiboot.html
[local-arch-log]: {{ site.url }}/opensource/2014/04/02/arch-install-log.html
