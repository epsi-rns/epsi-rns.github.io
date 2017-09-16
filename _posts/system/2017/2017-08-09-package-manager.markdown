---
layout: post
title: "Package Manager - Diversity in Linux"
date: 2017-08-09 09:45:15 +0700
categories: system
tags: [distro, package manager]
author: epsi

excerpt:
  Package Management Diversity in Linux,
  Repository, Cache and nice Screenshot.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration
  - 17080645  # Mageia 6 Upgrade
  - 17080545  # GRUB2 BTRFS Support
  - 17080345  # openSUSE Tumbleweed Install
  - 17080245  # Fedora 23 to 26 Install
  - 17080145  # Manjaro OpenRC Issues

---

{% include post/2017/08/topics-docker.html %}

-- -- --

### Preface

People see Diversity in Linux, as Distribution.
Beginner can see how a system can use many Desktop Environment.
And coder can see that they can rewrite from one scripting language to another.
This is true, but inaccurate.
There are other frontier, that is actually fundamental,
from the perspective of Operating System.

I do think that we need more focus on other main diversity as well such as

*	Package Manager

*	Init

*	File system

This will encourage beginner to go deeper,
than just mainstream blogger topic,
such as apt, systemd-based, and ext4-only. 
Enable broader range of discussion. 
Gain more diverse knowledge. 
Thus empower our beloved community.

	But I could be wrong about that.

As a result, beginner should narrow how to choose their OS,
from choosing distro to choosing package manager.

An enthusiast user should experience
package management diversity in linux. 
And try each.
Otherwise, most beginner would stuck,
in the mainstream APT-based distribution,
as you can see the fact in distrowatch top list.

	Do not forget to read the manual first.

-- -- --

### Testing Tools

The issue with package management learning is,
you have to live with it.

The downside of learning package manager is,
the need of good internet connection.

There are a few methods.

*	Multiboot.
	But install so distribution many would be exhausted.

*	Virtualization such as Virtual Box.
	I only have dual core.

*	Docker or LXC.
	I actually use docker to learn other Portage and Slackpkg.
	This is suitable for my resource limitation.
	I do not use LXC anymore.

*	Live OS (not recommended).
	You cannot just use flash disk, trying some command line,
	and pretend you understand package manager.

#### Screenshot: Emerge in LXC

OS: Artix Linux (formerly Manjaro-OpenRC)

*	Window Manager: HerbstluftWM

*	Panel: Lemonbar

*	Wallpaper: Original Wallpaper

Container: LXC

*	Client: Gentoo

*	Running: emerge

[![Artix LXC: Gentoo Emerge][image-ss-artix-lxc]{: .img-responsive }][photo-ss-artix-lxc]

	I wish you luck

-- -- --

### Genre

This is some mainstream Package Management in Linux.

APT: Advanced Package Tool

*	<https://en.wikipedia.org/wiki/APT_(Debian)>

ALPM: Arch Linux Package Management

*	<https://wiki.archlinux.org/index.php/pacman>

*	<https://wiki.archlinux.org/index.php/Pacman/Rosetta>

RPM: RPM Package Manager

*	<https://en.wikipedia.org/wiki/Rpm_(software)>

Portage:

*	<https://wiki.gentoo.org/wiki/Portage>

XBPS: X Binary Package System

*	<https://wiki.voidlinux.eu/XBPS>

*	<https://wiki.voidlinux.eu/Rosetta_stone>

As for enthusiast user I give each source code.
I am sure you all love manual.
Why don't we go further to read the source code ?

-- -- --

#### APT Frontend

APT: Python

*	<https://github.com/Debian/apt>
	
	Package Cache
	
	*	/var/cache/apt/archives/


DPKG: C: Eight Years Ago

*	<https://github.com/davidben/dpkg>

apt-src

*	No github yet

apt-get

*	No github yet

aptitude

*	No github yet

-- -- --

#### ALPM Frontend, ASP and AUR Helper"

ALPM = "Arch Linux Package Management"

Note that ABS tools is deprecated.

*	<https://wiki.archlinux.org/index.php/Arch_Build_System>

Pacman: C

*	<https://github.com/andrewgregory/pacman/>
	
	Package Cache
	
	*	/var/cache/abs/local/yaourtbuild/*-x86_64.pkg.tar.xz
	
	*	/var/cache/pacman/pkg/*-x86_64.pkg.tar.xz
	
ASP: BASH

*	<https://github.com/falconindy/asp>

Cower: C

*	<https://github.com/falconindy/cower>

Package Query: C

*	<https://github.com/archlinuxfr/package-query>

AURA: Haskell

*	<https://github.com/aurapm/aura>

Pacaur: BASH/C

*	<https://github.com/rmarquis/pacaur>

Packer: BASH

*	<https://github.com/keenerd/packer>

Yaourt: BASH/C

*	<https://github.com/archlinuxfr/yaourt>
	
	Package Cache
	
	*	/tmp/yaourt-tmp-epsi/
	
	Package Cache (deprecated abs)
	
	*	/var/cache/abs/local/yaourtbuild/*-x86_64.pkg.tar.xz

[![Arch: NCDU][image-ss-arch-ncdu]{: .img-responsive }][photo-ss-arch-ncdu]

-- -- -- 

#### RPM Frontend

**Format**

RPM: C

*	<https://github.com/rpm-software-management/rpm>

*	<https://en.wikipedia.org/wiki/Rpm_(software)>

RPM is special in a way that RPM has many front end implementation.

**Frontend**

URPMI: Mageia: Perl

*	<https://github.com/shlomif/urpmi>

*	<https://en.wikipedia.org/wiki/Urpmi>
	
	Package Cache
	
	*	/var/cache/urmi/rpms/*.rpm

DNF (Dandified YUM): Fedora: Python

*	<https://github.com/rpm-software-management/dnf>

*	<https://en.wikipedia.org/wiki/DNF_(software)>
	
	Package Cache
	
	*	/var/cache/dnf/*/packages/*.rpm

YUM: Python

*	<https://github.com/rpm-software-management/yum>

*	<https://en.wikipedia.org/wiki/Yum_(.rpm)>

#### ZYpp Frontend

Zypper: openSUSE

*	<https://github.com/openSUSE/libzypp>

*	https://github.com/openSUSE/zypper>

*	<https://en.wikipedia.org/wiki/ZYpp>
	
	Package Cache
	
	*	/var/cache/zypp/packages/*/x86_64/*.x86_64.rpm
	
	*	/var/cache/zypp/packages/*/suse/noarch/*.noarch.rpm

-- -- --

#### Portage Frontend

Emerge

*	<https://github.com/gentoo/portage>
	
	Portage Library
	
	*	/usr/lib/portage
	
![Gentoo: NCDU][image-ss-gentoo-ncdu]{: .img-responsive }

-- -- --

#### SlackPkg

: Slackware: C

*	Package Cache (slackpkg)
	
	*	/var/cache/packages/*/*/.txz
	
slapt-get

*	<https://github.com/jaos/slapt-get>

slpkg

*	<https://github.com/dslackw/slpkg>

-- -- --

#### XBPS

XBPS: C

*	<https://github.com/voidlinux/xbps>

-- -- --

#### Crux Ports

Pkgutils: Crux: C

*	<http://www.fukt.bsnet.se/~per/pkgutils/>

*	<https://github.com/nipuL/pkgutils>

Frontend

*	<https://github.com/winkj/prt-get>


-- -- --

#### Other Frontend

Sol (eopkg replacement in Solus): C

*	<https://github.com/solus-project/sol>

-- -- --

#### Screenshot: Package Manager in LXC

OS: Manjaro-OpenRC

*	Window Manager: Awesome (floating mode)

*	Running: pacman, emerge, yum, apt

[![Manjaro LXC][image-ss-lxc-package]{: .img-responsive }][photo-ss-lxc-package]

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/package-manager' %}

[image-ss-arch-ncdu]:      {{ asset_post }}/arch-ncdu.png
[photo-ss-arch-ncdu]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNJWgfTnwYdOcMq56Eg8fMhEsiM4Ki8ZaHLE1vv?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-gentoo-ncdu]:    {{ asset_post }}/gentoo-ncdu.png

[image-ss-artix-lxc]:      {{ asset_path }}/artix-lxc-gentoo-emerge-webrsync.png
[photo-ss-artix-lxc]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOrtBFrxcZLd6gB7QwIkR6hLpcBJI9PdwIR3-0J?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-lxc-package]:    {{ site.url }}/assets/posts/system/2016/02/lxc-screenfetch.png
[photo-ss-lxc-package]:    https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOIV-PL-MJDcC2TxUi8xKUBFNz419Cnrxi9FBWy

[image-ss-arch-query-aur]: {{ asset_post }}/arch-hlwm-aur-no-rofi.png
[photo-ss-arch-query-aur]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNE2B9cj-nGL6eUIr08vN3MLe1h78NVPglm6LKW?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
