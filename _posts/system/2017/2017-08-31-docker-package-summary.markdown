---
layout: post
title: "Docker - Package Management Summary"
date      : 2017-08-31 09:45:15 +0700
categories: system
tags      : [docker, distro, package manager]
keywords  : [overview, summary]
author: epsi

opengraph:
  image: /assets/site/images/topics/docker.png

excerpt:
  Docker flow for learning linux distribution,
  whether package management, or init.

related_link_ids: 
# - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian Portage
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports
# some old stuffs
  - 16022800  # Learn LXC
  - 16030301  # Docker Manjaro
  - 16030900  # Docker Debian 

---

{% include post/2017/08/topics-docker.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

	Heaven does exist !

I have been dreaming of having my own lab,
examining variety of package manager, using my own notebook.
Five years has past, yet still broken dreams I have. 
I cannot afford high end machine with virtual machine performance.
Like a **dream come true**, docker allow me to install,
other distribution without CPU burden.
I finally fulfill my destiny.

I can use Docker to learn package management such as,
Crux's Ports, Gentoo's Portage, Slackware's Package, Void Linux's XBPS,
Fedora's DNF, openSUSE's Zypper, Debian's APT, and LFS's Build.
Of course they are all interesting.

#### Diversity

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

#### Testing Tools

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
	As an alternative, there is this LXC.
	I do not use LXC anymore.

*	Live OS (not recommended).
	You cannot just use flash disk, trying some command line,
	and pretend you understand package manager.
	
#### Persistence Docker Process

Anyone new with Docker should differ between LXC and Docker.
With LXC, the state changed as we exit each image.
With Docker, each time we run image, docker spawn a new process.

	We need a persistence state that our distro
	keep the change we made after exiting the session.

This is why we should beware of the flow.
Docker is different with LXC.
Docker has image, and process.
Each image an be spawn into several container process.

	I wish you luck
	
-- -- --

### Package Manager
		
#### Genre

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


As for enthusiast user I give link for each source code.
I am sure you all love manual.

		Do not forget to read the manual first.

Why don't we go further to read the source code ?

#### Approximate Timeline for Linux Package Manager Frontend

Data taken from NEWS, wikipedia, Github First Commit, and Change Log.
This may not be accurate.

![Approximate Timeline for Linux Package Manager Frontend][image-ss-timeline]{: .img-responsive }

*	maybe 1993: pkgtools/slackpkg

*	1994: dpkg

*	1997: rpm

*	1998: apt-get

*	1999: aptitude

*	2000: portage/emerge

*	2000: ports/prt-get

*	2002: pacman/makepkg

*	2003: yum

*	2003: slapt-get

*	2006: zypper

*	2008: yaourt

*	2010: xbps

*	2010: package-query

*	2010: cower

*	2010: packer

*	2011: pacaur

*	2012: aura

*	2013: apt

*	2014: slpkg

*	2014: asp

*	2015: dnf

*	2016: eopkg

-- -- --

### Flow for Learning Linux Distribution

Since we want a persistence distro,
we only need one process.
So here the step.

#### (1) Terminal 1: List all images.

If this is empty, you can pull for docker hub.

{% highlight bash %}
$ sudo watch docker image list
{% endhighlight %}

![Terminal 1][image-ss-term-1]{: .img-responsive }

#### (2) Terminal 2: List all processes.

This list could be emptyor has several processess.
You can delete all unnecessary processes,
and start with a clean process.

Note that you can show based on docker id, or docker names.

{% highlight bash %}
$ sudo watch docker  ps -as
{% endhighlight %}

or

{% highlight bash %}
{% raw %}
$ sudo docker ps -a --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
{% endhighlight %}

or

{% highlight bash %}
{% raw %}
$ sudo watch "docker ps -a --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}\t{{.Size}}'"
{% endraw %}
{% endhighlight %}

![Terminal 2][image-ss-term-2]{: .img-responsive }

You can add the size, so you can monitor how the process grow
as you work with your container.
But size take a longer calculation as a drawback.

#### (3) Terminal 3: Run an image to create process

Do use interactive.

{% highlight bash %}
$ sudo docker run -it vbatts/slackware
{% endhighlight %}

And exit from process. You can start later.

{% highlight bash %}
sh-4.3# exit
{% endhighlight %}

![Terminal 3][image-ss-term-3]{: .img-responsive }

A new process will be shown in process list
such as _inspiring_davinci_. 
Docker give random name automatically.


#### (4) Terminal 4: Attach process to your favorite terminal

{% highlight bash %}
$ sudo docker start inspiring_davinci
{% endhighlight %}

{% highlight bash %}
$ sudo docker attach inspiring_davinci
{% endhighlight %}

![Terminal 1][image-ss-term-4]{: .img-responsive }

Do not forget to press any character.
The docker wait for your response.

After you exit, you need to start the process again.

#### (5) Open New Terminal

{% highlight bash %}
sudo docker exec -it inspiring_davinci bash
{% endhighlight %}

#### Tips: Without sudo

To avoid typing sudo all the time,
add user in <code>/etc/group<code>.

{% highlight conf %}
docker:x:1000:epsi
{% endhighlight %}

Do not forget to logout, and relogin.

#### Tips: Alias

For convenience you may put aliases to <code>~/.bashrc</code>.

{% highlight bash %}
{% raw %}
alias docker-ps="docker ps -a --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'"
alias docker-ls="docker image list --format 'table {{.Repository}}\t{{.Size}}'"
{% endraw %}
{% endhighlight %}

![Alias][image-ss-alias]{: .img-responsive }

-- -- --

### Screenshot:

*	OS: Artix Linux

*	WM: Herbstluftwm

[![Docker All Screenshot][image-ss-term-all]{: .img-responsive }][photo-ss-term-all]: 

#### Related Screenshot

*	[Learn Different Package Management Using LXC][local-manjaro-lxc]

*	[Docker Demonstration (Manjaro), Running Multiple Guest OS][local-manjaro-docker]

*	[Running Guest OS via Docker on top of Debian for the first time][local-debian-docker]

#### Alternative: Artix LXC

OS: Artix Linux (formerly Manjaro-OpenRC)

*	Window Manager: HerbstluftWM

*	Panel: Lemonbar

*	Wallpaper: Original Wallpaper

Container: LXC

*	Client: Gentoo

*	Running: emerge

[![Artix LXC: Gentoo Emerge][image-ss-artix-lxc]{: .img-responsive }][photo-ss-artix-lxc]

#### Alternative: Manjaro LXC

OS: Manjaro-OpenRC

*	Window Manager: Awesome (floating mode)

*	Running: pacman, emerge, yum, apt

[![Manjaro LXC][image-ss-lxc-package]{: .img-responsive }][photo-ss-lxc-package]

-- -- --

### Docker Collection

These are some docker containers, that I utilize in this blog.
For each I provide the command.
In addition the default <code>latest</code> tag,
you can use other tag such as <code>tumbleweed</code>, 
<code>rawhide</code>, <code>stretch</code>.

There are **[ links ]** for each docker topic,
each article has deeper package management discussion.

#### SlackPkg

**Article**: [[ Slackware - Package ]][local-slackware]

{% highlight conf %}
$ docker pull vbatts/slackware
$ docker run -it vbatts/slackware
{% endhighlight %}

: Slackware: C

*	Package Cache (slackpkg)
	
slapt-get

*	<https://github.com/jaos/slapt-get>

slpkg

*	<https://github.com/dslackw/slpkg>

#### Portage Frontend

**Article**: [[ Gentoo - Portage ]][local-gentoo]

{% highlight conf %}
$ docker pull gentoo/stage3-amd64
$ docker run -it gentoo/stage3-amd64
{% endhighlight %}

Emerge

*	<https://github.com/gentoo/portage>
	
	Portage Library
	
	*	/usr/lib/portage
	
![Gentoo: NCDU][image-ss-gentoo-ncdu]{: .img-responsive }

#### XBPS

**Article**: [[ Void - XBPS ]][local-void]

{% highlight conf %}
$ docker pull voidlinux/voidlinux
$ docker run -it voidlinux/voidlinux bash
{% endhighlight %}

XBPS: C

*	<https://github.com/voidlinux/xbps>

#### Crux Ports

**Article**: [[ Crux - Ports ]][local-crux]

{% highlight conf %}
$ docker pull crux
$ docker run -it crux
{% endhighlight %}

Pkgutils: Crux: C

*	<http://www.fukt.bsnet.se/~per/pkgutils/>

*	<https://github.com/nipuL/pkgutils>

Frontend

*	<https://github.com/winkj/prt-get>

![Docker Pull Crux Linux][image-ss-pull-crux]{: .img-responsive }

#### ALPM Frontend, ASP and AUR Helper"

**Article**: [[ Arch - ALPM ]][local-arch]

{% highlight conf %}
$ docker pull dock0/arch
$ docker run -it dock0/arch
{% endhighlight %}

![Docker Pull Arch Linux][image-ss-pull-arch]{: .img-responsive }

ALPM = "Arch Linux Package Management"

Note that ABS tools is deprecated.

*	<https://wiki.archlinux.org/index.php/Arch_Build_System>

Pacman: C

*	<https://github.com/andrewgregory/pacman/>
		
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

[![Arch: NCDU][image-ss-arch-ncdu]{: .img-responsive }][photo-ss-arch-ncdu]


#### RPM Frontend

	I do not cover RPM here.

**Format**

RPM: C

*	<https://github.com/rpm-software-management/rpm>

*	<https://en.wikipedia.org/wiki/Rpm_(software)>

RPM is special in a way that RPM has many front end implementation.

**Frontend**

URPMI: Mageia: Perl

*	<https://github.com/shlomif/urpmi>

*	<https://en.wikipedia.org/wiki/Urpmi>
	
YUM: Python

*	<https://github.com/rpm-software-management/yum>

*	<https://en.wikipedia.org/wiki/Yum_(.rpm)>

#### DNF Frontend

**Article**: [[ Fedora - DNF ]][local-fedora]

{% highlight conf %}
$ docker pull fedora:rawhide
$ docker run -it fedora:rawhide bash
{% endhighlight %}

![Docker Pull Fedora Rawhide][image-ss-pull-fedora]{: .img-responsive }

DNF (Dandified YUM): Fedora: Python

*	<https://github.com/rpm-software-management/dnf>

*	<https://en.wikipedia.org/wiki/DNF_(software)>

#### ZYpp Frontend

**Article**: [[ openSUSE - Zypper ]][local-opensuse]

{% highlight conf %}
$ docker pull opensuse/amd64:tumbleweed
$ docker run -it opensuse/amd64:tumbleweed bash
{% endhighlight %}

![Docker Pull openSUSE Tumbleweed][image-ss-pull-opensuse]{: .img-responsive }

Zypper: openSUSE

*	<https://github.com/openSUSE/libzypp>

*	<https://github.com/openSUSE/zypper>

*	<https://en.wikipedia.org/wiki/ZYpp>
	

#### APT Frontend

**Article**: [[ Debian - APT ]][local-debian]

{% highlight conf %}
$ docker pull debian:stretch
$ docker run -it debian:stretch
{% endhighlight %}

![Docker Pull Debian Stretch][image-ss-pull-debian]{: .img-responsive }

APT: Python

*	<https://github.com/Debian/apt>
	
DPKG: C: Eight Years Ago

*	<https://github.com/davidben/dpkg>

apt-src

*	No github yet

apt-get

*	No github yet

aptitude

*	No github yet

#### LFS Build

**Article**: [[ LFS - Build ]][local-lfs]

{% highlight conf %}
$ docker pull kevinleptons/lfs-auto
$ docker run -it voidlinux/voidlinux bash
{% endhighlight %}

#### Miscellanous Docker

{% highlight conf %}
$ docker pull busybox
$ docker run -it busybox
{% endhighlight %}

![Docker Pull BusyBox][image-ss-pull-busybox]{: .img-responsive }

#### Other Frontend

Sol (eopkg replacement in Solus): C

*	<https://github.com/solus-project/sol>

-- -- --

### Thank You

> #respect

My intention is to give appreciation to the community,
because I have many riddled that,
I can't solve without help from the Community.
I hope this people do not mind that I put their name here.

*	Arch Linux: "Dean Wallace": 

*	Fedora: "Станислав Нижниченко:, "s10e G" , "Flavio R. Cavalcanti"

*	openSUSE: "Tim Baker": 

*	Slackware: "Roberto Val", "Flavio R. Cavalcanti"

*	Gentoo: "Darko Luketic"

*	Void Linux: "cintrikz": 

*	Crux Linux: "pitillo"

*	LFS: "Michał Radwański"

Also "Michael Boyle" for EIX advice,
and "Linux Lady05 Winter" for giving support, 
and "Pașca Alexandru" for BTRFS issue for my First openSUSE install.

Thank you so much for being helpful. 
You are all great sweet people.

-- -- --

#### Conclusion

This is just a summary, we have not finished yet.
There are links for each docker topic on top of this page.
Consider reading each topic.

[[ Debian - APT ]][local-debian]
[[ openSUSE - Zypper ]][local-opensuse]
[[ Fedora - DNF ]][local-fedora]
[[ Arch - ALPM ]][local-arch]
[[ Crux - Ports ]][local-crux]
[[ Void - XBPS ]][local-void]
[[ Slackware - Package ]][local-slackware]
[[ Gentoo - Portage ]][local-gentoo]
[[ LFS - Build ]][local-lfs]

	Dream come true

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/assets-system' %}
{% assign asset_flow  = system_path | append: '/2017/08/docker-flow' %}
{% assign asset_pull  = system_path | append: '/2017/08/docker-pull' %}
{% assign asset_pack  = system_path | append: '/2017/08/package-manager' %}

[image-ss-arch-ncdu]:      {{ asset_pack }}/arch-ncdu.png
[photo-ss-arch-ncdu]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNJWgfTnwYdOcMq56Eg8fMhEsiM4Ki8ZaHLE1vv?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-gentoo-ncdu]:    {{ asset_pack }}/gentoo-ncdu.png

[image-ss-artix-lxc]:      {{ asset_pack }}/artix-lxc-gentoo-emerge-webrsync.png
[photo-ss-artix-lxc]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOrtBFrxcZLd6gB7QwIkR6hLpcBJI9PdwIR3-0J?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-lxc-package]:    {{ site.url }}/assets-system/2016/02/lxc-screenfetch.png
[photo-ss-lxc-package]:    https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOIV-PL-MJDcC2TxUi8xKUBFNz419Cnrxi9FBWy

[image-ss-arch-query-aur]: {{ asset_pack }}/arch-hlwm-aur-no-rofi.png
[photo-ss-arch-query-aur]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNE2B9cj-nGL6eUIr08vN3MLe1h78NVPglm6LKW?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-term-1]:	{{ asset_flow }}/terminal-1.png
[image-ss-term-2]:	{{ asset_flow }}/terminal-2.png
[image-ss-term-3]:	{{ asset_flow }}/terminal-3.png
[image-ss-term-4]:	{{ asset_flow }}/terminal-4.png
[image-ss-alias]:	{{ asset_flow }}/docker-alias-bashrc.png

[image-ss-term-all]:	{{ asset_flow }}/all.png
[photo-ss-term-all]:	https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOGBr5RBUPtwBBJw14l1otPZZYR0WIur16lolb-?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[local-manjaro-lxc]:    {{ site.url }}/system/2016/02/28/lxc-demonstration.html
[local-manjaro-docker]: {{ site.url }}/system/2016/03/03/docker-demonstration-manjaro.html
[local-debian-docker]:  {{ site.url }}/system/2016/03/09/docker-demonstration-debian.html

[image-ss-timeline]:	{{ asset_flow }}/timeline-04s.png

[image-ss-pull-arch]:		{{ asset_pull }}/arch.png
[image-ss-pull-crux]:		{{ asset_pull }}/crux.png
[image-ss-pull-debian]:		{{ asset_pull }}/debian-stretch.png
[image-ss-pull-fedora]:		{{ asset_pull }}/fedora-rawhide.png
[image-ss-pull-opensuse]:	{{ asset_pull }}/opensuse-tumbleweed.png
[image-ss-pull-busybox]:	{{ asset_pull }}/busybox.png

[local-crux]:		{{ site.url }}/system/2017/08/10/docker-crux-ports.html
[local-gentoo]:		{{ site.url }}/system/2017/08/11/docker-gentoo-portage.html
[local-void]:		{{ site.url }}/system/2017/08/13/docker-void-xbps.html
[local-opensuse]:	{{ site.url }}/system/2017/08/15/docker-opensuse-zypper.html
[local-fedora]:		{{ site.url }}/system/2017/08/18/docker-fedora-dnf.html
[local-slackware]:	{{ site.url }}/system/2017/08/21/docker-slackware-package.html
[local-debian]:		{{ site.url }}/system/2017/08/24/docker-debian-apt.html
[local-arch]:		{{ site.url }}/system/2017/08/27/docker-arch-alpm.html
[local-lfs]:		{{ site.url }}/system/2017/08/30/docker-lfs-build.html
