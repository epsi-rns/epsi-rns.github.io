---
layout: post
title: "Docker - Arch ALPM - Part One"
date: 2017-08-24 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
# - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-arch-alpm.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

We have this ALPM, The **Arch Linux Package Management**,
as a base for <code>pacman</code> binary package frontend,
and source package tools such as the famous <code>AUR</code>,
and <code>ASP</code> (formerly <code>ABS</code>),
or even the lower level <code>makepkg</code> tool.
They are all under ALPM umbrella.

Using Arch Linux minimal install in Docker,
is a good way to learn ALPM.
ALPM is considered a basic knowledge
utilized by Arch's based distribution
such as Manjaro, Antergos, Artix,
and some other cool Arch's influenced distribution such as KaOS.
In short, any distribution using <code>pacman</code>

Personally I have been an Arch user since 2014,
I have used so many pacman command, but never really understand.
Reading pacman manual again make me reaize that I know nothing.
That is why I have a need to write down this blog.

{% include post/2017/08/docker-test-bed.md %}

#### Must Read

You are encouraged to read the holy __archwiki__ first,
before even starting to read this article.

*	<https://wiki.archlinux.org/index.php/pacman>

There are already so many references for this pacman on the internet.
This blog article is just another reference.

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull dock0/arch
{% endhighlight %}

![Docker Pull Debian Strecth][image-ss-pull-debian]{: .img-responsive }

{% highlight bash %}
$ docker image list  
{% raw %}
  --filter "reference=*/arch:*"
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY          SIZE
dock0/arch          870MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it dock0/arch
root@f77ea94688d1:/# exit
exit
{% endhighlight %}

![Docker Arch: Running Docker][image-ss-running-arch]{: .img-responsive }

{% highlight bash %}
$ docker ps 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE               NAMES               STATUS
dock0/arch          musing_pike         Up About a minute
{% endhighlight %}

![Docker Arch: Docker ps][image-ss-docker-ps]{: .img-responsive }

{% highlight bash %}
$ docker start musing_pike
musing_pike
{% endhighlight %}

{% highlight bash %}
$ docker attach musing_pike
[root@37f48ad6a711 /]# 
{% endhighlight %}

![Docker Arch: Getting Started][image-ss-getting-started]{: .img-responsive }

-- -- --

### Package Management

Arch Linux use <code class="code-file">.pkg.tar.xz</code> extension for package.

#### ALPM Frontend, ASP and AUR Helper"

ALPM = "Arch Linux Package Management"

Note that ABS tools is deprecated.

*	<https://wiki.archlinux.org/index.php/Arch_Build_System>

Pacman: C

*	<https://wiki.archlinux.org/index.php/pacman>

*	<https://wiki.archlinux.org/index.php/Pacman/Rosetta>

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

#### Get Help

Read the fine manual. 

This one show nothing.

{% highlight bash %}
$ pacman --help
{% endhighlight %}

And this one show a lot.

{% highlight bash %}
$ man help
{% endhighlight %}

Note that with minimal install in docker,
we do not have <code>man-db</code> and <code>less</code> yet.

-- -- -- 

### Updating System

	First Thing First

First thing to do is updating my system as usual.

#### OS Release

Make sure it is Arch that we are using.

There is <code>/etc/os-release </code> in Manjaro and KaOS,
But Arch utilized an empty file <code>/etc/arch-release</code>
to indicate that this is an arch distribution.
I guess since Arch is rolling release,
there is no need for release number.

{% highlight bash %}
$ cat /etc/arch-release 
{% endhighlight %}

Other Arch based may have different file,
such as <code>/etc/artix-release </code>.

#### The Riddle

What is <code>pacman -Syu</code> ?
And What is <code>pacman -Syyuu</code> anyway ?
Of course there is <code>man pacman</code> that explain it all.
After reading the manual we can have something like 
<code>pacman --sync --refresh --quiet --sysupgrade --force</code>.

We can separate the <code>pacman -Syu</code> into few smaller steps.

*	Update

*	List Upgradable

*	Download

*	Upgrade

*	Common Form

#### Refresh Update

{% highlight bash %}
$ pacman -Sy
:: Synchronizing package databases...
 core                     124.4 KiB   138K/s 00:01 [##########] 100%
 extra                   1651.5 KiB   109K/s 00:15 [##########] 100%
 community                  4.0 MiB  55.4K/s 01:14 [##########] 100%
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --refresh
:: Synchronizing package databases...
 core is up to date
 extra is up to date
 community is up to date
{% endhighlight %}

![Docker Arch: pacman refresh][image-ss-pm-refresh]{: .img-responsive }

From the manual " _Passing two --refresh or -y flags will force a
refresh of all package databases, even if they appear to be up-to-date._ "

{% highlight bash %}
$ pacman -Syy
{% endhighlight %}

#### List Upgradable

{% highlight bash %}
$ pacman -Qu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --upgrades
archlinux-keyring 20170611-1 -> 20170823-1
coreutils 8.27-1 -> 8.28-1
curl 7.55.1-1 -> 7.55.1-2
e2fsprogs 1.43.5-1 -> 1.43.6-1
expat 2.2.3-1 -> 2.2.4-1
gcc-libs 7.1.1-4 -> 7.2.0-2
glibc 2.25-7 -> 2.26-3
gnupg 2.1.23-1 -> 2.2.0-1
gnutls 3.5.14-1 -> 3.5.15-1
iana-etc 20170512-1 -> 20170824-1
icu 59.1-1 -> 59.1-2
iproute2 4.12.0-2 -> 4.13.0-1
libgcrypt 1.8.0-1 -> 1.8.1-1
libldap 2.4.44-5 -> 2.4.45-4
libpsl 0.17.0-2 -> 0.18.0-1
linux-api-headers 4.10.1-1 -> 4.12.7-1
lz4 1:1.7.5-1 -> 1:1.8.0-1
ncurses 6.0+20170527-1 -> 6.0+20170902-1
p11-kit 0.23.7-1 -> 0.23.8-1
pacman-mirrorlist 20170729-1 -> 20170907-1
pcre2 10.23-1 -> 10.30-1
perl 5.26.0-1 -> 5.26.0-4
perl-error 0.17025-1 -> 0.17025-2
sqlite 3.20.0-1 -> 3.20.1-1
{% endhighlight %}

![Docker Arch: pacman upgradable][image-ss-pm-upgradable]{: .img-responsive }

#### Download Upgrade

{% highlight bash %}
$ pacman -Suw
{% endhighlight %}

{% highlight bash %}
$ pacman --sync --sysupgrade --downloadonly
:: Starting full system upgrade...
resolving dependencies...

Packages (24) archlinux-keyring-20170823-1  coreutils-8.28-1
              curl-7.55.1-2  e2fsprogs-1.43.6-1  expat-2.2.4-1
              gcc-libs-7.2.0-2  glibc-2.26-3  gnupg-2.2.0-1
              gnutls-3.5.15-1  iana-etc-20170824-1  icu-59.1-2
              iproute2-4.13.0-1  libgcrypt-1.8.1-1
              libldap-2.4.45-4  libpsl-0.18.0-1
              linux-api-headers-4.12.7-1  lz4-1:1.8.0-1
              ncurses-6.0+20170902-1  p11-kit-0.23.8-1
              pacman-mirrorlist-20170907-1  pcre2-10.30-1
              perl-5.26.0-4  perl-error-0.17025-2  sqlite-3.20.1-1

Total Download Size:  63.59 MiB

:: Proceed with download? [Y/n]
{% endhighlight %}

![Docker Arch: pacman download][image-ss-pm-download]{: .img-responsive }

You may add <code>--noconfirm</code> if you wish.
this one not asking for PGP confirmation anymore.

![Docker Arch: pacman confirm][image-ss-pm-noconfirm]{: .img-responsive }

#### Upgrade

This command will install package, and download if necessary.
This is a process with long output, therefore I put three images here.

This is why the first figure output looks pretty similar
with the download command above since it works on the same upgradable packages.
But the next figure is different.

{% highlight bash %}
$ pacman -Su
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --sysupgrade
{% endhighlight %}

![Docker Arch: pacman sysupgrade][image-ss-pm-sysupgrade]{: .img-responsive }

#### Common Form

Consider put all them command above together.
You can combine update dan upgrade in single command.
I usually only use this single command.

{% highlight bash %}
$ pacman -Syu
:: Synchronizing package databases...
 core is up to date
 extra is up to date
 community is up to date
:: Starting full system upgrade...
 there is nothing to do
{% endhighlight %}

![Docker Arch: pacman -Syu][image-ss-pacman-syu]{: .img-responsive }

-- -- --


### Package IRSI

To DO

-- -- --

### Not Finished Yet

To DO

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/24/docker-arch-alpm.html

[image-ss-pull-arch]:		{{ asset_pull }}/arch.png
[image-ss-running-arch]:	{{ asset_post }}/00-running-image.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-getting-started]: {{ asset_post }}/00-getting-started.png

[image-ss-pm-refresh]:		{{ asset_post }}/01-refresh.png
[image-ss-pm-upgradable]:	{{ asset_post }}/01-upgradable.png
[image-ss-pm-download]:		{{ asset_post }}/01-download.png
[image-ss-pm-noconfirm]:	{{ asset_post }}/01-noconfirm.png
[image-ss-pm-sysupgrade]:	{{ asset_post }}/01-sysupgrade.png
[image-ss-pacman-syu]:		{{ asset_post }}/01-pacman-syu.png
