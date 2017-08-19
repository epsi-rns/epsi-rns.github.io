---
layout: post
title: "Docker - Crux Ports"
date: 2017-08-15 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, lfs]
author: epsi

excerpt:
  Docker flow for Crux Linux pkgtools,
  first time using Crux Linux experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Preface

This Crux is even easier with docker container, no need to set up system.
Therefore we can dive right away to compilation.

Since we are going to use docker again,
you can read a common overview here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]

#### Reading

*	<https://crux.nu/Main/Handbook3-3>

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull crux
{% endhighlight %}

![Docker Pull Crux Linux][image-ss-pull-crux]{: .img-responsive }

{% highlight bash %}
$ docker image list 
{% raw %}
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY              SIZE
opensuse/amd64          101MB
dock0/arch              870MB
gentoo/stage3-amd64     873MB
debian                  100MB
fedora                  232MB
busybox                 1.13MB
vbatts/slackware        86.7MB
voidlinux/voidlinux     202MB
kevinleptons/lfs-auto   753MB
crux                    342MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it voidlinux/voidlinux bash
bash-4.3# exit
{% endhighlight %}

{% highlight bash %}
$ docker ps -a 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE                       NAMES                  STATUS
crux                        nifty_beaver           Exited (0) 8 seconds ago
busybox                     dazzling_agnesi        Exited (0) 38 hours ago
debian:stretch              dazzling_neumann       Exited (0) 38 hours ago
dock0/arch                  silly_leakey           Exited (1) 38 hours ago
fedora:rawhide              musing_torvalds        Exited (0) 38 hours ago
opensuse/amd64:tumbleweed   friendly_brahmagupta   Exited (0) 39 hours ago
kevinleptons/lfs-auto       wonderful_nobel        Exited (0) 2 days ago
voidlinux/voidlinux         awesome_davinci        Exited (0) 41 hours ago
gentoo/stage3-amd64         amazing_shirley        Exited (0) 40 hours ago
vbatts/slackware            cranky_keller          Exited (0) 3 days ago
{% endhighlight %}

{% highlight bash %}
$ docker start nifty_beaver
nifty_beaver
{% endhighlight %}

{% highlight bash %}
$ docker attach nifty_beaver
bash-4.3# 
{% endhighlight %}

![Docker Crux: Getting Started][image-ss-crux-docker]{: .img-responsive }

-- -- --

### Package Management

Before Arch, there was Crux.
Arch was intended as additional Crux package manager,
but then was recreated from scratch.
While Arch Linux use <code>.pkg.tar.xz</code> extension for package, 
Crus use <code>.pkg.tar.gz</code> extension for package,.

#### Reading

*	<http://www.fukt.bsnet.se/~per/pkgutils/>

#### Source

*	<https://github.com/nipuL/pkgutils>

#### Front End

prt-get

*	<https://github.com/winkj/prt-get>

-- -- -- 

### Updating System

First thing to do is updating my system as usual.

You can use <code>ports -u</code> command.
That remind me of <code>emerge -u</code> command.

{% highlight bash %}
$ ports -u
Updating file list from crux.nu::ports/crux-3.1/core/
Updating collection core
Finished successfully
Updating file list from crux.nu::ports/crux-3.1/opt/
Updating collection opt
Finished successfully
Updating file list from crux.nu::ports/crux-3.1/xorg/
Updating collection xorg
Finished successfully
{% endhighlight %}

[![Docker Crux: Ports Update][image-ss-ports-update]{: .img-responsive }][photo-ss-ports-update]

And system update using <code>prt-get</code>,
an advanced package management tool for CRUX.

{% highlight bash %}
$ prt-get help | less

$ prt-get sysup
prt-get: updating /usr/ports/core/openssl
=======> Building '/usr/ports/core/openssl/openssl#1.0.2g-1.pkg.tar.gz'.
bsdtar -p -o -C /usr/ports/core/openssl/work/src -xf /usr/ports/core/openssl/openssl-1.0.2g.tar.gz
cp mksslcert.sh /usr/ports/core/openssl/work/src
+ build
+ cd openssl-1.0.2g
+ export 'MAKEFLAGS= -j1'
+ MAKEFLAGS=' -j1'
...
{% endhighlight %}

[![Docker Crux: Update System][image-ss-prtget-sysup]{: .img-responsive }][photo-ss-prtget-sysup]

And check the ports difference later.
As you can see above, openssl has been failed to compile.

{% highlight bash %}
$ ports -d
Collection  Name     Port      Installed
core        openssl  1.0.2g-1  1.0.2d-1

$ prt-get diff 
Differences between installed packages and ports tree:

Port                Installed           Available in the ports tree

openssl             1.0.2d-1            1.0.2g-1            
bash-4.3#  
{% endhighlight %}

![Docker Crux: Ports difference][image-ss-prtget-diff]{: .img-responsive }

-- -- --

### Install

{% highlight bash %}
$ prt-get install nano htop mc ncdu fish
prt-get: installing /usr/ports/opt/mc
=======> Building '/usr/ports/opt/mc/mc#4.8.15-1.pkg.tar.gz'.
bsdtar -p -o -C /usr/ports/opt/mc/work/src -xf /usr/ports/opt/mc/mc-4.8.15.tar.xz
+ build
+ cd mc-4.8.15
...
{% endhighlight %}

[![Docker Crux: prt-get Install][image-ss-prtget-install]{: .img-responsive }][photo-ss-prtget-install]

-- -- --

### Conclusion




Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-crux-docker]:    {{ asset_path }}/docker-crux-0-start.png
[image-ss-pull-crux]:      {{ asset_path }}/docker-pull-crux.png

[image-ss-ports-update]:   {{ asset_path }}/docker-crux-1-ports-update-half.png
[photo-ss-ports-update]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNlVnnQZaB1hUzsq5627jq4kQvfTiKv6K2FZz1U?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-sysup]:   {{ asset_path }}/docker-crux-1-prtget-sysup-half.png
[photo-ss-prtget-sysup]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNgbQz6ZW3fjABCjtcUaLZNlOcs03-_XckXl_63?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-diff]:    {{ asset_path }}/docker-crux-1-ports-difference.png

[image-ss-prtget-install]: {{ asset_path }}/docker-crux-2-prtget-install-half.png
[photo-ss-prtget-install]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMQaSQgPAmbhyfZwXjkhT46X8JPTWJ5035-5Mhm?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
