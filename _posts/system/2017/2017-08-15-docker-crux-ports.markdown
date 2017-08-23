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

> Goal: Learning Package Manager, Focus on Command Line Interface

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
$ docker run -it crux
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

	First Thing First

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

Now is time to use <code>prt-get</code>,
an advanced package management tool for CRUX.
If you are helpless, just add this <code>help | less</code> argument.

{% highlight bash %}
$ prt-get help | less
{% endhighlight %}

Consider system update using <code>prt-get</code>.

{% highlight bash %}
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

As you can see above, openssl has been failed to compile.
Which we will solve later.

-- -- --

### Package IRSI

	Install, Remove, Search, Info

Read the fine manual.

{% highlight bash %}
$ prt-get help | less
{% endhighlight %}

#### Package Install

You can use <code>prt-get install</code>
to install new package,
or <code>prt-get update</code> 
to upgrade package currently installed package.

{% highlight bash %}
$ prt-get install man nano htop mc ncdu fish
prt-get: installing /usr/ports/opt/mc
=======> Building '/usr/ports/opt/mc/mc#4.8.15-1.pkg.tar.gz'.
bsdtar -p -o -C /usr/ports/opt/mc/work/src -xf /usr/ports/opt/mc/mc-4.8.15.tar.xz
+ build
+ cd mc-4.8.15
...
{% endhighlight %}

[![Docker Crux: prt-get Install][image-ss-prtget-install]{: .img-responsive }][photo-ss-prtget-install]

#### Package Remove

{% highlight bash %}
$ prt-get remove htop
{% endhighlight %}

![Docker Crux: prt-get Remove][image-ss-prtget-remove]{: .img-responsive }

#### Package Search

{% highlight bash %}
$ prt-get search htop
htop
{% endhighlight %}

{% highlight bash %}
$ prt-get dsearch htop
htop
{% endhighlight %}

{% highlight bash %}
$ prt-get fsearch htop
Found in /usr/ports/opt/bash-completion:
  /usr/share/bash-completion/completions/htop

Found in /usr/ports/opt/htop:
  /usr/bin/htop
{% endhighlight %}

![Docker Crux: prt-get Search][image-ss-prtget-search]{: .img-responsive } 

#### Package Info

{% highlight bash %}
$ prt-get info htop
{% endhighlight %}

![Docker Crux: prt-get Info][image-ss-prtget-info]{: .img-responsive }

-- -- --

### Ports Difference

You check the ports difference.

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

### pkgadd

Sometimes <code>prt-get</code> does not work, as you can see below.
We have to face <code>openssl</code> install issue.

{% highlight bash %}
$ prt-get update openssl
...
=======> ERROR: Building '/usr/ports/core/openssl/openssl#1.0.2g-1.pkg.tar.gz' failed.

-- Packages where update failed
openssl
{% endhighlight %}

![Docker Crux: prt-get Update][image-ss-prtget-update]{: .img-responsive }

I was lucky, the package is there in <code>openssl</code> directory.
Therefore I can install using lower level <code>pkg</code> command directly.

{% highlight bash %}
$ ls /usr/ports/core/openssl/
Pkgfile       openssl#1.0.2g-1.pkg.tar.gz
mksslcert.sh  openssl-1.0.2g.tar.gz

$ pkgadd -u /usr/ports/core/openssl/openssl#1.0.2g-1.pkg.tar.gz 

$ prt-get diff
No differences found
{% endhighlight %}

![Docker Crux: pkgadd Update][image-ss-pkgadd-update]{: .img-responsive }

-- -- --

### Dependency Build Issue with Unmaintained Docker Container

{% highlight bash %}
$ prt-get install mc
...
configure: WARNING: 'Check' utility not found. Check your environment
checking for GLIB... no
configure: error: glib-2.0 not found or version too old (must be >= 2.26)
=======> ERROR: Building '/usr/ports/opt/mc/mc#4.8.15-1.pkg.tar.gz' failed.

-- Packages where install failed
mc
{% endhighlight %}

![Docker Crux: mc error][image-ss-mc-error]{: .img-responsive }

{% highlight bash %}
$ prt-get search glib
dbus-glib
glib
glibc
glibc-32
poppler-glib
taglib
{% endhighlight %}

![Docker Crux: mc error][image-ss-search-glib]{: .img-responsive }

It turned out that <code>glibc</code> version is 
<code>2.19</code>, which is less than <code>2.26</code>.

{% highlight bash %}
$ prt-get info glibc
Name:         glibc
Path:         /usr/ports/core
Version:      2.19
Release:      5
Description:  The C library used in the GNU system
URL:          http://www.gnu.org/software/libc/
Maintainer:   CRUX System Team, core-ports at crux dot nu
Files:        post-install
{% endhighlight %}

![Docker Crux: info glibc][image-ss-info-glibc]{: .img-responsive }

Unfortunately, update command, not uprading the <code>glibc</code> version.

{% highlight bash %}
$ prt-get update glibc
prt-get: updating /usr/ports/core/glibc
=======> Package '/usr/ports/core/glibc/glibc#2.19-5.pkg.tar.gz' is up to date.
prt-get: reinstalling glibc 2.19-5
pkgadd: rejecting etc/ld.so.cache, keeping existing version
pkgadd: rejecting etc/resolv.conf, keeping existing version
pkgadd: rejecting etc/hosts, keeping existing version

-- Packages updated
glibc

prt-get: updated successfully
{% endhighlight %}

![Docker Crux: update glibc][image-ss-update-glibc]{: .img-responsive }

-- -- --

### Distribution Upgrade

Distribution upgrade from <code>3.1</code> to <code>3.2</code>
can be done using live DVD. 
But cannot be done directly due to ncurse break.
Distribution upgrade from <code>3.2</code> to <code>3.3</code>
also can be done using live DVD. 
Since we are using Docker, it can't be done with external boot.

	I should stay with 3.1 instead

-- -- --

### Conclusion

	There are things unsolved

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-crux' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-crux-docker]:    {{ asset_post }}/00-start.png
[image-ss-pull-crux]:      {{ asset_pull }}/crux.png

[image-ss-ports-update]:   {{ asset_post }}/01-ports-update-half.png
[photo-ss-ports-update]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNlVnnQZaB1hUzsq5627jq4kQvfTiKv6K2FZz1U?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-sysup]:   {{ asset_post }}/01-prtget-sysup-half.png
[photo-ss-prtget-sysup]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNgbQz6ZW3fjABCjtcUaLZNlOcs03-_XckXl_63?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-diff]:    {{ asset_post }}/01-ports-difference.png

[image-ss-prtget-install]: {{ asset_post }}/13-prtget-install-half.png
[photo-ss-prtget-install]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMQaSQgPAmbhyfZwXjkhT46X8JPTWJ5035-5Mhm?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-info]:    {{ asset_post }}/13-prtget-info.png
[image-ss-prtget-remove]:  {{ asset_post }}/13-prtget-remove.png
[image-ss-prtget-search]:  {{ asset_post }}/13-prtget-search.png

[image-ss-prtget-update]:  {{ asset_post }}/22-prtget-update.png
[image-ss-pkgadd-update]:  {{ asset_post }}/23-pkgadd-update.png

[image-ss-mc-error]:       {{ asset_post }}/24-prtget-install-mc-error.png
[image-ss-search-glib]:    {{ asset_post }}/24-prtget-search-glib.png
[image-ss-info-glibc]:     {{ asset_post }}/24-prtget-info-glibc.png
[image-ss-update-glibc]:   {{ asset_post }}/24-prtget-update-glibc.png


