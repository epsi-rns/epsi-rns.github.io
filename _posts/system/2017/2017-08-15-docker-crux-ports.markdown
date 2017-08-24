---
layout: post
title: "Docker - Crux Ports"
date: 2017-08-15 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, crux]
author: epsi

excerpt:
  Docker flow for Crux Linux pkgtools,
  first time using Crux Linux experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

This Crux is even easier with docker container, no need to set up system.
Therefore we can dive right away to compilation.

#### Test Bed

1.	Container: Docker

2.	Operating System: Artix (OpenRC )

3.	Window Manager: Herbstluftwm

Since we are going to use docker again,
you can read a common overview here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]

Of course you can use virtualization, the issue is distraction.
We need to avoid tendency to focus on GUI tools.
At the same time, limiting the scope to CLI tools.
Most of the time, CLI tools is considered lower level than the GUI one.

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
$ docker ps 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE                       NAMES                  STATUS
opensuse/amd64:tumbleweed   elegant_nightingale   Up 17 hours
crux                        lucid_brown           Up 6 hours
fedora:rawhide              musing_torvalds       Up 7 hours
voidlinux/voidlinux         awesome_davinci       Up 6 hours
gentoo/stage3-amd64         amazing_shirley       Up 8 hours
vbatts/slackware            cranky_keller         Up 6 hours
{% endhighlight %}

![Docker openSUSE: List Running Containers][image-ss-docker-ps]{: .img-responsive }

{% highlight bash %}
$ docker start lucid_brown
nifty_beaver
{% endhighlight %}

{% highlight bash %}
$ docker attach lucid_brown
bash-4.3# 
{% endhighlight %}

![Docker Crux: Getting Started][image-ss-crux-docker]{: .img-responsive }

-- -- --

### Package Management

Before Arch, there was Crux.
Arch was intended as additional Crux package manager,
but then was recreated from scratch.
While Arch Linux use <code>.pkg.tar.xz</code> extension for package, 
Crus use <code>.pkg.tar.gz</code> extension for package.

#### Reading

*	<https://crux.nu/Main/Handbook3-1>

*	<https://crux.nu/Main/Handbook3-3>

*	<http://www.fukt.bsnet.se/~per/pkgutils/>

#### Source

*	<https://github.com/nipuL/pkgutils>

#### Front End

prt-get

*	<https://github.com/winkj/prt-get>

#### Get Help

If you are helpless, just add this <code>help | less</code> argument.

{% highlight bash %}
$ prt-get help | less
{% endhighlight %}

Or if you have <code>man-db</code> installed.
I mean after, source downloaded, compiled, built, and finally installed,
yes you can do below command.

{% highlight bash %}
$ man prt-get
{% endhighlight %}

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

Consider system update using <code>prt-get sysup</code>.

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

#### Package Removal

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

### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.
For that reason, I put the recorded event here, 
before discussing about any further feature.

Log options in Crux is disabled as default.
You have to enable it manually by editing <code>prt-get.conf</code>

{% highlight bash %}
$ vim /etc/prt-get.conf 
writelog enabled           # (enabled|disabled)
logfile  /var/log/pkgbuild/%n.log
{% endhighlight %}

![Docker: vim /etc/prt-get.conf][image-ss-prtget-log]{: .img-responsive }

This is the nano log after nano installation.

{% highlight bash %}
$ less /var/log/pkgbuild/nano.log
=======> Building '/usr/ports/opt/nano/nano#2.4.3-1.pkg.tar.gz' succeeded.
prt-get: Using PKGMK_PACKAGE_DIR: /usr/ports/opt/nano
prt-get: installing nano 2.4.3-1
prt-get: /usr/bin/pkgadd nano#2.4.3-1.pkg.tar.gz
prt-get: build done Thu Aug 24 10:20:01 2017
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/pkgbuild/nano.log][image-ss-less-log]{: .img-responsive }

**Reading**

*	<https://crux.nu/doc/prt-get%20-%20User%20Manual.html#config_logging>

-- -- --

### Group

I cannot find any reference about group in Crux's Ports.
I guess there is no group concept.

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

[image-ss-crux-docker]:    {{ asset_post }}/00-getting-started.png
[image-ss-docker-ps]:      {{ asset_post }}/00-docker-ps.png
[image-ss-pull-crux]:      {{ asset_pull }}/crux.png

[image-ss-ports-update]:   {{ asset_post }}/01-ports-update-half.png
[photo-ss-ports-update]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNlVnnQZaB1hUzsq5627jq4kQvfTiKv6K2FZz1U?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-sysup]:   {{ asset_post }}/01-prtget-sysup-half.png
[photo-ss-prtget-sysup]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNgbQz6ZW3fjABCjtcUaLZNlOcs03-_XckXl_63?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-prtget-diff]:    {{ asset_post }}/01-ports-difference.png
[image-ss-less-log]:       {{ asset_post }}/07-log.png
[image-ss-prtget-log]:     {{ asset_post }}/07-prtget-log-conf.png

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
