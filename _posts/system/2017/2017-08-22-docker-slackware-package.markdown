---
layout: post
title: "Docker - Slackware Package - Part Two"
date: 2017-08-22 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, slackware]
author: epsi

excerpt:
  Docker flow for Slackware Package Management,
  from slackpkg binary to slackbuild compilation.
  First time using Slackware experience.
  One of Three Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
# - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-slackware-package.html %}

-- -- --

### Slackbuild

There are few options to have slackbuild compiled.

*	Manual Compilation

*	Using <code>sbopkg</code>

*	Other tools such as <code>slapt-src</code>, and <code>slpkg</code>

Here we I use <code>fish</code> as an experiment.
You can use other Slackbuild as well such as <code>ncdu</code>.

#### Manual Compilation

Get the slackbuild.
Using the appropriate slackware version.

{% highlight bash %}
$ cd ~

$ wget -c ftp://ftp.slackbuilds.org/pub/slackbuilds/14.2/system/fish.tar.gz

$ tar -xvf fish.tar.gz

$ cd fish

$ chmod +x fish.SlackBuild
{% endhighlight %}

Get the source code.
Using <code>fish.info</code>

{% highlight bash %}
$ cat fish.info 
PRGNAM="fish"
VERSION="2.6.0"
HOMEPAGE="http://fishshell.com/"
DOWNLOAD="https://github.com/fish-shell/fish-shell/releases/download/2.6.0/fish-2.6.0.tar.gz"
MD5SUM="ce9d8cc2a34d172a94cfa3ef9988937c"
DOWNLOAD_x86_64=""
MD5SUM_x86_64=""
REQUIRES="man-db"
MAINTAINER="Edinaldo P. Silva"
EMAIL="edps.mundognu@gmail.com"

$ wget -c --no-check-certificate https://github.com/fish-shell/fish-shell/releases/download/2.6.0/fish-2.6.0.tar.gz
{% endhighlight %}

Do the compilation process.
Be brave to track error in <code class="code-file">/tmp/*/config.log</code>
whenever you encounter error.

{% highlight bash %}
$ ./fish.SlackBuild
...
Slackware package /tmp/fish-2.6.0-x86_64-1_SBo.tgz created.
{% endhighlight %}

And finally install.

{% highlight bash %}
$ installpkg /tmp/fish-2.6.0-x86_64-1_SBo.tgz
Verifying package fish-2.6.0-x86_64-1_SBo.tgz.
Installing package fish-2.6.0-x86_64-1_SBo.tgz:
PACKAGE DESCRIPTION:
# fish (Friendly Interactive Shell)
#
# fish is a user friendly command line shell for UNIX-like systems.
#
# fish includes case insensitive completions, a multiline editing
# system, a new and simplified key binding system, and a large number
# of command specific completions.
#
# Homepage: http://fishshell.com/
#
Executing install script for fish-2.6.0-x86_64-1_SBo.tgz.
Package fish-2.6.0-x86_64-1_SBo.tgz installed.
{% endhighlight %}

![Docker Slackware: Build Fish using Slackbuild][image-ss-slackbuild-fish]{: .img-responsive }

#### Tracking Dependency

Sometimes (or usually for beginner),
error happened while building from source.
This step give me better understanding of my system.

{% highlight bash %}
$ ./fish.SlackBuild

...
...

configure.ac:28: installing './missing'
Makefile.am: installing './depcomp'
autoreconf: Leaving directory 'pcre2-10.22'
autoreconf: configure.ac: not using Libtool
autoreconf: configure.ac: not using Automake
autoreconf: Leaving directory '.'
checking if autoreconf needs to be run... no
checking if autoheader needs to be run... no
checking for gcc... gcc
checking whether the C compiler works... no
configure: error: in '/tmp/SBo/fish-2.6.0':
configure: error: C compiler cannot create executables
See 'config.log' for more details
{% endhighlight %}

You should take a look at the config log for more verbose message.

{% highlight bash %}
$ less /tmp/SBo/fish-2.6.0/config.log
{% endhighlight %}

![Docker Slackware: Take a look at config.log][image-ss-fish-config-log]{: .img-responsive }

You can see in that figure above that 
we need to install <code>as</code> the Portable GNU Assembly.
Other case we encounter a more complicated thing,
such as missing file that must be search first.
Here is some example.

{% highlight bash %}
$ find /usr/ -name crti*

$ slackpkg file-search crti.o

$ slackpkg search libxml2
{% endhighlight %}

This doesn't look as so bad. 
Actually searching for these files is fun if you have time.

	All you need is patience, and passion.

-- -- --

### sbopkg

After manual compilation, we can continue to automatic compilation.
There is this <code>sbopkg</code> tool to make your life easier.

**Reading**

*	Official Site: [Building and Installing Packages with sbopkg][slackware-sbopkg]

#### Install

{% highlight bash %}
$ cd ~

$ wget -c --no-check-certificate https://github.com/sbopkg/sbopkg/releases/download/0.38.1/sbopkg-0.38.1-noarch-1_wsr.tgz

$ installpkg sbopkg-0.38.1-noarch-1_wsr.tgz
{% endhighlight %}

Now you are ready to use <code>sbopkg</code>.
But however you need to install <code>rsync</code> first,
to populate repository data to local.

#### Update

	First Thing First

{% highlight bash %}
$ sbopkg

$ slackpkg install rsync

$ sbopkg -r

$ sbopkg -i fish
{% endhighlight %}

![Docker Slackware: Install Fish with sbopkg][image-ss-sbopkg-install]{: .img-responsive }

As long as you prepare toolchain well,
<code>sbopkg</code> is helpful.

#### The sbopkg Log File

{% highlight bash %}
$ tail -n 15 /var/log/sbopkg/sbopkg-build-log 
SUMMARY LOG
Using the SBo repository for Slackware 14.2
Queue Process:  Download, build, and install

slpkg:
  MD5SUM check for slpkg-3.2.8.tar.gz ... OK
  Building package slpkg-3.2.8-x86_64-1_SBo.tgz ... OK
  Installing package slpkg-3.2.8-x86_64-1_SBo.tgz ... OK
{% endhighlight %}

![Docker Slackware: sbopkg build log][image-ss-sbopkg-build-log]{: .img-responsive }

-- -- --

### Using slapt-get

There also unofficial package manager for slackware that emulate Debian apt-get.
<code>slapt-get</code> for binary,
and <code>slapt-src</code> for source such as slackbuild.

#### Reading

*	<https://software.jaos.org/>

*	<https://www.slackwiki.com/Slapt-get>

#### Install slapt-get

{% highlight bash %}
$ wget -c  --no-check-certificate https://software.jaos.org/slackpacks/14.2-x86_64/slapt-get/slapt-get-0.10.2t-x86_64-1.tgz

$ tar -xvf slapt-get-0.10.2t-x86_64-1.tgz
{% endhighlight %}

{% highlight bash %}
$ installpkg slapt-get-0.10.2t-x86_64-1.tgz 
Verifying package slapt-get-0.10.2t-x86_64-1.tgz.
Installing package slapt-get-0.10.2t-x86_64-1.tgz:
...
Executing install script for slapt-get-0.10.2t-x86_64-1.tgz.
Package slapt-get-0.10.2t-x86_64-1.tgz installed.
{% endhighlight %}

![Docker slapt-get: Installing][image-ss-install-slaptget]{: .img-responsive }

#### Dependencies of slapt-get

These packages are required to run <code>slapt-get</code>

{% highlight bash %}
$ slackpkg install gpgme

$ slackpkg install libassuan

$ slackpkg install libgpg

$ slackpkg install cyrus-sasl
{% endhighlight %}

#### Update

	First Thing First

{% highlight bash %}
$ slapt-get --update
Retrieving package data [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Retrieving patch list [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Retrieving checksum list [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Retrieving checksum signature [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Verifying checksum signature [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Verified
Retrieving ChangeLog.txt [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Reading Package Lists...Done
Retrieving package data [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Retrieving patch list [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Retrieving checksum list [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Retrieving checksum signature [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Verifying checksum signature [http://software.jaos.org/slackpacks/14.2-x86_64/]...No key for verification
Retrieving ChangeLog.txt [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Reading Package Lists...Done
{% endhighlight %}

[![Docker slapt-get: Update][image-ss-slaptget-update]{: .img-responsive }][photo-ss-slaptget-update]

#### Upgrade

{% highlight bash %}
$ slapt-get --upgrade
Reading Package Lists...Done
The following packages have been EXCLUDED:
  glibc-solibs 
0 upgraded, 0 reinstalled, 0 newly installed, 0 to remove, 1 not upgraded.

Done
{% endhighlight %}

[![Docker slapt-get: Upgrade][image-ss-slaptget-upgrade-half]{: .img-responsive }][photo-ss-slaptget-upgrade-full]

#### Install

This <code>htop</code> install succeed.

{% highlight bash %}
$ slapt-get --install htop
Reading Package Lists...Done
htop is up to date.
0 upgraded, 0 reinstalled, 0 newly installed, 0 to remove, 0 not upgraded.

Done
{% endhighlight %}

![Docker slapt-get: Install htop][image-ss-slaptget-htop]{: .img-responsive }

But this <code>ncdu</code> install failed,
because <code>ncdu</code> comes from slackbuild.

{% highlight bash %}
slapt-get --install ncdu
Reading Package Lists...Done
No such package: ncdu
{% endhighlight %}

![Docker slapt-get: Install ncdu][image-ss-slaptget-ncdu]{: .img-responsive }

#### Show Info

{% highlight bash %}
slapt-get --show ncdu
Package Name: ncdu
Package Mirror: 
Package Priority: Default
Package Location: 
Package Version: 1.8-x86_64-1_SBo
Package Size: 28 K
Package Installed Size: 80 K
Package Required: 
Package Conflicts: 
Package Suggests: 
Package MD5 Sum:  
Package Description:
 ncdu (NCurses Disk Usage)

 As the name already suggests, ncdu is an NCurses version of the famous
 old 'du' unix command. It provides a fast and easy interface to your
 harddrive. Where is your disk space going? Why is your home directory
 that large? ncdu can answer those questions for you in just a matter
 of seconds!

 http://dev.yorhel.nl/ncdu/


Package Installed: yes
{% endhighlight %}

![Docker slapt-get: Show Info ncdu][image-ss-slaptget-show-ncdu]{: .img-responsive }

Since ncdu come from slackbuild repository that needed to be compiled first.
We need another tool to manage.

-- -- --

### Using slapt-src

In order to use slackbuild form source,
we can utilize <code>slapt-src</code>.
This time no need to wget,
since we already have <code>slapt-get</code>.

{% highlight bash %}
$ slapt-get --install slapt-src
Reading Package Lists...Done
The following NEW packages will be installed:
  slapt-src 
0 upgraded, 0 reinstalled, 1 newly installed, 0 to remove, 0 not upgraded.
Need to get 48.0kB of archives.
After unpacking 220.0kB of additional disk space will be used.
1/1 Get http://software.jaos.org/slackpacks/14.2-x86_64/ slapt-src 0.3.2i-x86_64-1 [48.0kB]...Done

Preparing to install slapt-src-0.3.2i-x86_64-1
Verifying package slapt-src-0.3.2i-x86_64-1.tgz.
Installing package slapt-src-0.3.2i-x86_64-1.tgz:
PACKAGE DESCRIPTION:
# slapt-src (slapt slackbuild utility)
# slapt-src is a utility to make querying, retrieving, and building
# slackbuilds as easy as working with binary packages with slapt-get.
# 
#
Executing install script for slapt-src-0.3.2i-x86_64-1.tgz.
Package slapt-src-0.3.2i-x86_64-1.tgz installed.

Done
{% endhighlight %}

![Docker slapt-src: Installing][image-ss-slaptget-slaptsrc]{: .img-responsive }

#### Update

	First Thing First

Do update first before doing nay task with any command.

{% highlight bash %}
$ slapt-src -u
Fetching slackbuild list from http://www.slackware.org.uk/slackbuilds.org/14.0/...Cached
{% endhighlight %}

![Docker slapt-src: Update][image-ss-slaptsrc-update]{: .img-responsive }

#### Install

Now we can install <code>ncdu</code> from slackbuild.

{% highlight bash %}
sh-4.3# slapt-src --install ncdu
The following packages will be installed:
 ncdu 
Do you want to continue? [y/N] y
Fetching README...Done
Fetching ncdu.SlackBuild...Done
Fetching ncdu.info...Done
Fetching slack-desc...Done
ncdu-1.8/
ncdu-1.8/doc/
ncdu-1.8/doc/Makefile.am
ncdu-1.8/doc/Makefile.in
ncdu-1.8/doc/ncdu.1
ncdu-1.8/src/
{% endhighlight %}

[![Docker slapt-src: Install ncdu][image-ss-slaptsrc-ncdu-half]{: .img-responsive }][photo-ss-slaptsrc-ncdu-full]

-- -- --

### What's Next

Consider finish reading [ [Part Three][local-part-three] ]
that cover <code>slpkg</code> and repository.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_flow = site.url | append: '/assets/posts/system/2017/08/docker-flow' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-slackware' %}

[local-part-three]: {{ site.url }}/system/2017/08/23/docker-slackware-package.html

[slackware-sbopkg]: https://docs.slackware.com/howtos:slackware_admin:building_packages_with_sbopkg

[image-ss-sbopkg-build-log]:   {{ asset_post }}/19-sbopkg-log.png

[image-ss-slackbuild-fish]:  {{ asset_post }}/22-slackbuild-fish.png
[image-ss-fish-config-log]:  {{ asset_post }}/22-fish-config-log.png
[image-ss-sbopkg-install]:   {{ asset_post }}/22-sbopkg-fish.png

[image-ss-install-slaptget]:      {{ asset_post }}/24-install-slaptget.png
[image-ss-slaptget-update]:       {{ asset_post }}/24-slaptget-update.png
[photo-ss-slaptget-update]:       https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNqu4dS9aQFxUQhNUZYBaynXouzDmhv2vjmO1ms?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ss-slaptget-upgrade-half]: {{ asset_post }}/24-slaptget-upgrade-half.png
[photo-ss-slaptget-upgrade-full]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMC8TaVP2h4_9ptQHL0pGZ1axhn9GtdQ_9BuYP7?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ss-slaptget-htop]:         {{ asset_post }}/24-slaptget-install-htop.png
[image-ss-slaptget-show-ncdu]:    {{ asset_post }}/24-slaptget-show-ncdu.png
[image-ss-slaptget-ncdu]:         {{ asset_post }}/24-slaptget-install-ncdu.png
[image-ss-slaptget-slaptsrc]:     {{ asset_post }}/25-slaptget-install-slaptsrc.png
[image-ss-slaptsrc-update]:       {{ asset_post }}/25-slaptsrc-update.png
[image-ss-slaptsrc-ncdu-half]:    {{ asset_post }}/25-slaptsrc-install-ncdu-half.png
[photo-ss-slaptsrc-ncdu-full]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMQeESVsylBl5qU3t9C-l6fy0BsdAwezYvjhTjq?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
