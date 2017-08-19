---
layout: post
title: "Docker - Slackware Package Management"
date: 2017-08-11 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, slackware]
author: epsi

excerpt:
  Docker flow for Slackware Package Management,
  from slackpkg binary to slackbuild compilation.
  First time using Slackware experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Preface

I am so glad that finally 
I can learn Slackware Package Management using Docker.

You can read a common overview about docker here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]

There are few topics here.

*	Preparing Docker

*	Using slackpkg

*	Using slackbuild the hard way

*	Using sbopkg

*	Using slapt-get and slapt-src

There are still other topic uncovered here

*	slackpkgplus

*	adding non official repo such as alien and slacky


-- -- --

### Getting Started With Docker

Our first move is, of course attach docker process, as usual.

{% highlight bash %}
$ docker pull vbatts/slackware
{% endhighlight %}

{% highlight bash %}
$ docker image list 
{% raw %}
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY              SIZE
gentoo/stage3-amd64     873MB
vbatts/slackware        86.7MB
voidlinux/voidlinux     202MB
kevinleptons/lfs-auto   753MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it vbatts/slackware
{% endhighlight %}

{% highlight bash %}
$ docker ps -a
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %} 
IMAGE               NAMES
vbatts/slackware    cranky_keller
{% endhighlight %}

{% highlight bash %}
$ docker start cranky_keller
cranky_keller
{% endhighlight %}

{% highlight bash %}
$ docker attach cranky_keller
sh-4.3# cat /etc/slackware-version 
Slackware 14.2
sh-4.3# 
{% endhighlight %}

![Docker Slackware: Getting Started][image-ss-slack-docker]{: .img-responsive }

-- -- --

### Package Management

Slackware does not really have a sophisticated official package management,
but there are special unoffical tools to manage your packages such as <code>slapt-get</code>.

slackpkg
	
	Package Cache
	
	*	/var/adm/packages/*-x86_64-?

slapt-get

*	<https://github.com/jaos/slapt-get>

-- -- --

### First Thing First

My first attempt is to install package I cannot live with.
These package might be different with your favorites.

Remember to run <code>slackpkg update</code> first.

{% highlight bash %}
$ slackpkg update

$ slackpkg install man nano htop mc curl 
{% endhighlight %}

Unfortunately when I run <code>$ man man</code>, error did happened. 
I had to fix using <code>$ slackpkg file-search</code>

{% highlight bash %}
$ man man
sh: /usr/bin/nroff: No such file or directory
sh: /usr/bin/gtbl: No such file or directory
{% endhighlight %}

{% highlight bash %}
$ slackpkg file-search nroff

Looking for nroff in package list. Please wait... DONE

The list below shows the packages that contains "nroff" file.

[uninstalled] - enscript-1.6.6-x86_64-1
[uninstalled] - groff-1.22.3-x86_64-2
[uninstalled] - jed-0.99_19-x86_64-2
[ installed ] - mc-4.8.16-x86_64-2
[uninstalled] - vim-7.4.1938-x86_64-1
[uninstalled] - emacs-24.5-x86_64-2
[uninstalled] - aspell-0.60.6.1-x86_64-1

You can search specific packages using "slackpkg search package".
{% endhighlight %}

{% highlight bash %}
$ slackpkg install groff

Looking for groff in package list. Please wait... DONE

groff-1.22.3-x86_64-2.txz
{% endhighlight %}

And voila! Nomore encounter with error.

![Docker Slackware: Search file belonging][image-ss-file-search]{: .img-responsive }

-- -- --

### Two Kind of Toy

	What should I do with this Container ?

How you play with your toy is up to you.
At least we have two choice.

*	Populate Using Slackware64

*	Stay with Minimal Install.

#### Using Slackware64

The docker image <code>vbatts/slackware</code>
is a slackware minimal install.
Therefore you need to populate with full slackware distribution.

{% highlight bash %}
$ slackpkg update

$ slackpkg install slackware64
{% endhighlight %}

![Docker Slackware: Populate Slackware64][image-ss-slackware64]{: .img-responsive }

The issue with slackware64 is 
my docker container grown from 86.7 MB to 8.64 GB.
Slackware64 has this huge size as a drawback.
For that reason I decice to scrap the container.
I can't afford huge container,
as I intent to play with other docker container as well.

![Docker Slackware: Huge Container Slackware64][image-ss-slackware64-huge]{: .img-responsive }

I will do that when I have my own computer.
But I would never do that again with my docker.
Cheers.

#### Stay with Minimal Install

	Prepare toolchain

With minimal install, I am own my own.
I assume that I know what I am doing.

For binary package come form official repository, it is all alright.
The drawback is you have to prepare toolchain for slackbuild package,
this slackbuild is unofficial, and must be compiled.

I must admit that I know nothing about toolchain.
But here it is what you need based on my experience.

{% highlight bash %}
$ slackpkg install perl
$ slackpkg install gcc
$ slackpkg install autoconf automake m4 gettext libtool
$ slackpkg install glibc binutils kernel-headers make 
$ slackpkg install libmpc flex guille gc 
{% endhighlight %}

And some other package required
by make when I compile <code>fish</code>

{% highlight bash %}
$ slackpkg install libffi libcroco libxml2
{% endhighlight %}

It is not a big deal.
Just be brave to identify what each build need.

-- -- --

### Slackbuild

There are few options to have slackbuild compiled.

*	Manual Compilation

*	Using sbopkg

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
Be brave to track error in <code>/tmp/*/config.log</code>
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

#### Tracking Error

Sometimes (or usually for beginner),
error happened while building from source.

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


#### Using sbopkg

**Reading**

*	Official Site: [Building and Installing Packages with sbopkg][slackware-sbopkg]

There is this tool to make your life easier.

{% highlight bash %}
$ cd ~

$ wget -c --no-check-certificate https://github.com/sbopkg/sbopkg/releases/download/0.38.1/sbopkg-0.38.1-noarch-1_wsr.tgz

$ installpkg sbopkg-0.38.1-noarch-1_wsr.tgz
{% endhighlight %}

Now you are ready to use <code>sbopkg</code>.
But however you need to install <code>rsync</code> first,
to populate repository data to local.

{% highlight bash %}
$ sbopkg

$ slackpkg install rsync

$ sbopkg -r

$ sbopkg -i fish
{% endhighlight %}

![Docker Slackware: Install Fish with sbopkg][image-ss-sbopkg-install]{: .img-responsive }

As long as you prepare toolchain well,
<code>sbopkg</code> is helpful.

-- -- --

### Using slapt-get

There also unofficial package manager for slackware that emulate Debian apt-get.
<code>slapt-get</code> for binary,
and <code>slapt-src</code> for source such as slackbuild.

#### Reading

*	<https://software.jaos.org/>

*	<https://www.slackwiki.com/Slapt-get>

#### Installing slapt-get

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

### slapt-src

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

### Conclusion

It is said that "_Slackware does not resolve dependency_".
I haven't got enough experience about this situation.
I simply do not understand what it means.

	I guess I have to learn slackware more.

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[slackware-sbopkg]: https://docs.slackware.com/howtos:slackware_admin:building_packages_with_sbopkg

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-slack-docker]:     {{ asset_path }}/docker-slackware-0-start.png
[image-ss-slackware64]:      {{ asset_path }}/docker-slackware-1-install-slackware64.png
[image-ss-file-search]:      {{ asset_path }}/docker-slackware-1-file-search-groff.png
[image-ss-slackware64-huge]: {{ asset_path }}/docker-flow-terminal-2.png
[image-ss-slackbuild-fish]:  {{ asset_path }}/docker-slackware-2-slackbuild-fish.png
[image-ss-fish-config-log]:  {{ asset_path }}/docker-slackware-2-fish-config-log.png
[image-ss-sbopkg-install]:   {{ asset_path }}/docker-slackware-2-sbopkg-fish.png

[image-ss-install-slaptget]:      {{ asset_path }}/docker-slackware-4-install-slaptget.png
[image-ss-slaptget-update]:       {{ asset_path }}/docker-slackware-4-slaptget-update.png
[photo-ss-slaptget-update]:       https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNqu4dS9aQFxUQhNUZYBaynXouzDmhv2vjmO1ms?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ss-slaptget-upgrade-half]: {{ asset_path }}/docker-slackware-4-slaptget-upgrade-half.png
[photo-ss-slaptget-upgrade-full]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMC8TaVP2h4_9ptQHL0pGZ1axhn9GtdQ_9BuYP7?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ss-slaptget-htop]:         {{ asset_path }}/docker-slackware-4-slaptget-install-htop.png
[image-ss-slaptget-show-ncdu]:    {{ asset_path }}/docker-slackware-4-slaptget-show-ncdu.png
[image-ss-slaptget-ncdu]:         {{ asset_path }}/docker-slackware-4-slaptget-install-ncdu.png
[image-ss-slaptget-slaptsrc]:     {{ asset_path }}/docker-slackware-5-slaptget-install-slaptsrc.png
[image-ss-slaptsrc-update]:       {{ asset_path }}/docker-slackware-5-slaptsrc-update.png
[image-ss-slaptsrc-ncdu-half]:    {{ asset_path }}/docker-slackware-5-slaptsrc-install-ncdu-half.png
[photo-ss-slaptsrc-ncdu-full]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMQeESVsylBl5qU3t9C-l6fy0BsdAwezYvjhTjq?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
