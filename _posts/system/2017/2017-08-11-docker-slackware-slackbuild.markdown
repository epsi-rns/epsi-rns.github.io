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

> Goal: Learning Package Manager, Focus on Command Line Interface

I am so glad that finally, for the first time,
I can learn Slackware Package Management using Docker.
Here is my report as a _new slacker_ who just landed in slackware land,
my journey using minimal install.

You can read a common overview about docker here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]


#### Package Management

Slackware does not really have a sophisticated official package management.
Beside <code>slackpkg</code> there are special unoffical tools 
to manage your packages such as <code>sbopkg</code>,
<code>slapt-get</code>, <code>slapt-src</code>,
<code>slapkg</code> and <code>slackpkgplus</code>.

slapt-get

*	<https://github.com/jaos/slapt-get>

slpkg

*	<https://github.com/dslackw/slpkg>

slackpkg
	
	Package Cache
	
	*	/var/cache/packages/*/*/.txz

#### Topics

There are few topics here.

*	Preparing Docker

*	IRSI: slackpkg install, removepkg, search, info

*	Using slackbuild, manual compilation, and installpkg

*	Using sbopkg, automatic compilation

*	Using slapt-get and slapt-src

*	Using slpkg

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

### slackpkg

#### Update

	First Thing First

Remember to run <code>slackpkg update</code> first.

{% highlight bash %}
$ slackpkg update
{% endhighlight %}

#### Solve Dependency Issue

My first attempt is to install package I cannot live with.
These package might be different with your favorites.

{% highlight bash %}
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

And voila! No more encounter with error.

![Docker Slackware: Search file belonging][image-ss-file-search]{: .img-responsive }

#### Official Documentation

	More command please ...

**Reading**:

*	<https://docs.slackware.com/slackware:slackpkg>

There are still, other cool command though.
However, I choose not to bloat my minimal install.

{% highlight bash %}
$ slackpkg upgrade patches
{% endhighlight %}

![Docker Slackware: Upgrade Patches][image-ss-slackpkg-patches]{: .img-responsive }

{% highlight bash %}
$ slackpkg check-updates

No news is good news
{% endhighlight %}

-- -- --

### Package IRSI

	Install, Remove, Search, Info

#### Package Install

We have seen Install in previous example.
How about reinstall ?

{% highlight bash %}
$ slackpkg reinstall man nano htop
{% endhighlight %}

![Docker Slackpkg: Reinstall][image-ss-slackpkg-reinstall]{: .img-responsive }

#### Package Remove

{% highlight bash %}
$ removepkg htop
{% endhighlight %}

![Docker Removepkg][image-ss-removepkg]{: .img-responsive }

#### Package Search

We have seen search in action in previous section.

{% highlight bash %}
$ slackpkg file-search htop
{% endhighlight %}

{% highlight bash %}
$ slackpkg search htop
{% endhighlight %}

#### Package Info

{% highlight bash %}
$ slackpkg info htop
{% endhighlight %}

![Docker Slackpkg: Info][image-ss-slackpkg-info]{: .img-responsive }

-- -- --

### Minimal or Full Install ?

	What should I do with this Container ?

How you play with your toy is up to you.
At least we have two choice.
Two kind of toy.

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

#### D Package Series

You can install <code>D Package Series</code>
as mentioned in installation help:

*	<http://www.slackware.com/install/softwaresets.php>

{% highlight bash %}
$ slackpkg install d
{% endhighlight %}

I remind that I keep my minimal install for learning purpose.
So do nothave any plan to install <code>D Package Series</code>.
If storage is not a problem for you,
you can install <code>D Package Series</code>,
or even use full <code>Slackware64</code> install.

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

### Installing slpkg

I have been wondering how to add other repository,
such as <code>alien</code> and <code>slacky</code>.
And I found this <code>slpkg</code>. 

#### Dependency

{% highlight bash %}
$ slackpkg install python
{% endhighlight %}

![Docker slackpkg: Install Python][image-ss-slackpkg-python]{: .img-responsive }

#### Install

Now you can install <code>slpkg</code> using any method you like.

{% highlight bash %}
$ sbopkg -i slpkg
{% endhighlight %}

#### No certificate

My first attempt, is always failure as usual.

{% highlight bash %}
$ slpkg update 

To connect to mirrors.slackware.com insecurely, use --no-check-certificate.
{% endhighlight %}

![Docker slpkg: Update Certificate Error][image-ss-slpkg-update-err]{: .img-responsive }

If you encounter this error, you need to install <code>ca-certificates</code>.

{% highlight bash %}
$ slackpkg install ca-certificates
{% endhighlight %}

![Docker slackpkg: CA Certificates][image-ss-slackpkg-ca-cert]{: .img-responsive }

Now you should be ready.

-- -- --

### Using slpkg

#### Update

	First Thing First

{% highlight bash %}
$ slpkg update 

Check and update repositories:

Check repository [slack] ... Done
Check repository [sbo] ... Done
{% endhighlight %}

![Docker slpkg: Update Succeed][image-ss-slpkg-update]{: .img-responsive }

#### Repository 

{% highlight bash %}
$ slpkg repo-list

+==============================================================================
| Repo id  Repo URL                                            Default   Status
+==============================================================================
  alien    http://bear.alienbase.nl/mirrors/people/alien/sb~   yes     disabled
  connos   https://connochaetos.org/slack-n-free/              yes     disabled
  conrad   http://slack.conraid.net/repository/slackware64-~   yes     disabled
  csb      http://slackware.uk/csb/                            yes     disabled
  ktown    http://alien.slackbook.org/ktown/                   yes     disabled
  mles     http://slackware.uk/microlinux/                     yes     disabled
  msb      http://slackware.org.uk/msb/                        yes     disabled
  multi    http://bear.alienbase.nl/mirrors/people/alien/mu~   yes     disabled
  rested   http://bear.alienbase.nl/mirrors/people/alien/re~   yes     disabled
  rlw      http://slackware.uk/people/rlworkman/               yes     disabled
  salix    http://download.salixos.org/                        yes     disabled
  sbo      http://slackbuilds.org/slackbuilds/                 yes      enabled
  slack    http://mirrors.slackware.com/slackware/             yes      enabled
  slacke   http://ngc891.blogdns.net/pub/                      yes     disabled
  slackl   http://www.slackel.gr/repo/                         yes     disabled
  slacky   http://repository.slacky.eu/                        yes     disabled
  slonly   https://slackonly.com/pub/packages/                 yes     disabled

Repositories summary
===============================================================================
2/17 enabled default repositories and 0 custom.
For enable or disable default repositories edit '/etc/slpkg/repositories.conf'
file or run 'slpkg repo-enable' command.
{% endhighlight %}

![Docker slpkg: Repo List][image-ss-slpkg-repo-list]{: .img-responsive }

You can edit directly.

{% highlight bash %}
$ cat /etc/slpkg/repositories.conf
[REPOSITORIES]
slack
sbo
# alien
# rlw
...
{% endhighlight %}

![Docker slpkg: repositories.conf][image-ss-slpkg-etc-repo]{: .img-responsive }

#### Install:

{% highlight bash %}
$ slpkg -s sbo fish
Reading package lists... Done
Resolving dependencies... Done

The following packages will be automatically installed or upgraded 
with new version:

+==============================================================================
| Package                 New version        Arch    Build  Repos          Size
+==============================================================================
Installing:
  fish                    2.6.0              x86_64         SBo
Installing for dependencies:
  man-db                  2.7.6.1            x86_64         SBo

Installing summary
===============================================================================
Total 2 packages.
2 packages will be installed, 0 already installed and 0 package
will be upgraded.

Would you like to continue [y/N]? 
{% endhighlight %}

Unfortunately installation failed, we need to solve some dependency first.

{% highlight bash %}
$ slackpkg install gdbm
{% endhighlight %}

And install again. Do the installation command over again.

{% highlight bash %}
$ slpkg -s sbo fish
{% endhighlight %}

[![Docker slpkg: Install sbo fish][image-ss-slpkg-sbo-fish]{: .img-responsive }][photo-ss-slpkg-sbo-fish]

Mission accomplished, Now I can <code>fish</code>.

{% highlight bash %}
$ fish
Welcome to fish, the friendly interactive shell
root@662f86a36b6d ~# 
{% endhighlight %}

You are freely to try other package other than <code>fish</code>.

{% highlight bash %}
$ slpkg -s sbo ncdu
{% endhighlight %}

-- -- --

### Conclusion

It is said that "_Slackware does not resolve dependency_".
I haven't got enough experience about this situation.
I simply do not understand what it means.

	I guess I have to learn slackware more.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_flow = site.url | append: '/assets/posts/system/2017/08/docker-flow' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-slackware' %}

[slackware-sbopkg]: https://docs.slackware.com/howtos:slackware_admin:building_packages_with_sbopkg

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-slack-docker]:     {{ asset_post }}/00-start.png
[image-ss-slackware64]:      {{ asset_post }}/01-install-slackware64.png
[image-ss-file-search]:      {{ asset_post }}/01-file-search-groff.png
[image-ss-slackpkg-patches]: {{ asset_post }}/01-slackpkg-upgrade-patches.png
[image-ss-slackware64-huge]: {{ asset_flow }}/terminal-2.png

[image-ss-removepkg]:          {{ asset_post }}/13-removepkg.png
[image-ss-slackpkg-info]:      {{ asset_post }}/13-slackpkg-info.png
[image-ss-slackpkg-reinstall]: {{ asset_post }}/13-slackpkg-reinstall.png


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

[image-ss-slackpkg-python]:  {{ asset_post }}/27-slackpkg-install-python.png
[image-ss-slpkg-update-err]: {{ asset_post }}/27-slpkg-update-certificate-error.png
[image-ss-slackpkg-ca-cert]: {{ asset_post }}/27-slackpkg-install-ca-certificates.png
[image-ss-slpkg-update]:     {{ asset_post }}/27-slpkg-update.png

[image-ss-slpkg-repo-list]:  {{ asset_post }}/28-slpkg-repo-list.png
[image-ss-slpkg-etc-repo]:   {{ asset_post }}/28-slpkg-etc-repositories.png
[image-ss-slpkg-sbo-fish]:   {{ asset_post }}/28-slpkg-sbo-fish-half.png
[photo-ss-slpkg-sbo-fish]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP8DnPxKw_sjsW2jlR_LAjIBJiP2BcMjsDmHSyv?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

