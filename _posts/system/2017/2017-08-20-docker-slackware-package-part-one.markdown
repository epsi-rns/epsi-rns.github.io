---
layout: post
title: "Docker - Slackware Package - Part One"
date: 2017-08-20 09:45:15 +0700
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

### Topics

This is a two-parts article.
There are few topics here.

*	[ [Part One][local-part-one] ] Preface

*	[ [Part One][local-part-one] ] Preparing Docker

*	[ [Part One][local-part-one] ] IRSI: slackpkg install, removepkg, search, info

*	[ [Part One][local-part-one] ] Using slackbuild, manual compilation, and installpkg

*	[ [Part One][local-part-one] ] Using sbopkg, automatic compilation

*	[ [Part Two][local-part-two] ] Using slapt-get and slapt-src

*	[ [Part Two][local-part-two] ] Using slpkg

*	[ [Part Two][local-part-two] ] Conclusion

There are still other topic uncovered here

*	slackpkgplus

*	adding non official repo such as alien and slacky

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

I am so glad that finally, for the first time,
I can learn Slackware Package Management using Docker.
Here is my report as a _new slacker_ who just landed in slackware land,
my journey using minimal install.

You can read a common overview about docker here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]

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
sh-4.3# 
{% endhighlight %}

![Docker Slackware: Getting Started][image-ss-slack-docker]{: .img-responsive }

-- -- --

### Package Management

Slackware does not really have a sophisticated official package management.
Beside <code>slackpkg</code> there are special unoffical tools 
to manage your packages such as <code>sbopkg</code>,
<code>slapt-get</code>, <code>slapt-src</code>,
<code>slapkg</code> and <code>slackpkgplus</code>.

slackpkg

*	<https://docs.slackware.com/slackware:slackpkg>
	
	Package Cache
	
	*	/var/cache/packages/*/*/.txz

slapt-get

*	<https://github.com/jaos/slapt-get>

slpkg

*	<https://github.com/dslackw/slpkg>

#### Get Help

Read the fine manual.
Helpless or more.

{% highlight bash %}
$ slackpkg help | less
{% endhighlight %}

{% highlight bash %}
$ man sbopkg

$ man slapt-get
$ man slapt-src

$ man slpkg
{% endhighlight %}

-- -- --

### slackpkg

#### OS Release

{% highlight bash %}
$ cat /etc/slackware-version 
Slackware 14.2
{% endhighlight %}

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

#### Package Removal

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

### What's Next

Consider finish reading [ [Part Two][local-part-two] ]
that discuss <code>slapt-get</code>, <code>slapt-src</code>,
and <code>slpkg</code>.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_flow = site.url | append: '/assets/posts/system/2017/08/docker-flow' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-slackware' %}

[local-part-one]: {{ site.url }}/system/2017/08/20/docker-slackware-part-one.html
[local-part-two]: {{ site.url }}/system/2017/08/21/docker-slackware-part-two.html

[slackware-sbopkg]: https://docs.slackware.com/howtos:slackware_admin:building_packages_with_sbopkg

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-slack-docker]:     {{ asset_post }}/00-getting-started.png
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

