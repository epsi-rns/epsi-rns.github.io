---
layout: post
title: "Docker - Slackware Package - Part One"
date: 2017-08-21 13:15:35 +0700
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
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-slackware-package.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

I am so glad that finally, for the first time,
I can learn Slackware Package Management using Docker.
Here is my report as a _new slacker_ who just landed in slackware land,
my journey using minimal install.

{% include post/2017/08/docker-test-bed.md %}
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

### Container: Minimal or Full Install ?

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
So do not have any plan to install <code>D Package Series</code>.
If storage is not a problem for you,
you can install <code>D Package Series</code>,
or even use full <code>Slackware64</code> install.

-- -- --

### Mirror

Change mirror is slackware can be done,
by changing the /etc/slackpkg/mirrors.
Here I uncomment local university named kambing.

{% highlight bash %}
$ cat /etc/slackpkg/mirrors
...
# INDONESIA (ID)
http://kambing.ui.ac.id/slackware/slackware64-14.2/
...
{% endhighlight %}

![Docker Slackware: Change Mirrors][image-ss-slackpkg-mirrors]{: .img-responsive }

The result will shown when update.

{% highlight bash %}
$ slackpkg update

Updating the package lists...
	Downloading...
			Downloading http://kambing.ui.ac.id/slackware/slackware64-14.2/ChangeLog.txt...
--2017-09-09 21:53:58--  http://kambing.ui.ac.id/slackware/slackware64-14.2/ChangeLog.txt
{% endhighlight %}

![Docker Slackware: Update Use Kambing][image-ss-slackpkg-kambing]{: .img-responsive }

The repository is already using kambing.

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.
For that reason, I put the recorded event here, 
before discussing about any further feature.

Unfortunately, I cannot find any reference about slackpkg log file.
However, there is are some log file for

*	sbopkg:	/var/log/sbopkg/sbopkg-build-log

*	slpkg:	 /var/log/slpkg/sbo/build_logs/build_fish_log

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

-- -- --

### No Dependency

Slackware proud of their **no dependency resolution** philosophy.
A package that required by other packaged could be removed without prior warning.

#### Example

Removing <code>libpipeline</code> would **not** remove <code>man-db</code>.
<code>libpipeline</code> would be removed without prior warning.
Therefore be careful while remove.

{% highlight bash %}
$ removepkg groff

Removing package /var/log/packages/groff-1.22.3-x86_64-2...
Removing files:
  --> Deleting symlink /usr/bin/geqn
  --> Deleting symlink /usr/bin/gindxbib
  --> Deleting symlink /usr/bin/glookbib
...
{% endhighlight %}

![Docker Slackware: remove groff][image-ss-remove-groff]{: .img-responsive }

{% highlight bash %}
$ man man
sh: /usr/bin/gtbl: No such file or directory
sh: /usr/bin/nroff: No such file or directory
{% endhighlight %}

![Docker Slackware: man man][image-ss-man-man]{: .img-responsive }

The same result for <code>slapt-get --remove</code>.

{% highlight bash %}
slapt-get --remove groff
{% endhighlight %}

#### Using slpkg deps-status

However, you can use <code>slpkg</code>,
and achieve dependency resolution.

{% highlight bash %}
$ slpkg deps-status --tree
+==============================================================================
| Dependencies
| -- Packages
+==============================================================================
+ man-db
|
+-- fish
 

Status summary
===============================================================================
found 1 dependencies in 1 packages.
{% endhighlight %}

![Docker Slackware: slpkg deps-status][image-ss-slpkg-deps-status]{: .img-responsive }

-- -- --

### Hold 

Holding package ini Slackware
can be done using <code>/etc/slackpkg/blacklist</code>.

Consider this <code>install-new</code> command.
This will install a lot of package,
and we want to reduce he number of package by blacklisting.

{% highlight bash %}
$ slackpkg install-new

Looking for NEW packages to install. Please wait... DONE

ConsoleKit2-1.0.0-x86_64-3.txz
Cython-0.23.4-x86_64-1.txz
LibRaw-0.17.2-x86_64-1.txz
a52dec-0.7.4-x86_64-2.txz
adwaita-icon-theme-3.18.0-noarch-1.txz
alsa-plugins-1.1.1-x86_64-1.txz
amor-4.14.3-x86_64-2.txz
artikulate-4.14.3-x86_64-2.txz
atkmm-2.24.2-x86_64-1.txz
baloo-4.14.3-x86_64-2.txz
baloo-widgets-4.14.3-x86_64-2.txz
cairomm-1.12.0-x86_64-1.txz
calligra-l10n-en_GB-2.9.11-noarch-1.txz
calligra-l10n-ja-2.9.11-noarch-1.txz
calligra-l10n-tr-2.9.11-noarch-1.txz
cervisia-4.14.3-x86_64-2.txz
cgmanager-0.39-x86_64-1.txz
cups-filters-1.9.0-x86_64-2.txz
dconf-editor-3.18.2-x86_64-1.txz
{% endhighlight %}

![Docker Slackpkg: unlock example][image-ss-install-new-unlock]{: .img-responsive }

We can reduce by blacklist some few package

#### Blacklist

This is just an example.
Blacklisting <code>lib</code> is a bad idea.

{% highlight bash %}
$ cat /etc/slackpkg/blacklist
...
calligra
kde
gst
{% endhighlight %}

![Docker Slackpkg: Blacklist][image-ss-slackpkg-blacklist]{: .img-responsive }

Now we have fewer packages.

{% highlight bash %}
$ slackpkg install-new

Looking for NEW packages to install. Please wait... DONE

ConsoleKit2-1.0.0-x86_64-3.txz
Cython-0.23.4-x86_64-1.txz
LibRaw-0.17.2-x86_64-1.txz
a52dec-0.7.4-x86_64-2.txz
adwaita-icon-theme-3.18.0-noarch-1.txz
alsa-plugins-1.1.1-x86_64-1.txz
atkmm-2.24.2-x86_64-1.txz
cairomm-1.12.0-x86_64-1.txz
cgmanager-0.39-x86_64-1.txz
cups-filters-1.9.0-x86_64-2.txz
dconf-editor-3.18.2-x86_64-1.txz
dri3proto-1.0-x86_64-2.txz
eigen3-3.2.7-x86_64-2.txz
elfutils-0.163-x86_64-1.txz
eudev-3.1.5-x86_64-8.txz
glibmm-2.46.4-x86_64-1.txz
gnu-cobol-1.1-x86_64-1.txz
gparted-0.26.1-x86_64-1.txz
{% endhighlight %}

![Docker Slackpkg: lock example][image-ss-install-new-lock]{: .img-responsive }

This is just an example.
I do not have any intention to install anything.

-- -- --

### Clean Up

Time after time, your cache size may growing bigger and bigger.

Package Cache
	
*	/var/cache/packages/ * / * / * .txz

{% highlight bash %}
$ ls -lR /var/cache/packages/slackware64/d
{% endhighlight %}

![Docker Slack: Cache][image-ss-slack-cache]{: .img-responsive }

Unfortunately, I haven't find any reference,
on how to clean up this cache directory.

-- -- --

### What's Next

Consider finish reading [ [Part Two][local-part-two] ]
that cover <code>slackbuild</code>, <code>sbopkg</code>,
<code>slapt-get</code> and <code>slapt-src</code>.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_flow = site.url | append: '/assets/posts/system/2017/08/docker-flow' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-slackware' %}

[slackware-sbopkg]: https://docs.slackware.com/howtos:slackware_admin:building_packages_with_sbopkg

[local-part-two]:   {{ site.url }}/system/2017/08/22/docker-slackware-package.html

[image-ss-slack-docker]:     {{ asset_post }}/00-getting-started.png
[image-ss-slackware64]:      {{ asset_post }}/01-install-slackware64.png
[image-ss-file-search]:      {{ asset_post }}/01-file-search-groff.png
[image-ss-slackpkg-patches]: {{ asset_post }}/01-slackpkg-upgrade-patches.png
[image-ss-slackware64-huge]: {{ asset_flow }}/terminal-2.png

[image-ss-removepkg]:          {{ asset_post }}/13-removepkg.png
[image-ss-slackpkg-info]:      {{ asset_post }}/13-slackpkg-info.png
[image-ss-slackpkg-reinstall]: {{ asset_post }}/13-slackpkg-reinstall.png

[image-ss-man-man]:            {{ asset_post }}/14-man-man.png
[image-ss-remove-groff]:       {{ asset_post }}/14-remove-groff.png
[image-ss-slpkg-deps-status]:  {{ asset_post }}/14-slpkg-deps-status.png

[image-ss-slack-cache]:        {{ asset_post }}/17-cache.png

[image-ss-slackpkg-mirrors]:   {{ asset_post }}/26-etc-slackpkg-mirrors.png
[image-ss-slackpkg-kambing]:   {{ asset_post }}/26-slackpkg-kambing.png

[image-ss-slackpkg-blacklist]: {{ asset_post }}/27-blacklist.png
[image-ss-install-new-lock]:   {{ asset_post }}/27-install-new-lock.png
[image-ss-install-new-unlock]: {{ asset_post }}/27-install-new-unlock.png
