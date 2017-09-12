---
layout: post
title: "Docker - Fedora DNF - Part One"
date: 2017-08-18 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, fedora]
author: epsi

excerpt:
  Examine DNF step by step,
  using Fedora container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082215  # Debian Portage
  - 17082015  # Slackware Package
# - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-fedora-dnf.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

DNF comes, after RPM, Zypp, YUM, and URPMI.
Fedora container in Docker allow people to learn DNF more easily.
DNF is a Package Manager introduced by Fedora.

We need <code>Rawhide</code> rolling release with more often update,
so that we have a chance to play more with package.
No need to wait for another six month cycle.

{% include post/2017/08/docker-test-bed.md %}

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull fedora:rawhide
{% endhighlight %}

![Docker Pull Fedora Rawhide][image-ss-pull-fedora]{: .img-responsive }

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
{% endhighlight %}

{% highlight bash %}
$ docker run -it fedora:rawhide bash
[root@6f45f67e56ea /]# exit
{% endhighlight %}

{% highlight bash %}
$ docker ps -a 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE                       NAMES                  STATUS
opensuse/amd64:tumbleweed   elegant_nightingale   Exited (0) 5 hours ago
debian:stretch              dazzling_neumann      Exited (0) 5 days ago
dock0/arch                  silly_leakey          Exited (1) 5 days ago
fedora:rawhide              musing_torvalds       Up 4 hours
{% endhighlight %}

{% highlight bash %}
$ docker start musing_torvalds
musing_torvalds
{% endhighlight %}

{% highlight bash %}
$ docker attach musing_torvalds
[root@5f1ee8964269 /]# 
{% endhighlight %}

![Docker Fedora: Getting Started][image-ss-fedora-docker]{: .img-responsive }

-- -- --

### Package Management

#### RPM Frontend

DNF (Dandified YUM): Fedora: Python

*	<https://github.com/rpm-software-management/dnf>

*	<http://dnf.readthedocs.io/en/latest/command_ref.html>

*	<https://en.wikipedia.org/wiki/DNF_(software)>
	
#### Get Help

Read the fine manual. 

{% highlight bash %}
$ man dnf
{% endhighlight %}

Since we do need have <code>man-db</code> nor <code>less</code> yet.
This is what we can do.

{% highlight bash %}
$ dnf help | more
{% endhighlight %}

	I cannot even be 'help | less'.

#### DNF Shell

Most modern package manager has shell feature,
that enable user to focus on the task of managing package.

{% highlight bash %}
$ dnf shell
Fedora - Rawhide - Developmental pa 168 kB/s |  66 MB     06:38    
Last metadata expiration check: 0:13:32 ago on Thu Aug 24 04:58:06 2017.
> clean packages
0 files removed
> clean all
9 files removed
> 
{% endhighlight %}

![Docker Fedora: DNFShell][image-ss-dnf-shell]{: .img-responsive }

-- -- -- 

### Updating System

	First Thing First

First thing to do is updating my system as usual.

*	OS Release

*	Repository List

*	Upgrade

*	Some Extra Command

#### OS Release

{% highlight bash %}
$ cat /etc/fedora-release 
Fedora release 27 (Rawhide)
{% endhighlight %}

#### Repository List

This Fedora container size is only 232MB,
but the <code>$ dnf repolist</code> command is,
ridiculously required to download 66MB.

{% highlight bash %}
$ dnf repolist
Fedora - Rawhide - Developmental packages fo 115 kB/s |  66 MB     09:43    
Last metadata expiration check: 0:04:32 ago on Wed Aug 23 10:54:25 2017.
repo id  repo name                                                     status
*rawhide Fedora - Rawhide - Developmental packages for the next Fedora 61406
[root@5f1ee8964269 /]# 
{% endhighlight %}

![Docker DNF: Repository List][image-ss-dnf-repolist]{: .img-responsive }

#### System Upgrade

<code>update</code> is deprecated.

{% highlight bash %}
$ dnf update
{% endhighlight %}

Now <code>update</code> is just an alias for <code>upgrade</code>.

{% highlight bash %}
$ dnf upgrade --nogpgcheck
Last metadata expiration check: 0:16:45 ago on Wed Aug 23 10:54:25 2017.
Dependencies resolved.
====================================================================
 Package            Arch   Version                    Repository
                                                               Size
====================================================================
Upgrading:
 acl                x86_64 2.2.52-18.fc27             rawhide  78 k
 audit-libs         x86_64 2.7.7-5.fc27               rawhide 108 k
 basesystem         noarch 11-4.fc27                  rawhide 8.9 k
 bash               x86_64 4.4.12-9.fc27              rawhide 1.5 M
 bzip2-libs         x86_64 1.0.6-24.fc27              rawhide  46 k
 ca-certificates    noarch 2017.2.16-4.fc27           rawhide 452 k
 chkconfig          x86_64 1.10-3.fc27                rawhide 186 k
 coreutils          x86_64 8.27-15.fc28               rawhide 1.2 M
 coreutils-common   x86_64 8.27-15.fc28               rawhide 1.9 M
 cracklib           x86_64 2.9.6-9.fc28               rawhide  87 k
{% endhighlight %}

This is a long process with a lot of text,
that I have to put the output into four figures.

![Docker DNF: Upgrade][image-ss-dnf-upgrade-1]{: .img-responsive }

![Docker DNF: Upgrade][image-ss-dnf-upgrade-2]{: .img-responsive }

![Docker DNF: Upgrade][image-ss-dnf-upgrade-3]{: .img-responsive }

![Docker DNF: Upgrade][image-ss-dnf-upgrade-4]{: .img-responsive }

#### Extra Commands

Common clean up procedures.
Actual we will discuss each later.
Now I just feel the need to summarized the sequence here.

{% highlight bash %}
$ dnf clean packages
171 files removed
{% endhighlight %}

{% highlight bash %}
$ dnf clean all
9 files removed
{% endhighlight %}

{% highlight bash %}
$ dnf repoquery --unsatisfied
Fedora - Rawhide - Developmental pa 227 kB/s |  66 MB     04:56    
Last metadata expiration check: 0:03:58 ago on Wed Aug 23 11:50:45 2017.
{% endhighlight %}

{% highlight bash %}
$ dnf repoquery --duplicated
Last metadata expiration check: 0:08:58 ago on Wed Aug 23 11:50:45 2017.
{% endhighlight %}

{% highlight bash %}
$ dnf list extras
Last metadata expiration check: 0:07:14 ago on Wed Aug 23 11:50:45 2017.
{% endhighlight %}

{% highlight bash %}
$ sudo dnf autoremove
Last metadata expiration check: 0:11:24 ago on Wed Aug 23 11:50:45 2017.
Dependencies resolved.
Nothing to do.
{% endhighlight %}

![Docker DNF: Extra Commands][image-ss-dnf-extra-cmd]{: .img-responsive }

-- -- --

### Package IRSI

	Install, Remove, Search, Info

Read the fine manual. Helpless or help more. 
We need to install <code>man</code>.

#### Package Install

	I demand to read, manual pages sir!

{% highlight bash %}
$ dnf install man 
Last metadata expiration check: 0:58:52 ago on Wed Aug 23 11:50:45 2017.
Dependencies resolved.
====================================================================
 Package         Arch       Version               Repository   Size
====================================================================
Installing:
 man-db          x86_64     2.7.6.1-5.fc27        rawhide     881 k
Installing dependencies:
 groff-base      x86_64     1.22.3-11.fc27        rawhide     1.0 M
 less            x86_64     487-5.fc27            rawhide     159 k
 libpipeline     x86_64     1.4.2-3.fc27          rawhide      52 k
 libstdc++       x86_64     7.1.1-7.fc27.1        rawhide     461 k

Transaction Summary
====================================================================
Install  5 Packages

Total download size: 2.5 M
Installed size: 7.9 M
Is this ok [y/N]: 
{% endhighlight %}

Note that I accidentaly misstype <code>man</code> in command line,
rather than <code>man-db</code>. 

![Docker DNF: Package Install][image-ss-dnf-install]{: .img-responsive }

You can <code>reinstall</code>.

{% highlight bash %}
$ dnf reinstall man-db
{% endhighlight %}

And you can install many packages at once too.

{% highlight bash %}
$ dnf install man-db nano htop ncdu fish
{% endhighlight %}

#### Package Removal

{% highlight bash %}
$ dnf remove less
Dependencies resolved.
=============================================================================
 Package           Arch         Version                 Repository      Size
=============================================================================
Removing:
 less              x86_64       487-5.fc27              @rawhide       309 k
Removing depended packages:
 man-db            x86_64       2.7.6.1-5.fc27          @rawhide       1.9 M
Removing unused dependencies:
 groff-base        x86_64       1.22.3-11.fc27          @rawhide       3.7 M
 libpipeline       x86_64       1.4.2-3.fc27            @rawhide       106 k

Transaction Summary
=============================================================================
Remove  3 Packages

Freed space: 6.0 M
Is this ok [y/N]: 
{% endhighlight %}

![Docker DNF: Package Remove][image-ss-dnf-remove]{: .img-responsive }

#### Package Query Search

{% highlight bash %}
$ dnf search nano
Last metadata expiration check: 1:22:32 ago on Wed Aug 23 11:50:45 2017.
======================== Name Exactly Matched: nano =========================
nano.x86_64 : A small text editor
======================= Name & Summary Matched: nano ========================
protobuf-javanano.noarch : Protocol Buffer JavaNano API
nano-debugsource.x86_64 : Debug sources for package nano
============================ Name Matched: nano =============================
nodejs-nano.noarch : Minimalistic couchdb driver for Node.js
=========================== Summary Matched: nano ===========================
perl-Time-Clock.noarch : Twenty-four hour clock object with nanosecond
                       : precision
novprog.x86_64 : Tool to graph your progress in writing a NaNoWriMo style
               : novel
{% endhighlight %}

![Docker DNF: Package Query Search][image-ss-dnf-search]{: .img-responsive }

#### Package Show Info

{% highlight bash %}
$ dnf info man-db
Last metadata expiration check: 1:21:47 ago on Wed Aug 23 11:50:45 2017.
Installed Packages
Name         : man-db
Version      : 2.7.6.1
Release      : 5.fc27
Arch         : x86_64
Size         : 1.9 M
Source       : man-db-2.7.6.1-5.fc27.src.rpm
Repo         : @System
From repo    : rawhide
Summary      : Tools for searching and reading man pages
URL          : http://www.nongnu.org/man-db/
License      : GPLv2+ and GPLv3+
Description  : The man-db package includes five tools for browsing man-pages:
             : man, whatis, apropos, manpath and lexgrog. man formats and
             : displays manual pages. whatis searches the manual page names.
             : apropos searches the manual page names and descriptions.
             : manpath determines search path for manual pages. lexgrog
             : directly reads header information in manual pages.
{% endhighlight %}

![Docker DNF: Package Show Info][image-ss-dnf-info]{: .img-responsive }

-- -- --

### What's Next

These are just preliminary knowledge about DNF.
Consider finish reading [ [Part Two][local-part-two] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-fedora' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/19/docker-fedora-dnf.html

[image-ss-pull-fedora]:   {{ asset_pull }}/fedora-rawhide.png
[image-ss-fedora-docker]: {{ asset_post }}/00-getting-started.png
[image-ss-dnf-shell]:     {{ asset_post }}/01-dnf-shell.png

[image-ss-dnf-repolist]:  {{ asset_post }}/02-dnf-repolist.png
[image-ss-dnf-upgrade-1]: {{ asset_post }}/02-dnf-upgrade-1.png
[image-ss-dnf-upgrade-2]: {{ asset_post }}/02-dnf-upgrade-2.png
[image-ss-dnf-upgrade-3]: {{ asset_post }}/02-dnf-upgrade-3.png
[image-ss-dnf-upgrade-4]: {{ asset_post }}/02-dnf-upgrade-4.png
[image-ss-dnf-extra-cmd]: {{ asset_post }}/03-dnf-extra-commands.png

[image-ss-dnf-info]:      {{ asset_post }}/13-dnf-info.png
[image-ss-dnf-install]:   {{ asset_post }}/13-dnf-install.png
[image-ss-dnf-remove]:    {{ asset_post }}/13-dnf-remove.png
[image-ss-dnf-search]:    {{ asset_post }}/13-dnf-search.png


