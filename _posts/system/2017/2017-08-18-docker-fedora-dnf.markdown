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
  One of Two Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Topics

This is a two-parts article.
There are few topics here.

[ [Part One][local-part-one] ]

*	Preface: Test Bed

*	Getting Started With Docker

*	Package Management: RPM Frontend, Get Help, DNF Shell

*	Updating System: OS Release, Repository List, System Upgrade, Extra Commands

*	Package IRSI: Install, Removal, Query Search, Show Info

*	Dependency: Help, Dependency, Reverse Dependency, Test, Tree

*	Group: Group List, Group Info, Group Install, Beyond Group

*	What's Next

[ [Part Two][local-part-two] ]

*	Repositories: repolist, repoinfo, repo-pkgs, --enablerepo

*	Plugin: List, Install, Help, Config Manager Example

*	History: The Log File, DNF History

*	Clean Up

*	Conclusion

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

DNF comes, after RPM, Zypp, YUM, and URPMI.
Fedora container in Docker allow people to learn DNF more easily.
DNF is a Package Manager introduced by Fedora.

We need <code>Rawhide</code> rolling release with more often update,
so that we have a chance to play more with package.
No need to wait for another six month cycle.

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

### Dependency

There are two main topics in dependency,
_dependency_ itself, and _reverse dependency_.
Beside these two, there are other topic as well,
such as _managing conflict_ that we do not cover here.

#### Help

DNF has a <code>repoquery</code> help that show all dependency related options.

{% highlight bash %}
$ dnf help repoquery

search for packages matching keyword
...
  --whatconflicts REQ   show only results that conflict REQ
  --whatobsoletes REQ   show only results that obsolete REQ
  --whatprovides REQ    show only results that provide REQ
  --whatrequires REQ    shows results that requires package provides and files REQ
  --whatrecommends REQ  show only results that recommend REQ
  --whatenhances REQ    show only results that enhance REQ
  --whatsuggests REQ    show only results that suggest REQ
  --whatsupplements REQ
...
  --conflicts           Display capabilities that the package conflicts with.
  --enhances            Display capabilities that the package can enhance.
  --provides            Display capabilities provided by the package.
  --recommends          Display capabilities that the package recommends.
  --requires            Display capabilities that the package depends on.
  --requires-pre        Display capabilities that the package depends on for
                        running a %pre script.
  --suggests            Display capabilities that the package suggests.
{% endhighlight %}

![Docker DNF: Help Repoquery][image-ss-dnf-help-repoquery]{: .img-responsive }

#### Dependency

	Package that required by: such as man-db need less and other.

This _dependency_ information can be achieved by <code>repoquery --requires</code> command.
This will show required parts of the package.

{% highlight bash %}
$ dnf repoquery --requires man-db
Last metadata expiration check: 0:27:23 ago on Sat Aug 26 13:49:15 2017.
/bin/sh
coreutils
grep
groff-base
gzip
less
libc.so.6(GLIBC_2.17)(64bit)
libgdbm.so.4()(64bit)
libpipeline.so.1()(64bit)
libz.so.1()(64bit)
rtld(GNU_HASH)
{% endhighlight %}

![Docker DNF: Repoquery Requires][image-ss-dnf-requires]{: .img-responsive }

#### Reverse Dependency

	Package that require: such as less needed by man-db or other.

This _reverse dependency_ require <code>repoquery --whatrequires</code> command.

{% highlight bash %}
$ dnf repoquery --whatrequires less
Last metadata expiration check: 0:29:36 ago on Sat Aug 26 13:49:15 2017.
GMT-0:5.4.2-3.fc27.i686
GMT-0:5.4.2-3.fc27.x86_64
R-core-0:3.4.1-4.fc27.i686
R-core-0:3.4.1-4.fc27.x86_64
Singular-0:4.1.0p3-6.fc27.x86_64
backup-manager-0:0.7.10-22.fc27.noarch
c-graph-0:2.0-12.fc27.x86_64
colordiff-0:1.0.18-2.fc27.noarch
git-core-0:2.14.1-2.fc27.x86_64
libguestfs-1:1.37.21-2.fc28.i686
libguestfs-1:1.37.21-2.fc28.x86_64
libguestfs-tools-c-1:1.37.21-2.fc28.x86_64
man-db-0:2.7.6.1-5.fc27.x86_64
octave-6:4.2.1-4.fc27.2.i686
octave-6:4.2.1-4.fc27.2.x86_64
rpmreaper-0:0.2.0-13.fc27.x86_64
{% endhighlight %}

![Docker DNF: Repoquery What Requires][image-ss-dnf-whatrequires]{: .img-responsive }

#### Test

Removing <code>less</code> would remove <code>man-db</code>.
And also remove any unused dependency.

{% highlight bash %}
$ dnf remove less
Dependencies resolved.
====================================================================
 Package         Arch       Version              Repository    Size
====================================================================
Removing:
 less            x86_64     487-5.fc27           @rawhide     309 k
Removing depended packages:
 man-db          x86_64     2.7.6.1-5.fc27       @rawhide     1.9 M
Removing unused dependencies:
 groff-base      x86_64     1.22.3-11.fc27       @rawhide     3.7 M
 libpipeline     x86_64     1.4.2-3.fc27         @rawhide     106 k

Transaction Summary
====================================================================
Remove  3 Packages

Freed space: 6.0 M
Is this ok [y/N]: 
{% endhighlight %}

![Docker DNF: Test Dependency][image-ss-dnf-test-remove]{: .img-responsive }

#### Tree

	Most people love tree

This <code>rpmreaper</code> is an RPM tool rather than DNF tool.

{% highlight bash %}
$ dnf install rpmreaper
{% endhighlight %}

![Docker Fedora: rpmreaper][image-ss-fedora-rpmreaper]{: .img-responsive }

-- -- --

### Group

Is this docker Minimal Install ?
I always wonder what inside theis Fedora docker Container.
RPM <code>group</code> help me understand this riddle.

#### Group List

We can see <code>Minimal Install</code> group as below.

{% highlight bash %}
$ dnf grouplist
Last metadata expiration check: 2:12:21 ago on Wed Aug 23 11:50:45 2017.
Available Environment Groups:
   Fedora Custom Operating System
   Minimal Install
   Fedora Server Edition
   Fedora Workstation
   Fedora Cloud Server
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-list]{: .img-responsive }

#### Group Info

And there is this <code>Core</code> group as below.

{% highlight bash %}
$ dnf group info "Minimal Install"
Last metadata expiration check: 2:13:24 ago on Wed Aug 23 11:50:45 2017.
Environment Group: Minimal Install
 Description: Basic functionality.
 Mandatory Groups:
   Core
 Optional Groups:
   Guest Agents
   Standard
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-info1]{: .img-responsive }

And at the most bottom, there are only <code>Packages</code> as below.

{% highlight bash %}
$ dnf group info core
Last metadata expiration check: 2:14:07 ago on Wed Aug 23 11:50:45 2017.

Group: Core
 Description: Smallest possible installation
 Mandatory Packages:
   audit
   basesystem
   bash
   coreutils
   cronie
   curl
   dhcp-client
   dnf
   dnf-yum
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-info2]{: .img-responsive }

#### Group Install

Now we know, that not all the <code>Core</code> packages are installed,
as some packages not required by the container.

{% highlight bash %}
$ dnf group install core
Last metadata expiration check: 2:07:09 ago on Wed Aug 23 11:50:45 2017.
No match for group package "ppc64-utils"
Dependencies resolved.
=============================================================================
 Group     Packages                                                        
=============================================================================
Marking packages as installed by the group:
 @Core     curl                     dnf-plugins-core            dnf        
           sssd-common              policycoreutils             glibc      
           NetworkManager           openssh-clients             systemd    
           dracut-config-rescue     openssh-server              rootfiles  
           plymouth                 selinux-policy-targeted     dhcp-client
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-install]{: .img-responsive }

#### Beyond Group

You can even remove group of packages as you can read in manual,
or upgrade only for specific group.

-- -- --

### What's Next

These are just preliminary knowledge about DNF.
Consider finish reading [ [Part Two][local-part-two] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-fedora' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-one]: {{ site.url }}/system/2017/08/18/docker-fedora-dnf.html
[local-part-two]: {{ site.url }}/system/2017/08/19/docker-fedora-dnf.html

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

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

[image-ss-dnf-help-repoquery]: {{ asset_post }}/14-help-repoquery.png
[image-ss-dnf-requires]:       {{ asset_post }}/14-repoquery-requires.png
[image-ss-dnf-whatrequires]:   {{ asset_post }}/14-repoquery-whatrequires.png
[image-ss-dnf-test-remove]:    {{ asset_post }}/14-remove-less.png
[image-ss-fedora-rpmreaper]:   {{ asset_post }}/14-rpmreaper.png

[image-ss-dnf-g-info1]:   {{ asset_post }}/15-dnf-group-info-core.png
[image-ss-dnf-g-info2]:   {{ asset_post }}/15-dnf-group-info-min.png
[image-ss-dnf-g-install]: {{ asset_post }}/15-dnf-group-install-core.png
[image-ss-dnf-g-list]:    {{ asset_post }}/15-dnf-grouplist.png

[image-ss-dnf-r-info]:      {{ asset_post }}/16-repoinfo.png
[image-ss-dnf-r-list]:      {{ asset_post }}/16-repolist.png
[image-ss-dnf-r-list-all]:  {{ asset_post }}/16-repolist-all.png
[image-ss-dnf-r-pkgs-info]: {{ asset_post }}/16-repo-pkgs-info.png
[image-ss-dnf-r-pkgs-list]: {{ asset_post }}/16-repo-pkgs-list.png

[image-ss-dnf-cache]:     {{ asset_post }}/17-cache.png
[image-ss-dnf-clean]:     {{ asset_post }}/17-clean.png

[image-ss-var-log-dnf]:   {{ asset_post }}/19-log.png
[image-ss-dnf-history]:   {{ asset_post }}/19-history.png
