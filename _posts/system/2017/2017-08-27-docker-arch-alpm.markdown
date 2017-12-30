---
layout: post
title: "Docker - Arch ALPM - Part One"
date: 2017-08-27 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine ALPM step by step,
  using Debian container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
# - 17082715  # Arch ALPM
  - 17082415  # Debian Portage
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-arch-alpm.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

We have this ALPM, The **Arch Linux Package Management**,
as a base for <code>pacman</code> binary package frontend,
and source package tools such as the famous <code>AUR</code>,
and <code>ASP</code> (formerly <code>ABS</code>),
or even the lower level <code>makepkg</code> tool.
They are all under ALPM umbrella.

Using Arch Linux minimal install in Docker,
is a good way to learn ALPM.
ALPM is considered a basic knowledge
utilized by Arch's based distribution
such as Manjaro, Antergos, Artix,
and some other cool Arch's influenced distribution such as KaOS.
In short, any distribution using <code>pacman</code>

Personally I have been an Arch user since 2014,
I have used so many pacman command, but never really understand.
Reading pacman manual again makes me realize that I know nothing.
That is why I have a need to write down this blog article.

{% include post/2017/08/docker-test-bed.md %}

#### Must Read

You are encouraged to read the holy __archwiki__ first,
before even starting to read this article.

*	<https://wiki.archlinux.org/index.php/pacman>

There are already so many references for this pacman on the internet.
This blog article is just another reference.

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull dock0/arch
{% endhighlight %}

![Docker Pull Arch][image-ss-pull-arch]{: .img-responsive }

{% highlight bash %}
$ docker image list  
{% raw %}
  --filter "reference=*/arch:*"
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY          SIZE
dock0/arch          870MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it dock0/arch
root@f77ea94688d1:/# exit
exit
{% endhighlight %}

![Docker Arch: Running Docker][image-ss-running-arch]{: .img-responsive }

{% highlight bash %}
$ docker ps 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE               NAMES               STATUS
dock0/arch          musing_pike         Up About a minute
{% endhighlight %}

![Docker Arch: Docker ps][image-ss-docker-ps]{: .img-responsive }

{% highlight bash %}
$ docker start musing_pike
musing_pike
{% endhighlight %}

{% highlight bash %}
$ docker attach musing_pike
[root@37f48ad6a711 /]# 
{% endhighlight %}

![Docker Arch: Getting Started][image-ss-getting-started]{: .img-responsive }

-- -- --

### Package Management

Arch Linux use <code class="code-file">.pkg.tar.xz</code> extension for package.

#### ALPM Frontend, ASP and AUR Helper

ALPM = "Arch Linux Package Management"

Note that ABS tools is deprecated.

*	<https://wiki.archlinux.org/index.php/Arch_Build_System>

Pacman: C

*	<https://wiki.archlinux.org/index.php/pacman>

*	<https://wiki.archlinux.org/index.php/Pacman/Rosetta>

*	<https://github.com/andrewgregory/pacman/>
	
	
ASP: BASH

*	<https://github.com/falconindy/asp>

Cower: C

*	<https://github.com/falconindy/cower>

Package Query: C

*	<https://github.com/archlinuxfr/package-query>

AURA: Haskell

*	<https://github.com/aurapm/aura>

Pacaur: BASH/C

*	<https://github.com/rmarquis/pacaur>

Packer: BASH

*	<https://github.com/keenerd/packer>

Yaourt: BASH/C

*	<https://github.com/archlinuxfr/yaourt>

#### Get Help

Read the fine manual. 

This one show nothing.

{% highlight bash %}
$ pacman --help
{% endhighlight %}

And this one show a lot.

{% highlight bash %}
$ man help
{% endhighlight %}

Note that with minimal install in docker,
we do not have <code>man-db</code> and <code>less</code> yet.

-- -- -- 

### Updating System

	First Thing First

First thing to do is updating my system as usual.

#### OS Release

Make sure it is Arch that we are using.

There is <code>/etc/os-release </code> in Manjaro and KaOS,
But Arch utilized an empty file <code>/etc/arch-release</code>
to indicate that this is an arch distribution.
I guess since Arch is rolling release,
there is no need for release number.

{% highlight bash %}
$ cat /etc/arch-release 
{% endhighlight %}

Other Arch based may have different file,
such as <code>/etc/artix-release </code>.

#### The Riddle

What is <code>pacman -Syu</code> ?
And What is <code>pacman -Syyuu</code> anyway ?
Of course there is <code>man pacman</code> that explain it all.
After reading the manual we can have something like 
<code>pacman --sync --refresh --quiet --sysupgrade --force</code>.

We can separate the <code>pacman -Syu</code> into few smaller steps.

*	Update

*	List Upgradable

*	Download

*	Upgrade

*	Common Form

#### Refresh Update

{% highlight bash %}
$ pacman -Sy
:: Synchronizing package databases...
 core                     124.4 KiB   138K/s 00:01 [##########] 100%
 extra                   1651.5 KiB   109K/s 00:15 [##########] 100%
 community                  4.0 MiB  55.4K/s 01:14 [##########] 100%
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --refresh
:: Synchronizing package databases...
 core is up to date
 extra is up to date
 community is up to date
{% endhighlight %}

![Docker pacman: refresh][image-ss-pm-refresh]{: .img-responsive }

From the manual " _Passing two --refresh or -y flags will force a
refresh of all package databases, even if they appear to be up-to-date._ "

{% highlight bash %}
$ pacman -Syy
{% endhighlight %}

#### List Upgradable

{% highlight bash %}
$ pacman -Qu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --upgrades
archlinux-keyring 20170611-1 -> 20170823-1
coreutils 8.27-1 -> 8.28-1
curl 7.55.1-1 -> 7.55.1-2
e2fsprogs 1.43.5-1 -> 1.43.6-1
expat 2.2.3-1 -> 2.2.4-1
gcc-libs 7.1.1-4 -> 7.2.0-2
glibc 2.25-7 -> 2.26-3
gnupg 2.1.23-1 -> 2.2.0-1
gnutls 3.5.14-1 -> 3.5.15-1
iana-etc 20170512-1 -> 20170824-1
icu 59.1-1 -> 59.1-2
iproute2 4.12.0-2 -> 4.13.0-1
libgcrypt 1.8.0-1 -> 1.8.1-1
libldap 2.4.44-5 -> 2.4.45-4
libpsl 0.17.0-2 -> 0.18.0-1
linux-api-headers 4.10.1-1 -> 4.12.7-1
lz4 1:1.7.5-1 -> 1:1.8.0-1
ncurses 6.0+20170527-1 -> 6.0+20170902-1
p11-kit 0.23.7-1 -> 0.23.8-1
pacman-mirrorlist 20170729-1 -> 20170907-1
pcre2 10.23-1 -> 10.30-1
perl 5.26.0-1 -> 5.26.0-4
perl-error 0.17025-1 -> 0.17025-2
sqlite 3.20.0-1 -> 3.20.1-1
{% endhighlight %}

![Docker pacman: upgradable][image-ss-pm-upgradable]{: .img-responsive }

#### Download Upgrade

{% highlight bash %}
$ pacman -Suw
{% endhighlight %}

{% highlight bash %}
$ pacman --sync --sysupgrade --downloadonly
:: Starting full system upgrade...
resolving dependencies...

Packages (24) archlinux-keyring-20170823-1  coreutils-8.28-1
              curl-7.55.1-2  e2fsprogs-1.43.6-1  expat-2.2.4-1
              gcc-libs-7.2.0-2  glibc-2.26-3  gnupg-2.2.0-1
              gnutls-3.5.15-1  iana-etc-20170824-1  icu-59.1-2
              iproute2-4.13.0-1  libgcrypt-1.8.1-1
              libldap-2.4.45-4  libpsl-0.18.0-1
              linux-api-headers-4.12.7-1  lz4-1:1.8.0-1
              ncurses-6.0+20170902-1  p11-kit-0.23.8-1
              pacman-mirrorlist-20170907-1  pcre2-10.30-1
              perl-5.26.0-4  perl-error-0.17025-2  sqlite-3.20.1-1

Total Download Size:  63.59 MiB

:: Proceed with download? [Y/n]
{% endhighlight %}

![Docker pacman: download][image-ss-pm-download]{: .img-responsive }

You may add <code>--noconfirm</code> if you wish.
this one not asking for PGP confirmation anymore.

![Docker pacman: confirm][image-ss-pm-noconfirm]{: .img-responsive }

#### Upgrade

This command will install package, and download if necessary.
This is a process with long verbose output,
I have to split out to three figures.

This is why the first figure output looks pretty similar
with the download command above since it works on the same upgradable packages.
But the next figure is different.

{% highlight bash %}
$ pacman -Su
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --sysupgrade
{% endhighlight %}

![Docker pacman: sysupgrade][image-ss-pm-sysupgrade]{: .img-responsive }

#### Common Form

Consider put all them command above together.
You can combine update dan upgrade in single command.
I usually only use this single command.

{% highlight bash %}
$ pacman -Syu
:: Synchronizing package databases...
 core is up to date
 extra is up to date
 community is up to date
:: Starting full system upgrade...
 there is nothing to do
{% endhighlight %}

![Docker pacman: -Syu][image-ss-pacman-syu]{: .img-responsive }

-- -- --

### Package IRSIF

	Install, Remove, Search, Info, File

#### Package Install

Installing in Arch is just <code>--sync</code> with target.
Consider our favorite example package below.
This will also have a verbose long output.

{% highlight bash %}
$ pacman -S man-db nano less sudo
{% endhighlight %}

Or other favorites package.

{% highlight bash %}
$ pacman --sync man-db nano less sudo         
warning: less-487-1 is up to date -- reinstalling
resolving dependencies...
looking for conflicting packages...

Packages (7) file-5.32-1  groff-1.22.3-7  libpipeline-1.4.2-1
             less-487-1  man-db-2.7.6.1-2  nano-2.8.7-1
             sudo-1.8.21.p2-1

Total Download Size:    4.27 MiB
Total Installed Size:  21.87 MiB
Net Upgrade Size:      21.67 MiB

:: Proceed with installation? [Y/n]
{% endhighlight %} 

![Docker pacman: sync target][image-ss-pm-install]{: .img-responsive }

You can also download, and install later, with color

{% highlight bash %}
$ pacman -Sw wget curl --color always
...
Packages (2) curl-7.55.1-2  wget-1.19.1-2

Total Download Size:  0.57 MiB
...
{% endhighlight %}

{% highlight bash %}
$ pacman -S wget curl --color always
...
Packages (2) curl-7.55.1-2  wget-1.19.1-2

Total Installed Size:  4.02 MiB
Net Upgrade Size:      2.59 MiB
...
{% endhighlight %}

And you can also combine with system upgrade.

{% highlight bash %}
$ pacman -Syu htop ncdu fish vim
:: Synchronizing package databases...
 core is up to date
 extra                   1651.4 KiB   263K/s 00:06 [##########] 100%
 community                  4.0 MiB   190K/s 00:22 [##########] 100%
:: Starting full system upgrade...
resolving dependencies...
looking for conflicting packages...

Packages (8) bc-1.07.1-1  gpm-1.20.7-8  inetutils-1.9.4-5
             vim-runtime-8.0.1092-1  fish-2.6.0-1  htop-2.0.2-2
             ncdu-1.12-1  vim-8.0.1092-1

Total Download Size:    8.52 MiB
Total Installed Size:  39.99 MiB

:: Proceed with installation? [Y/n]
{% endhighlight %}

![Docker pacman: sysupgrade target][image-ss-pm-sync-target]{: .img-responsive }

#### Package Removal

We many cases to be shown here.

Consider this <code>fish</code>,
no package has any dependency to this friendly water creature.

{% highlight bash %}
$ pacman -R fish
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --remove fish
checking dependencies...

Packages (1) fish-2.6.0-1

Total Removed Size:  8.15 MiB

:: Do you want to remove these packages? [Y/n]
{% endhighlight %}

We can also remove any its dependency as well
by using <code>--recursive</code> option.

{% highlight bash %}
$ pacman -Rs fish
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --remove --recursive fish
checking dependencies...

Packages (3) bc-1.07.1-1  inetutils-1.9.4-5  fish-2.6.0-1

Total Removed Size:  9.35 MiB

:: Do you want to remove these packages? [Y/n]
{% endhighlight %}

![Docker pacman: Remove Recursive][image-ss-pm-rm-recursive]{: .img-responsive }

Consider this <code>groff</code> case,
that is dependency of <code>man-db</code>.
Have a look at how ALPM manage dependency,
this command will send error notification message.

{% highlight bash %}
$ pacman --remove groff
checking dependencies...
error: failed to prepare transaction (could not satisfy dependencies)
:: man-db: removing groff breaks dependency 'groff'
{% endhighlight %}

It can be solved by using <code>--cascade</code> option.

{% highlight bash %}
$ pacman -Rc groff
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --remove --cascade groff
checking dependencies...

Packages (2) man-db-2.7.6.1-2  groff-1.22.3-7

Total Removed Size:  10.39 MiB

:: Do you want to remove these packages? [Y/n]
{% endhighlight %}

![Docker pacman: Remove Cascade][image-ss-pm-rm-cascade]{: .img-responsive }

If you wish, you can have a cleaner removal.

{% highlight bash %}
$ pacman -Rcs groff
checking dependencies...

Packages (3) libpipeline-1.4.2-1  man-db-2.7.6.1-2  groff-1.22.3-7

Total Removed Size:  10.50 MiB

:: Do you want to remove these packages? [Y/n] 
{% endhighlight %}

It is just an example. 
We do not realy need to delete it.

#### Package Query Search

In short, <code>pacman -[Q|S]s</code>.

There are two kind of pacman searches, 

*	local query search 

*	repository sync search

Local query search.

{% highlight bash %}
$ pacman -Qs ncdu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --search ncdu
local/ncdu 1.12-1
    Disk usage analyzer with an ncurses interface
{% endhighlight %}

![Docker pacman: Query Search][image-ss-pm-query-search]{: .img-responsive }

And repository search. 

{% highlight bash %}
$ pacman -Ss ncdu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --search ncdu
community/ncdu 1.12-1 [installed]
    Disk usage analyzer with an ncurses interface
{% endhighlight %}

![Docker pacman: Sync Search][image-ss-pm-sync-search]{: .img-responsive }

There are also some AUR search command that we will discuss later.

#### Package Show Info

In short, <code>pacman -[Q|S][i|ii]</code>.

Pretty straightforward.

{% highlight bash %}
$ pacman -Qi ncdu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --info ncdu
Name            : ncdu
Version         : 1.12-1
Description     : Disk usage analyzer with an ncurses interface
{% endhighlight %}

![Docker pacman: Sync Info][image-ss-pm-sync-info]{: .img-responsive }

And repository search. With slightly different output,
the additional _Repository_ field.

{% highlight bash %}
$ pacman -Si ncdu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --info ncdu
Repository      : community
Name            : ncdu
Version         : 1.12-1
Description     : Disk usage analyzer with an ncurses interface
{% endhighlight %}

#### Change Log

I rarely use this feature.

{% highlight bash %}
$ pacman -Qc jq
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --changelog jq
Changelog for jq:
1.5-4:
fix CVE-2015-8863 (FS#50330)

1.5-2:
add oniguruma for regexp support

1.5-1:
upstream update

1.4-1:
move from AUR into [community]
{% endhighlight %}

![Docker pacman: changelog][image-ss-changelog]{: .img-responsive }

#### List Files

Pretty straightforward using <code>-Qi</code>.
Note that <code>-Si</code> is totally different task.

{% highlight bash %}
$ pacman --query --list ncdu
ncdu /usr/
ncdu /usr/bin/
ncdu /usr/bin/ncdu
ncdu /usr/share/
ncdu /usr/share/licenses/
ncdu /usr/share/licenses/ncdu/
ncdu /usr/share/licenses/ncdu/LICENSE
ncdu /usr/share/man/
ncdu /usr/share/man/man1/
ncdu /usr/share/man/man1/ncdu.1.gz
{% endhighlight %}

![Docker pacman: query list][image-ss-pm-query-list]{: .img-responsive }

Listing package files also can be achieved using <code>pkgfile</code>.

{% highlight bash %}
$ pkgfile --update
:: Updating 3 repos...
  download complete: core                 [   703.3 KiB  92.9K/s  2 remaining]
  download complete: extra                [     7.3 MiB   117K/s  1 remaining]
  download complete: community            [    16.7 MiB   176K/s  0 remaining]
:: download complete in 97.48s            <    24.7 MiB   260K/s  3 files    >
:: waiting for 1 process to finish repacking repos...
{% endhighlight %}

![Docker Arch: pkgfile update][image-ss-pkgfile-update]{: .img-responsive }

{% highlight bash %}
$ pkgfile --list ncdu
community/ncdu	/usr/
community/ncdu	/usr/bin/
community/ncdu	/usr/bin/ncdu
community/ncdu	/usr/share/
community/ncdu	/usr/share/licenses/
community/ncdu	/usr/share/licenses/ncdu/
community/ncdu	/usr/share/licenses/ncdu/LICENSE
community/ncdu	/usr/share/man/
community/ncdu	/usr/share/man/man1/
community/ncdu	/usr/share/man/man1/ncdu.1.gz
{% endhighlight %}

![Docker Arch: pkgfile list][image-ss-pkgfile-list]{: .img-responsive }


#### Verify

You can verify integrity of a package.

{% highlight bash %}
$ pacman -Qkk ncdu
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --check --check ncdu
ncdu: 10 total files, 0 altered files
{% endhighlight %}

![Docker pacman: query check][image-ss-pm-query-check]{: .img-responsive }

#### Package as File

You can examine downloaded package.

{% highlight bash %}
$ wget -c http://kambing.ui.ac.id/archlinux/community/os/x86_64/ncdu-1.12-1-x86_64.pkg.tar.xz
{% endhighlight %}

And gather information from the file directly

{% highlight bash %}
$ pacman --query --file ncdu-1.12-1-x86_64.pkg.tar.xz
{% endhighlight %}

Equal To:

{% highlight bash %}
$ pacman -Qp ncdu-1.12-1-x86_64.pkg.tar.xz
ncdu 1.12-1
{% endhighlight %}

![Docker Arch: pacman query file][image-ss-query-file]{: .img-responsive }

You can also query file
with list <code>-Qpl</code> or info <code>-Qpi</code>.

{% highlight bash %}
$ pacman -Qpl ncdu-1.12-1-x86_64.pkg.tar.xz
ncdu /usr/
ncdu /usr/bin/
ncdu /usr/bin/ncdu
ncdu /usr/share/
ncdu /usr/share/licenses/
ncdu /usr/share/licenses/ncdu/
ncdu /usr/share/licenses/ncdu/LICENSE
ncdu /usr/share/man/
ncdu /usr/share/man/man1/
ncdu /usr/share/man/man1/ncdu.1.gz
{% endhighlight %}

![Docker Arch: pacman query file list][image-ss-query-file-list]{: .img-responsive }

If you are a curious person, you can even extract the package.

{% highlight bash %}
$ tar -Jxvf ncdu-1.12-1-x86_64.pkg.tar.xz 
.PKGINFO
.BUILDINFO
.MTREE
usr/
usr/bin/
usr/share/
usr/share/licenses/
usr/share/man/
usr/share/man/man1/
usr/share/man/man1/ncdu.1.gz
usr/share/licenses/ncdu/
usr/share/licenses/ncdu/LICENSE
usr/bin/ncdu
{% endhighlight %}

-- -- --

### What's Next

<code>pacman</code> is a complete package management,
one command rule all, and it is also very fast.
So many commands that this topic deserve its own long article.
Consider finish reading [ [Part Two][local-part-two] ].


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/28/docker-arch-alpm.html

[image-ss-pull-arch]:		{{ asset_pull }}/arch.png
[image-ss-running-arch]:	{{ asset_post }}/00-running-image.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-getting-started]: {{ asset_post }}/00-getting-started.png

[image-ss-pm-refresh]:		{{ asset_post }}/01-refresh.png
[image-ss-pm-upgradable]:	{{ asset_post }}/01-upgradable.png
[image-ss-pm-download]:		{{ asset_post }}/01-download.png
[image-ss-pm-noconfirm]:	{{ asset_post }}/01-noconfirm.png
[image-ss-pm-sysupgrade]:	{{ asset_post }}/01-sysupgrade.png
[image-ss-pacman-syu]:		{{ asset_post }}/01-pacman-syu.png

[image-ss-pm-install]:		{{ asset_post }}/13-install.png
[image-ss-pm-sync-target]:	{{ asset_post }}/13-syu-target.png
[image-ss-pm-rm-cascade]:	{{ asset_post }}/13-remove-cascade.png
[image-ss-pm-rm-recursive]:	{{ asset_post }}/13-remove-recursive.png
[image-ss-pm-query-search]:	{{ asset_post }}/13-query-search.png
[image-ss-pm-sync-search]:	{{ asset_post }}/13-sync-search.png
[image-ss-pm-sync-info]:	{{ asset_post }}/13-sync-info.png

[image-ss-pkgfile-update]:	{{ asset_post }}/19-pkgfile-update.png
[image-ss-pkgfile-list]:	{{ asset_post }}/19-pkgfile-list.png
[image-ss-changelog]:		{{ asset_post }}/19-changelog.png

[image-ss-query-file]:		{{ asset_post }}/19-query-file.png
[image-ss-query-file-list]:	{{ asset_post }}/19-query-file-list.png

[image-ss-pm-query-list]:	{{ asset_post }}/13-query-list.png
[image-ss-pm-query-check]:	{{ asset_post }}/13-query-check.png
