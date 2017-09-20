---
layout: post
title: "Docker - Debian APT - Part One"
date: 2017-08-24 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
# - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-debian-apt.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

Using Debian minimal install in Docker,
is a good way to learn The **Advance Package Tool**.
APT is considered a basic knowledge
utilized by Debian's derived distribution
such as Ubuntu, Mint, Elementary, Zorin, Kali Linux, Parrot, Cyborg,
and some other cool Distribution.

We only need <code>Stretch</code> stable release.
We will switch to testing later on part three,
so that we have a chance to play more with package cycle.

{% include post/2017/08/docker-test-bed.md %}

#### Must Read

You are encouraged to read this first,
before even starting to read this article.

*	<https://debian-handbook.info/browse/stable/>

There are already so many references for this APT.
This is just another reference.
I think that I'm late.
I should have write these ten years ago.

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull debian:stretch
{% endhighlight %}

![Docker Pull Debian Strecth][image-ss-pull-debian]{: .img-responsive }

{% highlight bash %}
$ docker image list  
{% raw %}
  --filter "reference=debian:*"
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY          SIZE
debian              100MB
{% endhighlight %}

By the container image size,
Debian is good at managing minimal install.

{% highlight bash %}
$ docker run -it debian:stretch
root@f77ea94688d1:/# exit
exit
{% endhighlight %}

![Docker Debian: Running Docker][image-ss-running-debian]{: .img-responsive }

{% highlight bash %}
$ docker ps
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE               NAMES               STATUS
debian:stretch      goofy_allen         Up 7 minutes
{% endhighlight %}

![Docker Debian: Docker ps][image-ss-docker-ps]{: .img-responsive }

{% highlight bash %}
$ docker start goofy_allen
goofy_allen
{% endhighlight %}

{% highlight bash %}
$ docker attach goofy_allen
root@f77ea94688d1:/# 
{% endhighlight %}

![Docker Debian: Getting Started][image-ss-getting-started]{: .img-responsive }

-- -- --

### Package Management

Debian utilize APT, The **Advanced Package Tool**.
APT has a lower level tool caled **dpkg**
that handle <code>.deb</code> package files.

It has many front end, 

*	<https://en.wikipedia.org/wiki/APT_(Debian)>

*	<https://en.wikipedia.org/wiki/Dpkg>

#### DPKG Frontend

Last updated Eight Years Ago.

*	<https://github.com/davidben/dpkg>


#### APT Frontend

*	APT (apt-get and family): Python:	

	*	<https://github.com/Debian/apt>
	
	*	<https://anonscm.debian.org/git/apt/apt.git>
	
*	apt-src: No github yet

*	aptitude: No github yet

	*	https://anonscm.debian.org/git/aptitude/aptitude.git
	
#### Get Help

Read the fine manual.

{% highlight bash %}
$ apt help
{% endhighlight %}

Note that with minimal install in docker,
we do not have <code>man-db</code> and <code>less</code> yet.

{% highlight bash %}
$ man apt
{% endhighlight %}

#### The new APT

I have been a Debian user since 2007.
It means a decade.
My hand automatically type every few days.

{% highlight bash %}
$ apt-get update
$ apt-get upgrade
$ apt-get autoremove
$ apt-get autoclean
{% endhighlight %}

Whenever there is a conflict,
I use <code>aptitude</code>
instead of <code>apt-get</code>. 

But this is going to change.
Now we have <code>apt</code>
instead of <code>apt-get</code>.
<code>apt</code> is a combination of most common command from
<code>apt-get</code> and <code>apt-cache</code>.

Now you can see there are some front end.
Our focus now is <code>apt</code>.

#### APT Shell

There also APT shell, separated from the official APT
that enable user to focus on the task of managing package.

{% highlight bash %}
$ aptsh
Generating and mapping caches...
Reading commands history...
 apt sh > 
{% endhighlight %}

Note that with minimal install in docker,
we do not have <code>aptsh</code>.

I personally never use this <code>aptsh</code>.

-- -- -- 

### Updating System

	First Thing First

First thing to do is updating my system as usual.

*	OS Release

*	Update

*	List Upgradable

*	Upgrade

#### OS Release

{% highlight bash %}
$ cat /etc/os-release 
PRETTY_NAME="Debian GNU/Linux 9 (stretch)"
NAME="Debian GNU/Linux"
...
{% endhighlight %}

#### Refresh Update

{% highlight bash %}
$ apt-get update
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt update
Get:1 http://security.debian.org stretch/updates InRelease [62.9 kB]
Get:2 http://security.debian.org stretch/updates/main amd64 Packages [200 kB]
Ign:3 http://deb.debian.org/debian stretch InRelease
Get:4 http://deb.debian.org/debian stretch-updates InRelease [91.0 kB]
Get:5 http://deb.debian.org/debian stretch Release [118 kB]        
Get:6 http://deb.debian.org/debian stretch-updates/main amd64 Packages [5553 B]
Get:7 http://deb.debian.org/debian stretch Release.gpg [2373 B]    
Get:8 http://deb.debian.org/debian stretch/main amd64 Packages [9497 kB]
Fetched 9977 kB in 22s (444 kB/s)                                                    
Reading package lists... Done
Building dependency tree       
Reading state information... Done
1 package can be upgraded. Run 'apt list --upgradable' to see it.
{% endhighlight %}

Alternatively:

{% highlight bash %}
$ aptitude update
aptitude update 
Ign http://deb.debian.org/debian stretch InRelease
Hit http://deb.debian.org/debian stretch-updates InRelease
Hit http://deb.debian.org/debian stretch Release
Hit http://security.debian.org stretch/updates InRelease
{% endhighlight %}

![Docker APT: Update][image-ss-apt-update]{: .img-responsive }

#### List Upgradable

just relax, folow what the command above,
told us to do to list upgradable packages.

{% highlight bash %}
$ apt list --upgradable -a
Listing... Done
libgcrypt20/stable 1.7.6-2+deb9u2 amd64 [upgradable from: 1.7.6-2+deb9u1]
libgcrypt20/stable,now 1.7.6-2+deb9u1 amd64 [installed,upgradable to: 1.7.6-2+deb9u2]
{% endhighlight %}

![Docker APT: List Upgradable][image-ss-apt-upgradable]{: .img-responsive }

#### Upgrade

{% highlight bash %}
$ apt-get upgrade
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt upgrade
Reading package lists... Done
Building dependency tree       
Reading state information... Done
Calculating upgrade... Done
0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.
{% endhighlight %}

Alternatively:

{% highlight bash %}
$ aptitude upgrade
{% endhighlight %}


![Docker APT: Upgrade][image-ss-apt-upgrade]{: .img-responsive }

#### Full Upgrade

There is also this <code>full-upgrade</code>,
that resolve dependency conflict automatically.

{% highlight bash %}
$ apt-get full-upgrade
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt full-upgrade
{% endhighlight %}

Alternatively:

{% highlight bash %}
$ aptitude full-upgrade
{% endhighlight %}

There is however.

{% highlight bash %}
$ aptitude safe-upgrade
{% endhighlight %}

And later we will discuss

{% highlight bash %}
$ apt dist-upgrade
{% endhighlight %}

-- -- --

### Package IRSIF

	Install, Remove, Search, Info, File

#### Package Install

Consider our favorite example package below.
This will have a verbose long output.

{% highlight bash %}
$ apt-get install htop ncdu fish wget curl vim sudo aptitude
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt install man-db nano less aptsh  
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following additional packages will be installed:
  bsdmainutils groff-base libbsd0 libgdbm3 libgpm2 libncurses5
  libpipeline1 libreadline5 readline-common
Suggested packages:
  cpp wamerican | wordlist whois vacation groff gpm www-browser
  spell readline-doc
The following NEW packages will be installed:
  aptsh bsdmainutils groff-base less libbsd0 libgdbm3 libgpm2
  libncurses5 libpipeline1 libreadline5 man-db nano
  readline-common
0 upgraded, 13 newly installed, 0 to remove and 0 not upgraded.
Need to get 3511 kB of archives.
After this operation, 9857 kB of additional disk space will be used.
Do you want to continue? [Y/n]
...
{% endhighlight %}

Alternatively:

{% highlight bash %}
$ aptitude install man-db nano less aptsh
man-db is already installed at the requested version (2.7.6.1-2)
nano is already installed at the requested version (2.7.4-1)
less is already installed at the requested version (481-2.1)
aptsh is already installed at the requested version (0.0.8)
man-db is already installed at the requested version (2.7.6.1-2)
nano is already installed at the requested version (2.7.4-1)
less is already installed at the requested version (481-2.1)
aptsh is already installed at the requested version (0.0.8)
No packages will be installed, upgraded, or removed.
0 packages upgraded, 0 newly installed, 0 to remove and 0 not upgraded.
Need to get 0 B of archives. After unpacking 0 B will be used.
{% endhighlight %}

![Docker APT: Install][image-ss-apt-install]{: .img-responsive }

You can add <code>-y</code> to avoid confirmation.
This will assume yes for each confirmation.

{% highlight bash %}
$ apt-get install -y htop ncdu fish wget curl vim sudo aptitude
{% endhighlight %}

#### Download

There are two ways to download package without installing.

{% highlight bash %}
$ apt-get install --download-only ncdu
...
Get:1 http://deb.debian.org/debian testing/main amd64 ncdu amd64 1.12-1+b1 [41.3 kB]
Fetched 41.3 kB in 1s (27.8 kB/s)
Download complete and in download only mode
{% endhighlight %}

![Docker Debian: Cache][image-ss-download-only]{: .img-responsive }

The package will be in cache.

{% highlight bash %}
$ ls /var/cache/apt/archives/
lock  ncdu_1.12-1+b1_amd64.deb	partial
{% endhighlight %}

![Docker APT: Download Only][image-ss-download-cache]{: .img-responsive }

The next time you do install, it does need to download anymore,
because the package is alre4ady in the cache.

Alternatively you can bypass the cache.

{% highlight bash %}
$ cd ~
{% endhighlight %}

{% highlight bash %}
$ apt-get download htop
Get:1 http://deb.debian.org/debian testing/main amd64 htop amd64 2.0.2-1 [88.2 kB]
Fetched 88.2 kB in 1s (69.2 kB/s)
W: Download is performed unsandboxed as root as file '/root/htop_2.0.2-1_amd64.deb' couldn't be accessed by user '_apt'. - pkgAcquire::Run (13: Permission denied)
{% endhighlight %}

![Docker APT: Download][image-ss-download-nocache]{: .img-responsive }

And you can install using low level <code>dpkg</code> later on.

{% highlight bash %}
$ dpkg -i htop_2.0.2-1_amd64.deb 
Selecting previously unselected package htop.
(Reading database ... 36837 files and directories currently installed.)
Preparing to unpack htop_2.0.2-1_amd64.deb ...
Unpacking htop (2.0.2-1) ...
Setting up htop (2.0.2-1) ...
Processing triggers for mime-support (3.60) ...
Processing triggers for man-db (2.7.6.1-2) ...
{% endhighlight %}

![Docker DPKG: Install][image-ss-dpkg-install]{: .img-responsive }

#### Package Removal

There are two kind of removal in APT.
It is <code>apt remove</code> and <code>apt purge</code>.
The last one also purge configuration.

Consider this <code>groff-base</code>,
that is dependency of <code>man-db</code>.
Debian is mature in managing dependency.

{% highlight bash %}
$ apt-get remove groff-base
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt remove groff-base
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following packages were automatically installed and are no longer required:
  libgdbm3 libpipeline1
Use 'apt autoremove' to remove them.
The following packages will be REMOVED:
  groff-base man-db
0 upgraded, 0 newly installed, 2 to remove and 0 not upgraded.
After this operation, 5624 kB disk space will be freed.
Do you want to continue? [Y/n] 
{% endhighlight %}

![Docker APT: Remove][image-ss-apt-remove]{: .img-responsive }

Alternatively, with <code>aptitude</code> use different approach:

{% highlight bash %}
$ aptitude remove groff-base
The following packages will be REMOVED:  
  groff-base 
0 packages upgraded, 0 newly installed, 1 to remove and 0 not upgraded.
Need to get 0 B of archives. After unpacking 3339 kB will be freed.
The following packages have unmet dependencies:
 man-db : Depends: groff-base (>= 1.18.1.1-15) but it is not going to be installed
The following actions will resolve these dependencies:

     Remove the following packages:    
1)     man-db [2.7.6.1-2 (now, stable)]



Accept this solution? [Y/n/q/?]
{% endhighlight %}

![Docker Aptitude: Remove][image-ss-aptitude-remove]{: .img-responsive }

This different <code>aptitude</code> approach is,
why I use aptitude whenever I have package conflict.
Before the new <code>apt</code> comes out.

It is just an example. 
We do not realy need to delete it.

#### Package Query Search

Search used to be handled by <code>apt-cache</code>.
But the output is slightly different.

{% highlight bash %}
$ apt search ncdu
Sorting... Done
Full Text Search... Done
ncdu/stable 1.12-1+b1 amd64
  ncurses disk usage viewer

$ apt-cache search ncdu
ncdu - ncurses disk usage viewer
{% endhighlight %}

Alternatively:

{% highlight bash %}
$ aptitude search ncdu
{% endhighlight %}

![Docker APT: Search][image-ss-apt-search]{: .img-responsive }

There are a lower level <code>dpkg-query --list</code> tool as well.
I must admit this is my favorite.

{% highlight bash %}
$ dpkg-query --list apt*
Desired=Unknown/Install/Remove/Purge/Hold
| Status=Not/Inst/Conf-files/Unpacked/halF-conf/Half-inst/trig-aWait/Trig-pend
|/ Err?=(none)/Reinst-required (Status,Err: uppercase=bad)
||/ Name            Version      Architecture Description
+++-===============-============-============-====================================
ii  apt             1.4.7        amd64        commandline package manager
un  apt-doc         <none>       <none>       (no description available)
un  apt-utils       <none>       <none>       (no description available)
un  apt-xapian-inde <none>       <none>       (no description available)
ii  aptitude        0.8.7-1      amd64        terminal-based package manager
ii  aptitude-common 0.8.7-1      all          architecture independent files for t
un  aptitude-doc    <none>       <none>       (no description available)
un  aptitude-doc-en <none>       <none>       (no description available)
ii  aptsh           0.0.8        amd64        apt interactive shell
{% endhighlight %}

![Docker DPKG: Query List][image-ss-dpkg-list]{: .img-responsive }

#### Package Show Info

Pretty straightforward.

{% highlight bash %}
$ apt-cache show ncdu
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt show ncdu
Package: ncdu
Version: 1.12-1+b1
Priority: optional
Section: admin
Source: ncdu (1.12-1)
Maintainer: Eugene V. Lyubimkin <jackyf@debian.org>
Installed-Size: 94.2 kB
Depends: libc6 (>= 2.14), libncursesw5 (>= 6), libtinfo5 (>= 6)
Homepage: http://dev.yorhel.nl/ncdu/
Tag: admin::monitoring, implemented-in::c, interface::text-mode,
 role::program, scope::utility, uitoolkit::ncurses
Download-Size: 41.3 kB
APT-Sources: http://deb.debian.org/debian stretch/main amd64 Packages
Description: ncurses disk usage viewer
{% endhighlight %}

Alternatively:

{% highlight bash %}
$ aptitude show ncdu
{% endhighlight %}

![Docker APT: Show][image-ss-apt-show]{: .img-responsive }

This <code>show</code> will grab all package with respected pattern.
You can narrow the result using regular expression.

{% highlight bash %}
$ apt search '^apt$'
{% endhighlight %}

There are a lower level <code>dpkg-query --status</code> tool as well.

{% highlight bash %}
$ dpkg-query --status apt
Package: apt
Status: install ok installed
Priority: important
Section: admin
Installed-Size: 3538
Maintainer: APT Development Team <deity@lists.debian.org>
Architecture: amd64
Version: 1.4.7
Replaces: apt-utils (<< 1.3~exp2~)
Depends: adduser, gpgv | gpgv2 | gpgv1, debian-archive-keyring, init-system-helpers (>= 1.18~), libapt-pkg5.0 (>= 1.3~rc2), libc6 (>= 2.15), libgcc1 (>= 1:3.0), libstdc++6 (>= 5.2)
Recommends: gnupg | gnupg2 | gnupg1
Suggests: apt-doc, aptitude | synaptic | wajig, dpkg-dev (>= 1.17.2), powermgmt-base, python-apt
Breaks: apt-utils (<< 1.3~exp2~)
{% endhighlight %}

![Docker DPKG: Query Status][image-ss-dpkg-status]{: .img-responsive }

You may find <code>dpkg -I</code> useful to get information directly from the package.

{% highlight bash %}
$ dpkg-deb --info htop_2.0.2-1_amd64.deb 
 new debian package, version 2.0.
 size 88164 bytes: control archive=856 bytes.
     581 bytes,    17 lines      control              
     564 bytes,     9 lines      md5sums              
 Package: htop
 Version: 2.0.2-1
 Architecture: amd64
 Maintainer: Daniel Lange <dl.ml1@usrlocal.de>
...
{% endhighlight %}

![Docker DPKG: Deb Info][image-ss-dpkg-deb-info]{: .img-responsive }

#### List Files

Listing package files can be achieved using dpkg.

{% highlight bash %}
$ dpkg -L ncdu
{% endhighlight %}

Or

{% highlight bash %}
$ dpkg-query --listfiles ncdu
/.
/usr
/usr/bin
/usr/bin/ncdu
/usr/share
/usr/share/doc
/usr/share/doc/ncdu
/usr/share/doc/ncdu/changelog.Debian.amd64.gz
/usr/share/doc/ncdu/changelog.Debian.gz
/usr/share/doc/ncdu/changelog.gz
/usr/share/doc/ncdu/copyright
/usr/share/man
/usr/share/man/man1
/usr/share/man/man1/ncdu.1.gz
{% endhighlight %}

![Docker DPKG: List Files][image-ss-dpkg-listfiles]{: .img-responsive }

#### Extract

If you are a curious person, you can even extract the package.

{% highlight bash %}
$ dpkg-deb --extract htop_2.0.2-1_amd64.deb .
{% endhighlight %}

-- -- --

### What's Next

APT is a mature package management,
so many commands that this topic deserve its own long article.
Consider finish reading [ [Part Two][local-part-two] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/25/docker-debian-apt.html

[image-ss-pull-debian]:		{{ asset_pull }}/debian-stretch.png
[image-ss-running-debian]:	{{ asset_post }}/00-running-image.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-getting-started]: {{ asset_post }}/00-getting-started.png

[image-ss-apt-update]:		{{ asset_post }}/01-update.png
[image-ss-apt-upgradable]:	{{ asset_post }}/01-upgradable.png
[image-ss-apt-upgrade]:		{{ asset_post }}/01-upgrade.png

[image-ss-apt-install]:		{{ asset_post }}/13-apt-install.png
[image-ss-apt-remove]:		{{ asset_post }}/13-apt-remove.png
[image-ss-apt-search]:		{{ asset_post }}/13-apt-search.png
[image-ss-apt-show]:		{{ asset_post }}/13-apt-show.png

[image-ss-dpkg-list]:		{{ asset_post }}/13-dpkg-list.png
[image-ss-dpkg-status]:		{{ asset_post }}/13-dpkg-status.png

[image-ss-download-cache]:		{{ asset_post }}/13-download-cache.png
[image-ss-download-only]:		{{ asset_post }}/13-download-only.png
[image-ss-download-nocache]:	{{ asset_post }}/13-download-no-cache.png
[image-ss-dpkg-install]:		{{ asset_post }}/13-dpkg-install.png

[image-ss-aptitude-remove]:		{{ asset_post }}/13-aptitude-remove.png
[image-ss-dpkg-listfiles]:		{{ asset_post }}/13-dpkg-query-listfiles.png
[image-ss-dpkg-deb-info]:		{{ asset_post }}/13-dpkg-deb-info.png
