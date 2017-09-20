---
layout: post
title: "Docker - openSUSE Zypper - Part One"
date: 2017-08-15 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, opensuse]
author: epsi

excerpt:
  Examine zypper step by step,
  using openSUSE container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
# - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-opensuse-zypper.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

Using openSUSE minimal install in Docker,
is a good way to learn zypper.
Zypper is a Package Manager utilized by openSUSE.
With minimal install, there is no need,
to download 4GB of full installation packages.

We need <code>Tumbleweed</code> rolling release with more often update than leap,
so that we have a chance to play more with package cycle.

{% include post/2017/08/docker-test-bed.md %}

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull opensuse/amd64:tumbleweed
{% endhighlight %}

![Docker Pull openSUSE Tumbleweed][image-ss-pull-opensuse]{: .img-responsive }

{% highlight bash %}
$ docker image list  
{% raw %}
  --filter "reference=opensuse/*:*"
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY          SIZE
opensuse/amd64      101MB
{% endhighlight %}

By the container image size,
openSUSE is good at managing minimal install.

{% highlight bash %}
$ docker run -it opensuse/amd64:tumbleweed bash
bash-4.4# exit
{% endhighlight %}

{% highlight bash %}
$ docker ps
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE                       NAMES                 STATUS
opensuse/amd64:tumbleweed   elegant_nightingale   Up 9 hours
{% endhighlight %}

![Docker openSUSE: List Running Containers][image-ss-docker-ps]{: .img-responsive }

{% highlight bash %}
$ docker start elegant_nightingale
elegant_nightingale
{% endhighlight %}

{% highlight bash %}
$ docker attach elegant_nightingale
bash-4.4# 
{% endhighlight %}

![Docker openSUSE: Getting Started][image-ss-opensuse-docker]{: .img-responsive }

-- -- --

### Issues on Minimal Install

#### No Reset

Since I use terminal with a lot of screenshot,
I utilize the <code>$ reset</code> command frequently.
Which is not available in openSUSE.
I can understand that minimal install,
could be a based for embedded system,
that is no need for either reset nor manual. 

There are two workaround,
first is to emulate with character.

{% highlight bash %}
$ echo -e '\0033\0143'
{% endhighlight %}

Or install <code>ncurses-utils</code>,
which contain <code>$ reset</code> command.

{% highlight bash %}
$ zypper install ncurses-utils
{% endhighlight %}

{% highlight bash %}
$ reset
{% endhighlight %}

#### No Manual

No manual in openSUSE Docker.
Minimal install is designed to be minimal,
such as embedded system.
The manual should not be in minimal install.
Therefore we need to tweak this setting for our need.

Change <code>rpm.install.excludedocs</code>
from <code>yes</code> to <code>no</code>.

{% highlight bash %}
$ cat /etc/zypp/zypp.conf | grep excludedocs
## Options for package installation: excludedocs
rpm.install.excludedocs = no
{% endhighlight %}

If necessary later, you still need to reinstall some packages,
so that the manual pages available in <code>/usr/share/man/</code>

{% highlight bash %}
$ zypper --quiet install --force man-pages man coreutils
The following package is going to be upgraded:
  coreutils

The following 2 packages are going to be reinstalled:
  man man-pages

1 package to upgrade, 2 to reinstall.
Overall download size: 5.5 MiB. Already cached: 0 B. After the
operation, additional 34.8 KiB will be used.
Continue? [y/n/...? shows all options] (y): y
{% endhighlight %}

![Docker openSUSE: Enable Manual Issue][image-ss-man-issue]{: .img-responsive }

-- -- --

### Package Management

#### ZYpp Frontend

Zypper

*	<https://github.com/openSUSE/libzypp>

*	<https://github.com/openSUSE/zypper>

*	<https://en.opensuse.org/SDB:Zypper_usage>

*	<https://en.wikipedia.org/wiki/ZYpp>
	
#### Get Help

Read the fine manual. Helpless or help more.

{% highlight bash %}
$ zypper help | less
{% endhighlight %}

{% highlight bash %}
$ zypper help install
{% endhighlight %}

#### Zypper Shell

Most modern package manager has shell feature,
that enable user to focus on the task of managing package.

{% highlight bash %}
$ zypper shell
zypper> help in
install (in) [options] <capability|rpm_file_uri> ...
{% endhighlight %}

![Docker openSUSE: Zypper Shell][image-ss-zypper-shell]{: .img-responsive }

-- -- -- 

### Updating System

	First Thing First

First thing to do is updating my system as usual.

*	OS Release

*	List Update

*	Update

*	Distribution Upgrade

*	Process that still being used after update and upgrade.

#### OS Release

{% highlight bash %}
$ cat /etc/os-release 
NAME="openSUSE Tumbleweed"
...
{% endhighlight %}

#### List Updates

{% highlight bash %}
$ zypper lu
Loading repository data...
Reading installed packages...
No updates found.
{% endhighlight %}

Equal to:

{% highlight bash %}
$ zypper list-updates
Loading repository data...
Reading installed packages...
S | Repository | Name             | Current Version | Available Version | Arch  
--+------------+------------------+-----------------+-------------------+-------
v | OSS        | coreutils        | 8.27-2.3        | 8.27-3.1          | x86_64
v | OSS        | krb5             | 1.15.1-3.2      | 1.15.1-4.1        | x86_64
v | OSS        | libzypp          | 16.14.0-1.1     | 16.15.3-1.1       | x86_64
v | OSS        | openSUSE-release | 20170816-1.2    | 20170821-1.2      | x86_64
v | OSS        | pam-config       | 0.91-3.2        | 0.92-1.1          | x86_64
v | OSS        | zypper           | 1.13.29-1.1     | 1.13.31-1.1       | x86_64
{% endhighlight %}

![Docker Zypper: List Update][image-ss-zypper-lu]{: .img-responsive }

#### Update

This will update _only_ newer packages.

{% highlight bash %}
$ zypper up
Loading repository data...
Reading installed packages...

Nothing to do.
{% endhighlight %}

Equal To:

{% highlight bash %}
$ zypper update
Loading repository data...
Reading installed packages...

The following 6 packages are going to be upgraded:
  coreutils krb5 libzypp openSUSE-release pam-config zypper

The following product is going to be upgraded:
  "openSUSE Tumbleweed"

6 packages to upgrade.
Overall download size: 5.9 MiB. Already cached: 0 B. After the
operation, additional 94.7 KiB will be used.
Continue? [y/n/...? shows all options] (y):
{% endhighlight %}

![Docker Zypper: Update][image-ss-zypper-up]{: .img-responsive }

#### Upgrade

This is similar to update, but affect all packages.
Wider than just _new_ package.
This distribution upgrade also works in tumbleweed rolling release.
You can see that both are slightly different.

{% highlight bash %}
$ zypper dup
Warning: ...
Loading repository data...
Reading installed packages...
Computing distribution upgrade...

Nothing to do.
{% endhighlight %}

Equal To:

{% highlight bash %}
$ zypper dist-upgrade
Warning: You are about to do a distribution upgrade with all enabled repositories. Make sure these repositories are compatible before you continue. See 'man zypper' for more information about this command.
Loading repository data...
Reading installed packages...
Computing distribution upgrade...

The following NEW package is going to be installed:
  openSUSE-release-ftp

1 new package to install.
Overall download size: 7.1 KiB. Already cached: 0 B. After the
operation, additional 66.0 B will be used.
Continue? [y/n/...? shows all options] (y):
{% endhighlight %}

![Docker Zypper: Distribution Upgrade][image-ss-zypper-dup]{: .img-responsive }

#### Patch Check

Another update method.

{% highlight bash %}
$ zypper pchk
{% endhighlight %}

Equal To:

{% highlight bash %}
$ zypper patch-check
Loading repository data...
Reading installed packages...

0 patches needed (0 security patches)
{% endhighlight %}

#### Process Being Used

Sometimes there are process still being used by upgraded package,
we can check this using <code>$ zypper ps -s</code>.

{% highlight bash %}
$ zypper ps 
Check failed:
Please install package 'lsof' first.

$ zypper in lsof

$ zypper ps -s
No processes using deleted files found.
{% endhighlight %}

You can see a more complete example about this process,
in my previous opensuse full install article.

-- -- --

### Package IRSIF

	Install, Remove, Search, Info, File

#### Package Install

Consider our favorite example package below.

{% highlight bash %}
$ zypper in man nano htop ncdu fish
{% endhighlight %}

Equal To:

{% highlight bash %}
$ zypper install man nano htop ncdu fish
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following 31 NEW packages are going to be installed:
  aaa_base bc cron cronie dbus-1 desktop-file-utils fish
  glibc-locale groff htop kbd kmod less libgdbm4 libglib-2_0-0
  libpcre2-32-0 libpipeline1 libpython2_7-1_0 man nano ncdu
  ncurses-utils pkg-config python-base python-curses
  python-rpm-macros system-user-man systemd
  systemd-presets-branding-CAASP udev update-desktop-files

31 new packages to install.
Overall download size: 24.7 MiB. Already cached: 0 B. After the
operation, additional 190.3 MiB will be used.
Continue? [y/n/...? shows all options] (y): 
{% endhighlight %}

![Docker Zypper: Install][image-ss-zypper-in]{: .img-responsive }

Note that you can <code>reinstall</code> using <code>-f</code> argument.

{% highlight bash %}
$ zypper install --force man nano htop ncdu fish
{% endhighlight %}

#### Download

Download without install is possible.

{% highlight bash %}
$ zypper in --download-only wget
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following NEW package is going to be installed:
  wget

1 new package to install.
Overall download size: 667.1 KiB. Already cached: 0 B. Download
only.
Continue? [y/n/...? shows all options] (y): 
{% endhighlight %}

![Docker Zypper: Download Only][image-ss-download]{: .img-responsive }

And you can continue with install later.

{% highlight bash %}
$ zypper in wget
{% endhighlight %}


#### Package Removal

{% highlight bash %}
$ zypper rm systemd
{% endhighlight %}

Equal to:

{% highlight bash %}
$ zypper remove systemd
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following 13 packages are going to be REMOVED:
  aaa_base ca-certificates ca-certificates-mozilla cron cronie
  desktop-file-utils fish htop issue-generator man openSUSE-release
  systemd update-desktop-files

The following product is going to be REMOVED:
  "openSUSE Tumbleweed"

13 packages to remove.
After the operation, 21.3 MiB will be freed.
Continue? [y/n/...? shows all options] (y):
{% endhighlight %}

![Docker Zypper: Remove][image-ss-zypper-rm]{: .img-responsive }

#### Dependency Removal

Supposed you install ViM, and later desire to remove ViM, with all dependencies.

{% highlight bash %}
$ zypper remove --clean-deps vim
{% endhighlight %}

Equal to:

{% highlight bash %}
$ zypper rm -u vim
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following 3 packages are going to be REMOVED:
  libdb-4_8 perl vim

3 packages to remove.
After the operation, 45.3 MiB will be freed.
Continue? [y/n/...? shows all options] (y): 
{% endhighlight %}

![Docker Zypper: Dependency][image-ss-dependency]{: .img-responsive }

#### Package Query Search

{% highlight bash %}
$ zypper se fish
{% endhighlight %}

Equal to:

{% highlight bash %}
$ zypper search fish
Loading repository data...
Reading installed packages...

S  | Name                           | Summary          | Type       
---+--------------------------------+------------------+------------
   | Bluefish                       | Text editor wi-> | application
   | Catfish                        | Versatile file-> | application
   | Cuttlefish                     | Icon Previewer-> | application
   | bluefish                       | A feature-Rich-> | package    
   | catfish                        | Versatile File-> | package    
   | catfish-lang                   | Translations f-> | package    
i+ | fish                           | A user friendl-> | package    
   | ghc-cipher-blowfish            | Blowfish cipher  | package    
   | ghc-cipher-blowfish-devel      | Haskell cipher-> | package    
{% endhighlight %}

![Docker Zypper: Query Search][image-ss-zypper-se]{: .img-responsive }

#### Package Show Info

{% highlight bash %}
$ zypper if fish
{% endhighlight %}

Eauql to

{% highlight bash %}
$ zypper info fish
Loading repository data...
Reading installed packages...


Information for package fish:
-----------------------------
Repository     : OSS                              
Name           : fish                             
Version        : 2.6.0-1.2                        
Arch           : x86_64                           
Vendor         : openSUSE                         
Installed Size : 7.5 MiB                          
Installed      : Yes                              
Status         : up-to-date                       
Source package : fish-2.6.0-1.2.src               
Summary        : A user friendly interactive shell
Description    :                                  
    fish is a user friendly command line shell for UNIX-like
    operating systems
{% endhighlight %}

![Docker Zypper: Show Info][image-ss-zypper-if]{: .img-responsive }

#### Package File List

My bad, I cannot find any reference about listing files in particular package.
Therefore I use the lower level <code>rpm -ql</code> instead.

{% highlight bash %}
$ rpm --query --list ncdu
/usr/bin/ncdu
/usr/share/doc/packages/ncdu
/usr/share/doc/packages/ncdu/COPYING
/usr/share/doc/packages/ncdu/ChangeLog
/usr/share/doc/packages/ncdu/README
/usr/share/man/man1/ncdu.1.gz
{% endhighlight %}

![Docker RPM: Query List][image-ss-rpm-ql]{: .img-responsive }

However there is this <code>zypper -f</code> command,
similar with <code>rpm -qf</code> instead.

{% highlight bash %}
$ zypper search --file /etc/manpath.config 
Loading repository data...
Reading installed packages...

S  | Name | Summary                            | Type   
---+------+------------------------------------+--------
i+ | man  | A Program for Displaying man Pages | package
{% endhighlight %}

![Docker Zypper: Search File][image-ss-search-file]{: .img-responsive }

-- -- --

### What's Next

Zypper has amazing <code>repository</code> commands,
so many commands that this topic deserve its own long article.
Consider finish reading [ [Part Two][local-part-two] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-opensuse' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/16/docker-opensuse-zypper.html

[image-ss-pull-opensuse]:   {{ asset_pull }}/opensuse-tumbleweed.png
[image-ss-opensuse-docker]: {{ asset_post }}/00-getting-started.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-zypper-shell]:    {{ asset_post }}/01-zypper-shell.png

[image-ss-zypper-lu]:	{{ asset_post }}/01-list-updates.png
[image-ss-zypper-up]:	{{ asset_post }}/01-update.png
[image-ss-zypper-dup]:	{{ asset_post }}/01-distribution-upgrade.png

[image-ss-zypper-if]:	{{ asset_post }}/13-info-fish.png
[image-ss-zypper-in]:	{{ asset_post }}/13-install.png
[image-ss-zypper-rm]:	{{ asset_post }}/13-remove-systemd.png
[image-ss-zypper-se]:	{{ asset_post }}/13-search-fish.png

[image-ss-man-issue]:	{{ asset_post }}/05-conclusion-man-issue.png

[image-ss-download]:	{{ asset_post }}/13-download-only.png
[image-ss-dependency]:	{{ asset_post }}/13-dependency-removal.png
[image-ss-rpm-ql]:		{{ asset_post }}/13-rpm-ql.png
[image-ss-search-file]:	{{ asset_post }}/13-search-file.png
