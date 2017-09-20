---
layout: post
title: "Docker - Gentoo Portage - Part One"
date: 2017-08-11 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, gentoo]
author: epsi

excerpt:
  Docker flow for Gentoo Portage,
  from emerge-webrsync to manual rsync.
  (not so) first time using Gentoo experience.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
# - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-gentoo-portage.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

I am so glad that finally I can learn Gentoo Portage using Docker.

{% include post/2017/08/docker-test-bed.md %}

-- -- --

### Getting Started With Docker

Our first move is, of course attach docker process, as usual.

{% highlight bash %}
$ docker pull gentoo/stage3-amd64
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
$ docker run -it gentoo/stage3-amd64
ca9efc06241d / # exit
{% endhighlight %}

{% highlight bash %}
$ docker ps -a 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE                 NAMES               STATUS
gentoo/stage3-amd64   amazing_shirley     Exited (0) 24 seconds ago
vbatts/slackware      cranky_keller       Exited (0) 37 minutes ago
{% endhighlight %}

{% highlight bash %}
$ docker start amazing_shirley
amazing_shirley
{% endhighlight %}

{% highlight bash %}
$ docker attach amazing_shirley
ca9efc06241d / #
{% endhighlight %}

![Docker Gentoo: Getting Started][image-ss-gentoo-docker]{: .img-responsive }

-- -- --

### Package Management

Gentoo use Portage to manage their packages.

*	<https://wiki.gentoo.org/wiki/Portage>

#### Portage Frontend

Emerge

*	<https://github.com/gentoo/portage>
	

#### Get Help

Read the fine manual instead.

{% highlight bash %}
$ man emerge
{% endhighlight %}

-- -- --

### Updating System

	First Thing First

First thing to do in my mind is updating my system.

#### OS Release

{% highlight bash %}
$ cat /etc/gentoo-release 
Gentoo Base System release 2.3
{% endhighlight %}

#### webrsync

{% highlight bash %}
ca9efc06241d / # emerge-webrsync
Fetching most recent snapshot ...
 * Latest snapshot date: 20170814
 * 
 * Approximate snapshot timestamp: 1502757900
 *        Current local timestamp: 1502757301
 * 
 * The current local timestamp is possibly identical to the
 * timestamp of the latest snapshot. In order to force sync, use
 * the --revert option or remove the timestamp file located at
 * '/usr/portage/metadata/timestamp.x'.
{% endhighlight %}

[![Docker Gentoo: emerge-webrsync][image-ss-emerge-webrsync]{: .img-responsive }][photo-ss-emerge-webrsync]


#### Update Packages

You can use <code>emerge -u</code> command.

{% highlight bash %}
ca9efc06241d / # emerge -u system
Calculating dependencies... done!
>>> Auto-cleaning packages...

>>> No outdated packages were found on your system.
{% endhighlight %}

{% highlight bash %}
ca9efc06241d / # emerge -u world
Calculating dependencies... done!
>>> Auto-cleaning packages...

>>> No outdated packages were found on your system.
{% endhighlight %}

{% highlight bash %}
ca9efc06241d / # eselect news read
No news is good news.
{% endhighlight %}

![Docker Gentoo: emerge --update][image-ss-emerge-update]{: .img-responsive }

#### Updating System Manually

As a beginner, I always curious how things works.

Consider a fresh clean gentoo container.

{% highlight bash %}
$ emerge
!!! Section 'gentoo' in repos.conf has location attribute set to nonexistent directory: '/usr/portage'
!!! Section 'x-portage' in repos.conf has location attribute set to nonexistent directory: '/usr/portage'
!!! Invalid Repository Location (not a dir): '/usr/portage'


!!! /etc/portage/make.profile is not a symlink and will probably prevent most merges.
!!! It should point into a profile within /usr/portage/profiles/
!!! (You can safely ignore this message when syncing. It's harmless.)


!!! Your current profile is invalid. If you have just changed your profile
!!! configuration, you should revert back to the previous configuration.
!!! Allowed actions are limited to --help, --info, --search, --sync, and
!!! --version.
{% endhighlight %}

You can manually populate portage tree.
It is about only 70 megabytes download.

{% highlight bash %}
$ cd /usr/
$ wget -c http://distfiles.gentoo.org/snapshots/portage-latest.tar.xz
$ tar -xvf portage-latest.tar.xz
$ rm portage-latest.tar.xz
{% endhighlight %}

#### Updating More

There are other command such as <code>emerge --sync</code>
Read the fine manual instead.

-- -- --

### Package IRSIF

	Install, Remove, Search, Info, File

Read the fine manual.

#### Package Install

What good is it life without our favorites packages ?
Consider install it at the first place after system update.

	Merge

{% highlight bash %}
$ emerge htop fish ncdu
{% endhighlight %}

With <code>emerge</code> command,
these packages will be compiled, built and installed.
<code>emerge</code> has a very nice looks, 
many colors while building package.

[![Docker Emerge: Install Package][image-ss-emerge-install]{: .img-responsive }][photo-ss-emerge-install]

#### Ask for Confirmation

	Ask

{% highlight bash %}
$ emerge -a htop fish ncdu
{% endhighlight %}

![Docker Emerge: Install][image-ss-emerge-install-ask]{: .img-responsive }

#### Ambiguous Package

How about this ?

{% highlight bash %}
$ emerge mc
{% endhighlight %}

![Docker Emerge: ebuild name is ambiguous][image-ss-emerge-ambigous]{: .img-responsive }

Use fully qualified ebuild name instead.

{% highlight bash %}
$ emerge app-misc/mc 
{% endhighlight %}

#### Download

You can download package and install later on.

{% highlight bash %}
$ emerge -a --fetchonly curl

These are the packages that would be fetched, in order:

Calculating dependencies... done!
[ebuild     U  ] net-misc/curl-7.55.1 [7.55.0]

Would you like to fetch the source files for these packages? [Yes/No] y

>>> Fetching (1 of 1) net-misc/curl-7.55.1::gentoo
>>> Downloading 'rsync://ftp.jaist.ac.jp/pub/Linux/Gentoo/distfiles/curl-7.55.1.tar.bz2'
...
receiving incremental file list
curl-7.55.1.tar.bz2
      2,786,830 100%  157.30kB/s    0:00:17 (xfr#1, to-chk=0/1)

sent 43 bytes  received 2,787,617 bytes  50,228.11 bytes/sec
total size is 2,786,830  speedup is 1.00
 * curl-7.55.1.tar.bz2 SHA256 SHA512 WHIRLPOOL size ;-) ...  [ ok ]
{% endhighlight %}

![Docker Emerge: Fetch Only][image-ss-emerge-fetchonly]{: .img-responsive }

Now you can continue to install.

{% highlight bash %}
$ emerge -a curl

These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild     U  ] net-misc/curl-7.55.1 [7.55.0]

Would you like to merge these packages? [Yes/No] y

>>> Verifying ebuild manifests

>>> Emerging (1 of 1) net-misc/curl-7.55.1::gentoo
...
{% endhighlight %}

#### Only Dependencies

Just any other build system, we can prepare dependency first,
before build our package.

{% highlight bash %}
$ emerge --onlydeps herbstluftwm
Calculating dependencies... done!

>>> Verifying ebuild manifests

>>> Emerging (1 of 16) dev-libs/libbsd-0.8.3::gentoo
 * libbsd-0.8.3.tar.xz SHA256 SHA512 WHIRLPOOL size ;-) ...  [ ok ]
>>> Unpacking source...
>>> Unpacking libbsd-0.8.3.tar.xz to /var/tmp/portage/dev-libs/libbsd-0.8.3/work
>>> Source unpacked in /var/tmp/portage/dev-libs/libbsd-0.8.3/work
>>> Preparing source in /var/tmp/portage/dev-libs/libbsd-0.8.3/work/libbsd-0.8.3 ...
>>> Source prepared.
...
{% endhighlight %}

![Docker Emerge: onlydeps][image-ss-emerge-onlydeps]{: .img-responsive }

If you need a more verbose message,
all you need is <code>--ask</code>.

{% highlight bash %}
$ emerge --ask --onlydeps herbstluftwm

These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild  N     ] dev-libs/libbsd-0.8.3  USE="-static-libs" ABI_X86="(64) -32 (-x32)" 
[ebuild  N     ] x11-proto/xproto-7.0.31  USE="-doc" ABI_X86="(64) -32 (-x32)" 
[ebuild  N     ] x11-proto/xextproto-7.3.0  USE="-doc" ABI_X86="(64) -32 (-x32)" 
[ebuild  N     ] media-fonts/font-util-1.3.1 
[ebuild  N     ] x11-misc/util-macros-1.19.1 
[ebuild  N     ] x11-libs/xtrans-1.3.5  USE="-doc" 
[ebuild  N     ] x11-proto/xf86bigfontproto-1.2.0-r1  ABI_X86="(64) -32 (-x32)" 
{% endhighlight %}

![Docker Emerge: onlydeps-ask][image-ss-emerge-onlydeps-ask]{: .img-responsive }

#### Package Removal

	Depclean

{% highlight bash %}
$ emerge -c htop
{% endhighlight %}

	Unmerge

{% highlight bash %}
$ emerge -C htop
{% endhighlight %}

![Docker Emerge: Remove][image-ss-emerge-remove]{: .img-responsive }

#### Package Query Search

	Search

{% highlight bash %}
$ emerge -s htop
{% endhighlight %}

	Search Description

{% highlight bash %}
$ emerge -S htop
{% endhighlight %}

![Docker Emerge: Search][image-ss-emerge-search]{: .img-responsive }

#### Package Show Info

{% highlight bash %}
$ emerge -pv htop
{% endhighlight %}

{% highlight bash %}
$ emerge -S htop
{% endhighlight %}

#### Package File List

TBD

-- -- --

### Dependency

There are two main topics in dependency,
_dependency_ itself, and _reverse dependency_.
Beside these two, there are other topic as well,
such as _managing conflict_ that we do not cover here.

#### Dependency

	Package that required by: such as man need groff and other.

This _dependency_ information can be achieved by <code>-ep man</code> command.
This will show required parts of the package.

{% highlight bash %}
$ emerge -ep man

These are the packages that would be merged, in order:

Calculating dependencies... done!

...
[ebuild   R    ] dev-libs/iniparser-3.1-r1 
[ebuild   R    ] sys-apps/groff-1.22.2 
[ebuild   R    ] sys-devel/gettext-0.19.8.1 
...
{% endhighlight %}

![Docker Emerge: EP][image-ss-emerge-ep]{: .img-responsive }

#### Reverse Dependency

	Package that require: such as groff needed by man or other.

This _reverse dependency_ require <code>equery depends</code> command.

{% highlight bash %}
$ equery depends groff
 * These packages depend on groff:
sys-apps/man-1.6g (>=sys-apps/groff-1.19.2-r1)
{% endhighlight %}

![Docker Equery: Depends][image-ss-equery-depends]{: .img-responsive }

#### Pretend

Consider pretend to remove the <code>groff</code> package.
These commands have the same result.

{% highlight bash %}
$ emerge -pv --depclean groff
{% endhighlight %}

{% highlight bash %}
$ emerge -c --verbose groff 

Calculating dependencies... done!
  sys-apps/groff-1.22.2 pulled in by:
    sys-apps/man-1.6g requires >=sys-apps/groff-1.19.2-r1

>>> No packages selected for removal by depclean
Packages installed:   233
Packages in world:    6
Packages in system:   44
Required packages:    233
Number removed:       0
{% endhighlight %}

![Docker Emerge: PV Depclean][image-ss-emerge-pv-depclean]{: .img-responsive }

#### Verify

Gentoo has a tool, to install or repair, missing dependencies.

{% highlight bash %}
$ emerge -uDN world
Calculating dependencies... done!
>>> Auto-cleaning packages...

>>> No outdated packages were found on your system.
{% endhighlight %}

![Docker Emerge: Verify Dependency][image-ss-emerge-verify]{: .img-responsive }

-- -- --

### Group

	There is no group concept in portage.

I have been looking for a concept of
[group or pattern or metapackages] in portage.
And found something better called <code>sets</code>.

#### Metapackages

If you insist to use metapackage,
you can still use the search feature.

{% highlight bash %}
$ emerge -s meta
{% endhighlight %}

#### Sets

Official

*	<https://wiki.gentoo.org/wiki//etc/portage/sets>

Awesome Blog

*	<https://makuro.wordpress.com/2010/12/12/intro-to-portage-sets/>

-- -- --

### System Wide

There is this <code>emerge --info</code> command
to dump the system wide information.

#### Emerge

{% highlight bash %}
$ emerge --info
Portage 2.3.8 (python 3.4.5-final-0, default/linux/amd64/13.0, gcc-5.4.0, glibc-2.23-r4, 4.9.44-1-lts x86_64)
=================================================================
System uname: Linux-4.9.44-1-lts-x86_64-Intel-R-_Core-TM-2_Duo_CPU_T7300_@_2.00GHz-with-gentoo-2.3
KiB Mem:     1921768 total,    304060 free
KiB Swap:    3431420 total,   3431016 free
Timestamp of repository gentoo: Mon, 18 Sep 2017 00:45:02 +0000
Head commit of repository gentoo: 6bbdcd277441846d6b9c1dd01fd75f1835cbd0cc
Timestamp of repository xwing: Mon, 04 Sep 2017 16:00:05 +0000
sh bash 4.3_p48-r1
...
{% endhighlight %}

![Docker Emerge: Info][image-ss-emerge-info]{: .img-responsive }

#### evdep-rebuild

Verify integrity of package database, such as dependencies.

{% highlight bash %}
$ revdep-rebuild
 * This is the new python coded version
 * Please report any bugs found using it.
 * The original revdep-rebuild script is installed as revdep-rebuild.sh
 * Please file bugs at: https://bugs.gentoo.org/
 * Collecting system binaries and libraries
 * Checking dynamic linking consistency

Your system is consistent
{% endhighlight %}

![Docker Gentoo: revdep-rebuild][image-ss-revdep-rebuild]{: .img-responsive }

#### Upgradables

There are also this command that show upgradable packages.

{% highlight bash %}
$ emerge -uDNp world

These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild  rR    ] app-arch/bzip2-1.0.6-r8 [1.0.6-r8]
[ebuild     U  ] sys-apps/gentoo-functions-0.12 [0.10]
[ebuild  rR    ] sys-libs/zlib-1.2.11 [1.2.11]
[ebuild     U  ] dev-libs/openssl-1.0.2l [1.0.2k]
[ebuild     U  ] dev-libs/libgcrypt-1.8.1 [1.7.8]
[ebuild     U  ] dev-libs/libtasn1-4.12-r1 [4.10-r2]
[ebuild  rR    ] dev-lang/python-3.4.5 
[ebuild  rR    ] dev-lang/python-2.7.12 
[ebuild     U  ] dev-libs/libxml2-2.9.4-r3 [2.9.4-r1]
[ebuild     U  ] dev-libs/libpcre-8.41 [8.40-r1]
[ebuild  rR    ] net-misc/openssh-7.5_p1-r1 

The following packages are causing rebuilds:

  (sys-libs/zlib-1.2.11:0/1::gentoo, ebuild scheduled for merge) causes rebuilds for:
    (net-misc/openssh-7.5_p1-r1:0/0::gentoo, ebuild scheduled for merge)
    (dev-lang/python-2.7.12:2.7/2.7::gentoo, ebuild scheduled for merge)
    (dev-lang/python-3.4.5:3.4/3.4m::gentoo, ebuild scheduled for merge)
  (app-arch/bzip2-1.0.6-r8:0/1::gentoo, ebuild scheduled for merge) causes rebuilds for:
    (dev-lang/python-2.7.12:2.7/2.7::gentoo, ebuild scheduled for merge)
    (dev-lang/python-3.4.5:3.4/3.4m::gentoo, ebuild scheduled for merge)
{% endhighlight %}

![Docker Emerge: List Upgradables][image-ss-emerge-undp]{: .img-responsive }

You may consider to use <code>--columns</code> for prettier output.

{% highlight bash %}
$ emerge --update --deep --newuse --pretend --columns world
{% endhighlight %}

-- -- --

### Clean Up

Keep your system neat and tidy.

#### depclean

{% highlight bash %}
$ emerge --depclean
...

Calculating dependencies... done!
>>> No packages selected for removal by depclean
>>> To see reverse dependencies, use --verbose
Packages installed:   229
Packages in world:    10
Packages in system:   44
Required packages:    229
Number removed:       0
{% endhighlight %}

![Docker emerge: depclean][image-ss-depclean]{: .img-responsive }

-- -- --

### What's Next

No comment yet.
Just like everything else I have to go deeper to comprehend.
Consider finish reading [ [Part Two][local-part-two] ].

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-gentoo' %}

[local-part-two]: {{ site.url }}/system/2017/08/12/docker-gentoo-portage.html

[image-ss-gentoo-docker]:    {{ asset_post }}/00-getting-started.png

[image-ss-emerge-webrsync]:  {{ asset_post }}/01-webrsync-half.png
[photo-ss-emerge-webrsync]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipN4az7eFD30LhWON67bxjIl07QM5l2DlmxjGSGX?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-emerge-update]:    {{ asset_post }}/01-emerge-update.png

[image-ss-emerge-install]:     {{ asset_post }}/13-emerge-01-install-half.png
[photo-ss-emerge-install]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMZZ4_7ak9JzR1bzgSdjvMft3xCQ5hd8CAZlWjS?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-emerge-ambigous]:		{{ asset_post }}/13-emerge-01-install-mc.png

[image-ss-emerge-install-ask]:	{{ asset_post }}/13-emerge-01-install-ask.png
[image-ss-emerge-remove]:		{{ asset_post }}/13-emerge-02-remove-depclean.png
[image-ss-emerge-search]:		{{ asset_post }}/13-emerge-03-search-search.png
[image-ss-emerge-fetchonly]:	{{ asset_post }}/13-emerge-fetchonly.png
[image-ss-emerge-onlydeps]:		{{ asset_post }}/13-emerge-onlydeps.png
[image-ss-emerge-onlydeps-ask]:	{{ asset_post }}/13-emerge-onlydeps-ask.png

[image-ss-emerge-ep]:		{{ asset_post }}/14-emerge-ep.png
[image-ss-equery-depends]:	{{ asset_post }}/14-equery-depends.png
[image-ss-emerge-pv-depclean]:	{{ asset_post }}/14-emerge-pv-depclean.png
[image-ss-emerge-verify]:	{{ asset_post }}/14-emerge-udn-world.png

[image-ss-portage-source]:	{{ asset_post }}/17-dir-source.png
[image-ss-eclean-distfiles]:	{{ asset_post }}/17-eclean-distfiles.png
[image-ss-eclean-dist-deep]:	{{ asset_post }}/17-eclean-dist-deep.png

[image-ss-less-log]:			{{ asset_post }}/19-log.png
[image-ss-emerge-info]:			{{ asset_post }}/19-emerge-info.png
[image-ss-revdep-rebuild]:		{{ asset_post }}/19-revdep-rebuild.png
[image-ss-depclean]:			{{ asset_post }}/19-depclean.png
[image-ss-emerge-undp]:	{{ asset_post }}/19-emerge-undp.png
