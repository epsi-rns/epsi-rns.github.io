---
layout: post
title: "Docker - Gentoo Portage - Part One"
date      : 2017-08-11 09:45:15 +0700
categories: system
tags      : [docker, distro, package manager, gentoo]
keywords  : [emerge]
author: epsi

opengraph:
  image: /assets/site/images/topics/docker.png

excerpt:
  Docker flow for Gentoo Portage,
  from emerge-webrsync to manual rsync.
  (not so) first time using Gentoo experience.
  One of Three Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
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

#### Change Log

{% highlight bash %}
$ equery changes ncdu
*ncdu-1.12 (29 Aug 2016)

  29 Aug 2016; Jeroen Roovers <jer@gentoo.org> +ncdu-1.12.ebuild:
  Version bump.

  Package-Manager: portage-2.3.0

  29 Aug 2016; Jeroen Roovers <jer@gentoo.org>
  -files/ncdu-1.9-pkgconfig.patch:
  Drop obsolete patch.

  Package-Manager: portage-2.3.0
{% endhighlight %}

![Docker Gentoo: equery changes][image-ss-equery-changes]{: .img-responsive }

All change log.

{% highlight bash %}
$ equery changes -f ncdu
{% endhighlight %}

#### Check

Verification

{% highlight bash %}
$ equery check ncdu
* Checking sys-fs/ncdu-1.12 ...
   11 out of 11 files passed
{% endhighlight %}

![Docker Gentoo: equery check][image-ss-equery-check]{: .img-responsive }

Check for Possible Issue

{% highlight bash %}
$ repoman ncdu
{% endhighlight %}


#### Package File List

Listing package files can be achieved using <code>equery files</code>.

{% highlight bash %}
$ equery files ncdu
 * Searching for ncdu ...
 * Contents of sys-fs/ncdu-1.12:
/usr
/usr/bin
/usr/bin/ncdu
/usr/share
/usr/share/doc
/usr/share/doc/ncdu-1.12
/usr/share/doc/ncdu-1.12/ChangeLog.bz2
/usr/share/doc/ncdu-1.12/README.bz2
/usr/share/man
/usr/share/man/man1
/usr/share/man/man1/ncdu.1.bz2
{% endhighlight %}

![Docker Gentoo: equery files][image-ss-equery-files]{: .img-responsive }

Searching file owner

{% highlight bash %}
$ equery belongs /etc/man.conf 
 * Searching for /etc/man.conf ... 
sys-apps/man-1.6g (/etc/man.conf)
{% endhighlight %}

![Docker Gentoo: equery belong][image-ss-equery-belong]{: .img-responsive }

-- -- --

### What's Next

No comment yet. I know nothing.
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
[image-ss-equery-belong]:		{{ asset_post }}/13-equery-belong.png
[image-ss-equery-files]:		{{ asset_post }}/13-equery-files.png
[image-ss-equery-changes]:		{{ asset_post }}/13-equery-changes.png
[image-ss-equery-check]:		{{ asset_post }}/13-equery-check.png
