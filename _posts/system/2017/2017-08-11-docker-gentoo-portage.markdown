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

### Package IRSI

	Install, Remove, Search, Info

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

[image-ss-emerge-ambigous]:    {{ asset_post }}/13-emerge-01-install-mc.png

[image-ss-emerge-install-ask]: {{ asset_post }}/13-emerge-01-install-ask.png
[image-ss-emerge-remove]:      {{ asset_post }}/13-emerge-02-remove-depclean.png
[image-ss-emerge-search]:      {{ asset_post }}/13-emerge-03-search-search.png

[image-ss-emerge-ep]:          {{ asset_post }}/14-emerge-ep.png
[image-ss-equery-depends]:     {{ asset_post }}/14-equery-depends.png
[image-ss-emerge-pv-depclean]: {{ asset_post }}/14-emerge-pv-depclean.png
[image-ss-emerge-verify]:      {{ asset_post }}/14-emerge-udn-world.png

[image-ss-portage-source]:     {{ asset_post }}/17-dir-source.png
[image-ss-eclean-distfiles]:   {{ asset_post }}/17-eclean-distfiles.png
[image-ss-eclean-dist-deep]:   {{ asset_post }}/17-eclean-dist-deep.png

[image-ss-less-log]:           {{ asset_post }}/19-log.png
