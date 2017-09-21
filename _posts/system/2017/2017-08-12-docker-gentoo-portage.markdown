---
layout: post
title: "Docker - Gentoo Portage - Part Two"
date: 2017-08-12 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, gentoo]
author: epsi

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

It can also query all packages including not installed packages.
The process take long and need a lot of CPU.

{% highlight bash %}
$ equery depends - agroff
{% endhighlight %}

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

System wide information.

#### Emerge

There is this <code>emerge --info</code> command
to dump the system wide information.


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

#### Upgradable Packages

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

![Docker Emerge: List Upgradable Packages][image-ss-emerge-undp]{: .img-responsive }

You may consider to use <code>--columns</code> for prettier output.

{% highlight bash %}
$ emerge --update --deep --newuse --pretend --columns world
{% endhighlight %}

#### Installed Packages

Reinstall target and entire deep dependency tree,
as though no packages are currently installed.

{% highlight bash %}
$ emerge -ep world

These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild   R    ] dev-lang/python-exec-2.4.4 
[ebuild   R    ] virtual/libintl-0-r2 
[ebuild   R    ] sys-libs/ncurses-6.0-r1 
[ebuild   R    ] app-arch/bzip2-1.0.6-r8 [1.0.6-r8]
[ebuild   R    ] sys-devel/gnuconfig-20161104 
[ebuild     U  ] sys-apps/gentoo-functions-0.12 [0.10]
[ebuild   R    ] virtual/libiconv-0-r2 
...
{% endhighlight %}

![Docker Emerge: Installed Packages][image-ss-emerge-ep]{: .img-responsive }

You may consider to use <code>--columns</code> for prettier output.

{% highlight bash %}
$ emerge --emptytree --pretend --columns @world
{% endhighlight %}

#### Obsolete Packages

Obsolete packages in current system.

{% highlight bash %}
$ eix-test-obsolete
...
Redundant in /etc/portage/package.mask:

... considered as REDUNDANT_IF_MASK_NO_CHANGE
[I] dev-lang/perl (5.24.1-r2@08/03/17): Larry Wall's Practical Extraction and Report Language
[I] sys-apps/openrc (0.28@08/15/17): OpenRC manages the services, startup and shutdown of a host
[I] sys-devel/libtool (2.4.6-r3(2)@08/03/17): A shared library tool for developers
[I] sys-process/procps (3.3.12@08/03/17): standard informational utilities and process-handling tools
Found 4 matches
...
Installed packages with a version not in the database (or masked):
[?] sys-apps/file (5.31@09/18/17 -> 5.30): identify a file's format by scanning binary data for patterns
[?] sys-apps/portage (2.3.8@09/18/17 -> 2.3.6): Portage is the package management and distribution system for Gentoo
Found 2 matches
{% endhighlight %}

![Docker Gentoo: eix-test-obsolete][image-ss-eix-test-obsolete]{: .img-responsive }

#### Recent Packages

Packages that has been changed recently in repository.

{% highlight bash %}
$ eix-diff
[>]   == app-admin/ansible (2.3.1.0^t -> 2.3.2.0-r1^t): Model-driven deployment, config management, and command execution framework
[>]   == app-admin/logsurfer+ (1.8-r2^d -> 1.8-r3^d): Real Time Log Monitoring and Alerting
[>]   == app-arch/par2cmdline (0.6.12 -> 0.7.3): A PAR-2.0 file verification and repair tool
...
{% endhighlight %}

![Docker Gentoo: eix-diff][image-ss-eix-diff]{: .img-responsive }

-- -- --

### Package

#### Building Package

{% highlight bash %}
$ quickpkg ncdu
 * Building package for sys-fs/ncdu-1.12 ...                 [ ok ]

 * Packages now in '/usr/portage/packages':
 * sys-fs/ncdu-1.12: 50.8K
{% endhighlight %}

Now it is there.

{% highlight bash %}
$ ls -l /usr/portage/packages/sys-fs/
total 52
-rw------- 1 root root 52025 Sep 21 06:12 ncdu-1.12.tbz2
{% endhighlight %}

![Docker Gentoo: quickpkg][image-ss-quickpkg]{: .img-responsive }

#### Extract

{% highlight bash %}
$ cd ~
$ tar -jxvf /usr/portage/packages/sys-fs/ncdu-1.12.tbz2
tar -jxvf /usr/portage/packages/sys-fs/ncdu-1.12.tbz2
./usr/
./usr/bin/
./usr/bin/ncdu

bzip2: (stdin): trailing garbage after EOF ignored
./usr/share/
./usr/share/doc/
./usr/share/doc/ncdu-1.12/
./usr/share/doc/ncdu-1.12/ChangeLog.bz2
./usr/share/doc/ncdu-1.12/README.bz2
./usr/share/man/
./usr/share/man/man1/
./usr/share/man/man1/ncdu.1.bz2

$ ls -l
total 4
drwxr-xr-x 4 root root 4096 Aug 15 19:31 usr
{% endhighlight %}

![Docker Gentoo: extract package][image-ss-extract]{: .img-responsive }

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.

{% highlight bash %}
$ less /var/log/portage/elog/summary.log
>>> Messages generated by process 282 on 2017-08-22 16:59:40 -00 for
 package sys-process/htop-2.0.2:

WARN: setup
To use lsof features in htop (what processes are accessing
what files), you must have sys-process/lsof installed.
Unable to find kernel sources at /usr/src/linux
Unable to calculate Linux Kernel version for build, attempting to use running version
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/portage/elog/summary.log][image-ss-less-log]{: .img-responsive }

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

#### Cache

Time after time, your portage source directory,
may growing bigger and bigger in size.
You can clean these directory.

Package Source
	
*	/usr/portage/distfiles

{% highlight bash %}
$ ls -lR /usr/portage/distfiles
{% endhighlight %}

![Docker portage: Source][image-ss-portage-source]{: .img-responsive }

This will only remove package if there are newer version.
This command takes long time.
Do not do that when you are very tired.

{% highlight bash %}
$ eclean distfiles
 * Building file list for distfiles cleaning...
 * Your distfiles directory was already clean.
{% endhighlight %}

![Docker portage: Clean Dist][image-ss-eclean-distfiles]{: .img-responsive }

This will will keep only installed package.

{% highlight bash %}
$ eclean-dist -d
 * Building file list for distfiles cleaning...
 * Cleaning distfiles...
 [    3.3 M ] fish-2.4.0.tar.gz
 [  465.2 K ] htop-2.0.2.tar.gz
 ===========
 [    3.8 M ] Total space from 2 files were freed in the distfiles directory

{% endhighlight %}

![Docker portage: Clean Deep][image-ss-eclean-dist-deep]{: .img-responsive }

-- -- --

### What's Next

Still no comment.  Just like everything else. I still need to dive deeper.
Consider finish reading [ [Part Three][local-part-three] ].

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-gentoo' %}

[local-part-three]: {{ site.url }}/system/2017/08/13/docker-gentoo-portage.html

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
[image-ss-emerge-ep]:	{{ asset_post }}/19-emerge-ep.png

[image-ss-portage-source]:     {{ asset_post }}/17-dir-source.png
[image-ss-eclean-distfiles]:   {{ asset_post }}/17-eclean-distfiles.png
[image-ss-eclean-dist-deep]:   {{ asset_post }}/17-eclean-dist-deep.png

[image-ss-less-log]:           {{ asset_post }}/19-log.png

[image-ss-eix-test-obsolete]:	{{ asset_post }}/19-eix-test-obsolete.png
[image-ss-eix-diff]:			{{ asset_post }}/19-eix-diff.png

[image-ss-quickpkg]:	{{ asset_post }}/29-quickpkg.png
[image-ss-extract]:		{{ asset_post }}/29-extract.png
