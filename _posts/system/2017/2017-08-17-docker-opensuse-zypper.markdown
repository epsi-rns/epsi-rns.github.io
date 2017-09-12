---
layout: post
title: "Docker - openSUSE Zypper - Part Three"
date: 2017-08-17 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, opensuse]
author: epsi

excerpt:
  Examine zypper step by step,
  using openSUSE container in Docker
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
# - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-opensuse-zypper.html %}

-- -- --

### Group

I cannot find any reference about group in Zypper.
Although there is group concept in YaST.

#### Metapackage

Neither metapackage exist in Zypper.

#### Pattern

The closest concept about group in zypper is,
by using <code>pattern</code> package.

{% highlight bash %}
$ zypper pt
Loading repository data...
Reading installed packages...
S | Name                 | Version       | Repository | Dependency
--+----------------------+---------------+------------+-----------
  | apparmor             | 20170319-10.2 | OSS        |           
  | apparmor             | 20170319-10.2 | OSS        |           
  | base                 | 20170319-10.2 | OSS        |           
  | base                 | 20170319-10.2 | OSS        |           
  | basesystem           | 20170319-10.2 | OSS        |           
  | basesystem           | 20170319-10.2 | OSS        |           
  | books                | 20170319-4.1  | OSS        |           
{% endhighlight %}

![Docker Zypper: Pattern][image-ss-zypper-pattern]{: .img-responsive }

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.

{% highlight bash %}
$ less /var/log/zypp/history
# 2017-08-23 08:57:51 man-2.7.6-3.3.x86_64.rpm installed ok
# Additional rpm output:
# Updating /etc/sysconfig/cron ...
# 
2017-08-23 08:57:51|install|man|2.7.6-3.3|x86_64|root@d2e88a46e111|oss|21490bfff69e98449f8ae00bb9e91b15038566ca|
2017-08-23 10:01:09|command|root@d2e88a46e111|'zypper' 'install' '--force' 'man' 'nano' 'htop' 'ncdu' 'fish'|
2017-08-23 10:01:12|install|htop|2.0.2-3.4|x86_64|root@d2e88a46e111|oss|2807afd80fa606228799ab76baddfc2e688d60b8|
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/zypp/history][image-ss-less-log]{: .img-responsive }

-- -- --

### Clean Up

Opensuse as default does not keep downloaded package,
unless <code>keeppackages=1</code>
sets in <code class="code-file">/etc/zypp/repos.d/</code>.
But sometimes cache files left for some reason.

Package Cache
	
*	/var/cache/zypp/packages/ * /x86_64/ * .x86_64.rpm
	
*	/var/cache/zypp/packages/ * /suse/noarch/ * .noarch.rpm

{% highlight bash %}
$ ls -lR /var/cache/zypp/packages/
{% endhighlight %}

![Docker Zypper: Cache][image-ss-zypper-cache]{: .img-responsive }

You can clean these directory.

{% highlight bash %}
$ zypper clean
{% endhighlight %}

![Docker Zypper: Clean][image-ss-zypper-clean]{: .img-responsive }

-- -- --

### Build from Source

Zypper has the capability to download the source code.
Then we can utilize other tool to build from source.

#### General Requirement

We require to install <code>rpm-build</code> and toolchain.

{% highlight bash %}
$ zypper in gcc make rpm-build
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following 28 NEW packages are going to be installed:
  binutils cpp cpp7 dwz gcc gcc7 gettext-runtime gettext-tools libasan4
  libatomic1 libcilkrts5 libcroco-0_6-3 libgomp1 libisl15 libitm1 liblsan0
  libmpc3 libmpfr4 libmpx2 libmpxwrappers2 libtsan0 libubsan0 make patch
  rpm-build systemd-rpm-macros tar which

The following package is going to be upgraded:
  libgcc_s1

1 package to upgrade, 28 new.
Overall download size: 38.9 MiB. Already cached: 0 B. After the operation,
additional 148.1 MiB will be used.
Continue? [y/n/...? shows all options] (y):
{% endhighlight %}

![Docker Zypper: Install Toolchain][image-ss-install-toolchain]{: .img-responsive }

#### Download and Extract

	I choose herbstluftwm as an example.

Source install in zypper will *download* source code,
and *extract* to into <code class="code-file">/usr/src/packages/</code>.
Downloading also install required package dependencies.

{% highlight bash %}
$ zypper source-install herbstluftwm
Retrieving repository 'Window Managers' metadata .............[done]
Building repository 'Window Managers' cache ..................[done]
...

The following 213 NEW packages are going to be installed:
  Mesa Mesa-libEGL-devel Mesa-libEGL1 Mesa-libGL-devel Mesa-libGL1
  ...
  xorg-x11-devel xorg-x11-util-devel xproto-devel xtrans xz-devel
  zlib-devel

The following source package is going to be installed:
  herbstluftwm

The following 5 packages are going to be upgraded:
  glibc glibc-locale libgcrypt20 libstdc++6 python-base

5 packages to upgrade, 213 new, 1 source package.
Overall download size: 77.4 MiB. Already cached: 0 B. After the
operation, additional 281.5 MiB will be used.
Continue? [y/n/...? shows all options] (y):
{% endhighlight %}

Please click the figure for longer content.

[![Docker Zypper: Install Source][image-ss-install-source]{: .img-responsive }][photo-ss-install-source]

#### Source Path

Now we have these directories,
we need the herbstluftwm.spec.

{% highlight bash %}
$ ls /usr/src/packages/      
BUILD  BUILDROOT  RPMS	SOURCES  SPECS	SRPMS
{% endhighlight %}

{% highlight bash %}
$ ls -l /usr/src/packages/SPECS/                  
total 8
-rw-r--r-- 1 root root 4468 Aug 31 16:19 herbstluftwm.spec
{% endhighlight %}

![Docker Source: /usr/src/packages/SPECS/][image-ss-usr-src-specs]{: .img-responsive }

Consider use SPECS path as working directory for build.

{% highlight bash %}
$ cd /usr/src/packages/SPECS/
{% endhighlight %}

#### Build

Just one command <code>rpmbuild -ba</code>.

{% highlight bash %}
$ rpmbuild -ba herbstluftwm.spec
Executing(%prep): /bin/sh -e /var/tmp/rpm-tmp.bXhF1w
+ umask 022
+ cd /usr/src/packages/BUILD
+ cd /usr/src/packages/BUILD
+ rm -rf herbstluftwm-1335135043
+ /usr/bin/bzip2 -dc /usr/src/packages/SOURCES/herbstluftwm-1335135043.tar.bz2
...
Executing(%clean): /bin/sh -e /var/tmp/rpm-tmp.SxVkky
+ umask 022
+ cd /usr/src/packages/BUILD
+ cd herbstluftwm-1335135043
+ rm -rf /usr/src/packages/BUILDROOT/herbstluftwm-1335135043-3.280.x86_64
+ exit 0
{% endhighlight %}

![Docker RPM: rpmbuild -ba][image-ss-rpmbuild-ba]{: .img-responsive }

#### Install The RPM Output

Consider check if the output exist, and install.

{% highlight bash %}
$ ls -l /usr/src/packages/RPMS/x86_64/
total 152
-rw-r--r-- 1 root root 152794 Sep  3 17:28 herbstluftwm-1335135043-3.280.x86_64.rpm
{% endhighlight %}

![Docker Source: /usr/src/packages/RPMS/][image-ss-usr-src-rpms]{: .img-responsive }

{% highlight bash %}
$ rpm -iv /usr/src/packages/RPMS/x86_64/herbstluftwm-1335135043-3.280.x86_64.rpm 
Preparing packages...
herbstluftwm-1335135043-3.280.x86_64
{% endhighlight %}

![Docker RPM: rpm -iv][image-ss-rpm-iv]{: .img-responsive }

Now we are done.

-- -- --

### Hold Package

Zypper hold package using lock mechanism.
This is just terminology.

#### Case Example

Suppose you want to do system upgrade,
but you do not want to upgrade certain package.
There is a good reason for these,
such as keeping old driver,
because the latest has a issue or such reason.
Or maybe we want to keep our current beloved newly compiled herbstluftwm
that equipped with super duper specific configuration parameter optimization.

{% highlight bash %}
$ zypper dup

Warning: ...
...
The following package is going to change vendor:
  herbstluftwm   -> obs://build.opensuse.org/X11
...
{% endhighlight %}

![Docker Zypper: dup unlocked][image-ss-zypper-h-dup-unlock]{: .img-responsive }

You can skip or ignore by hold package
so that the package will kept intact
while doing system upgrade.

#### Add Lock

We can add lock easily using zypper.

{% highlight bash %}
$ zypper al herbstluftwm
Specified lock has been successfully added.
{% endhighlight %}

This will put new package entry in <code>/etc/zypp/locks</code>.

{% highlight bash %}
$ cat /etc/zypp/locks 

type: package
match_type: glob
case_sensitive: on
solvable_name: herbstluftwm
{% endhighlight %}

Now you can view the entry in nice table.

{% highlight bash %}
$ zypper ll

# | Name         | Type    | Repository
--+--------------+---------+-----------
1 | herbstluftwm | package | (any)  
{% endhighlight %}

![Docker Zypper: Add Lock][image-ss-zypper-h-add-lock]{: .img-responsive }

#### Test Example

Now we have different result
when doing the same <code>zypper dup</code> command.

{% highlight bash %}
$ zypper dup
Warning: ...
...

The following item is locked and will not be changed by any action:
 Installed:
  herbstluftwm
{% endhighlight %}

![Docker Zypper: dup locked][image-ss-zypper-h-dup-lock]{: .img-responsive }

#### Remove Lock

You can unlock package by removing list 
from configuration <code class="code-file">/etc/zypp/locks</code> 
or command line 

{% highlight bash %}
$ zypper rl herbstluftwm
1 lock has been successfully removed.
{% endhighlight %}

![Docker Zypper: Remove Lock][image-ss-zypper-h-rem-lock]{: .img-responsive }

-- -- --

### Unsolved Issues on Minimal Install

This is my bad. Not openSUSE's fault.

#### No Manual

No manual in openSUSE Docker.
I have two others openSUSE full installation in PC,
and all manual works well.

{% highlight bash %}
$ man man
No manual entry for man

$ echo $MANPATH

$ cat /etc/manpath.config
{% endhighlight %}

I have tried to reinstall, but it doesn't work.

{% highlight bash %}
$ zypper in -f man man-pages man-pages-posix 
{% endhighlight %}

I still do not know what to do about it.

-- -- --

### Interesting Issue

#### systemd Dependencies

While solving the _no manual_ problem,
I encountered another issue.

{% highlight bash %}
$ zypper in man
{% endhighlight %}

This is somehow interesting,
manual pages in openSUSE depend on systemd.

![systemd dependency issue on openSUSE][image-ss-zypper-systemd]{: .img-responsive }

Why would a manual <code>man</code> need to depend to an init ?

{% highlight bash %}
$ zypper info --requires man
Requires : [32]
cron

$ zypper info --requires cron
Requires : [5]
cronie = 1.5.1-66.3

$ zypper info --requires cronie
Requires : [26]
systemd
{% endhighlight %}

-- -- --

### Conclusion

	These are just preliminary knowledge about Zypper.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-opensuse' %}

[image-ss-zypper-systemd]: {{ asset_post }}/13-install-man-systemd-issue.png

[image-ss-zypper-pattern]: {{ asset_post }}/15-pattern.png

[image-ss-zypper-cache]: {{ asset_post }}/17-cache.png
[image-ss-zypper-clean]: {{ asset_post }}/17-clean.png

[image-ss-less-log]:     {{ asset_post }}/19-log.png

[image-ss-install-toolchain]:  {{ asset_post }}/25-install-toolchain.png
[image-ss-install-source]:     {{ asset_post }}/25-source-install-herbstluftwm-half.png
[image-ss-rpmbuild-ba]:        {{ asset_post }}/25-rpmbuild.png
[image-ss-rpm-iv]:             {{ asset_post }}/25-rpm-iv.png
[image-ss-usr-src-rpms]:       {{ asset_post }}/25-usr-src-packages-rpms-x86-64.png
[image-ss-usr-src-specs]:      {{ asset_post }}/25-usr-src-packages-specs.png

[photo-ss-install-source]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOvBGGd9_F4XOFUUup12Q8qhy5YiFwWXKD22oNw?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-zypper-h-add-lock]:   {{ asset_post }}/27-add-lock.png
[image-ss-zypper-h-dup-lock]:   {{ asset_post }}/27-dup-lock.png
[image-ss-zypper-h-dup-unlock]: {{ asset_post }}/27-dup-unlock.png
[image-ss-zypper-h-rem-lock]:   {{ asset_post }}/27-remove-lock.png
