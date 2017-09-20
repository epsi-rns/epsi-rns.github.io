---
layout: post
title: "Docker - openSUSE Zypper - Part Four"
date: 2017-08-18 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, opensuse]
author: epsi

excerpt:
  Examine zypper step by step,
  using openSUSE container in Docker
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081845  # Fedora DNF
# - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-opensuse-zypper.html %}

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

#### Build Dependencies

You can also install only updated dependencies.
This happen sometimes after a while,
like after a month, there should be package update in tumbleweed.

{% highlight bash %}
$ zypper si --build-deps-only herbstluftwm
Reading installed packages...
Loading repository data...
Resolving package dependencies...

The following 2 NEW packages are going to be installed:
  gcc-c++ gcc7-c++

2 new packages to install.
Overall download size: 9.3 MiB. Already cached: 0 B. After the
operation, additional 25.6 MiB will be used.
Continue? [y/n/...? shows all options] (y): 
{% endhighlight %}

[![Docker Zypper: --build-deps-only][image-ss-build-deps-only]{: .img-responsive }

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

Now we are almost done.

#### rpmlint

Check for possible issue, similar to lintian or namcap.

{% highlight bash %}
$ rpmlint herbstluftwm-0.7.0-2.1.x86_64.rpm 
herbstluftwm.x86_64: W: unstripped-binary-or-object /usr/bin/herbstclient
herbstluftwm.x86_64: W: unstripped-binary-or-object /usr/bin/herbstluftwm
herbstluftwm.x86_64: W: position-independent-executable-suggested /usr/bin/herbstclient
herbstluftwm.x86_64: W: position-independent-executable-suggested /usr/bin/herbstluftwm
herbstluftwm.x86_64: W: no-manual-page-for-binary dmenu_run_hlwm
herbstluftwm.x86_64: W: install-file-in-docs /usr/share/doc/herbstluftwm/INSTALL
herbstluftwm.x86_64: W: file-contains-current-date /usr/bin/herbstclient
herbstluftwm.x86_64: W: file-contains-current-date /usr/bin/herbstluftwm
1 packages and 0 specfiles checked; 0 errors, 8 warnings.
{% endhighlight %}

![Docker openSUSE: rpmlint][image-ss-rpmlint]{: .img-responsive }

### Inspect Files

Often we need to inspect <code>.rpm</code> package.
No need any <code>zypper</code> command,
as this more like an <code>rpm</code> issue.

#### rpm -Qpl

You can also query file <code>-Qpl</code> directly to the <code>.rpm</code> package.

{% highlight bash %}
$ rpm -qpl /usr/src/packages/RPMS/x86_64/herbstluftwm-0.7.0-2.1.x86_64.rpm 
/etc/bash_completion.d/herbstclient-completion
/etc/xdg/herbstluftwm
/etc/xdg/herbstluftwm/autostart
/etc/xdg/herbstluftwm/panel.sh
/etc/xdg/herbstluftwm/restartpanels.sh
/usr/bin/dmenu_run_hlwm
/usr/bin/herbstclient
/usr/bin/herbstluftwm
/usr/share/doc/herbstluftwm
/usr/share/doc/herbstluftwm/BUGS
{% endhighlight %}

![Docker openSUSE: rpm -qpl][image-ss-rpm-qpl]{: .img-responsive }

#### rpmls

Or use the <code>rpmls</code> from the <code>rpmdevtools</code> package

{% highlight bash %}
$ rpmls /usr/src/packages/RPMS/x86_64/herbstluftwm-0.7.0-2.1.x86_64.rpm 
-rw-r--r--  /etc/bash_completion.d/herbstclient-completion
drwxr-xr-x  /etc/xdg/herbstluftwm
-rwxr-xr-x  /etc/xdg/herbstluftwm/autostart
-rwxr-xr-x  /etc/xdg/herbstluftwm/panel.sh
-rwxr-xr-x  /etc/xdg/herbstluftwm/restartpanels.sh
-rwxr-xr-x  /usr/bin/dmenu_run_hlwm
-rwxr-xr-x  /usr/bin/herbstclient
-rwxr-xr-x  /usr/bin/herbstluftwm
drwxr-xr-x  /usr/share/doc/herbstluftwm
-rw-r--r--  /usr/share/doc/herbstluftwm/BUGS
{% endhighlight %}

![Docker openSUSE: rpmls][image-ss-rpmls]{: .img-responsive }

#### rpm2cpio

This one also extract.

{% highlight bash %}
$ rpm2cpio /usr/src/packages/RPMS/x86_64/herbstluftwm-0.7.0-2.1.x86_64.rpm | cpio -vid
./etc/bash_completion.d/herbstclient-completion
./etc/xdg/herbstluftwm
./etc/xdg/herbstluftwm/autostart
./etc/xdg/herbstluftwm/panel.sh
./etc/xdg/herbstluftwm/restartpanels.sh
./usr/bin/dmenu_run_hlwm
./usr/bin/herbstclient
./usr/bin/herbstluftwm
./usr/share/doc/herbstluftwm
./usr/share/doc/herbstluftwm/BUGS
{% endhighlight %}

![Docker openSUSE: rpm2cpio, cpio -vid][image-ss-rpm2cpio-vid]{: .img-responsive }

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

openSUSE minimal install allow the user to remove systemd,
leaving the system without init.

-- -- --

### Conclusion

	These are just preliminary knowledge about Zypper.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-opensuse' %}

[image-ss-zypper-systemd]: {{ asset_post }}/13-install-man-systemd-issue.png

[image-ss-install-toolchain]:	{{ asset_post }}/25-install-toolchain.png
[image-ss-install-source]:	{{ asset_post }}/25-source-install-herbstluftwm-half.png
[image-ss-rpmbuild-ba]:		{{ asset_post }}/25-rpmbuild.png
[image-ss-rpm-iv]:			{{ asset_post }}/25-rpm-iv.png
[image-ss-rpmlint]:			{{ asset_post }}/25-rpmlint.png
[image-ss-usr-src-rpms]:	{{ asset_post }}/25-usr-src-packages-rpms-x86-64.png
[image-ss-usr-src-specs]:	{{ asset_post }}/25-usr-src-packages-specs.png
[image-ss-build-deps-only]:	{{ asset_post }}/25-source-install-build-deps-only.png

[photo-ss-install-source]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOvBGGd9_F4XOFUUup12Q8qhy5YiFwWXKD22oNw?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-zypper-h-add-lock]:   {{ asset_post }}/27-add-lock.png
[image-ss-zypper-h-dup-lock]:   {{ asset_post }}/27-dup-lock.png
[image-ss-zypper-h-dup-unlock]: {{ asset_post }}/27-dup-unlock.png
[image-ss-zypper-h-rem-lock]:   {{ asset_post }}/27-remove-lock.png

[image-ss-rpm-qpl]:			{{ asset_post }}/26-rpm-qpl.png
[image-ss-rpmls]:			{{ asset_post }}/26-rpmls.png
[image-ss-rpm2cpio-vid]:	{{ asset_post }}/26-rpm2cpio.png
