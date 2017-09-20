---
layout: post
title: "Docker - Fedora DNF - Part Four"
date: 2017-08-21 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, fedora]
author: epsi

excerpt:
  Examine DNF step by step,
  using Fedora container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
# - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-fedora-dnf.html %}

-- -- --

### Build from Source

DNF has the capability to download the source code.
Then we can utilize other tool to build from source.

Consider choose <code>herbstluftwm</code> as our guinea pig example.

#### Thank You Community

I was wondering how DNF handle source code,
and I finally figure out a generic steps,
from the very nice *fedora Community in Google Plus*.


Let me quote form "Johan Heikkilä":

	Dnf has capability to install source packages, as long as they are available in the installed/enabled repositories. It is recommended to do the following steps as user and not as root.

In this example:

*	I actually alter the steps a bit for clarity.

*	And I still use root for simplicity. It is just docker experience anyway.

*	Using herbstluftwm as an example case

#### General Requirement

We require to install <code>rpm-build</code>

{% highlight bash %}
$ dnf install rpm-build
Last metadata expiration check: 0:25:14 ago on Sun Sep  3 02:01:23 2017.
Dependencies resolved.
====================================================================
 Package             Arch   Version                   Repository
                                                               Size
====================================================================
Installing:
 rpm-build           x86_64 4.13.90-0.git14002.7.fc28 rawhide 158 k
Upgrading:
 python3-rpm         x86_64 4.13.90-0.git14002.7.fc28 rawhide 115 k
 rpm                 x86_64 4.13.90-0.git14002.7.fc28 rawhide 528 k
...
 xz                  x86_64 5.2.3-4.fc27              rawhide 150 k
 zip                 x86_64 3.0-20.fc27               rawhide 270 k

Transaction Summary
====================================================================
Install  35 Packages
Upgrade   6 Packages

Total download size: 17 M
Is this ok [y/N]:
Downloading Packages:
(1/41): bzip2-1.0.6-24.fc27.x86_64.rpm        44 kB/s |  58 kB     00:01
...
-----------------------------------------------------------------------------
Total                                        565 kB/s |  17 MB     00:30   
...
Running transaction
  Preparing        :                                                     1/1 
  Upgrading        : rpm-plugin-selinux-4.13.90-0.git14002.7.fc28.x8    1/47 
...
Installed:
  rpm-build.x86_64 4.13.90-0.git14002.7.fc28   
...  
Upgraded:
  python3-rpm.x86_64 4.13.90-0.git14002.7.fc28                               
  rpm.x86_64 4.13.90-0.git14002.7.fc28                                       
  rpm-build-libs.x86_64 4.13.90-0.git14002.7.fc28                            
...            

Complete!
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-install-rpmbuild]{: .img-responsive }

And also these toolchain.

{% highlight bash %}
$ dnf install gcc make
{% endhighlight %}

#### Download

Now consider using home directory.

{% highlight bash %}
cd ~
{% endhighlight %}

Since we have already enable <code>rawhide-source</code>,
we can directly download.

{% highlight bash %}
$ dnf download --source herbstluftwm
Fedora - Rawhide - Developmental pa 840 kB/s |  66 MB     01:20    
Last metadata expiration check: 0:01:12 ago on Sun Sep  3 01:53:48 2017.
herbstluftwm-0.6.2-8.fc27.src.rpm    84 kB/s | 228 kB     00:02
{% endhighlight %}

or use <code>--enablerepo</code> and <code>--source</code> instead.

{% highlight bash %}
$ dnf --enablerepo rawhide download --source herbstluftwm
{% endhighlight %}

![Docker DNF: Download Source][image-ss-bs-download-source]{: .img-responsive }

#### Extract

We need to extract the previously downloaded package 
to <code>~/rpmbuild</code> directory.
As you can see below, there are warnings for using root.
For real system, please do not use root.

{% highlight bash %}
$ rpm -ivh herbstluftwm-0.6.2-8.fc27.src.rpm 
warning: herbstluftwm-0.6.2-8.fc27.src.rpm: Header V3 RSA/SHA256 Signature, key ID 9db62fb1: NOKEY
Updating / installing...
   1:herbstluftwm-0.6.2-8.fc27        
warning: user mockbuild does not exist - using root
warning: group mockbuild does not exist - using root
warning: user mockbuild does not exist - using root
warning: group mockbuild does not exist - using root
################################# [100%]
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-rpm-ivh]{: .img-responsive }

Now we have two directories inside.

{% highlight bash %}
$ ls rpmbuild/
SOURCES  SPECS
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-ls-rpmbuild]{: .img-responsive }

#### Dependencies

Since the case is Herbstluftwm, 
we also need require to install these packages
to avoid missing dependencies.

{% highlight bash %}
$ dnf builddep ~/rpmbuild/SPECS/herbstluftwm.spec 
Last metadata expiration check: 2:39:31 ago on Wed Sep 20 14:19:59 2017.
Dependencies resolved.
====================================================================
 Package                Arch     Version            Repository
                                                               Size
====================================================================
Installing:
 glib2-devel            x86_64   2.54.0-1.fc28      rawhide   453 k
 libX11-devel           x86_64   1.6.5-4.fc27       rawhide   984 k
 libXinerama-devel      x86_64   1.1.3-9.fc27       rawhide    17 k
Installing dependencies:
 libX11-xcb             x86_64   1.6.5-4.fc27       rawhide    22 k
 libXau-devel           x86_64   1.0.8-9.fc27       rawhide    18 k
 libXext-devel          x86_64   1.3.3-7.fc27       rawhide    79 k
 libxcb-devel           x86_64   1.12-5.fc27        rawhide   1.0 M
 pcre-cpp               x86_64   8.41-1.fc27.2      rawhide    41 k
 pcre-devel             x86_64   8.41-1.fc27.2      rawhide   548 k
 pcre-utf16             x86_64   8.41-1.fc27.2      rawhide   190 k
 pcre-utf32             x86_64   8.41-1.fc27.2      rawhide   181 k
 xorg-x11-proto-devel   noarch   7.7-23.fc27        rawhide   287 k

Transaction Summary
====================================================================
Install  12 Packages

Total download size: 3.8 M
Installed size: 10 M
Is this ok [y/N]: 
{% endhighlight %}

![Docker DNF: Build Dependency][image-ss-bs-dnf-builddep]{: .img-responsive }

#### Build

Consider get in to <code>~/rpmbuild/SPECS</code> to build.

{% highlight bash %}
$ cd ./rpmbuild/SPECS
{% endhighlight %}

{% highlight bash %}
$ rpmbuild -ba herbstluftwm.spec
...
Executing(%clean): /bin/sh -e /var/tmp/rpm-tmp.jys9eY
+ umask 022
+ cd /root/rpmbuild/BUILD
+ cd herbstluftwm-0.6.2
+ /usr/bin/rm -rf /root/rpmbuild/BUILDROOT/herbstluftwm-0.6.2-8.fc28.x86_64
+ exit 0
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-rpmbuild-ba]{: .img-responsive }

Consider going back to <code>~/rpmbuild</code> directory.
Now we can see additional directories inside as built result.

{% highlight bash %}
$ ls   
BUILD  BUILDROOT  RPMS  SOURCES  SPECS  SRPMS
{% endhighlight %}

And the build output result

{% highlight bash %}
$ ls ~/rpmbuild/RPMS/x86_64
herbstluftwm-0.6.2-8.fc28.x86_64.rpm
herbstluftwm-debuginfo-0.6.2-8.fc28.x86_64.rpm
herbstluftwm-debugsource-0.6.2-8.fc28.x86_64.rpm
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-result-rpm]{: .img-responsive }

#### Install 

Now we can install the previously built rpm.

{% highlight bash %}
$ rpm -iv ~/rpmbuild/RPMS/x86_64/herbstluftwm-0.6.2-8.fc28.x86_64.rpm 
Preparing packages...
herbstluftwm-0.6.2-8.fc28.x86_64
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-rpm-iv-hlwm]{: .img-responsive }

#### Using Mock

There are also Mock solution. But it is beyond this scope.

#### Thank You

I respect the help from community.
Again, thank you.

-- -- --

### Inspect Package

Often we need to inspect <code>.rpm</code> package.
No need any <code>dnf</code> command,
as this more like an <code>rpm</code> issue.

{% highlight bash %}
$ dnf install rpmlint rpmdevtools 
...
Installing:
 rpmdevtools            noarch 8.10-3.fc27            rawhide 105 k
 rpmlint                noarch 1.10-3.fc28            rawhide 189 k
...
{% endhighlight %}

#### rpmlint

Check for possible issue, similar to lintian or namcap.

{% highlight bash %}
$ rpmlint ~/rpmbuild/RPMS/x86_64/herbstluftwm-0.6.2-8.fc28.x86_64.rpm 
herbstluftwm.x86_64: W: spelling-error %description -l en_US subframes -> sub frames, sub-frames, frames
herbstluftwm.x86_64: W: spelling-error %description -l en_US workspaces -> work spaces, work-spaces, works paces
herbstluftwm.x86_64: W: spelling-error %description -l en_US runtime -> run time, run-time, rudiment
herbstluftwm.x86_64: W: spelling-error %description -l en_US ipc -> pic, inc
herbstluftwm.x86_64: W: spelling-error %description -l en_US startup -> start up, start-up, upstart
herbstluftwm.x86_64: W: only-non-binary-in-usr-lib
1 packages and 0 specfiles checked; 0 errors, 6 warnings.
{% endhighlight %}

![Docker Fedora: rpmlint][image-ss-rpmlint]{: .img-responsive }

#### rpm -Qpl

You can also query file <code>-Qpl</code> directly to the <code>.rpm</code> package.

{% highlight bash %}
$ rpm -qpl ~/rpmbuild/RPMS/x86_64/herbstluftwm-0.6.2-8.fc28.x86_64.rpm
/etc/xdg/herbstluftwm
/etc/xdg/herbstluftwm/autostart
/etc/xdg/herbstluftwm/panel.sh
/etc/xdg/herbstluftwm/restartpanels.sh
/usr/bin/herbstclient
/usr/bin/herbstluftwm
/usr/lib/.build-id
/usr/lib/.build-id/3d
/usr/lib/.build-id/3d/0a8089936f6a6836c973eafe14d619b086c60a
/usr/lib/.build-id/da
...
{% endhighlight %}

![Docker Fedora: rpm -qpl][image-ss-rpm-qpl]{: .img-responsive }

#### rpmls

Or use the <code>rpmls</code> from the <code>rpmdevtools</code> package

{% highlight bash %}
$ rpmls ~/rpmbuild/RPMS/x86_64/herbstluftwm-0.6.2-8.fc28.x86_64.rpm
drwxr-xr-x  /etc/xdg/herbstluftwm
-rwxr-xr-x  /etc/xdg/herbstluftwm/autostart
-rwxr-xr-x  /etc/xdg/herbstluftwm/panel.sh
-rwxr-xr-x  /etc/xdg/herbstluftwm/restartpanels.sh
-rwxr-xr-x  /usr/bin/herbstclient
-rwxr-xr-x  /usr/bin/herbstluftwm
drwxr-xr-x  /usr/lib/.build-id
drwxr-xr-x  /usr/lib/.build-id/3d
lrwxrwxrwx  /usr/lib/.build-id/3d/0a8089936f6a6836c973eafe14d619b086c60a
...
{% endhighlight %}

![Docker Fedora: rpmls][image-ss-rpmls]{: .img-responsive }

#### rpm2cpio

This one also extract.

{% highlight bash %}
$ rpm2cpio ~/rpmbuild/RPMS/x86_64/herbstluftwm-0.6.2-8.fc28.x86_64.rpm | cpio -vid
./etc/xdg/herbstluftwm
./etc/xdg/herbstluftwm/autostart
./etc/xdg/herbstluftwm/panel.sh
./etc/xdg/herbstluftwm/restartpanels.sh
./usr/bin/herbstclient
./usr/bin/herbstluftwm
./usr/lib/.build-id
./usr/lib/.build-id/3d
./usr/lib/.build-id/3d/0a8089936f6a6836c973eafe14d619b086c60a
./usr/lib/.build-id/da
...
{% endhighlight %}

![Docker Fedora: rpm2cpio, cpio -vid][image-ss-rpm2cpio-vid]{: .img-responsive }

-- -- --

### Conclusion

	These are just preliminary knowledge about DNF.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-fedora' %}

[image-ss-bs-download-source]:   {{ asset_post }}/25-download-source.png
[image-ss-bs-install-rpmbuild]:  {{ asset_post }}/25-install-rpmbuild.png
[image-ss-bs-ls-rpmbuild]:       {{ asset_post }}/25-ls-rpmbuild.png
[image-ss-bs-result-rpm]:        {{ asset_post }}/25-result-rpm.png
[image-ss-bs-rpmbuild-ba]:       {{ asset_post }}/25-rpmbuild-ba.png
[image-ss-bs-rpm-ivh]:           {{ asset_post }}/25-rpm-ivh.png
[image-ss-bs-rpm-iv-hlwm]:       {{ asset_post }}/25-rpm-iv-hlwm.png
[image-ss-bs-dnf-builddep]:       {{ asset_post }}/25-dnf-builddep.png

[image-ss-rpm2cpio-vid]:	{{ asset_post }}/26-rpm2cpio-cpio-vid.png
[image-ss-rpmlint]:	{{ asset_post }}/26-rpmlint.png
[image-ss-rpmls]:	{{ asset_post }}/26-rpmls.png
[image-ss-rpm-qpl]:	{{ asset_post }}/26-rpm-qpl.png

