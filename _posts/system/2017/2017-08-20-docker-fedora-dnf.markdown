---
layout: post
title: "Docker - Fedora DNF - Part Three"
date: 2017-08-20 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, fedora]
author: epsi

excerpt:
  Examine DNF step by step,
  using Fedora container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082035  # Slackware Package
# - 17081845  # Fedora DNF
  - 17081535  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081435  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/toc-docker-fedora-dnf.html %}

-- -- --

### Plugin

One of the DNF feature that differ DNF from other package is Plugin.

#### List

Unfortunately I cannot any command to list DNF plugins.
DNF search also won't work either.
Luckily there are good documentation.

*	<http://dnf-plugins-core.readthedocs.io/en/latest/>

Or google if the plugin is not mentioned in above link.

*	<https://fedoraproject.org/wiki/DNF_system_upgrade>

#### Install 

Consider this example: <code>config-manager</code>

{% highlight bash %}
$ dnf install dnf-plugin-config-manager
Last metadata expiration check: 0:33:51 ago on Sat Sep  2 18:26:16 2017.
Dependencies resolved.
=============================================================================
 Package                      Arch       Version           Repository   Size
=============================================================================
Installing:
 dnf-plugins-core             noarch     2.1.3-2.fc27      rawhide      50 k
Installing dependencies:
 python3-dnf-plugins-core     noarch     2.1.3-2.fc27      rawhide     138 k

Transaction Summary
=============================================================================
Install  2 Packages

Total download size: 188 k
Installed size: 451 k
Is this ok [y/N]: 
{% endhighlight %}

![Docker DNF: Plugin: config-manager][image-ss-dnf-cm-install]{: .img-responsive }

#### Help

{% highlight bash %}
$ dnf help config-manager
{% endhighlight %}

![Docker DNF: Plugin: config-manager][image-ss-dnf-cm-help]{: .img-responsive }

#### Config Manager Example

Now we can enable a repository without the need to edit configuration manually.

{% highlight bash %}
$ dnf config-manager --set-enabled rawhide-source
{% endhighlight %}

![Docker DNF: Plugin: config-manager][image-ss-dnf-cm-set-enabled]{: .img-responsive }

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.
For that reason, I put the recorded event here, 
before discussing about any further feature.

{% highlight bash %}
$ less /var/log/dnf.log
2017-08-23T10:47:59Z INFO --- logging initialized ---
2017-08-23T10:47:59Z DDEBUG timer: config: 1381 ms
2017-08-23T10:47:59Z DEBUG DNF version: 2.5.1
2017-08-23T10:47:59Z DDEBUG Command: dnf repolist 
2017-08-23T10:47:59Z DDEBUG Installroot: /
2017-08-23T10:47:59Z DDEBUG Releasever: 27
2017-08-23T10:47:59Z DEBUG cachedir: /var/cache/dnf
2017-08-23T10:47:59Z DDEBUG Base command: repolist
2017-08-23T10:47:59Z DDEBUG Extra commands: ['repolist']
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/dnf.log][image-ss-var-log-dnf]{: .img-responsive }

#### DNF History

DNF has a very nice history feature.

{% highlight bash %}
$ dnf history
ID     | Command line             | Date and time    | Action(s)      | Altered
-------------------------------------------------------------------------------
     8 | upgrade                  | 2017-08-24 14:48 | Update         |   15 **
     7 | reinstall man-db         | 2017-08-23 15:47 | Reinstall      |    1   
     6 | install man              | 2017-08-23 15:45 | Install        |    4   
     5 | remove less              | 2017-08-23 13:14 | Erase          |    4   
     4 | install man-db nano htop | 2017-08-23 12:56 | Install        |    5   
     3 | install man              | 2017-08-23 12:55 | Install        |    5   
     2 | upgrade --nogpgcheck     | 2017-08-23 11:31 | I, U           |  171 **
     1 |                          | 2017-07-11 11:29 | Install        |  170 EE
{% endhighlight %}

![Docker DNF: History][image-ss-dnf-history]{: .img-responsive }

-- -- --

### Hold Package

DNF can hold package using versionlock plugin.
Or using <code>exclude</code> directive in configuration.

#### Example

Suppose you want to do system upgrade,
but you do not want to upgrade certain package.
There is a good reason for these,
such as keeping old driver,
because the latest has a issue or such reason.
Or maybe we want to keep our current beloved newly compiled package
that equipped with super duper specific configuration parameter optimization.

{% highlight bash %}
$  dnf upgrade
Last metadata expiration check: 0:22:55 ago on Sat Sep  9 11:49:43 2017.
Dependencies resolved.
====================================================================
 Package                  Arch   Version              Repository
                                                               Size
====================================================================
Upgrading:
 bash                     x86_64 4.4.12-11.fc28       rawhide 1.5 M
 coreutils                x86_64 8.28-1.fc28          rawhide 1.2 M
 coreutils-common         x86_64 8.28-1.fc28          rawhide 1.9 M
 cpp                      x86_64 7.2.1-1.fc28         rawhide 9.2 M
{% endhighlight %}

![Docker DNF: BASH Unlocked][image-ss-h-dnf-bash-unlocked]{: .img-responsive }

Consider BASH as our guinea pig locking example.

#### Add Lock

We can add lock easily using DNF.
By using <code>exclude</code> directive in configuration.

{% highlight bash %}
$ nano /etc/dnf/dnf.conf 
[main]
gpgcheck=1
installonly_limit=3
clean_requirements_on_remove=True
exclude=bash
{% endhighlight %}

![Docker DNF: Nano DNF Configuration][image-ss-h-nano-dnf-conf]{: .img-responsive }

Now bash will be ignored.

{% highlight bash %}
$ dnf upgrade
Last metadata expiration check: 0:40:43 ago on Sat Sep  9 11:49:43 2017.
Dependencies resolved.
====================================================================
 Package                  Arch   Version              Repository
                                                               Size
====================================================================
Upgrading:
 coreutils                x86_64 8.28-1.fc28          rawhide 1.2 M
 coreutils-common         x86_64 8.28-1.fc28          rawhide 1.9 M
 cpp                      x86_64 7.2.1-1.fc28         rawhide 9.2 M
 crypto-policies          noarch 20170823-1.git8d18c27.fc28
{% endhighlight %}

![Docker DNF: BASH Locked][image-ss-h-dnf-bash-locked]{: .img-responsive }
  
#### Remove Lock

You can unlocked package by commenting the exclude in dnf.conf

{% highlight bash %}
$ cat /etc/dnf/dnf.conf 
# exclude=bash
{% endhighlight %}

Example done successfully.
Guinea pig is alive.

-- -- --

### Clean Up

Time after time, your cache size may growing bigger and bigger.

Package Cache
	
*	/var/cache/dnf/ * /packages/ * .rpm

{% highlight bash %}
$ ls -lR /var/cache/dnf/
{% endhighlight %}

![Docker DNF: Cache][image-ss-dnf-cache]{: .img-responsive }

You can clean these directory.

{% highlight bash %}
$ dnf clean packages
18 files removed
{% endhighlight %}

{% highlight bash %}
$ dnf clean all
9 files removed
{% endhighlight %}

![Docker DNF: Clean][image-ss-dnf-clean]{: .img-responsive }

-- -- --

### Build from Source

DNF has the capability to download the source code.
Then we can utilize other tool to build from source.

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

#### Example Requirement

Since the case is Herbstluftwm, 
we also need require to install these packages
to avoid missing dependencies.

{% highlight bash %}
$ dnf install glib2-devel libX11-devel libXinerama-devel
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

### Conclusion

	These are just preliminary knowledge about DNF.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-fedora' %}

[image-ss-dnf-cm-set-enabled]:  {{ asset_post }}/18-cm-set-enabled.png
[image-ss-dnf-cm-help]:         {{ asset_post }}/18-help-cm.png
[image-ss-dnf-cm-install]:      {{ asset_post }}/18-plugin-install.png

[image-ss-dnf-cache]:     {{ asset_post }}/17-cache.png
[image-ss-dnf-clean]:     {{ asset_post }}/17-clean.png

[image-ss-var-log-dnf]:   {{ asset_post }}/19-log.png
[image-ss-dnf-history]:   {{ asset_post }}/19-history.png

[image-ss-bs-download-source]:   {{ asset_post }}/25-download-source.png
[image-ss-bs-install-rpmbuild]:  {{ asset_post }}/25-install-rpmbuild.png
[image-ss-bs-ls-rpmbuild]:       {{ asset_post }}/25-ls-rpmbuild.png
[image-ss-bs-result-rpm]:        {{ asset_post }}/25-result-rpm.png
[image-ss-bs-rpmbuild-ba]:       {{ asset_post }}/25-rpmbuild-ba.png
[image-ss-bs-rpm-ivh]:           {{ asset_post }}/25-rpm-ivh.png
[image-ss-bs-rpm-iv-hlwm]:       {{ asset_post }}/25-rpm-iv-hlwm.png

[image-ss-h-dnf-bash-locked]:    {{ asset_post }}/27-hold-dnf-upgrade-bash-locked.png
[image-ss-h-dnf-bash-unlocked]:  {{ asset_post }}/27-hold-dnf-upgrade-bash-unlocked.png
[image-ss-h-nano-dnf-conf]:      {{ asset_post }}/27-hold-nano-dnf-conf.png
