---
layout: post
title: "Docker - Fedora DNF - Part Two"
date: 2017-08-19 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, fedora]
author: epsi

excerpt:
  Examine DNF step by step,
  using Fedora container in Docker.
  One of Two Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Topics

This is a two-parts article.
There are few topics here.

[ [Part One][local-part-one] ]

*	Preface: Test Bed

*	Getting Started With Docker

*	Package Management: RPM Frontend, Get Help, DNF Shell

*	Updating System: OS Release, Repository List, System Upgrade, Extra Commands

*	Package IRSI: Install, Removal, Query Search, Show Info

*	Dependency: Help, Dependency, Reverse Dependency, Test, Tree

*	Group: Group List, Group Info, Group Install, Beyond Group

*	What's Next

[ [Part Two][local-part-two] ]

*	Repositories: repolist, repoinfo, repo-pkgs, --enablerepo

*	Plugin: List, Install, Help, Config Manager Example

*	History: The Log File, DNF History

*	Clean Up

*	Build from Source

*	Conclusion

-- -- --

### Repository

A few DNF repository commands.

#### Repository List

{% highlight bash %}
$ dnf repolist    
Last metadata expiration check: 0:07:48 ago on Mon Aug 28 18:12:32 2017.
repo id  repo name                                                     status
*rawhide Fedora - Rawhide - Developmental packages for the next Fedora 61447
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-r-list]{: .img-responsive }

All repository.

{% highlight bash %}
$ dnf repolist all
Last metadata expiration check: 0:06:27 ago on Mon Aug 28 18:12:32 2017.
repo id                         repo name                      status
fedora                          Fedora 28 - x86_64             disabled
fedora-cisco-openh264           Fedora 28 openh264 (From Cisco disabled
fedora-cisco-openh264-debuginfo Fedora 28 openh264 (From Cisco disabled
fedora-debuginfo                Fedora 28 - x86_64 - Debug     disabled
fedora-source                   Fedora 28 - Source             disabled
*rawhide                        Fedora - Rawhide - Development enabled: 61447
rawhide-debuginfo               Fedora - Rawhide - Debug       disabled
rawhide-source                  Fedora - Rawhide - Source      disabled
updates                         Fedora 28 - x86_64 - Updates   disabled
updates-debuginfo               Fedora 28 - x86_64 - Updates - disabled
updates-source                  Fedora 28 - Updates Source     disabled
updates-testing                 Fedora 28 - x86_64 - Test Upda disabled
updates-testing-debuginfo       Fedora 28 - x86_64 - Test Upda disabled
updates-testing-source          Fedora 28 - Test Updates Sourc disabled
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-r-list-all]{: .img-responsive }

Consider examine the <code>/etc/yum.repos.d/</code>.

{% highlight bash %}
$ ls -l /etc/yum.repos.d/
total 20
-rw-r--r-- 1 root root  707 Aug 16 19:38 fedora-cisco-openh264.repo
-rw-r--r-- 1 root root 2103 Aug 16 19:38 fedora-rawhide.repo
-rw-r--r-- 1 root root 1403 Aug 16 19:38 fedora-updates-testing.repo
-rw-r--r-- 1 root root 1345 Aug 16 19:38 fedora-updates.repo
-rw-r--r-- 1 root root 1332 Aug 16 19:38 fedora.repo
{% endhighlight %}

![Docker DNF: /etc/yum.repos.d/][image-ss-yum-repos-d]

#### Repository Info

{% highlight bash %}
$ dnf repoinfo
Last metadata expiration check: 0:08:34 ago on Mon Aug 28 18:12:32 2017.

Repo-id      : rawhide
Repo-name    : Fedora - Rawhide - Developmental packages for the next Fedora
             : release
Repo-revision: 1503851723
Repo-updated : Sun Aug 27 16:35:23 2017
Repo-pkgs    : 61447
Repo-size    : 59 G
Repo-metalink: https://mirrors.fedoraproject.org/metalink?repo=rawhide&arch=x86_64
  Updated    : Mon Aug 28 18:12:32 2017
Repo-baseurl : http://ftp.jaist.ac.jp/pub/Linux/Fedora/development/rawhide/Everything/x86_64/os/
             : (13 more)
Repo-expire  : 21600 second(s) (last: Mon Aug 28 18:12:32 2017)
Repo-filename: /etc/yum.repos.d/fedora-rawhide.repo
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-r-info]{: .img-responsive }

or just

{% highlight bash %}
$ dnf repoinfo rawhide
{% endhighlight %}

Consider inspect the <code>/fedora-rawhide.repo</code>

{% highlight bash %}
$ cat /etc/yum.repos.d/fedora-rawhide.repo
# These packages are untested and still under development. This
# repository is used for development of new releases.
...

[rawhide]
name=Fedora - Rawhide - Developmental packages for the next Fedora release
...

[rawhide-debuginfo]
name=Fedora - Rawhide - Debug
...

[rawhide-source]
name=Fedora - Rawhide - Source
failovermethod=priority
#baseurl=http://download.fedoraproject.org/pub/fedora/linux/development/rawhide/Everything/source/tree/
metalink=https://mirrors.fedoraproject.org/metalink?repo=rawhide-source&arch=$basearch
enabled=0
repo_gpgcheck=0
type=rpm
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-$releasever-$basearch
skip_if_unavailable=False
{% endhighlight %}

![Docker DNF: /etc/yum.repos.d/fedora-rawhide.repo][image-ss-yum-repos-d-rawhide]

Now you know that you can edit directly.

#### Repository Packages

You can use <code>repo-pkgs</code>,
it is just an alias to <code>repository-packages</code>.

{% highlight bash %}
$ dnf repository-packages rawhide list installed
Installed Packages
bc.x86_64                       1.07.1-3.fc27               @rawhide
fish.x86_64                     2.3.1-6.fc27                @rawhide
groff-base.x86_64               1.22.3-11.fc27              @rawhide
htop.x86_64                     2.0.2-4.fc27                @rawhide
less.x86_64                     487-5.fc27                  @rawhide
libpipeline.x86_64              1.4.2-3.fc27                @rawhide
libstdc++.x86_64                7.1.1-7.fc27.1              @rawhide
man-db.x86_64                   2.7.6.1-5.fc27              @rawhide
nano.x86_64                     2.8.6-3.fc27                @rawhide
ncdu.x86_64                     1.12-5.fc27                 @rawhide
rpmreaper.x86_64                0.2.0-13.fc27               @rawhide
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-r-pkgs-list]{: .img-responsive }

Or even disabled repository.

{% highlight bash %}
$ dnf repository-packages rawhide-source list | grep herbstluftwm
herbstluftwm.src                      0.6.2-8.fc27                rawhide-source
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-r-pkgs-source]{: .img-responsive }

And info for each repository

{% highlight bash %}
$ dnf repository-packages rawhide info | less
Last metadata expiration check: 0:14:24 ago on Mon Aug 28 18:12:32 2017.
Installed Packages
Name         : bc
Version      : 1.07.1
Release      : 3.fc27
Arch         : x86_64
Size         : 225 k
Source       : bc-1.07.1-3.fc27.src.rpm
Repo         : @System
From repo    : rawhide
Summary      : GNU's bc (a numeric processing language) and dc (a calculator)
URL          : http://www.gnu.org/software/bc/
License      : GPLv2+
...
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-r-pkgs-info]{: .img-responsive }

#### enablerepo Directive

You can use <code>--enablerepo</code> 
to operate on a single command without permanently enable the repository.
The same rule applies to <code>--disablerepo</code> temporarily.

{% highlight bash %}
$ dnf info herbstluftwm.src
Last metadata expiration check: 0:09:50 ago on Sat Sep  2 17:47:44 2017.
Error: No matching Packages to list

$ dnf info herbstluftwm.src --enablerepo=rawhide-source
Last metadata expiration check: 0:06:44 ago on Sat Sep  2 17:50:56 2017.
Available Packages
Name         : herbstluftwm
Version      : 0.6.2
Release      : 8.fc27
Arch         : src
Size         : 228 k
Source       : None
Repo         : rawhide-source
{% endhighlight %}

![Docker DNF: Repository][image-ss-dnf-enablerepo]{: .img-responsive }

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
rpm -iv ~/rpmbuild/RPMS/x86_64/herbstluftwm-0.6.2-8.fc28.x86_64.rpm 
Preparing packages...
herbstluftwm-0.6.2-8.fc28.x86_64
{% endhighlight %}

![Docker RPM: Build Source][image-ss-bs-rpm-iv-hlwm]{: .img-responsive }

#### Thank You

I respect the help from community.
Again, thank you.

-- -- --

### Conclusion

Those above are my docker journey knowledge about DNF.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-fedora' %}

[local-part-one]: {{ site.url }}/system/2017/08/18/docker-fedora-dnf.html
[local-part-two]: {{ site.url }}/system/2017/08/19/docker-fedora-dnf.html

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-dnf-r-info]:          {{ asset_post }}/16-repoinfo.png
[image-ss-dnf-r-list]:          {{ asset_post }}/16-repolist.png
[image-ss-dnf-r-list-all]:      {{ asset_post }}/16-repolist-all.png
[image-ss-dnf-r-pkgs-info]:     {{ asset_post }}/16-repo-pkgs-info.png
[image-ss-dnf-r-pkgs-list]:     {{ asset_post }}/16-repo-pkgs-list.png
[image-ss-yum-repos-d]:         {{ asset_post }}/16-yum-repos-d.png
[image-ss-yum-repos-d-rawhide]: {{ asset_post }}/16-yum-repos-d-rawhide.png
[image-ss-dnf-r-pkgs-source]:   {{ asset_post }}/16-repo-pkgs-list-source.png
[image-ss-dnf-enablerepo]:      {{ asset_post }}/16-enablerepo.png

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
