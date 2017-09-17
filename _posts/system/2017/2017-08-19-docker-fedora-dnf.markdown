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
  One of Three Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
# - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-fedora-dnf.html %}

-- -- --

### Dependency

There are two main topics in dependency,
_dependency_ itself, and _reverse dependency_.
Beside these two, there are other topic as well,
such as _managing conflict_ that we do not cover here.

#### Help

DNF has a <code>repoquery</code> help that show all dependency related options.

{% highlight bash %}
$ dnf help repoquery

search for packages matching keyword
...
  --whatconflicts REQ   show only results that conflict REQ
  --whatobsoletes REQ   show only results that obsolete REQ
  --whatprovides REQ    show only results that provide REQ
  --whatrequires REQ    shows results that requires package provides and files REQ
  --whatrecommends REQ  show only results that recommend REQ
  --whatenhances REQ    show only results that enhance REQ
  --whatsuggests REQ    show only results that suggest REQ
  --whatsupplements REQ
...
  --conflicts           Display capabilities that the package conflicts with.
  --enhances            Display capabilities that the package can enhance.
  --provides            Display capabilities provided by the package.
  --recommends          Display capabilities that the package recommends.
  --requires            Display capabilities that the package depends on.
  --requires-pre        Display capabilities that the package depends on for
                        running a %pre script.
  --suggests            Display capabilities that the package suggests.
{% endhighlight %}

![Docker DNF: Help Repoquery][image-ss-dnf-help-repoquery]{: .img-responsive }

#### Dependency

	Package that required by: such as man-db need less and other.

This _dependency_ information can be achieved by <code>repoquery --requires</code> command.
This will show required parts of the package.

{% highlight bash %}
$ dnf repoquery --requires man-db
Last metadata expiration check: 0:27:23 ago on Sat Aug 26 13:49:15 2017.
/bin/sh
coreutils
grep
groff-base
gzip
less
libc.so.6(GLIBC_2.17)(64bit)
libgdbm.so.4()(64bit)
libpipeline.so.1()(64bit)
libz.so.1()(64bit)
rtld(GNU_HASH)
{% endhighlight %}

![Docker DNF: Repoquery Requires][image-ss-dnf-requires]{: .img-responsive }

#### Reverse Dependency

	Package that require: such as less needed by man-db or other.

This _reverse dependency_ require <code>repoquery --whatrequires</code> command.

{% highlight bash %}
$ dnf repoquery --whatrequires less
Last metadata expiration check: 0:29:36 ago on Sat Aug 26 13:49:15 2017.
GMT-0:5.4.2-3.fc27.i686
GMT-0:5.4.2-3.fc27.x86_64
R-core-0:3.4.1-4.fc27.i686
R-core-0:3.4.1-4.fc27.x86_64
Singular-0:4.1.0p3-6.fc27.x86_64
backup-manager-0:0.7.10-22.fc27.noarch
c-graph-0:2.0-12.fc27.x86_64
colordiff-0:1.0.18-2.fc27.noarch
git-core-0:2.14.1-2.fc27.x86_64
libguestfs-1:1.37.21-2.fc28.i686
libguestfs-1:1.37.21-2.fc28.x86_64
libguestfs-tools-c-1:1.37.21-2.fc28.x86_64
man-db-0:2.7.6.1-5.fc27.x86_64
octave-6:4.2.1-4.fc27.2.i686
octave-6:4.2.1-4.fc27.2.x86_64
rpmreaper-0:0.2.0-13.fc27.x86_64
{% endhighlight %}

![Docker DNF: Repoquery What Requires][image-ss-dnf-whatrequires]{: .img-responsive }

#### Test

Removing <code>less</code> would remove <code>man-db</code>.
And also remove any unused dependency.

{% highlight bash %}
$ dnf remove less
Dependencies resolved.
====================================================================
 Package         Arch       Version              Repository    Size
====================================================================
Removing:
 less            x86_64     487-5.fc27           @rawhide     309 k
Removing depended packages:
 man-db          x86_64     2.7.6.1-5.fc27       @rawhide     1.9 M
Removing unused dependencies:
 groff-base      x86_64     1.22.3-11.fc27       @rawhide     3.7 M
 libpipeline     x86_64     1.4.2-3.fc27         @rawhide     106 k

Transaction Summary
====================================================================
Remove  3 Packages

Freed space: 6.0 M
Is this ok [y/N]: 
{% endhighlight %}

![Docker DNF: Test Dependency][image-ss-dnf-test-remove]{: .img-responsive }

#### Tree

	Most people love tree

This <code>rpmreaper</code> is an RPM tool rather than DNF tool.

{% highlight bash %}
$ dnf install rpmreaper
{% endhighlight %}

![Docker Fedora: rpmreaper][image-ss-fedora-rpmreaper]{: .img-responsive }

-- -- --

### Group

Is this docker Minimal Install ?
I always wonder what inside theis Fedora docker Container.
RPM <code>group</code> help me understand this riddle.

#### Group List

We can see <code>Minimal Install</code> group as below.

{% highlight bash %}
$ dnf grouplist
Last metadata expiration check: 2:12:21 ago on Wed Aug 23 11:50:45 2017.
Available Environment Groups:
   Fedora Custom Operating System
   Minimal Install
   Fedora Server Edition
   Fedora Workstation
   Fedora Cloud Server
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-list]{: .img-responsive }

#### Group Info

And there is this <code>Core</code> group as below.

{% highlight bash %}
$ dnf group info "Minimal Install"
Last metadata expiration check: 2:13:24 ago on Wed Aug 23 11:50:45 2017.
Environment Group: Minimal Install
 Description: Basic functionality.
 Mandatory Groups:
   Core
 Optional Groups:
   Guest Agents
   Standard
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-info1]{: .img-responsive }

And at the most bottom, there are only <code>Packages</code> as below.

{% highlight bash %}
$ dnf group info core
Last metadata expiration check: 2:14:07 ago on Wed Aug 23 11:50:45 2017.

Group: Core
 Description: Smallest possible installation
 Mandatory Packages:
   audit
   basesystem
   bash
   coreutils
   cronie
   curl
   dhcp-client
   dnf
   dnf-yum
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-info2]{: .img-responsive }

#### Group Install

Now we know, that not all the <code>Core</code> packages are installed,
as some packages not required by the container.

{% highlight bash %}
$ dnf group install core
Last metadata expiration check: 2:07:09 ago on Wed Aug 23 11:50:45 2017.
No match for group package "ppc64-utils"
Dependencies resolved.
=============================================================================
 Group     Packages                                                        
=============================================================================
Marking packages as installed by the group:
 @Core     curl                     dnf-plugins-core            dnf        
           sssd-common              policycoreutils             glibc      
           NetworkManager           openssh-clients             systemd    
           dracut-config-rescue     openssh-server              rootfiles  
           plymouth                 selinux-policy-targeted     dhcp-client
{% endhighlight %}

![Docker DNF: Group][image-ss-dnf-g-install]{: .img-responsive }

#### Beyond Group

You can even remove group of packages as you can read in manual,
or upgrade only for specific group.

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

#### Mirror

There is no need to explicitly specify mirror,
in Fedora since DNF utilize metalink.
As you can see in previous figure <code>dnf repoinfo</code>,
the rawhide repo is already use <code>ftp.jaist.ac.jp</code>.

{% highlight bash %}
$ dnf repoinfo rawhide
...
Repo-baseurl : http://ftp.riken.jp/Linux/fedora/development/rawhide/Everything/x86_64/os/
{% endhighlight %}

If you wish you can inspect what is in there.
Consider grab the metalink from <code>yum.repos.d</code>.

{% highlight bash %}
$ cat /etc/yum.repos.d/fedora-rawhide.repo 
...

[rawhide]
name=Fedora - Rawhide - Developmental packages for the next Fedora release
failovermethod=priority
#baseurl=http://download.fedoraproject.org/pub/fedora/linux/development/rawhide//Everything/$basearch/os/
metalink=https://mirrors.fedoraproject.org/metalink?repo=rawhide&arch=$basearch

...
{% endhighlight %}

Now see what is it in the metalink.

{% highlight bash %}
$ curl -L "https://mirrors.fedoraproject.org/mirrorlist?repo=rawhide&arch=x86_64"
# repo = rawhide arch = x86_64 country = JP country = PH country = CN country = KR country = TH 
https://ftp.yz.yamagata-u.ac.jp/pub/linux/fedora-projects/fedora/linux/development/rawhide/Everything/x86_64/os/
http://ftp.jaist.ac.jp/pub/Linux/Fedora/development/rawhide/Everything/x86_64/os/
http://ftp.riken.jp/Linux/fedora/development/rawhide/Everything/x86_64/os/
http://ftp.kaist.ac.kr/fedora/development/rawhide/Everything/x86_64/os/
https://mirrors.ustc.edu.cn/fedora/development/rawhide/Everything/x86_64/os/
https://mirror.pregi.net/fedora/development/rawhide/Everything/x86_64/os/
http://mirror2.totbb.net/fedora/linux/development/rawhide/Everything/x86_64/os/
{% endhighlight %}

![Docker DNF: Mirror Metalink][image-ss-dnf-mirror-metalink]{: .img-responsive }

Now we can explicity use <code>ftp.riken.jp</code>.

{% highlight bash %}
$ nano /etc/yum.repos.d/fedora-rawhide.repo
baseurl=http://ftp.riken.jp/Linux/fedora/development/rawhide/Everything/x86_64/os/
#baseurl=http://download.fedoraproject.org/pub/fedora/linux/development/rawhide//Everything/$basearch/o$
metalink=https://mirrors.fedoraproject.org/metalink?repo=rawhide&arch=$basearch
{% endhighlight %}

![Docker YUM: Mirror Base URL][image-ss-dnf-mirror-baseurl]{: .img-responsive }

Check what happened to the rawhide repository

{% highlight bash %}
$ dnf repoinfo rawhide
Fedora - Rawhide - Developmental packages for the nex 976 kB/s |  66 MB     01:09    
Last metadata expiration check: 0:00:23 ago on Sat Sep  9 11:49:43 2017.
...
Repo-baseurl : http://ftp.riken.jp/Linux/fedora/development/rawhide/Everything/x86_64/os/
{% endhighlight %}

This is all about repository for now.

-- -- --

### What's Next

These are just preliminary knowledge about DNF.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-fedora' %}

[local-part-three]: {{ site.url }}/system/2017/08/20/docker-fedora-dnf.html

[image-ss-dnf-help-repoquery]: {{ asset_post }}/14-help-repoquery.png
[image-ss-dnf-requires]:       {{ asset_post }}/14-repoquery-requires.png
[image-ss-dnf-whatrequires]:   {{ asset_post }}/14-repoquery-whatrequires.png
[image-ss-dnf-test-remove]:    {{ asset_post }}/14-remove-less.png
[image-ss-fedora-rpmreaper]:   {{ asset_post }}/14-rpmreaper.png

[image-ss-dnf-g-info1]:   {{ asset_post }}/15-dnf-group-info-core.png
[image-ss-dnf-g-info2]:   {{ asset_post }}/15-dnf-group-info-min.png
[image-ss-dnf-g-install]: {{ asset_post }}/15-dnf-group-install-core.png
[image-ss-dnf-g-list]:    {{ asset_post }}/15-dnf-grouplist.png

[image-ss-dnf-r-info]:          {{ asset_post }}/16-repoinfo.png
[image-ss-dnf-r-list]:          {{ asset_post }}/16-repolist.png
[image-ss-dnf-r-list-all]:      {{ asset_post }}/16-repolist-all.png
[image-ss-dnf-r-pkgs-info]:     {{ asset_post }}/16-repo-pkgs-info.png
[image-ss-dnf-r-pkgs-list]:     {{ asset_post }}/16-repo-pkgs-list.png
[image-ss-yum-repos-d]:         {{ asset_post }}/16-yum-repos-d.png
[image-ss-yum-repos-d-rawhide]: {{ asset_post }}/16-yum-repos-d-rawhide.png
[image-ss-dnf-r-pkgs-source]:   {{ asset_post }}/16-repo-pkgs-list-source.png
[image-ss-dnf-enablerepo]:      {{ asset_post }}/16-enablerepo.png

[image-ss-dnf-mirror-metalink]: {{ asset_post }}/16-mirror-curl-metalink.png
[image-ss-dnf-mirror-baseurl]:  {{ asset_post }}/16-nano-repos-d-baseurl.png
