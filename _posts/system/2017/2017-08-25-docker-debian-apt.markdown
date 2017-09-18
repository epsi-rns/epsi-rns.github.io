---
layout: post
title: "Docker - Debian APT - Part Two"
date: 2017-08-25 09:35:15 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
# - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-debian-apt.html %}

-- -- --

### Dependency

There are two main topics in package dependency,
the _dependency_ itself, and _reverse dependency_.
Beside these two, there are other topic as well,
such as _managing conflict_ that we do not cover here.

#### Dependency

	Package that required by: such as man-db need groff-base and other.

This will show required parts of the package.

{% highlight bash %}
$ apt-cache depends man-db  
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt depends man-db   
man-db
  PreDepends: dpkg
  Depends: groff-base
  Depends: bsdmainutils
 |...
  Suggests: groff
  Suggests: less
  Suggests: <www-browser>
    ...
    lynx
    ...
  Replaces: <man>
    man-db
  ...
  Replaces: <nlsutils>
{% endhighlight %}

![Docker APT: Dependency][image-ss-apt-depends]{: .img-responsive }

This <code>apt show</code> provide dependency too.

{% highlight bash %}
$ apt show man-db
root@d0c5bd2fc5b5:/# apt show man-db
...
Depends: groff-base (>= 1.18.1.1-15), bsdmainutils, debconf (>= 1.2.0) | debconf-2.0, libc6 (>= 2.17), libgdbm3 (>= 1.8.3), libpipeline1 (>= 1.4.0), zlib1g (>= 1:1.1.4)
...
{% endhighlight %}

<code>aptitude search</code> also works too.

{% highlight bash %}
$ aptitude search ~Rman-db    
i A bsdmainutils               - collection of more utilities from F
p   cdebconf                   - Debian Configuration Management Sys
i A debconf                    - Debian configuration management sys
v   debconf-2.0                -                                    
i A dpkg                       - Debian package management system   
i A groff-base                 - GNU troff text-formatting system (b
i A libc6                      - GNU C Library: Shared libraries    
i A libgdbm3                   - GNU dbm database routines (runtime 
i A libpipeline1               - pipeline manipulation library      
i A zlib1g                     - compression library - runtime      
{% endhighlight %}

![Docker Aptitude: Search ~R][image-ss-aptitude-s-r]{: .img-responsive }

#### Reverse Dependency

	Package that require: such as groff-base needed by man-db or other.

{% highlight bash %}
$ apt-cache rdepends groff-base 
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt rdepends groff-base
groff-base
Reverse Depends:
  Depends: man-db (>= 1.18.1.1-15)
  Suggests: imagemagick-6.q16hdri
  Suggests: imagemagick-6.q16
  Recommends: rpmlint
  Suggests: perl-doc
  Replaces: groff (<< 1.17.2-9)
  Suggests: imagemagick-6.q16hdri
  Suggests: imagemagick-6.q16
  Depends: groff (= 1.22.3-9)
  Suggests: bioperl
  Recommends: gdisk
  Recommends: debian-el
  Recommends: doclifter
  Depends: cppman
{% endhighlight %}

![Docker APT: Reverse Dependency][image-ss-apt-rdepends]{: .img-responsive }

<code>aptitude search</code> also works too.

{% highlight bash %}
$ aptitude search ~Dgroff-base
p   cppman                     - C++ 98/11 manual pages for Linux, w
p   groff                      - GNU troff text-formatting system   
i   man-db                     - on-line manual pager               
{% endhighlight %}

![Docker Aptitude: Search ~D][image-ss-aptitude-s-d]{: .img-responsive }

This <code>aptitude why</code> also provide reverse dependency.

{% highlight bash %}
$ aptitude why groff-base
i   man-db Depends groff-base (>= 1.18.1.1-15)
{% endhighlight %}

![Docker Aptitude: Why][image-ss-aptitude-why]{: .img-responsive }

#### Test

Removing <code>groff-base</code> would remove <code>man-db</code>.

{% highlight bash %}
$ apt remove groff-base
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following package was automatically installed and is no longer required:
  libpipeline1
Use 'apt autoremove' to remove it.
The following packages will be REMOVED:
  groff-base man-db
0 upgraded, 0 newly installed, 2 to remove and 0 not upgraded.
After this operation, 5624 kB disk space will be freed.
Do you want to continue? [Y/n]
{% endhighlight %}

![Docker APT: Test Dependency][image-ss-apt-test-remove]{: .img-responsive }

-- -- --

### Repository

Switch repository in Debian based is simple.

#### Configuration

Most of the time I manage repository using 
<code class="code-file">sources.list</code>
configuration directly in Debian based distribution.
I don't know if there is better way.

{% highlight bash %}
$ cat /etc/apt/sources.list
deb http://deb.debian.org/debian stretch main
deb http://deb.debian.org/debian stretch-updates main
deb http://security.debian.org stretch/updates main
{% endhighlight %}

![Docker sources.list: original][image-ss-sources-list]{: .img-responsive }

**Case**: Other derivation may use different repository.

{% highlight bash %}
deb http://auto.mirror.devuan.org/merged jessie          main
deb http://auto.mirror.devuan.org/merged jessie-updates  main
deb http://auto.mirror.devuan.org/merged jessie-security main

deb http://auto.mirror.devuan.org/merged ascii           main
{% endhighlight %}

**Case**: Sometimes there is additional PPA as well,
inside <code class="code-file">/etc/apt/sources.list.d/</code> directory.

{% highlight bash %}
deb http://ppa.launchpad.net/wagungs/kali-linux2/ubuntu raring main
deb-src http://ppa.launchpad.net/wagungs/kali-linux2/ubuntu raring main
deb http://ppa.launchpad.net/wagungs/kali-linux/ubuntu raring main
deb-src http://ppa.launchpad.net/wagungs/kali-linux/ubuntu raring main
{% endhighlight %}

#### Policy

However if you insist using command line,
you can get a list anyway by using <code>apt-cache policy</code>

{% highlight bash %}
$ apt policy
Package files:
 100 /var/lib/dpkg/status
     release a=now
 500 http://security.debian.org stretch/updates/main amd64 Packages
     release v=9,o=Debian,a=stable,n=stretch,l=Debian-Security,c=main,b=amd64
     origin security.debian.org
 500 http://deb.debian.org/debian stretch-updates/main amd64 Packages
     release o=Debian,a=stable-updates,n=stretch-updates,l=Debian,c=main,b=amd64
     origin deb.debian.org
 500 http://deb.debian.org/debian stretch/main amd64 Packages
     release v=9.1,o=Debian,a=stable,n=stretch,l=Debian,c=main,b=amd64
     origin deb.debian.org
Pinned packages:
{% endhighlight %}

![Docker APT: Policy][image-ss-apt-policy]{: .img-responsive }

#### Add Repository

This require a few steps.

(1)	Append the repository to /etc/apt/sources.list. I usually utilize text editor.

(2)	Add the repository key to the machine, if needed such as in PPA case.
Or maybe using <code>apt-key</code> if necessary.

(3) Do not forget to <code>apt update</code> to refresh.

Note that Ubuntu based have this <code>add-apt-repository</code> command.

#### Mirror

Consider change our repository to nearest local server such as university.

{% highlight bash %}
$ nano /etc/apt/sources.list
deb http://kambing.ui.ac.id/debian stretch main
deb http://kambing.ui.ac.id/debian stretch-updates main
deb http://security.debian.org stretch/updates main
{% endhighlight %}

![Docker sources.list: mirror][image-ss-nano-kambing]{: .img-responsive }

No need any repository key. 
We can update directly.

{% highlight bash %}
$ apt update
Ign:1 http://kambing.ui.ac.id/debian stretch InRelease
Get:2 http://kambing.ui.ac.id/debian stretch-updates InRelease [91.0 kB]
Get:3 http://kambing.ui.ac.id/debian stretch Release [118 kB]      
Hit:4 http://security.debian.org stretch/updates InRelease         
Get:5 http://kambing.ui.ac.id/debian stretch-updates/main amd64 Packages [5553 B]
Get:6 http://kambing.ui.ac.id/debian stretch Release.gpg [2373 B]
Get:7 http://kambing.ui.ac.id/debian stretch/main amd64 Packages [9497 kB]
Fetched 9714 kB in 47s (205 kB/s)                                                    
Reading package lists... Done
Building dependency tree       
Reading state information... Done
All packages are up to date.
{% endhighlight %}

![Docker APT: update kambing][image-ss-update-kambing]{: .img-responsive }

-- -- --

### Group

I cannot find any reference about group in APT.

#### Task

The closest concept about group in APT is,
by using <code>task</code>.

{% highlight bash %}
$ apt install tasksel
{% endhighlight %}

{% highlight bash %}
$ tasksel --list-tasks 
u desktop	Debian desktop environment
u gnome-desktop	GNOME
u xfce-desktop	Xfce
u kde-desktop	KDE
u cinnamon-desktop	Cinnamon
u mate-desktop	MATE
u lxde-desktop	LXDE
u web-server	web server
u print-server	print server
u ssh-server	SSH server
u laptop	laptop
{% endhighlight %}

![Docker APT: Task List (Group)][image-ss-tasksel-list]{: .img-responsive }

Now you can install. Note the additional prefix <code>task-</code>.

{% highlight bash %}
$ apt install task-xfce-desktop
{% endhighlight %}

More complete information provided in manual.

{% highlight bash %}
$ man tasksel
{% endhighlight %}

-- -- --

### Hold Package

APT can lock package using either hold or pinning.

#### Building Example

Suppose you want to do system upgrade,
but you do not want to upgrade certain package.
There is a good reason for these,
such as keeping old driver,
because the latest has a issue or such reason.
Or maybe we want to keep our current beloved newly compiled package
that equipped with super duper specific configuration parameter optimization.

Consider change <code>sources.list</code> repository,
from <code>stretch/stable</code> to <code>testing</code>.
We need an example package that we can hold as a guinea pig example.
We can achieveed this using <code>update</code>
and <code>upgrade</code> or <code>list --upgradable</code> .

{% highlight bash %}
$ nano /etc/apt/sources.list
deb http://deb.debian.org/debian testing main

# deb http://deb.debian.org/debian stretch main
# deb http://deb.debian.org/debian stretch-updates main

# deb http://kambing.ui.ac.id/debian stretch main
# deb http://kambing.ui.ac.id/debian stretch-updates main

# deb http://security.debian.org stretch/updates main
{% endhighlight %}

![Docker APT: Testing: sources.list][image-ss-h-sources-list]{: .img-responsive }

{% highlight bash %}
$ apt update
Get:1 http://deb.debian.org/debian testing InRelease [135 kB]
Get:2 http://deb.debian.org/debian testing/main amd64 Packages [9669 kB]
Fetched 9804 kB in 7s (1270 kB/s)                                           
Reading package lists... Done
Building dependency tree       
Reading state information... Done
73 packages can be upgraded. Run 'apt list --upgradable' to see them.
{% endhighlight %}

{% highlight bash %}
$ apt list --upgradable
Listing... Done
adduser/testing 3.116 all [upgradable from: 3.115]
apt/testing 1.5~rc1 amd64 [upgradable from: 1.4.7]
aptitude/testing 0.8.9-1 amd64 [upgradable from: 0.8.7-1]
aptitude-common/testing 0.8.9-1 all [upgradable from: 0.8.7-1]
base-files/testing 10 amd64 [upgradable from: 9.9+deb9u1]
bsdutils/testing 1:2.29.2-4 amd64 [upgradable from: 1:2.29.2-1]
{% endhighlight %}

And the winner is <code>apt</code> and <code>aptitude*</code>
as our guinea pig locking example.

![Docker APT: Testing: List Upgradable][image-ss-h-upgradable]{: .img-responsive }

#### Mark Hold

We can hold package easily using APT.
By using <code>apt-mark hold</code>.
Note that there is <code>no apt hold</code>.

{% highlight bash %}
$ apt-mark hold apt
apt set on hold.
{% endhighlight %}

![Docker APT: Mark Hold][image-ss-h-mark-hold]{: .img-responsive }

Now apt will be ignored when upgrade.

{% highlight bash %}
$ apt upgrade
Reading package lists... Done
Building dependency tree       
Reading state information... Done
Calculating upgrade... Done
The following packages were automatically installed and are no longer required:
  libperl5.24 perl-modules-5.24
Use 'apt autoremove' to remove them.
The following NEW packages will be installed:
  fdisk gcc-7-base libperl5.26 perl-modules-5.26
The following packages have been kept back:
  apt libapt-pkg5.0
The following packages will be upgraded:
  adduser aptitude aptitude-common base-files bsdutils dash
...
{% endhighlight %}

![Docker APT: apt Kept Back][image-ss-h-upgrade-kept]{: .img-responsive }

{% highlight bash %}
$ apt-mark unhold apt
Canceled hold on apt.
{% endhighlight %}
  
#### Pinning

Youcan also lock by Pinning using negative value.
Consider make a new <code>apt</code> file
in <code class="code-file">/etc/apt/preferences.d</code>

{% highlight bash %}
$ touch /etc/apt/preferences.d/apt

$ nano /etc/apt/preferences.d/apt
Package: apt
Pin: release a=testing
Pin-Priority: -1
{% endhighlight %}

<code>apt</code> won't even shown up in upgradable list.

![Docker APT: /etc/apt/preferences.d][image-ss-h-preferences-d]{: .img-responsive }

{% highlight bash %}
$ apt list --upgradable
Listing... Done
adduser/testing 3.116 all [upgradable from: 3.115]
aptitude/testing 0.8.9-1 amd64 [upgradable from: 0.8.7-1]
aptitude-common/testing 0.8.9-1 all [upgradable from: 0.8.7-1]
base-files/testing 10 amd64 [upgradable from: 9.9+deb9u1]
bsdutils/testing 1:2.29.2-4 amd64 [upgradable from: 1:2.29.2-1]
{% endhighlight %}

![Docker APT: Not Shown Up in Upgradable][image-ss-h-unupgradable]{: .img-responsive }

Example done successfully.
Guinea pig is alive.
Do not forget to unpin.

{% highlight bash %}
$ rm /etc/apt/preferences.d/apt
{% endhighlight %}

I personally used this method to **prevent** my <code>sis671</code> driver
to be updated for almost one and a half year.
Finally, the new driver works, so I do not pin this driver anymore.

-- -- --

### System Wide Information

There is this <code>stats</code> command
to dump the system wide information.

{% highlight bash %}
$ apt-cache stats
Total package names: 1275 (25.5 k)
Total package structures: 1275 (56.1 k)
  Normal packages: 388
  Pure virtual packages: 4
  Single virtual packages: 192
  Mixed virtual packages: 1
  Missing: 690
Total distinct versions: 389 (31.1 k)
Total distinct descriptions: 389 (9336 )
Total dependencies: 2677/1765 (81.8 k)
Total ver/file relations: 389 (9336 )
Total Desc/File relations: 389 (9336 )
Total Provides mappings: 201 (4824 )
Total globbed strings: 2808 (39.1 k)
Total slack space: 20.8 k
Total space accounted for: 691 k
Total buckets in PkgHashTable: 50503
  Unused: 49243
  Used: 1260
  Utilization: 2.4949%
  Average entries: 1.0119
  Longest: 3
  Shortest: 1
Total buckets in GrpHashTable: 50503
  Unused: 49243
  Used: 1260
  Utilization: 2.4949%
  Average entries: 1.0119
  Longest: 3
  Shortest: 1
{% endhighlight %}

![Docker APT: cache-stats][image-ss-cache-stats]{: .img-responsive }

-- -- --

### History

This is most the forgotten part of package management,
although it is not uncommon to notice messages.

#### The Log File

There are few log files.

*	/var/log/dpkg.log

*	/var/log/apt/history.log

*	/var/log/apt/term.log

*	/var/log/aptitude

<code>less</code> or <code>more</code> is a good tool to read log file.
Most likely you want the <code>tail</code>, latest transaction,
at the bottom of the recorded event.

{% highlight bash %}
$ tail /var/log/apt/history.log
Install: dselect:amd64 (1.18.24)
End-Date: 2017-09-10  18:28:00

Start-Date: 2017-09-10  19:07:40
End-Date: 2017-09-10  19:07:40

Start-Date: 2017-09-10  20:27:17
Commandline: apt install tasksel
Install: tasksel-data:amd64 (3.39, automatic), laptop-detect:amd64 (0.13.8, automatic), dmidecode:amd64 (3.0-4, automatic), tasksel:amd64 (3.39)
End-Date: 2017-09-10  20:27:23
{% endhighlight %}

![Docker: /var/log/apt/history.log][image-ss-tail-log-apt]{: .img-responsive }

Here is another

{% highlight bash %}
$ tail /var/log/dpkg.log
2017-09-11 13:52:44 status unpacked lynx:amd64 2.8.9dev11-1
2017-09-11 13:52:44 status half-configured lynx:amd64 2.8.9dev11-1
2017-09-11 13:52:44 status installed lynx:amd64 2.8.9dev11-1
2017-09-11 13:52:44 configure fish:amd64 2.4.0-1 <none>
2017-09-11 13:52:44 status unpacked fish:amd64 2.4.0-1
2017-09-11 13:52:44 status half-configured fish:amd64 2.4.0-1
2017-09-11 13:52:44 status installed fish:amd64 2.4.0-1
2017-09-11 13:52:44 trigproc libc-bin:amd64 2.24-11+deb9u1 <none>
2017-09-11 13:52:44 status half-configured libc-bin:amd64 2.24-11+deb9u1
2017-09-11 13:52:44 status installed libc-bin:amd64 2.24-11+deb9u1
{% endhighlight %}

![Docker: /var/log/dpkg.log][image-ss-tail-log-dpkg]{: .img-responsive }

-- -- --

### Clean Up

APT as default keep downloaded package.

Package Cache
	
*	/var/cache/apt/archives/ * .deb

You can clean this directory by using either

{% highlight bash %}
$ apt clean

$ apt-get clean

$ aptitude clean
{% endhighlight %}

![Docker APT: clean][image-ss-apt-clean]{: .img-responsive }

{% highlight bash %}
$ apt autoclean
Reading package lists... Done
Building dependency tree       
Reading state information... Done

$ apt-get autoclean
Reading package lists... Done
Building dependency tree       
Reading state information... Done

$ aptitude autoclean
Freed 0 B of disk space   
{% endhighlight %}

![Docker APT: autoclean][image-ss-apt-autoclean]{: .img-responsive }

Note that we do not need to do clean up with docker
because of these rules.

{% highlight bash %}
$ less /etc/apt/apt.conf.d/docker-clean
DPkg::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };
APT::Update::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };
{% endhighlight %}

![Docker APT: autoclean][image-ss-docker-clean]{: .img-responsive }

-- -- --

### What's Next

There are still, some interesting topic for <code>APT</code>.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}

[local-part-three]:   {{ site.url }}/system/2017/08/26/docker-debian-apt.html

[image-ss-apt-depends]:		{{ asset_post }}/14-apt-depends.png
[image-ss-apt-rdepends]:	{{ asset_post }}/14-apt-rdepends.png
[image-ss-aptitude-s-d]:	{{ asset_post }}/14-aptitude-d-groff-base.png
[image-ss-aptitude-s-r]:	{{ asset_post }}/14-aptitude-r-man-db.png
[image-ss-aptitude-why]:	{{ asset_post }}/14-aptitude-why-groff-base.png

[image-ss-apt-policy]:		{{ asset_post }}/16-apt-policy.png
[image-ss-sources-list]:	{{ asset_post }}/16-etc-apt-sources-list.png
[image-ss-nano-kambing]:	{{ asset_post }}/16-nano-kambing.png
[image-ss-update-kambing]:	{{ asset_post }}/16-update-kambing.png
[image-ss-apt-test-remove]:	{{ asset_post }}/16-test-remove.png

[image-ss-tasksel-list]:	{{ asset_post }}/15-group-task.png
[image-ss-apt-clean]:		{{ asset_post }}/17-clean.png
[image-ss-apt-autoclean]:	{{ asset_post }}/17-autoclean.png
[image-ss-docker-clean]:	{{ asset_post }}/17-docker-clean.png
[image-ss-tail-log-apt]:	{{ asset_post }}/19-tail-log-apt.png
[image-ss-tail-log-dpkg]:	{{ asset_post }}/19-tail-log-dpkg.png

[image-ss-cache-stats]:		{{ asset_post }}/19-stats.png

[image-ss-h-upgradable]:	{{ asset_post }}/27-list-upgradable.png
[image-ss-h-sources-list]:	{{ asset_post }}/27-sources-list-testing.png
[image-ss-h-mark-hold]:		{{ asset_post }}/27-mark-hold.png
[image-ss-h-upgrade-kept]:	{{ asset_post }}/27-upgrade-kept-back.png
[image-ss-h-preferences-d]:	{{ asset_post }}/27-preferences-d.png
[image-ss-h-unupgradable]:	{{ asset_post }}/27-upgradable-pinned.png

