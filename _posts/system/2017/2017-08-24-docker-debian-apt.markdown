---
layout: post
title: "Docker - Debian APT - Part Two"
date: 2017-08-24 09:35:15 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082035  # Slackware Package
  - 17081845  # Fedora DNF
# - 17081535  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081435  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

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

Most of the time I manage repository using configuration directly,
in Debian based distribution. I don't know if there is better way.

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
inside <code>/etc/apt/sources.list.d/</code> directory.

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

### What's Next

There are still, some interesting topic for <code>APT</code>.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}

[local-part-three]:   {{ site.url }}/system/2017/08/25/docker-debian-apt.html

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
