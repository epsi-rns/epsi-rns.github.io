---
layout: post
title: "Docker - Arch ALPM - Part Three"
date: 2017-08-26 09:35:15 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
# - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-debian-apt.html %}

-- -- --

### Introducing AUR

One of the most important strength to Arch,
is the number of packages can be achieved from AUR (Arch User Repository).
In order to use AUR you have to go through automatic build process.
Don't be scared, this process is easy, and won't hurt you.

It is a good idea to compile as a non-root user.
Consider prepare a user if not exist yet.

#### User Privilege

Build require regular user, non root one. Consider make one first.

{% highlight bash %}
$ useradd -m -g users -G wheel -s /bin/bash epsi

$ groups epsi
epsi : users sudo

$ passwd epsi
Enter new UNIX password: 
Retype new UNIX password: 
passwd: password updated successfully

$ su epsi

$ whoami
epsi
{% endhighlight %}

![Docker AUR: Add User and Privilege][image-ss-user-privilege]{: .img-responsive }

You should also setup sudoers,
otherwise this message.

{% highlight bash %}
$ sudo nano
epsi is not in the sudoers file.  This incident will be reported.
{% endhighlight %}

{% highlight bash %}
$ nano /etc/sudoers
epsi    ALL=(ALL:ALL) ALL
{% endhighlight %}

{% highlight bash %}
$ su epsi
$ cd ~
{% endhighlight %}

#### PKGBUILD

AUR handle source code only in one text file named <code>PKGBUILD</code>.
Indeed a smart design, only one text file.

Consider check this <code>package query</code> package.

*	https://aur.archlinux.org/packages/package-query/

You can read <code>PKGBUILD</code> here.

*	https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=package-query

Consider download the PKGBUILD snapshot.

*	https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz

{% highlight bash %}
$ wget https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz
--2017-09-15 21:23:58--  https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz
Loaded CA certificate '/etc/ssl/certs/ca-certificates.crt'
...
{% endhighlight %}

![Docker AUR: Package Query: Download][image-ss-pq-wget-aur]{: .img-responsive }

{% highlight bash %}
$ tar -xzvf package-query.tar.gz 
package-query/
package-query/.SRCINFO
package-query/PKGBUILD
{% endhighlight %}

{% highlight bash %}
$ cat package-query/PKGBUILD 
# Contributor: tuxce <tuxce.net@gmail.com>
# Contributor: Skunnyk <skunnyk@archlinux.fr>
pkgname=package-query
pkgver=1.9
pkgrel=2
pkgdesc="Query ALPM and AUR"
arch=('i686' 'x86_64' 'mips64el' 'armv6h' 'armv7h' 'arm' 'aarch64')
url="https://github.com/archlinuxfr/package-query/"
license=('GPL')
depends=('pacman>=5.0' 'yajl>=2.0')
{% endhighlight %}

![Docker AUR: Package Query: PKGBUILD][image-ss-pq-pkgbuild]{: .img-responsive }

As you can see, it needs <code>yajl</code> _Yet Another JSON Library_,
therefore we should install <code>yajl</code> first.
This time using <code>sudo</code>.

{% highlight bash %}
$ sudo pacman -S yajl
resolving dependencies...
looking for conflicting packages...

Packages (1) yajl-2.1.0-1

Total Download Size:   0.03 MiB
Total Installed Size:  0.16 MiB

:: Proceed with installation? [Y/n]
{% endhighlight %}

### makepkg

There is this <code>makepkg</code> at the heart of automatic build.
This will compile package, and create the <code>.tar.xz</code> ALPM package.
In order to do this we need to change our working directory first.
This is a long verbose output.

{% highlight bash %}
$ cd package-query

$ makepkg
==> Making package: package-query 1.9-2 (Fri Sep 15 21:44:23 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading package-query-1.9.tar.gz...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  357k  100  357k    0     0   178k      0  0:00:02  0:00:02 --:--:--  136k
==> Validating source files with sha256sums...
    package-query-1.9.tar.gz ... Passed
==> Extracting sources...
  -> Extracting package-query-1.9.tar.gz with bsdtar
==> Starting build()...
checking for a BSD-compatible install... /usr/bin/install -c
...
checking that generated files are newer than configure... done
configure: creating ./config.status
config.status: creating src/Makefile
...
make  all-recursive
make[1]: Entering directory '/home/epsi/package-query/src/package-query-1.9'
...
make[1]: Leaving directory '/home/epsi/package-query/src/package-query-1.9'
==> Tidying install...
  -> Removing libtool files...
  -> Purging unwanted files...
  -> Removing static library files...
  -> Stripping unneeded symbols from binaries and libraries...
  -> Compressing man and info pages...
==> Checking for packaging issue...
==> Creating package "package-query"...
  -> Generating .PKGINFO file...
  -> Generating .BUILDINFO file...
  -> Generating .MTREE file...
  -> Compressing package...
==> Leaving fakeroot environment.
==> Finished making: package-query 1.9-2 (Fri Sep 15 21:44:42 UTC 2017)
{% endhighlight %}

![Docker AUR: Package Query: makepkg][image-ss-pq-makepkg]{: .img-responsive }

If build goes well, the package should be ready to served.

{% highlight bash %}
$ ls -l
total 400
-rw-r--r-- 1 epsi users    754 Jul 20 02:06 PKGBUILD
-rw-r--r-- 1 epsi users  27336 Sep 15 21:40 package-query-1.9-2-x86_64.pkg.tar.xz
-rw-r--r-- 1 epsi users 366513 Sep 15 21:39 package-query-1.9.tar.gz
drwxr-xr-x 3 epsi users   4096 Sep 15 21:40 pkg
drwxr-xr-x 3 epsi users   4096 Sep 15 21:39 src
{% endhighlight %}

And install using <code>--upgrade</code>

{% highlight bash %}
$ sudo pacman -U package-query-1.9-2-x86_64.pkg.tar.xz 
{% endhighlight %}

Equal to:

{% highlight bash %}
$ sudo pacman --upgrade package-query-1.9-2-x86_64.pkg.tar.xz 
[sudo] password for epsi: 
loading packages...
resolving dependencies...
looking for conflicting packages...

Packages (1) package-query-1.9-2

Total Installed Size:  0.08 MiB

:: Proceed with installation? [Y/n] 
{% endhighlight %}

![Docker AUR: Package Query: Install][image-ss-pq-install]{: .img-responsive }

You can also use <code>--install</code> to automatically install.

{% highlight bash %}
$ makepkg -i
{% endhighlight %}

Equal to:

{% highlight bash %}
$ makepkg --install
{% endhighlight %}

#### Package Query

<code>package-query</code> is,
the base dependency of some AUR tools,
such as <code>cower</code> and <code>yaourt</code>

Now that we already have <code>package-query</code> installed.
Consider see it in action, querying _sync_, _local_, and _AUR_.

{% highlight bash %}
$ cd ..

$ package-query -S package-query

$ package-query -Q package-query
local/package-query 1.9-2

$ package-query -A package-query
aur/package-query 1.9-2 [installed] (1148) (9.78)
{% endhighlight %}

![Docker AUR: Package Query: Action][image-ss-pq-action]{: .img-responsive }

In its manual, you can see that <code>package-query</code>
provide flexible nice formatting.

#### cower

-- -- --

### AUR Frontend

#### yaourt

#### packer

#### pacaur

#### aura

-- -- --

### ASP

#### Deprecated ABS

#### Using SVN

-- -- --

### Screenshot

-- -- --

### Conclusion

-- -- --

### Not Finished Yet

TO DO

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}

[image-ss-pq-action]:		{{ asset_post }}/25-action-package-query.png
[image-ss-pq-pkgbuild]:		{{ asset_post }}/25-cat-pkgbuild.png
[image-ss-pq-makepkg]:		{{ asset_post }}/25-makepkg.png
[image-ss-pq-install]:		{{ asset_post }}/25-upgrade-package-query.png
[image-ss-user-privilege]:	{{ asset_post }}/25-user-privilege.png
[image-ss-pq-wget-aur]:		{{ asset_post }}/25-wget-aur-package-query.png
