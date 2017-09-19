---
layout: post
title: "Docker - Arch ALPM - Part Three"
date: 2017-08-29 09:35:15 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
# - 17082715  # Arch ALPM
  - 17082415  # Debian Portage
  - 17082115  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-arch-alpm.html %}

-- -- --

### Introducing AUR

One of the most important strength to Arch,
is the number of packages can be achieved from AUR (Arch User Repository).
In order to use AUR you have to go through automatic build process.
Don't be scared, this process is easy, and won't hurt you.

	Personally I love the colored output

In case you forget, this good reading is a must.

*	https://wiki.archlinux.org/index.php/Arch_User_Repository

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

#### makepkg

There is this <code>makepkg</code> at the heart of automatic build.
This will compile package, and create the <code>.tar.xz</code> ALPM package.
In order to do this we need to change our working directory first.
This is a long verbose output.

Although we can have AUR Frontend as described later,
sometimes we need to go to lower level, use this sweet good <code>makepkg</code>.

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

#### makepkg Feature

Consider this <code>asp-git</code> package form AUR.

{% highlight bash %}
$ wget https://aur.archlinux.org/cgit/aur.git/snapshot/asp-git.tar.gz
$ tar -xvf asp-git.tar.gz
$ cd asp-git
{% endhighlight %}

You can download package using  <code>--nobuild</code>.
This is useful if you want to alter the configuration,
or even the source code.

{% highlight bash %}
$ makepkg -o
==> Making package: asp-git 1.3.g375b035-1 (Tue Sep 19 17:02:41 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Cloning asp git repo...
Cloning into bare repository '/home/epsi/asp-git/asp'...
remote: Counting objects: 347, done.
remote: Total 347 (delta 0), reused 0 (delta 0), pack-reused 347
Receiving objects: 100% (347/347), 65.53 KiB | 175.00 KiB/s, done.
Resolving deltas: 100% (204/204), done.
==> Validating source files with md5sums...
    asp ... Skipped
==> Extracting sources...
  -> Creating working copy of asp git repo...
Cloning into 'asp'...
done.
==> Starting pkgver()...
==> Updated version: asp-git 2-1
==> Sources are ready.
{% endhighlight %}

![Docker makepkg: nobuild][image-ss-asp-nobuild]{: .img-responsive }

Now you can continue, without cloning.

{% highlight bash %}
$ makepkg
==> Making package: asp-git 2-1 (Tue Sep 19 17:07:18 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Updating asp git repo...
Fetching origin
==> Validating source files with md5sums...
    asp ... Skipped
==> Extracting sources...
  -> Creating working copy of asp git repo...
Switched to a new branch 'makepkg'
==> Starting pkgver()...
==> Starting build()...
...
{% endhighlight %}

![Docker makepkg: asp-git][image-ss-asp-makepkg]{: .img-responsive }

You can also use <code>--install</code> to automatically install.

{% highlight bash %}
$ makepkg -i
{% endhighlight %}

Equal to:

{% highlight bash %}
$ makepkg --install
==> Making package: asp-git 2-1 (Tue Sep 19 17:12:18 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Updating asp git repo...
Fetching origin
==> Validating source files with md5sums...
    asp ... Skipped
==> Extracting sources...
  -> Creating working copy of asp git repo...
Reset branch 'makepkg'
==> Starting pkgver()...
==> WARNING: A package has already been built, installing existing package...
==> Installing package asp-git with pacman -U...
[sudo] password for epsi: 
...
{% endhighlight %}

![Docker makepkg: install][image-ss-asp-install]{: .img-responsive }

#### Package Query

<code>package-query</code> is,
the base dependency of some AUR tools,
such as <code>cower</code> and <code>yaourt</code>.

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

There is AUR tools that could make our life easier such as <code>cower</code>.
First we have to find the <code>cower</code> origin.
Since cower is AUR package, we can repeat the same process,
like what we does to package-query to cower.

{% highlight bash %}
$ package-query --aur --sync --query cower
aur/cower 17-2 (932) (28.91)

$ cd ~

$ wget -q https://aur.archlinux.org/cgit/aur.git/snapshot/cower.tar.gz

$ tar -xzf cower.tar.gz 

$ cd cower
{% endhighlight %}

![Docker AUR: Cower: Build][image-ss-cower-build]{: .img-responsive }

There are these errors however.

{% highlight bash %}
$ makepkg -i
...
==> Verifying source file signatures with gpg...
    cower-17.tar.gz ... FAILED (unknown public key 1EB2638FF56C0C53)
==> ERROR: One or more PGP signatures could not be verified!
{% endhighlight %}

Use <code>--skippgpcheck</code> to pass PGP Check.

{% highlight bash %}
$ makepkg -i --skippgpcheck
...
/bin/sh: pod2man: command not found
make: *** [Makefile:90: cower.1] Error 127
==> ERROR: A failure occurred in build().
    Aborting...
{% endhighlight %}

And fix Perl <code>$PATH</code> if necessary.

{% highlight bash %}
$ touch .profile

$ nano .profile

$ cat .profile
export PATH=$PATH:/usr/bin/core_perl

$ source .profile && export PATH

$ echo $PATH
/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/bin/core_perl
{% endhighlight %}

{% highlight bash %}
$ cd ~/cower
$ makepkg -i --skippgpcheck
makepkg -i --skippgpcheck
==> WARNING: A package has already been built, installing existing package...
==> Installing package cower with pacman -U...
...
{% endhighlight %}

![Docker AUR: Cower: makepkg][image-ss-cower-makepkg]{: .img-responsive }

Always RTFM

{% highlight bash %}
$ man cower
{% endhighlight %}

Consider see cower in action

{% highlight bash %}
$ cower -c -s cower
aur/burgaur 2.2-2 (7, 0.10)
    A delicious AUR helper. Made from cower.
aur/burgaur-git 2.2-2 (1, 0.49)
    A delicious AUR helper. Made from cower.
aur/cower 17-2 (932, 28.91) [installed]
    A simple AUR agent with a pretentious name
aur/cower-git 17-1 (81, 2.11)
    A simple AUR agent with a pretentious name
aur/owlman 0.8-1 (1, 0.00)
    A pacman and cower wrapper focused on simplicity
{% endhighlight %}

![Docker AUR: Cower: Action][image-ss-cower-action]{: .img-responsive }

You can have more screenshot here

*	[Unbundling AUR Helper Process][local-unbundling]

#### Foreign Package

We can identify any foreign package, such as build from AUR. 

{% highlight bash %}
$ pacman -Qm
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --query --foreign
asp-git 2-1
aura-bin 1.3.9-1
cower 17-2
pacaur 4.7.10-1
package-query 1.9-2
packer 20160325-1
yaourt 1.9-1
{% endhighlight %}

![Docker pacman: query foreign][image-ss-query-foreign]{: .img-responsive }

-- -- --

### ABS

AUR is not the only ALPM's strength,
You can also build official package.
Sometimes we need to build instead of binary
to add build configuration to enable feature that was disabled,
or disable feature we do not want,
or any change in the build  for an official package.

ABS stand for _The Arch Build System_.

*	<https://wiki.archlinux.org/index.php/Arch_Build_System>

#### Deprecation

ASP is an alternative for ABS that has been deprecated.

*	<https://www.archlinux.org/news/deprecation-of-abs/>

#### ASP

Using <code>asp</code> is as simple as below:

{% highlight bash %}
$ asp export ncdu
==> Initializing ASPROOT in /home/epsi/.cache/asp
remote: Counting objects: 100, done.
remote: Compressing objects: 100% (80/80), done.
remote: Total 100 (delta 9), reused 78 (delta 5)
Receiving objects: 100% (100/100), 14.46 KiB | 7.23 MiB/s, done.
Resolving deltas: 100% (9/9), done.
From https://git.archlinux.org/svntogit/community
 * branch            packages/ncdu -> FETCH_HEAD
 * [new branch]      packages/ncdu -> community/packages/ncdu
==> exporting ncdu:trunk
{% endhighlight %}

Now we can continue with <code>makepkg</code>.

{% highlight bash %}
$ cd ncdu

$ makepkg -i --skippgpcheck
{% endhighlight %}

![Docker ASP: export ncdu][image-ss-asp-ncdu]{: .img-responsive }

#### Using SVN

This is a lower level method than ASP for curious reader.
But first you have to install SVN.

{% highlight bash %}
$ sudo pacman -S svn
{% endhighlight %}

I was a little bit confused, about this SVN thing.
But then I got helpful answer from community.
_Dean Wallace_ has pointed me to the right direction.
And suggest to stick with <code>asp</code>

{% highlight bash %}
$ svn checkout --depth=empty svn://svn.archlinux.org/packages
Checked out revision 305653.

$ svn checkout --depth=empty svn://svn.archlinux.org/community
Checked out revision 257755.

$ svn update ncdu
Updating 'ncdu':
A    ncdu
A    ncdu/trunk
A    ncdu/trunk/PKGBUILD
A    ncdu/repos
A    ncdu/repos/community-i686
A    ncdu/repos/community-i686/PKGBUILD
A    ncdu/repos/community-x86_64
A    ncdu/repos/community-x86_64/PKGBUILD
Updated to revision 257755.
{% endhighlight %}

![Docker SVN: update ncdu][image-ss-svn-ncdu]{: .img-responsive }

Now you can continue with newly downloaded PKGBUILD.

-- -- --

### What's Next

There are still, some <code>AUR</code> topic to go.
Consider finish reading [ [Part Four][local-part-four] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}

[local-part-four]:		{{ site.url }}/system/2017/08/30/docker-arch-alpm.html

[local-unbundling]:		{{ site.url }}/system/2014/12/26/unbundling-aur-helper-process.html

[image-ss-user-privilege]:	{{ asset_post }}/25-user-privilege.png
[image-ss-pq-action]:		{{ asset_post }}/25-action-package-query.png
[image-ss-pq-pkgbuild]:		{{ asset_post }}/25-cat-pkgbuild.png
[image-ss-pq-makepkg]:		{{ asset_post }}/25-makepkg-package-query.png
[image-ss-pq-install]:		{{ asset_post }}/25-upgrade-package-query.png
[image-ss-pq-wget-aur]:		{{ asset_post }}/25-wget-aur-package-query.png

[image-ss-cower-action]:	{{ asset_post }}/25-action-cower-cs.png
[image-ss-cower-build]:		{{ asset_post }}/25-build-cower.png
[image-ss-cower-makepkg]:	{{ asset_post }}/25-makepkg-cower.png

[image-ss-asp-makepkg]:		{{ asset_post }}/25-makepkg-asp-git.png
[image-ss-asp-install]:		{{ asset_post }}/25-makepkg-asp-git-install.png
[image-ss-asp-nobuild]:		{{ asset_post }}/25-makepkg-asp-git-nobuild.png

[image-ss-asp-ncdu]:		{{ asset_post }}/27-asp-ncdu.png
[image-ss-svn-ncdu]:		{{ asset_post }}/27-svn-community-ncdu.png

[image-ss-query-foreign]:	{{ asset_post }}/19-foreign-package.png
