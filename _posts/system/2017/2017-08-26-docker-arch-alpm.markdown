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

### makepkg

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

-- -- --

### AUR Helper

Consider make our live easier with AUR Helper.
Reading official documentation is a must.

*	https://wiki.archlinux.org/index.php/AUR_helpers

#### yaourt

I also wrote an article once.
With some configuration explained.

*	[Install Yaourt, the AUR Helper][local-install-yaourt]

Now consider cower again.

{% highlight bash %}
$ cd ~

$ cower -d -c yaourt
:: yaourt downloaded to /home/epsi

$ cd yaourt

$ makepkg -i
makepkg -i
==> Making package: yaourt 1.9-1 (Sat Sep 16 04:20:38 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading yaourt-1.9.tar.gz...
...
{% endhighlight %}

![Docker AUR: Yaourt: Install][image-ss-yaourt-install]{: .img-responsive }

Always RTFM

{% highlight bash %}
$ man yaourt
{% endhighlight %}

Consider use <code>yaourt</code> to query other AUR Helper.

{% highlight bash %}
$ yaourt pacaur
...
3 aur/pacaur 4.7.10-1 (1001) (52.44)
    An AUR helper that minimizes user interaction
...
==> Enter n° of packages to be installed (e.g., 1 2 3 or 1-3)
==> ----------------------------------------------------------
==> 3
{% endhighlight %}

![Docker AUR: Yaourt: Action][image-ss-yaourt-action]{: .img-responsive }

Pressing <code>3</code> and <code>Enter</code> will install <code>aur/pacaur</code>

Yaourt comes with config file.
Just copy them to your home directory

{% highlight bash %}
$ cat /etc/yaourtrc
$ cp /etc/yaourtrc ~/.yaourtrc
{% endhighlight %}

I left my <code class="code-file">~/.yauortrc</code> config as below to avoid
too many confirmation question from yaourt.

{% highlight conf %}
$ cat ~/.yauortrc
# SUDO
SUDONOVERIF=1      # Avoid multiple sudo checks when timestamp_timeout=0

# Prompt
BUILD_NOCONFIRM=1  # Only prompt for editing files
EDITFILES=0

# Command
MAKEPKG="makepkg --skippgpcheck"
{% endhighlight %}

![Docker AUR: Yaourtrc: ViM][image-ss-yaourtrc-vim]{: .img-responsive }

This configuration is very helpful
if you maintain your AUR upgrade regularly
with a lot of Syua option in yaourt.

This <code>yaourt -Syua</code> command will update all your AUR at once.
Make sure you know what you are doing when skipping PGP Verification.

{% highlight bash %}
$ yaourt -Syua
{% endhighlight %}

![Docker AUR: yaourt -Syua][image-ss-yaourt-syua]{: .img-responsive }

#### pacaur

Set editor as environment variable if necessary.

{% highlight bash %}
$ cat .profile
export EDITOR=vim

$ . .profile
{% endhighlight %}

Consider use <code>pacaur</code> to query other AUR Helper.

{% highlight bash %}
$ pacaur -S packer
:: Package packer not found in repositories, trying AUR...
:: resolving dependencies...
:: looking for inter-conflicts...
:: packer-20150808-1 has been flagged out of date on Sun Apr  2 10:26:23 2017

AUR Packages  (1) packer-20150808-1  
Repo Packages (2) jansson-2.10-2  jshon-20131105-1  

Repo Download Size:   0.05 MiB
Repo Installed Size:  0.19 MiB

:: Proceed with installation? [Y/n] 
...
==> Cleaning up...
:: Installing packer package(s)...
:: packer package(s) failed to install.
:: ensure package version does not mismatch between .SRCINFO and PKGBUILD
:: ensure package name has a VCS suffix if this is a devel package
:: jshon is now an orphan package
{% endhighlight %}

![Docker AUR: pacaur -S packer][image-ss-pacaur-packer]{: .img-responsive }

On most cases <code>pacaur</code> goes well.
But oouch, sometimes an issue happened such as _failed to install_.
This time we need a lower level method.

{% highlight bash %}
$ cower -d packer
:: packer downloaded to /home/epsi

$ cd packer/

$ makepkg
==> Making package: packer 20150808-1 (Sat Sep 16 05:21:21 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Cloning packer git repo...

$ sudo pacman -U packer-20160325-1-any.pkg.tar.xz 
...
:: Processing package changes...
(1/1) installing packer                            [##########] 100%
...
{% endhighlight %}

#### packer

Consider use <code>packer</code> to query other AUR Helper.

{% highlight bash %}
$ packer -S aura-bin
packer -S aura-bin

Aur Targets    (1): aura-bin

Proceed with installation? [Y/n] y
Edit aura-bin PKGBUILD with $EDITOR? [Y/n] n
==> Making package: aura-bin 1.3.9-1 (Sat Sep 16 05:28:01 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading aura-1.3.9-x86_64.tar.gz...
...
==> Checking for packaging issue...
==> Creating package "aura-bin"...
  -> Generating .PKGINFO file...
  -> Generating .BUILDINFO file...
  -> Generating .MTREE file...
  -> Compressing package...
==> Leaving fakeroot environment.
==> Finished making: aura-bin 1.3.9-1 (Sat Sep 16 05:28:47 UTC 2017)
{% endhighlight %}

![Docker AUR: packer -S aura-bin][image-ss-packer-aura-bin]{: .img-responsive }

I like to have aliases in <code>.bashrc</code>
so I do not have to remember all the options.
This is an unoffocial tip for unofficial package.

{% highlight bash %}
alias pksyu="packer -Syu --noconfirm --noedit"
{% endhighlight %}

![Docker AUR: alias packer][image-ss-packer-alias]{: .img-responsive }

{% highlight bash %}
$ pksyu
[sudo] password for epsi: 
:: Synchronizing package databases...
 core is up to date
 extra is up to date
 community is up to date
:: Starting full system upgrade...
 there is nothing to do
:: Synchronizing aur database...
 aur                                          6  6 [##########]100%
:: Starting full aur upgrade...
 local database is up to date
 {% endhighlight %}

![Docker AUR: pksyu][image-ss-pkyu]{: .img-responsive }

There are still some other AUR helpers, but I never use any of them.

-- -- --

### ASP

#### Deprecated ABS

{% highlight bash %}
$ 
{% endhighlight %}

#### Using SVN

{% highlight bash %}
$ 
{% endhighlight %}

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

[local-unbundling]:		{{ site.url }}/system/2014/12/26/unbundling-aur-helper-process.html
[local-install-yaourt]:	{{ site.url }}/system/2016/06/21/install-yaourt.html

[image-ss-user-privilege]:	{{ asset_post }}/25-user-privilege.png
[image-ss-pq-action]:		{{ asset_post }}/25-action-package-query.png
[image-ss-pq-pkgbuild]:		{{ asset_post }}/25-cat-pkgbuild.png
[image-ss-pq-makepkg]:		{{ asset_post }}/25-makepkg-package-query.png
[image-ss-pq-install]:		{{ asset_post }}/25-upgrade-package-query.png
[image-ss-pq-wget-aur]:		{{ asset_post }}/25-wget-aur-package-query.png

[image-ss-cower-action]:	{{ asset_post }}/25-action-cower-cs.png
[image-ss-cower-build]:		{{ asset_post }}/25-build-cower.png
[image-ss-cower-makepkg]:	{{ asset_post }}/25-makepkg-cower.png

[image-ss-yaourt-install]:	{{ asset_post }}/26-install-yaourt.png
[image-ss-yaourt-action]:	{{ asset_post }}/26-yaourt-pacaur.png
[image-ss-yaourtrc-vim]:	{{ asset_post }}/26-vim-yaourtrc.png
[image-ss-yaourt-syua]:		{{ asset_post }}/26-yaourt-syua.png
[image-ss-pacaur-packer]:	{{ asset_post }}/26-pacaur-packer.png
[image-ss-packer-aura-bin]:	{{ asset_post }}/26-packer-aura-bin.png
[image-ss-packer-alias]:	{{ asset_post }}/26-packer-alias.png
[image-ss-pkyu]:	{{ asset_post }}/26-pksyu.png
