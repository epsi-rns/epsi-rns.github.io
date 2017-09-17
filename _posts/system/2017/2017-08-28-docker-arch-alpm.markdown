---
layout: post
title: "Docker - Arch ALPM - Part Two"
date: 2017-08-28 09:35:15 +0700
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
# - 17082715  # Arch ALPM
  - 17082415  # Debian Portage
  - 17082115  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Portse

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-arch-alpm.html %}

-- -- --

### Dependency

There are two main topics in package dependency,
the _dependency_ itself, and _reverse dependency_.

*	Dependency

	Package that required by: such as man-db need groff-base and other.

*	Reverse Dependency

	Package that require: such as groff-base needed by man-db or other.

Luckily both can be achieved with single command.

#### Using pacman

In short, <code>pacman -[Q|S]ii</code>.

{% highlight bash %}
$ pacman --sync --info --info groff
..
Depends On      : perl  gcc-libs
Optional Deps   : netpbm: for use together with man -H command
                  interaction in browsers
                  psutils: for use together with man -H command
                  interaction in browsers
                  libxaw: for gxditview
Required By     : man-db
...
{% endhighlight %}

![Docker pacman: Info groff][image-ss-info-groff]{: .img-responsive }

{% highlight bash %}
$ pacman -Qii man-db
Depends On      : bash  gdbm  zlib  groff  libpipeline  less
Optional Deps   : gzip [installed]
Required By     : None
{% endhighlight %}

![Docker pacman: Info man-db][image-ss-info-man-db]{: .img-responsive }

#### Test

Removing <code>groff-base</code>.
We ahve already discuss this on package removal.

{% highlight bash %}
$ pacman --remove groff
checking dependencies...
error: failed to prepare transaction (could not satisfy dependencies)
:: man-db: removing groff breaks dependency 'groff'
{% endhighlight %}

#### Using pactree

There is a cool official tool, the <code>pactree</code>.

{% highlight bash %}
$ pactree man-db --depth 2 --color
man-db
├─bash
│ ├─readline
│ ├─glibc
│ └─ncurses
├─gdbm
│ ├─glibc
│ └─bash provides sh
├─zlib
│ └─glibc
├─groff
│ ├─perl
│ └─gcc-libs
├─libpipeline
│ └─glibc
└─less
  ├─glibc
  ├─ncurses
  └─pcre
{% endhighlight %}

![Docker pactree: Depth][image-ss-pactree-depth]{: .img-responsive }

It could also resolve reverse dependency well.

{% highlight bash %}
$ pactree groff --color --reverse
groff
└─man-db
{% endhighlight %}

![Docker pactree: Reverse][image-ss-pactree-reverse]{: .img-responsive }

-- -- --

### Repository

Switch repository in Arch based is simple.

#### Configuration

Most of the time I manage repository using 
<code class="code-file">pacman.conf</code>
configuration directly in Arch based distribution.
I don't know if there is better way.

{% highlight bash %}
$ cat /etc/pacman.conf
# REPOSITORIES
#   - can be defined here or included from another file

[core]
Include = /etc/pacman.d/mirrorlist

[extra]
Include = /etc/pacman.d/mirrorlist

[community]
Include = /etc/pacman.d/mirrorlist
{% endhighlight %}

![Docker Arch: pacman.conf][image-ss-pacman-conf]{: .img-responsive }

**Case**: Other distribution utilized ALPM may use different repository.

{% highlight bash %}
# Artix repos
[system]
Include = /etc/pacman.d/mirrorlist
[world]
Include = /etc/pacman.d/mirrorlist
[galaxy]
Include = /etc/pacman.d/mirrorlist
{% endhighlight %}

**Case**: Sometimes there is additional repository as well,
inside <code class="code-file">/etc/apt/pacman.conf/</code> directory.

{% highlight bash %}
[archlinuxfr]
SigLevel = PackageOptional
Server = http://repo.archlinux.fr/$arch

[blackarch]
Server = http://blackarch.mirror.digitalpacific.com.au/blackarch/os/$arch
SigLevel = Optional
{% endhighlight %}

#### Mirror

Before you begin.
This is the official documentation.

*	<https://wiki.archlinux.org/index.php/mirrors>

Mirror configuration is in <code class="code-file">/etc/pacman.d/mirrorlist</code>.
It is originnaly just consist one line.

{% highlight bash %}
$ cat /etc/pacman.d/mirrorlist
Server = https://mirrors.kernel.org/archlinux/$repo/os/$arch
{% endhighlight %}

![Docker Arch: cat mirror][image-ss-mirror-cat]{: .img-responsive }

We can fill more repository from this mirror generator site.

* 	https://www.archlinux.org/mirrorlist/

I filter the option for Indonesia only.
Just append by copy paste the result under the original 
<code class="code-file">/etc/pacman.d/mirrorlist</code>.
Do not forget to uncomment.

I also append my other three favorite repositories as well.

{% highlight bash %}
Server = https://mirrors.kernel.org/archlinux/$repo/os/$arch

Server = http://kambing.ui.ac.id/archlinux/$repo/os/$arch
Server = ftp://archlinux.cbn.net.id/pub/archlinux/$repo/os/$arch
Server = http://archlinux.cbn.net.id/$repo/os/$arch

##
## Arch Linux repository mirrorlist
## Generated on 2017-09-13
##

## Indonesia
Server = http://mirror.devilzc0de.org/archlinux/$repo/os/$arch
Server = http://mirror.poliwangi.ac.id/archlinux/$repo/os/$arch
Server = http://suro.ubaya.ac.id/archlinux/$repo/os/$arch
{% endhighlight %}

![Docker Arch: nano mirror][image-ss-mirror-nano]{: .img-responsive }

We are not done yet.
Now we need to rank the mirror to find the fastest mirror.
First backup the mirrorlist.

{% highlight bash %}
$ cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup
{% endhighlight %}

{% highlight bash %}
$ rankmirrors /etc/pacman.d/mirrorlist.backup > /etc/pacman.d/mirrorlist
{% endhighlight %}

{% highlight bash %}
$ /etc/pacman.d/mirrorlist
Server = http://suro.ubaya.ac.id/archlinux/$repo/os/$arch
Server = http://mirror.poliwangi.ac.id/archlinux/$repo/os/$arch
Server = http://kambing.ui.ac.id/archlinux/$repo/os/$arch
Server = https://mirrors.kernel.org/archlinux/$repo/os/$arch
Server = ftp://archlinux.cbn.net.id/pub/archlinux/$repo/os/$arch
Server = http://archlinux.cbn.net.id/$repo/os/$arch
Server = http://mirror.devilzc0de.org/archlinux/$repo/os/$arch
{% endhighlight %}

![Docker Arch: rankmirrors][image-ss-rankmirrors]{: .img-responsive }

Do not forget to refresh.

{% highlight bash %}
$ pacman -Syy 
{% endhighlight %}

-- -- --

### Group

ALPM has a very nice group support.

#### Blackarch Example

I have already wrote an article about this.

*	[Selectively Install Blackarch Tool][local-selectively]

With BlackArch, you can install tools by category.
One category today. And other category the day after.
Until all completely installed. 
Or you can install only what you need. 
And keep our system clean.

But we are not talking about BlackArch here.
Therefore I must make a more genreci example.

#### List Group

To show all group, just use <code>--sync --group</code>

{% highlight bash %}
$ pacman -Sg
{% endhighlight %}

Equal to:

{% highlight bash %}
$ pacman --sync --group
{% endhighlight %}

![Docker pacman: List Group][image-ss-group-list]{: .img-responsive }

Cool! Now we can install a specific group.
I choose <code>base-devel</code>, as we need toolchain later for automatic compilation.

{% highlight bash %}
$ pacman --sync base-devel
:: There are 25 members in group base-devel:
:: Repository core
   1) autoconf  2) automake  3) binutils  4) bison  5) fakeroot
   6) file  7) findutils  8) flex  9) gawk  10) gcc  11) gettext
   12) grep  13) groff  14) gzip  15) libtool  16) m4  17) make
   18) pacman  19) patch  20) pkg-config  21) sed  22) sudo
   23) texinfo  24) util-linux  25) which

Enter a selection (default=all): 
{% endhighlight %}

![Docker pacman: Install Group][image-ss-group-install]{: .img-responsive }

-- -- --

### Lock Package

Two method here

*	<code>IgnorePkg</code> to avoid change, upgrade or downgrade.

*	<code>HoldPkg</code> to avoid removal.

#### IgnorePkg

Consider an water creature case here that I installed from outdated repo.
It will always be on the upgradable list.

{% highlight bash %}
$ pacman -Qu
fish 2.4.0-1 -> 2.6.0-1
{% endhighlight %}

![Docker pacman: Outdated Package][image-ss-outdated]{: .img-responsive }

Unless we explicitly **ignore** it, by using <code>IgnorePkg</code>
in <code>/etc/pacman.conf</code>.

{% highlight bash %}
$ cat /etc/pacman.conf | grep fish
IgnorePkg    = fish

$ pacman -Qu
fish 2.4.0-1 -> 2.6.0-1 [ignored]

$ pacman -Su
:: Starting full system upgrade...
warning: fish: ignoring package upgrade (2.4.0-1 => 2.6.0-1)
 there is nothing to do
{% endhighlight %}

![Docker pacman.conf: IgnoredPkg][image-ss-ignored]{: .img-responsive }

I have been using this <code>IgnorePkg</code>
to avoid driver upgrade for my <code>SiS671</code> driver,
because there was about one year that,
the later driver does not work with <code>xorg</code>.

### HoldPkg

What if I want this water creature to be always in my system ?
We explicitly **hold** it, by using <code>HoldPkg</code>
in <code>/etc/pacman.conf</code>.

{% highlight bash %}
$ cat /etc/pacman.conf | grep fish
HoldPkg      = fish

$ pacman -R fish
checking dependencies...
warning: fish is designated as a HoldPkg.
:: HoldPkg was found in target list. Do you want to continue? [y/N]
{% endhighlight %}

![Docker pacman.conf: HoldPkg][image-ss-hold]{: .img-responsive }

-- -- --

### History

This is most the forgotten part of package management,
although it is not uncommon to notice messages.

#### The Log File

There are few log files.

*	/var/log/pacman.log

<code>less</code> or <code>more</code> is a good tool to read log file.
Most likely you want the <code>tail</code>, latest transaction,
at the bottom of the recorded event.

{% highlight bash %}
$ grc tail /var/log/pacman.log
[2017-09-13 21:48] [ALPM] installed gc (7.6.0-1)
[2017-09-13 21:48] [ALPM] installed guile (2.2.2-1)
[2017-09-13 21:48] [ALPM] installed make (4.2.1-2)
[2017-09-13 21:48] [ALPM] reinstalled pacman (5.0.2-2)
[2017-09-13 21:48] [ALPM] installed patch (2.7.5-1)
[2017-09-13 21:48] [ALPM] installed pkg-config (0.29.2-1)
[2017-09-13 21:48] [ALPM] reinstalled sudo (1.8.21.p2-1)
[2017-09-13 21:48] [ALPM] reinstalled which (2.21-2)
[2017-09-13 21:48] [ALPM] transaction completed
[2017-09-13 21:48] [ALPM] running 'texinfo-install.hook'...
{% endhighlight %}

![Docker Arch: grc tail /var/log/pacman.log][image-ss-pacman-log]{: .img-responsive }

-- -- --

### Clean Up

APT as default keep downloaded package.

Package Cache
	
*	/var/cache/pacman/pkg/ * -x86_64.pkg.tar.xz

*	/var/cache/abs/local/yaourtbuild/ * -x86_64.pkg.tar.xz

You can clean this directory by using

{% highlight bash %}
$ pacman -Sc
{% endhighlight %}

Equal to:
{% highlight bash %}
$ pacman --sync --clean
Packages to keep:
  All locally installed packages

Cache directory: /var/cache/pacman/pkg/
:: Do you want to remove all other packages from cache? [Y/n] y
removing old packages from cache...

Database directory: /var/lib/pacman/
:: Do you want to remove unused repositories? [Y/n] y
removing unused sync repositories...
{% endhighlight %}

![Docker Arch: Cache Clean][image-ss-cache-clean]{: .img-responsive }

Note that all package that you use will still in that directory.
You can uninstall manually if you want.
Please preserve system packages such as driver and kernel,
just in case upgrade issue happened.

I personally like to collect important package to other directory,
and remove all package manually.

-- -- --

### What's Next

There are still, some interesting topic for <code>APT</code>.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}

[local-part-three]:		{{ site.url }}/system/2017/08/29/docker-arch-alpm.html
[local-selectively]:	{{ site.url }}/system/2014/12/27/selectively-install-blackarch-tools.html

[image-ss-info-groff]:		{{ asset_post }}/14-sii-groff.png
[image-ss-info-man-db]:		{{ asset_post }}/14-qii-man-db.png
[image-ss-pactree-depth]:	{{ asset_post }}/14-pactree.png
[image-ss-pactree-reverse]:	{{ asset_post }}/14-pactree-reverse.png

[image-ss-mirror-cat]:		{{ asset_post }}/16-cat-mirrorlist.png
[image-ss-mirror-nano]:		{{ asset_post }}/16-nano-mirrorlist.png
[image-ss-pacman-conf]:		{{ asset_post }}/16-pacman-conf.png
[image-ss-rankmirrors]:		{{ asset_post }}/16-rankmirrors.png

[image-ss-group-install]:	{{ asset_post }}/15-install-group.png
[image-ss-group-list]:		{{ asset_post }}/15-list-group.png

[image-ss-pacman-log]:		{{ asset_post }}/19-grc-tail-log-pacman.png

[image-ss-cache-clean]:		{{ asset_post }}/17-clean.png

[image-ss-hold]:		{{ asset_post }}/28-hold-unremoved.png
[image-ss-ignored]:		{{ asset_post }}/28-upgradable-ignored.png
[image-ss-outdated]:	{{ asset_post }}/28-upgradable-outdated.png
