---
layout: post
title: "Docker - Debian APT - Part Three"
date: 2017-08-26 09:35:15 +0700
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
  - 17082715  # Arch ALPM
# - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
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

#### Dotty

there are also <code>graphviz</code> output named <code>dotty</code>

{% highlight bash %}
$ apt-cache -o APT::Cache::GivenOnly=1 dotty groff-base
digraph packages {
concentrate=true;
size="30,40";
"groff-base" -> "libc6";
"groff-base" -> "libgcc1";
"groff-base" -> "libstdc++6";
"groff-base" -> "groff"[color=springgreen];
"groff-base" -> "jgroff"[color=springgreen];
"groff-base" -> "pmake"[color=springgreen];
"groff-base" -> "troffcvt"[color=springgreen];
"pmake" [color=orange,shape=diamond];
"libc6" [color=orange,shape=box];
"libgcc1" [color=orange,shape=box];
"groff-base" [shape=box];
"groff" [color=orange,shape=box];
"troffcvt" [color=orange,shape=box];
"libstdc++6" [color=orange,shape=box];
"jgroff" [shape=triangle];
}
{% endhighlight %}

![Docker APT: Cache Dotty][image-ss-cache-dotty]{: .img-responsive }

{% highlight bash %}
$ dot -Tpng groff-base.dot > groff-base.png
{% endhighlight %}

![Docker APT: Cache Dotty: Graphvis][image-ss-graphvis]{: .img-responsive }

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

#### Distribution Upgrade

There are cases that a system need more than upgrade,
such as migrating python from python2 to python3,
or ncurse case, especially when many application involved.
Switching from one release to other release
require <code>dist-upgrade</code>.
I have never had issue with Debian,
but I experienced issues in Debian derivatives.

	No need to dist-upgrade in rolling release

Now consider pulling oldstable <code>jessie</code> container.

{% highlight bash %}
$ docker pull debian:jessie
jessie: Pulling from library/debian
aa18ad1a0d33: Pull complete 
Digest: sha256:f31c837c5dda4c4730a6f1189c9ae39031e966410d7b0885863bd6c43b5ff281
Status: Downloaded newer image for debian:jessie

$ docker run -it debian:jessie
root@0bf84cd70449:/#
{% endhighlight %}

![Docker Debian: Pull Jessie][image-ss-dup-pull-jessie]{: .img-responsive }

And do some basic task so that we can edit <code>sources.list</code>.

{% highlight bash %}
$ apt update
$ apt upgrade
$ apt install man-db nano less vim wget curl sudo
{% endhighlight %}

From Jessie

{% highlight bash %}
$ nano /etc/apt/sources.list
deb http://deb.debian.org/debian jessie main
deb http://deb.debian.org/debian jessie-updates main
deb http://security.debian.org jessie/updates main
{% endhighlight %}

![Docker sources.list: jessie][image-ss-dup-nano-jessie]{: .img-responsive }

To Stretch

{% highlight bash %}
$ cat /etc/apt/sources.list
deb http://deb.debian.org/debian stretch main
{% endhighlight %}

![Docker sources.list: stretch][image-ss-dup-nano-stretch]{: .img-responsive }

Do not forget to update.

{% highlight bash %}
$ apt update
...
132 packages can be upgraded. Run 'apt list --upgradable' to see them.
{% endhighlight %}

And <code>dist-upgrade</code>.

{% highlight bash %}
$ apt dist-upgrade
{% endhighlight %}

![Docker APT: dist-upgrade][image-ss-dup-dist-upgrade]{: .img-responsive }

This example above using minimal install,
in this case <code>upgrade</code> is sufficient
and the same result with <code>dist-upgrade</code>.
Nothing to worry about, the command finished successfully.

-- -- --

### Repository Pinning

Sometimes we need a package from unstable repository.
There are few reason, such as trying latest version,
or such version has not available yet in _testing_ or _stable_.
I did when I curious about tomahawk while it was just landed in_unstable_.
Do not be afraid to do this as long you do not mess with system packages.

#### nmap Case

Consider have a look at this <code>nmap</code> case.
<code>nmap</code> has different version for stable and unstable.

*	https://packages.debian.org/search?keywords=nmap

*	stretch (stable): 7.40-1

*	sid (unstable): 7.60-1

nmap from sid (unstable) can be installed in stretch (stable).
Steps below.

#### New Docker for Stretch

Ww need a new docker,
because we use _stable_ release for <code>nmap</code>.
Now update and upgrade silently using <code>-qq</code> option.

{% highlight bash %}
$ docker run -it debian:stretch

root:/# apt update -y -qq
1 package can be upgraded. Run 'apt list --upgradable' to see it.

root:/# apt upgrade -y -qq
The following packages will be upgraded:
  libgcrypt20
1 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.
Need to get 523 kB of archives.
...
{% endhighlight %}

![Docker APT Pinning: New Stable Container][image-ss-rp-new-stable]{: .img-responsive }

#### Stable nmap 

{% highlight bash %}
$ apt-get install -y -qq wget curl man-db nano less nmap
{% endhighlight %}

{% highlight bash %}
$ nmap -V
Nmap version 7.40 ( https://nmap.org )
...
{% endhighlight %}

![Docker APT Pinning: nmap Stable][image-ss-rp-nmap-stable]{: .img-responsive }

#### Pinning Unstable

We need to switch the repository,
to contain both _stable_ and _unstable_.

{% highlight bash %}
$ nano /etc/apt/sources.list
{% endhighlight %}

{% highlight bash %}
$ cat /etc/apt/sources.list
deb http://deb.debian.org/debian stable main
deb http://deb.debian.org/debian unstable main
{% endhighlight %}

![Docker APT Pinning: Nano sources.list][image-ss-rp-nano-sources]{: .img-responsive }

And give negative number in pinning preferences,
so that this _unstable_ repository ignored,
except for direct install with _target_.

{% highlight bash %}
$ touch /etc/apt/preferences.d/unstable
$ nano /etc/apt/preferences.d/unstable
{% endhighlight %}

{% highlight bash %}
$ cat /etc/apt/preferences.d/example 
Package: *
Pin: release a=unstable
Pin-Priority: -10
{% endhighlight %}

![Docker APT Pinning: nano preferences.d ][image-ss-rp-nano-pinning]{: .img-responsive }

{% highlight bash %}
$ apt update
...
All packages are up to date.
{% endhighlight %}

{% highlight bash %}
$ apt upgrade
...
0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.
{% endhighlight %}

Make sure that system cannot be upgraded to _unstable_.

{% highlight bash %}
$ apt list --upgradable
Listing... Done
{% endhighlight %}

![Docker APT Pinning: List Upgradables][image-ss-rp-upgradables]{: .img-responsive }

#### Unstable nmap 

Have a look at thispolicy before we install <code>nmap</code>.

{% highlight bash %}
$ apt policy
Package files:
 100 /var/lib/dpkg/status
     release a=now
 -10 http://deb.debian.org/debian unstable/main amd64 Packages
     release o=Debian,a=unstable,n=sid,l=Debian,c=main,b=amd64
     origin deb.debian.org
 500 http://deb.debian.org/debian stable/main amd64 Packages
     release v=9.1,o=Debian,a=stable,n=stretch,l=Debian,c=main,b=amd64
     origin deb.debian.org
Pinned packages:
{% endhighlight %}

![Docker APT Pinning: Policy Pinning][image-ss-rp-policy-pinning]{: .img-responsive }

Install <code>nmap</code> with explicit target <code>-t unstable</code>.

{% highlight bash %}
$ apt install --target unstable nmap
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following packages will be upgraded:
  nmap
1 upgraded, 0 newly installed, 0 to remove and 136 not upgraded.
Need to get 5401 kB of archives.
...
{% endhighlight %}

![Docker APT Pinning: Target Unstable][image-ss-rp-target-unstable]{: .img-responsive }

{% highlight bash %}
$ nmap -V
Nmap version 7.60 ( https://nmap.org )
{% endhighlight %}

![Docker APT Pinning: nmap Unstable][image-ss-rp-nmap-unstable]{: .img-responsive }

Consider examine,
have a look at the <code>nmap</code> version difference.
Now we have _unstable_ package in _stable_ system.

-- -- --

### What's Next

There are still, build form source topic.
Consider finish reading [ [Part Four][local-part-four] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}

[local-part-four]:   {{ site.url }}/system/2017/08/27/docker-debian-apt.html

[image-ss-apt-depends]:		{{ asset_post }}/14-apt-depends.png
[image-ss-apt-rdepends]:	{{ asset_post }}/14-apt-rdepends.png
[image-ss-aptitude-s-d]:	{{ asset_post }}/14-aptitude-d-groff-base.png
[image-ss-aptitude-s-r]:	{{ asset_post }}/14-aptitude-r-man-db.png
[image-ss-aptitude-why]:	{{ asset_post }}/14-aptitude-why-groff-base.png
[image-ss-cache-dotty]:		{{ asset_post }}/14-dotty.png
[image-ss-graphvis]:		{{ asset_post }}/14-groff-base-dotty.png

[image-ss-apt-policy]:		{{ asset_post }}/16-apt-policy.png
[image-ss-sources-list]:	{{ asset_post }}/16-etc-apt-sources-list.png
[image-ss-nano-kambing]:	{{ asset_post }}/16-nano-kambing.png
[image-ss-update-kambing]:	{{ asset_post }}/16-update-kambing.png
[image-ss-apt-test-remove]:	{{ asset_post }}/16-test-remove.png

[image-ss-dup-dist-upgrade]:	{{ asset_post }}/23-dist-upgrade.png
[image-ss-dup-pull-jessie]:		{{ asset_post }}/23-docker-jessie.png
[image-ss-dup-nano-jessie]:		{{ asset_post }}/23-sources-jessie.png
[image-ss-dup-nano-stretch]:	{{ asset_post }}/23-docker-stretch.png

[image-ss-rp-upgradables]:	{{ asset_post }}/28-list-upgradables.png
[image-ss-rp-nano-pinning]:	{{ asset_post }}/28-nano-pinning.png
[image-ss-rp-nano-sources]:	{{ asset_post }}/28-nano-sources.list.png
[image-ss-rp-new-stable]:	{{ asset_post }}/28-new-docker-stretch.png
[image-ss-rp-nmap-stable]:	{{ asset_post }}/28-nmap-stable.png
[image-ss-rp-nmap-unstable]:	{{ asset_post }}/28-nmap-unstable.png
[image-ss-rp-policy-pinning]:	{{ asset_post }}/28-policy-pinning.png
[image-ss-rp-target-unstable]:	{{ asset_post }}/28-target-unstable.png
