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

### Build from Source

APT has the capability to build the source code,
not just download the package.

I actually have cover this last year.
I just need to recompile the material,
to fit this package management (in docker) series articles.

*	[Compile .deb Source with apt-src][local-apt-source]

**Reading**

*	<https://wiki.debian.org/apt-src>

*	<https://wiki.debian.org/BuildingTutorial>

#### The Toolchain

Preparing the toolchain is as simply as install <code>apt-src</code>.
What you need to know is,
this toolchain is a lot of packages in number as this message said
"_ 10 upgraded, 80 newly installed_ ", " _Need to get 145 MB of archives_ ".

{% highlight bash %}
$ apt install apt-src
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following packages were automatically installed and are no longer required:
  ...
Use 'apt autoremove' to remove them.
The following additional packages will be installed:
  ...
Suggested packages:
  ...
The following NEW packages will be installed:
  apt-src binutils binutils-common binutils-x86-64-linux-gnu
  ...
The following packages will be upgraded:
  ...
10 upgraded, 80 newly installed, 0 to remove and 63 not upgraded.
Need to get 145 MB of archives.
After this operation, 635 MB of additional disk space will be used.
Do you want to continue? [Y/n]        
{% endhighlight %}

![Docker APT-SRC: Prepare Toolchain][image-ss-s-toolchain]{: .img-responsive }

#### Source Repository

Make sure that source repository
is enabled in your <code class="code-file">/etc/apt/sources.list</code>.
Add the <code>deb-src</code> and <code>apt-src update</code>.

Now <code class="code-file">sources.list</code>
seems like an appropriate name.

{% highlight bash %}
$ nano /etc/apt/sources.list
deb http://deb.debian.org/debian testing main
deb-src http://deb.debian.org/debian testing main
{% endhighlight %}

![Docker APT-SRC: Prepare Repository][image-ss-s-repository]{: .img-responsive }

#### User Privilege

Build require regular user, non root one. Consider make one first.

{% highlight bash %}
$ useradd -m -g users -G sudo -s /bin/bash epsi

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

If you wish you can also setup sudoers.

{% highlight bash %}
$ nano /etc/sudoers
epsi    ALL=(ALL:ALL) ALL
{% endhighlight %}

![Docker APT-SRC: adduser Epsi][image-ss-s-adduser-epsi]{: .img-responsive }

#### Build Directory

Make any directory as a working directory 
for your compilation process

{% highlight bash %}
$ mkdir ~/build
$ cd ~/build
{% endhighlight %}

![Docker APT-SRC: Prepare Directory][image-ss-s-directory]{: .img-responsive }

#### Example

Again, consider <code>apt</code> package as our guinea pig example.
Simply issue one of this command below to check package version.

{% highlight bash %}
$ dpkg --list apt 

$ apt show apt

$ apt search '^apt$'
apt search '^apt$'
Sorting... Done
Full Text Search... Done
apt/testing,now 1.5~rc1 amd64 [installed,automatic]
  commandline package manager
{% endhighlight %}

It is going to be a good idea to upgrade system
to latest version to avoid missing libraries.
Since we are going to modify system package, this is a **must** do part.
Or you may end up with something similar as message below

{% highlight bash %}
$ apt
apt: relocation error: /lib/x86_64-linux-gnu/libapt-private.so.0.0: 
symbol _ZTI17pkgAcquireStatus2, version APTPKG_5.0 not defined in 
file libapt-pkg.so.5.0 with link time reference
{% endhighlight %}

#### Download Source

Building a package could be as simple as this.

{% highlight bash %}
$ sudo apt-src --build install apt
{% endhighlight %}

This above is the most important part of the process.
You can also unbundling this process into two steps.

{% highlight bash %}
$ apt-src install apt
$ apt-src build apt
{% endhighlight %}

The first one, will also install the needed package,
such as <code>code</code> and al its dependencies.

{% highlight bash %}
$ apt-src install apt
Reading package lists... Done
NOTICE: 'apt' packaging is maintained in the 'Git' version control system at:
https://anonscm.debian.org/git/apt/apt.git
Please use:
git clone https://anonscm.debian.org/git/apt/apt.git
to retrieve the latest (possibly unreleased) updates to the package.
Need to get 2098 kB of source archives.
Get:1 http://deb.debian.org/debian testing/main apt 1.5~rc1 (dsc) [2590 B]
Get:2 http://deb.debian.org/debian testing/main apt 1.5~rc1 (tar) [2095 kB]
Fetched 2098 kB in 1s (1865 kB/s)
dpkg-source: info: extracting apt in apt-1.5~rc1
dpkg-source: info: unpacking apt_1.5~rc1.tar.xz
{% endhighlight %}

![Docker APT-SRC: install apt][image-ss-s-install-apt]{: .img-responsive }

Now we have this files our working directory.

{% highlight bash %}
$ ls
apt-1.5~rc1  apt_1.5~rc1.dsc  apt_1.5~rc1.tar.xz
{% endhighlight %}

#### Build Source

Consider Build the package. Let's do it.
This is the most interesting part.
This is going to takes time.

{% highlight bash %}
$ apt-src build apt
I: Building in ./apt-1.5~rc1 ..
dpkg-buildpackage: info: source package apt
dpkg-buildpackage: info: source version 1.5~rc1
dpkg-buildpackage: info: source distribution unstable
dpkg-buildpackage: info: source changed by Julian Andres Klode <jak@debian.org>
dpkg-buildpackage: info: host architecture amd64
 dpkg-source --before-build apt-1.5~rc1
dpkg-source: info: using options from apt-1.5~rc1/debian/source/options: --compression=xz
 fakeroot debian/rules clean
dh clean
   dh_auto_clean
   dh_autoreconf_clean
   debian/rules override_dh_clean
make[1]: Entering directory '/home/epsi/build/apt-1.5~rc1'
cp COPYING debian/copyright
{% endhighlight %}

![Docker APT-SRC: build apt][image-ss-s-build-top]{: .img-responsive }

![Docker APT-SRC: build apt][image-ss-s-build-center]{: .img-responsive }

![Docker APT-SRC: build apt][image-ss-s-build-bottom]{: .img-responsive }

#### Examine Directory

Check your directoy, 
your <code>apt_1.5~rc1_amd64.deb</code> package should already be there.

{% highlight bash %}
$ ls 
...
apt_1.5~rc1.dsc
apt_1.5~rc1.tar.xz
apt_1.5~rc1_amd64.buildinfo
apt_1.5~rc1_amd64.changes
apt_1.5~rc1_amd64.deb
...
{% endhighlight %}

![Docker APT-SRC: Directory Result][image-ss-s-directory-res]{: .img-responsive }

-- -- --

#### Install Result

Install package using <code class="code-command">dpkg</code> command

{% highlight bash %}
$ sudo dpkg --install apt_1.5~rc1_amd64.deb
(Reading database ... 29517 files and directories currently installed.)
Preparing to unpack apt_1.5~rc1_amd64.deb ...
Unpacking apt (1.5~rc1) over (1.5~rc1) ...
Setting up apt (1.5~rc1) ...
Processing triggers for libc-bin (2.24-17) ...
Processing triggers for man-db (2.7.6.1-2) ...
{% endhighlight %}

![Docker APT-SRC: DPKG Install][image-ss-s-dpkg-install]{: .img-responsive }

Does it work? 
You can check your <code>apt</code> version now

{% highlight bash %}
$ apt --version
apt 1.5~rc1 (amd64)
{% endhighlight %}

![Docker APT-SRC: Does it works?][image-ss-s-version-works]{: .img-responsive }

After a few days, you will feels like compiling is not difficult.

-- -- --

### Conclusion

	These are just preliminary knowledge about APT.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}


[local-apt-source]:	{{ site.url }}/system/2016/07/01/apt-compile-source.html

[image-ss-s-repository]:	{{ asset_post }}/25-sources-list.png
[image-ss-s-toolchain]:		{{ asset_post }}/25-toolchain.png
[image-ss-s-directory]:		{{ asset_post }}/25-working-dir.png
[image-ss-s-adduser-epsi]:	{{ asset_post }}/25-adduser-epsi.png
[image-ss-s-install-apt]:	{{ asset_post }}/25-src-install-apt.png
[image-ss-s-build-top]:		{{ asset_post }}/25-src-build-apt-top.png
[image-ss-s-build-center]:	{{ asset_post }}/25-src-build-apt-center.png
[image-ss-s-build-bottom]:	{{ asset_post }}/25-src-build-apt-bottom.png

[image-ss-s-directory-res]:	{{ asset_post }}/25-directory-result.png
[image-ss-s-dpkg-install]:	{{ asset_post }}/25-install-apt.png
[image-ss-s-version-works]:	{{ asset_post }}/25-version.png

[image-ss-rp-upgradables]:	{{ asset_post }}/28-list-upgradables.png
[image-ss-rp-nano-pinning]:	{{ asset_post }}/28-nano-pinning.png
[image-ss-rp-nano-sources]:	{{ asset_post }}/28-nano-sources.list.png
[image-ss-rp-new-stable]:	{{ asset_post }}/28-new-docker-stretch.png
[image-ss-rp-nmap-stable]:	{{ asset_post }}/28-nmap-stable.png
[image-ss-rp-nmap-unstable]:	{{ asset_post }}/28-nmap-unstable.png
[image-ss-rp-policy-pinning]:	{{ asset_post }}/28-policy-pinning.png
[image-ss-rp-target-unstable]:	{{ asset_post }}/28-target-unstable.png
