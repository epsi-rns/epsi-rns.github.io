---
layout: post
title: "Docker - Debian APT - Part Three"
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
# - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/toc-docker-debian-apt.html %}

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
Consider make a new <code>apt<code> file
in <code>/etc/apt/preferences.d<code>

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

[image-ss-h-upgradable]:	{{ asset_post }}/27-list-upgradable.png
[image-ss-h-sources-list]:	{{ asset_post }}/27-sources-list-testing.png
[image-ss-h-mark-hold]:		{{ asset_post }}/27-mark-hold.png
[image-ss-h-upgrade-kept]:	{{ asset_post }}/27-upgrade-kept-back.png
[image-ss-h-preferences-d]:	{{ asset_post }}/27-preferences-d.png
[image-ss-h-unupgradable]:	{{ asset_post }}/27-upgradable-pinned.png

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
