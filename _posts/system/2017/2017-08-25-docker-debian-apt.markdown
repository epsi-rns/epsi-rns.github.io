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

### Package More

More about Package, than just IRSIF.

#### Change Log

{% highlight bash %}
$ apt-get changelog ncdu
{% endhighlight %}

Or:

{% highlight bash %}
$ apt changelog ncdu
{% endhighlight %}

![Docker APT: Changelog][image-ss-apt-changelog]{: .img-responsive }

#### Package Policy

We can examine the policy too.

{% highlight bash %}
$ apt-cache policy ncdu
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt policy ncdu
ncdu:
  Installed: (none)
  Candidate: 1.12-1+b1
  Version table:
     1.12-1+b1 500
        500 http://deb.debian.org/debian stretch/main amd64 Packages
{% endhighlight %}

There is <code>no aptitude policy</code> this time.
" _This aptitude does not have Super Cow Powers._ "
However there is <code>aptitude versions</code>.

{% highlight bash %}
$ aptitude versions ncdu  
p   1.12-1+b1                               stable              500 
{% endhighlight %}

![Docker APT: Policy][image-ss-apt-policy]{: .img-responsive }

We will discuss policy later.

#### Aptitude

<code>aptitude</code> provide text based user interface.

![Docker Aptitude: Dialog][image-ss-aptitude-dialog]{: .img-responsive }

#### Dselect

There are alternative text based user interface as well called <code>dselect</code>.

![Docker Dselect: Dialog][image-ss-dselect-dialog]{: .img-responsive }

#### Interrupted Process

Sometimes DPKG interrupted, for some reason such as,
I have to immediately turn off my notebook or stuff.
You can continue with this command.

{% highlight bash %}
$ dpkg --configure -a
{% endhighlight %}

#### Search Files

This looks like list files command, but very different task.
This command looking for any package that match the corresponding search.

{% highlight bash %}
$ dpkg -S ncdu
{% endhighlight %}

Or

{% highlight bash %}
$ dpkg-query --search ncdu
ncdu: /usr/share/doc/ncdu
ncdu: /usr/share/doc/ncdu/changelog.Debian.amd64.gz
fish-common: /usr/share/fish/completions/ncdu.fish
ncdu: /usr/share/doc/ncdu/changelog.gz
ncdu: /usr/share/doc/ncdu/changelog.Debian.gz
ncdu: /usr/share/man/man1/ncdu.1.gz
ncdu: /usr/share/doc/ncdu/copyright
ncdu: /usr/bin/ncdu
{% endhighlight %}

![Docker DPKG: Search][image-ss-dpkg-search]{: .img-responsive }

As you can see, it found in both _ncdu_ and _fish_ package.

#### dlocate

Alternatively

{% highlight bash %}
$ dlocate ncdu
ncdu: /.
ncdu: /usr
ncdu: /usr/bin
ncdu: /usr/bin/ncdu
ncdu: /usr/share
ncdu: /usr/share/doc
ncdu: /usr/share/doc/ncdu
ncdu: /usr/share/doc/ncdu/changelog.Debian.amd64.gz
ncdu: /usr/share/doc/ncdu/changelog.Debian.gz
ncdu: /usr/share/doc/ncdu/changelog.gz
ncdu: /usr/share/doc/ncdu/copyright
ncdu: /usr/share/man
ncdu: /usr/share/man/man1
ncdu: /usr/share/man/man1/ncdu.1.gz
fish-common: /usr/share/fish/completions/ncdu.fish
{% endhighlight %}

![Docker Debian: dlocate][image-ss-dlocate]{: .img-responsive }

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

### System Wide

System Wide Information

#### Cache Statistics

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

#### List Packages

There is this <code>dumpavail</code>
and <code>dump</code> command
with long output to list packages

{% highlight bash %}
$ apt-cache dumpavail | less
{% endhighlight %}

{% highlight bash %}
$ apt-cache dump | less
Package: 0ad
Version: 0.0.21-2
Installed-Size: 18473
Maintainer: Debian Games Team <pkg-games-devel@lists.alioth.debian.org>
Architecture: amd64
...
{% endhighlight %}

![Docker APT: cache dump][image-ss-cache-dump]{: .img-responsive }

Or just package names if you wish.
Listing packages handled by package manager,
can be achieved by <code>apt-cache pkgnames</code>.

{% highlight bash %}
$ apt-cache pkgnames | less
{% endhighlight %}

#### APT Config

{% highlight bash %}
$ apt-config dump
APT "";
APT::Architecture "amd64";
APT::Build-Essential "";
APT::Build-Essential:: "build-essential";
APT::Install-Recommends "1";
APT::Install-Suggests "0";
APT::Sandbox "";
APT::Sandbox::User "_apt";
APT::NeverAutoRemove "";
APT::NeverAutoRemove:: "^firmware-linux.*";
APT::NeverAutoRemove:: "^linux-firmware$";
...
{% endhighlight %}

![Docker APT: config dump][image-ss-config-dump]{: .img-responsive }

#### Verify

Verify integrity of package database, such as dependencies.

{% highlight bash %}
$ apt-get check
Reading package lists... Done
Building dependency tree       
Reading state information... Done
{% endhighlight %}

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

Keep your system neat and tidy.

#### Clean

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

#### Unused Dependency Removal

Many times we remove package,
and the dependency packages left in the system.
We can clean up by utilize <code>apt-get autoremove</code>.

{% highlight bash %}
$ apt-get autoremove
{% endhighlight %}

Almost equal to:

{% highlight bash %}
$ apt autoremove
Reading package lists... Done
Building dependency tree       
Reading state information... Done
The following packages will be REMOVED:
  bc bzip2 file fish-common javascript-common libexpat1 libffi6
  libgmp10 libgnutls30 libhogweed4 libidn11 libjs-jquery
  libmagic-mgc libmagic1 libnettle6 libp11-kit0 libpcre2-32-0
  libpython-stdlib libpython2.7-minimal libpython2.7-stdlib
  libreadline7 libssl1.1 libtasn1-6 libx11-6 libx11-data libxau6
  libxcb1 libxdmcp6 lynx lynx-common mime-support python
  python-minimal python2.7 python2.7-minimal xsel xz-utils
0 upgraded, 0 newly installed, 37 to remove and 0 not upgraded.
After this operation, 47.0 MB disk space will be freed.
Do you want to continue? [Y/n] 
{% endhighlight %}

![Docker APT: Autoremove Unused Dependency][image-ss-apt-autoremove]{: .img-responsive }

There is <code>no aptitude autoremove</code> this time.
" _This aptitude does not have Super Cow Powers._ "

#### Orphan

Quote from the manual,
<code>deborphan</code> _finds packages that have no packages depending on them_.
This is different with <code>autoremove</code>.

{% highlight bash %}
$ deborphan
dh-systemd
{% endhighlight %}

![Docker Debian: deborphan][image-ss-deborphan]{: .img-responsive }

-- -- --

### What's Next

There are still, repository topic and building from source.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}

[local-part-three]:   {{ site.url }}/system/2017/08/26/docker-debian-apt.html

[image-ss-apt-changelog]:		{{ asset_post }}/13-changelog.png

[image-ss-apt-policy]:		{{ asset_post }}/13-apt-policy.png

[image-ss-aptitude-dialog]:		{{ asset_post }}/13-aptitude-dialog.png
[image-ss-dselect-dialog]:		{{ asset_post }}/13-dselect-dialog.png

[image-ss-dlocate]:			{{ asset_post }}/13-dlocate.png
[image-ss-dpkg-search]:		{{ asset_post }}/13-dpkg-query-search.png

[image-ss-tasksel-list]:	{{ asset_post }}/15-group-task.png
[image-ss-apt-clean]:		{{ asset_post }}/17-clean.png
[image-ss-apt-autoclean]:	{{ asset_post }}/17-autoclean.png
[image-ss-docker-clean]:	{{ asset_post }}/17-docker-clean.png
[image-ss-tail-log-apt]:	{{ asset_post }}/19-tail-log-apt.png
[image-ss-tail-log-dpkg]:	{{ asset_post }}/19-tail-log-dpkg.png

[image-ss-cache-stats]:		{{ asset_post }}/19-stats.png
[image-ss-config-dump]:		{{ asset_post }}/19-config-dump.png
[image-ss-cache-dump]:		{{ asset_post }}/19-cache-dump.png
[image-ss-deborphan]:		{{ asset_post }}/19-deborphan.png
[image-ss-apt-autoremove]:	{{ asset_post }}/13-apt-autoremove.png

[image-ss-h-upgradable]:	{{ asset_post }}/27-list-upgradable.png
[image-ss-h-sources-list]:	{{ asset_post }}/27-sources-list-testing.png
[image-ss-h-mark-hold]:		{{ asset_post }}/27-mark-hold.png
[image-ss-h-upgrade-kept]:	{{ asset_post }}/27-upgrade-kept-back.png
[image-ss-h-preferences-d]:	{{ asset_post }}/27-preferences-d.png
[image-ss-h-unupgradable]:	{{ asset_post }}/27-upgradable-pinned.png
