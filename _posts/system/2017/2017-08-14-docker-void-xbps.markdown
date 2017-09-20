---
layout: post
title: "Docker - Void XBPS - Part Two"
date: 2017-08-14 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, void]
author: epsi

excerpt:
  Docker flow for Void Linux XBPS,
  first time using Void experience.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
# - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-void-xbps.html %}

-- -- --

### Repository

I found very few reference about repository in <code>xbps</code>.

#### Configuration

Configuration can be found in <code>/usr/share/xbps.d</code>

{% highlight bash %}
$ cat /usr/share/xbps.d/00-repository-main.conf 
repository=https://repo.voidlinux.eu/current
{% endhighlight %}

Consider check the sign.

{% highlight bash %}
$ xbps-query -vL
 8242 https://repo.voidlinux.eu/current (RSA signed)
      Signed-by: Void Linux
      4096 60:ae:0c:d6:f0:95:17:80:bc:93:46:7a:89:af:a3:2d
{% endhighlight %}

![Docker XBPS: Repository: Install][image-ss-r-configuration]{: .img-responsive }

#### Add Subrepository

Sub-repositories can be found as packages.

{% highlight bash %}
$ xbps-query -Rs void-repo
[-] void-repo-debug-9_1            Void Linux drop-in file for t...
[-] void-repo-multilib-6_1         Void Linux drop-in file for t...
[-] void-repo-multilib-nonfree-6_1 Void Linux drop-in file for t...
[-] void-repo-nonfree-9_1          Void Linux drop-in file for t...
{% endhighlight %}

![Docker XBPS: Repository: Query][image-ss-r-query-repository]{: .img-responsive }

Therefore a sub-repository can be installed as package.

{% highlight bash %}
xbps-install void-repo-nonfree
1 package will be downloaded:
  void-repo-nonfree-9_1 
1 package will be installed:
  void-repo-nonfree-9_1 

Size to download:              1284B
Size required on disk:         1337B
Free space on disk:             22GB

Do you want to continue? [Y/n] 
{% endhighlight %}

![Docker XBPS: Repository: Configuration][image-ss-r-repository-install]{: .img-responsive }

Do not forget to obey the official manual to synchronize

{% highlight bash %}
$ xbps-install -S
[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...
x86_64-repodata: 1315KB [avg rate: 1228KB/s]
[*] Updating 'https://repo.voidlinux.eu/current/nonfree/x86_64-repodata' ...
x86_64-repodata: 13KB [avg rate: 180MB/s]
{% endhighlight %}

![Docker XBPS: Repository: Synchronize][image-ss-r-repository-sync]{: .img-responsive }

#### List Packages

Now we can list avaliable packages from that repository.

{% highlight bash %}
$ cat /usr/share/xbps.d/10-repository-nonfree.conf 
repository=https://repo.voidlinux.eu/current/nonfree
{% endhighlight %}

{% highlight bash %}
$ xbps-query --repository=https://repo.voidlinux.eu/current/nonfree -Mis \*
[-] CopyAgent-1.48.0451_1                    Copy.com sync agent
[-] android-studio-2.3.3_1                   The official Androi...
[-] broadcom-wl-dkms-6.30.223.271_6          Broadcom proprietar...
[-] btsync-2.0.105_2                         Automatically sync ...
[-] caja-CopyAgent-1.48.0451_1               Copy.com sync agent...
[-] catalyst-15.302_2                        AMD catalyst driver...
[-] catalyst-dkms-15.302_2                   AMD catalyst driver...
[-] catalyst-libs-15.302_2                   AMD catalyst driver...
[-] catalyst-opencl-15.302_2                 AMD catalyst driver...
{% endhighlight %}

![Docker XBPS: Repository: Query Packages][image-ss-r-query-packages]{: .img-responsive }

#### Mirror

We can adjust the mirror by changing the configuration.

{% highlight bash %}
$ cat /usr/share/xbps.d/00-repository-main.conf 
repository=https://repo.voidlinux.eu/current
{% endhighlight %}

{% highlight bash %}
$ xbps-install -S
[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...
{% endhighlight %}

{% highlight bash %}
$ echo "repository=https://repo2.voidlinux.eu/current" \
> /usr/share/xbps.d/00-repository-main.conf 
{% endhighlight %}

{% highlight bash %}
$ cat /usr/share/xbps.d/00-repository-main.conf 
repository=https://repo2.voidlinux.eu/current
{% endhighlight %}

{% highlight bash %}
$ xbps-install -S
[*] Updating 'https://repo2.voidlinux.eu/current/x86_64-repodata' ...
{% endhighlight %}

![Docker XBPS: Mirror][image-ss-r-xbps-mirror]{: .img-responsive }

-- -- --

### History

#### The Log File

	Unfortunately, nothing in /var/log

{% highlight bash %}
$ ls -l /var/log/
total 0
{% endhighlight %}

-- -- --

### Clean Up

Time after time, your cache size may growing bigger and bigger.

Package Cache
	
*	/var/cache/xbps/ * .xbps

{% highlight bash %}
$ ls -lR  /var/cache/xbps/
{% endhighlight %}

![Docker XBPS: Cache][image-ss-xbps-cache]{: .img-responsive }

You can clean these directory.
Unused package will be removed.
Some other stay.

{% highlight bash %}
$ xbps-remove -O
Removed file-5.30_1.x86_64.xbps from cachedir (obsolete)
Removed diffutils-3.5_2.x86_64.xbps from cachedir (obsolete)
Removed libtls15-2.5.4_1.x86_64.xbps from cachedir (obsolete)
Removed iproute2-4.10.0_1.x86_64.xbps from cachedir (obsolete)
{% endhighlight %}

![Docker XBPS: Clean][image-ss-xbps-clean]{: .img-responsive }

-- -- --

### Build from Source

This part is about <code>xbps-src</code>.

#### Reading

* 	<https://wiki.voidlinux.eu/Xbps-src>

*	<https://github.com/voidlinux/void-packages>

#### Requirement

First we need xtools.
Later we will need qemu and gcc, or maybe proot.

{% highlight bash %}
$ xbps-install xtools
2 packages will be downloaded:
  libpcre2-10.30_1 expat-2.2.4_1 
5 packages will be installed:
  libpcre2-10.30_1 expat-2.2.4_1 git-2.14.1_1 
  make-4.2.1_3 xtools-0.48_1 

Size to download:              418KB
Size required on disk:          33MB
Free space on disk:             22GB

Do you want to continue? [Y/n] 
{% endhighlight %}

![Docker XBPS-SRC: XTools][image-ss-b-install-xtools]{: .img-responsive }

#### Void Packages

Consider to go home.

{% highlight bash %}
$ cd ~
{% endhighlight %}

{% highlight bash %}
$ git clone https://github.com/voidlinux/void-packages
Cloning into 'void-packages'...
remote: Counting objects: 407954, done.
Receiving objects:  28% (117077/407954), 27.34 MiB | 1012.00 KiB/s  
Receiving objects:  29% (118307/407954), 27.90 MiB | 1014.00 KiB/s  
Receiving objects:  29% (118556/407954), 28.43 MiB | 1014.00 KiB/s  
Receiving objects:  99% (407294/407954), 144.01 MiB | 866.00 KiB/s  
remote: Total 407954 (delta 0), reused 0 (delta 0), pack-reused 407954
Receiving objects: 100% (407954/407954), 144.01 MiB | 866.00 KiB/s  
Receiving objects: 100% (407954/407954), 144.63 MiB | 1.30 MiB/s, done.
Resolving deltas: 100% (233370/233370), done.
Checking out files: 100% (12670/12670), done.
{% endhighlight %}

![Docker XBPS-SRC: Void Packages][image-ss-b-git-clone-void]{: .img-responsive }

Consider to change working directory.

{% highlight bash %}
$ cd ~/void-packages/
{% endhighlight %}

#### Getting Help

Read The Fine Manual. As usual.

{% highlight bash %}
$ ./xbps-src     
xbps-src: [options] <target> [arguments]

Targets: (only one may be specified)

binary-bootstrap [arch]
    Install bootstrap packages from host repositories into <masterdir>.
    If the optional 'arch' argument is set, it will install bootstrap packages
    from this architecture, and its required xbps utilities. The <masterdir>
    will be initialized for chroot operations.
{% endhighlight %}

![Docker XBPS-SRC: Getting Help][image-ss-b-getting-help]{: .img-responsive }

#### Bootstrap

{% highlight bash %}
$ ./xbps-src binary-bootstrap
=> Installing bootstrap from binary package repositories...
[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...
x86_64-repodata: 1315KB [avg rate: 2117KB/s]
[*] Updating 'https://repo.voidlinux.eu/current/musl/x86_64-repodata' ...
...
52 packages will be downloaded:
  glibc-devel-2.26_2 glibc-locales-2.26_2 binutils-2.29_1 
  ...
56 packages will be installed:
  xbps-triggers-0.102_3 base-files-0.139_8 
   ...

Size to download:               47MB
Size required on disk:         295MB
Free space on disk:             22GB

[*] Downloading binary packages
glibc-devel-2.26_2.x86_64.xbps: 2358KB [avg rate: 1350KB/s]
...
xbps-0.51_18: installed successfully.
xz-5.2.3_1: configuring ...
xz-5.2.3_1: installed successfully.
which-2.21_3: configuring ...
which-2.21_3: installed successfully.
base-chroot-0.65_2: configuring ...
base-chroot-0.65_2: installed successfully.

14 downloaded, 56 installed, 0 updated, 56 configured, 0 removed.
=> Installed bootstrap successfully!
{% endhighlight %}

![Docker XBPS-SRC: Binary Bootstrap][image-ss-b-binary-bootstrap]{: .img-responsive }

Now we should be ready to use <code>xbps-src</code>.

#### Under Docker

Unfortunately my host kernel does not support feature required by xbps-src.

-- -- --

### Hold

Hold in **xbps** is pretty straightforward.

#### Example

Consider a system upgrade,
we choose to hold <code>nano</code> and <code>mc</code> as our guinea pig example.

{% highlight bash %}
$ xbps-install -Su
[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...
x86_64-repodata: 1332KB [avg rate: 814KB/s]
18 packages will be downloaded:
  bc-1.07.1_2 btrfs-progs-4.13_1 ca-certificates-20170717_1 
  dash-0.5.9.1_2 dhcpcd-6.11.5_4 dnssec-anchors-20170822_1 
  e2fsprogs-1.43.6_1 e2fsprogs-libs-1.43.6_1 
  glibc-locales-2.26_2 libcap-ng-0.7.8_3 libharfbuzz-1.5.1_1 
  mc-4.8.19_2 nano-2.8.7_1 nghttp2-1.25.0_1 qemu-2.10.0_1 
  runit-2.1.2_8 runit-void-20170907_1 sudo-1.8.21p2_1 
18 packages will be updated:
  bc (1.07.1_1 -> 1.07.1_2) btrfs-progs (4.12_1 -> 4.13_1) 
  ca-certificates (20161130+nmu1_1 -> 20170717_1) 
...
{% endhighlight %}

#### Lock Package

{% highlight bash %}
$ xbps-pkgdb -m hold nano mc
{% endhighlight %}

![Docker XBPS: Hold][image-ss-h-xbps-hold]{: .img-responsive }

#### Result

We can see there is no more <code>mc</code> and <code>nano</code>.

{% highlight bash %}
$ xbps-install -Su
[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...
16 packages will be downloaded:
  bc-1.07.1_2 btrfs-progs-4.13_1 ca-certificates-20170717_1 
  dash-0.5.9.1_2 dhcpcd-6.11.5_4 dnssec-anchors-20170822_1 
  e2fsprogs-1.43.6_1 e2fsprogs-libs-1.43.6_1 
  glibc-locales-2.26_2 libcap-ng-0.7.8_3 libharfbuzz-1.5.1_1 
  nghttp2-1.25.0_1 qemu-2.10.0_1 runit-2.1.2_8 
  runit-void-20170907_1 sudo-1.8.21p2_1 
...
{% endhighlight %}

#### Unlock Package

{% highlight bash %}
$ xbps-pkgdb -m unhold nano mc
{% endhighlight %}

-- -- --

### Conclusion

There are still things that I do not understand,
such as using <code>xbps-src</code>.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-void' %}

[image-ss-xbps-cache]:      {{ asset_post }}/17-cache.png
[image-ss-xbps-clean]:      {{ asset_post }}/17-clean.png

[image-ss-r-query-packages]:     {{ asset_post }}/16-query-packages.png
[image-ss-r-query-repository]:   {{ asset_post }}/16-query-void-repo.png
[image-ss-r-repository-sync]:    {{ asset_post }}/16-repository-sync.png
[image-ss-r-configuration]:      {{ asset_post }}/16-usr-share-xbps-d.png
[image-ss-r-repository-install]: {{ asset_post }}/16-void-repo-nonfree-install.png

[image-ss-b-binary-bootstrap]:     {{ asset_post }}/25-binary-bootstrap-half.png
[image-ss-b-git-clone-void]:       {{ asset_post }}/25-git-clone-void-packages.png
[image-ss-b-getting-help]:         {{ asset_post }}/25-help.png
[image-ss-b-install-xtools]:       {{ asset_post }}/25-install-xtools-half.png

[image-ss-h-xbps-hold]:     {{ asset_post }}/27-xbps-pkgdb-hold.png

[image-ss-r-xbps-mirror]:   {{ asset_post }}/16-mirror.png
