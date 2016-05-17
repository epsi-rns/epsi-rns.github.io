---
layout: post
title:  "Learn Different Package Management Using LXC"
date:   2016-02-28 23:00:15 +0700
categories: opensource
tags: [cloud, lxc, package manager]
author: epsi
excerpt:
  There are interesting differences between major distribution.
  One interest most is how they manage their packages.
  This is a cheap way to learn different package management 
  from major linux distribution using Linux Container.
---

**Description**: A cheap way to learn different package management from major linux distribution using Linux Container.

* * *

Once upon a time, a rookie desire to install different linux distribution. People usually told that rookie to pick one of this two options:

1. Multi Boot. Install in different partition.

2. Virtual Machine. As a Guest OS.

The issue with Multiboot is you don't want to mess your system with too many partition, even if you can. I have 4 OS(s) in my notebook, and I don't want to risk data just to install different distribution. It is technically safe, but I avoid to.

Virtual Machine is slugish. Guest OS ate my RAM, ate my processor. For a low-cost-notebook, VM is not a comfortable environment to work with.

* * *

There are other options.

3. Docker

4. Linux Container (LXC)

Both are quite similar, although the approach is different. I found LXC is easier to setup and also lighter. CMIIW, I haven't tried docker yet. Both won't give you full blown comfortable Desktop Environment as Multiboot/VM does, but are enough for learning console based stuff, and even good enough for server.

I have seen a few linux distribution, all I can say is the applications are the same. But there are interesting difference between major distribution, it is how they manage their packages.

* * *

LXC have the ability to install different distribution in user space in your distribution. One host, many guest. From my Manjaro partition.

I have: Debian,/Ubuntu, Centos, and Gentoo. All running seamlessly. But I failed to install Fedora (mirror problem) and OpenSUSE (after manually compiling zypper). Plamo also installed succesfully, but I don't have any exerienced with slackware package management. LXC is great, but some templates need to be updated. Not perfect, but it helps a lot.

Link.

* <https://zyisrad.com/linux-apache-mysql-php-in-an-lxc-container/>

* <https://linuxcontainers.org/>

* <https://wiki.archlinux.org/index.php/Linux_Containers>

* <https://en.wikipedia.org/wiki/LXC>



* * *

Here is my screenshot.

**OS**: Manjaro<br/>
+ **WM**: Awesome (floating mode).<br/>
+ Theme: Kali :-)<br/>
+ Wallpaper<br/>
<br/>
**Screenshot 1**: Terminal<br/>
1: (host) Manjaro + Pacman<br/>
2: (LXC) Gentoo + Emerge<br/>
3: (LXC) CentOS + YUM<br/>
4: (LXC) Debian + APT<br/>


[![LXC Screenfetch][image-ss-lxc-screenfetch]][picasa-ss-lxc-screenfetch]
<br/>

**Screenshot 2**: Screenfetch
<br/>

[![LXC Package][image-ss-lxc-package]][picasa-ss-lxc-package]
<br/>

* * *

Between VM, LXC, Docker, Multiboot.
What do you think of LXC as cheap way to learn about other distribution ?

Thank you for reading.


[image-ss-lxc-screenfetch]: {{ site.url }}/assets/posts/opensource/2016/02/lxc-screenfetch.png
[picasa-ss-lxc-screenfetch]: https://lh3.googleusercontent.com/-3P3Mo-dD3BM/Vzmd1cgqRbI/AAAAAAAAAMk/Eve6CDNrio4Lu6IWV1VO-33XzJyl3LXLwCCo/s0/lxc-screenfetch.png
[image-ss-lxc-package]: {{ site.url }}/assets/posts/opensource/2016/02/lxc-package.png
[picasa-ss-lxc-package]: https://lh3.googleusercontent.com/-ymDyCDSEAtM/Vzmd1xbH2sI/AAAAAAAAAMk/48SelNtqDcIR0aFNvfSfluICHUG_lp7YQCCo/s0/lxc-package.png

