---
layout: post
title:  "Learn Different Package Management Using LXC"
date:   2016-02-28 23:00:15 +0700
categories: system
tags: [cloud, package manager]
author: epsi

excerpt:
  There are interesting differences between major distribution.
  One interest most is how they manage their packages.
  This is a cheap way to learn different package management 
  from major linux distribution using Linux Container.

related_link_ids: 
  - 16030301  # Docker Manjaro
  - 16030900  # Docker Debian  

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

{% capture ss_content %}
<strong>OS</strong>: Manjaro<br/>
  + <strong>WM</strong>: Awesome (floating mode).<br/>
  + Theme: Kali :-)<br/>
  + Wallpaper<br/>
<br/>
<strong>Screenshot 1</strong>: Terminal<br/>
  1: (host) Manjaro + Pacman<br/>
  2: (LXC) Gentoo + Emerge<br/>
  3: (LXC) CentOS + YUM<br/>
  4: (LXC) Debian + APT
{% endcapture %}

{% include part/screenshot.html 
   title = 'Screenshot 1: Terminal' 
   ss_content = ss_content
%}

[![LXC Screenfetch: manjaro, gentoo, centos, debian][image-ss-lxc-package]{: .img-responsive }][photo-ss-lxc-package]
<br/>

{% capture ss_content %}
<strong>OS</strong>: Manjaro<br/>
  + <strong>WM</strong>: Awesome (floating mode).<br/>
{% endcapture %}

{% include part/screenshot.html 
   title = 'Screenshot 2: Screenfetch' 
   ss_content = ss_content
%}

[![LXC Package: pacman, emerge, yum, apt][image-ss-lxc-screenfetch]{: .img-responsive }][photo-ss-lxc-screenfetch]
<br/>

-- -- --

Between VM, LXC, Docker, Multiboot.
What do you think of LXC as cheap way to learn about other distribution ?

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-lxc-package]: {{ site.url }}/assets/posts/system/2016/02/lxc-screenfetch.png
[photo-ss-lxc-package]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOIV-PL-MJDcC2TxUi8xKUBFNz419Cnrxi9FBWy
[image-ss-lxc-screenfetch]: {{ site.url }}/assets/posts/system/2016/02/lxc-package.png
[photo-ss-lxc-screenfetch]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOytPeGreF98b0YutShuQ7YVKkxeSdUkAcVJUkS



