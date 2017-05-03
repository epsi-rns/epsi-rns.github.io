---
layout: post
title:  "Docker Demonstration (Manjaro), Running Multiple Guest OS"
date:   2016-03-03 13:01:15 +0700
categories: system
tags: [cloud, docker, package manager]
author: epsi

excerpt: 
  A cheap way to learn different package management 
  from major linux distribution using Docker.
  No need Virtual Machine nor Multiboot. Easy to setup.

related_link_ids: 
  - 16022800  # Learn LXC
  - 16030900  # Docker Debian   

---

**Description**: A cheap way to learn different package management from major linux distribution using Docker. No need Virtual Machine nor Multiboot. Easy to setup.
<br/><br/>

{% capture ss_content %}
<strong>Host OS</strong>: Manjaro<br/>
<strong>Container Service</strong>: Docker<br/>
  + Guest OS: Gentoo<br/>
  + Guest OS: Slackware<br/>
  + Guest OS: Fedora<br/>
  + Guest OS: Kali<br/>
  + Guest OS: Ubuntu<br/>
<br/>
<strong>WM</strong>: XMonad<br/>
  + Bar: Conky + Dzen<br/>
  + Terminal: XFCE Terminal<br/>
  + Script: Screenfetch<br/>
  + Wallpaper: Self Vexel (inkscape) with 3D background<br/>
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

[![Manjaro Docker Light: xmonad screenfetch (fedora, kali, ubuntu)][image-ss-fki]{: .img-responsive }][photo-ss-fki]
<br/><br/>
[![Manjaro Docker Dark: xmonad screenfetch (gentoo, slackware)][image-ss-gs]{: .img-responsive }][photo-ss-gs]
<br/>

Note: just another nix's porn. :-\ <br/>

**Reference**:<br/>

* <https://docs.docker.com/engine/quickstart/>

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-fki]: {{ site.url }}/assets/posts/system/2016/03/manjaro-docker-fki.png
[photo-ss-fki]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNsM4yvZme4-07XgB4_JI1HMrdVtz5uYJeAZS4b
[image-ss-gs]: {{ site.url }}/assets/posts/system/2016/03/manjaro-docker-gs.png
[photo-ss-gs]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMFzGFcdbyslKMvTYPGXjQ700NOyFuPh-nnm81Y
