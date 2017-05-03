---
layout: post
title:  "Running Guest OS via Docker on top of Debian for the first time"
date:   2016-03-09 23:00:15 +0700
categories: system
tags: [cloud, docker, package manager]
author: epsi

excerpt:
  Yet another Docker showcase. Now using Debian.
  Docker is supposed to do one special thing.
  But this is not a limit. Just use your imagination.

related_link_ids: 
  - 16022800  # Learn LXC
  - 16030301  # Docker Manjaro
  
---

**Disclaimer**: I'm a beginner in cloud area.
<br/><br/>

**Description**: A cheap way to learn different package management from major linux distribution using Docker. No need Virtual Machine nor Multiboot. Easy to setup.
<br/><br/>

{% capture ss_content %}
<strong>Host OS</strong>: Debian<br/>
<strong>Container Service</strong>: Docker<br/>
  + Terminal: Debian screenfetch<br/>
  + Guest OS: OpenSUSE (screenfetch)<br/>
  + Guest OS: Gentoo (emerge)<br/>
  + Guest OS: Slackware (screenfetch)<br/>
  + Terminal: iftop<br/>
<br/>
<strong>DE</strong>: Plasma 5 (KDE)<br/>
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}


[![Debian Docker: openbox+terminator+(debian, opensuse, htop, emerge gentoo, slackware)][image-ss]{: .img-responsive }][photo-ss]
<br/><br/>

**Reference**:<br/>

* <https://docs.docker.com/engine/quickstart/>


[//]: <> ( -- -- -- links below -- -- -- )


[image-ss]: {{ site.url }}/assets/posts/system/2016/03/debian-docker-ogs.png
[photo-ss]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOLrIvqVOO-FD6p0b9ks12xToT65xqwZMpK2DjH



