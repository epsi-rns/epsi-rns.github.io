---
layout: post
title:  "Debian APT Pinning"
date:   2015-03-17 12:49:15 +0700
# categories: opensource
tags: [debian, apt, package management]
---


Trouble is a Friend, said Lenka.

Yups... Last night my Debian failed to load my notebook screen. 

Silly me, I didn't have enough time to troubleshoot this /var/log/Xorg.0.log. But I can see I've been using Debian Testing since last week. So all I need is to downgrade my xorg to Debian Stable aka Debian 8.0 Jessie.

So here we are, apt-pinning xorg, purge xorg, and re-install xorg.

![APT Pinning]({{ site.url }}/assets/opensource/2015/03/apt-pinning.png)

**Reading**:<br/>
* <https://debian-handbook.info/browse/stable/sect.apt-get.html#sect.apt.priorities>


All is good now


