---
layout: post
title:  "Control Your Package with APT Pinning"
date:   2015-03-17 12:49:15 +0700
categories: system
tags: [debian, apt, package manager]
author: epsi

excerpt:
  APT Pinning is not a very interesting topic. But it saved my notebook's life.
  Now my notebook can live longer. By keeping the old driver, and never update it ever.

related_link_ids: 
  - 16070105  # Debian APT Compile Source
  - 16020803  # Update Arch no Bloated

---


Trouble is a Friend, said Lenka.

Yups... Last night my Debian failed to load my notebook screen. 

Silly me, I didn't have enough time to troubleshoot this /var/log/Xorg.0.log. But I can see I've been using Debian Testing since last week. So all I need is to downgrade my xorg to Debian Stable aka Debian 8.0 Jessie.

So here we are, apt-pinning xorg, purge xorg, and re-install xorg.

[![APT Pinning][image-ss]{: .img-responsive }][picasa-ss]

**Reading**:<br/>
* <https://debian-handbook.info/browse/stable/sect.apt-get.html#sect.apt.priorities>

All is good now

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss]: {{ site.url }}/assets/posts/system/2015/03/apt-pinning.png
[picasa-ss]: https://lh3.googleusercontent.com/-LKk0TT15FqM/VznBcvueHjI/AAAAAAAAANg/qET1zVBOm5I5Fn2kHo3EgkwSIRjH-AN-wCCo/s0/apt-pinning.png

