---
layout: post
title:  "What to Do, When the System Stuck, on Boot"
date      : 2016-05-19 15:39:15 +0700
categories: system
tags      : [package manager]
keywords  : [boot, grub2, troubleshooting]
author: epsi

excerpt:
  Very simple GRUB2 tips when linux stuck on boot.

related_link_ids:
  - 15031749  # APT Pinning

---

One of the very common riddle in linux community, a frequently asked one,
is when a user stuck on boot after loading GRUB2.
 
Take a deep breath don't be panic, even when you have kernel panic, you need to calm down. 
Most of the time it is just something happened in init process,
sometimes it is just a display driver or else e.g. mount time,
so what you need is to have a more verbose error message.
All you need to do is, removing the quiet option in GRUB2.

Please Click for higher resolution.

[![GRUB2 Quiet Option][image-grub2-quiet]{: .img-responsive }][hires-grub2-quiet]

You can remove the quiet option in GRUB2 in either way.

* Temporary, by pressing e-key when booting in GRUB, and edit the entry.

* Change the grub.cfg in /boot/grub/, using Live CD/DVD/USB.
  It will be wiped to default when you update grub later.
  You can also edit grub.cfg from other partition in multiboot environment.
  
Well, it doesn't solve your boot problem,
but at least you have a chance to investigate,
what is going on in your boot process.
Especially, what service issue, that might block your system.

Furthermore you should also check the log, related the service issue.
e.g. XOrg when it come to display driver problem.

This is just my 2 Cents

-- -- --


Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

[image-grub2-quiet]: {{ site.url }}/assets/posts/system/2016/05/vim-grub-quiet.png

[hires-grub2-quiet]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMdI_Ocuj3BPV4MwB7FVEuwJBT-6Ay0PINbTtYR
