---
layout: post
title:  "What to Do, When the System Stuck, at Boot"
date:   2016-05-19 15:39:15 +0700
categories: system
tags: [package manager]
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
sometimes it is just a display driver or else,
so what you need is to have a more verbose error message.
All you need to do is, removing the quiet option in GRUB2.

Please Click for higher resolution.

[![GRUB2 Quiet Option][image-grub2-quiet]{: .img-responsive }][hires-grub2-quiet]

You can remove the quiet option in GRUB2 in either way.

* Temporary, by pressing e-key when booting in GRUB, and edit the entry.

* Change the grub.cfg in /boot/grub/, using Live CD/DVD/USB.
  It will be wipd to default when you update grub later.
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

[hires-grub2-quiet]: https://lh3.googleusercontent.com/e--6y5ffcgksD2Gjsyo4fM1caPeeKw9D0Iufa0CrIaQsrGFxeCEpk2UEJJ-DJKgUyftkonr9tDPbKJ1BRGSNeOzmDMUK-xPfgU8VJOHgPqJxPJXiX9qfBV96fNTw8me8cP_psnrVCkfWG6z8Da_Y6jbcb4IAyaKxM_wBksJ2np4O9CQHsMKIQZVqC54PpjH4JlPx1d-2Bw35D1Q7R1Qauv7XOmnYUuL1iVAdKOztYIIKHqx4W94lW2TUc5JRd8ageXNwLmz7L7VaCogcX-xPlaqgF2eRh1Zkq9CmfjXEvidNadoixMSKDZf835jrULIUTquukh_YptRfXpSTILvhqxU2FGdAaRRwdAIzG1hxmzNXxcwNfnlnPityYtyKiiWMRsS2AL3eqBn9_4nAB-lXGDsoIy-Pj5QJct3lhyYx4f3qtrav1ufe8s-giniL5bVIr1mQ_kGYXV-G2peZkFt_cGVaKxXcLLUT8_qDsNBG25BaIpFemCnKYwOHH3XIlSITwjZB1uatcgBDCFcOFfQRr14bif92WMaqvJ-O4m0Ifyo1YQcPysXPcAzQpdUkLsJCp2BHyqLPgDskPmHCiM7eB0w18AD26prYvqreizkTl3IqngeF=w0
