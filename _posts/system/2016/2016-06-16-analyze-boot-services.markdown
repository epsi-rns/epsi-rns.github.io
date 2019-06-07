---
layout: post
title:  "Analyze Services in Linux Boot Process"
date      : 2016-06-16 18:57:15 +0700
categories: system
tags      : [init]
keywords  : [systemd, svg]
author: epsi

excerpt:
  If you want to gain a faster linux boot,
  you can analyze services loaded on boot with systemd.

related_link_ids: 
  - 14112819  # Init Civil War

---

Faster boot linux ?

There are usually these things that make your boot heavy. 
Your bunch of services. And your choice of Desktop Environment.

Luckily with with <code>systemd</code>, 
you can analyze boot process so you can decide what optimization necessary.

{% highlight bash %}
 $ systemd-analyze plot > my-distro.svg
{% endhighlight %}


You can see the result in both figures below.
First is my Arch, and the Second is my Debian. 
Both were taken in 2014 from the computer.

You can see how the differences that it makes
when you have many services loaded at boot.

**arch**

[![Arch Boot systemd][image-boot-arch]{: .img-responsive }][photo-boot-arch]
&nbsp;

**debian**

[![Debian Boot systemd][image-boot-debian]{: .img-responsive }][photo-boot-debian]

This is not an Arch versus Debian comparison.
I put a lot of services in Debian
because I had my development server on Debian.
I did not put many services in Arch,
because it was a result of a clean installation.

The choice, is yours to optimize.

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

[image-boot-arch]:   {{ site.url }}/assets/posts/system/2016/06/arch-boot.png
[image-boot-debian]: {{ site.url }}/assets/posts/system/2016/06/debian-boot.png

[photo-boot-arch]:   https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMpJyvPdwv396veW0xxUUiQwaUkW9k0uKWyBCfV
[photo-boot-debian]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMBBkisgbw-6sZS5ckq_6jjIfzqX9sbyfL-b6EJ
