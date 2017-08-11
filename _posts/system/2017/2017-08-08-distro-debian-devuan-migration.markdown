---
layout: post
title: "Distribution - Debian to Devuan Migration"
date: 2017-08-08 09:45:15 +0700
categories: system
tags: [debin, devuan, sysvinit, distro, package manager]
author: epsi

excerpt:
  Debian Systemd to Devuan Sysvinit, Migrate Log.

---

### Migrate

> Migrate Devuan, and go further, Prevent libsystemd0 to be Installed.

I have migrate from Debian Stretch (systemd) to Devuan Ascii (sysvinit).
Yes, I'm officialy a Devuan user, finally. 

![Devuan Mirror][image-ss-devuan-mirror]{: .img-responsive }


I followed the guidance.

*	<https://git.devuan.org/dev1fanboy/Upgrade-Install-Devuan/wikis/Upgrade-to-Devuan>

![Devuan Keyring][image-ss-devuan-keyring]{: .img-responsive }

There were issue, but solved.

*	NetworkManager is gone, but I can use wicd+dhcpcd

The rest works as a charm.

-- -- --

### And Go Further

Then, I decided to go further, remove libsystemd0 and prevent it to be installed.

{% highlight bash %}
$ cat /etc/apt/preferences.d/libsystemd0 
Package: libsystemd0
Pin: release *
Pin-Priority: -1
{% endhighlight %}

I do not think libsystemd0 is a dummy package.
I just need to know, How bad this could be, prevent libsystemd0 to be installed.

-- -- --

### Result

*	Work well: Libreoffice, Inkscape and Chromium.

*	Lost: SDDM, GIMP, CUPS, SAMBA, Firefox.

It is good to know how bad each dependencies are.
Please click for more complete investigation using uncropped figure.

[![Devuan Keyring][image-ss-devuan-nosystemd]{: .img-responsive }][photo-ss-devuan-nosystemd]

Note that Devuan works well if you keep <code>libsystemd0</code>.

-- -- --

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[image-ss-devuan-keyring]:    {{ asset_path }}/devuan-keyring.png
[image-ss-devuan-mirror]:     {{ asset_path }}/devuan-mirror.png

[image-ss-devuan-nosystemd]:  {{ asset_path }}/devuan-no-libsystemd0.png
[photo-ss-devuan-nosystemd]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPl9Prff4wrSj-I77e-qZpirHYYlPUxR1_1L-iB?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn