---
layout: post
title: "Distribution - Debian to Devuan Migration"
date: 2017-08-08 09:45:15 +0700
categories: system
tags: [debian, devuan, sysvinit, distro, package manager]
author: epsi

excerpt:
  Debian Systemd to Devuan Sysvinit, Migrate Log.

related_link_ids: 
  - 17083145  # Docker Package Manager Summary
  - 17080745  # Manjaro Artix Migration
  - 17080645  # Mageia 6 Upgrade
  - 17080545  # GRUB2 BTRFS Support
  - 17080345  # openSUSE Tumbleweed Install
  - 17080245  # Fedora 23 to 26 Install
  - 17080145  # Manjaro OpenRC Issues
  - 14112819  # Init Civil War

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

### Screenshot

OS: Devuan

*	Window Manager: HerbstluftWM

*	Panel: Dzen2

[![Devuan Screenshot][image-ss-devuan-hlwm-dzen2]{: .img-responsive }][photo-ss-devuan-hlwm-dzen2]

-- -- --

### Consider Going Further

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

### Checking GRUB

Debian is somewhat interesting that 
Debian allow user to choose different init at boot.
A Debian system can be installed with two different init,
although you can only have one init running at a time.
Debian configure this kernel parameter in GRUB2.

{% highlight bash %}
init=/lib/sysvinit/init
{% endhighlight %}

Or maybe

{% highlight bash %}
init=/lib/systemd/systemd
{% endhighlight %}

	Now, I know.

This kernel parameter that handle <code>init</code>,
used to be a mystery to me. Now, nomore secret.

#### Devuan

Here is what I found in Devuan.

{% highlight bash %}
menuentry 'Debian GNU/Linux, with Linux 4.9.0-3-amd64' ... {
	...
	echo	'Loading Linux 4.9.0-3-amd64 ...'
	linux	/boot/vmlinuz-4.9.0-3-amd64 root=/dev/sda10 ro  init=/lib/systemd/systemd
	echo	'Loading initial ramdisk ...'
	initrd	/boot/initrd.img-4.9.0-3-amd64
}
{% endhighlight %}

#### Debian

Here is what I found in Debian.

{% highlight bash %}
menuentry 'Debian GNU/Linux, with Linux 4.9.0-3-amd64' ... {
	...
	echo	'Loading Linux 4.9.0-3-amd64 ...'
	linux	/boot/vmlinuz-4.9.0-3-amd64 root=/dev/sda7 ro  init=/lib/sysvinit/init
	echo	'Loading initial ramdisk ...'
	initrd	/boot/initrd.img-4.9.0-3-amd64
}
{% endhighlight %}

-- -- --

### Other Issues

I haven't got time to have deeper exploration on Devuan yet.
There are some issues such as slim that has no reboot option.
Maybe next time.

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/distro-devuan' %}

[image-ss-devuan-keyring]:    {{ asset_post }}/keyring.png
[image-ss-devuan-mirror]:     {{ asset_post }}/mirror.png

[image-ss-devuan-nosystemd]:  {{ asset_post }}/no-libsystemd0.png
[photo-ss-devuan-nosystemd]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPl9Prff4wrSj-I77e-qZpirHYYlPUxR1_1L-iB?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-devuan-hlwm-dzen2]: {{ asset_post }}/hlwm-dzen2.png
[photo-ss-devuan-hlwm-dzen2]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipORZB9aEXv-lA1OgQpa_FDT5hA7r9jr5Ehi7p88?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
