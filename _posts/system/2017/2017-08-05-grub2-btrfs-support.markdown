---
layout: post
title: "File System - GRUB2 support for BTRFS"
date      : 2017-08-05 09:45:15 +0700
categories: system
tags      : [btrfs, grub2, filesystem]
keywords  : [opensuse, partition, grub2]
author: epsi

opengraph:
  image: /assets/site/images/topics/opensuse.png

excerpt:
  openSUSE Post Install Log.

related_link_ids: 
  - 17083145  # Docker Package Manager Summary
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration
  - 17080645  # Mageia 6 Upgrade
  - 17080345  # openSUSE Tumbleweed Install
  - 17080245  # Fedora 23 to 26 Install
  - 17080145  # Manjaro OpenRC Issues

---

### Preface

> GRUB2 from other distribution cannot boot into distribution using BTRFS as root

I have a multiboot situation consist of Debian, Fedora, OpenSUSE and KaOS. 
I'm just willing to learn different package managers. 
This is more like a GRUB2 issue, and not a BTRFS issue,
Since I cannot find a GRUB2 community, I blog instead.

### Partition

Partition as seen in KDE Partitioner Manager form KaOSx.

*	/dev/sda1  : NTFS  : W7

*	/dev/sda8  : Swap

*	/dev/sda7  : Ext4  : Debian

*	/dev/sda9  : Ext4  : Fedora

*	/dev/sda10 : BTRFS : openSUSE Tumbleweed

*	/dev/sda11 : XFS   : openSUSE Tumbleweed /home

*	/dev/sda12 : XFS   : KaOS

![KDE Partitioner Manager][image-ss-partitioner]{: .img-responsive }

All distro can mount other partition using root privileges.
No issue in reading XFS and BTRFS in File Manager.

-- -- --

### Distribution Test

#### GRUB2 installed by openSUSE (BTRFS)

GRUB2 boot successfully to openSUSE.
This openSUSE use BTRFS mounted at root <code>/</code>.
Of course, the GRUB installed from this openSUSE

#### GRUB2 installed by Fedora (ext4)

Fedora seems to recognize openSUSE in BTRFS.
It shown in grub2-mkconfig, but does not write 
openSUSE entries to output in /boot/grub2/grub.cfg.

![Fedora GRUB2 Issue][image-ss-fedora-grub]{: .img-responsive }

{% highlight bash %}
$ sudo grub2-mkconfig -o /boot/grub2/grub.cfg 
Generating grub configuration file ...
Found linux image: /boot/vmlinuz-4.11.10-300.fc26.x86_64
Found initrd image: /boot/initramfs-4.11.10-300.fc26.x86_64.img
Found linux image: /boot/vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
Found initrd image: /boot/initramfs-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e.img
mount: /var/lib/os-prober/mount: mount(2) system call failed: No such file or directory.
device-mapper: reload ioctl on osprober-linux-sda6 failed: Device or resource busy
Command failed
Found Windows 7 on /dev/sda1
Found openSUSE Tumbleweed on /dev/sda10
Found KaOS (rolling) on /dev/sda12
Found Debian GNU/Linux 9 (stretch) on /dev/sda7
{% endhighlight %}

#### GRUB2 installed by Debian (ext4)

Debian does not seems to recognize openSUSE at all. 

![Debian GRUB2 Issue][image-ss-debian-grub]{: .img-responsive }

#### GRUB2 installed by KaOS (XFS)

KaOS does not seems to recognize openSUSE at all. 
Same issue with Debian.

-- -- --

### Workaround

#### Failed

I try a workaround, copy paste specific entries 
from openSUSE's grub.cfg to Fedora's grub.cfg. 

{% highlight conf %}
menuentry 'openSUSE Tumbleweed'  --class opensuse --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-simple-c114d95e-bc0a-4b41-a2db-abd21aa9850f' {
	load_video
	set gfxpayload=keep
	insmod gzio
	insmod part_msdos
	insmod btrfs
	set root='hd0,msdos10'
	if [ x$feature_platform_search_hint = xy ]; then
	  search --no-floppy --fs-uuid --set=root --hint-bios=hd0,msdos10 --hint-efi=hd0,msdos10 --hint-baremetal=ahci0,msdos10 --hint='hd0,msdos10'  c114d95e-bc0a-4b41-a2db-abd21aa9850f
	else
	  search --no-floppy --fs-uuid --set=root c114d95e-bc0a-4b41-a2db-abd21aa9850f
	fi
	echo	'Loading Linux 4.11.8-2-default ...'
	linux	/boot/vmlinuz-4.11.8-2-default root=UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f  ${extra_cmdline} resume=/dev/disk/by-uuid/b623e30b-c7a5-4f99-a250-45372da4c5b4 splash=silent quiet showopts
	echo	'Loading initial ramdisk ...'
	initrd	/boot/initrd-4.11.8-2-default
}
{% endhighlight %}

But it doesn't work. It said

{% highlight conf %}
"error: file '/boot/vmlinuz-4.11.8-2-default' not found"
{% endhighlight %}

I have checked that the file "vmlinuz-4.11.8-2-default" is there in btrfs partition. 
I aware there is option "insmod btrfs". 
And I suspect that Fedora's GRUB2 cannot read BTRFS. 
The same case also happened with Debian.

What is going on here ?
Any solution I can do?
A workaround  ?
I need an enlightenment.

#### Working

Three workarounds, two responses from Fedora Community, and the third is mine.

1.	Using systemd-boot, forget about GRUB.
	Require GPT and EFI. Which is I use MBR without EFI.

2.	Create a special /boot partition using ext4 for openSUSE BTRFS.
	I finally use this method.

3.	Install GRUB2 from openSUSE. 
	I can do it using chroot from openSUSE USB live stick.

### Conclusion

	It works !

I decide to create /boot partition in /dev/sda13 using /ext4.

But I still wonder, whether there is GRUB2 support for BTRFS, or not.

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/filesystem-btrfs' %}

[image-ss-partitioner]:      {{ asset_post }}/kaosx-kde-partitioner-manager.png
[image-ss-fedora-grub]:      {{ asset_post }}/fedora-grub-btrfs-issue.png
[image-ss-debian-grub]:      {{ asset_post }}/debian-grub-btrfs-support.png

