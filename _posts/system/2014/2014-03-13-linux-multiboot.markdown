---
layout: post
title:  "Linux Multiboot, Setting up Partitions"
date:   2014-03-13 18:31:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.

related_link_ids: 
  - 14010246  # Debian Install
  - 14040246  # Arch Install

---

{% include post/2018/05/toc-multiboot.html %}

This is a remake of my facebook note with the same title.

* [facebook.com/notes/.../426339144178768][facebook-note]

Why stay with one distro (linux distribution)?<br/>
Let's play with multiboot.

In this example I'm using four OS

	Win7 + Debian + Kali + Arch

![Multiboot: Cover][image-cover]{: .img-responsive }




## Choose your Distro 

Before trying any new Distro,<br/>
consider Live DVD/USB before making decision.

VM would also help for temporary solution.<br/>
But you will feel more like home using multi boot solution.


## Windows user? 

If you are coming from Windows, you might be familiar with this image.

Let's start with this example.

[![Multiboot: Ease-us Partition][image-easeus]{: .img-responsive }][photo-easeus]


Please prepare before installing linux.

* Continuous free space in EXTENDED partition

* MBR type should be BASIC, this type is more flexible than DYNAMIC.

* Disable UEFI if you are using Win 8.


To rearrange MS Windows partition, you can use

* Easeus Partition Manager.: http://www.filehippo.com/download_easeus_partition_master_home/

Note: I have never tried this with Windows 8.

Caution: While installing, choose largest continuous space, and DO NOT choose ENTIRE SPACE, as it will wipe your Windows entirely.


## Which to install first 

It is easier to install windows first, then Linux.<br/>
But it's okay to install linux first, then windows.<br/>
There's a trick for this, but the scope is outside of this article.


## Tips: Linux learning stage 

With Dual boot, Windows + Linux

1. Download ISO burn to DVD, or USB

2. Backup your important data.

3. Prepare your Partition. Easeus is a nice tool. 10 GB would be enough. 20 GB better. 60 GB luxury.

4. Install, follow the step. Click. Click. Click.

5. Now you have dual-boot. Win 7, and Ubuntu

6. First step to linux, have fun.

7. Try some DE: KDE, XFCE, LXDE, Cinnamon.

8. Experiencing Command line. Read manual. # man man

9. You don't need to erase your windows. But at this stage you don't want to go home through windows.


## Partition Structure 

This my partition structure in my computer,<br/>
just in case you need some example.

[![Multiboot: GParted sda][image-gparted]{: .img-responsive }][photo-gparted]

Set-up as multi boot system,

* (0) /dev/sda (MBR): GRUB2

OS Available

* (1) /dev/sda1: C:> Windows in primary

* (2) /dev/sda8: Debian Linux

* (3) /dev/sda7: Kali Linux

* (4) /dev/sda10: Arch Linux


The other part contain

* (5) /dev/sda9: Linux Swap

* (6) /dev/sda5: D:> NTFS Partition

* (7) /dev/sda6: E:> FAT32 Partition

The only primary partition is (/dev/sda1)<br/>
While others (5-10) belong to extended (/dev/sda2)

[![Multiboot: cfdisk][image-cfdisk]{: .img-responsive }][photo-cfdisk]

## How do I keep my Windows Recovery Partition safely? 

It is a matter of Primary and Logical Partition.<br/>
So my answer is, it should work on Primary Partition.

1. Windows (and Recovery) should be placed on Primary Partition.

2. Linux is very nice, any distribution could be placed in Logical Partition. Swap also ini Logical.

3. GRUB (boot loader) should be placed in MBR (master boot record).

4. An HDD may have four (Primary and Extended) Partition. An extended partition can have many Logical Partition.

So a partition can be rearranged to something similar like this, but order is not neccesary:

* Primary: /dev/sda1 Windows Recovery

* Primary: /dev/sda2 Windows

* Extended: /dev/sda3


# Bonus: Post Install: fstab 

This section does not really related with multiboot, but rather a file system or disk management.<br/>
The /etc/fstab for that sample partition scheme could be arranged just like this:

(Debian/ Kali)

{% highlight bash %}
># pico /etc/fstab
{% endhighlight %}

OR

(Ubuntu)

{% highlight bash %}
 $ sudo nano /etc/fstab
{% endhighlight %}

OR

(Ubuntu)

{% highlight bash %}
 $ gksu gedit /etc/fstab
{% endhighlight %}

[![Multiboot: /etc/fstab][image-fstab]{: .img-responsive }][photo-fstab]

For more information you can

{% highlight bash %}
 $ man fstab
 $ man mount
{% endhighlight %}

-- -- --

### What's Next

Consider continue reading [ [Multiboot: Partition Schema][local-part-config] ].


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2014/03' %}

[local-part-config]:    /system/2018/05/21/linux-multiboot.html

[facebook-note]: https://www.facebook.com/notes/epsi-r-nurwijayadi/linux-multiboot/426339144178768
[image-cover]: {{ asset_path }}/multiboot-cover.png
[image-easeus]: {{ asset_path }}/multiboot-ease-us-partition.png
[photo-easeus]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipNhgwwtuUixyQh2Nm1Xbfwhk6H7poVmapwG24Us
[image-gparted]: {{ asset_path }}/multiboot-gparted-sda.jpg
[photo-gparted]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipO3VgkrKAO7a3lEa4TldF5lvI7Chc3psTbuIH6B
[image-fstab]: {{ asset_path }}/multiboot-fstab-sample.png
[photo-fstab]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipOHu3kTAX60inCasXq_TcoKnUlV6cTD7Gmjsi5c
[image-cfdisk]: {{ asset_path }}/multiboot-cfdisk.png
[photo-cfdisk]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPEpYu-6gI2ewKHRxOG0FQc5WQRajUfnCDc_sly
