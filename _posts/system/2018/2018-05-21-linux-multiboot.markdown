---
layout: post
title:  "Linux Multiboot - Partition Schema"
date:   2018-05-21 09:25:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.

related_link_ids: 
  - 14010246  # Debian Install
  - 14040246  # Arch Install

---

### Preface

It has been four years since the last time I wrote my multiboot article.

*	[Linux Multiboot, Setting up Partitions][basic-multiboot]

#### Shared Partition

Since then, I use multiboot in other computer as well.
I found that multiboot requires a shared partition,
that I could use, along with every installed linux.

	I usually named them Works or Docs.

Of course you can use different name.
However this is my configuration:

*	Works: shared partition for any linux in my PC.

*	Docs: Windows Data.

#### Samba Configuration

And I also use the same samba configuration,
so my network always find the right document,
no matter linux, that I boot.

To be exact, my samba path always here.

*	<code>/media/Works/Samba</code>

{% highlight conf %}
% tree -L 1 /media/Works
/media/Works
├── ...
├── e-Pajak
├── ...
├── githublab
├── ..
├── Mailbox
├── Music
├── Samba
└── ...
{% endhighlight %}

And in <code>smb.conf</code> for each distribution installed:

{% highlight conf %}
[Samba]
   path = /media/Works/Samba/
{% endhighlight %}

#### Table of Content

This time, I need to show up more configuration,
how to make, a shared partition across different distribution.

*	Partition Schema

*	/etc/fstab to make shared partition

*	samba configuration

*	updating with chroot

#### Dotfiles Document

Config is available at:

* [github.com/epsi-rns/dotfiles/.../multiboot][dotfiles-multiboot]

Including **grub**, **fstab**, and **samba**.

-- -- --

### Partition Schema

It is all installed well.
And I also have other partition as well.

#### Operating System

All setup is done without any issue.

*	System (NTFS: Windows 7 System)

*	openSUSE (/: btrfs, /home: xfs, /boot: ext4)

*	Fedora (ext4)

*	Debian (ext4)

*	KaOSx (xfs)

#### OpenSUSE Case

Each OS in just one partition,
except openSUSE that has three partitions.

*	/: btrfs, 

*	/home: xfs, 

*	/boot: ext4

I need to make separate /boot partition as a workaround.
Because GRUB2 from other OS cannot read the BTRFS at boot.

#### Shared Partition

*	Works (ext4)

*	Docs (ntfs: windows data)

#### By Label

I had labeled each partition.

The root / of openSUSE has BTRFS type on **/dev/sda9**,
it cannot be labeled, therefore it is not shown below.

Since all linux OS has /home,
to avoid confusion, I name the /home partition of openSUSE as **Fun**.

{% highlight conf %}
% tree /dev/disk/by-label
/dev/disk/by-label
├── boot -> ../../sda7
├── Debian -> ../../sda12
├── Docs -> ../../sda6
├── Fedora -> ../../sda11
├── Fun -> ../../sda8
├── KaOSx -> ../../sda10
├── System -> ../../sda1
└── Works -> ../../sda5

0 directories, 8 files
{% endhighlight %}

![Partition: Disk by Label][image-ss-by-label]{: .img-responsive }

#### Partition Information

You can get partition information,
for each device path such as **/dev/sda9**,
by using this command.

{% highlight bash %}
% sudo file -sL /dev/sda9
/dev/sda9: BTRFS Filesystem sectorsize 4096, nodesize 16384, leafsize 16384, UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f, 32208326656/52743372800 bytes used, 1 devices
{% endhighlight %}

![Partition: Device Information][image-ss-file-sda9]{: .img-responsive }

-- -- --

### Preparation: Mount Point

Now we need to set up mount point.
I set it up at **/media**.
And each OS should have these mount point.

#### openSUSE

{% highlight conf %}
% tree /media -x
/media
├── Boot
├── Debian
├── Docs
├── Fedora
├── KaOSx
├── System
└── Works

7 directories, 0 files
{% endhighlight %}

Note: Without openSUSE directory, no Fun Directory either.

![Multiboot: Partition Mount Point Preparation][image-ss-media-dir]{: .img-responsive }

#### Fedora

{% highlight conf %}
% tree /media -x
/media
├── Boot
├── Debian
├── Docs
├── Fun
├── openSUSE
├── KaOSx
├── System
└── Works

8 directories, 0 files
{% endhighlight %}

Note: Without Fedora directory.

#### KaOSx

{% highlight conf %}
% tree /media -x
/media
├── Boot
├── Debian
├── Docs
├── Fedora
├── Fun
├── openSUSE
├── System
└── Works

8 directories, 0 files
{% endhighlight %}

Note: Without KaOSx directory.

#### Debian

Debian has additional default mount point.

{% highlight conf %}
% tree /media -x
/media
├── Boot
├── cdrom -> cdrom0
├── cdrom0
├── Docs
├── Fedora
├── Fun
├── KaOSx
├── openSUSE
├── System
└── Works
{% endhighlight %}

Note: Without Debian directory.

-- -- --

### Goal

How it is going to be ?

Before going further to /etc/fstab.
This is what we want to achieve.

#### KDE Partition Manager.

With this GUI, you will have better understanding.

![Multiboot: KDE Partition Manager][image-ss-partman]{: .img-responsive }

#### Block Device Attribute

GUI is nice, however, there is a more geeky way using **blkid**.

{% highlight conf %}
% sudo blkid -o list                                                         ~ andalan
device         fs_type label    mount point        UUID
---------------------------------------------------------------------------------------
/dev/sda1      ntfs    System   /media/System      442456A824569CAC
/dev/sda5      ext4    Works    /media/Works       954a9b1e-c8c0-4f38-8877-fa891c79c9ae
/dev/sda6      ntfs    Docs     /media/Docs        01CF01333AD2FF20
/dev/sda7      ext4    boot     /boot              23342d48-c3be-402c-b049-b3e9ddeafbc0
/dev/sda8      xfs     Fun      /home              0095473d-ae63-4722-8350-f5716e5df333
/dev/sda9      btrfs            (in use)           c114d95e-bc0a-4b41-a2db-abd21aa9850f
/dev/sda10     xfs     KaOSx    /media/KaOSx       50f93bc6-711d-4f70-84cf-09748e653543
/dev/sda11     ext4    Fedora   /media/Fedora      9c76fb33-fa7a-46af-b2bb-f82d385b81b6
/dev/sda12     ext4    Debian   /media/Debian      4190d7a9-fb03-4d19-864f-7d04f89c3be0
/dev/sda13     swap             [SWAP]             b623e30b-c7a5-4f99-a250-45372da4c5b4
{% endhighlight %}

![Multiboot: blkid -o list][image-ss-blkid]{: .img-responsive }

-- -- --

### What's next

How do we achieve it ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/multiboot/pc-01' %}

[basic-multiboot]: http://localhost:4000/system/2014/03/13/linux-multiboot.html

[dotfiles-multiboot]:     {{ dotfiles }}
[dotfiles-grub-debian]:   {{ dotfiles }}/grub.debian.cfg
[dotfiles-grub-fedora]:   {{ dotfiles }}/grub.fedora.cfg
[dotfiles-grub-opensuse]: {{ dotfiles }}/grub.opensuse.cfg
[dotfiles-grub-kaosx]:    {{ dotfiles }}/grub.kaosx.cfg

[image-ss-partman]:    {{ asset_path }}/kaosx-kde-partman.png
[image-ss-by-label]:   {{ asset_path }}/opensuse-disk-by-label.png
[image-ss-media-dir]:  {{ asset_path }}/opensuse-media-directory.png
[image-ss-blkid]:      {{ asset_path }}/opensuse-blkid-list.png

[image-ss-file-sda9]:  {{ asset_path }}/fedora-file-sda9.png
