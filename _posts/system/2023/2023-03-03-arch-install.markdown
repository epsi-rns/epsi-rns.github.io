---
layout    : post
title     : "Arch: Install"
categories: system
date      : 2023-04-03 09:25:15 +0700
tags      : [install]
keywords  : [vanilla arch, lenovo, install]
author: epsi
toc        : toc/2023/05/toc-install.html

excerpt:
  Examine arch install for multiboot situation.

opengraph:
  image: /assets/posts/system/2023/04/014-lsinitcpio.png

---

### Preface

> Goal: Examine arch install for multiboot situation.

The last time I wrote about Arch install is in 2014 (nine years ago).

I do every single command
in black TTY terminal under arch installer.
But for illustration,
I give it some white terminal screenshot.
So my article could be easier to be read.

### Reading

Nine years ago I wrote this:
_Arch philosophy is about **knowing your system**. It doesn't give fancy installer._
_In fact you have to do it all manually with command line without installer._
_Before you step in, you have to be brave to walk with command line._
_And be sure you have read the manual._

* [Arch Wiki: Beginners's Guide: Installation][link-archwiki]

The difference is,
now you can have archinstall script that can guide you,
and that means you can entirely skip this article.

I still prefere manual install.
Because I still need to update my knowledge.

#### Rewrite

Nine years ago I wrote this article:

* [Arch Linux - Install/ Post Install Log][local-arch-install]

It is worth the risk. Your few days of installing Arch
give you more knowledge than a year with Kali.
After you get the knowledge for a while,
you can install it again in just a few minutes.

Many beginners asking for guidance to install Arch. 
There is actualy two steps.
First step is Install Vanilla Arch itself. Until you get your first root login after boot.
The second step is Post Install. e.g. setup Wireless, or setup WM/DE (Desktop Environment).

If you feel that you are not ready for Arch,
you can still improve your skill with other distro.

<a name="toc"></a>

#### Table of Content

* [Table of Content](#toc)
* A: [Prepare the Bootable](#flashdisk)
* B: [BIOS and Windows Setting](#bioswindows)
* C: [Partition Setup](#partition)
* D: [Wireless Connexion](#wireless)
* Arch :: [Install Log](#install-log)
* 1: [Changing Root](#chroot)
* 2: [Basic Setting](#basic)
* 3: [Prepare System](#system)
* [Reboot](#reboot)

* [What is Next?](#whats-next)

-- -- --

### A: Prepare the Bootable

<a name="flashdisk"></a>

> Flash Disk

If you are a windows user, you can use `rufus`.
Linux user would simply love `dd`.
Since I also use pipe to `pv`,
and pipe don't do well with sudo,
I intentionally use `sudo ls`,
so the next command (`dd` and `pv`) already has the priviliges.

{% highlight bash %}
$ sudo ls
$ sudo dd \
  if=/media/Works/archlinux-2023.06.01-x86_64.iso \
  | pv -s 900M |sudo dd \
  of=/dev/sda bs=4096
{% endhighlight %}

![Arch Install: Prepare the CD: dd][001-dd-arch-linux]

-- -- --

<a name="bioswindows"></a>

### B: BIOS and Windows Setting

Yes we need to learn to coexist.

#### BIOS Setting

Please disable the boot secure.

#### Windows Setting

If you are using Windows 10 or later,
without boot secure, you need to disable `bitlocker`.

![Arch Install: Turn off the windows bitlocker][002-ss-bitlocker]

-- -- --

<a name="partition"></a>

### C: Partition Setup

As usual we need to prepare the partition.

#### Planned Partition

My SSD capacity is about 500 GB,
This is what I plan.

{% highlight bash %}
Windows: 200 GB ntfs
Docs:     60 GB ntfs
Works:    60 GB ext4
Arch:     75 GB ext4
{% endhighlight %}

In real life situtation
We have to deal with `/boot/efi`,
and also swap.

{% highlight bash %}
Win11 EFI: 260 MB 
Windows:   195 GB ntfs
Docs:       59 GB ntfs
SWAP:       14 GB swap
Works:      59 GB ext4
Arch:       74 GB ext4
Empty:      74 GB ext4
Recovery:   2 GB
--------------------
Total     477 GB
{% endhighlight %}

I also preserved about 74 GB empty space for Gentoo.
Note that the number is rounded, and the total sum is just approximation.

#### Real Partition

I'm using the free Easeus to manage the partition.
First I create the Docs as NTFS.
Then I create empty space for swap, rot and other partition as well.

![Arch Install: Prepare the Partition][003-easeus-part]

As a linux user, you can see the GPT artition structure is flat, different with MBR.

{% highlight bash %}
❯ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINTS
nvme0n1     259:0    0 476.9G  0 disk 
├─nvme0n1p1 259:1    0   260M  0 part /boot/efi
├─nvme0n1p2 259:2    0    16M  0 part 
├─nvme0n1p3 259:3    0 195.3G  0 part /media/System
├─nvme0n1p4 259:4    0  58.6G  0 part /media/Docs
├─nvme0n1p5 259:5    0  14.6G  0 part [SWAP]
├─nvme0n1p6 259:6    0  58.6G  0 part /media/Works
├─nvme0n1p7 259:7    0  73.8G  0 part /
└─nvme0n1p8 259:8    0     2G  0 part
{% endhighlight %}

![Arch Install: Partition Block Device][004-lsblk]

The docs is, my data partition in windows.
And the works is, my data partition for daily usage in linux.

#### Reading

For more multiboot fun,
you can read in my other article series.

* [Linux Multiboot - Partition Schema][local-multiboot]

-- -- --

<a name="wireless"></a>

### D: Wireless Connexion

This is something you can try in other linux,
as practice preparation,
before you get into real arch linux installation.

{% highlight bash %}
❯# iwctl

[iwd]# device list
[iwd]# device wlan0 set-property Powered on
[iwd]# station wlan0 scan
[iwd]# station wlan0 get-networks
[iwd]# station wlan0 connect "E.R. Nurwijayadi"
[iwd]# exit

❯# ping archlinux.org
{% endhighlight %}

The arch linux install comes with this good `iwd`.
More about iwd can be read here:

* [WLAN: INet Wireless Daemon][local-iwd]

`iwd` is definitely easy to be used.

-- -- --

<a name="install-log"></a>

### Arch :: Install Log

Let's get into it.
Boot the arch installer.
And see what we've got.

#### First impression

What I feel is the boot take
a little bit longer than I expect. 
Maybe it becuase of my notebook.

Then I found that sometimes
my terminal suffocated with a lot of information.
I try to reboot, then it's gone.
But sometimes it comes again, very annoying.
I solve this with this two commands:

{% highlight bash %}
❯# auditctl -e0
❯# dmesg -n 1
{% endhighlight %}

#### Wireless Connexion

> Connecting to the internet

Yes, of course, first thing to do is,
connecting to the internet.
As I said you can use `iwd`.

After connected with internet,
you can immediately use `timedatectl`.

{% highlight bash %}
❯# timedatectl
{% endhighlight %}

-- -- --

<a name="chroot"></a>

### 1: Changing Root

#### Formatting The Root Partition

I wpuld like a fresh start.
So I format the `nvme0n1p71` partition with ext4.

{% highlight bash %}
❯# mkfs.ext4 /dev/nvme0n1p7
{% endhighlight %}

#### Mount All Partition

We are going to chroot,
so we need to mount all required partition.

{% highlight bash %}
❯# mount /dev/nvme0n1p7 /mnt
{% endhighlight %}

And also the EFI part.

{% highlight bash %}
❯# mkdir /mnt/boot/efi
❯# mount /dev/nvme0n1p1 /mnt/boot/efi
{% endhighlight %}

And finally the swap.

{% highlight bash %}
❯# swapon /dev/nvme0n1p5
{% endhighlight %}

Note that, some tutorial mount the EFI in `/mnt/boot/`.
But I choose different method.

#### Base Install

Now we can fill with some basic packages,
required to run the system.

{% highlight bash %}
❯# pacstrap -K /mnt base linux linux-firmware
{% endhighlight %}

And we also need to configure `fstab`,
in order to `chroot`.

{% highlight bash %}
❯# genfstab -U /mnt >> /mnt/etc/fstab
{% endhighlight %}

#### Change Root

This is the most important part.
Changing the root, from the arch installer,
into your newly installed one.

{% highlight bash %}
❯# arch-chroot /mnt
{% endhighlight %}

I'm a fan of chroot in multiboot situation.
You can also chroot, from any running linux partition.
Not just arch installer from bootable flash disk,
but also from different partition.

#### Mount

I would like to know the result of our previous `genfstab`.
Since it has a lot of comment `#`,
I filter using `-v` grep exclusion .

{% highlight bash %}
❯ cat /etc/fstab | grep -v "^#"


UUID=6d64b51a-155e-4c9e-9a3c-d19abebc5126  /           ext4        rw,relatime  0 1

UUID=3E7D-B1CB        /boot/efi   vfat        umask=0077 0 2

UUID=b125fe63-4ae8-4228-9e6b-9475081dce86  swap        swap    defaults,noatime 0 0
{% endhighlight %}

![Arch Install: cat /etc/fstab][018-fstab]

The fstab format is concise,
but it doesn't seems clear to me in narrow terminal.
so I'd better use `findmnt` instead.

{% highlight bash %}

❯ findmnt -su
TARGET    SOURCE               FSTYPE OPTIONS
/         UUID=6d64b51a-155e-4c9e-9a3c-d19abebc5126
                               ext4   rw,relatime
/boot/efi UUID=3E7D-B1CB       vfat   umask=0077
swap      UUID=b125fe63-4ae8-4228-9e6b-9475081dce86
                               swap   defaults,noatime
{% endhighlight %}

![Arch Install: findmnt -su][018-findmnt-su]

#### Text Editor

I really think that text editor can be useful.

{% highlight bash %}
❯# pacman -S nano
{% endhighlight %}

Do this inside the chroot.

-- -- --

<a name="basic"></a>

### 2: Basic Setting

#### Localtime

Localtime: Asia/Jakarta

{% highlight bash %}
❯# ln -sf /usr/share/zoneinfo/Asia/Jakarta /etc/localtime
{% endhighlight %}

#### Clock

{% highlight bash %}
❯# hwclock --systohc
{% endhighlight %}

#### Locale

Also uncomment your chosen locale,
or just write it if you dpn't have any text editor already.

{% highlight bash %}
❯ cat /etc/locale.gen | grep -v "^#"
en_US.UTF-8 UTF-8  
{% endhighlight %}

Here I invert the grep with `-v`.

![Arch Install: Locale Gen][011-locale-gen]

Now we have required file to build by `locale-gen`

{% highlight bash %}
❯# locale-gen
{% endhighlight %}

The system wide setting,
that actually used the locale is in the `conf` file.

{% highlight bash %}
touch /etc/locale.conf
nano /etc/locale.conf
{% endhighlight %}

You can still set without text editor.

{% highlight bash %}
❯# echo LANG=en_US.UTF-8 > /etc/locale.conf
{% endhighlight %}

{% highlight bash %}
❯ cat /etc/locale.conf
LANG=en_US.UTF-8
{% endhighlight %}

![Arch Install: Locale Configuration][012-locale-conf]

#### Hostname

Let's change a bit,
such as `utama` is the hostname I want.

{% highlight bash %}
❯# touch /etc/hostname
❯# nano /etc/hostname
{% endhighlight %}

Now we see what's in there.

{% highlight bash %}
❯ cat /etc/hostname
utama
{% endhighlight %}

![Arch Install: hostname][013-hostname]

-- -- --

<a name="system"></a>

### 3: Prepare System

#### Initial Ramdisk

I understand in install process we become doer,
without really understanding of what is really going on.

{% highlight bash %}
mkinitcpio -P
{% endhighlight %}

This will make the `initramfs` as below:

{% highlight bash %}
❯ ls /boot
efi   initramfs-linux-fallback.img  vmlinuz-linux
grub  initramfs-linux.img
{% endhighlight %}

![Arch Install: initramfs][014-ls-boot]

If you are curious you can go further examining,
by uisng `lsinitcpio` after you finish your install,
and do not forget the sudo thing.

![Arch Install: ksinitcpio][014-lsinitcpio]

{% highlight bash %}
❯ sudo lsinitcpio -a /boot/initramfs-linux.img
==> Image: /boot/initramfs-linux.img
==> Created with mkinitcpio 36
==> Kernel: 6.3.9-arch1-1
==> Size: 15.93 MiB
==> Compressed with: zstd
  -> Uncompressed size: 44.68 MiB (.356 ratio)
  -> Estimated decompression time: 0.040s

==> Included modules:
  ...

==> Included binaries:
  blkid        mount
  busybox      switch_root
  e2fsck      systemd-tmpfiles
  fsck        udevadm
  kmod

==> Early hook run order:
  udev

==> Hook run order:
  udev keymap

==> Cleanup hook run order:
  udev
{% endhighlight %}

We can do it later.

#### Root Password

This is also a task you don not want to forget.

{% highlight bash %}
❯# passwd
New password: 
Retype new password: 
passwd: password updated successfully
{% endhighlight %}

![Arch Install: passwd][014-passwd]

If you forget, you can always `chroot`.

#### GRUB Install

I should learn new knowledge about EFI stuff.

{% highlight bash %}
❯# grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader=GRUB
Installing for x86_64-efi platform.
Installation finished. No error reported.
{% endhighlight %}

![Arch Install: grub-install EFI][016-grub-install]

The command would install the `grubx64.efi`
or something similar.

{% highlight bash %}
❯ sudo ls /boot/efi/EFI
Boot  GRUB  Microsoft
❯ sudo tree /boot/efi/EFI/GRUB
/boot/efi/EFI/GRUB
└── grubx64.efi

1 directory, 1 file
{% endhighlight %}

![Arch Install: EFI GRUB][016-efi-grub]

We require the EFI boot manager,
to understand the situation.

{% highlight bash %}
❯# pacman -S grub efibootmgr
{% endhighlight %}

Now you can see this:

{% highlight bash %}
❯ efibootmgr
BootCurrent: 0004
Timeout: 0 seconds
BootOrder: 2002,2001,0004,0003,2003
Boot0001* EFI PXE 0 for IPv4 (E4-A8-DF-E2-6B-A3)   PciRoot(0x0)/Pci(0x1c,0x0)/Pci(0x0,0x0)/MAC(e4a8dfe26ba3,0)/IPv4(0.0.0.00.0.0.0,0,0)RC
Boot0002* EFI PXE 0 for IPv6 (E4-A8-DF-E2-6B-A3)   PciRoot(0x0)/Pci(0x1c,0x0)/Pci(0x0,0x0)/MAC(e4a8dfe26ba3,0)/IPv6([::]:<->[::]:,0,0)RC
...
{% endhighlight %}

![Arch Install: efibootmgr][015-efibootmgr]

#### Grub Configuration

With BIOS managed boot,
we do not need to worry about windows boot
in the first place.
This means, we can ignore os-probe
while doing installation,
and take care of the windows boot in GRUB later on.

{% highlight bash %}
❯# grub-mkconfig -o /boot/grub/grub.cfg
{% endhighlight %}

Your screen would output text similar to this menu entry:

{% highlight bash %}
Generating grub configuration file ...
Found linux image: /boot/vmlinuz-linux
Found initrd image: /boot/initramfs-linux.img
Found fallback initrd image(s) in /boot:  initramfs-linux-fallback.img
...
{% endhighlight %}

The configuration itself would looks like something below:

{% highlight bash %}
### BEGIN /etc/grub.d/10_linux ###
menuentry 'Arch Linux' --class arch --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-simple-6d64b51a-155e-4c9e-9a3c-d19abebc5126' {
  load_video
  set gfxpayload=keep
  insmod gzio
  insmod part_gpt
  insmod ext2
  search --no-floppy --fs-uuid --set=root 6d64b51a-155e-4c9e-9a3c-d19abebc5126
  echo  'Loading Linux linux ...'
  linux  /boot/vmlinuz-linux root=UUID=6d64b51a-155e-4c9e-9a3c-d19abebc5126 rw  loglevel=3 quiet
  echo  'Loading initial ramdisk ...'
  initrd  /boot/initramfs-linux.img
}
{% endhighlight %}

![Arch Install: GRUB Configuration][017-grub-config]

#### OS Prober

I will skip this part.
My goal is succeed in booting linux.

The windows menu configuration can be configured later after boot.

-- -- --

### Prepare Post Install.

Networking is only one thing to be rpepared for post install.
So you'd better install `iwd` in the first place.
You can change to network manager later.
Or if you can also install network manager.

{% highlight bash %}
❯# pacman -S iw iwd wpa_supplicant networkmanager dhcpcd
{% endhighlight %}

-- -- --

<a name="reboot"></a>

### Reboot

That is all.
We can finally `exit` your chroot.
go back to acrh installer.
Then reboot.

{% highlight bash %}
❯# umount -R /mnt
{% endhighlight %}

{% highlight bash %}
❯# poweroff
{% endhighlight %}

Check if your arch linux can boot correctly using EFI from BIOS.
Also check from BIOS if your windows boot is still there,
and run correctly.

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

Form Installment, we can dive into post installment.

Consider continue reading [ [Arch: Post Install][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/04' %}

[local-whats-next]: /system/2023/04/05/arch-post-install.html

[link-archwiki]:    https://wiki.archlinux.org/index.php/Beginners%27_guide/Installation

[local-arch-install]:/system/2014/04/02/arch-install-log.html
[local-multiboot]:   /system/2018/05/21/linux-multiboot.html

[local-iwd]:         /system/2023/05/05/iwd.html

[001-dd-arch-linux]:{{ asset_path }}/001-dd-arch-linux.png
[002-ss-bitlocker]: {{ asset_path }}/002-ss-bitlocker.png
[003-easeus-part]:  {{ asset_path }}/003-easeus-partition.png
[004-lsblk]:        {{ asset_path }}/004-lsblk.png

[011-locale-gen]:   {{ asset_path }}/011-locale-gen.png
[012-locale-conf]:  {{ asset_path }}/012-locale-conf.png
[013-hostname]:     {{ asset_path }}/013-hostname.png

[014-lsinitcpio]:   {{ asset_path }}/014-lsinitcpio.png
[014-ls-boot]:      {{ asset_path }}/014-ls-boot.png
[015-efibootmgr]:   {{ asset_path }}/015-efibootmgr.png
[016-efi-grub]:     {{ asset_path }}/016-efi-grub.png
[017-grub-config]:  {{ asset_path }}/017-grub-config.png

[018-findmnt-su]:   {{ asset_path }}/018-findmnt-su.png
[018-fstab]:        {{ asset_path }}/018-fstab.png

[014-passwd]:       {{ asset_path }}/014-passwd.png
[016-grub-install]: {{ asset_path }}/016-grub-install.png

[//]: <> ( -- -- -- links below -- -- -- )

[021-efivars-01]:   {{ asset_path }}/021-efivars-01.png
[021-efivars-02]:   {{ asset_path }}/021-efivars-02.png
[021-efivars-03]:   {{ asset_path }}/021-efivars-03.png