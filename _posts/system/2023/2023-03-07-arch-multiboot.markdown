---
layout    : post
title     : "Arch: Multiboot"
categories: system
date      : 2023-04-07 09:25:15 +0700
tags      : [install]
keywords  : [vanilla arch, lenovo, multiboot]
author: epsi
toc        : toc/2023/05/toc-install.html

excerpt:
  Handling multiboot with GRUB, fstab, EFI.

opengraph:
  image: /assets/posts/system/2023/04/014-lsinitcpio.png

---

### Preface

> Goal: Handling multiboot with GRUB, fstab, EFI.

After basic administration and GUI,
we can go back to our homework.
Make the linux coexist with existing OS,
such as Windows or FreeBSD.

<a name="toc"></a>

#### Table of Content

* [Table of Content](#toc)

* [GRUB](#grub)
* [EFI](#efi)
* [fstab](#fstab)

* [What is Next?](#whats-next)

-- -- --

<a name="grub"></a>

### GRUB

We still need to do GRUB configuration,
to enable windows boot directly from the GRUB.

#### OS Prober

Since I use windows,
I want my windows entry to be also included.
This require `os prober.

{% highlight bash %}
❯# pacman -S grub-bios
❯# pacman -S os-prober
{% endhighlight %}

You still need to enable the `os-prober`.

{% highlight bash %}
❯ cat /etc/default/grub
GRUB_DISABLE_OS_PROBER=false
{% endhighlight %}

![Arch Install: GRUB OS Prober][017-grub-osprober]

And your windows entry will appear.

{% highlight bash %}
[root@utama epsi]# grub-mkconfig -o /boot/grub/grub.cfg
Generating grub configuration file ...
Found linux image: /boot/vmlinuz-linux
Found initrd image: /boot/initramfs-linux.img
Found fallback initrd image(s) in /boot:  initramfs-linux-fallback.img
Warning: os-prober will be executed to detect other bootable partitions.
Its output will be used to detect bootable binaries on them and create new boot entries.
Found Windows Boot Manager on /dev/nvme0n1p1@/EFI/Microsoft/Boot/bootmgfw.efi
Adding boot menu entry for UEFI Firmware Settings ...
done
{% endhighlight %}

![Arch Install: GRUB Detecting][017-grub-mkconfig]

The windows menu entry contain chainloader.

{% highlight bash %}
menuentry 'Windows Boot Manager (on /dev/nvme0n1p1)' -
-class windows --class os $menuentry_id_option 'osprob
er-efi-3E7D-B1CB' {
        insmod part_gpt
        insmod fat
        search --no-floppy --fs-uuid --set=root 3E7D-B
1CB
        chainloader /EFI/Microsoft/Boot/bootmgfw.efi
}
{% endhighlight %}

![Arch Install: GRUB menu entry: Windows][017-grub-windows]

While the EFI menu entry contain`fwsetup`.
This is new stuff for me.

{% highlight bash %}
menuentry 'UEFI Firmware Settings' $menuentry_id_option 'uefi-firmware' {
        fwsetup
}
{% endhighlight %}

![Arch Install: GRUB menu entry: UEFI][017-grub-uefi]

The linux menu entry part, you can examine yourself.

-- -- --

<a name="efi"></a>

### EFI Variables

We have to deal with EFI before.
I guess we need to go deeper for the sake of knowledge.
If we we boot using EFI,
we can find this directory.

{% highlight bash %}
❯# ls /sys/firmware/efi/efivars/
{% endhighlight %}

It is all in binary

{% highlight bash %}
❯# hexdump -C /sys/firmware/efi/efivars/
{% endhighlight %}

![Arch Post Install: EFI Variables][024-efivars-01]

It said that,
`systemd` would automatically mount this efivars.
But I alos find this folder moount in my `openrc`.

{% highlight bash %}
❯# mount | grep efivars
efivarfs on /sys/firmware/efi/efivars type efivarfs (rw,nosuid,nodev,noexec,relatime)
{% endhighlight %}

![Arch Post Install: EFI Variables][024-efivars-02]

The variables list can be obtain here

{% highlight bash %}
❯# efivar --list
{% endhighlight %}

![Arch Post Install: EFI Variables][024-efivars-03]

then we can query the variable content by this command.

{% highlight bash %}
❯ efivar -p -n 8be4df61-93ca-11d2-aa0d-00e098032b8c-Boot0000
GUID: 8be4df61-93ca-11d2-aa0d-00e098032b8c
Name: "Boot0000"
Attributes:
  Non-Volatile
  Boot Service Access
  Runtime Service Access
Value:
00000000  01 00 00 00 5e 00 47 00  52 00 55 00 42 00 00 00  |....^.G.R.U.B...|
00000010  04 01 2a 00 01 00 00 00  00 08 00 00 00 00 00 00  |..*.............|
00000020  00 20 08 00 00 00 00 00  72 f4 6f d0 a1 d7 17 46  |. ......r.o....F|
00000030  b4 8e c6 b2 ef f6 fe 8d  02 02 04 04 30 00 5c 00  |............0.\.|
00000040  45 00 46 00 49 00 5c 00  47 00 52 00 55 00 42 00  |E.F.I.\.G.R.U.B.|
00000050  5c 00 67 00 72 00 75 00  62 00 78 00 36 00 34 00  |\.g.r.u.b.x.6.4.|
00000060  2e 00 65 00 66 00 69 00  00 00 7f ff 04 00        |..e.f.i.......  |
{% endhighlight %}

![Arch Post Install: EFI Variable: Print by Name][024-efivar-name]


-- -- --

<a name="fstab"></a>

### fstab

I have prepared a few partitions to work coexist with my arch linux.

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

![Arch Post Install: lsblk][025-lsblk]

I can get the UUID by using `ls` command.

{% highlight bash %}
❯ ls -l /dev/disk/by-uuid --color=never
{% endhighlight %}

or better

{% highlight bash %}
❯ exa -T /dev/disk/by-uuid --color=never
/dev/disk/by-uuid
├── 01D90E3F2148D4D0 -> ../../nvme0n1p4
├── 3E7D-B1CB -> ../../nvme0n1p1
├── 6d64b51a-155e-4c9e-9a3c-d19abebc5126 -> ../../nvme0n1p7
├── 9E467E96467E6EC1 -> ../../nvme0n1p8
├── 705E7E355E7DF3E6 -> ../../nvme0n1p3
├── b125fe63-4ae8-4228-9e6b-9475081dce86 -> ../../nvme0n1p5
└── e736a5e7-d810-47a8-b6a9-8bc66421eebb -> ../../nvme0n1p6
{% endhighlight %}

![Arch Post Install: Get Disk UUID][[025-exa-uuid]

So I can setup my `/etc/fstab`.

{% highlight bash %}
# <file system> <dir> <type> <options> <dump> <pass>

# /dev/nvme0n1p7
UUID=6d64b51a-155e-4c9e-9a3c-d19abebc5126	/         	ext4      	rw,relatime	0 1

# /dev/nvme0n1p1 LABEL=SYSTEM_DRV
UUID=3E7D-B1CB      	/boot/efi 	vfat      	umask=0077 0 2

# /dev/nvme0n1p5
UUID=b125fe63-4ae8-4228-9e6b-9475081dce86	swap        swap    defaults,noatime 0 0

# /dev/nvme0n1p3
UUID=705E7E355E7DF3E6                     /media/System  ntfs-3g defaults,x-systemd.automount,noauto,locale=en_US.UTF-8 0 0

# /dev/nvme0n1p4
UUID=01D90E3F2148D4D0                     /media/Docs    ntfs-3g defaults,x-systemd.automount,locale=en_US.UTF-8 0 0

# /dev/nvme0n1p6
UUID=e736a5e7-d810-47a8-b6a9-8bc66421eebb /media/Works   ext4    defaults,users,exec 0 2

tmpfs                                     /tmp           tmpfs   defaults,noatime,mode=1777 0 0
{% endhighlight %}

![Arch Post Install: /etc/fstab][[025-vim-fstab]

And finally get the nice output of the mount point.

{% highlight bash %}
❯ findmnt -su
TARGET SOURCE FSTYPE  OPTIONS
/      UUID=6d64b51a-155e-4c9e-9a3c-d19abebc5126
              ext4    rw,relatime
/boot/efi
       UUID=3E7D-B1CB
              vfat    umask=0077
swap   UUID=b125fe63-4ae8-4228-9e6b-9475081dce86
              swap    defaults,noatime
/media/System
       UUID=705E7E355E7DF3E6
              ntfs-3g defaults,x-systemd.automount,noauto,locale=en_US.UTF-8
/media/Docs
       UUID=01D90E3F2148D4D0
              ntfs-3g defaults,x-systemd.automount,locale=en_US.UTF-8
/media/Works
       UUID=e736a5e7-d810-47a8-b6a9-8bc66421eebb
              ext4    defaults,users,exec
/tmp   tmpfs  tmpfs   defaults,noatime,mode=1777
{% endhighlight %}

![Arch Post Install: findmnt -su][025-findmnt-su]

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

After all basic stuff done,
we can continue, installing application,
so we can get to work with our notebook in daily basis.

Consider continue reading [ [Arch: Application Install][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/04' %}

[local-whats-next]: /system/2023/04/09/arch-app-install.html

[017-grub-mkconfig]:{{ asset_path }}/017-grub-mkconfig.png
[017-grub-osprober]:{{ asset_path }}/017-grub-os-prober.png
[017-grub-uefi]:    {{ asset_path }}/017-grub-uefi.png
[017-grub-windows]: {{ asset_path }}/017-grub-windows.png

[024-efivars-01]:   {{ asset_path }}/024-efivars-01.png
[024-efivars-02]:   {{ asset_path }}/024-efivars-02.png
[024-efivars-03]:   {{ asset_path }}/024-efivars-03.png
[024-efivar-name]:  {{ asset_path }}/024-efivar-name.png

[025-exa-uuid]:     {{ asset_path }}/025-exa-uuid.png
[025-findmnt-su]:   {{ asset_path }}/025-findmnt-su.png
[025-lsblk]:        {{ asset_path }}/025-lsblk.png
[025-vim-fstab]:    {{ asset_path }}/025-vim-fstab.png