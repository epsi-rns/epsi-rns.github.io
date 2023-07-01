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

* [What is Next?](#whats-next)

-- -- --

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

### fstab

/media/works

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

Form Installment, we can dive into post installment.

Consider continue reading [ [Arch: Post Install][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/04' %}

[local-whats-next]: /system/2023/04/05/arch-post-install.html

[017-grub-mkconfig]:{{ asset_path }}/017-grub-mkconfig.png
[017-grub-osprober]:{{ asset_path }}/017-grub-os-prober.png
[017-grub-uefi]:    {{ asset_path }}/017-grub-uefi.png
[017-grub-windows]: {{ asset_path }}/017-grub-windows.png

[024-efivars-01]:   {{ asset_path }}/024-efivars-01.png
[024-efivars-02]:   {{ asset_path }}/024-efivars-02.png
[024-efivars-03]:   {{ asset_path }}/024-efivars-03.png
[024-efivar-name]:  {{ asset_path }}/024-efivar-name.png