---
layout: post
title:  "Linux Multiboot - /etc/fstab"
date:   2018-05-22 09:25:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.
  Thorough /etc/fstab/ example.

---

{% include post/2018/05/toc-multiboot.html %}

### Overview

> Goal: Thorough /etc/fstab/ example

There are three parts of mount point on **fstab**,
that we are going to talk about.

*	Initial: clean install: root and swap

*	Common: shared partition, and windows data partititon

*	Custom: access to other OS

	*	OpenSUSE: access to: Fedora, Debian, KaOSx.

	*	Fedora: access to: openSUSE, Debian, KaOSx.

	*	Debian: access to: openSUSE, Fedora, KaOSx.

	*	KaOSx: access to: openSUSE, Fedora, Debian.

#### Device Name and UUID

There are two approach of setting mount point

*	Using device path: e.g /dev/sda9

*	Using UUID: e.g c114d95e-bc0a-4b41-a2db-abd21aa9850f

#### List Block Device

We are going to use **lsblk**,
to have a listing of our beloved block device .

{% highlight conf %}
% lsblk   
NAME    MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
sda       8:0    0 465.8G  0 disk 
├─sda1    8:1    0 131.9G  0 part /media/System
├─sda2    8:2    0     1K  0 part 
├─sda5    8:5    0  57.7G  0 part /media/Works
├─sda6    8:6    0  66.4G  0 part /media/Docs
├─sda7    8:7    0   999M  0 part /media/Boot
├─sda8    8:8    0   9.9G  0 part /media/Fun
├─sda9    8:9    0  49.1G  0 part /media/openSUSE
├─sda10   8:10   0  49.1G  0 part /media/KaOSx
├─sda11   8:11   0  49.1G  0 part /
├─sda12   8:12   0  49.1G  0 part /media/Debian
└─sda13   8:13   0   2.5G  0 part [SWAP]
{% endhighlight %}

![Multiboot: custom: lsblk][image-ss-lsblk-custom]{: .img-responsive }

#### Dotfiles Document

Config is available at:

* [github.com/epsi-rns/dotfiles/.../multiboot][dotfiles-multiboot]

-- -- --

### Using Device Path

Since openSUSE with BTRFS is complex,
I'm going to start with Fedora.

Before we use UUID, I start with device name.
It is easier to read **/dev/sda9**
than read **c114d95e-bc0a-4b41-a2db-abd21aa9850f**.

#### Manual Page

This is where to get help.

{% highlight conf %}
$ man fstab
{% endhighlight %}

![Multiboot: man fstab][image-ss-man-fstab]{: .img-responsive }

#### Initial

Depend on your installation,
your minimum fstab contain only this two entry.

{% highlight conf %}
# Fedora

/dev/sda11   /                 ext4      defaults                                1       1
/dev/sda13   none              swap      swap                                    0       0
{% endhighlight %}

![Multiboot: initial: lsblk -o][image-ss-lsblk-o-initial]{: .img-responsive }

#### Common

Add shared Partition.
And this Windows is common to all linux.

{% highlight conf %}
### Common

/dev/sda1    /media/System     ntfs-3g   defaults,noauto,locale=en_US.UTF-8      0       0
/dev/sda6    /media/Docs       ntfs-3g   defaults,locale=en_US.UTF-8             0       0
/dev/sda5    /media/Works      ext4      defaults,users,exec                     0       2
{% endhighlight %}

![Multiboot: common: lsblk -o][image-ss-lsblk-o-common]{: .img-responsive }

#### Custom Access

How about the rest.

{% highlight conf %}
### Other Distribution

/dev/sda10   /media/KaOSx      xfs       defaults,noauto,users                   0       0
/dev/sda12   /media/Debian     ext4      defaults,noauto,users                   0       0

### OpenSUSE

/dev/sda9    /media/openSUSE   btrfs     defaults,noauto,users                   0       0
/dev/sda7    /media/Boot       ext4      defaults,noauto,user,errors=remount-ro  0       0
/dev/sda8    /media/Fun        xfs       defaults,noauto,users                   0       0
{% endhighlight %}

![Multiboot: custom: lsblk -o][image-ss-lsblk-o-custom]{: .img-responsive }

And the result of **lsblk -o** is:

{% highlight conf %}
% lsblk -o NAME,FSTYPE,LABEL,MOUNTPOINT
NAME    FSTYPE LABEL  MOUNTPOINT
sda                   
├─sda1  ntfs   System /media/System
├─sda2                
├─sda5  ext4   Works  /media/Works
├─sda6  ntfs   Docs   /media/Docs
├─sda7  ext4   boot   /media/Boot
├─sda8  xfs    Fun    /media/Fun
├─sda9  btrfs         /media/openSUSE
├─sda10 xfs    KaOSx  /media/KaOSx
├─sda11 ext4   Fedora /
├─sda12 ext4   Debian /media/Debian
└─sda13 swap          [SWAP]
{% endhighlight %}

#### Complete

And finally this is all.

{% highlight conf %}
# Fedora
# /etc/fstab

/dev/sda11   /                 ext4      defaults                                1       1
/dev/sda13   none              swap      swap                                    0       0

### Common

/dev/sda1    /media/System     ntfs-3g   defaults,noauto,locale=en_US.UTF-8      0       0
/dev/sda6    /media/Docs       ntfs-3g   defaults,locale=en_US.UTF-8             0       0
/dev/sda5    /media/Works      ext4      defaults,users,exec                     0       2

### Other Distribution

/dev/sda10   /media/KaOSx      xfs       defaults,noauto,users                   0       0
/dev/sda12   /media/Debian     ext4      defaults,noauto,users                   0       0

### OpenSUSE

/dev/sda9    /media/openSUSE   btrfs     defaults,noauto,users                   0       0
/dev/sda7    /media/Boot       ext4      defaults,noauto,user,errors=remount-ro  0       0
/dev/sda8    /media/Fun        xfs       defaults,noauto,users                   0       0
{% endhighlight %}

#### Additional entry

However, there are entry on the fourth field, like this one:

{% highlight conf %}
defaults,noauto,users 
{% endhighlight %}

You can read what it mean in **mount** manual page.

{% highlight conf %}
$ man 8 mount
{% endhighlight %}

![Multiboot: man mount][image-ss-man-mount]{: .img-responsive }

#### Shared partition

I have my shared partition at **/media/Works**

Except for working partition, I want to make my script executable.

{% highlight conf %}
defaults,users,exec
{% endhighlight %}

My working partition is going to be automatically mounted, at boot time.
Because I put my samba (network neighbourhood) directory over there.

-- -- --

### Using UUID

Now we are going to convert each device name such as */dev/sda9*,
to UUID such as **c114d95e-bc0a-4b41-a2db-abd21aa9850f**.
First, consider what the map in your PC.

{% highlight conf %}
% tree /dev/disk/by-uuid
/dev/disk/by-uuid
├── 0095473d-ae63-4722-8350-f5716e5df333 -> ../../sda8
├── 01CF01333AD2FF20 -> ../../sda6
├── 23342d48-c3be-402c-b049-b3e9ddeafbc0 -> ../../sda7
├── 4190d7a9-fb03-4d19-864f-7d04f89c3be0 -> ../../sda12
├── 442456A824569CAC -> ../../sda1
├── 50f93bc6-711d-4f70-84cf-09748e653543 -> ../../sda10
├── 954a9b1e-c8c0-4f38-8877-fa891c79c9ae -> ../../sda5
├── 9c76fb33-fa7a-46af-b2bb-f82d385b81b6 -> ../../sda11
├── b623e30b-c7a5-4f99-a250-45372da4c5b4 -> ../../sda13
└── c114d95e-bc0a-4b41-a2db-abd21aa9850f -> ../../sda9

0 directories, 10 files
{% endhighlight %}

![Partition: Disk by UUID][image-ss-by-uuid]{: .img-responsive }

#### Fedora

Still, I'm going to start with Fedora.
Clean Install without additional mount point.

Depend on your installation,
your minimum fstab contain only this two entries.

{% highlight conf %}
UUID=9c76fb33-fa7a-46af-b2bb-f82d385b81b6   /       ext4   defaults                         1       1
UUID=b623e30b-c7a5-4f99-a250-45372da4c5b4   none    swap    swap                            0       0
{% endhighlight %}

In a complete fashion, this would be:

{% highlight conf %}
# Fedora
#
# /etc/fstab
# Created by anaconda on Fri Jul 21 22:03:59 2017
#
# Accessible filesystems, by reference, are maintained under '/dev/disk'
# See man pages fstab(5), findfs(8), mount(8) and/or blkid(8) for more info
#

# / was on /dev/sda11 during installation
UUID=9c76fb33-fa7a-46af-b2bb-f82d385b81b6   /       ext4   defaults                         1       1

# swap was on /dev/sda13 during installation
UUID=b623e30b-c7a5-4f99-a250-45372da4c5b4   none    swap    swap                            0       0
{% endhighlight %}

#### mtab

Consider cross check the result with **/etc/mtab**.
Note that normally swap would not be shown.

{% highlight conf %}
% cat /etc/mtab | grep sda | column -t
/dev/sda11  /  ext4  rw,relatime,data=ordered  0  0
{% endhighlight %}

![Multiboot: initial mtab][image-ss-mtab-initial]{: .img-responsive }

#### Debian

In a same way, consider have a look at ext4 in Debian.

{% highlight conf %}
# / was on /dev/sda12 during installation
UUID=4190d7a9-fb03-4d19-864f-7d04f89c3be0   /       ext4     errors=remount-ro          0       1

# swap was on /dev/sda13 during installation
UUID=b623e30b-c7a5-4f99-a250-45372da4c5b4   none    swap    sw                          0       0

/dev/sr0          /media/cdrom0     udf,iso9660 user,noauto                             0       0
{% endhighlight %}

#### KaOSx

And, also have a look at XFS in KaOSx.

{% highlight conf %}
# / was on /dev/sda10 during installation
UUID=50f93bc6-711d-4f70-84cf-09748e653543 /         xfs     defaults,noatime            0       1

# swap was on /dev/sda13 during installation
UUID=b623e30b-c7a5-4f99-a250-45372da4c5b4 swap      swap    defaults,noatime            0       0
{% endhighlight %}

It is pretty much the same with XFS.

#### openSUSE

But very different with BTRFS in openSUSE.

{% highlight conf %}
# / was on /dev/sda9 during installation

UUID=23342d48-c3be-402c-b049-b3e9ddeafbc0 /boot                     ext4       errors=remount-ro     0 1
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /                         btrfs      defaults              0 0
UUID=0095473d-ae63-4722-8350-f5716e5df333 /home                     xfs        defaults              1 2
UUID=b623e30b-c7a5-4f99-a250-45372da4c5b4 swap                      swap       defaults              0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /boot/grub2/i386-pc       btrfs      subvol=@/boot/grub2/i386-pc 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /boot/grub2/x86_64-efi    btrfs      subvol=@/boot/grub2/x86_64-efi 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /opt                      btrfs      subvol=@/opt          0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /srv                      btrfs      subvol=@/srv          0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /tmp                      btrfs      subvol=@/tmp          0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /usr/local                btrfs      subvol=@/usr/local    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/cache                btrfs      subvol=@/var/cache    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/crash                btrfs      subvol=@/var/crash    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/libvirt/images   btrfs      subvol=@/var/lib/libvirt/images 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/machines         btrfs      subvol=@/var/lib/machines 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/mailman          btrfs      subvol=@/var/lib/mailman 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/mariadb          btrfs      subvol=@/var/lib/mariadb 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/mysql            btrfs      subvol=@/var/lib/mysql 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/named            btrfs      subvol=@/var/lib/named 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/pgsql            btrfs      subvol=@/var/lib/pgsql 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/log                  btrfs      subvol=@/var/log      0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/opt                  btrfs      subvol=@/var/opt      0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/spool                btrfs      subvol=@/var/spool    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/tmp                  btrfs      subvol=@/var/tmp      0 0
{% endhighlight %}

-- -- --

### Common Mount Point

No matter what your distribution,
You can just add this mount point to your **fstab**.

{% highlight conf %}
#Entry for /dev/sda1 (windows system):
UUID=442456A824569CAC                           /media/System       ntfs-3g     defaults,noauto,locale=en_US.UTF-8      0       0

#Entry for /dev/sda6 (windows data):
UUID=01CF01333AD2FF20                           /media/Docs         ntfs-3g     defaults,locale=en_US.UTF-8             0       0

#Entry for /dev/sda5 (shared partition) :
UUID=954a9b1e-c8c0-4f38-8877-fa891c79c9ae       /media/Works        ext4        defaults,users,exec                     0       2
{% endhighlight %}

I mean for every **fstab** in my system (Fedora, openSUSE, Debian, KaOSx).

#### mtab

Consider cross check the result with **/etc/mtab**.

![Multiboot: common mtab][image-ss-mtab-common]{: .img-responsive }

-- -- --

### Custom Mount Point

Now mount point to access other distribution.

#### Fedora

Still, start with Fedora.

Access: openSUSE, Debian, KaOSx.

{% highlight conf %}
#Entry for /dev/sda10 :
#/dev/sda10       /media/KaOSx      xfs         defaults,noauto,users                   0       0
UUID=50f93bc6-711d-4f70-84cf-09748e653543       /media/KaOSx        xfs         defaults,noauto,users                   0       0

#Entry for /dev/sda12 :
UUID=4190d7a9-fb03-4d19-864f-7d04f89c3be0       /media/Debian       ext4        defaults,noauto,users                   0       0

#Entry for /dev/sda9 (openSUSE root):
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f       /media/openSUSE     btrfs       defaults,noauto,users                   0       0

#Entry for /dev/sda7 (openSUSE boot):
UUID=23342d48-c3be-402c-b049-b3e9ddeafbc0       /media/Boot         ext4        defaults,noauto,user,errors=remount-ro  0       0

#Entry for /dev/sda8 (openSUSE home):
UUID=0095473d-ae63-4722-8350-f5716e5df333       /media/Fun          xfs         defaults,noauto,users                   0       0
{% endhighlight %}

Have a look at the <code class="code-file">/etc/fstab</code> config for Fedora:

*	[gitlab.com/.../dotfiles/.../fstab.fedora][dotfiles-fstab-fedora]


#### mtab

Again, consider cross check the result with **/etc/mtab**.

{% highlight conf %}
% cat /etc/mtab | grep sda | column -t
/dev/sda11  /                ext4     rw,relatime,data=ordered                                            0  0
/dev/sda5   /media/Works     ext4     rw,nosuid,nodev,relatime,data=ordered                               0  0
/dev/sda1   /media/System    fuseblk  rw,relatime,user_id=0,group_id=0,allow_other,blksize=4096           0  0
/dev/sda6   /media/Docs      fuseblk  rw,relatime,user_id=0,group_id=0,allow_other,blksize=4096           0  0
/dev/sda12  /media/Debian    ext4     rw,nosuid,nodev,noexec,relatime,data=ordered                        0  0
/dev/sda9   /media/openSUSE  btrfs    rw,nosuid,nodev,noexec,relatime,space_cache,subvolid=257,subvol=/@  0  0
/dev/sda10  /media/KaOSx     xfs      rw,nosuid,nodev,noexec,relatime,attr2,inode64,noquota               0  0
/dev/sda7   /media/Boot      ext4     rw,nosuid,nodev,noexec,relatime,errors=remount-ro,data=ordered      0  0
/dev/sda8   /media/Fun       xfs      rw,nosuid,nodev,noexec,relatime,attr2,inode64,noquota               0  0
{% endhighlight %}

![Multiboot: custom mtab][image-ss-mtab-custom]{: .img-responsive }

#### Debian

In a same way, we can do the same thing again with Debian.

Access: Fedora, openSUSE, KaOSx.

{% highlight conf %}
#Entry for /dev/sda10 :
#/dev/sda10       /media/KaOSx      xfs         defaults,noauto,users                   0       0
UUID=50f93bc6-711d-4f70-84cf-09748e653543       /media/KaOSx        xfs         defaults,noauto,users                   0       0

#Entry for /dev/sda11 :
UUID=9c76fb33-fa7a-46af-b2bb-f82d385b81b6       /media/Fedora       ext4        defaults,noauto,user,errors=remount-ro  0       0

#Entry for /dev/sda9 (openSUSE root):
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f       /media/openSUSE     btrfs       defaults,noauto,users                   0       0

#Entry for /dev/sda7 (openSUSE boot):
UUID=23342d48-c3be-402c-b049-b3e9ddeafbc0       /media/Boot         ext4        defaults,noauto,user,errors=remount-ro  0       0

#Entry for /dev/sda8 (openSUSE home):
UUID=0095473d-ae63-4722-8350-f5716e5df333       /media/Fun          xfs         defaults,noauto,users                   0       0
{% endhighlight %}

Have a look at the <code class="code-file">/etc/fstab</code> config for Debian:

*	[gitlab.com/.../dotfiles/.../fstab.debian][dotfiles-fstab-debian]

#### KaOSx

And so does KaOSx.

Access: Fedora, openSUSE, Debian.

{% highlight conf %}
#Entry for /dev/sda12 :
UUID=4190d7a9-fb03-4d19-864f-7d04f89c3be0       /media/Debian       ext4        defaults,noauto,users                   0       0

#Entry for /dev/sda11 :
UUID=9c76fb33-fa7a-46af-b2bb-f82d385b81b6       /media/Fedora       ext4        defaults,noauto,user,errors=remount-ro  0       0

#Entry for /dev/sda9 (openSUSE root):
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f       /media/openSUSE     btrfs       defaults,noauto,users                   0       0

#Entry for /dev/sda7 (openSUSE boot):
UUID=23342d48-c3be-402c-b049-b3e9ddeafbc0       /media/Boot         ext4        defaults,noauto,user,errors=remount-ro  0       0

#Entry for /dev/sda8 (openSUSE home):
UUID=0095473d-ae63-4722-8350-f5716e5df333       /media/Fun          xfs         defaults,noauto,users                   0       0
{% endhighlight %}

Have a look at the <code class="code-file">/etc/fstab</code> config for KaOSx:

*	[gitlab.com/.../dotfiles/.../fstab.kaosx][dotfiles-fstab-kaosx]

#### openSUSE

And also does openSUSE.

Access: Fedora, Debian, KaOSx.

{% highlight conf %}
### Other Distribution

#Entry for /dev/sda10 :
#/dev/sda10       /media/KaOSx      xfs         defaults,noauto,users                   0       0
UUID=50f93bc6-711d-4f70-84cf-09748e653543       /media/KaOSx        xfs         defaults,noauto,users                   0       0

#Entry for /dev/sda11 :
UUID=9c76fb33-fa7a-46af-b2bb-f82d385b81b6       /media/Fedora       ext4        defaults,noauto,user,errors=remount-ro  0       0

#Entry for /dev/sda12 :
UUID=4190d7a9-fb03-4d19-864f-7d04f89c3be0       /media/Debian       ext4        defaults,noauto,users                   0       0
{% endhighlight %}

Have a look at the <code class="code-file">/etc/fstab</code> config for openSUSE:

*	[gitlab.com/.../dotfiles/.../fstab.opensuse][dotfiles-fstab-opensuse]

#### GParted

Again with GUI, to have better understanding.

![Multiboot: gparted][image-ss-gparted]{: .img-responsive }

-- -- --

### What's next

Consider continue reading [ [Multiboot: chroot][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/multiboot/pc-01' %}

[local-part-config]:       /system/2018/05/23/linux-multiboot.html

[dotfiles-multiboot]:      {{ dotfiles }}
[dotfiles-fstab-debian]:   {{ dotfiles }}/fstab.debian
[dotfiles-fstab-fedora]:   {{ dotfiles }}/fstab.fedora
[dotfiles-fstab-opensuse]: {{ dotfiles }}/fstab.opensuse
[dotfiles-fstab-kaosx]:    {{ dotfiles }}/fstab.kaosx

[image-ss-gparted]:        {{ asset_path }}/opensuse-gparted.png
[image-ss-by-uuid]:        {{ asset_path }}/opensuse-disk-by-uuid.png
[image-ss-man-fstab]:      {{ asset_path }}/man-fstab.png
[image-ss-man-mount]:      {{ asset_path }}/man-mount.png

[image-ss-lsblk-common]:       {{ asset_path }}/fedora-lsblk-common.png
[image-ss-lsblk-custom]:       {{ asset_path }}/fedora-lsblk-custom.png
[image-ss-lsblk-initial]:      {{ asset_path }}/fedora-lsblk-initial.png
[image-ss-lsblk-o-common]:     {{ asset_path }}/fedora-lsblk-o-common.png
[image-ss-lsblk-o-custom]:     {{ asset_path }}/fedora-lsblk-o-custom.png
[image-ss-lsblk-o-initial]:    {{ asset_path }}/fedora-lsblk-o-initial.png
[image-ss-mtab-common]:        {{ asset_path }}/fedora-mtab-column-common.png
[image-ss-mtab-custom]:        {{ asset_path }}/fedora-mtab-column-custom.png
[image-ss-mtab-initial]:       {{ asset_path }}/fedora-mtab-column-initial.png
