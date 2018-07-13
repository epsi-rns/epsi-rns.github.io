---
layout: post
title:  "Linux Multiboot - chroot"
date:   2018-05-23 09:25:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.
  Updating other linux in chroot environment.
  
---

{% include post/2018/05/toc-multiboot.html %}

### Overview

> Goal: Updating linux in chroot environment.

One advantange of multiboot is that you can update other OS,
while you are still working with current OS.

Here I use openSUSE as a primary OS,
while updating other OS in chroot environment

These three OS below share very similar chroot method.
There are only minor differences.

*	Debian

*	Fedora

*	KaOSx

-- -- --

### Debian

Just use normal chroot method.
The only issue is **resolv.conf** symbolic link.

#### chroot

Do this sequence of command, to do chroot:

{% highlight conf %}
% mount /media/Debian

% sudo mount --rbind  /dev /media/Debian/dev
% sudo mount --make-rslave /media/Debian/dev
% sudo mount -t proc /proc /media/Debian/proc
% sudo mount --rbind  /sys /media/Debian/sys
% sudo mount --make-rslave /media/Debian/sys
% sudo mount --rbind  /tmp /media/Debian/tmp 

% sudo chroot /media/Debian
{% endhighlight %}

I adapt the command above from Gentoo manual.

![chroot: Debian: chroot][image-ss-debian-chroot]{: .img-responsive }

This will take you to Debian root.

#### Dio Putra suggestion

My friend, Dio Putra that has enycypted Debian partition, 
private message me, and gave me this suggestion.

*	<code>mount -o bind</code> instead of <code>mount --rbind</code>,
	This rbind make this hard to be unmounted.

*	<code>umount -R</code> instead of <code>umount -f</code>

*	No need to bind <code>/tmp</code>


#### Rename resolv.conf

To enable internat access, we must have **/etc/resolv.conf**.
We need to get rid of the original symlink, and replace with a new one.

	/etc/resolv.conf -> /run/NetworkManager/resolv.conf

{% highlight conf %}
% sudo mv /media/Debian/etc/resolv.conf /media/Debian/etc/resolv.conf.symlink
% sudo cp -L /etc/resolv.conf /media/Debian/etc/
{% endhighlight %}

![chroot: Debian: rename /etc/resolv.conf][image-ss-debian-rename]{: .img-responsive }

There is this other method using fstab.

*	[slackware chroot](https://docs.slackware.com/howtos:general_admin:setting_up_a_slackware_chroot)

{% highlight conf %}
/etc/resolv.conf chroot_folder/etc/resolv.conf none bind,auto 0 0
{% endhighlight %}

#### Update

{% highlight conf %}
# mount -a
# apt update
# apt upgrade
{% endhighlight %}

![chroot: Debian: APT update][image-ss-debian-update]{: .img-responsive }

#### Restore resolv.conf

Do not forget to restore the original **/etc/resolv.conf**

{% highlight conf %}
% sudo rm /media/Debian/etc/resolv.conf
% sudo mv /media/Debian/etc/resolv.conf.symlink /media/Debian/etc/resolv.conf
{% endhighlight %}

![chroot: Debian: restore /etc/resolv.conf][image-ss-debian-restore]{: .img-responsive }

#### Post chroot

Just exit, you will be back to original user prompt.

{% highlight conf %}
# exit
{% endhighlight %}

Unmount all

{% highlight conf %}
% sudo umount /media/Debian/dev
% sudo umount /media/Debian/proc
% sudo umount /media/Debian/sys
% sudo umount /media/Debian/tmp 
{% endhighlight %}

Sometimes the volume is busy,
you need the <code>-f</code> argument.

{% highlight conf %}
umount: /media/Debian/dev: target is busy
        (In some cases useful info about processes that
         use the device is found by lsof(8) or fuser(1).)
{% endhighlight %}

Sometimes it can't be unmount.

-- -- --

### Fedora

Just use normal chroot method.
The only issue is **resolv.conf** symbolic link.

#### chroot

Do this sequence of command, to do chroot:

{% highlight conf %}
% mount /media/Fedora

% sudo mount --rbind  /dev /media/Fedora/dev
% sudo mount --make-rslave /media/Fedora/dev
% sudo mount -t proc /proc /media/Fedora/proc
% sudo mount --rbind  /sys /media/Fedora/sys
% sudo mount --make-rslave /media/Fedora/sys
% sudo mount --rbind  /tmp /media/Fedora/tmp 

% sudo chroot /media/Fedora
{% endhighlight %}

This will take you to Fedora root.

{% highlight conf %}
# mount -a
# dnf refresh
{% endhighlight %}

#### Rename resolv.conf

But there is an issue that we have to solve with Fedora.
There is no internet access, because **/etc/resolv.conf** does not exist.
Normally we just need to copy the file, but it cannot be copied because
it is a symlink to **/var/run/NetworkManager/resolv.conf** that does not exit.

	/etc/resolv.conf -> /var/run/NetworkManager/resolv.conf

![chroot: Fedora: resolv][image-ss-fedora-resolv]{: .img-responsive }

To solve this, we just need to rename

{% highlight conf %}
# mv /etc/resolv.conf /etc/resolv.conf.symlink
{% endhighlight %}

Open other terminal, and now you can copy.

{% highlight conf %}
% sudo cp -L /etc/resolv.conf /media/Fedora/etc/
{% endhighlight %}

#### Update

{% highlight conf %}
# dnf refresh
# dnf update
{% endhighlight %}

![chroot: Fedora: DNF update][image-ss-fedora-update]{: .img-responsive }

#### Restore resolv.conf

Do not forget to restore the original **/etc/resolv.conf**

{% highlight conf %}
# rm /etc/resolv.conf
rm: remove regular file '/etc/resolv.conf'? y

# mv /etc/resolv.conf.symlink /etc/resolv.conf
{% endhighlight %}

#### Post chroot

Just exit, you will be back to original user prompt.

{% highlight conf %}
# exit
{% endhighlight %}

Unmount all

{% highlight conf %}
% sudo umount /media/Fedora/dev
% sudo umount /media/Fedora/proc
% sudo umount /media/Fedora/sys
% sudo umount /media/Fedora/tmp 
{% endhighlight %}

Sometimes the volume is busy,
you need the <code>-f</code> argument.

-- -- --

### KaOSx

Just use normal chroot method.
No issue at all.

#### chroot

Do this sequence of command, to do chroot:

{% highlight conf %}
% mount /media/KaOSx

% sudo mount --rbind  /dev /media/KaOSx/dev
% sudo mount --make-rslave /media/KaOSx/dev
% sudo mount -t proc /proc /media/KaOSx/proc
% sudo mount --rbind  /sys /media/KaOSx/sys
% sudo mount --make-rslave /media/KaOSx/sys
% sudo mount --rbind  /tmp /media/KaOSx/tmp 

% sudo chroot /media/KaOSx
{% endhighlight %}

I adapt the command above from Gentoo manual.

![chroot: KaOSx: chroot][image-ss-kaosx-chroot]{: .img-responsive }

This will take you to KaOSx root.

#### Copy resolv.conf

To enable internat access, we must have **/etc/resolv.conf**.

{% highlight conf %}
% sudo cp -L /etc/resolv.conf /media/KaOSx/etc/
{% endhighlight %}

![chroot: KaOSx: rename /etc/resolv.conf][image-ss-kaosx-resolv]{: .img-responsive }

No need to restore anything later.

#### Update

{% highlight conf %}
# pacman -Syu
{% endhighlight %}

![chroot: KaOSx: pacman -Syu][image-ss-kaosx-update]{: .img-responsive }

#### Post chroot

Just exit, you will be back to original user prompt.

{% highlight conf %}
# exit
{% endhighlight %}

Unmount all

{% highlight conf %}
% sudo umount /media/KaOSx/dev
% sudo umount /media/KaOSx/proc
% sudo umount /media/KaOSx/sys
% sudo umount /media/KaOSx/tmp 
{% endhighlight %}

![chroot: KaOSx: umount][image-ss-kaosx-umount]{: .img-responsive }

Sometimes the volume is busy,
you need the <code>-f</code> argument.
Sometimes it can't be unmount.

-- -- --

### Miscellanous

#### Reading

*	[https://wiki.archlinux.org/index.php/change_root](https://wiki.archlinux.org/index.php/change_root)

#### arch-chroot

If you are in Ubuntu, you are lucky,
that Ubuntu support arch-install-scripts.

#### Tools

*	Bhaskar Chowdhury: 
	[github.com/.../change_os_root](https://github.com/unixbhaskar/AdminScripts/blob/master/change_os_root)

-- -- --

### What's next

Consider continue reading [ [Network: Samba][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/multiboot/pc-01' %}

[local-part-config]:       /system/2018/05/25/linux-samba.html

[dotfiles-multiboot]:      {{ dotfiles }}

[image-ss-fedora-update]:  {{ asset_path }}/fedora-chroot-dnf-update.png
[image-ss-fedora-resolv]:  {{ asset_path }}/fedora-etc-resolv.png

[image-ss-debian-chroot]:  {{ asset_path }}/debian-chroot.png
[image-ss-debian-update]:  {{ asset_path }}/debian-chroot-apt-update.png
[image-ss-debian-upgrade]: {{ asset_path }}/debian-chroot-apt-upgrade.png
[image-ss-debian-rename]:  {{ asset_path }}/debian-resolv-rename.png
[image-ss-debian-restore]: {{ asset_path }}/debian-resolv-restore.png

[image-ss-kaosx-chroot]:   {{ asset_path }}/kaosx-chroot.png
[image-ss-kaosx-update]:   {{ asset_path }}/kaosx-pacman-syu.png
[image-ss-kaosx-resolv]:   {{ asset_path }}/kaosx-resolv-copy.png
[image-ss-kaosx-umount]:   {{ asset_path }}/kaosx-umount.png
