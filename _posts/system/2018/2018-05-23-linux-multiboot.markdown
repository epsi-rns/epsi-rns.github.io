---
layout: post
title:  "Linux Multiboot - chroot"
date:   2018-05-23 09:25:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.

related_link_ids: 
  - 14010246  # Debian Install
  - 14040246  # Arch Install

---

### Overview

One andvatange of multiboot is that you can update other OS,
while you are still working with current OS.

Here I use openSUSE as a primary OS,
while updating other OS.

-- -- --

### Debian

Just use normal chroot method.
The only issue is resolv.conf symbolic link.

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

![chroot: Debian: chroot][image-ss-debian-chroot]{: .img-responsive }

This will take you to Debian root.

#### Rename resolv.conf

To enable internat access, we must have **/etc/resolv.conf**.
We need to get rid of the original symlink, and replace with a new one.

	/etc/resolv.conf -> /run/NetworkManager/resolv.conf

{% highlight conf %}
% sudo mv /media/Debian/etc/resolv.conf /media/Debian/etc/resolv.conf.symlink
% sudo cp -L /etc/resolv.conf /media/Debian/etc/
{% endhighlight %}

![chroot: Debian: /etc/resolv.conf][image-ss-debian-resolv]{: .img-responsive }

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

-- -- --

### Fedora

Just use normal chroot method.
The only issue is resolv.conf symbolic link.

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

### What's next

Samba ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/multiboot/pc-01' %}

[dotfiles-multiboot]:      {{ dotfiles }}

[image-ss-fedora-update]:  {{ asset_path }}/fedora-chroot-dnf-update.png
[image-ss-fedora-resolv]:  {{ asset_path }}/fedora-etc-resolv.png

[image-ss-debian-chroot]:  {{ asset_path }}//debian-chroot.png
[image-ss-debian-update]:  {{ asset_path }}//debian-chroot-apt-update.png
[image-ss-debian-upgrade]: {{ asset_path }}//debian-chroot-apt-upgrade.png
[image-ss-debian-resolv]:  {{ asset_path }}//debian-etc-resolv.png
