---
layout: post
title: "Distribution - openSUSE Tumbleweed First Time Install"
date      : 2017-08-03 09:45:15 +0700
categories: system
tags      : [opensuse, distro, package manager]
keywords  : [zypper, tumbleweed]
author: epsi

opengraph:
  image: /assets/site/images/topics/opensuse.png

excerpt:
  openSUSE Tumbleweed Post Install Log.

related_link_ids: 
  - 17083145  # Docker Package Manager Summary
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration
  - 17080645  # Mageia 6 Upgrade
  - 17080545  # GRUB2 BTRFS Support
  - 17080245  # Fedora 23 to 26 Install
  - 17080145  # Manjaro OpenRC Issues

---

### Preface

	Tumbleweed install, using USB Live Plasma, was succeed.

Still on July.
I finally get a chance to experience OpenSUSE. 
I choose the rolling release caled <code>Tumbleweed</code>.
And I plan to use it for few years in my Office.
This is my third Linux Partition after Debian and Fedora in my PC in my Office.

### First Impression

Surprisingly the installment was succeed. There are few things in my experience.

*	Each install, download a huge amount from the internet, about 4 Gigabytes.

*	My bad, I mistakenly install i586 version instead of x86_64 version.
	So I have to redownload the USB stick installer,
	and also download aout 4GB installation package.

*	My first install using i586, I skipped network configuration.
	And that confused me while I configure later.
	But it fixed using YaST while doing x86_64 install.

*	Using nomodeset for my PC, or  blinking screen.	

This openSUSE utilize <code>BTRFS</code> in <code>root /</code>,
and <code>XFS</code> at <code>/home</code>.
I'm very happy that I finally get my first non <code>ext4</code>,
but I haven't got any time to explore them yet.

[![Tumbleweed YaST Install][image-ss-yast-install]{: .img-responsive }][photo-ss-yast-install]

openSUSE installation using <code>YaST</code> as shown in figure above.
I haven't explore <code>YaST</code> yet.

One thing for sure while install.
No need to touch command line.

-- -- --

### Post Install

#### Repository Check

{% highlight bash %}
$ zypper lr -r
Repository priorities are without effect. All enabled repositories share the same priority.

# | Alias               | Name                        | Enabled | GPG Check | Refresh
--+---------------------+-----------------------------+---------+-----------+--------
1 | openSUSE-20170721-0 | openSUSE-20170721-0         | Yes     | (r ) Yes  | Yes    
2 | repo-debug          | openSUSE-Tumbleweed-Debug   | No      | ----      | ----   
3 | repo-non-oss        | openSUSE-Tumbleweed-Non-Oss | Yes     | (r ) Yes  | Yes    
4 | repo-source         | openSUSE-Tumbleweed-Source  | No      | ----      | ----   
5 | repo-update         | openSUSE-Tumbleweed-Update  | Yes     | (r ) Yes  | Yes   
{% endhighlight %}

#### Playing with Package

{% highlight bash %}
$ sudo zypper up
$ sudo zypper in clementine
$ sudo zypper in vlc
$ sudo zypper in mpv
$ sudo zypper in gstreamer-plugins-ugly
$ sudo zypper in ffmpeg

Codec not supported:
VLC could not decode the format "h264" (H264 - MPEG-4 AVC (part 10))
Codec not supported:
VLC could not decode the format "mp4a" (MPEG AAC Audio)
{% endhighlight %}

It seems to be that it need <code>packman</code> repository.

#### Enable Packman repository

{% highlight bash %}
$ sudo zypper ar -f -n packman http://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Tumbleweed/ packman
{% endhighlight %}

And finally get it installed

{% highlight bash %}
$ sudo zypper in vlc-codecs
$ sudo zypper in mplayer
$ sudo zypper in smplayer
$ sudo zypper install k3b-codecs ffmpeg lame phonon-backend-vlc phonon4qt5-backend-vlc vlc-codecs
{% endhighlight %}

#### More Package

{% highlight bash %}
$ sudo zypper in htop fish powerline git rfkill sddm inkscape
{% endhighlight %}

It all done successfully.

-- -- --

### Zypper

Zypper use abbreviation, to make the command shorter. 

*	repos (lr) 

*	install (in)

*	update (up)

*	dist-upgrade (dup)

For more information use the manual and type <code>/</code> to search.

{% highlight bash %}
$ man zypper
{% endhighlight %}

I'm doing <code>zypper dup</code> in tumbleweed as suggested from the community.
It seems interesting that zypper ask user about license.
I respect license, so I type yes.

{% highlight bash %}
$ sudo zypper dup
Warning: ...
...
Loading repository data...
Reading installed packages...
Computing distribution upgrade...

The following 26 NEW packages are going to be installed:
  ...

The following 75 applications are going to be REMOVED:
  ...
                                                                                                       
The following 8 packages are going to be REMOVED:                                                        
  ...
                                                                                                           
The following 653 packages are going to be upgraded:                                                       
  ...
{% endhighlight %}

This is a screenshot for anyone who curious about openSUSE. 
After about a week leaving tumbleweed installation in peace
and suddenly running <code>zypper dup</code>.

Please click for bigger figure.
[![Zypper Distribution Upgrade][image-ss-zypper-dup]{: .img-responsive }][photo-ss-zypper-dup]

I found something interesting that zypper have. 
I have nerver seen this in APT nor ALPM nor DNF nor Portage nor Slackpkg.

{% highlight bash %}
$ zypper ps - s
{% endhighlight %}

Please click for bigger figure.
[![Zypper Process][image-ss-zypper-ps]{: .img-responsive }][photo-ss-zypper-ps]

-- -- --

### BTRFS

I have never used BTRFS.
For that reason, this <code>/etc/fstab</code> looks interesting.

{% highlight conf %}
UUID=23342d48-c3be-402c-b049-b3e9ddeafbc0 /boot                ext4       errors=remount-ro     0 1
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /                    btrfs      defaults              0 0
UUID=0095473d-ae63-4722-8350-f5716e5df333 /home                xfs        defaults              1 2
UUID=b623e30b-c7a5-4f99-a250-45372da4c5b4 swap                 swap       defaults              0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /boot/grub2/i386-pc  btrfs      subvol=@/boot/grub2/i386-pc 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /boot/grub2/x86_64-efi btrfs      subvol=@/boot/grub2/x86_64-efi 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /opt                 btrfs      subvol=@/opt          0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /srv                 btrfs      subvol=@/srv          0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /tmp                 btrfs      subvol=@/tmp          0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /usr/local           btrfs      subvol=@/usr/local    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/cache           btrfs      subvol=@/var/cache    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/crash           btrfs      subvol=@/var/crash    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/libvirt/images btrfs      subvol=@/var/lib/libvirt/images 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/machines    btrfs      subvol=@/var/lib/machines 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/mailman     btrfs      subvol=@/var/lib/mailman 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/mariadb     btrfs      subvol=@/var/lib/mariadb 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/mysql       btrfs      subvol=@/var/lib/mysql 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/named       btrfs      subvol=@/var/lib/named 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/lib/pgsql       btrfs      subvol=@/var/lib/pgsql 0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/log             btrfs      subvol=@/var/log      0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/opt             btrfs      subvol=@/var/opt      0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/spool           btrfs      subvol=@/var/spool    0 0
UUID=c114d95e-bc0a-4b41-a2db-abd21aa9850f /var/tmp             btrfs      subvol=@/var/tmp      0 0
{% endhighlight %}

-- -- --

#### Update

	First Thing First

{% highlight bash %}
$ sudo zypper up
{% endhighlight %}

or

{% highlight bash %}
$ sudo zypper dup
{% endhighlight %}

You can see the difference below.
Please click for bigger figure.

[![Zypper Up Dup Difference][image-ss-zypper-up-dup]{: .img-responsive }][photo-ss-zypper-up-dup]

-- -- --

### Rescue

Three Weeks later after I wrote this post,
I messed up my partition using Windows Easeus. 
Since openSUSE use BTRFS,
I have to download Tumbleweed Live all over again,
and use <code>dd</code> to write to flash disk.
Then using <code>nomodeset</code> to boot.
And <code>chroot</code> before doing both
<code>grub2-mkconfig</code> and <code>grub2-install</code>.
Finally I have all my partition back again,
but five minutes later my area has poweroutage issue.
For that reason I decide to get some rest, instead of working.

Guidance

*	<https://www.suse.com/support/kb/doc/?id=7018126>

First I have to make sure,
that this is the right partition by <code>os-release</code>

{% highlight bash %}
$ mount /dev/sda7 /mnt
$ cat /mnt/etc/os-release
{% endhighlight %}

And mount the rest

{% highlight bash %}
$ mount --rbind /proc /mnt/proc
$ mount --rbind /sys /mnt/sys
$ mount --rbind /dev /mnt/dev
{% endhighlight %}

And get my <code>ext4</code> partition to <code>/boot</code>.

{% highlight bash %}
$ chroot /mnt
$ mount -a
{% endhighlight %}

And fix grub.

All done except the power outage.

-- -- -- 

### Conclusion

I finally happy using openSUSE.
Better late than never.

I must admit I know nothing about 
<code>YaST</code> nor <code>zypper</code> nor <code>BTRFS</code>.
I need time to experience these interesting stuff.

Thank you for reading

-- -- -- 

### Post Scriptum

I have decide to make be a separate article about zypper using docker.
Focusing on package manager in command line rather than installation in GUI.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/distro-opensuse' %}

[image-ss-yast-install]:    {{ asset_post }}/yast-install-i586.png
[photo-ss-yast-install]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNQ6K8rEv1WPht6zwJhInaGEifO_TC-eAJV3Fyk?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-zypper-dup]:      {{ asset_post }}/zypper-dup.png
[photo-ss-zypper-dup]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMjKKoThrBZPC64w6dKm3qKZedQnSRLz5KhcHHj?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-zypper-ps]:       {{ asset_post }}/zypper-ps-narrow.png
[photo-ss-zypper-ps]:       https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOKr8Czn6fxqgrCQuVqs8qaRals97xHRlqlkHS-?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-zypper-up-dup]:   {{ asset_post }}/zypper-up-dup-difference.png
[photo-ss-zypper-up-dup]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP3zbkffQJopJj2Y7I2GaTZuuxjZNKou3qJss2t?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
