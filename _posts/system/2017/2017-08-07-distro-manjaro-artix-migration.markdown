---
layout: post
title: "Distribution - Manjaro OpenRC to Artix Migration"
date: 2017-08-07 09:45:15 +0700
categories: system
tags: [manjaro, artix, openrc, distro, package manager]
author: epsi

excerpt:
  Manjaro OpenRC to Artix, Migrate Log.

---

### Preface

	I did migration to Artix Linux just five days after the announcement.

The world spinning so fast.
Suddenly Manjaro-OpenRC become discontinued,
and morphing into new distro called Artix Linux.

*	<https://forum.manjaro.org/t/manjaro-openrc-will-be-discontinued/28387/204>

Artix Linux has stronger focus on init.
Not just OpenRC but other init as well such as runit and sheperd.

	The switch goes smoothly

After migation, Artix works well.
Just like Manjaro-OpenRC. I see no difference.

Except for these two things

*	Some package lagged behind upstream, such as Perl. 
	This normally fixed within few days.

*	Docker has not work yet. This is a kernel issue.
	I don't know when this will be fixed. This state is unknown. 
	But it is okay, I can wait.
	Note that other cloud such as LXC works well.

-- -- --

### Follow The Development

No do not follow the rabbit.
But follow the octocat github instead.

Artix Linux, formerly Manjaro-OpenRC.

*	<https://github.com/artix-linux>

*	<https://artix-linux.github.io/migrate/>

*	<https://sourceforge.net/projects/artix-linux/>

Not that I need spoonfeeding.
I just need to know more about this repository.

(1) Official Site

Jekyll: I understand, I have my own github dot io

*	<https://github.com/artix-linux/artix-linux.github.io>

(2) Repository

PKGBUILD: No clue

*	<https://github.com/artix-linux/system>

*	<https://github.com/artix-linux/world>

*	<https://github.com/artix-linux/galaxy>

Reference

*	<https://wiki.archlinux.org/index.php/PKGBUILD>

I guess it has soemthing to do with ABS to create repository automatically.

(3) Development and build tools by artoo

artools: No Clue

*	<https://github.com/artix-linux/artools>

I guess it is something cool from the official Manjaro-OpenRC maintainer.

*	<https://github.com/artix-linux/artools/blob/master/data/import.list.d/system-arch.list>

(4) Other Stuff

VCS-GIT: no clue
*	<https://github.com/artix-linux/artix-vcs>

### Cromnix

Also this Cromnix Repository.

*	<https://github.com/cromnix/>

Inside

*	<https://github.com/cromnix/packages>

*	<https://github.com/cromnix/cromnix-buildtools>

*	<https://github.com/cromnix/cromnix-utils>

*	<https://github.com/cromnix/openrc-services>


Official BBS Forum.

*	<https://cromnix.org>

Just my personal note.

-- -- --

### Migration Log

Guidance. **Please read this first**.
You should follow the official guidance.

*	<https://artix-linux.github.io/migrate/>

And this is from bash history.
With little differences.
This is my personal note.

{% highlight bash %}
# rmdir /etc/pacman.d/mirrorlist
# touch /etc/pacman.d/mirrorlist
# stat /etc/pacman.d/mirrorlist
# stat /etc/pacman.d/mirrorlist-arch 
{% endhighlight %}

{% highlight bash %}
# pacman -Scc && pacman -Syy

# pacman -S artix-keyring
# pacman-key --populate cromnix

# pacman -Sw artix-keyring
# pacman -U /var/cache/pacman/pkg/artix-keyring-20170619-1-any.pkg.tar.xz 
# pacman-key --populate cromnix
{% endhighlight %}

[![Artix Keyring][image-ss-artix-keyring]{: .img-responsive }][photo-ss-artix-keyring]

{% highlight bash %}
# pacman -Rdd manjaro-system
# pacman -Rsc manjaro-tools-base manjaro-system mhwd mhwd-db manjaro-firmware manjaro-settings-manager intel-ucode lsb-release rpcbind-openrc
# pacman -Rsc mhwd mhwd-db manjaro-firmware manjaro-settings-manager lsb-release 
{% endhighlight %}

[![Remove Manjaro][image-ss-manjaro-remove]{: .img-responsive }][photo-ss-manjaro-remove]

{% highlight bash %}
# pacman -Rdd sysvinit udev-openrc consolekit consolekit-openrc
# pacman -Rdd sysvinit 
# pacman -S --asdeps systemd-dummy libsystemd-dummy
{% endhighlight %}

[![systemd Dummy][image-ss-systemd-dummy]{: .img-responsive }][photo-ss-systemd-dummy]

I have Perl dependency problem, different with guidance.

{% highlight bash %}
# pacman -Su base base-devel openrc-system grub linux-lts linux-lts-headers
# pacman -R udisks2 gvfs kdelibs solid udiskie
# pacman -R mariadb net-snmp
# pacman -R mariadb net-snmp mysql-openrc hplip akonadi-qt4
# pacman -R perl-clone perl-dbi perl-net-dbus perl-text-iconv perl-xml-libxml perl-xml-parser perl-xml-twig intltool foomatic-db-engine python-distutils-extra python2-distutils-extra oblogout
# pacman -R imagemagick lvm2-nosystemd dhcpcd-nosystemd cups-filters inkscape lvm2-openrc dhcpcd-openrc cups-nosystemd cups-openrc
# pacman -Su base base-devel openrc-system grub linux-lts linux-lts-headers
{% endhighlight %}

Continue using the guidance.

{% highlight bash %}
# pacman -Qg base base-devel | awk '{print $2}' | sort | uniq > installed
# pacman -Sg base base-devel | awk '{print $2}' | sort | uniq >| groups
# cat installed 
# cat groups 
# pacman -S `comm -2 installed groups`
# pacman -R tcp_wrappers mdm
{% endhighlight %}

{% highlight bash %}
# for p in `pacman -Qq|grep nosystemd`; do pacman -S `sed s/-nosystemd// <<<$p`; done
# pacman -Qq|grep nosystemd

# for p in `pacman -Qq|grep elogind`; do pacman -S `sed s/-elogind// <<<$p`; done
# pacman -Qq|grep elogind
{% endhighlight %}

{% highlight bash %}
# pacman -Rsdd systemd-sysusers
# pacman -Su
# pacman -S --needed opensysusers
{% endhighlight %}

{% highlight bash %}
# rc-update add udev boot
# rc-update del elogind default
# rc-update add elogind boot
# rc-update add dbus default
{% endhighlight %}

{% highlight bash %}
# mkinitcpio -p linux-lts
# mkinitcpio -p linux314
# update-grub
{% endhighlight %}

{% highlight bash %}
# pacman -S sddm-elogind sddm-qt-manjaro-theme sddm-kcm sddm-andromeda-qt-theme plasma-meta
{% endhighlight %}

	Now it works well sir ! After reboot still well.

-- -- --

### Screenshot: Emerge in LXC

OS: Artix Linux (formerly Manjaro-OpenRC)

*	Window Manager: HerbstluftWM

*	Panel: Lemonbar

*	Wallpaper: Original Wallpaper

Container: LXC

*	Client: Gentoo

*	Running: emerge

[![Artix LXC: Gentoo Emerge][image-ss-artix-lxc]{: .img-responsive }][photo-ss-artix-lxc]

-- -- --

### Docker OpenRC Crash

Seems to be Kernel Issue.
Thank you for any suggestion.

{% highlight bash %}
$ cat  /var/log/docker.log 
time="2017-08-08T06:57:35.815197432+07:00" level=info msg="libcontainerd: new containerd process, pid: 4693" 
time="2017-08-08T06:57:36.840372103+07:00" level=error msg="'overlay' not found as a supported filesystem on this host. Please ensure kernel is new enough and has overlay support loaded." 
time="2017-08-08T06:57:36.842205914+07:00" level=error msg="'overlay' not found as a supported filesystem on this host. Please ensure kernel is new enough and has overlay support loaded." 
time="2017-08-08T06:57:36.908660908+07:00" level=info msg="Graph migration to content-addressability took 0.00 seconds" 
time="2017-08-08T06:57:36.909620837+07:00" level=warning msg="Your kernel does not support cgroup rt period" 
time="2017-08-08T06:57:36.909672801+07:00" level=warning msg="Your kernel does not support cgroup rt runtime" 
time="2017-08-08T06:57:36.910338270+07:00" level=info msg="Loading containers: start." 
time="2017-08-08T06:57:36.917103872+07:00" level=warning msg="Running modprobe bridge br_netfilter failed with message: modprobe: WARNING: Module bridge not found in directory /lib/modules/4.9.40-2-lts\nmodprobe: WARNING: Module br_netfilter not found in directory /lib/modules/4.9.40-2-lts\n, error: exit status 1" 
time="2017-08-08T06:57:36.919252538+07:00" level=warning msg="Running modprobe nf_nat failed with message: `modprobe: WARNING: Module nf_nat not found in directory /lib/modules/4.9.40-2-lts`, error: exit status 1" 
time="2017-08-08T06:57:36.921144739+07:00" level=warning msg="Running modprobe xt_conntrack failed with message: `modprobe: WARNING: Module xt_conntrack not found in directory /lib/modules/4.9.40-2-lts`, error: exit status 1" 
Error starting daemon: Error initializing network controller: error obtaining controller instance: failed to create NAT chain: iptables failed: iptables -t nat -N DOCKER: modprobe: FATAL: Module ip_tables not found in directory /lib/modules/4.9.40-2-lts
iptables v1.6.1: can't initialize iptables table 'nat': Table does not exist (do you need to insmod?)
Perhaps iptables or your kernel needs to be upgraded.
 (exit status 3)
{% endhighlight %}

[![Artix Docker: Error][image-ss-artix-docker]{: .img-responsive }][photo-ss-artix-docker]

Relax... this is a new distro.

	I can wait. Just be Patient.

I haven't found other bug yet with Artix Linux.
Everything seems to be allright except this Docker thing.

-- -- --

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[image-ss-artix-keyring]:  {{ asset_path }}/artix-keyring.png
[photo-ss-artix-keyring]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP8bgFwybhoBa-4DdHaE0hG2Two6POd-43sIVRI?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-manjaro-remove]: {{ asset_path }}/artix-remove-manjaro.png
[photo-ss-manjaro-remove]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNePVhvKc8dn-_Ge4VaFvkD9yvBBB3-luPRo_Ec?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn 

[image-ss-systemd-dummy]:  {{ asset_path }}/artix-systemd-dummy.png
[photo-ss-systemd-dummy]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPFMKW1rFxXOfA6S6-rwnTDwXv_LVGDUP3C8reD?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-artix-lxc]:     {{ asset_path }}/artix-lxc-gentoo-emerge-webrsync.png
[photo-ss-artix-lxc]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOrtBFrxcZLd6gB7QwIkR6hLpcBJI9PdwIR3-0J?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-artix-docker]:  {{ asset_path }}/artix-docker-openrc-error.png
[photo-ss-artix-docker]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMRzUrJTpvJnpBKKYgDfS6y_MhW3FjvUP98om_b?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
