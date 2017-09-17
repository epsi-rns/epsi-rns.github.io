---
layout: post
title: "Distribution - Manjaro OpenRC to Artix Migration"
date: 2017-08-07 09:45:15 +0700
categories: system
tags: [manjaro, artix, openrc, distro, package manager]
author: epsi

excerpt:
  Manjaro OpenRC to Artix, Migrate Log.

related_link_ids: 
  - 17083145  # Docker Package Manager Summary
  - 17080845  # Debian Devuan Migration
  - 17080645  # Mageia 6 Upgrade
  - 17080545  # GRUB2 BTRFS Support
  - 17080345  # openSUSE Tumbleweed Install
  - 17080245  # Fedora 23 to 26 Install
  - 17080145  # Manjaro OpenRC Issues
  - 15101649  # LAMP Stack OpenRC
  - 14112819  # Init Civil War

---

### Preface

	I did migration to Artix Linux just five days after the announcement.

The world spinning so fast.
Suddenly Manjaro-OpenRC become discontinued,
and morphing into new distro called Artix Linux.

*	<https://forum.manjaro.org/t/manjaro-openrc-will-be-discontinued/28387/204>

Artix Linux has stronger focus on init.
Not just OpenRC but other init as well such as runit and sheperd.

	The migration done smoothly

After migation, Artix works well.
Just like Manjaro-OpenRC. I see no difference.

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

#### Setup Repository, Mirror, and Keyring

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


#### Migrating from Manjaro

{% highlight bash %}
# pacman -Rdd manjaro-system
# pacman -Rsc manjaro-tools-base manjaro-system mhwd mhwd-db manjaro-firmware manjaro-settings-manager intel-ucode lsb-release rpcbind-openrc
# pacman -Rsc mhwd mhwd-db manjaro-firmware manjaro-settings-manager lsb-release 
{% endhighlight %}

[![Remove Manjaro][image-ss-manjaro-remove]{: .img-responsive }][photo-ss-manjaro-remove]


#### Setup base

{% highlight bash %}
# pacman -Rdd sysvinit udev-openrc consolekit consolekit-openrc
# pacman -Rdd sysvinit 
# pacman -S --asdeps systemd-dummy libsystemd-dummy
{% endhighlight %}

[![systemd Dummy][image-ss-systemd-dummy]{: .img-responsive }][photo-ss-systemd-dummy]

I have Perl dependency problem while doing this below,
different with guidance.
I have to remove some packages manually.
And added later after installation completed.

{% highlight bash %}
# pacman -Su base base-devel openrc-system grub linux-lts linux-lts-headers
{% endhighlight %}

After manual removal, continue using the guidance.

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

#### Reinstall GRUB

{% highlight bash %}
# mkinitcpio -p linux-lts
# mkinitcpio -p linux314
# update-grub
{% endhighlight %}

#### My Personal Preferences

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

### Troubleshooting Examples

This is how I managed my own issues.
Troubleshooting helps you understand your system.

	Do not be afraid of issues.

As I said, I did migration to Artix Linux
just five days after the announcement.
Most of the problems fixed in just few days.

Everything works well.
Except for these two things

*	Some package lagged behind upstream, such as Perl. 
	This normally fixed within few days.

*	Docker has not work yet. This is a kernel issue.

(1)	Dependency problem

Most are part of Perl dependency problem (perl < 5.25).

{% highlight bash %}
# pacman -Su base base-devel openrc-system grub linux-lts linux-lts-headers
# pacman -R udisks2 gvfs kdelibs solid udiskie
# pacman -R mariadb net-snmp
# pacman -R mariadb net-snmp mysql-openrc hplip akonadi-qt4
# pacman -R perl-clone perl-dbi perl-net-dbus perl-text-iconv perl-xml-libxml perl-xml-parser perl-xml-twig intltool foomatic-db-engine python-distutils-extra python2-distutils-extra oblogout
# pacman -R imagemagick lvm2-nosystemd dhcpcd-nosystemd cups-filters inkscape lvm2-openrc dhcpcd-openrc cups-nosystemd cups-openrc
# pacman -Su base base-devel openrc-system grub linux-lts linux-lts-headers
{% endhighlight %}

This has been solved after waiting for three days.
Perl upgraded to 5.26.

{% highlight bash %}
$ pacman -Qi perl
Name            : perl
Version         : 5.26.0-1
Description     : A highly capable, feature-rich programming language
Architecture    : x86_64
URL             : http://www.perl.org
Licenses        : GPL  PerlArtistic
Groups          : base
{% endhighlight %}

{% highlight bash %}
# pacman -S perl
# pacman -S perl-clone perl-dbi perl-net-dbus perl-text-iconv perl-xml-libxml perl-xml-parser perl-xml-twig intltool foomatic-db-engine python-distutils-extra python2-distutils-extra oblogout
# pacman -S imagemagick  cups-filters inkscape  
# pacman -S dhcpcd lvm2 cups
# pacman -S mariadb net-snmp mysql-openrc hplip akonadi-qt4
# pacman -S udisks2 gvfs kdelibs solid udiskie
{% endhighlight %}

(2) Docker OpenRC Crash

There is a kernel issue.
After waiting for three days.
It finally works normally.

[![Artix Docker: Slackware Slackpkg][image-ss-artix-docker]{: .img-responsive }][photo-ss-artix-docker]

(3) No official Jekyll packages yet.

Takes time to propagate all packages to repository.
Meanwhile, there are AUR's packages.

{% highlight conf %}
jekyll-sass-converter jekyll-watch 
rb-fsevent rb-inotify
ruby-listen ruby-colorator ruby-kramdown
ruby-liquid ruby-mercenary ruby-pathutil
ruby-forwardable-extended ruby-safe_yaml 
ruby-jekyll ruby-jekyll-paginate
{% endhighlight %}

[![Artix: Using AUR Jekyll][image-ss-artix-jekyll]{: .img-responsive }][photo-ss-artix-jekyll]

Jekyll works well in Artix.

-- -- --

### Conclusion

I haven't found other bug yet with Artix Linux.
Everything seems to be allright.
If there is a bug, it is okay.
Relax... this is a new distro.

	I can wait. Just be Patient.



Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/distro-artix' %}

[image-ss-artix-keyring]:  {{ asset_post }}/keyring.png
[photo-ss-artix-keyring]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP8bgFwybhoBa-4DdHaE0hG2Two6POd-43sIVRI?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-manjaro-remove]: {{ asset_post }}/remove-manjaro.png
[photo-ss-manjaro-remove]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNePVhvKc8dn-_Ge4VaFvkD9yvBBB3-luPRo_Ec?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn 

[image-ss-systemd-dummy]:  {{ asset_post }}/systemd-dummy.png
[photo-ss-systemd-dummy]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPFMKW1rFxXOfA6S6-rwnTDwXv_LVGDUP3C8reD?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-artix-lxc]:     {{ asset_path }}/artix-lxc-gentoo-emerge-webrsync.png
[photo-ss-artix-lxc]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOrtBFrxcZLd6gB7QwIkR6hLpcBJI9PdwIR3-0J?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-artix-docker]:  {{ asset_post }}/docker-slackware.png
[photo-ss-artix-docker]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM_M5Tj36VL_H-RD1i_JniyJUKFOHy58MVmzTyq?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-artix-jekyll]:  {{ asset_post }}/jekyll.png
[photo-ss-artix-jekyll]:  https://photos.google.com/photo/AF1QipMbJISXgyLHcNPGex9EWqYapRgB8CjTHEuqqyO_
