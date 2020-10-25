---
layout    : post
title     : "Slides - Linux Diversity"
categories: system
date      : 2020-10-11 09:25:15 +0700
tags      : [presentation]
keywords  : [filesystem, init, package manager, standard C library, window manager, desktop environment]
author: epsi

opengraph:
  image: /assets/posts/system/modular-linux.png
excerpt:
  Dive into Linux Subsystem for Personal Educational Purpose.

---

### The Document

This article contain all article slides.
Slide by slide, so anyone can copy desired slide,
without the need to open the Impress slide, or the PDF version.

![Teaser Preview: Linux Diversity][teaser-preview]

I believe, a ready to use slide,
is useful to help people in the group discussion.

#### Original Presentation

I usually draft a presentation in a simple text based,
that I can write down in any text editor.

Then I show in web based for a few moment (actually months).
This give time to append any additional material freely,
create necessary figure, and also correct any typo, or even dead links.

You can watch the presentation here:

* [Presentation - Linux Diversity][local-presentation]

This text based presentation is the most authentic version,
with all the links required provided.

#### Impress

This presentation is made in [LibreOffice Impress][libreoffice-impress],
using [candyclone template][candyclone-template].

You can download the Impress document from a page in here:

* [presentation-linux-diversity-diagram.odp][document-impress]

With this downloadable document,
you can use the style of this presentation,
for use with your own presentation works,
either for school material, office works, homeworks, or anything else.

#### PDF

You can also download the exported pdf from a page in here:

* [presentation-linux-diversity-diagram.pdf][document-pdf]

The pdf version cannot contain animation.
So the content might be little different,
compared with the original Impress document.

#### Inkscape

The Impress pages also utilize diagram illustration made in Inkscape.
I also provide the source image in SVG format,
that you can download from a page in here:

* [presentation-linux-diversity-content.svg][document-inkscape]

Here is the preview.

![Inkscape Illustration: Linux Diversity][inkscape-thumbs]

I intentionally share the source SVG image,
so you can alter the content for your own personal use.
I know there are already so many stock images for presentation,
I just need to share, what I have, with tutorial.
This way, anyone can make pretty presentation easier.
Of course you still have to learn Inkscape,
to suit the illustration for your own use.

#### Template

> What is this candyclone rubbish?

Candyclone is an Impress template that I have made for LibreOffice contest.

* [LibreOffice Impress Template Contest by the Indonesian Community][template-contest]

There are also other free templates as well in `lumbung` repository.

* [Lumbung LibreOffice Indonesia Repository][template-lumbung]
 
This candyclone is just an example template that you can use freely.
With this candyclone example you can also learn how to make your own template.
Your very own template to suit your ecosystem.

#### Disagreement

> What if I do not agree?

The source is available, so you can freely make your own slide.
Feel free to express your thoughts, either with text, or illustration.

-- -- --

### The Slides

Here I represent all the slides.

#### Slide 01: Cover

![Slide - Cover][slide-01]

> Learning Linux Diversity

Dive into Linux Subsystem for Personal Educational Purpose.

#### Slide 02: About The Author

![Slide - About Author][slide-02]

> I have my own blog

#### Slide 03: About This Material

![Slide - About Material][slide-03]

After watching this, you will understand:

* A more systematic steps to learn GNU/linux.
* How to make your own learning plan (syllabus).

This material is not really comprehensive.
I still have so much to learn.

#### Slide 04: Chapter Break

![Slide - After First Install][slide-04]

#### Slide 05: After First Linux Install

![Slide - After Install Desire][slide-05]

You might desire to:

* Join linux community.
* Read documentation (statistically rare person).
* Update system.
* Install a bunch of application.
* Get busy with command line terminal.
* Surfing wiki and search engine.

> And then what?

#### Slide 06: Where to Go from Here

![Slide - First Install - Where to Go][slide-06]

Where to go from here?

* Should I try other distro?
* What other distro should I try?
* So many distro, so little differences!
* Should I use VM or multiboot?

> Learn part of system!
> Instead of just switching distro.

#### Slide 07: Chapter Break

![Slide - Modular Linux][slide-07]

#### Slide 08: How Modular is Linux?

![Slide - Modular Linux - How Modular?][slide-08]

How Modular is Linux?

1. Package Manager:
   * APT, ALPM, DNF, XBPS, Zypper, Portage.

2. Init:
   * SysV, systemd, openRC, runit, S6.

3. Filesystem:
   * ext4, XFS, Reiserfs, BTRFS, ZFS.

4. Standard C library in OS:
   * glibc or musl.

5. DE (Desktop Environment):
   * GTK+ based, QT based, enlightenment.

6. WM (Window Manager):
   * Stacking, Tiling, Dynamic, Compositor.

Local Groups:

* Init: [Init Freedom](https://t.me/initfreedom)

* Desktop Environment: [Desktop Environment Indonesia](https://t.me/DesktopEnvironmentID)

* Window Manager: [Dotfiles Indonesia](https://t.me/dotfiles_id)

* Learning GNU/Linux: [Belajar GNU/Linux Indonesia](https://t.me/GNULinuxIndonesia)

#### Slide 09: Three Diversity

![Slide - Modular Linux - Switch Distro][slide-09]

You still need to Switch distro,
by considering these three diversity:

* Package Manager,

* File system,

* Init.

#### Slide 10: When do I need a physical OS?

![Slide - Modular Linux - Physical OS][slide-10]

When do I need a physical OS?

* You can learn Package Manager using Docker.
* You can also learn Init using Docker.
* But you cannot learn Filesystem using Docker.

File system experience require long time examination.
Thus you have to live with baremetal (physical) OS.

#### Slide 11: Desktop Environment/ Window Manager

![Slide - Modular Linux - DE/WM][slide-11]

Yet Another Presentation.

* [Desktop Customization][desktop-customization]

[desktop-customization]: /desktop/2020/10/11/slides-desktop-customization.html

Most beginner start from switching DE/WM:

#### Slide 12: Common Subsystem?

![Slide - Modular Linux - Common Subsystem][slide-12]

Links:

* [Wireless Command Line](/system/2014/03/13/wireless-command-line.html)

* [Audio Command Line](/system/2018/06/22/audio-command-line.html)

* [Boot Process: GRUB2](/system/2016/05/19/stuck-on-boot.html)

* [dbus in Window Manager](/system/2018/01/18/dbus-window-manager.html)

#### Slide 13: Chapter Break

![Slide - Package Manager][slide-13]

#### Slide 14: Docker Test Bed

![Slide - Package Manager with Docker][slide-14]

You can utilize docker to learn other package manager.

* APT, ALPM, DNF, XBPS, Zypper, Portage.

[Docker - Package Management Summary](/system/2017/08/31/docker-package-summary.html)

Docker is suitable for old notebook with low resources.

#### Slide 15: More Articles about Docker Test Bed

![Slide - Package Manager - Docker Test Bed][slide-15]

Links:

* [Debian - APT](/system/2017/08/24/docker-debian-apt.html)

* [openSUSE - Zypper](/system/2017/08/15/docker-opensuse-zypper.html)

* [Fedora - DNF](/system/2017/08/18/docker-fedora-dnf.html)

* [Void - XBPS](/system/2017/08/13/docker-void-xbps.html)

* [Slackware - Package](/system/2017/08/21/docker-slackware-package.html)

* [Gentoo - Portage](/system/2017/08/11/docker-gentoo-portage.html)

#### Slide 16: Package Manager Feature

![Slide - Package Manager - Feature][slide-16]

#### Slide 17: Package Manager Advantage/Issue

![Slide - Package Manager - Advantage/Issue][slide-17]

Links:

* [APT Pinning](/system/2015/03/17/apt-pinning.html)

* [Selective Emerge](/system/2019/01/01/gentoo-selective-emerge.html)

* [Unbundling AUR in ALPM](/system/2014/12/26/unbundling-aur-helper-process.html)

* [Upgrading Fedora](/system/2017/08/02/distro-fedora-install.html)

* [GhostBSD Ports](/system/2019/04/02/ghostbsd-ports.html)

Deep knowledge require long time experience.
Most of issues comes months after install.

#### Slide 18: Chapter Break

![Slide - Init][slide-18]

#### Slide 19: Init: Who use what

![Slide - Init - Who Use What][slide-19]

Who use what.

* OpenRC: Gentoo, Artix, Devuan.
* runit: Void, Artix.
* s6: Obarun, Artix.
* SysV: Slackware, Devuan.
* systemd: Most Major Distro

#### Slide 20: Init Civil War

![Slide - Init - Civil Wars][slide-20]

Link:

* [The systemd Controversy](/system/2014/11/28/init-civil-wars.html)

The systemd Controversy:
Still debating in 2020 between: systemd+gnome versus linux+diversity.

#### Slide 21: Init Elements

![Slide - Init - Elements][slide-21]

Reference:

* [s6_lightning_talk.pdf](https://archive.fosdem.org/2017/schedule/event/s6_supervision/attachments/slides/1587/export/events/attachments/s6_supervision/slides/1587/s6_lightning_talk.pdf)

The four elements of an init system:

* `/sbin/init`
* `pid 1`
* `Process Supervision`
* `Service Management`

And nothing more.

#### Slide 22: Example Usage of Init

![Slide - Init - Example Usage][slide-22]

Link:

* [Setup LAMP stack with Manjaro OpenRC](https://epsi-rns.gitlab.io/backend/2015/10/16/lamp-stack-manjaro-openrc/)

#### Slide 23: Chapter Break: File System

![Slide - Filesystem][slide-23]

File System: ext4, XFS, Reiserfs, BTRFS, ZFS.

Deep knowledge require long time experience.
Most of issues comes months after install.
You cannot just install, and just understand file system instantly.

#### Slide 24: File System, and How to Find It.

![Slide - Filesystem - Example][slide-24]

File System, and How to Find It.

* ext4: most common in linux.
* ZFS: common in BSD.
* UFS: common in BSD.
* BRTRFS: Default in openSUSE `/`.
* XFS: can be installed in most linux.

#### Slide 25: Filesystem: Example Issues and Workarounds

![Slide - Filesystem - Issues and Workarounds][slide-25]

Links:

* [File System - Trapped in Snapper Rollback](/system/2018/01/07/btrfs-snapper.html)

* [File System - GRUB2 support for BTRFS](/system/2017/08/05/grub2-btrfs-support.html)

* [GhostBSD - Multiboot](/system/2019/03/20/ghostbsd-multiboot.html)

#### Slide 26: Standard C library in OS?

![Slide - Standard C Library][slide-26]

> glibc or musl.

I must admit, I do not have any experiece with `musl`.

#### Slide 27: Chapter Break

![Slide - Switching Distro][slide-27]

#### Slide 28: Switching Distro: Custom Plan

![Slide - Switching Distro - Custom Plan][slide-28]

While you are young and still have time.
Get yourself quarterly (three months) curriculum/plan.

Just get pass through it.
No need to go deep with coding.
Be an ordinary user.

After this one year,
you are already mature enough with broader view
to choose whatever linux you want.

If you want to get more wisdom.
Learn BSD land in the second year.

#### Slide 29: Switching Distro: Example Syllabus

![Slide - Switching Distro - Example Syllabus][slide-29]

> Example Syllabus (learning plan):

Make a target of first year with linux.

* Q1: ubuntu/mint/manjaro
  * learn the DE universe: gnome-shell, plasma, xfce4
  * learn basic command line.

* Q2: opensuse
  * learn filesystem: btrfs, xfs
  * also learn yast2

* Q3: arch
  * know your system,
    dive into the world of cli/terminal shell.

* Q4: gentoo
  * learn init other than systemd
  * learn patience

#### Slide 30: Switching Distro: First Quarter

![Slide - Switching Distro - Example First Quarter][slide-30]

Manuals:

* [ubuntu-manual](https://ubuntu-manual.org/)

* [debian-handbook](https://debian-handbook.info/browse/stable/)

Example Q1: Ubuntu/ Mint/ Manjaro

* Basic Terminal Command
  * ls, cat, grep, ps, top, man, info, su, sudo
  * always use __$ man__ for documentation before google.

* Also learn about basic linux briefly
  * Examine Filesystem Hierarchy Standard (FHS)
  * Examine boot process.
  * Solving audio or wireless issue.

* Read The Fine Manual
  * [ubuntu-manual](https://ubuntu-manual.org/)
  * [debian-handbook](https://debian-handbook.info/browse/stable/)  (hertzog)

* More Terminal Command
  * nmcli, ncdu, ntop, cfdisk
  * lspci, lsusb, lsmod, dmesg, ip, iw

#### Slide 31: Switching Distro: Third Quarter

![Slide - Switching Distro - Example Third Quarter][slide-31]

Example Q3: Arch: Leverage yourself to Arch

* Install with command line only
  * add driver manually, add username manually, add DE manually, use pacman.

* Read the holy arch wiki
  * there is a lot of good material here

* Use packer, cower, or other AUR Helper
  * automatic compilation, try any unofficial application from AUR

* Dare to use minimalis WM, rather than eyecandy DE
  * openbox, awesome, i3, bspwm

* Penetration Testing Application
  * add and install repo: BlackArch or maybe ArchStrike

> Philosophy: Knowing Your System

#### Slide 32: Switching Distro: Install Log/ Post Install Log

![Slide - Switching Distro - Install Log/ Post Install Log][slide-32]

Links: Install Log/ Post Install Log:

* [Fedora](/system/2017/08/02/distro-fedora-install.html)

* [openSUSE](/system/2017/08/03/distro-opensuse-install.html)

* [Debian Wheezy](/system/2014/01/02/debian-post-install-log.html)

* [GhostBSD](/system/2019/03/25/ghostbsd-driver.html)

* [Mageia](/system/2014/05/09/mageia-experiment.html)

* [Slackware](/system/2018/01/17/distro-slackware-post-install.html)

#### Slide 33: Chapter Break

![Slide - Multiboot][slide-33]

#### Slide 34: Partition Schema using MBR (old example)

![Slide - Multiboot - Partition Schema][slide-34]

Link:

* [Multiboot - Partition Schema](/system/2018/05/21/linux-multiboot.html)

> For linux enthusiast.

#### Slide 35: Multiboot: Using MBR (old schema)

![Slide - Multiboot - Partition Schema][slide-35]

Partition Schema using MBR (old example)

* Primary: Windows

* Extended: EBR
  * Linux Swap
  * Shared Partition
  * First Linux Distro
  * Second Linux Distro
  * Third Linux Distro

* Extended: BSD
  * BSD Swap
  * One or more UFS

#### Slide 36: Multiboot - /etc/fstab

![Slide - Multiboot - /etc/fstab][slide-36]

Link:

* [Multiboot - /etc/fstab](/system/2018/05/21/linux-multiboot.html)

`/etc/fstab`

* Learn to make shared partition.
* BTRFS subvolume is interesting.

#### Slide 37: Multiboot: chroot

![Slide - Multiboot - chroot][slide-37]

Link:

* [Multiboot - chroot](/system/2018/05/23/linux-multiboot.html)

chroot

* Some OS is comfortably installed using chroot:
  * such as: Gentoo, LFS.

* Other OS can be updated using chroot:
 * beware of small issues.

#### Slide 38: Multiboot: Tips: Samba Share

![Slide - Multiboot - Tips: Samba Share][slide-38]

Link:

* [Multiboot - Samba](/system/2018/05/25/linux-samba.html)

> Tips: Keep your samba's network share persistence along multiboot.

#### Slide 39: Multiboot - Tips: FreeBSD

![Slide - Multiboot - Tips: FreeBSD][slide-39]

Link:

* [GhostBSD: Multiboot](/system/2019/03/20/ghostbsd-multiboot.html)

> Tips: Linux Multiboot with BSD, can be done using UFS, instead of ZFS.

#### Slide 40: Kernel

![Slide - Kernel][slide-40]

> `make menuconfig`

#### Slide 41: Chapter Break

![Slide - Interesting Diversity][slide-41]

#### Slide 42: Interesting Diversity: Example

![Slide - Interesting Diversity - Example][slide-42]

Interesting Diversity?

* Project Trident
  * runit + zol + xbps + musl

* Alpine Linux
  * Busybox (no GNU tools)

* NixOS
  * Exotic Package Manager

* LFS
  * No comment.

* And many more
  * You name it!

#### Slide 43: What's Next?

![Slide - What is Next][slide-43]

> More Wisdom!

Learn BSD land!

#### Slide 44: Questions

![Slide - Questions][slide-44]

> Don't be shy!

#### Slide 45: Thank You

![Slide - Thank You][slide-45]

> Thank you for your time.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = 'https://epsi-rns.github.io/assets-system/2020/diversity' %}
{% assign dotfiles = 'https://github.com/epsi-rns/berkas2/blob/master/impress-presentation/01-linux-diversity' %}

[teaser-preview]:           /assets/posts/system/modular-linux.png
[local-presentation]:       /system/2020/02/02/presentation-linux-diversity.html
[candyclone-template]:      https://epsi-rns.gitlab.io/design/2020/09/21/inkscape-impress-slides-01/
[libreoffice-impress]:      https://www.libreoffice.org/discover/impress/
[document-impress]:         {{ dotfiles }}/presentation-linux-diversity-diagram.odp
[document-pdf]:             {{ dotfiles }}/presentation-linux-diversity-diagram.pdf
[document-inkscape]:        {{ dotfiles }}/presentation-linux-diversity-content.svg
[inkscape-thumbs]:          {{ asset_path }}/thumbs.png
[template-contest]:         https://blog.documentfoundation.org/blog/2020/10/12/libreoffice-impress-template-contest-by-the-indonesian-community/
[template-lumbung]:         https://lumbung.libreoffice.id/

[slide-01]: {{ asset_path }}/slide-01-cover.png
[slide-02]: {{ asset_path }}/slide-02-about-author.png
[slide-03]: {{ asset_path }}/slide-03-about-material.png
[slide-04]: {{ asset_path }}/slide-04-after-first-install.png
[slide-05]: {{ asset_path }}/slide-05-after-first-install-desire.png
[slide-06]: {{ asset_path }}/slide-06-after-first-install-where-to-go.png
[slide-07]: {{ asset_path }}/slide-07-modular-linux.png
[slide-08]: {{ asset_path }}/slide-08-modular-linux-how-modular.png
[slide-09]: {{ asset_path }}/slide-09-modular-linux-switch-distro.png
[slide-10]: {{ asset_path }}/slide-10-modular-linux-physical-os.png
[slide-11]: {{ asset_path }}/slide-11-modular-linux-de-wm.png
[slide-12]: {{ asset_path }}/slide-12-modular-linux-common-subsytem.png
[slide-13]: {{ asset_path }}/slide-13-package-manager.png
[slide-14]: {{ asset_path }}/slide-14-package-manager-with-docker.png
[slide-15]: {{ asset_path }}/slide-15-package-manager-docker-test-bed.png
[slide-16]: {{ asset_path }}/slide-16-package-manager-feature.png
[slide-17]: {{ asset_path }}/slide-17-package-manager-advantage.png
[slide-18]: {{ asset_path }}/slide-18-init.png
[slide-19]: {{ asset_path }}/slide-19-init-who-use-what.png
[slide-20]: {{ asset_path }}/slide-20-init-civil-wars.png
[slide-21]: {{ asset_path }}/slide-21-init-elements.png
[slide-22]: {{ asset_path }}/slide-22-init-example-usage.png
[slide-23]: {{ asset_path }}/slide-23-filesystem.png
[slide-24]: {{ asset_path }}/slide-24-filesystem-example.png
[slide-25]: {{ asset_path }}/slide-25-filesystem-issue-and-workaround.png
[slide-26]: {{ asset_path }}/slide-26-standard-c-library.png
[slide-27]: {{ asset_path }}/slide-27-switching-distro.png
[slide-28]: {{ asset_path }}/slide-28-switching-distro-custom-plan.png
[slide-29]: {{ asset_path }}/slide-29-switching-distro-example-syllabus.png
[slide-30]: {{ asset_path }}/slide-30-switching-distro-example-first-quarter.png
[slide-31]: {{ asset_path }}/slide-31-switching-distro-example-third-quarter.png
[slide-32]: {{ asset_path }}/slide-32-switching-distro-install-log.png
[slide-33]: {{ asset_path }}/slide-33-multiboot.png
[slide-34]: {{ asset_path }}/slide-34-multiboot-partition-schema.png
[slide-35]: {{ asset_path }}/slide-35-multiboot-using-mbr.png
[slide-36]: {{ asset_path }}/slide-36-multiboot-fstab.png
[slide-37]: {{ asset_path }}/slide-37-multiboot-chroot.png
[slide-38]: {{ asset_path }}/slide-38-multiboot-tips-samba-share.png
[slide-39]: {{ asset_path }}/slide-39-multiboot-tips-freebsd.png
[slide-40]: {{ asset_path }}/slide-40-kernel.png
[slide-41]: {{ asset_path }}/slide-41-interesting-diversity.png
[slide-42]: {{ asset_path }}/slide-42-interesting-diversity-example.png
[slide-43]: {{ asset_path }}/slide-43-what-is-next.png
[slide-44]: {{ asset_path }}/slide-44-questions.png
[slide-45]: {{ asset_path }}/slide-45-thank-you.png
