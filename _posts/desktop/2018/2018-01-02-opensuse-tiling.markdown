---
layout: post-sidemenu-wm
title:  "openSUSE Tumbleweed Tiling Experience"
categories: desktop
date:   2018-01-02 09:25:15 +0700
tags: [i3wm, install]
author: epsi

excerpt:
  Tiling window manager experience in openSUSE.

related_link_ids:
  - 16080325  # Install i3 Arch
  - 16080224  # Install i3 Debian

---

This 2017, I accidentaly moved from Arch to openSUSE. It just happen without
planning. I must admit, my plan is Gentoo. I know this article is a little bit too average, and the screenshot is also
boring. Boring is good for business. I'm a little bit of running out of time,
so I'm a kind of in need of stable OS to get my job done.

Two times in 2017, my 2009 old notebook has some hardware issue. The first
time is in August, this event pushed me to use other computer other than my notebook. So I installed
tumbleweed at home multiboot, along with my previously installed Mageia, and I
installed [tumbleweed, rawhide, and KaOS] in my office multiboot along with previously installed Debian. After my notebook get better form the service shop, I switch from Manjaro to Artix and from Debian to Devuan.

My second hardware issue is in December, pushed me to use my openSUSE
again. So I decide to get the environment right for me. In my office, I set up
administrative thing such as samba. And live happily with KDE. At home, I set
up window manager. I'm not really a fans of Desktop Ricing, but I love working
Tiling Window Manager. And surprisingly openSUSE repository is good.

[![tumbleweed: i3][image-ss-i3]{: .img-responsive }][photo-ss-i3]

Well, it is my opinion as a Debian user and also and AUR user. So here is what I just did

Download from repository with zypper

* xmonad herbstluftwm awesome bspwm i3-gaps 

* mpd cava ncmpcpp mpclient cmus

* scrot feh nitrogen scrot compton xcompmgr

* rofi dunst dzen2

* lxappearance conky xfontsel gucharmap  

* oh-mybash, oh-my-zsh, fish, powerline

* vim, emacs, vim-plugin-NERDtree


Download rpm manually then zypper

* lemonbar

* termite


Installer for specific language

* TERM::Extended-Color (via cpan)

* ls++ (git clone)

* gtop (via npm)

* qtile (via pip)

* jekyll (via gem) to blog


Copy paste manually form Arch

* from .fonts in Arch Linux to .fonts in openSUSE

Not solved yet (I'm busy) (maybe compile manually later)

* polybar

I had setup a few, such as i3, it works flawlessly. And surprisingly it does
not take a long time to setup. Awesome works well, but I still have trouble
login to bspwm.

-- -- --

### Feliz Año Nuevo

This new year, I also give up my geany for ViM and emacs.
It is just me that need to learn.

[![tumbleweed: Vim and emacs][image-ss-editor]{: .img-responsive }][photo-ss-editor]

And I also learn to communicate in some spanish telegram linux group, with help of
google translate. Larga Vida Resistencia.

I guess I'm going to continue using openSUSE.

Welcome 2018

-- -- --

Thank you for reading and visiting.


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = site.url | append: '/assets/posts/desktop/2018/01' %}

[image-ss-i3]: {{ asset_path }}/opensuse-i3.png
[photo-ss-i3]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipM0UianivdO-I2orCzgEb1XoJNcmSxpRAWZamq9

[image-ss-editor]: {{ asset_path }}/opensuse-i3-vim-emacs.png
[photo-ss-editor]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipOBeN0tqWvbr1N-V2NxtCOJnup8ly6DCIKt1tZe

