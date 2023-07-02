---
layout    : post
title     : "Arch: App Install"
categories: system
date      : 2023-04-09 09:25:15 +0700
tags      : [install]
keywords  : [vanilla arch, lenovo, multiboot]
author: epsi
toc        : toc/2023/05/toc-install.html

excerpt:
  Bloating your system.

opengraph:
  image: /assets/posts/system/2023/04/014-lsinitcpio.png

---

### Preface

> Goal: Bloating your system.

The title should be like: "*How to bloat yourself*".

I do not consider myself such an idealist.
I would like to learn so much,
so I think this is a good time to bloat my system.

<a name="toc"></a>

#### Table of Content

* [Table of Content](#toc)
* [Miscellanous Packages](#packages)
* [Printing](#cups)
* [AUR](#aur)
* [What is Next?](#whats-next)

-- -- --

<a name="packages"></a>

### Miscellanous Packages

Oh yeah, I have a lot things to do in daily basis.
So I require this packages.

#### Terminal

{% highlight bash %}
❯# pacman -S htop neofetch
❯# pacman -S tmux vim neovim
{% endhighlight %}

#### Window Manager

> For Ricing

{% highlight bash %}
❯# pacman -S sddm
❯# pacman -S rofi wofi wlroots
❯# pacman -S feh nitrogen lxappearance picom
❯# pacman -S awesome openbox tint2 polybar
❯# pacman -S xmonad
❯# pacman -S i3-wm sway
{% endhighlight %}


#### Desktop Environment

{% highlight bash %}
❯# pacman -S archlinux wallpaper
❯# pacman -S xfce4 xfce4-goodies
❯# pacman -S mate mate-extra

❯# pacman -S cinnamon
❯# pacman -S lxqt
❯# pacman -S gnome-gnome-extra
❯# pacman -S plasma-meta plasma-desktop plasma-wayland-session kde-applications
{% endhighlight %}

This is buggy,
making the call to some major application so slow,
so I remove for a while

{% highlight bash %}
❯# pacman -R xdg-desktop-portal-gnome
{% endhighlight %}


#### Utility

{% highlight bash %}
❯# pacman -S network-manager-applet
❯# pacman -S gparted ntfs-3g gpart gvfs ncdu hdparm nvme-cli
❯# pacman -S nethogs iftop dstat atop iotop nmon
❯# pacman -S polkit mate-polkit
{% endhighlight %}

#### Office

> Daily Base

{% highlight bash %}
❯# pacman -S firefox chromium qutebrowser
❯# pacman -S libreoffice-still
❯# pacman -S inkscape gimp blender
❯# pacman -S ffmpeg mpv vlc clementine
❯# pacman -S telegram-desktop
❯# pacman -S qcad librecad
❯# pacman -S texlive-bin texlive-core
{% endhighlight %}

#### Development

{% highlight bash %}
❯# pacman -S --needed git base-devel
❯# pacman -S electron geany code
❯# pacman -S llvm gcc ghc go rust ruby python nodejs npm
❯# pacman -S git base-devel
❯# pacman -S jre-openjdk
{% endhighlight %}

-- -- --

<a name="cups"></a>

### Printing

{% highlight bash %}
❯# pacman -S cups
❯# systemctl enable cupsd.service
❯# systemctl start cupsd.service
$ yay epson-inkjet-printer-escpr
{% endhighlight %}

-- -- --

<a name="aur"></a>

### AUR

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

We need to concentrate to network.
There are a few alternatives.
We should start with the driver and interface.

Consider continue reading [ [Wireless: Device, Driver, Interface][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/04' %}

[local-whats-next]: /system/2023/05/01/driver-interface.html

