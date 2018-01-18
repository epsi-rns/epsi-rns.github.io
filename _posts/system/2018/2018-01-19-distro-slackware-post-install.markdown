---
layout: post
title: "Distribution - Slackware Post Install Log"
date: 2018-01-17 09:45:15 +0700
categories: system
tags: [slackware, distro, package manager]
author: epsi

excerpt:
  My Slackware Experience,
  setting up suitable environment at home.

related_link_ids: 
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration

---

### Preface

	Any desktop ricer can use this guidance, no matter what distribution they use.

I made an experiment with BTRFS snapshot in my openSUSE,
and it make my openSUSE unbootable.
I can make my BTRFS state back by using Live USB.
But an idea popped up in my mind.
It is time to switch to Slackware.
So I download the Live USB, install it.
And it turned out that Slackware is easier than I thought.

I have some roles about suitable environment

* Working: Libreoffice: Office and Family. Sometimes inkscape.

* Dotfiles: Tiling Window Manager. And ricing stuff. Including Haskell.

* Blog and Coding: Ruby Jekyll and SaSS, Go Huho, NPM Grunt and Gulp, PHP Composer.

* Minor curiosity: such as network monitoring. Depend on my mood.


#### Slackware Issue

The issue with Slackware is that the repository is scattered.

* Slackpkg: Official Packages

* Slackbuilds: when prefer compiling.

* Slpkg: When prefer binary

* git clone or other method: When no application in any repository.

[![Slackware: slpkg slackonly herbstluftwm][image-ss-slpkg]{: .img-responsive }][photo-ss-slpkg]

These below are the list.

-- -- --

### Common Software

	If you are not a desktop ricer, you can skip this part.

My most important part is setting up DM (Desktop Manager).

sbopkg

* fish

* slim

* lxdm

* sddm :< extra-cmake-modules

* libreoffice (rpm)

slpkg alien

* chromium :< ninja

* ffmpeg

* vlc

* dropbox-client

slpkg slonly

* inkscape

* evince

slackpkg

* xfce4-mixer

git clone

* [lightdm (idnux)](https://github.com/idnux/idnux_slackbuilds)

#### network/monitor 

slpkg slonly

* nethogs, iftop, glances, dstat, atop, iotop, nbwmon, nmon


Note that pktstat and nload, are available in github.

-- -- --

### language 

For mostly blogging.

sbopkg

* pip (python)

* lua

* ghc (deb8)

slpkg alien

* nodejs

slpkg slonly

* ghc (binary)

* google-go-lang (1.9.2), (ln -s /usr/bin/go)

manual

* gem install jekyll

#### Notes

* I have to remove the official go-lang (ver 1.4), because I need a new version.

* I cannot install RVM due to lbrary conflict. It can be solved by compiling, but I am lazy.

* There are two versions of GHC. Source and Binary.

-- -- --

### Desktop Ricing

#### Font 

Using git clone, then manually copy to .fonts

* awesome

* siji

* takao

* powerline symbols

#### window-manager 

Due to complexity and my laziness,
I switch from source from sbopkg to binary from slonly.

sbopkg 

* dwm

* herbstluftwm

* sxhkd

* bspwm

* i3status :< confuse, yajl

* i3blocks

* i3lock

* i3 :< libev, xcb-util-xrm, libxcbcommon

* i3 (perl) :< -JSON-XS, -common-sense, -Types-Serialiser, -AnyEvent, -

* awesome :< lgi

* xmonad :< -utf8-string, -X11, -mtl, -extensible-exceptions, -setlocale, -

* xmobar :< -stm, -parsec, -HTTP -X11-xft, hinotify, regex-compat, -

* dmenu

* dzen

* lemonbar

* polybar

* tint2

* obmenu

* openbox

slack

* xcompmgr

* xfontsel

* gucharmap

slpkg slonly

* i3

* herbstluftwm

* bspwm

* awesome

* awesome-extra

#### Ricing Component

sbopkg

* rxvt-unicode

* scrot :< giblib :< imlib2

* lxappearance

* feh

* nitrogen

* compton :< libconfig

* cmus

* mpd

* mpc :< libmpdclient

* ncmpcpp

* dunst :<libxdg-basedir

* rofi :< libxkbcommon, check

* conky :< tolua++

* parcellite

* neofetch

slpkg slonly

* powerline-status

(via cpan)

* TERM::Extended-Color 

(git clone)

* ls++

* cava


* vim-plugin-NERDtree

* powerline fonts

* lemonbar-xft

(via pip)

* qtile 

* powerline

(via npm)

* gtop

#### Not solved yet

* termite :< libvterm :< gtk/gtk.h

* i3-gaps (git clone, error on compiling)

-- -- --

### Conclusion

That's all

	I have to live with slackware, so I can learn about it.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/01' %}

[image-ss-slpkg]: {{ asset_path }}/slackware-slpkg-herbstluftwm.png
[photo-ss-slpkg]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipO-7d8WdZeJbdOAs9VK6lRWjonT7a2LL1dTyC2a?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
