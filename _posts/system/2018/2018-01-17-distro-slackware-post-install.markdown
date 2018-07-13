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
  "Boys and Toys, Because boys will be boys".

related_link_ids: 
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration

---

### Preface

This is my **slackware post-install log**, for my personal use, 
including most desktop ricing stuff, except wallpaper :-) .

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

> "Boys and Toys: Because boys will be boys".

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

[![Slackware: network/ monitoring][image-ss-netmon]{: .img-responsive }][photo-ss-netmon]

-- -- --

### language 

For mostly blogging.

sbopkg

* [pip](https://pypi.python.org/pypi/pip) (python)

* [lua](https://www.lua.org/)

* [ghc](https://www.haskell.org/ghc/) (deb8)

slpkg alien

* [nodejs](https://nodejs.org/en/)

slpkg slonly

* [ghc](https://www.haskell.org/ghc/) (binary)

* [google-go-lang](https://golang.org/) (1.9.2), (ln -s /usr/bin/go)

manual

* [gem](https://rubygems.org/) install [jekyll](https://jekyllrb.com/)

#### Notes

* I have to remove the official go-lang (ver 1.4), because I need a new version.

* I cannot install RVM due to lbrary conflict. It can be solved by compiling, but I am lazy.

* There are two versions of GHC. Source and Binary.

-- -- --

### Desktop Ricing

[![Slackware: teamocil/tmux on herbstluftwm][image-ss-teamocil]{: .img-responsive }][photo-ss-teamocil]

#### Font 

Using git clone, then manually copy to .fonts

* [awesome font](https://github.com/FortAwesome/Font-Awesome)

* [siji](https://github.com/stark/siji)

* [takao](https://github.com/SumiTomohiko/UnnamedFukidashi/tree/master/takao-fonts-ttf)

* [powerline symbols](http://powerline.readthedocs.io/en/master/installation/linux.html)

#### window-manager 

Due to complexity and my laziness,
I switch from source from sbopkg to binary from slonly.

sbopkg 

* [dwm](https://dwm.suckless.org/)

* [herbstluftwm](https://www.herbstluftwm.org/)

* [sxhkd]

* [bspwm](https://github.com/baskerville/sxhkd)

* i3status :< confuse, yajl

* i3blocks

* i3lock

* i3 :< libev, xcb-util-xrm, libxcbcommon

* i3 (perl) :< -JSON-XS, -common-sense, -Types-Serialiser, -AnyEvent, -

* [awesomewm](https://awesomewm.org/) :< lgi

* [xmonad](http://xmonad.org/) :< -utf8-string, -X11, -mtl, -extensible-exceptions, -setlocale, -

* xmobar :< -stm, -parsec, -HTTP -X11-xft, hinotify, regex-compat, -

* [dmenu](https://tools.suckless.org/dmenu/)

* [dzen2](https://github.com/robm/dzen)

* [lemonbar](https://github.com/LemonBoy/bar)

* [polybar](https://github.com/jaagr/polybar)

* [tint2](https://gitlab.com/o9000/tint2/blob/master/doc/tint2.md#configuration)

* obmenu

* [openbox](http://openbox.org/wiki/Main_Page)

slack

* xcompmgr

* xfontsel

* gucharmap

slpkg slonly

* [i3wm](https://i3wm.org/)

* [herbstluftwm](https://www.herbstluftwm.org/)

* [bspwm](https://github.com/baskerville/bspwm)

* [awesomewm](https://awesomewm.org/)

* awesome-extra

#### Shell

* [oh-my-bash][github-oh-my-bash] ([ohmybash.github.io][site-oh-my-bash])

* [oh-my-zsh][github-oh-my-zsh] ([ohmyz.sh][site-oh-my-zsh])

* [oh-my-fish][github-oh-my-fish]

#### Ricing Component

sbopkg

* rxvt-unicode

* [scrot](https://github.com/dreamer/scrot) :< giblib :< imlib2

* [lxappearance](https://wiki.lxde.org/en/LXAppearance)

* [feh](https://feh.finalrewind.org/)

* [nitrogen](https://github.com/l3ib/nitrogen)

* [compton](https://github.com/chjj/compton) :< libconfig

* [cmus](https://github.com/cmus/cmus)

* [mpd](https://www.musicpd.org/)

* [mpc](https://github.com/MusicPlayerDaemon/mpc) :< libmpdclient

* [ncmpcpp](https://github.com/arybczak/ncmpcpp)

* [dunst](https://dunst-project.org/) :<libxdg-basedir

* [rofi](https://github.com/DaveDavenport/rofi/) :< libxkbcommon, check

* [conky](https://github.com/brndnmtthws/conky) :< tolua++

* [w3m](https://github.com/tats/w3m)

* parcellite

* [neofetch](https://github.com/dylanaraps/neofetch)

slpkg slonly

* [powerline-status](http://powerline.readthedocs.io/)

* [Pipe Terminal Screensaver](https://github.com/pipeseroni/pipes.sh/blob/master/pipes.sh)

* [byobu](https://github.com/dustinkirkland/byobu)

(via cpan)

* [TERM::Extended-Color](https://github.com/trapd00r/Term-ExtendedColor)
* [asciiquarium](https://github.com/cmatsuoka/asciiquarium)

(git clone)

* [ls++ (ls on steroid)](https://github.com/trapd00r/ls--)

* [cava](https://github.com/karlstav/cava)

* [ViM NERDtree](https://github.com/scrooloose/nerdtree)

* [powerline fonts](https://github.com/powerline/fonts)

* [lemonbar-xft](https://github.com/drscream/lemonbar-xft)

(via pip)

* [qtile ](http://www.qtile.org/)

* [powerline](http://powerline.readthedocs.io/)

(via npm)

* [gtop](https://github.com/aksakalli/gtop)

(via go install)

* [mpdviz](https://github.com/lucy/mpdviz)

(via gem)

* [teamocil](http://www.teamocil.com/)

(just script)

* [info.sh](http://pub.z3bra.org/monochromatic/misc/info.sh)

#### Other Toys

Star Wars Movie ?

{% highlight bash %}
% telnet towel.blinkenlights.nl
{% endhighlight %}

#### Not solved yet

* termite :< libvterm :< gtk/gtk.h

* i3-gaps (git clone, error on compiling)

-- -- --

### Conclusion

That's all

	I have to live with slackware, so I can learn about it.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/system/2018/01' %}

[image-ss-slpkg]: {{ asset_path }}/slackware-slpkg-herbstluftwm.png
[photo-ss-slpkg]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipO-7d8WdZeJbdOAs9VK6lRWjonT7a2LL1dTyC2a?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-teamocil]: {{ asset_path }}/slackware-teamocil-neofetch.png
[photo-ss-teamocil]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipPGC_-QPc2mxu8NEIrM21-A3ChZ2KPTi1AftDjc?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-netmon]: {{ asset_path }}/slackware-net-monitor.png
[photo-ss-netmon]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipPXf6cSRKkcB7FDooJXs8E3e2QOSKKWvNsF5_3z?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

