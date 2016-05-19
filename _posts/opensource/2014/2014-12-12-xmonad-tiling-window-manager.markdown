---
layout: post
title:  "XMonad Tiling Window Manager"
date:   2014-12-12 22:59:15 +0700
categories: opensource
tags: [screenshot, xmonad, dotfiles]
author: epsi
excerpt: 
  This show case show you the looks of Tiling Window Manager with XMonad.
  XMonad is an amazing Tiling Window Manager. 
  Its configuration written in Haskell,
  so you can make a very flexible configuration.
---

This show case show you the looks of Tiling Window Manager with XMonad.

XMonad is an amazing Tiling Window Manager. 
Its configuration written in Haskell,
so you can make a very flexible configuration.

-- -- --

## Self Explanatory

Simple configuration in Vim,
and manual page.

**OS**: Arch<br>
**WM**: XMonad<br>
+ Statusbar: Dzen2<br>
+ Run: xmonad.hs, man xmonad<br>

![Self Explanatory][image-ss-self-explanatory]

-- -- --

## XMobar

No gaps by default.

**OS**: Debian<br>
**WM**: XMonad<br>
+ Statusbar: XMobar<br>
+ Terminal: Transparency: 85%<br>
+ Run: nload, nethogs, httpry<br>

![XMobar: No Gap][image-ss-xmobar-nogap]

<br/>

Three columns with gaps.

**OS**: Arch<br>
**WM**: XMonad<br>
+ Statusbar: XMobar<br>
+ Run: nload, newsbeuter, tmux[glances,vim], mutt)<br>

![XMobar: Three Column with Gap][image-ss-xmobar-threecolumns-gap]


Source

* [github.com/epsi-rns/dotfiles/.../xmobar][dotfiles-xmobar]

-- -- --

## Dzen2

Running six terminals.

**OS**: Arch<br>
**WM**: XMonad<br>
+ Statusbar: Dzen<br>
+ Run: mutt, irssi, newsbeuter, vim)<br>

![Dzen2 symmetric][image-ss-dzen2]

<br/>

Running five terminals.

**OS**: Arch<br>
**WM**: XMonad<br>
+ Statusbar: Dzen<br>
+ Run: (vnstat+httpry+pktstat)+(vim+root+hexedit)+powerline<br>

![Dzen2 on symmetric][image-ss-dzen2-five-terms]


Source

* [github.com/epsi-rns/dotfiles/.../xmobar][dotfiles-dzen]

-- -- --

I think that's all.
Five selfieshoot are worth five thousand words



[dotfiles-xmobar]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-xmobar/
[dotfiles-dzen]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-dzen/


[image-ss-self-explanatory]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-self-explanatory.png
[image-ss-xmobar-nogap]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-xmobar-nogap.png
[image-ss-xmobar-threecolumns-gap]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-xmobar-threecolumns.png
[image-ss-dzen2]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-dzen2.png
[image-ss-dzen2-five-terms]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-dzen2-five-terms.png

