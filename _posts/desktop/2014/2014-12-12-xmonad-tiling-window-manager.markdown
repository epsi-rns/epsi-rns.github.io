---
layout: post-sidemenu-wm
title:  "XMonad Tiling Window Manager"
date:   2014-12-12 22:59:15 +0700
categories: desktop
tags: [screenshot, xmonad, dotfiles]
author: epsi

excerpt:
  This show case show you the looks of Tiling Window Manager with XMonad.
  XMonad is an amazing Tiling Window Manager.
  Its configuration written in Haskell,
  so you can make a very flexible configuration.

related_link_ids:
  - 16061316  # The Difference Between DE and WM
  - 14121558  # XMonad Menu Bar
  - 16032658  # Modularized XMonad Config  
  - 16051102  # XMonad with Conkyless Dzen
  - 16051403  # How Haskell Syntax
  - 14110202  # Openbox SWM
  - 14110804  # Fluxbox SWM
  - 14113019  # Awesome TWM Beginner
  - 14120646  # i3 TWM

---

This show case show you the looks of Tiling Window Manager with XMonad.

I like the way Tiling Window Manager remove all window decoration.
It makes the screenshot looks cooler.

XMonad is an amazing Tiling Window Manager.
Its configuration written in Haskell,
so you can make a very flexible configuration.

-- -- --

## Self Explanatory

{% capture ss_content %}
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
  + Statusbar: Dzen2<br>
  + Run: xmonad.hs, man xmonad
{% endcapture %}

{% include part/screenshot.html
   title = 'Simple configuration in Vim, and manual page.'
   ss_content = ss_content
%}

[![Self Explanatory][image-ss-self-explanatory]{: .img-responsive }][photo-ss-self-explanatory]

-- -- --

## XMobar

{% capture ss_content %}
<strong>OS</strong>: Debian<br>
<strong>WM</strong>: XMonad<br>
  + Statusbar: XMobar<br>
  + Terminal: Transparency: 85%<br>
  + Run: nload, nethogs, httpry
{% endcapture %}

{% include part/screenshot.html
   title = 'No gaps by default.'
   ss_content = ss_content
%}


[![XMobar: No Gap][image-ss-xmobar-nogap]{: .img-responsive }][photo-ss-xmobar-nogap]

<br/>

{% capture ss_content %}
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
  + Statusbar: XMobar<br>
  + Run: nload, newsbeuter, tmux[glances,vim], mutt)
{% endcapture %}

{% include part/screenshot.html
   title = 'Three columns with gaps.'
   ss_content = ss_content
%}

[![XMobar: Three Column with Gap][image-ss-xmobar-threecolumns-gap]{: .img-responsive }][photo-ss-xmobar-threecolumns-gap]


Source

* [github.com/epsi-rns/dotfiles/.../xmobar][dotfiles-xmobar]

-- -- --

## Dzen2

{% capture ss_content %}
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
  + Statusbar: Dzen<br>
  + Run: mutt, irssi, newsbeuter, vim)
{% endcapture %}

{% include part/screenshot.html
   title = 'Running six terminals.'
   ss_content = ss_content
%}


[![Dzen2 symmetric][image-ss-dzen2]{: .img-responsive }][photo-ss-dzen2]

<br/>

{% capture ss_content %}
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
  + Statusbar: Dzen<br>
  + Run: (vnstat+httpry+pktstat)+(vim+root+hexedit)+powerline
{% endcapture %}

{% include part/screenshot.html
   title = 'Running five terminals.'
   ss_content = ss_content
%}


[![Dzen2 asymmetric][image-ss-dzen2-five-terms]{: .img-responsive }][photo-ss-dzen2-five-terms]


Source

* [github.com/epsi-rns/dotfiles/.../xmobar][dotfiles-dzen]

-- -- --

I think that's all.
Five selfieshoot are worth five thousand words

[//]: <> ( -- -- -- links below -- -- -- )

[dotfiles-xmobar]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-xmobar/
[dotfiles-dzen]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-dzen/


[image-ss-self-explanatory]: {{ site.url }}/assets/posts/desktop/2014/12/xmonad-self-explanatory.png
[photo-ss-self-explanatory]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPL60aooDDHmvjL4x-Uge_L19JoK0B5_Y5zaMRX
[image-ss-xmobar-nogap]: {{ site.url }}/assets/posts/desktop/2014/12/xmonad-xmobar-nogap.png
[photo-ss-xmobar-nogap]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPdOCZNYmmh4PsvdDrve9LtQY8WdyEWzjVJrcep
[image-ss-xmobar-threecolumns-gap]: {{ site.url }}/assets/posts/desktop/2014/12/xmonad-xmobar-threecolumns.png
[photo-ss-xmobar-threecolumns-gap]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPtamGAwMDJ51aL1-RkvuBwZZ57aiRm6A04_0yk
[image-ss-dzen2]: {{ site.url }}/assets/posts/desktop/2014/12/xmonad-dzen2.png
[photo-ss-dzen2]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPwrSBh_z0F3XFb8LusEcaDFRzu1O0I2lv7Dtvz
[image-ss-dzen2-five-terms]: {{ site.url }}/assets/posts/desktop/2014/12/xmonad-dzen2-five-terms.png
[photo-ss-dzen2-five-terms]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipNZrGXQjKC8DwMLaCS44BOtpRVTsm2k-G77XPvl

