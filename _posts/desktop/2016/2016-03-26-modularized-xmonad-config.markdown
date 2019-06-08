---
layout: post-sidemenu-wm
title:  "Modularized XMonad Config"
date      : 2016-03-26 14:58:15 +0700
categories: desktop
tags      : [screenshot, xmonad, dotfiles]
keywords  : [modularized, refactoring, decomposition]
author: epsi

opengraph:
  image: /assets/site/images/topics/haskell.png

excerpt: 
  A show case of who to separate XMonad configuration
  to make your configuration tidy, and easier to read.
  From xmonad.hs to six libraries [MyColor.hs, MyStatusBar.hs
  MyLogHookWS.hs, MyLogHookLT.hs, MyLayoutHook.hs, MyManageHook.hs]
  plus xmonad.hs itself.
  
related_link_ids: 
  - 16031941  # Refactoring Awesome
  - 14121259  # XMonad Tiling
  - 14121558  # XMonad Menu Bar
  - 16051102  # XMonad with Conkyless Dzen
  - 16051403  # How Haskell Syntax

---


Haskell Operator is easy.
Anyone with IQ over 90 can write custom XMonad Configuration.
<br><br>
However, **decomposition** even make it more comfortable to read.<br>
Simple journey to be a masochicst-desktoper.
<br><br>
Again, talk is cheap.
Use the source Luke
<br>
* [github.com/epsi-rns/dotfiles/.../lib][dotfiles-lib]

* * *

{% capture ss_content %}
<strong>OS</strong>: Arch/Manjaro<br>
  + <strong>WM</strong>: XMonad<br>
  + Compositor: Compton<br>
  + Statusbar: Dzen-Conky<br>
  + Terminal: Termite<br>
  + Wallpaper: 3D Bar<br>
  + File Manager: Ranger<br>
  + Viewer: VIM (Vi IMproved)
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

[![Modularized Haskell XMonad: arch + dzen + termite + neofetch + powerline (light) (floating)][image-ss-xmonad-1]{: .img-responsive }][photo-ss-xmonad-1]
<br/><br/>
[![Modularized Haskell XMonad: manjaro + dzen + termite + vim + powerline (dark) (ranger)][image-ss-xmonad-2]{: .img-responsive }][photo-ss-xmonad-2]

* * *

Design inspired and taken from<br>
* [github.com/harukachan/dotfiles][code-haruka]

* * *

I don't mind people using Windows, Mac, Android, *BSD, or even HURD.
<br><br>
I'm using linux, because I had limitation in my past. A matter of survival.
<br><br>
Please consider it as my weakness. Not my strength.
<br><br>
#iamnotalinuxevangelist<br>
#ilikeevangelionmovie

* * *

A Cruel Angel's Thesis
<br><br>
-- shine like a legend,<br>1
-- Holding the sky in your arms.
<br><br>
* <https://www.youtube.com/watch?v=rt4zTczforY>

[//]: <> ( -- -- -- links below -- -- -- )

[code-haruka]:  https://github.com/codeharuka/dotfiles
[dotfiles-lib]: https://gitlab.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-dzen-2/lib
[image-ss-xmonad-1]: {{ site.url }}/assets/posts/desktop/2016/03/modularized-xmonad-1.png
[photo-ss-xmonad-1]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOKT57PCBcojHBHxvol8dl_brt0PdPtckpNTniu
[image-ss-xmonad-2]: {{ site.url }}/assets/posts/desktop/2016/03/modularized-xmonad-2.png
[photo-ss-xmonad-2]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOGhDEo1AyQ7dIJUzl67KGj0THvLO7G2sw72fHs

