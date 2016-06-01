---
layout: post
title:  "Modularized XMonad Config"
date:   2016-03-26 14:58:15 +0700
categories: opensource
tags: [screenshot, xmonad, dotfiles]
author: epsi

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

[![Modularized Haskell XMonad: arch + dzen + termite + neofetch + powerline (light) (floating)][image-ss-xmonad-1]{: .img-responsive }][picasa-ss-xmonad-1]
<br/><br/>
[![Modularized Haskell XMonad: manjaro + dzen + termite + vim + powerline (dark) (ranger)][image-ss-xmonad-2]{: .img-responsive }][picasa-ss-xmonad-2]

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

[code-haruka]: https://github.com/codeharuka/dotfiles
[dotfiles-lib]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-dzen-2/lib
[image-ss-xmonad-1]: {{ site.url }}/assets/posts/opensource/2016/03/modularized-xmonad-1.png
[picasa-ss-xmonad-1]: https://lh3.googleusercontent.com/-wvBpvtxgRoM/Vzmdogxu8DI/AAAAAAAAAMk/MJHotl2DFJsIOLPMODDehnAQD_IR581uwCCo/s0/modularized-xmonad-1.png
[image-ss-xmonad-2]: {{ site.url }}/assets/posts/opensource/2016/03/modularized-xmonad-2.png
[picasa-ss-xmonad-2]: https://lh3.googleusercontent.com/-q_oRSCjFDUo/Vzmdor9QKPI/AAAAAAAAAMk/0H8i6r-gjbohji9LCSG3ThMrUfS6rgC2QCCo/s0/modularized-xmonad-2.png

