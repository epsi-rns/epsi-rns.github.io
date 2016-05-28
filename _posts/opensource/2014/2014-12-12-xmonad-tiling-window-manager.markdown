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

I like the way Tiling Window Manager remove all window decoration.
It makes the screenshot looks cooler.

XMonad is an amazing Tiling Window Manager. 
Its configuration written in Haskell,
so you can make a very flexible configuration.

-- -- --

## Self Explanatory



<div class="sectionbox">
  <div class="sectionbox-heading">
Simple configuration in Vim,
and manual page.
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
+ Statusbar: Dzen2<br>
+ Run: xmonad.hs, man xmonad
    </div>
  </div>
</div>



[![Self Explanatory][image-ss-self-explanatory]{: .img-responsive }][picasa-ss-self-explanatory]

-- -- --

## XMobar



<div class="sectionbox">
  <div class="sectionbox-heading">
No gaps by default.
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Debian<br>
<strong>WM</strong>: XMonad<br>
+ Statusbar: XMobar<br>
+ Terminal: Transparency: 85%<br>
+ Run: nload, nethogs, httpry
    </div>
  </div>
</div>



[![XMobar: No Gap][image-ss-xmobar-nogap]{: .img-responsive }][picasa-ss-xmobar-nogap]

<br/>



<div class="sectionbox">
  <div class="sectionbox-heading">
Three columns with gaps.
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
+ Statusbar: XMobar<br>
+ Run: nload, newsbeuter, tmux[glances,vim], mutt)
    </div>
  </div>
</div>


[![XMobar: Three Column with Gap][image-ss-xmobar-threecolumns-gap]{: .img-responsive }][picasa-ss-xmobar-threecolumns-gap]


Source

* [github.com/epsi-rns/dotfiles/.../xmobar][dotfiles-xmobar]

-- -- --

## Dzen2



<div class="sectionbox">
  <div class="sectionbox-heading">
Running six terminals.
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
+ Statusbar: Dzen<br>
+ Run: mutt, irssi, newsbeuter, vim)
    </div>
  </div>
</div>


[![Dzen2 symmetric][image-ss-dzen2]{: .img-responsive }][picasa-ss-dzen2]

<br/>



<div class="sectionbox">
  <div class="sectionbox-heading">
Running five terminals.
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Arch<br>
<strong>WM</strong>: XMonad<br>
+ Statusbar: Dzen<br>
+ Run: (vnstat+httpry+pktstat)+(vim+root+hexedit)+powerline
    </div>
  </div>
</div>


[![Dzen2 on symmetric][image-ss-dzen2-five-terms]{: .img-responsive }][picasa-ss-dzen2-five-terms]


Source

* [github.com/epsi-rns/dotfiles/.../xmobar][dotfiles-dzen]

-- -- --

I think that's all.
Five selfieshoot are worth five thousand words



[dotfiles-xmobar]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-xmobar/
[dotfiles-dzen]: https://github.com/epsi-rns/dotfiles/tree/master/xmonad/xmonad-dzen/


[image-ss-self-explanatory]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-self-explanatory.png
[picasa-ss-self-explanatory]: https://lh3.googleusercontent.com/-VDWvJwgHlL4/Vz2mh_HDNkI/AAAAAAAAAO8/lZ-OnxAfSDwvrYoYZeoBPl_bdKVS06S5wCCo/s0/xmonad-self-explanatory.png
[image-ss-xmobar-nogap]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-xmobar-nogap.png
[picasa-ss-xmobar-nogap]: https://lh3.googleusercontent.com/-1bcBUJihphI/Vz2mlHnK4BI/AAAAAAAAAO8/2e_49iMs8kwSn-pqhSAcpge5LLmBeSajQCCo/s0/xmonad-xmobar-nogaps.png
[image-ss-xmobar-threecolumns-gap]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-xmobar-threecolumns.png
[picasa-ss-xmobar-threecolumns-gap]: https://lh3.googleusercontent.com/-2IkVXzKSn3Q/Vz2mpPdfJiI/AAAAAAAAAPg/-oDI1U8y-DEGrl9UK51udhjF18Nel2WSwCCo/s0/xmonad-xmobar-threecolumn.png
[image-ss-dzen2]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-dzen2.png
[picasa-ss-dzen2]: https://lh3.googleusercontent.com/-gG9fubXyTpI/Vz2m_RPgSEI/AAAAAAAAAP8/ZNXGlGJrb3YTKH5jFDAcFj5sGBxBWGVywCCo/s0/xmonad-dzen2.png
[image-ss-dzen2-five-terms]: {{ site.url }}/assets/posts/opensource/2014/12/xmonad-dzen2-five-terms.png
[picasa-ss-dzen2-five-terms]: https://lh3.googleusercontent.com/-sAfgbEpNTAs/Vz2mYpQ_MpI/AAAAAAAAAO8/E9gCAtDPoIgjnsMKNoGnY94v2QhSLOSVACCo/s0/xmonad-five-terms.png

