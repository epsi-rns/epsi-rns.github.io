---
layout: post
title:  "XMonad with Conkyless Dzen"
date:   2016-05-11 21:02:15 +0700
categories: opensource
tags: [screenshot, xmonad, dotfiles]
author: epsi
excerpt:
  Tidier code by removing conky in any dzen statusbar.
  Create a thematic XMonad configuration by controlling 
  of theme color and screen size, only from Haskell itself.
  And See how far we can make a port from BASH script to native Haskell.
---

# Removing Conky Dependency between XMonad and Dzen2

We can improve XMonad configuration, by removing extensive use of conky.
Everytime I look at XMonad dotfiles, conky is used as a feed to dzen2 in statusbar. 
I can understand the need of using conky-dzen tier from portability between WM perpective.
But portability have its drawback.

Conky is completely unnecessary,
and you can replace conky with simple while-sleep-do bash script.

{% highlight bash %}
 $ while sleep 1; do date +'%a %b %d %H:%M:%S'; done | \
   dzen2 -ta r -h 25 -y -30 -w 200 -x -200
{% endhighlight %}

The second issue with conky is total control of color for theming.
There is no way that xmonad configuration could alter colors inside conky.
All colors should be in haskell variables, 
not as a constant inside the conky, nor inside bash script.

{% highlight haskell %}
csbdTopBackground = "echo '^fg("++dcColor++")^p(;-10)^r("++screenWidth++"x5)' |"
    ++ " dzen2 -ta c -h 35 -w "++screenWidth++" "
    ++ dzenArgs ++ dzenColors
{% endhighlight %} 

So here we are, the result os porting bunch of conkys and bash-scripts,
and bundle it inside just one haskell script.

**Source**:<br/>
* [github.com/epsi-rns/dotfiles/.../MyStatusBar.hs][dotfiles-statusbar]

* * *

<div class="sectionbox">
  <div class="sectionbox-heading">
    Screenshot Information
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Arch<br/>
+ <strong>WM</strong>: XMonad<br/>
+ Compositor: Compton<br/>
+ Statusbar: Dzen2 without Conky<br/>
+ Terminal: Termite<br/>
+ Viewer: VIM (Vi IMproved)
    </div>
  </div>
</div>


[![Conkyless XMonad][image-ss-xmonad]{: .img-responsive }][picasa-ss-xmonad]

Let's compare this code, and see how our code transformed.

* That one right side console box contain haskell script for conkyless statusbar.

* Those two left side console boxes contain conky and bash script.

<br/>

* * *

There always a challenge. 
If you think that the script above is not easy to be read. 
<br/><br/>
Yes, It is.
<br/>

{% highlight haskell %}
scriptMem = "\
 \  echo -n '^fg("++spColor++"):: ^fg()\
    \^i(.xmonad/assets/monitor/mem.xbm) ';\
 \  mem_total=$(free | awk 'FNR == 2 {print $2}');\
 \  mem_used=$(free | awk 'FNR == 2 {print $3}');\
 \  echo -n $[$mem_used * 100 / $mem_total];"
{% endhighlight %} 

<br/>
As you can see, each line is a bash command inside haskell.
So the next step is to make the sleep loop as a native haskell script.
<br/><br/>

And pipe the native IO process to dzen2.
<br><br><br/>

Well. As a haskell n00b. I must admit still don't know how to do it.

-- -- --

**Related Links**

* [XMonad Tiling Window Manager][related-xmonad-twm]

* [XMonad Menu Bar][related-xmonad-bar]

* [Modularized XMonad Config][related-xmonad-modularized]

* [How Haskell Syntax can Make Your Code Cleaner][related-haskell-syntax]

[//]: <> ( -- -- -- links below -- -- -- )

[dotfiles-statusbar]: https://github.com/epsi-rns/dotfiles/blob/master/xmonad/xmonad-dzen-2/lib/MyStatusBar.hs
[image-ss-xmonad]: {{ site.url }}/assets/posts/opensource/2016/05/xmonad-with-conkyless-dzen.png
[picasa-ss-xmonad]: https://lh3.googleusercontent.com/-IaiuuSzgdMI/VzmdPYfRF-I/AAAAAAAAAMQ/BEnRnFrCFsw0CnK0oosnfrxRL975EZB4QCCo/s0/xmonad-with-conkyless-dzen.png


[related-xmonad-bar]: {{ site.url }}/opensource/2014/12/15/xmonad-menu-bar.html
[related-xmonad-modularized]: {{ site.url }}/opensource/2016/03/26/modularized-xmonad-config.html
[related-xmonad-twm]: {{ site.url }}/opensource/2014/12/12/xmonad-tiling-window-manager.html
[related-haskell-syntax]: {{ site.url }}/opensource/2016/05/14/haskell-dollar-syntax.html
