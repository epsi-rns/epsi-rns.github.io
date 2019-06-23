---
layout: post-sidemenu-wm
title:  "Refactoring Awesome WM's Lua Configuration Script"
date      : 2016-03-19 16:41:15 +0700
categories: desktop
tags      : [screenshot, awesome, dotfiles]
keywords  : [tiling, window manager, modularized]
author: epsi

opengraph:
  image: /assets/site/images/topics/lua.png

excerpt: 
  A show case of how to separate Awesome WM configuration
  to make your configuration tidy, and easier to read.
  From rx.lua script to six libraries [rc, my.keys, my.menu, my.volume, my.wibox]. 
  And separate my.wibox.lua to four smaller scripts.

related_link_ids: 
  - 16032658  # Modularized XMonad Config
  - 16062934  # Install Awesome Arch
  - 16063052  # Install Awesome Debian  
  - 14113019  # Awesome TWM Beginner

---

Have a nice weekend everbody,
<br/><br/>

I must admit that as an average guy, I don't have so much knowledge. 
I never have my own personal successful product. No remarkable achievement. 
I don't even have any formal IT education. 
<br/><br/>

I just want to share a little, from what I just learn. To speed up learning process, so younger people can be better than me. Before I'm getting too old for this.

* * *
<br/>

Talk is cheap. Here is the code on github account:

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome>

<br/>
[![Refactoring Awesome Light with Midnight Commander][image-ss-awesome-light]{: .img-responsive }][photo-ss-awesome-light]
<br/><br/>
[![Refactoring Awesome Dark with Midnight Commander][image-ss-awesome-dark]{: .img-responsive }][photo-ss-awesome-dark]
<br/>

Awesome is a Dynamic Tiling Window Manager. For whatever that means.
<br/><br/>

What I like from Awesome is, its lua-based configuration script. Lua is a programming language. Its script is simple enough to be a short configuration file. And it is extensible for various capabilities. You can add more line that suitable for your needs. The issue is, that configuration script soon become a looong script before you know it. And you'll get lost often in long script.
<br/><br/>

Just like any other scripting language, you can separate your script into a few short script, and call each script in their main script. With this modular design, you can focus each concern, in different script. This means you can analyse easier whenever error happened.
<br/><br/>

The original Awesome Config is 'rc.lua'. But here, I have separated rc into [rc, my.keys, my.menu, my.volume, my.wibox]. Since I feel that my.wibox.lua is still too long to read. I also separated my.wibox into [my.wibox, my.wibox.list, my.wibox.vicious, my.wibox.multicolor]. Now I feel satisfied..
<br/><br/>

* * *
<br/>

Thank you for reading.
<br/><br/>

Sorry for my english<br/>
I know how terrible it is.

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-awesome-light]: {{ site.url }}/assets/posts/desktop/2016/03/refactoring-awesome-light.png
[photo-ss-awesome-light]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNCFvyl03d1gCrOg3HspYPF3K-W9Z9wdSxHoN0M
[image-ss-awesome-dark]:  {{ site.url }}/assets/posts/desktop/2016/03/refactoring-awesome-dark.png
[photo-ss-awesome-dark]:  https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMg3vsE81GPuWinJOCe7CnAcTSdQnnw-anBEuXX
