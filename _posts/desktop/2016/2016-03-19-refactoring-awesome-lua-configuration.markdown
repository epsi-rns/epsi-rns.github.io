---
layout: post
title:  "Refactoring Awesome WM's Lua Configuration Script"
date:   2016-03-19 16:41:15 +0700
categories: opensource
tags: [screenshot, awesome, dotfiles]
author: epsi

excerpt: 
  A show case of how to separate Awesome WM configuration
  to make your configuration tidy, and easier to read.
  From rx.lua script to six libraries [rc, my.keys, my.menu, my.volume, my.wibox]. 
  And separate my.wibox.lua to four smaller scripts.

related_link_ids: 
  - 16032658  # Modularized XMonad Config
  - 14113019  # Awesome TWM Beginner

---

Have a nice weekend everbody,
<br/><br/>

I must admit that as an average guy, I don't have so much knowledge. I never have my own personal successful product. No remarkable achievement. I don't even have any formal IT education. 
<br/><br/>

I just want to share a little, from what I just learn. To speed up learning process, so younger people can be better than me. Before I'm getting too old for this.

* * *
<br/>

Talk is cheap. Here is the code on github account:

* <https://github.com/epsi-rns/dotfiles/tree/master/awesome>

<br/>
[![Refactoring Awesome Light with Midnight Commander][image-ss-awesome-light]{: .img-responsive }][picasa-ss-awesome-light]
<br/><br/>
[![Refactoring Awesome Dark with Midnight Commander][image-ss-awesome-dark]{: .img-responsive }][picasa-ss-awesome-dark]
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


[image-ss-awesome-light]: {{ site.url }}/assets/posts/opensource/2016/03/refactoring-awesome-light.png
[picasa-ss-awesome-light]: https://lh3.googleusercontent.com/-HSziviMzatY/VzmdsAkfKNI/AAAAAAAAAMk/7rFDLvZUp_MqC--shJyPTB5MB894dnyMACCo/s0/refactoring-awesome-light.png
[image-ss-awesome-dark]: {{ site.url }}/assets/posts/opensource/2016/03/refactoring-awesome-dark.png
[picasa-ss-awesome-dark]: https://lh3.googleusercontent.com/-Ag5vRw54Fdk/VzmdrJ2x1GI/AAAAAAAAAMk/5nUQb2JZbkUH-xFYg0FP0f41ME2Uh5VIACCo/s0/refactoring-awesome-dark.png
