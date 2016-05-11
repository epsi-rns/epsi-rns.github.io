---
layout: post
title:  "Refactoring Awesome WM's Lua Configuration Script"
date:   2016-03-19 16:41:15 +0700
categories: desktop customization
tags: [screenshot, awesome, dotfiles]
---

Have a nice weekend everbody,
<br/><br/>

I must admit that as an average guy, I don't have so much knowledge. I never have my own personal successful product. No remarkable achievement. I don't even have any formal IT education. 
<br/><br/>

I just want to share a little, from what I just learn. To speed up learning process, so younger people can be better than me. Before I'm getting too old for this.

* * *
<br/>

Talk is cheap. Here is the code on github account:

* https://github.com/epsi-rns/dotfiles/tree/master/awesome


![Refactoring Awesome Light]({{ site.url }}/assets/2016/03/refactoring-awesome-light.png)
<br/><br/>
![Refactoring Awesome Dark]({{ site.url }}/assets/2016/03/refactoring-awesome-dark.png)

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
