---
layout: post
title:  "Standalone Openbox WM with Tint2 Panel"
date:   2014-11-02 03:02:15 +0700
categories: opensource
tags: [screenshot, openbox, tint2, ricing]
author: epsi
excerpt:
  I recommend beginner to learn Openbox.
  Openbox can be used as a standalone WM,
  or as a WM inside a DE (Desktop Environment). 
  In fact, Openbox is the only Stacking WM that I use daily.  
---

I recommend beginner to learn Openbox.
Openbox can be used as a standalone WM,
or as a WM inside a DE (Desktop Environment). 

In LXQT or XFWM you can simply type

{% highlight bash %}
$ openbox --replace
{% endhighlight %} 

As a standalone Stacking Window manager.
Openbox is the only Stacking WM that I use daily.

**OS**: Debian<br/>
**WM**: Openbox<br/>
+ Kali Menu in openbox<br/>
+ Tint2 (custom color)<br/>
+ dockbarx<br/>
+ conky bar (manual modification)<br/>
+ Font: Capture It, Terminus<br/>
+ OB3 Theme: [Shiki Wine + Onyx, Modified]<br/>
+ Wallpaper: my own photo shot from my garden<br/>
+ tmux in xfce-terminal (no scrollbar)<br/>
+ cmus<br/>
+ screenfetch<br/>
+ compton transparency for conky and menu<br/>
<br/>
::.. Green Garden ..::<br/>
<br/>

This screenshot use a lot of transparency.
<br/>

![openbox][image-ss-openbox]
<br/>

<hr/>

I also separate menu files in configuration to make it more modular.

**Openbox**: Arch Configuration:

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox.arch>

**Openbox**: Debian Configuration:

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox.debian>

<hr/>

Openbox is the default Window Manager for the lightweight #! crunchbang/ 
bunsenlabs linux distribution. So I clone theme in my arch.

**OS**: Debian<br/>
**WM**: Openbox<br/>
<br/>

![openbox clone][image-ss-openbox-clone]

<hr/>

One of the most common component in Openbox is the famous tint2 panel.

You can pick one of my collection here 

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox.arch/tint2>

Or make your own cool tint2 taskbar based on this thread.

* <http://crunchbang.org/forums/viewtopic.php?id=3232>



[image-ss-openbox]: {{ site.url }}/assets/posts/opensource/2014/11/openbox-green-garden.png
[image-ss-openbox-clone]: {{ site.url }}/assets/posts/opensource/2014/11/openbox-arch-crunchbang-clone.png

