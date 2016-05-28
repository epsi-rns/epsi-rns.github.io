---
layout: post
title:  "Standalone Openbox WM with Tint2 Panel"
date:   2014-11-02 03:02:15 +0700
categories: opensource
tags: [screenshot, openbox, tint2, ricing]
author: epsi
excerpt:
  I recommend beginner to learn Openbox.
  Openbox can be used as a standalone Window Manager,
  or as a Window Manager that holds a DE (Desktop Environment). 
  In fact, Openbox is the only Stacking WM that I use daily.  
---

I recommend beginner to learn Openbox.
Openbox can be used as a standalone Window Manager,
or as a Window Manager inside a DE (Desktop Environment). 

In LXQT or XFCE4 you can simply type

{% highlight bash %}
$ openbox --replace
{% endhighlight %} 

As a standalone Stacking Window manager.
Openbox is the only Stacking WM that I use daily.

<div class="sectionbox">
  <div class="sectionbox-heading">
    Screenshot Information
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Arch<br/>
<strong>WM</strong>: Openbox<br/>
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
::.. Green Garden ..::
    </div>
  </div>
</div>



This screenshot use a lot of transparency.
<br/>

[![openbox][image-ss-openbox]{: .img-responsive }][picasa-ss-openbox]
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

[![openbox clone][image-ss-openbox-clone]{: .img-responsive }][picasa-ss-openbox-clone]

<hr/>

One of the most common component in Openbox is the famous tint2 panel.
You can see how the taskbar has different tint2 looks form those screenshot above.

You can pick tint2 configuration from my collection here 

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox.arch/tint2>

Or make your own cool tint2 taskbar based on this thread.

* <http://crunchbang.org/forums/viewtopic.php?id=3232>



[image-ss-openbox]: {{ site.url }}/assets/posts/opensource/2014/11/openbox-green-garden.png
[picasa-ss-openbox]: https://lh3.googleusercontent.com/-IAK0W6hIhVg/Vz2oEHxs41I/AAAAAAAAARc/PioYvuCxLy8Tqy2h2LacXQoP-I4TdC_MgCCo/s0/openbox-green-garden.png
[image-ss-openbox-clone]: {{ site.url }}/assets/posts/opensource/2014/11/openbox-arch-crunchbang-clone.png
[picasa-ss-openbox-clone]: https://lh3.googleusercontent.com/-FfVK47gbWnI/Vz2n78YRaXI/AAAAAAAAARM/i2ix7ajBjhYrAk0VXi_EsPxa382GhzZ_ACCo/s0/openbox-arch-crunchbang-clone.png
