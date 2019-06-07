---
layout: post-sidemenu-wm
title:  "Standalone Openbox WM with Tint2 Panel"
date      : 2014-11-02 03:02:15 +0700
categories: desktop
tags      : [screenshot, openbox, tint2, ricing]
author: epsi

excerpt:
  I recommend beginner to learn Openbox.
  Openbox can be used as a standalone Window Manager,
  or as a Window Manager that holds a DE (Desktop Environment).
  In fact, Openbox is the only Stacking WM that I use daily.  

related_link_ids:
  - 16061316  # The Difference Between DE and WM
  - 14110804  # Fluxbox SWM
  - 14113019  # Awesome TWM Beginner  
  - 14120646  # i3 TWM
  - 14121259  # XMonad Tiling

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

{% capture ss_content %}
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
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

This screenshot use a lot of transparency.
<br/>

[![debian+openbox+tint2+conky+tmux+cmus+screenfetch+font(captureit)][image-ss-openbox]{: .img-responsive }][photo-ss-openbox]
<br/>

-- -- --

I also separate menu files in configuration to make it more modular.

**Openbox**: Arch Configuration:

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/legacy-arch>

**Openbox**: Debian Configuration:

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/legacy-debian>

<hr/>

Openbox is the default Window Manager for the lightweight #! Crunchbang/
Bunsenlabs Linux distribution. So I clone theme in my arch.

**OS**: Debian<br/>
**WM**: Openbox<br/>
<br/>

[![openbox clone][image-ss-openbox-clone]{: .img-responsive }][photo-ss-openbox-clone]

-- -- --

One of the most common component in Openbox is the famous <code>tint2</code> panel.
You can see how the taskbar has different tint2 looks form those screenshot above.

You can pick tint2 configuration from my collection here

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/legacy-arch/tint2>

Or make your own cool tint2 taskbar based on this thread.

* <http://crunchbang.org/forums/viewtopic.php?id=3232>

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-openbox]: {{ site.url }}/assets/posts/desktop/2014/11/openbox-green-garden.png
[photo-ss-openbox]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipNggBdzYid-9w2oKBHjgerSDakdOrwrFD8QnOwO
[image-ss-openbox-clone]: {{ site.url }}/assets/posts/desktop/2014/11/openbox-arch-crunchbang-clone.png
[photo-ss-openbox-clone]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipMDfHbGl3yQuVUC55L0h-WWkn-raIkNlyxAE6Dj
