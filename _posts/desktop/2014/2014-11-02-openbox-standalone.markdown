---
layout: post-sidemenu
title:  "Standalone Openbox WM with Tint2 Panel"
date:   2014-11-02 03:02:15 +0700
categories: desktop
tags: [screenshot, openbox, tint2, ricing]
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

[![debian+openbox+tint2+conky+tmux+cmus+screenfetch+font(captureit)][image-ss-openbox]{: .img-responsive }][picasa-ss-openbox]
<br/>

-- -- --

I also separate menu files in configuration to make it more modular.

**Openbox**: Arch Configuration:

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox/arch>

**Openbox**: Debian Configuration:

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox/debian>

<hr/>

Openbox is the default Window Manager for the lightweight #! Crunchbang/
Bunsenlabs Linux distribution. So I clone theme in my arch.

**OS**: Debian<br/>
**WM**: Openbox<br/>
<br/>

[![openbox clone][image-ss-openbox-clone]{: .img-responsive }][picasa-ss-openbox-clone]

-- -- --

One of the most common component in Openbox is the famous <code>tint2</code> panel.
You can see how the taskbar has different tint2 looks form those screenshot above.

You can pick tint2 configuration from my collection here

* <https://github.com/epsi-rns/dotfiles/tree/master/openbox.arch/tint2>

Or make your own cool tint2 taskbar based on this thread.

* <http://crunchbang.org/forums/viewtopic.php?id=3232>

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-openbox]: {{ site.url }}/assets/posts/desktop/2014/11/openbox-green-garden.png
[picasa-ss-openbox]: https://lh3.googleusercontent.com/-IAK0W6hIhVg/Vz2oEHxs41I/AAAAAAAAARc/PioYvuCxLy8Tqy2h2LacXQoP-I4TdC_MgCCo/s0/openbox-green-garden.png
[image-ss-openbox-clone]: {{ site.url }}/assets/posts/desktop/2014/11/openbox-arch-crunchbang-clone.png
[picasa-ss-openbox-clone]: https://lh3.googleusercontent.com/-FfVK47gbWnI/Vz2n78YRaXI/AAAAAAAAARM/i2ix7ajBjhYrAk0VXi_EsPxa382GhzZ_ACCo/s0/openbox-arch-crunchbang-clone.png
