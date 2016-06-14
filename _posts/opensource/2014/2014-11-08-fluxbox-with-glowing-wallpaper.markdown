---
layout: post
title:  "Fluxbox Stacking Window Manager"
date:   2014-11-08 00:04:15 +0700
categories: opensource
tags: [screenshot, fluxbox]
author: epsi

excerpt:
  Before Openbox, there was Fluxbox. Fluxbox is not as cool as Openbox.
  But if you need config or just curious about, you can check this out.

related_link_ids: 
  - 16061316  # The Difference Between DE and WM
  - 14110202  # Openbox SWM
  - 14113019  # Awesome TWM Beginner  
  - 14120646  # i3 TWM
  - 14121259  # XMonad Tiling

---

Before Openbox, there was Fluxbox. Fluxbox is not as cool as Openbox.
But if you need config or just curious about, you can check this out.

I myself, still don't know why I even learn this.

{% capture ss_content %}
<strong>OS</strong>: Arch<br/>
<strong>WM</strong>: Fluxbox<br/>
  + blackarch menu in fluxbox<br/>
  + dockbarx<br/>
  + tmux [ncmpcpp play, ncmpcpp viz] in xfce-terminal<br/>
  + fb-theme: axonkolor (manual modification)<br/>
  + conky bar (manual modification)<br/>
<br/>
::.. Glowing Flux  ..::
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

[![arch+fluxbox (glow) +tmux+ncmpcpp+root+scrot][image-ss-fluxbox]{: .img-responsive }][picasa-ss-fluxbox]
<br/><br/>

This Fluxbox utilised a modified version of Axonkolor theme that I found in Deviantart.


**Fluxbox**: Arch Configuration:

* <https://github.com/epsi-rns/dotfiles/tree/master/fluxbox.arch>

**Fluxbox**: Debian Configuration:

* <https://github.com/epsi-rns/dotfiles/tree/master/fluxbox.debian>

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-fluxbox]: {{ site.url }}/assets/posts/opensource/2014/11/fluxbox-glow.png
[picasa-ss-fluxbox]: https://lh3.googleusercontent.com/-U6rIIOLyMLg/Vz2oBSAABUI/AAAAAAAAARM/WXU-K0lwXyAJ9PC-mgky7vOO0LV23STYQCCo/s0/fluxbox-glow.png
