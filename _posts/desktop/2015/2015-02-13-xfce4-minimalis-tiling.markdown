---
layout: post-sidemenu-wm
title:  "Minimalist XFCE4 with Limited Tiling Support"
date:   2015-02-13 08:48:15 +0700
categories: desktop
tags: [screenshot, xfce4]
author: epsi

excerpt:
  XFCE4 is great, so simple, but yet it has this cool simple tiling.
  It is a built in feature, that some people forget to explore.

related_link_ids: 
  - 14051435  # Selfieshoot of XFCE4 Customization

---

Archwiki said: "XFWM4 can 'tile' a window automatically when it is moved to an edge of the screen. It does so by resizing it to fit the top half of the screen. "

**Reading**:<br/>
* <https://forum.xfce.org/viewtopic.php?id=7405>

{% capture ss_content %}
<strong>OS</strong>: Debian 8.0 Jessie<br/>
  + Kernel: x86_64 Linux 3.16.0-4-amd64<br/>
  + Resolution: 1024x768<br/>
  + WM: Xfwm4<br/>
  + WM Theme: Numix<br/>
  + GTK Theme: Bluebird [GTK2], Bluebird [GTK3]<br/>
  + Icon Theme: hicolor<br/>
  + Font: Sans 10<br/>
  + Workspace: No miniature view<br/>
  + Title Font: Capture It<br/>
  + Tiling: Left + Right
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

[![debian+xfwm+tiling+minimalist][image-ss]{: .img-responsive }][picasa-ss]

Note on SS : user vs root.

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss]: {{ site.url }}/assets/posts/desktop/2015/02/xfwm4-tiling.png
[picasa-ss]: https://lh3.googleusercontent.com/-N-UOOX2gpK4/VznBgNgJ2DI/AAAAAAAAANg/PnzKVsjAkbEI2oImmgtxGBd6EXlAYndJgCCo/s0/xfwm4-tiling.png

