---
layout: post
title:  "Customizing Shell Prompt with Powerline"
date:   2016-03-21 19:17:15 +0700
categories: desktop
tags: [ricing, powerline, dotfiles]
author: epsi

excerpt: 
  For those who wants to looks cool with their console screenshot.
  Or people who does ricing a lot. Well, Powerline is for you.
  
---

Dear command line fans, here is my 2 cents, powerline configuration.<br/>

* [github.com/epsi-rns/dotfiles/.../powerline][dotfiles-powerline]

All you need is to add these two configuration. They are theme (multiline) and colorscheme. And alter config.json.<br/>

I haven't done anything yet for tmux, vim, and other powerline capabilities.<br/>

* * *

{% capture ss_content %}
<strong>OS</strong>: Arch<br/>
  + <strong>WM</strong>: Awesome WM<br/>
  + <strong>Shell</strong>: Fish<br/>
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

![Powerline Light: arch+awesome][image-ss-powerline-light]{: .img-responsive }
<br/><br/>
![Powerline Dark: arch+awesome][image-ss-powerline-dark]{: .img-responsive }


Happy terminal customizing

* * *


**Reading**:<br/>
* [http://powerline.readthedocs.org/.../common.html][docs-powerline]

[//]: <> ( -- -- -- links below -- -- -- )

[docs-powerline]: http://powerline.readthedocs.org/en/master/configuration/segments/common.html
[dotfiles-powerline]: https://github.com/epsi-rns/dotfiles/tree/master/config/powerline

[image-ss-powerline-light]: {{ site.url }}/assets/posts/desktop/2016/03/powerline-light.png
[image-ss-powerline-dark]: {{ site.url }}/assets/posts/desktop/2016/03/powerline-dark.png
