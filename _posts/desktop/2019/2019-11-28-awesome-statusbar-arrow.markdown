---
layout: post
title:  "Awesome WM - Statusbar - Arrow Style"
categories: desktop
date      : 2019-11-28 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/11/04-gentoo-statusbar-arrow-1024x768.png

excerpt:
  Awesome WM statusbar step by step.
  Statusbar customization, with powerline style using Lain.

---

{% include post/2019/06/toc-awesome.html %}

-- -- --

### Preface

> Goal: Statusbar customization, with powerline style using Lain.

This is not another library, such as Vicious and Lain,
but just Arrow decoration using Lain, mixed with Lain widgets.

![Awesome WM: Lain Statusbar in Gentoo (640x480)][image-ss-arrow-1024]{: .img-responsive }

If the previous article talk about how to use Lain,
this articel explain how to decorate it.

#### Table of Content

> I know this section is short

* 1: Prerequisite

* 2: Arrow Example

-- -- --

### 1: Prerequisite

#### rc.lua

Put the statusbar code at the end of the `rc.lua`,
and comment other statusbar.

*	[gitlab.com/.../dotfiles/.../awesome/4.3-statusbar/rc.lua][dotfiles-config]

{% highlight lua %}
-- Statusbar: Wibar
--local statusbar = require("statusbar.lain.statusbar")
local statusbar = require("statusbar.arrow.statusbar")
{% endhighlight %}

#### Prerequisite: The Lain Library

Since we use `lain`, the configuration is the same with previous article.

#### Artefacts

This is basically just using `Lain` library.
We need additional `custom.lua` for non `lain` widget.

{% highlight lua %}
tree ~/.config/awesome/statusbar/arrow
/home/epsi/.config/awesome/statusbar/arrow
├── custom.lua
├── helper_one.lua
├── helper_two.lua
├── lain-battery.lua
├── lain-diskfree.lua
├── lain.lua
├── lain-sound.lua
└── statusbar.lua

0 directories, 8 files
{% endhighlight %}

![Awesome WM: Arrow Statusbar Directory Using Lain Library][image-ss-arrow-dir]{: .img-responsive }

I use `one` and `two` for helper name.

#### Arrow Decoration

The same as previous article

-- -- --

### 2: Arrow Example

It is a good time to put these bunch of technique together.

#### Left Wibox

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  return {
    layout = wibox.layout.fixed.horizontal,
    WB.arrow_rd,
    WB.spacer,
    setar("alpha",              gmc.color['blue200']),
    setar(gmc.color['blue200'], gmc.color['blue300']),
    setbg(cis.netdown,          gmc.color['blue300']),
    setbg(cws.netdowninfo,      gmc.color['blue300']),
    setar(gmc.color['blue300'], gmc.color['blue500']),
    setbg(cis.netup,            gmc.color['blue500']),
    setbg(cws.netupinfo,        gmc.color['blue500']),
    setar(gmc.color['blue500'], gmc.color['blue700']),
    setbg(cis.mem,              gmc.color['blue700']),
    setbg(cws.mem,              gmc.color['blue700']),
    setar(gmc.color['blue700'], gmc.color['blue900']),
    setbg(cis.cpu,              gmc.color['blue900']),
    setbg(cws.cpu,              gmc.color['blue900']),
    setal(gmc.color['blue900'], gmc.color['blue700']),
    setbg(cis.fs,               gmc.color['blue700']),
    setbg(cws.fs,               gmc.color['blue700']),
    setal(gmc.color['blue700'], gmc.color['blue500']),
    setbg(cis.temp,             gmc.color['blue500']),
    setbg(cws.temp,             gmc.color['blue500']),
    setal(gmc.color['blue500'], gmc.color['blue300']),
    setbg(cis.bat,              gmc.color['blue300']),
    setbg(cws.bat,              gmc.color['blue300']),
    setal(gmc.color['blue300'], gmc.color['blue200']),
    setal(gmc.color['blue200'], "alpha"),
    WB.spacer,
  }
end
{% endhighlight %}

![Awesome WM: Left Arrow Widget][image-ss-arrow-left]{: .img-responsive }

Playing with color would makes the status bar pretty right!

#### Right Wibox

And if you want more simple color.

{% highlight lua %}
function WB.add_widgets_monitor_right (line, s)
  return {
    layout = wibox.layout.fixed.horizontal,
    WB.arrow_dl,         WB.arrow_ld,
    WB.spacer,
    cis.volume,  cws.volume,
    cis.mpd,     cws.mpd,
    WB.spacer,
    WB.arrow_dl,         WB.arrow_ld,
    cis.uptime,          cws.uptime,
    WB.spacerline,
    WB.arrow_dl,
  }
end
{% endhighlight %}

![Awesome WM: Right Arrow Widget][image-ss-arrow-right]{: .img-responsive }

#### Top Right Wibox

I also move the progressbar to the top.

{% highlight lua %}
function WB.add_widgets_top_right (s)
  local cws = clone_widget_set
  local cis = clone_icon_set

  return { -- Right widgets
    layout = wibox.layout.fixed.horizontal,

    --  progressbar
    cis.volume_dynamic,  cws.volumewidget,
    cis.disk,            cws.disk_bar_widget,
    cis.battery,         cws.battery_bar_widget,

    -- default
    WB.arrow_dl,         WB.arrow_ld,
    mykeyboardlayout,
    wibox.widget.systray(),
    mytextclock,
    s.layoutbox,
  }
end
{% endhighlight %}

![Awesome WM: Top Right Arrow Widget][image-ss-arrow-top]{: .img-responsive }

-- -- --

### Conclusion

> I'll be back!

I think that is all. After this article, we are done.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/11' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-statusbar' %}

[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html
[local-statusbar]:      /desktop/2019/06/19/awesome-modularized-statusbar.html

[lain-wiki]:            https://github.com/lcpz/lain/wiki

[dotfiles-config]:      {{ dotfiles }}/rc.lua
[dotfiles-theme]:       {{ dotfiles }}/themes/clone/theme.lua
[dotfiles-statusbar]:   {{ dotfiles }}/statusbar/lain/statusbar.lua
[dotfiles-h-default]:   {{ dotfiles }}/statusbar/lain/helper_default.lua
[dotfiles-h-lain]:      {{ dotfiles }}/statusbar/lain/helper_lain.lua
[dotfiles-lain]:        {{ dotfiles }}/statusbar/lain/lain.lua
[dotfiles-lain-battery]:    {{ dotfiles }}/statusbar/lain/lain-battery.lua
[dotfiles-lain-diskfree]:   {{ dotfiles }}/statusbar/lain/lain-diskfree.lua
[dotfiles-lain-sound]:      {{ dotfiles }}/statusbar/lain/lain-sound.lua

[image-ss-arrow-1024]:  {{ asset_path }}/04-gentoo-statusbar-arrow-1024x768.png
[image-ss-arrow-dir]:   {{ asset_path }}/04-directory-arrow.png
[image-ss-arrow-right]: {{ asset_path }}/04-arrow-right.png
[image-ss-arrow-left]:  {{ asset_path }}/04-arrow-left.png
[image-ss-arrow-top]:   {{ asset_path }}/04-arrow-top.png



[image-ss-lain-mem]:    {{ asset_path }}/04-lain-memory.png

[image-ss-lain-01]:     {{ asset_path }}/04-lain-more-01.png
[image-ss-lain-02]:     {{ asset_path }}/04-lain-more-02.png
[image-ss-progressbar]: {{ asset_path }}/04-lain-progressbar.png
