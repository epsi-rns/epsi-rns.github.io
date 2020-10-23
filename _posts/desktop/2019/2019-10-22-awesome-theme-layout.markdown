---
layout     : post
title      :  "Awesome WM - Theme - Layout Icons"
categories : desktop
date       : 2019-10-22 09:25:15 +0700
tags       : [awesome, inkscape]
keywords   : [tiling, window manager, modularized, lua]
author     : epsi
toc        : toc/2019/06/toc-awesome.html

opengraph:
  image: /assets-desktop/2019/10/clone-layout.png

excerpt:
  Awesome WM theme step by step.
  Custom Multicolor Layout Icons, Minimalist PNG Button.
  Using Inkscape, one SVG file to create each PNG resource.
---

<a name="preface"></a>

### Preface

> Goal: Custom Flat Design Layout Icon, Minimalist PNG Button

With similar fashioned, we create the layout icon.

#### Table of Content

* [Preface](#preface): Table of Content

* 1: [Preparation](#preparation)

* 2: [Unified Graphic Material](#ugm)

* 3: [Configuration](#configuration)

* [What is Next?](#whats-next)

-- -- --

<a name="preparation"></a>

### 1: Preparation

This layour icons utilize PNG file.
You can spot the layout icons on the top-right of the desktop screen.

![Awesome WM: Layout][image-ss-02-layout]{: .img-responsive }

Consider have a look at the resources in file manager.

![Awesome WM: Thunar Layout Icons][image-fm-02-layout]{: .img-responsive }

I decide to remade the the PNG using Inkscape.
Inkscape use SVG as default format.

* [github.com/epsi-rns/dotfiles/.../layouts/][dotfiles-svg-icons]

Again, if you are not familiar with theming using SVG,
I recommend you to read this article below before continue:

* [XFWM4 Theme - Part One][local-xfwm4-theme]

-- -- --

<a name="ugm"></a>

### 2: Unified Graphic Material

> This is the Inkscape Part.

Instead of separated UI design for each icon,
we can put all icons into one SVG file.

#### SVG Icon Set

I have made my own custom SVG source,
inspired by multicolor theme icons.

![SVG Source: Title Bar][image-svg-titlebar]{: .img-responsive }

You might want to alter the UI/UGM image yourself to suit your needs,
e.g. black and white, or minimalist something.

#### SVG Source

SVG source available at:

* [github.com/epsi-rns/dotfiles/.../clone-layout.svg][dotfiles-svg-source]

-- -- --

<a name="configuration"></a>

### 3: Configuration

We need to map from generated PNG to Awesome predefined variable names.

*	[gitlab.com/.../dotfiles/.../clone/layouts.lua][dotfiles-layouts]

{% highlight lua %}
-- You can use your own layout icons like this:

local layout_icons = "clone"
local layout_path = theme_path .. "layouts/" .. layout_icons .. "/"

-- default awful related
theme.layout_dwindle        = layout_path .. "dwindle.png"
theme.layout_fairh          = layout_path .. "fairh.png"
theme.layout_fairv          = layout_path .. "fairv.png"
theme.layout_floating       = layout_path .. "floating.png"
theme.layout_magnifier      = layout_path .. "magnifier.png"
theme.layout_max            = layout_path .. "max.png"
theme.layout_spiral         = layout_path .. "spiral.png"
theme.layout_tilebottom     = layout_path .. "tilebottom.png"
theme.layout_tileleft       = layout_path .. "tileleft.png"
theme.layout_tile           = layout_path .. "tile.png"
theme.layout_tiletop        = layout_path .. "tiletop.png"

theme.layout_fullscreen     = layout_path .. "fullscreen.png"
theme.layout_cornernw       = layout_path .. "cornernw.png"
theme.layout_cornerne       = layout_path .. "cornerne.png"
theme.layout_cornersw       = layout_path .. "cornersw.png"
theme.layout_cornerse       = layout_path .. "cornerse.png"
{% endhighlight %}

-- -- --

<a name="whats-next"></a>

### What is Next?

Consider continue reading [ [Awesome WM - Theme - Layout Icons][local-whats-next] ].
There is this topic, about multicolor layout icons.
Creating minimalist PNG Button using Inkscape,
one SVG file to create each PNG resource.

What do you think?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets-desktop/2019/10' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-theme' %}

[local-whats-next]: /desktop/2019/10/23/awesome-theme-icons.html

[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html

[image-ss-02-layout]:   {{ asset_path }}/02-layout.png
[image-fm-02-layout]:   {{ asset_path }}/02-thunar-layout.png

[image-svg-titlebar]:   {{ asset_path }}/clone-layout.png

[dotfiles-layouts]:     {{ dotfiles }}/themes/clone/layouts.lua
[dotfiles-svg-icons]:   {{ dotfiles }}/themes/clone/layouts/clone/
[dotfiles-svg-source]:  {{ dotfiles }}/themes/clone/layouts/clone/clone-layout.svg
