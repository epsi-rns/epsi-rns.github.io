---
layout: post
title:  "Awesome WM - Theme - Titlebar"
categories: desktop
date      : 2019-10-21 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/06/01-menu-custom.png

excerpt:
  Awesome WM customization step by step.
  Statusbar and titlebar modules in modularized configuration.

related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

{% include post/2019/06/toc-awesome.html %}

-- -- --

#### Table of Content

* 1: Preparation

* 2: SVG Icons

* 3: Configuration

-- -- --

### 1: Preparation

This titlebar utilize PNG file.

![Awesome WM: Title Bar][image-ss-02-titlebar]{: .img-responsive }

Consider have a look at the resource in file manager.

![Awesome WM: Thunar Title Bar][image-fm-02-titlebar]{: .img-responsive }

I decide to remade the the PNG using Inkscape.
Inkscape use SVG as default format.

If you never had experience on theming using SVG,
I recommend you to read this article below before continue:

* [XFWM4 Theme - Part One][local-xfwm4-theme]

-- -- --

### 2: SVG Icons

I have made my own custom SVG source:

![SVG Source: Title Bar][image-svg-titlebar]{: .img-responsive }

You might want to alter the image yourself to suit your needs.

#### Exporting

Inkscape is a very nice tools.
It can name each object, so we can have the right name,
while exporting to PNG.

![Inkscape: Export Selection with Name][image-ix-02-titlebar]{: .img-responsive }

Generating icon has never been easier than this.

-- -- --

### 3: Configuration

We need to map from generated PNG to Awesome predefined variable names.

*	[gitlab.com/.../dotfiles/.../clone/titlebar.lua][dotfiles-titlebar]

{% highlight lua %}
local icondir = ""

icondir = theme_path .. "titlebar/"
icondir = icondir .. "clone/"

-- Define the image to load
theme.titlebar_close_button_normal              = icondir .. "close_normal.png"
theme.titlebar_close_button_focus               = icondir .. "close_focus.png"

theme.titlebar_minimize_button_normal           = icondir .. "minimize_normal.png"
theme.titlebar_minimize_button_focus            = icondir .. "minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive     = icondir .. "ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = icondir .. "ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = icondir .. "ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = icondir .. "ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive    = icondir .. "sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = icondir .. "sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = icondir .. "sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = icondir .. "sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive  = icondir .. "floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = icondir .. "floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = icondir .. "floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = icondir .. "floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = icondir .. "maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = icondir .. "maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = icondir .. "maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = icondir .. "maximized_focus_active.png"
{% endhighlight %}

And the result is a very nice titlebar.


![AwesomeWM: Two Titlebars][image-rvm-two-panes]{: .img-responsive }

-- -- --

### Conclusion

I think that is all.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/10' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-theme' %}

[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html

[image-ss-02-titlebar]: {{ asset_path }}/02-titlebar.png
[image-fm-02-titlebar]: {{ asset_path }}/02-thunar-titlebar.png
[image-ix-02-titlebar]: {{ asset_path }}/02-export-titlebar.png

[image-svg-titlebar]:   {{ asset_path }}/clone-titlebar.png
[image-rvm-two-panes]:  {{ asset_path }}/gentoo-rvm-jekyll-two-panes.png

[dotfiles-titlebar]:    {{ dotfiles }}/themes/clone/titlebar.lua

