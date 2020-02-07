---
layout: post
title:  "Awesome WM - Theme - Titlebar"
categories: desktop
date      : 2019-10-21 09:25:15 +0700
tags      : [awesome, inkscape]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/10/clone-titlebar.png

excerpt:
  Awesome WM theme step by step.
  Custom Flat Design Titlebar, Minimalist PNG Button.
  Using Inkscape, one SVG file to create each PNG resource.

---

{% include post/2019/06/toc-awesome.html %}

-- -- --

### Preface

> Goal: Custom Flat Design Titlebar, Minimalist PNG Button

#### Table of Content

* 1: Preparation

* 2: Unified Graphic Material

* 3: Theme Configuration

* 4: Titlebar Configuration

-- -- --

### 1: Preparation

This titlebar utilize PNG file.

![Awesome WM: Title Bar][image-ss-02-titlebar]{: .img-responsive }

Consider have a look at the resources in file manager.

![Awesome WM: Thunar Title Bar][image-fm-02-titlebar]{: .img-responsive }

I decide to remade the the PNG using Inkscape.
Inkscape use SVG as default format.

* [github.com/epsi-rns/dotfiles/.../titlebar/][dotfiles-svg-icons]

If you never had experience on theming using SVG,
I recommend you to read this article below before continue:

* [XFWM4 Theme - Part One][local-xfwm4-theme]

-- -- --

### 2: Unified Graphic Material

	This is the Inkscape Part.

Instead of separated UI design for each icon,
we can put all icons into one SVG file.

#### SVG Icon Set

I have made my own custom SVG source:

![SVG Source: Title Bar][image-svg-titlebar]{: .img-responsive }

You might want to alter the UI/UGM image yourself to suit your needs.

#### SVG Source

SVG source available at:

* [github.com/epsi-rns/dotfiles/.../clone-titlebar.svg][dotfiles-svg-source]

#### Exporting

Inkscape is a very nice tools. Inkscape can name each object properly.

![Inkscape: Naming Object Property][image-ink-02-object]{: .img-responsive }

So we can have the right name, while exporting to PNG.

![Inkscape: Export Selection with Name][image-ink-02-export]{: .img-responsive }

Any SVG can be edited in text editor,
so you can change the export properties at once,
by search and replace.
In this case I change from `export-xdpi="900"` to `export-xdpi="384"`.

{% highlight lua %}
    <rect
       y="2"
       x="82"
       height="16"
       width="16"
       id="close_normal"
       style="opacity:1;fill:#fafafa;fill-opacity:1;stroke:none;stroke-width:0.99999994;stroke-opacity:1"
       inkscape:label="#rect1194"
       inkscape:export-xdpi="384"
       inkscape:export-ydpi="384" />
{% endhighlight %}

Generating icon has never been easier than this.

-- -- --

### 3: Theme Configuration

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

### 4: Titlebar Configuration

Just in case you did not notice,
there is these lines in Awesome 4.x series:

{% highlight lua %}
theme.titlebar_minimize_button_normal           = icondir .. "minimize_normal.png"
theme.titlebar_minimize_button_focus            = icondir .. "minimize_focus.png"
{% endhighlight %}

Consider go back to previous article,
and add `minimizebutton (c)` to this module:

*	[gitlab.com/.../dotfiles/.../deco/titlebar.lua][dotfiles-titlebar]

{% highlight lua %}
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.minimizebutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
{% endhighlight %}

And a new minimized button is there below.

![AwesomeWM: Titlebars with Minimize Button][image-ss-minimize]{: .img-responsive }

-- -- --

### What is Next ?

Consider continue reading [ [Awesome WM - Theme - Layout Icons][local-whats-next] ].
There is this topic, about multicolor layout icons.
Creating minimalist PNG Button using Inkscape,
one SVG file to create each PNG resource.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/10' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-theme' %}

[local-whats-next]: /desktop/2019/10/22/awesome-theme-layout.html

[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html

[image-ss-02-titlebar]: {{ asset_path }}/02-titlebar.png
[image-fm-02-titlebar]: {{ asset_path }}/02-thunar-titlebar.png
[image-ink-02-export]:  {{ asset_path }}/02-inkscape-export-titlebar.png
[image-ink-02-object]:  {{ asset_path }}/02-inkscape-object-properties.png

[image-svg-titlebar]:   {{ asset_path }}/clone-titlebar.png
[image-rvm-two-panes]:  {{ asset_path }}/gentoo-rvm-jekyll-two-panes.png
[image-ss-minimize]:    {{ asset_path }}/02-minimize-button.png

[dotfiles-titlebar]:    {{ dotfiles }}/themes/clone/titlebar.lua
[dotfiles-svg-icons]:   {{ dotfiles }}/themes/clone/titlebar/clone/
[dotfiles-svg-source]:  {{ dotfiles }}/themes/clone/titlebar/clone/clone-titlebar.svg
