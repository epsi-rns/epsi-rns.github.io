---
layout: post
title:  "Awesome WM - Theme - Main"
categories: desktop
date      : 2019-10-20 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/10/gentoo-rvm-jekyll-two-panes.png

excerpt:
  Awesome WM theme step by step.
  Covering most common configuration.

---

{% include post/2019/06/toc-awesome.html %}

### Preface

> Goal: Awesome theming in modularized configuration.

Awesome support theming:

* Simple Border

* Native Gap

* Element Color: Task, Tag, Menu

* Titlebar Icon

* Layout Icon

We are going to do this in modularized way.

For these to works we also need to utilize some inkscape to generate icons.

#### Theme Artefacts

The layout of a theme is free.
It is actuallly up to you to manage.

Here below I refactor the the theme into five `lua` artefacts.

{% highlight bash %}
$  tree clone
clone
├── elements.lua
├── gmc.lua
├── layouts.lua
├── theme.lua
└── titlebar.lua

0 directories, 5 files
{% endhighlight %}

![Awesome WM: Theme Artefacts][image-ss-02-list]{: .img-responsive }

#### Resources

How about resources?

Here below I put each resource, such as icon or image,
in its own respective directory.

{% highlight bash %}
$  % tree -d
.
└── clone
    ├── launcher
    ├── layouts
    │   └── clone
    ├── misc
    │   └── default
    ├── taglist
    │   └── clone
    └── titlebar
        └── clone
{% endhighlight %}

Furthermore I also put resources,
from other theme as well, so you can switch easily.
Or at least easier for you to compare my resources to other example.

The complete resources in tree fashioned view is as below:

![Awesome WM: Resources Artefacts][image-ss-02-tree]{: .img-responsive }

#### Bold Border and Huge Gaps.

I personally do not use huge gaps and bold border on daily basis.
It is just wasting up spaces.
There are also time that I do not even like titlebar.

If you find my current theme setting irritating, you might be right.
It is not just a matter of taste.
But rather exploration of what AwesomeWM can do.
and you can always configure it as you like later.

#### Transparency

I use external tool called compton.
This is beyond Awesome WM explanation.

-- -- --

#### Table of Content

* Preface: Table of Content

* 1: Main Theme Configuration

* 2: Google Material Color

* 3: Elements

* 4: Titlebar (next article)

* Conclusion

-- -- --

### 1: Main Theme Configuration

*	[gitlab.com/.../dotfiles/.../clone/theme.lua][dotfiles-theme]

{% highlight lua %}
local awful = require("awful")
awful.util = require("awful.util")

theme_path = awful.util.getdir("config") .. "/themes/clone/"

-- default variables

theme = {}

dofile(theme_path .. "elements.lua")
dofile(theme_path .. "titlebar.lua")
dofile(theme_path .. "layouts.lua")

theme.wallpaper          = theme_path .. "background.jpg"
theme.awesome_icon       = theme_path .. "launcher/logo20_kali_black.png"
theme.awesome_subicon    = theme_path .. "launcher/logo20_kali_black.png"

return theme
{% endhighlight %}

#### Sub configuration

To avoid complexity, I separate setting in separate file,
e.g `elements.lua, `titlebar.lua`, and `layouts.lua`.

All variables in that file will be bound to `theme` variable.

#### Changing Variables

You may change the variable such as below:

{% highlight lua %}
theme.wallpaper          = "/home/epsi/Pictures/Pictures2/little_shadow-wallpaper-1280x800.jpg"
{% endhighlight %}

#### Launcher Icon

You can choose any icon for the launcher.
I just, simply like the Kali logo, even though I'm using Gentoo.
It is going to be shown on the left widgets.

{% highlight lua %}
  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      RC.launcher,
      s.mytaglist,
      s.mypromptbox,
    },
    ...
  }
{% endhighlight %}

![Awesome WM: Launcher Icon][image-ss-02-launcher]{: .img-responsive }

-- -- --

### 2: Google Material Color

Before we step into theming,
we need pallete as a based of our colorscheme.

My choice is **Google Material Color**.
This is the `gmc.lua`.

*	[gitlab.com/.../dotfiles/.../clone/gmc.lua][dotfiles-gmc]

{% highlight lua %}
local _M = {}

-- Associative Array (Hash)

_M.color = {
    ['white'] = '#ffffff',
    ['black'] = '#000000',

    ['grey50']  = '#fafafa',
    ...
    ['grey900'] = '#212121',

    ['red50']   = '#ffebee',
    ...
    ['redA700'] = '#d50000',

    ['pink50']   = '#fce4ec',
   ...
    ['pinkA700'] = '#c51162',

    ['blue50']   = '#e3f2fd',
    ...
}

return _M
{% endhighlight %}

You might prefer different pallete_._

#### Reference

* [material.io/guidelines/style/color.html](https://material.io/guidelines/style/color.html)

-- -- --

### 3: Elements

This is where the most of the theme setting configured.
Most of it just setting the color.

*	[gitlab.com/.../dotfiles/.../clone/elements.lua][dotfiles-elements]

#### Module Dependency

Pretty straightforward.

{% highlight lua %}
local gmc = require("themes.clone.gmc")

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
{% endhighlight %}

#### Font

There are only two fonts.

{% highlight lua %}
theme.font          = "Terminus 8" -- "Tamsyn 10" -- "Sans 8"
theme.taglist_font  = "Inconsolata Medium 9"
{% endhighlight %}

Here is the beauty of Lua,
I can put alternative setting as comments.
I mean, if I;m bored with `Terminus`, I can switch to `Tamsyn`,
by changing the code.

#### Gap

Awesome 4.x series come with native gaps.
No need external library.

{% highlight lua %}
theme.useless_gap   = dpi(20)
{% endhighlight %}

You can omit the dpi by write this as below

{% highlight lua %}
theme.useless_gap   = 20
{% endhighlight %}

And I always think, `20` is too big for daily basis.
This is just an example.

#### Border

{% highlight lua %}
theme.border_width  = dpi(7)

theme.border_normal = gmc.color['blue500']
theme.border_focus  = gmc.color['red300']
theme.border_marked = gmc.color['orange500']
{% endhighlight %}

Here, instead of hardcoding the color to something,
such as `theme.border_normal = '#2196f3'`.
We can refer to material pallete such as `blue500`.

And yeah, I also think, `7` is too big for daily basis.
This is just an example.

#### Tag List

It is self explanatory here.

{% highlight lua %}
theme.bg_normal     = gmc.color['white']     .. "cc"
theme.bg_focus      = gmc.color['red300']    .. "cc"
theme.bg_urgent     = gmc.color['orange900'] .. "cc"
theme.bg_minimize   = gmc.color['grey500']   .. "cc"
theme.bg_systray    = gmc.color['grey100']   .. "cc"

theme.fg_normal     = gmc.color['black']
theme.fg_focus      = gmc.color['white']
theme.fg_urgent     = gmc.color['white']
theme.fg_minimize   = gmc.color['white']

theme.taglist_bg_focus = gmc.color['red500'] .. "cc"
theme.taglist_fg_focus = gmc.color['white']
{% endhighlight %}

You can directly add alpha transparency `cc`.
So the `blue500` .. `cc` is equivalent to `#2196f3cc`.

![Awesome WM: Tag List][image-ss-02-taglist]{: .img-responsive }

As an alternative, you might consider to use image instead of just color.

{% highlight lua %}
theme.taglist_bg_focus = "png:" .. theme_path .. "misc/copycat-holo/taglist_bg_focus.png"
{% endhighlight %}

#### Tag List Square

In default awesome you can use this configuration setting below:

{% highlight lua %}
-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, gmc.color['black']
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, gmc.color['white']
)
{% endhighlight %}

But I prefer the oldschool image.

{% highlight lua %}
theme.taglist_squares_sel      = theme_path .. "taglist/clone/square_sel.png"
theme.taglist_squares_unsel    = theme_path .. "taglist/clone/square_unsel.png"
{% endhighlight %}

#### SVG Source

I have made my own custom SVG source:

![SVG Source: Tag List][image-svg-taglist]{: .img-responsive }

You might want to alter the image yourself to suit your needs,
such as switch color or else.

SVG source available at:

* [github.com/epsi-rns/dotfiles/.../clone-taglist.svg][dotfiles-svg-source]

#### Task List

There is not much to say about task list

{% highlight lua %}
theme.tasklist_bg_normal = gmc.color['white']    .. "88"
theme.tasklist_bg_focus  = gmc.color['red300']   .. "88"
theme.tasklist_fg_focus  = gmc.color['black']
{% endhighlight %}

![Awesome WM: Task List][image-ss-02-tasklist]{: .img-responsive }

Alternatively.

{% highlight lua %}
theme.tasklist_bg_normal = "png:" .. theme_path .. "misc/copycat-holo/bg_focus.png"
theme.tasklist_bg_focus  = "png:" .. theme_path .. "misc/copycat-holo/bg_focus_noline.png"
{% endhighlight %}

#### Title Bar

Title bar in here is only set the color.

{% highlight lua %}
theme.titlebar_bg_normal = gmc.color['white']   .. "cc"
theme.titlebar_bg_focus  = gmc.color['white']   .. "cc"
theme.titlebar_fg_focus  = gmc.color['black']   .. "cc"
{% endhighlight %}

![Awesome WM: Title Bar][image-ss-02-titlebar]{: .img-responsive }

We will deal with the icon in other artefacts.

#### Menu

This menu is also self explanatory.

{% highlight lua %}
theme.menu_submenu_icon  = theme_path .. "misc/default/submenu.png"

theme.menu_height = 20      -- dpi(15)
theme.menu_width  = 180     -- dpi(100)
--theme.menu_context_height = 20

theme.menu_bg_normal = gmc.color['white']  .. "cc"
theme.menu_bg_focus  = gmc.color['red300'] .. "cc"
theme.menu_fg_focus  = gmc.color['black']

theme.menu_border_color = gmc.color['blue500'] .. "cc"
theme.menu_border_width = 1
{% endhighlight %}

![Awesome WM: Menu][image-ss-02-menu]{: .img-responsive }

#### Complete

As a asummary, here is the complete code.

{% highlight lua %}
local gmc = require("themes.clone.gmc")

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

theme.font          = "Terminus 8" -- "Tamsyn 10" -- "Sans 8"
theme.taglist_font  = "Inconsolata Medium 9"

theme.bg_normal     = gmc.color['white']     .. "cc"
theme.bg_focus      = gmc.color['red300']    .. "cc"
theme.bg_urgent     = gmc.color['orange900'] .. "cc"
theme.bg_minimize   = gmc.color['grey500']   .. "cc"
theme.bg_systray    = gmc.color['grey100']   .. "cc"

theme.fg_normal     = gmc.color['black']
theme.fg_focus      = gmc.color['white']
theme.fg_urgent     = gmc.color['white']
theme.fg_minimize   = gmc.color['white']

theme.useless_gap   = dpi(10)
theme.border_width  = dpi(3)

theme.border_normal = gmc.color['blue500']
theme.border_focus  = gmc.color['red300']
theme.border_marked = gmc.color['orange500']

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:

theme.taglist_bg_focus = gmc.color['red500'] .. "cc"
--theme.taglist_bg_focus = "png:" .. theme_path .. "misc/copycat-holo/taglist_bg_focus.png"
theme.taglist_fg_focus = gmc.color['white']


theme.tasklist_bg_normal = gmc.color['white']    .. "88"
--theme.tasklist_bg_normal = "png:" .. theme_path .. "misc/copycat-holo/bg_focus.png"
theme.tasklist_bg_focus  = gmc.color['red300']   .. "88"
--theme.tasklist_bg_focus  = "png:" .. theme_path .. "misc/copycat-holo/bg_focus_noline.png"
theme.tasklist_fg_focus  = gmc.color['black']

theme.titlebar_bg_normal = gmc.color['white']   .. "cc"
theme.titlebar_bg_focus  = gmc.color['white']   .. "cc"
theme.titlebar_fg_focus  = gmc.color['black']   .. "cc"

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, gmc.color['black']
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, gmc.color['white']
)

-- Display the taglist squares

-- override
theme.taglist_squares_sel      = theme_path .. "taglist/clone/square_sel.png"
theme.taglist_squares_unsel    = theme_path .. "taglist/clone/square_unsel.png"

-- alternate override
-- theme.taglist_squares_sel   = theme_path .. "taglist/copycat-blackburn/square_sel.png"
-- theme.taglist_squares_unsel = theme_path .. "taglist/copycat-blackburn/square_unsel.png"
-- theme.taglist_squares_sel   = theme_path .. "taglist/copycat-zenburn/squarefz.png"
-- theme.taglist_squares_unsel = theme_path .. "taglist/copycat-zenburn/squareza.png"


-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon  = theme_path .. "misc/default/submenu.png"

theme.menu_height = 20      -- dpi(15)
theme.menu_width  = 180     -- dpi(100)
--theme.menu_context_height = 20

theme.menu_bg_normal = gmc.color['white']  .. "cc"
theme.menu_bg_focus  = gmc.color['red300'] .. "cc"
theme.menu_fg_focus  = gmc.color['black']

theme.menu_border_color = gmc.color['blue500'] .. "cc"
theme.menu_border_width = 1

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"
{% endhighlight %}

-- -- --

### What is Next ?

Consider continue reading [ [Awesome WM - Theme - Titlebar][local-whats-next] ].
There is this topic, about custom flat design titlebar.
Creating minimalist PNG Button using Inkscape,
one SVG file to create each PNG resource.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/10' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-theme' %}

[local-whats-next]: /desktop/2019/10/21/awesome-theme-titlebar.html

[image-ss-02-tree]:     {{ asset_path }}/02-tree-theme.png
[image-ss-02-list]:     {{ asset_path }}/02-list-lua.png
[image-ss-02-launcher]: {{ asset_path }}/02-launcher-icon.png
[image-ss-02-taglist]:  {{ asset_path }}/02-taglist.png
[image-ss-02-tasklist]: {{ asset_path }}/02-tasklist.png
[image-ss-02-titlebar]: {{ asset_path }}/02-titlebar.png
[image-ss-02-menu]:     {{ asset_path }}/02-menu.png

[image-svg-taglist]:    {{ asset_path }}/clone-taglist.png

[dotfiles-theme]:       {{ dotfiles }}/themes/clone/theme.lua
[dotfiles-gmc]:         {{ dotfiles }}/themes/clone/gmc.lua
[dotfiles-elements]:    {{ dotfiles }}/themes/clone/elements.lua
[dotfiles-svg-source]:  {{ dotfiles }}/themes/clone/taglist/clone/clone-taglist.svg
