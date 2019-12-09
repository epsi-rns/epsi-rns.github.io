---
layout: post
title:  "Awesome WM - Statusbar - Stacked"
categories: desktop
date      : 2019-11-25 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/11/04-statusbar-stacked.png

excerpt:
  Awesome WM statusbar step by step.
  Stacked statusbar with arrow decoration using lain library.

---

{% include post/2019/06/toc-awesome.html %}

-- -- --

### Preface

> Goal: Stacked statusbar with arrow decoration using lain library.

Let me introduce you a few decoration, using Lain library.
Instead using glyph, Awesome can also draw vector in its panel.

![Awesome WM: Stacked Statusbar with Second Panel at the Bottom][image-ss-stacked]{: .img-responsive }

Oh yeah, Awesome also works well with GhostBSD.

#### Table of Content

* 1: Stacked Bar

* 2: Bottom Bar: Arrow

-- -- --

### 1: Stacked Bar

Arranging stacked bar in AwesomeWM 4.3 is easier than in 3.5 series.
All you need to do is add another panel, such as `tasklist` as below:

#### Main Init

*	[gitlab.com/.../dotfiles/.../stacked/statusbar.lua][dotfiles-statusbar]

{% highlight lua %}
function _M.init()
  WB.taglist  = deco.taglist()
  WB.tasklist = deco.tasklist()

  WB.initdeco()

  awful.screen.connect_for_each_screen(function(s)
    WB.setup_common_boxes (s)
    
    -- Create the top wibox
    WB.generate_wibox_one(s)
    WB.generate_wibox_tasklist(s)

    -- Create the bottom wibox
    WB.generate_wibox_two(s)
  end)

end
{% endhighlight %}

#### File Naming

Since I often change my taskbar place,
I do not want use `top` or `bottom` as file helper name.
Instead I use `one` and `two`.

{% highlight lua %}
-- default statusbar
require("statusbar.stacked.helper_one")
require("statusbar.stacked.helper_two")
{% endhighlight %}

#### Task List Panel

You can either put the `tasklist` at the bottom or on top.

{% highlight lua %}
function WB.generate_wibox_tasklist (s)
  -- layout: tasklist

  -- Create the wibox
  s.wibox_top_bottom = awful.wibar({
    position = "bottom",
    screen = s,
    height = "16",
    widget = s.tasklist
  })
end
{% endhighlight %}

![Awesome WM: Stacked Statusbar with Second Panel at the Bottom][image-ss-s-example]{: .img-responsive }

#### Remove from Default Taskbar

Do not forget to remove `tasklist` from the default taskbar.

*	[gitlab.com/.../dotfiles/.../stacked/helper-one.lua][dotfiles-helper-one]

{% highlight lua %}
function WB.generate_wibox_one (s)
  -- layout: l_left, nil, l_right

  -- Create the wibox
  s.wibox_top_top = awful.wibar({ position = "top", screen = s, height = "24" })

  -- Add widgets to the wibox
  s.wibox_top_top:setup {
    layout = wibox.layout.align.horizontal,
    WB.add_widgets_top_left (s),
    nil,
    WB.add_widgets_top_right (s),
  }
end
{% endhighlight %}

-- -- --

### 2: Bottom Bar: Arrow

How about drawing arrow without glyph ?
This will give us more arrow color that we can set on the fly.

#### Prerequisite: The Lain Library

If your distribution does not support lain library,
you can clone `Lain` in Awesome configuration directory.

{% highlight bash %}
$ git clone https://github.com/lcpz/lain
{% endhighlight %}

Now you can include the `lain` as any other library.

{% highlight lua %}
-- Standard awesome library
local awful     = require("awful")
local beautiful = require("beautiful")

local wibox = require("wibox")
local lain  = require("lain")

-- Custom Local Library
local gmc = require("themes.gmc")
{% endhighlight %}

I also use google material color (`gmc.lua`) to colorize the arrow.

#### Initialize Common Variable

Now we can define any variable with `lain.util.separators`:

*	[gitlab.com/.../dotfiles/.../stacked/helper-two.lua][dotfiles-helper-two]

{% highlight lua %}
local WB = wibox_package

function WB.initdeco ()
    -- Spacer
    WB.spacer = wibox.widget.textbox(" ")
    WB.spacerline = wibox.widget.textbox(" | ")

    -- Separators lain
    local separators  = lain.util.separators
    local arrow_color = gmc.color['red300']
    WB.arrow_dl = separators.arrow_left("alpha", arrow_color)
    WB.arrow_ld = separators.arrow_left(arrow_color, "alpha")
    WB.arrow_dr = separators.arrow_right("alpha", arrow_color)
    WB.arrow_rd = separators.arrow_right(arrow_color, "alpha")
end
{% endhighlight %}

To make our life easier, consider make a shortcut:

{% highlight lua %}
-- Separators lain
local separators  = lain.util.separators

-- shortcuts
local setbg = wibox.widget.background
local setar = separators.arrow_right
local setal = separators.arrow_left
{% endhighlight %}

#### Icon Example

Since this is just an example, we are going to use only one icon:

{% highlight lua %}
-- example
local icon_example = wibox.widget.imagebox(beautiful.widget_example)
{% endhighlight %}

In this case I put cat icon in the theme

{% highlight lua %}
theme.widget_example      = icondir .. "cat.png"
{% endhighlight %}

Now we are ready to use them in these examples: `left` and `right`.

#### Left Arrow Example

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  return {
    layout = wibox.layout.fixed.horizontal,
    WB.arrow_rd,
    WB.spacerline,
    WB.arrow_dr,  WB.arrow_rd,
    WB.spacer,
    setar("alpha",              gmc.color['blue200']),
    setar(gmc.color['blue200'], gmc.color['blue300']),
    setbg(icon_example,         gmc.color['blue300']),
    setar(gmc.color['blue300'], gmc.color['blue500']),
    setbg(icon_example,         gmc.color['blue500']),
    setar(gmc.color['blue500'], gmc.color['blue700']),
    setbg(icon_example,         gmc.color['blue700']),
    setar(gmc.color['blue700'], gmc.color['blue900']),
    setar(gmc.color['blue900'], "alpha"),
    WB.spacer,
  }
end
{% endhighlight %}

![Awesome WM: Empty: Left Bottom][image-ss-s-left]{: .img-responsive }

#### Right Arrow Example

{% highlight lua %}
function WB.add_widgets_monitor_right (line, s)
  return {
    layout = wibox.layout.fixed.horizontal,
    setal("alpha", gmc.color['blue900']),
    setal(gmc.color['blue900'], gmc.color['blue700']),
    setbg(icon_example,         gmc.color['blue700']),
    setal(gmc.color['blue700'], gmc.color['blue500']),
    setbg(icon_example,         gmc.color['blue500']),
    setal(gmc.color['blue500'], gmc.color['blue300']),
    setbg(icon_example,         gmc.color['blue300']),
    setal(gmc.color['blue300'], gmc.color['blue200']),
    setal(gmc.color['blue200'], "alpha"),
    WB.spacer,
    WB.arrow_dl,  WB.arrow_ld,
    WB.spacerline,
    WB.arrow_dl,
  }
end
{% endhighlight %}

![Awesome WM: Empty: Right Bottom][image-ss-s-right]{: .img-responsive }

#### The Panel

Now it is a good time to call both in one bottom panel.

{% highlight lua %}
function WB.generate_wibox_two (s)
  -- layout: l_left, nil, tasklist

  -- Create the wibox
  s.wibox_two = awful.wibar({ position = "bottom", screen = s })

  -- Add widgets to the wibox
  s.wibox_two:setup {
    layout = wibox.layout.align.horizontal,
    WB.add_widgets_monitor_left (s),
    nil,
    WB.add_widgets_monitor_right (s),
  }
end
{% endhighlight %}

In a more sophisticated panel,
we can have more stuff between arrow as below:

![Awesome WM: Between Arrow][image-ss-a-between]{: .img-responsive }

-- -- --

### What is Next ?

Now that we are done with stacked statusbar,
we should be ready for a more complex statusbar,
such as using Vicious library, or Lain library.

![Awesome WM: GhostBSD with Arrow Statusbar][image-ss-arrow-800]{: .img-responsive }

### Conclusion

> I'll be back!

After this Awesome WM modularization
there will be another article,
about Awesome WM statusbar stacked panel customization.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/11' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-statusbar' %}

[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html
[local-statusbar]:      /desktop/2019/06/19/awesome-modularized-statusbar.html

[dotfiles-config]:      {{ dotfiles }}/rc.lua
[dotfiles-theme]:       {{ dotfiles }}/themes/clone/theme.lua
[dotfiles-statusbar]:   {{ dotfiles }}/statusbar/stacked/statusbar.lua
[dotfiles-helper-one]:  {{ dotfiles }}/statusbar/stacked/helper_one.lua
[dotfiles-helper-two]:  {{ dotfiles }}/statusbar/stacked/helper_two.lua

[image-ss-stacked-640]: {{ asset_path }}/04-ghostbsd-awesome-stacked.png
[image-ss-s-example]:   {{ asset_path }}/04-statusbar-stacked-example.png
[image-ss-s-left]:      {{ asset_path }}/04-statusbar-stacked-left-bottom.png
[image-ss-s-right]:     {{ asset_path }}/04-statusbar-stacked-right-bottom.png
[image-ss-a-between]:   {{ asset_path }}/04-statusbar-arrow-between.png

[image-ss-arrow-800]:   {{ asset_path }}/04-ghostbsd-awesome-arrow.png
