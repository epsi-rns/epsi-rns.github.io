---
layout     : post
title      :  "Awesome WM - Statusbar - Refactoring Default"
categories : desktop
date       : 2019-11-24 09:25:15 +0700
tags       : [awesome]
keywords   : [tiling, window manager, modularized, lua]
author     : epsi
toc        : toc/2019/06/toc-awesome.html

opengraph:
  image: /assets/posts/desktop/2019/11/04-statusbar-default.png

excerpt:
  Awesome WM statusbar step by step.
  Refactoring default statusbar into modularized helper.
---

<a name="preface"></a>

### Preface

> Goal: Refactoring default statusbar into different helper.

As I promised you, this is the statusbar article.
It is full of Lua coding and Lua refactoring.

I also introduce the second panel,
at the bottom as shown in figure below:

![Awesome WM: Default Statusbar with Second Panel at the Bottom][image-ss-default]{: .img-responsive }

#### Table of Content

* [Preface](#preface): Table of Content

* 1: [Statusbar Directory](#statusbar-directory)

* 2: [Initialization](#initialization)

* 3: [The Skeleton](#skeleton)

* 4: [Main Statusbar](#main-statusbar)

* 5: [Top Bar](#top-bar): Helper Default

* 6: [SVG Arrows](#svg-arrows)

* 7: [Bottom Bar](#bottom-bar): Helper Empty

* [What is Next?](#whats-next)

-- -- --

<a name="statusbar-directory"></a>

### 1: Statusbar Directory

I have already explain default statusbar in

* [Awesome WM - Statusbar Modules][local-statusbar]

Now we need to refactor further.
First thing to do is move your statusbar code,
to special statusbar directory,
so that we can have many kind of statusbar.

#### statusbar.lua

Prepare the directory first.

{% highlight bash %}
% cd ~/.config/awesome/statusbar/default
{% endhighlight %}

Move your `statusbar.lua` form

* ~/.config/awesome/deco/statusbar.lua

to

* ~/.config/awesome/statusbar/default/statusbar.lua

#### rc.lua

Put the statusbar code at the end of the `rc.lua`.

{% highlight lua %}
-- Statusbar: Wibar
local statusbar = require("statusbar.default.statusbar")
{% endhighlight %}

#### Header

The header of `statusbar.lua` remains the same.

*	[gitlab.com/.../dotfiles/.../default/statusbar.lua][dotfiles-statusbar]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Wibox handling library
local wibox = require("wibox")

-- Custom Local Library: Wallpaper, Keys and Mouse Binding
local deco = {
  wallpaper = require("deco.wallpaper"),
  taglist   = require("deco.taglist"),
  tasklist  = require("deco.tasklist")
}
{% endhighlight %}

We are going to use this later.

#### Code Preview

For you impatient folks out there,
here I represent, all modules that we are going to use.

![Awesome WM: ViM Panes: Statusbars: Default Modules][image-ss-04-panes-modules]{: .img-responsive }

Do not get intimidated with codes above.
These can be explained step by step.

-- -- --

<a name="initialization"></a>

### 2: Initialization

We are going to make a callable init,
to support some variable initialization, and such functions.

{% highlight lua %}
local _M = {}

function _M.init()
  ...
end

return setmetatable({}, { __call = function(_, ...) return _M.init(...) end })
{% endhighlight %}

__.__

Now we can call it in `rc.lua`.

*	[gitlab.com/.../dotfiles/.../awesome/4.3-statusbar/rc.lua][dotfiles-config]

{% highlight lua %}
-- Statusbar: Wibar
local statusbar = require("statusbar.default.statusbar")

statusbar()
{% endhighlight %}

I hope that this is clear.

-- -- --

<a name="skeleton"></a>

### 3: The Skeleton

#### The WB Object in One File

I would like to separate some code into different file.
Thus we need other object than `_M`.
Consider call it `WB`.

{% highlight lua %}
local WB = {}

function WB.generate_wibox_one (s)
  ...
end

function WB.generate_wibox_two (s)
  ...
end

function WB.setup_common_boxes (s)
  ...
end
{% endhighlight %}

And we can call it later:

{% highlight lua %}
function _M.init()
  WB.taglist  = deco.taglist()
  WB.tasklist = deco.tasklist()

  WB.initdeco()

  awful.screen.connect_for_each_screen(function(s)
    WB.setup_common_boxes (s)
    
    -- Create the top wibox
    WB.generate_wibox_one(s)

    -- Create the bottom wibox
    WB.generate_wibox_two(s)
  end)

end
{% endhighlight %}

The `taglist` and `tasklist` is defined on the header above.

#### The WB Object in Separate File

Unfortunately there is a tendecy that the code,
become longer as complexity added,
so we need to refactor more into different file.

We need two more helper, and each helper use the same `WB Object`.
For this to be happened,
we need to create global object called `wibox_package`.

*	[gitlab.com/.../dotfiles/.../default/statusbar.lua][dotfiles-statusbar]

{% highlight lua %}
local WB = {}

wibox_package = WB -- global object name

-- default statusbar
require("statusbar.default.helper_default")
require("statusbar.default.helper_empty")

function WB.setup_common_boxes (s)
  ...
end
{% endhighlight %}

The statusbar still have the same structure,
but we will move `generate_wibox_one` and `generate_wibox_two`,
into each respective helper.

*	[gitlab.com/.../dotfiles/.../default/helper-default.lua][dotfiles-h-default]

{% highlight lua %}
local WB = wibox_package

function WB.generate_wibox_one (s)
  ...
end
{% endhighlight %}

And respectively

*	[gitlab.com/.../dotfiles/.../default/helper-empty.lua][dotfiles-h-empty]

{% highlight lua %}
local WB = wibox_package

function WB.generate_wibox_two (s)
  ...
end
{% endhighlight %}

Now we know how it works.
Consider go further into the details.

-- -- --

<a name="main-statusbar"></a>

### 4: Main Statusbar

Remember this loop in `init`?

{% highlight lua %}
function _M.init()
  awful.screen.connect_for_each_screen(function(s)
    WB.setup_common_boxes (s)
    ...
  end)
end
{% endhighlight %}

We move each common step in loop in this function below:

{% highlight lua %}
function WB.setup_common_boxes (s)
  -- Wallpaper
  set_wallpaper(s)

  -- Create a promptbox for each screen
  s.promptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.layoutbox = awful.widget.layoutbox(s)
  s.layoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)
  ))

  -- Create a taglist widget
  s.taglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = WB.taglist
  }

  -- Create a tasklist widget
  s.tasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = WB.tasklist
  }
end
{% endhighlight %}

-- -- --

<a name="top-bar"></a>

### 5: Top Bar: Helper Default

Consider dive into detail

*	[gitlab.com/.../dotfiles/.../default/helper-default.lua][dotfiles-h-default]

#### Main Wibox

Not exactly the same as the default statusbar.
In fact, I refactor into three layouts: [left, tasklist, right].

{% highlight lua %}
function WB.generate_wibox_one (s)
  -- layout: l_left, tasklist, l_right

  -- Create the wibox
  s.wibox_top = awful.wibar({ position = "top", screen = s })

  -- Add widgets to the wibox
  s.wibox_top:setup {
    layout = wibox.layout.align.horizontal,
    WB.add_widgets_left (s),
    WB.add_widgets_middle (s),
    WB.add_widgets_right (s),
  }
end
{% endhighlight %}

#### Each Widget

And each widget has each own function:

{% highlight lua %}
function WB.add_widgets_left (s)
  return { -- Left widgets
    layout = wibox.layout.fixed.horizontal,
    RC.launcher,
    s.taglist,
    wibox.widget.textbox(" | "),
    s.promptbox,
  }
end

function WB.add_widgets_middle (s)
  return s.tasklist -- Middle widget
end

function WB.add_widgets_right (s)
  return { -- Right widgets
    layout = wibox.layout.fixed.horizontal,
    mykeyboardlayout,
    wibox.widget.systray(),
    mytextclock,
    s.layoutbox,
  }
end
{% endhighlight %}

I also rename `promptbox` from `mypromptbox`,
so you shold also change it in keybinding.

#### The Header

Do not forget the header, as usual:

{% highlight lua %}
-- Standard awesome library
local awful     = require("awful")
local beautiful = require("beautiful")

-- Wibox handling library
local wibox = require("wibox")

-- global object
local WB = wibox_package

-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()
{% endhighlight %}

-- -- --

<a name="svg-arrows"></a>

### 6: SVG Arrows

The reason I put the second panel is that,
I want to introduce some decoration.
The next statusbar might use this decoration,
and I do not want to burden the next turorial with decoration explanation.
I should settle this here, in the first place.

#### The Decoration

Consider have a look at the bottom panel code:

*	[gitlab.com/.../dotfiles/.../default/helper-default.lua][dotfiles-h-default]

{% highlight lua %}
function WB.initdeco ()
    -- Spacer
    WB.spacer = wibox.widget.textbox(" ")
    WB.spacerline = wibox.widget.textbox(" | ")

    -- Separators png
    WB.ar_lr_pre   = wibox.widget.imagebox()
    WB.ar_lr_pre:set_image(beautiful.arrow_lr_pre)
    WB.ar_lr_post  = wibox.widget.imagebox()
    WB.ar_lr_post:set_image(beautiful.arrow_lr_post)
    WB.ar_lr_thick = wibox.widget.imagebox()
    WB.ar_lr_thick:set_image(beautiful.arrow_lr_thick)
    WB.ar_lr_thin  = wibox.widget.imagebox()
    WB.ar_lr_thin:set_image(beautiful.arrow_lr_thin)
    
    WB.ar_rl_pre   = wibox.widget.imagebox()
    WB.ar_rl_pre:set_image(beautiful.arrow_rl_pre)
    WB.ar_rl_post  = wibox.widget.imagebox()
    WB.ar_rl_post:set_image(beautiful.arrow_rl_post)
    WB.ar_rl_thick = wibox.widget.imagebox()
    WB.ar_rl_thick:set_image(beautiful.arrow_rl_thick) 
    WB.ar_rl_thin  = wibox.widget.imagebox()
    WB.ar_rl_thin:set_image(beautiful.arrow_rl_thin) 
end
{% endhighlight %}

And we call it in `statusbar.lua`:

{% highlight lua %}
function _M.init()
  WB.taglist  = deco.taglist()
  WB.tasklist = deco.tasklist()

  WB.initdeco()

  awful.screen.connect_for_each_screen(function(s)
    ...
  end)
end
{% endhighlight %}

Where are this arrow variable comming from?
Of course from theme.

*	[gitlab.com/.../dotfiles/.../clone/theme.lua][dotfiles-theme]

{% highlight lua %}
theme.arrow_lr_thick     = theme_path .. "misc/clone/arrow_lr_thick.png"
theme.arrow_lr_thin      = theme_path .. "misc/clone/arrow_lr_thin.png"
theme.arrow_lr_pre       = theme_path .. "misc/clone/arrow_lr_pre.png"
theme.arrow_lr_post      = theme_path .. "misc/clone/arrow_lr_post.png"
theme.arrow_rl_thick     = theme_path .. "misc/clone/arrow_rl_thick.png"
theme.arrow_rl_thin      = theme_path .. "misc/clone/arrow_rl_thin.png"
theme.arrow_rl_pre       = theme_path .. "misc/clone/arrow_rl_pre.png"
theme.arrow_rl_post      = theme_path .. "misc/clone/arrow_rl_post.png"
{% endhighlight %}

#### Custom Arrow

I have made my own custom SVG source,
inspired by multicolor theme icons.

![SVG Source: Arrow Decoration][image-svg-arrow]{: .img-responsive }

You might want to alter the image yourself to suit your needs,
e.g. blue or pink, or minimalist shape something.

#### SVG Source

SVG source available at:

* [github.com/epsi-rns/dotfiles/.../clone-arrow.svg][dotfiles-svg-source]

-- -- --

<a name="bottom-bar"></a>

### 7: Bottom Bar: Helper Empty

Again, consider dive into detail

*	[gitlab.com/.../dotfiles/.../default/helper-default.lua][dotfiles-h-default]

#### Main Wibox

This time, we do not need middle layout.

{% highlight lua %}
function WB.generate_wibox_two (s)
  -- layout: l_left, l_mid, tasklist

  -- Create the wibox
  s.wibox_two = awful.wibar({ position = "bottom", screen = s })

  -- Add widgets to the wibox
  s.wibox_two:setup {
    layout = wibox.layout.align.horizontal,
    WB.add_widgets_monitor_left (s),
    WB.spacer,
    WB.add_widgets_monitor_right (s),
  }
end
{% endhighlight %}

#### Each Widget

So we only need two function: left and right.

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  return {
    layout = wibox.layout.fixed.horizontal,
    WB.ar_lr_post,
    WB.spacerline,
    WB.ar_lr_thin
  }
end
{% endhighlight %}

![Awesome WM: Empty: Left Bottom][image-ss-d-left]{: .img-responsive }

{% highlight lua %}
function WB.add_widgets_monitor_right (line, s)
  return {
    layout = wibox.layout.fixed.horizontal,
    WB.ar_rl_thin,
    WB.spacerline,
    WB.ar_rl_pre
  }
end
{% endhighlight %}

![Awesome WM: Empty: Right Bottom][image-ss-d-right]{: .img-responsive }

This basically, just showing those tiny arrows.

#### The Header

Just in case you forget:

{% highlight lua %}
-- Standard awesome library
local awful     = require("awful")
local beautiful = require("beautiful")

-- Wibox handling library
local wibox = require("wibox")

-- global object
local WB = wibox_package
{% endhighlight %}

-- -- --

<a name="whats-next"></a>

### What is Next?

Now that we are done with refactoring the default statusbar,
we should be ready for a more complex statusbar,
such as using Vicious library, or Lain library.

![Awesome WM: Stacked Statusbar][image-ss-stacked]{: .img-responsive }

Consider continue reading [ [Awesome WM - Statusbar - Stacked][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/11' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-statusbar' %}

[local-whats-next]: /desktop/2019/11/25/awesome-statusbar-stacked.html
[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html
[local-statusbar]:      /desktop/2019/06/19/awesome-modularized-statusbar.html


[dotfiles-config]:      {{ dotfiles }}/rc.lua
[dotfiles-theme]:       {{ dotfiles }}/themes/clone/theme.lua
[dotfiles-statusbar]:   {{ dotfiles }}/statusbar/default/statusbar.lua
[dotfiles-h-default]:   {{ dotfiles }}/statusbar/default/helper_default.lua
[dotfiles-h-empty]:     {{ dotfiles }}/statusbar/default/helper_empty.lua

[image-ss-default]:     {{ asset_path }}/04-statusbar-default.png
[image-ss-stacked]:     {{ asset_path }}/04-statusbar-stacked.png
[image-ss-d-left]:      {{ asset_path }}/04-statusbar-default-left-bottom.png
[image-ss-d-right]:     {{ asset_path }}/04-statusbar-default-right-bottom.png
[image-ss-04-panes-modules]:{{ asset_path }}/04-panes-modules-default.png

[image-svg-arrow]:      {{ asset_path }}/clone-arrow.png
[dotfiles-svg-icons]:   {{ dotfiles }}/themes/clone/misc/clone/
[dotfiles-svg-source]:  {{ dotfiles }}/themes/clone/misc/clone/clone-arrow.svg
