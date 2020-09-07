---
layout     : post
title      :  "Awesome WM - Statusbar Modules"
categories : desktop
date       : 2019-06-19 09:25:15 +0700
tags       : [awesome]
keywords   : [tiling, window manager, modularized, lua]
author     : epsi
toc        : toc/2019/06/toc-awesome.html

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

<a name="preface"></a>

### Preface

> Goal: Statusbar and titlebar binding modules in modularized configuration.

#### Table of Content

* [Preface](#preface): Table of Content

* [Artefacts](#artefacts)

* 1: [Titlebar](#titlebar)

* 2: [Statusbar Skeleton](#statusbar-skeleton)

* 3: [Wallpaper](#wallpaper)

* 4: [Binding: Tag List Buttons](#tag-list-buttons)

* 5: [Binding: Task List Buttons](#task-list-buttons)

* 6: [Statusbar Summary](#statusbar-summary)

* [What is Next?](#whats-next)

-- -- --

<a name="artefacts"></a>

### Artefacts

Statusbar is a complex task that deserve its own article.
Both statusbar and titlebar, are using wibox.
We need to examine the titlebar first, and then continue exploring statusbar.

So far, statusbar can be refactored into four modules:
`statusbar` itself, then `taglist`, `tasklist`, `wallpaper`.
Because each `statusbar` can contain its own wallpaper.

{% highlight bash %}
$ tree deco/
deco/
├── statusbar.lua
├── taglist.lua
├── tasklist.lua
├── titlebar.lua
└── wallpaper.lua

0 directories, 5 files

{% endhighlight %}

![Awesome WM: Artefacts in Binding][image-ss-01-tree-deco]{: .img-responsive }

#### Calling Script

We are going to call these scripts in `deco/statusbar.lua`.

{% highlight lua %}
-- Custom Local Library: Common Functional Decoration
local deco = {
  wallpaper = require("deco.wallpaper"),
  taglist   = require("deco.taglist"),
  tasklist  = require("deco.tasklist")
}
{% endhighlight %}

And call the `deco/titlebar.lua` script in `theme` later.

![Awesome WM: ViM Panes: Statusbars and Titlebars Modules][image-ss-01-panes-modules]{: .img-responsive }

Do not get intimidated with codes above.
These can be explained step by step.

-- -- --

<a name="titlebar"></a>

### 1: Titlebar

Title bar script is responsible for configuring:

* Create titlebar Widget

* Button bindings for the titlebar

#### Module Script

*	[gitlab.com/.../dotfiles/.../deco/titlebar.lua][dotfiles-titlebar]

{% highlight lua %}
-- module("anybox.titlebar", package.seeall)

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Widget and layout library
local wibox = require("wibox")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    -- create titlebar Widget
end)
{% endhighlight %}

#### Calling Script

*	[gitlab.com/.../dotfiles/.../main/signals.lua][dotfiles-signals]

In signal module:

{% highlight lua %}
-- Custom Local Library: Common Functional Decoration
require("deco.titlebar")
{% endhighlight %}

#### Create Titlebar Widget

{% highlight lua %}
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )
{% endhighlight %}

#### Button Bindings for the Titlebar

We have three parts in titlebar:

* Left: Icon

* Middle: Title

* Right: Buttons

{% highlight lua %}
    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
{% endhighlight %}

![Awesome WM: Wibox Layout in Titlebar][image-ss-01-titlebar]{: .img-responsive }

#### Wibox

Now we have already introduced `wibox` layout as below:

{% highlight lua %}
layout = wibox.layout.align.horizontal
{% endhighlight %}

We are going to work complex wibox later,
to create nice looking custom panel.

-- -- --

<a name="statusbar-skeleton"></a>

### 2: Statusbar Skeleton

As a summary we need to preview the sekeleton of the configuration.

#### Module Script

*	[gitlab.com/.../dotfiles/.../deco/statusbar.lua][dotfiles-statusbar]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Wibar

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper

  -- Create a promptbox for each screen

  -- Create an imagebox widget 

  -- Create a taglist widget

  -- Create a tasklist widget

  -- Create the wibox

  -- Add widgets to the wibox
end)
{% endhighlight %}

#### Calling Script

In main configuration:

{% highlight lua %}
-- Statusbar: Wibar
require("deco.statusbar")
{% endhighlight %}

This is enough for now,
we are going to revisit this script,
by the end of this article.

-- -- --

<a name="wallpaper"></a>

### 3: Wallpaper

#### Module Script

*	[gitlab.com/.../dotfiles/.../deco/wallpaper.lua][dotfiles-wallpaper]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local beautiful = require("beautiful")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)
{% endhighlight %}

#### Calling Script

In statusbar module:

{% highlight lua %}
-- Custom Local Library: Common Functional Decoration
local deco = {
  wallpaper = require("deco.wallpaper")
}

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

end)
{% endhighlight %}

As you can see i code above,
the wallpaper use <code>s</code> argument.
The wallpaper set for each screen.

-- -- --

<a name="tag-list-buttons"></a>

### 4: Binding: Tag List Buttons

This is binding button for tag list in statusbar.

#### Module Script

*	[gitlab.com/.../dotfiles/.../deco/taglist.lua][dotfiles-taglist]

{% highlight lua %}
-- Required libraries
local gears = require("gears")
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  -- Create a wibox for each screen and add it
  local taglist_buttons = gears.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
      if client.focus then
        client.focus:move_to_tag(t)
      end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
      if client.focus then
       client.focus:toggle_tag(t)
      end
    end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
  )

  return taglist_buttons
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

__.__

#### Calling Script

In statusbar module:

{% highlight lua %}
-- Custom Local Library: Common Functional Decoration
local deco = {
  wallpaper = require("deco.wallpaper"),
  taglist   = require("deco.taglist")
}

local taglist_buttons  = deco.taglist()

awful.screen.connect_for_each_screen(function(s)
  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = taglist_buttons
  }
end)
{% endhighlight %}

-- -- --

<a name="task-list-buttons"></a>

### 5: Binding: Task List Buttons

This is binding button for task list in statusbar.

#### Module Script

*	[gitlab.com/.../dotfiles/.../deco/tasklist.lua][dotfiles-tasklist]

{% highlight lua %}
-- Required libraries
local gears = require("gears")
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local tasklist_buttons = gears.table.join(
    awful.button({ }, 1, function (c)
      if c == client.focus then
        c.minimized = true
      else
        c:emit_signal(
          "request::activate",
          "tasklist",
          {raise = true}
        )
      end
    end),
    awful.button({ }, 3, function()
      awful.menu.client_list({ theme = { width = 250 } })
    end),
    awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
    end),
    awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
    end)
  )

  return tasklist_buttons
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

__.__

#### Calling Script

In statusbar module:

{% highlight lua %}
-- Custom Local Library: Common Functional Decoration
local deco = {
  wallpaper = require("deco.wallpaper"),
  taglist   = require("deco.taglist"),
  tasklist  = require("deco.tasklist")
}

local taglist_buttons  = deco.taglist()
local tasklist_buttons = deco.tasklist()

awful.screen.connect_for_each_screen(function(s)
  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = tasklist_buttons
  }
end)
{% endhighlight %}

-- -- --

<a name="statusbar-summary"></a>

### 6: Statusbar Summary

Now that all parts are ready,
we need to gather them as a complete wibox.

![Awesome WM: Wibox Layout in Statusbar][image-ss-01-statusbar]{: .img-responsive }

#### Wibox

{% highlight lua %}
-- Wibox handling library
local wibox = require("wibox")

-- Create a textclock widget
mytextclock = wibox.widget.textclock()

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget 
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)
  ))

  -- Create a taglist widget
  -- s.mytaglist = ...

  -- Create a tasklist widget
  -- s.mytasklist = ...

  -- Create the wibox
  s.mywibox = awful.wibar({ position = "top", screen = s })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      RC.launcher,
      s.mytaglist,
      s.mypromptbox,
    },
    s.mytasklist, -- Middle widget
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      mykeyboardlayout,
      wibox.widget.systray(),
      mytextclock,
      s.mylayoutbox,
    },
  }
end)
{% endhighlight %}

That is all, for simple taskbar.

Notice the variable that renamed from `mylauncher` to `RC.launcher`.

#### Module Script

As a summary, here is the module script

*	[gitlab.com/.../dotfiles/.../deco/statusbar.lua][dotfiles-statusbar]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful     = require("awful")

-- Wibox handling library
local wibox = require("wibox")

-- Custom Local Library: Common Functional Decoration
local deco = {
  wallpaper = require("deco.wallpaper")
  taglist   = require("deco.taglist"),
  tasklist  = require("deco.tasklist")
}

local taglist_buttons  = deco.taglist()
local tasklist_buttons = deco.tasklist()

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Wibar

-- Create a textclock widget
mytextclock = wibox.widget.textclock()

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)
  ))

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = taglist_buttons
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = tasklist_buttons
  }

  -- Create the wibox
  s.mywibox = awful.wibar({ position = "top", screen = s })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      RC.launcher,
      s.mytaglist,
      s.mypromptbox,
    },
    s.mytasklist, -- Middle widget
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      mykeyboardlayout,
      wibox.widget.systray(),
      mytextclock,
      s.mylayoutbox,
    },
  }
end)
{% endhighlight %}

-- -- --

<a name="whats-next"></a>

### What is Next?

Consider continue reading [ [Awesome WM - Theme - Main][local-whats-next] ].
There are, some interesting topic, Awesome WM theme step by step.
Refactoring <code>Theme</code> configuration using <code>Lua</code>.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/06' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3' %}

[local-whats-next]: /desktop/2019/10/20/awesome-theme-main.html

[image-ss-01-tree-deco]:{{ asset_path }}/01-tree-deco.png
[image-ss-01-titlebar]: {{ asset_path }}/01-titlebar.png
[image-ss-01-statusbar]:{{ asset_path }}/01-statusbar.png
[image-ss-01-panes-modules]:{{ asset_path }}/01-panes-modules-deco.png

[image-ss-01-help-simple]:  {{ asset_path }}/01-awesome-help-simple.png
[image-ss-01-help-standard]:{{ asset_path }}/01-awesome-help-standard.png

[dotfiles-config]:          {{ dotfiles }}/rc.lua
[dotfiles-signals]:         {{ dotfiles }}/main/signals.lua
[dotfiles-statusbar]:       {{ dotfiles }}/deco/statusbar.lua
[dotfiles-taglist]:         {{ dotfiles }}/deco/taglist.lua
[dotfiles-tasklist]:        {{ dotfiles }}/deco/tasklist.lua
[dotfiles-titlebar]:        {{ dotfiles }}/deco/titlebar.lua
[dotfiles-wallpaper]:       {{ dotfiles }}/deco/wallpaper.lua
