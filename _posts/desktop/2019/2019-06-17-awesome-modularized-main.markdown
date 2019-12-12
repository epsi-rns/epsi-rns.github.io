---
layout: post
title:  "Awesome WM - Main Modules"
categories: desktop
date      : 2019-06-17 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/06/01-menu-custom.png

excerpt:
  Awesome WM customization step by step.
  Main modules in modularized configuration.

related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

{% include post/2019/06/toc-awesome.html %}

### Preface

> Goal: Main modules in modularized configuration.

### Artefacts

Now consider have a walk, moving some codes to files in main directory.

{% highlight bash %}
$ tree main
main
├── error-handling.lua
├── layouts.lua
├── menu.lua
├── rules.lua
├── signals.lua
├── tags.lua
└── user-variables.lua

0 directories, 7 files
{% endhighlight %}

![Awesome WM: Artefacts in Main][image-ss-01-tree-main]{: .img-responsive }

We have already moved `error-handling.lua`
and `user-variables.lua` in previous article.
We will discuss here about the rest five: `layouts`,
`tags`, `menu`, `rules`, and `signal`.

#### Table of Content

* Preface: Table of Content

* 1: Layouts

* 2: Tags

* 3: Menu

* 4: Rules

* 5: Signals

* What is Next ?

-- -- --

### 1: Layouts

The layout icon can be seen in top-right statusbar.

![Awesome WM: Layout Icon][image-ss-01-layout]{: .img-responsive }

#### Module Script

You can add or remove, layout using this script below:

*	[gitlab.com/.../dotfiles/.../main/layout.lua][dotfiles-layout]

{% highlight lua %}
-- Standard awesome library
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get ()
  -- Table of layouts to cover with awful.layout.inc, order matters.
  local layouts = {
    awful.layout.suit.floating,           -- 1:

    awful.layout.suit.tile,               -- 2:
    awful.layout.suit.tile.left,          -- 3:
    awful.layout.suit.tile.bottom,        -- 4:
    awful.layout.suit.tile.top,           -- 5:

    awful.layout.suit.fair,               -- 6:
    awful.layout.suit.fair.horizontal,    -- 7:

    awful.layout.suit.spiral,             -- 8:
    awful.layout.suit.spiral.dwindle,     -- 9:

    awful.layout.suit.max,                -- 10:
    awful.layout.suit.max.fullscreen,     -- 11:
    awful.layout.suit.magnifier,          -- 12:

    awful.layout.suit.corner.nw           -- 13:
--  awful.layout.suit.corner.ne,
--  awful.layout.suit.corner.sw,
--  awful.layout.suit.corner.se,
  }

  return layouts
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable(
  {}, 
  { __call = function(_, ...) return _M.get(...) end }
)
{% endhighlight %}

Now we have these available: `floating`, `tile`, `tile,left`,
`tile.bottom`, `tile.top`, `fair`, `fair.horizontal`,
`spiral`, `spiral.dwindle`, `max`, `max.fullscreen`, `magnifier`,
`corner.nw`. You can also add layout using other third party library.

_._

I put number a comment,
because I want to give different layout for different tag.
what I mean is, this comment is required.

#### Layout Preview

It is easier to understand the layoout using image.

![Awesome WM: Layouts][image-layouts-animate]{: .img-responsive }

#### Calling Script

In main configuration:

{% highlight bash %}
-- Custom Local Library
local main = {
  layouts = require("main.layouts")
}

-- Layouts
RC.layouts = main.layouts()
{% endhighlight %}

-- -- --

### 2: Tags

#### Standar Tag

Standard tags are using number.

![Awesome WM: Tags Number][image-ss-01-tags-number]{: .img-responsive }

*	[gitlab.com/.../dotfiles/.../main/tags.lua][dotfiles-tags]

{% highlight lua %}
-- Standard awesome library
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get ()
  tags = {}

  awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
    tags[s] = awful.tag(
      { "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, RC.layouts[1]
    )
  end)
  
  return tags
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable(
  {}, 
  { __call = function(_, ...) return _M.get(...) end }
)
{% endhighlight %}

__.__

Remember that <code>RC.layouts[1]</code> is,
<code>awful.layout.suit.floating</code>.

#### Calling Script

In main configuration:

{% highlight bash %}
-- Custom Local Library
local main = {
  layouts = require("main.layouts"),
  tags    = require("main.tags")
}

-- Tags
RC.tags = main.tags()
{% endhighlight %}

#### Custom Tag

What about different tag names?

![Awesome WM: Tags Takao][image-ss-01-tags-takao]{: .img-responsive }

Well, you can rewrite the script as below:

{% highlight lua %}
function _M.get ()
  local tags = {}

  local tagpairs = {
--  names  = { "term", "net", "edit", "place", 5, 6, 7, 8, 9 },
    names  = {
      "❶ 一 term", "❷ 二 net", "❸ 三 edit",
      "❹ 四 place", "❺ 五 mail",
      " ❻ 六", "❼ 七", " ❽ 八", "❾ 九" },

    layout = {
      RC.layouts[1], RC.layouts[2], RC.layouts[4],
      RC.layouts[5], RC.layouts[6], RC.layouts[12],
      RC.layouts[9], RC.layouts[3], RC.layouts[7]
    }
  }

  awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tagpairs.names, s, tagpairs.layout)
  end)
  
  return tags
end
{% endhighlight %}

_._

-- -- --

### 3: Menu

This menu is very helpful for beginner.
You can setup manually like what this article explain,
or you can use third party to build XDG menu,
for works with Awesome WM.

#### Module Script: Initialization.

This script is a little bit long.
It takes some libraries to be loaded,
and predefined variables to be set.

*	[gitlab.com/.../dotfiles/.../main/menu.lua][dotfiles-menu]

{% highlight lua %}
-- Standard awesome library
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- Theme handling library
local beautiful = require("beautiful") -- for awesome.icon

local M = {}  -- menu
local _M = {} -- module

-- reading
-- https://awesomewm.org/apidoc/popups%20and%20bars/awful.menu.html

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- This is used later as the default terminal and editor to run.
-- local terminal = "xfce4-terminal"
local terminal = RC.vars.terminal

-- Variable definitions
-- This is used later as the default terminal and editor to run.
local editor = os.getenv("EDITOR") or "nano"
local editor_cmd = terminal .. " -e " .. editor
{% endhighlight %}

#### Module Script: Configuration Values

Right below the initialization,
lays code that set configuration values.

{% highlight lua %}
M.awesome = {
  { "hotkeys", function() 
      hotkeys_popup.show_help(nil, awful.screen.focused()) 
    end },
  { "manual", terminal .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "Terminal", terminal },
  { "Shutdown/Logout", "oblogout" },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end }
}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()

  -- Main Menu
  local menu_items = {
    { "awesome", M.awesome, beautiful.awesome_subicon },
    { "open terminal", terminal }
  }

  return menu_items
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable(
  {}, 
  { __call = function(_, ...) return _M.get(...) end }
)
{% endhighlight %}

__.__

And the result is:

![Awesome WM: Menu Simple][image-ss-01-menu-simple]{: .img-responsive }

#### Calling Script

In main configuration:

{% highlight bash %}
-- Custom Local Library
local main = {
  layouts = require("main.layouts"),
  tags    = require("main.tags"),
  menu    = require("main.menu")
}

-- Menu
RC.mainmenu = awful.menu({ items = main.menu() })
RC.launcher = awful.widget.launcher(
  { image = beautiful.awesome_icon, menu = RC.mainmenu }
)
menubar.utils.terminal = RC.vars.terminal
{% endhighlight %}

#### Custom Menu

Of course we can append menu items.
It is easy actually, by adding these setting.

{% highlight lua %}
M.favorite = {
  { "caja", "caja" },
  { "thunar", "thunar" },
  { "geany", "geany" },
  { "clementine", "clementine" },
  { "firefox", "firefox", awful.util.getdir("config") .. "/firefox.png" },
  { "chromium", "chromium" },
  { "&firefox", "firefox" },
  { "&thunderbird", "thunderbird" },
  { "libreoffice", "libreoffice" },
  { "transmission", "transmission-gtk" },
  { "gimp", "gimp" },
  { "inkscape", "inkscape" },
  { "screenshooter", "xfce4-screenshooter" }
}

M.network_main = {
  { "wicd-curses", "wicd-curses" },
  { "wicd-gtk", "wicd-gtk" }
}
{% endhighlight %}

And add those variables above in main menu.

{% highlight lua %}
function _M.get()

  -- Main Menu
  local menu_items = {
    { "awesome", M.awesome, beautiful.awesome_subicon },
    { "open terminal", terminal },
    { "network", M.network_main },
    { "favorite", M.favorite }
  }

  return menu_items
end
{% endhighlight %}

And the result is:

![Awesome WM: Menu Custom][image-ss-01-menu-custom]{: .img-responsive }

-- -- --

### 4: Rules

I mostly just cut and paste the rule.
This is long, it has long documentation,
and maybe deserve its own article.

*	[gitlab.com/.../dotfiles/.../main/rules.lua][dotfiles-rules]

{% highlight lua %}
-- Standard awesome library
local awful     = require("awful")
-- Theme handling library
local beautiful = require("beautiful")

local _M = {}

-- reading
-- https://awesomewm.org/apidoc/libraries/awful.rules.html

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get(clientkeys, clientbuttons)
  local rules = {

    -- All clients will match this rule.
    { rule = { },
      properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus     = awful.client.focus.filter,
        raise     = true,
        keys      = clientkeys,
        buttons   = clientbuttons,
        screen    = awful.screen.preferred,
        placement = awful.placement.no_overlap+awful.placement.no_offscreen
      }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      },
      properties = { 
        floating = true 
      }
    },

    -- Add titlebars to normal clients and dialogs
    { rule_any = {
        type = { "normal", "dialog" }
      }, 
      properties = { 
        titlebars_enabled = true
      }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },

  }

  return rules
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable(
  {}, 
  { __call = function(_, ...) return _M.get(...) end }
)
{% endhighlight %}

__.__

#### Calling Script

In main configuration:

{% highlight bash %}
-- Custom Local Library
local main = {
  layouts = require("main.layouts"),
  tags    = require("main.tags"),
  menu    = require("main.menu"),
  rules   = require("main.rules"),
}

-- Rules
awful.rules.rules = main.rules(clientkeys, clientbuttons)
{% endhighlight %}

You can later change the code to

{% highlight bash %}
-- Rules
awful.rules.rules = main.rules(
  binding.clientkeys(),
  binding.clientbuttons()
)
{% endhighlight %}

But now, leave it that way.
Since we haven't touch the <code>binding</code> section.

-- -- --

### 5: Signals

On most my Awesome WM time,
I rarely make any customization in this signal section.

*	[gitlab.com/.../dotfiles/.../main/signals.lua][dotfiles-signals]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  -- if not awesome.startup then awful.client.setslave(c) end

  if awesome.startup
    and not c.size_hints.user_position
    and not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
  end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
{% endhighlight %}

I put titlebar signal in other files.

{% highlight lua %}
-- Custom Local Library: Common Functional Decoration
require("deco.titlebar")
{% endhighlight %}

You can choose not to have titlebar.
People doing ricing, love borderless window.

#### Calling Script

In main configuration:

{% highlight lua %}
-- Signals
require("main.signals")
{% endhighlight %}

Since it does not return value,
that code above is enough.

-- -- --

### What is Next ?

Consider continue reading [ [Awesome WM - Binding Modules][local-whats-next] ].
There are, some interesting topic,
about refactoring<code>Awesome WM</code> using <code>Lua</code>.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/06' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3' %}

[local-whats-next]: /desktop/2019/06/18/awesome-modularized-binding.html

[image-ss-01-tree-main]:    {{ asset_path }}/01-tree-main.png
[image-ss-01-layout]:       {{ asset_path }}/01-layout.png
[image-ss-01-tags-number]:  {{ asset_path }}/01-tags-number.png
[image-ss-01-tags-takao]:   {{ asset_path }}/01-tags-takao.png
[image-ss-01-menu-simple]:  {{ asset_path }}/01-menu-simple.png
[image-ss-01-menu-custom]:  {{ asset_path }}/01-menu-custom.png
[image-layouts-animate]:    {{ asset_path }}/clone-layouts-animate.gif

[dotfiles-config]:      {{ dotfiles }}/rc.lua
[dotfiles-layout]:      {{ dotfiles }}/main/layout.lua
[dotfiles-tags]:        {{ dotfiles }}/main/tags.lua
[dotfiles-menu]:        {{ dotfiles }}/main/menu.lua
[dotfiles-rules]:       {{ dotfiles }}/main/rules.lua
[dotfiles-signals]:     {{ dotfiles }}/main/signals.lua
