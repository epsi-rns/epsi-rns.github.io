---
layout: post-sidemenu-wm
title:  "Preparing Modularized Awesome WM Configuration"
categories: desktop
date      : 2016-07-13 12:50:15 +0700
tags      : [awesome]
keywords  : [modularized]
author: epsi

excerpt:
  This article explain coding style approach of
  modularized Awesome WM configuration (rc.lua)
  and the process of splitting the codes.

related_link_ids:
  - 16070650  # Modularized Structure
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

This post is about what must be done before modularizing rc.lua.
It is more about Lua Code style, and the process of splitting the codes.
And not the configuration itself.
I'm using Object Oriented Approach, a beginner perspective.

It is too early for me to release my modularized awesome WM  configuration.
This topic itself is long enough, that I don't want to mix it with another topic.


-- -- --

## Goal

> No long configuration, short enough to be analyzed

I have refactored <code class="code-file">rc.lua</code> into some files few month ago,
but it is not enough.
I'm a beginner in lua world,
and I'm not smart enough to read long source code.
So I decide to make my <code class="code-file">rc.lua</code> part smaller.
Each files should be small enough to be analyzed file by file.

-- -- --

## Problem Definition

After a hard time of reading <code class="code-file">rc.lua</code>,
I finaly realized that <code class="code-file">rc.lua</code> consist of these parts

1. Sequence of code, e.g

	* Error Handling

	* Initializing Theme

	* Signals

	* User Variables: Theme, Device, Statusbar Design

2. Defining Variable

	* Layouts and Tags

	* Menu, submenu, menu generator

	* Rules

	* GlotbalKeys, GlobalButtons

	* ClientKeys, Clientbuttons

3. Other than those two above

	* Creating Wibox: Pack sequence of code into Function

	* Miscelannous

> It takes Lua knowledge to split lua-based configuration

[![Modularized Awesome WM Code Main][image-ss-awesome-main]{: .img-responsive }][photo-ss-awesome-fullscreen]

-- -- --

## Loading module in Lua

There is a strange concept of array in Lua called Table.
Table is a container of associative array stored in key/value pairs.
Since Module in Lua is just container of stuff.
Then Module in Lua is just a Table.

You can call a module by simply use the <code>require</code> keyword.

Optionally, when calling a Module, it can be stored in a variable,
this variable can be thought as an alias of modules.

{% highlight lua %}
-- Required libraries
local awful     = require("awful")
local beautiful = require("beautiful")
local menubar = require("menubar")
naughty = require('naughty')
{% endhighlight %}

-- -- --

## Splitting Config

Splitting configuration source code is easy,
the 'dofile' function can do the horsework.

This <code>dofile</code> simply run another lua file,
and doesn't do any check like <code>require</code> does.

{% highlight lua %}
local config_path = awful.util.getdir("config") .. "/"

dofile(config_path .. "main/error-handling.lua")
dofile(config_path .. "main/theme.lua")

{% endhighlight %}

Now I can move-and-paste the lua-code
from <code class="code-file">rc.lua</code> to other lua file.

Now let's see what is in this file <code class="code-file">main/theme.lua</code>.

Configuration Source Code: [main/theme.lua][source-main-theme]

{% highlight lua %}
home = os.getenv("HOME")

-- Themes define colours, icons, font and wallpapers.
beautiful.init("/usr/share/awesome/themes/default/theme.lua")


if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
{% endhighlight %}

-- -- --

### Require in Each Lua

There is something you should do each time
move-and-paste lua-code.
You should also move the required module.

In that configuration above,
it needs some <code>beautiful</code> and <code>gears</code>.

{% highlight lua %}
---- Standard awesome library
local gears     = require("gears")

-- Theme handling library
local beautiful = require("beautiful")

{% endhighlight %}


## Module Containing Variable

The next steps is move each variables to Lua files.

Sample Configuration Source Code: [main/user-variables.lua][source-user-vars]

Let's say we have terminal variable

{% highlight lua %}
terminal = "xfce4-terminal"

{% endhighlight %}

We can just move-and-paste to a file, e.g <code class="code-file">myvar.lua</code>
and let the variable be a global variable.

{% highlight lua %}
terminal = "xfce4-terminal"

{% endhighlight %}

As n00b coming from other language,
I avoid global variable as possible.

There are some alternative on how to make module in Lua

### Lua 5.0, using module function

{% highlight lua %}
module(main.myvar)

function get_terminal()
  return "xfce4-terminal"
end
{% endhighlight %}

This is deprecated.

### Lua 5.2, using table

We can use any table name, e.g. <code>_M</code>

{% highlight lua %}
local _M = {}

function _M.get_terminal()
  return "xfce4-terminal"
end

return _M
{% endhighlight %}

And in main <code class="code-file">rc.lua</code>, we can call

{% highlight lua %}
local myvar = require("main.myvar")

local terminal = myvar.get_terminal()

{% endhighlight %}

I also made the terminal in <code class="code-file">rc.lua</code> as local.

-- -- --

## Module Containing Only One Variable

For module with only one variable,
We can also make the call simple.

{% highlight lua %}
local _M = {}

function _M.get()
  return "xfce4-terminal"
end

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
{% endhighlight %}

And in main <code class="code-file">rc.lua</code>, we can call

{% highlight lua %}
local myvar = require("main.myvar")

local terminal = myvar()
{% endhighlight %}

No need to call <code>get()</code> explicitly.

-- -- --

## Real lua.rc Sample

Let's see our globalbutton moved to <code class="code-file">main/globalbuttons.lua</code>.
Do not confuse the name globalbuttons with global variables.
It is just a variable name, it could be global or local.

[![Modularized Awesome WM Code Module][image-ss-awesome-module]{: .img-responsive }][photo-ss-awesome-fullscreen]

### Original in rc.lua.

{% highlight lua %}
root.keys(awful.util.table.join(
  awful.button({ }, 3, function () mymainmenu:toggle() end),
  awful.button({ }, 4, awful.tag.viewnext),
  awful.button({ }, 5, awful.tag.viewprev)
))
{% endhighlight %}

### Splitted from rc.lua.

To <code class="code-file">binding/globalbuttons.lua</code>

Configuration Source Code: [binding/globalbuttons.lua][source-globalbuttons]

{% highlight lua %}
local awful = require("awful")

local _M = {}

function _M.get()
  local globalbuttons = awful.util.table.join(
      awful.button({ }, 3, function () mymainmenu:toggle() end),
      awful.button({ }, 4, awful.tag.viewnext),
      awful.button({ }, 5, awful.tag.viewprev)
  )

  return globalbuttons
end

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
{% endhighlight %}

### Calling from rc.lua

{% highlight lua %}
local binding = {
  globalbuttons = require("binding.globalbuttons")
}

-- Set root
root.buttons(binding.globalbuttons())
{% endhighlight %}

-- -- --

## Splitting Long Module

Not everything should be packed with these code style.
It all depends on your creativity and imagination.

I once had my wibox statusbar configuration.
I have packed some plain sequence of code into bunch of function.
Short enough for expert, but long enough for me,
so I move some function into a helper.

Let's see the code.
Instead of the _M table that can be returned as public.
I also make a WB table that is private, only visible for this module.

Note how I declare global table wibox_package as a bridge between these two Lua file.

in <code class="code-file">anybox/arrow/statusbar.lua</code>

Configuration Source Code: [statusbar.lua][source-statusbar]

{% highlight lua %}
local _M = {}

local WB = {}
wibox_package = WB               -- global object name

local config_path = awful.util.getdir("config") .. "/anybox/"
dofile(config_path .. "helper.lua")

WB.generate_wibox_top = function (s)
  ...
end  

WB.generate_wibox_bottom = function (s)
  ...
end

function _M.init()
  ...
end

return _M
{% endhighlight %}

in <code class="code-file">anybox/arrow/helper.lua</code>

Configuration Source Code: [helper.lua][source-helper]

{% highlight lua %}
local WB = wibox_package

function WB.initdeco() ... end
function WB.updatelayoutbox(layout, s) ... end
WB.setup_common_boxes = function (s) ... end
WB.vicious_widgets = function (screen) ... end
WB.multicolor_widgets_top = function (screen) ... end

{% endhighlight %}

-- -- --

## Module with many containers

The issue goes further when I decorate Wibox.
It has a bunch of monitoring stuff,
e.g cpu, mem, netup, netdown, time, battery, diskfree, alsabar, mpd and so on.
Each widget utilize icon and text.
For example cpu, it has memicon and memtext.

Instead of using namespace as memicon and cmemtext.
I'm using table approach <code>I.mem</code>, and <code>W.mem</code>.

I'm using global table, instead of <code>local _M</code> table.

in <code class="code-file">anybox/lain/lain.lua</code>

Configuration Source Code: [lain.lua][source-lain]

{% highlight lua %}

local W = {}
lain_widget_set = W           -- object name

local I = {}
lain_icon_set = I             -- object name

I.weather = ...
W.weather = ...

I.mem = ...
W.mem = ...

...

{% endhighlight %}

I also move long chunks, that require a lot of variable.

in <code class="code-file">anybox/lain/lain-diskfree</code>

Configuration Source Code: [lain-diskfree.lua][source-laindiskfree]

{% highlight lua %}
I.disk = ...

W.disk_bar = ...
W.disk_margin = wibox.layout.margin(W.disk_bar, 2, 7)

local disk_widget_settings = function()
  ...
end

W.disk_widget_update = lain.widgets.fs({
  ...
})

W.disk_bar_widget = wibox.widget.background(W.disk_margin)
W.disk_bar_widget:set_bgimage(...)

{% endhighlight %}

That's all for now.

It doesn't look complicated once,
you get in to the source code.

In fact the source configuration is easier to be read now.

## Configuration Source

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome>

-- -- --


This the result.

{% highlight bash %}
.
├── rc.lua
│
├── main
│   ├── error-handling.lua
│   ├── layouts.lua
│   ├── menu.lua
│   ├── rules.lua
│   ├── signals.lua
│   ├── tags.lua
│   ├── theme.lua
│   └── user-variables.lua
├── binding
│   ├── bindtotags.lua
│   ├── clientbuttons.lua
│   ├── clientkeys.lua
│   ├── globalbuttons.lua
│   ├── globalkeys.lua
│   ├── taglist.lua
│   └── tasklist.lua
├── modules
│   └── menugen
├── themes
│   └── clone
└── anybox
    ├── titlebar.lua
    ├── simple
    │   └── statusbar.lua
    └── *
{% endhighlight %}

-- -- --

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/awesome/3.5' %}

[image-ss-awesome-main]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-modularized-code-main.png
[image-ss-awesome-module]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-modularized-code-binding.png
[photo-ss-awesome-fullscreen]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPPFTkZLKgqQVrvGwBn4nYJo74xIf-IATzz7YlG

[source-main-theme]:    {{ dotfiles_path }}/main/theme.lua
[source-user-vars]:     {{ dotfiles_path }}/main/user-variables.lua
[source-globalbuttons]: {{ dotfiles_path }}/binding/globalbuttons.lua
[source-statusbar]:     {{ dotfiles_path }}/anybox/arrow/statusbar.lua
[source-helper]:        {{ dotfiles_path }}/anybox/arrow/helper.lua
[source-lain]:          {{ dotfiles_path }}/anybox/lain/lain.lua
[source-laindiskfree]:  {{ dotfiles_path }}/anybox/lain/lain-diskfree.lua
