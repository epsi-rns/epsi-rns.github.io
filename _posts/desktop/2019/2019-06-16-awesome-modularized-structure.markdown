---
layout: post
title:  "Awesome WM - Modularized Structure"
categories: desktop
date      : 2019-06-16 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/06/01-menu-custom.png

excerpt:
  Awesome WM customization step by step.
  Preparing modularized configuration.
  Examine folder structure.
  
related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

### Preface

> Goal: Preparing modularized configuration by examining folder structure.


#### Table of Content

* Preface: Table of Content

* 1: Config Comparation

* 2: Directory Structure

* 3: Passing Value from Module

* What is Next ?

-- -- --

### 1: Config Comparation

As already said, Awesome configuration is long as 564 lines, 
as you can see in original provided configuration for 4.3 series below:

*	[gitlab.com/.../dotfiles/.../awesome/4.3/rc.43.lua][dotfiles-config-43]

After modularized, the config would be,
similar as 80 lines of code as below:

{% highlight lua %}
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Theme handling library
local beautiful = require("beautiful")

-- Miscellanous awesome library
local menubar = require("menubar")

RC = {} -- global namespace, on top before require any modules
RC.vars = require("main.user-variables")
modkey = RC.vars.modkey

-- Error handling
require("main.error-handling")

-- Variable definitions
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.wallpaper = RC.vars.wallpaper

-- -- --

-- Calling All Module Libraries

-- Custom Local Library
local main = {
  layouts = require("main.layouts"),
  tags    = require("main.tags"),
  menu    = require("main.menu"),
  rules   = require("main.rules"),
}

-- Custom Local Library: Keys and Mouse Binding
local binding = {
  globalbuttons = require("binding.globalbuttons"),
  clientbuttons = require("binding.clientbuttons"),
  globalkeys    = require("binding.globalkeys"),
  bindtotags    = require("binding.bindtotags"),
  clientkeys    = require("binding.clientkeys")
}

-- Layouts
RC.layouts = main.layouts()

-- Tags
RC.tags = main.tags()

-- Menu
RC.mainmenu = awful.menu({ items = main.menu() }) -- in globalkeys
RC.launcher = awful.widget.launcher(
  { image = beautiful.awesome_icon, menu = RC.mainmenu }
)
menubar.utils.terminal = RC.vars.terminal

-- Mouse and Key bindings
RC.globalkeys = binding.globalkeys()
RC.globalkeys = binding.bindtotags(RC.globalkeys)

-- Set root
root.buttons(binding.globalbuttons())
root.keys(RC.globalkeys)

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- Statusbar: Wibar
require("deco.statusbar")

-- Rules
awful.rules.rules = main.rules(
  binding.clientkeys(),
  binding.clientbuttons()
)

-- Signals
require("main.signals")
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../awesome/4.3/rc.lua][dotfiles-config]

The main <code>rc.lua</code>,
should only contain skeleton of the configuration.

-- -- --

### 2: Directory Structure

Beside the main <code>rc.lua</code>,
all codes resides in these three folders:

* main

* bindings

* deco

Of course you can use any folder name that suitable for you,
and even use very different structure,
other than provided in this article.

#### Directory Tree

I try not to make any customization at all,
so that the structure should reflect,
the original configuration provided by Awesome WM.

{% highlight bash %}
$ tree
.
├── binding
│   ├── bindtotags.lua
│   ├── clientbuttons.lua
│   ├── clientkeys.lua
│   ├── globalbuttons.lua
│   └── globalkeys.lua
├── deco
│   ├── statusbar.lua
│   ├── taglist.lua
│   ├── tasklist.lua
│   ├── titlebar.lua
│   └── wallpaper.lua
├── main
│   ├── error-handling.lua
│   ├── layouts.lua
│   ├── menu.lua
│   ├── rules.lua
│   ├── signals.lua
│   ├── tags.lua
│   └── user-variables.lua
└── rc.lua

3 directories, 18 files
{% endhighlight %}

![Awesome WM: Directory Structure][image-ss-01-tree]{: .img-responsive }

This is the basic structure. Structure may grow as needed.
There will be other structure, for Awesome WM customization,
in other articles.

-- -- --

### 3: Passing Value from Module

Moving code can be easy, or though task, depend on each case.
For most part there are only these two cases:

* just cut-then-paste, and done

* require using metatable, usually when returning values.

#### Module: Error Handling

Consider jump with an example.

> Always remember to add required library

Moving code is just a matter of cut-then-paste,
from the main <code>rc.lua</code>,
to related module.

*	[gitlab.com/.../dotfiles/.../main/error-handling.lua][dotfiles-error-handling]

{% highlight bash %}
-- Notification library
local naughty = require("naughty")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({ 
    preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors
  })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function (err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify({
      preset = naughty.config.presets.critical,
      title = "Oops, an error happened!",
      text = tostring(err) 
    })
    in_error = false
  end)
end
{% endhighlight %}

The only thing different is, you still have to add this.

{% highlight bash %}
-- Notification library
local naughty = require("naughty")
{% endhighlight %}

#### Calling Module without Passing Value

In the main <code>rc.lua</code>,
you can call the moudle by either of using <code>do</code>:

{% highlight bash %}
local config_path = awful.util.getdir("config") .. "/"

-- Error handling
dofile(config_path .. "main/error-handling.lua")
{% endhighlight %}

Or <code>require</code>:

{% highlight bash %}
-- Error handling
require("main.error-handling")
{% endhighlight %}

I found that <code>require</code> is easier.
<code>require</code> also use <code>dot (.)</code>,
so you do not need to worry about using slash in linux,
or backslash in windows.

This way is sufficient,
since it doesn't need any value to be returned, from this module

#### Module: User Variables

Returning value require different approach.
Consider moving user variables into modules.

*	[gitlab.com/.../dotfiles/.../main/user-variables.lua][dotfiles-user-variables]

{% highlight lua %}
local home = os.getenv("HOME")

local _M = {
  -- This is used later as the default terminal and editor to run.
  terminal = "xfce4-terminal",
   
  -- Default modkey.
  modkey = "Mod4",

  -- user defined wallpaper
  --wallpaper = home .. "/Pictures/your-wallpaper-here.jpg",
}

return _M
{% endhighlight %}

This user variables module will grow as your code grown.

_._

As you can see in above code, we need to return values from the module.
Thi would be easier by wrapping these values in local variable.
You can use any name that you like,
now consider name this internal variable as <code>_M</code>.

{% highlight lua %}
local _M = {
  -- some setting here
}

return _M
{% endhighlight %}

This internal variables <code>_M</code>,
would not be used in main configuration.

These approach works for simple value passing,
in fact most module here only need this approach.

#### Calling Module with Returning Value

In main configuration, we can call use <code>require</code> as usual.

{% highlight lua %}
vars = require("main.user-variables")
modkey = RC.vars.modkey
{% endhighlight %}

And later we can access inside value with

{% highlight lua %}
vars = require("main.user-variables")
modkey = vars.modkey
{% endhighlight %}

For a reason, I do not want this configuration,
to be polluted by having too many variables.
So I wrapped them in one variable named <code>RC</code>.
And the final result is as below.

{% highlight lua %}
RC = {} -- global namespace, on top before require any modules
RC.vars = require("main.user-variables")
modkey = RC.vars.modkey
{% endhighlight %}


#### Using Metatable

For complex situation, we need a better approach,
using <code>metatable</code>,
such as when a module contain some other function,
that should be called from main configuration.

We can rewrite code above using <code>metatable</code>.

{% highlight lua %}
local _M = {}

function _M.get ()
  local variable_name = {
    -- some setting here
  }

  return variable_name
end

return setmetatable(
  {}, 
  { __call = function(_, ...) return _M.get(...) end }
)
{% endhighlight %}

__.__

This basic modularized modules does need any `metatable`,
but I use it anyway. For personal reason, 
I need to be familiar with this `metatable`.

-- -- --

### What is Next ?

Consider continue reading [ [Awesome WM - Main Modules][local-whats-next] ].
There are, some interesting topic,
about refactoring<code>Awesome WM</code> using <code>Lua</code>.

What do you think ?


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/06' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3' %}

[local-whats-next]: /desktop/2019/06/17/awesome-modularized-main.html

[image-ss-01-tree]:     {{ asset_path }}/01-tree-all.png

[dotfiles-config-43]:   {{ dotfiles }}/rc.43.lua
[dotfiles-config]:      {{ dotfiles }}/rc.lua

[dotfiles-error-handling]:  {{ dotfiles }}/main/error-handling.lua
[dotfiles-user-variables]:  {{ dotfiles }}/main/user-variables.lua
