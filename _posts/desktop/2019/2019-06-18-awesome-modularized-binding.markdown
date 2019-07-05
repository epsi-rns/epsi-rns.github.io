---
layout: post
title:  "Awesome WM - Binding Modules"
categories: desktop
date      : 2019-06-18 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/site/images/topics/lua.png

excerpt:
  Awesome WM customization step by step.
  Keyboard and mouse binding modules in modularized configuration.

related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

### Preface

> Goal: Keyboard and mouse binding modules in modularized configuration.

### Artefacts

So far, binding configurations can be refactored into five modules.
Now consider have another task,
moving some configurations to files in main directory.

{% highlight bash %}
$ tree binding
binding
├── bindtotags.lua
├── clientbuttons.lua
├── clientkeys.lua
├── globalbuttons.lua
└── globalkeys.lua

0 directories, 5 files
{% endhighlight %}

![Awesome WM: Artefacts in Binding][image-ss-01-tree-binding]{: .img-responsive }

We will discuss here about:

* Mouse Buttons: `global`, and `client`.
  For simplicity reason, we start from mouse buttons,
  that have fewer configuration codes.

* keyboard shortcut: `global`, and `client`.
  Be aware of long configuration codes here.

* And later bind them to `tags`.

#### Table of Content

* Preface: Table of Content

* 1: Global Buttons

* 2: Client Buttons

* 3: Global Keys

* 4: Client Keys

* 5: Bind To Tags

* What is Next ?

-- -- --

### 1: Global Buttons

You can <kbd>right click</kbd> your mouse button to open menu,
as you can see in configuration below:

#### Module Script

*	[gitlab.com/.../dotfiles/.../binding/bindtotags.lua][dotfiles-bindtotags]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local globalbuttons = gears.table.join(
    awful.button({ }, 3, function () RC.mainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
  )

  return globalbuttons
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

__.__

#### Calling Script

In main configuration:

{% highlight lua %}
-- Custom Local Library: Keys and Mouse Binding
local binding = {
  globalbuttons = require("binding.globalbuttons")
}

-- Set root
root.buttons(binding.globalbuttons())
{% endhighlight %}

-- -- --

### 2: Client Buttons

> What is client, and what is global?

Now you can also configure,
window client response for mouse button click.

#### Module Script

*	[gitlab.com/.../dotfiles/.../binding/clientbuttons.lua][dotfiles-clientbuttons]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

local _M = {}
local modkey = RC.vars.modkey

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
  )

  return clientbuttons
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

Try <kbd>mod+right click</kbd>  window client area__.__
Notice that title bar also has its own binding,
as we will describe later.

#### Calling Script

In main configuration:

{% highlight lua %}
-- Custom Local Library: Keys and Mouse Binding
local binding = {
  globalbuttons = require("binding.globalbuttons"),
  clientbuttons = require("binding.clientbuttons")
}

-- Rules
awful.rules.rules = main.rules(
  clientkeys,
  binding.clientbuttons()
)
{% endhighlight %}

-- -- --

### 3: Global Keys

Global keys script is responsible for configuring:
popup help, tag browsing, standard program, 
layout manipulation, prompt, resize/move, and menubar.

#### Module Script

This is a long script, so I give an minimal example,
just to show how it works.

*	[gitlab.com/.../dotfiles/.../binding/globalbuttons.lua][dotfiles-globalbuttons]

{% highlight lua %}
function _M.get()
  local globalkeys = gears.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})

  )

  return globalkeys
end
{% endhighlight %}

With this <kbd>mod+s</kbd> you can popup binding cheatsheet.

![Awesome WM: Simple Binding Cheatsheet][image-ss-01-help-simple]

#### Calling Script

In main configuration:

{% highlight lua %}
-- Custom Local Library: Keys and Mouse Binding
local binding = {
  globalbuttons = require("binding.globalbuttons"),
  clientbuttons = require("binding.clientbuttons"),
  globalkeys    = require("binding.globalkeys")
}

-- Mouse and Key bindings
RC.globalkeys = binding.globalkeys()

-- Set root
root.buttons(binding.globalbuttons())
root.keys(RC.globalkeys)
{% endhighlight %}

#### Complete Script

Complete scripts has these topics:

* Tag browsing

* Standard program

* Layout manipulation

* Prompt

* Resize and Move

* Menubar

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
-- local hotkeys_popup = require("awful.hotkeys_popup").widget
local hotkeys_popup = require("awful.hotkeys_popup")
-- Menubar library
local menubar = require("menubar")

-- Resource Configuration
local modkey = RC.vars.modkey
local terminal = RC.vars.terminal

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local globalkeys = gears.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})

  )

  return globalkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

Yeah.. it is a pretty long configuration __.__
That we need to explain part by part.

#### Standard Program

This is where you want to add your custom application,
such as other kind of terminal, or anything.

{% highlight lua %}
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),
{% endhighlight %}

#### Tag Browsing

{% highlight lua %}
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () RC.mainmenu:show() end,
              {description = "show main menu", group = "awesome"}),
{% endhighlight %}

#### Layout Manipulation

{% highlight lua %}
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),
{% endhighlight %}

#### Layout Manipulation using Increment

{% highlight lua %}

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"}),
{% endhighlight %}

#### Prompt

{% highlight lua %}
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),
{% endhighlight %}

#### Resize and Move

Resize

{% highlight lua %}
    awful.key({ modkey, "Control" }, "Down",  
              function () awful.client.moveresize( 0, 0, 0, -20) end),
    awful.key({ modkey, "Control" }, "Up",    
              function () awful.client.moveresize( 0, 0, 0,  20) end),
    awful.key({ modkey, "Control" }, "Left",  
              function () awful.client.moveresize( 0, 0, -20, 0) end),
    awful.key({ modkey, "Control" }, "Right", 
              function () awful.client.moveresize( 0, 0,  20, 0) end),
{% endhighlight %}

Move

{% highlight lua %}
    awful.key({ modkey, "Shift"   }, "Down",  
              function () awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ modkey, "Shift"   }, "Up",    
              function () awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ modkey, "Shift"   }, "Left",  
              function () awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ modkey, "Shift"   }, "Right", 
              function () awful.client.moveresize( 20,   0,   0,   0) end),
{% endhighlight %}

#### Menubar

As we already seen in above example code.

{% highlight lua %}
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
{% endhighlight %}

This is where you want to add your custom menu application,
such as rofi, dmenu or stuff.

-- -- --

### 4: Client Keys

Again, we configure the window client.

Global keys script is responsible for configuring:
client layout manipulation, maximized (unmaximized),
and custom resize/move.

#### Module Script

This is a long script, so again I give an minimal example,
just to show how it works.

*	[gitlab.com/.../dotfiles/.../binding/clientkeys.lua][dotfiles-clientkeys]

{% highlight lua %}
-- Standard Awesome library
local gears = require("gears")
local awful = require("awful")

local _M = {}
local modkey = RC.vars.modkey

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local clientkeys = gears.table.join(

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Original Example Key Bindings
    awful.key({ modkey,           }, "f",
      function (c)
        c.fullscreen = not c.fullscreen
        c:raise()
      end,
      {description = "toggle fullscreen", group = "client"})
  )

  return clientkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

__.__

#### Calling Script

In main configuration:

{% highlight lua %}
-- Custom Local Library: Keys and Mouse Binding
local binding = {
  globalbuttons = require("binding.globalbuttons"),
  clientbuttons = require("binding.clientbuttons"),
  globalkeys    = require("binding.globalkeys"),
  clientkeys    = require("binding.clientkeys")
}

-- Rules
awful.rules.rules = main.rules(
  binding.clientkeys(),
  binding.clientbuttons()
)
{% endhighlight %}

#### Client Layout Manipulation

These are the keys binding shortcut,
from the original example configutration script.

{% highlight lua %}
    awful.key({ modkey,           }, "f",
      function (c)
        c.fullscreen = not c.fullscreen
        c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
      function (c)
        -- The client currently has the input focus, so it cannot be
        -- minimized, since minimized clients can't have the focus.
        c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
{% endhighlight %}

#### Maximized (Unmaximized)

{% highlight lua %}
    -- Maximized
    awful.key({ modkey,           }, "m",
      function (c)
        c.maximized = not c.maximized
        c:raise()
      end ,
      {description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
      function (c)
        c.maximized_vertical = not c.maximized_vertical
        c:raise()
      end ,
      {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
      function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c:raise()
      end ,
      {description = "(un)maximize horizontally", group = "client"})
{% endhighlight %}

#### Standard Binding

All standard binding can be seen in screenshot figure below:

![Awesome WM: Standard Binding Cheatsheet][image-ss-01-help-standard]

#### Custom Fix Size

Beyond those standard binding,
I also put an example of custom resize.
I need this fix size while making screenshot for blogging.

{% highlight lua %}
    awful.key({ modkey, "Mod1"    }, "Up", 
      function (c)   
        c.floating = not c.floating
        c.width    = 480
        c.x        = (c.screen.geometry.width-c.width) * 0.5
        c.height   = 400
        c.y        = (c.screen.geometry.height-c.height) * 0.5
      end ,
      {description = "480px * 400px", group = "client"}),
    awful.key({ modkey, "Mod1"    }, "Down", 
      function (c)   
        c.floating = not c.floating
        c.width    = 480
        c.x        = (c.screen.geometry.width-c.width) * 0.5
        c.height   = 600
        c.y        = (c.screen.geometry.height-c.height) * 0.5
      end ,
      {description = "480px * 600px", group = "client"}),
         awful.key({ modkey, "Mod1"    }, "Left", 
      function (c)   
        c.floating = not c.floating
        c.width    = 600
        c.x        = (c.screen.geometry.width-c.width) * 0.5
        c.height   = c.screen.geometry.height * 0.5
        c.y        = c.screen.geometry.height * 0.25
      end ,
      {description = "600px * 50%", group = "client"}),
    awful.key({ modkey, "Mod1"    }, "Right", 
      function (c)   
        c.floating = not c.floating
        c.width    = 320
        c.x        = (c.screen.geometry.width-c.width) * 0.5
        c.height   = 400
        c.y        = (c.screen.geometry.height-c.height) * 0.5
      end ,
      {description = "800px * 50%", group = "client"}),
{% endhighlight %}

I put these codes in the middle.
So I do not to worry about removing comma, at the end of this section.

-- -- --

### 5: Bind To Tags

This binding is actually part of global keys.
This is why we have this code

{% highlight lua %}
function _M.get(globalkeys)
  -- Bind all key numbers to tags.
  for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
      -- some awful binding here
    )
  end

  return globalkeys
end
{% endhighlight %}

__.__

This consist of these four parts:

* View tag only.

* Toggle tag display.

* Move client to tag.

* Toggle tag on focused client.

#### Module Script

*	[gitlab.com/.../dotfiles/.../binding/globalkeys.lua][dotfiles-globalkeys]

{% highlight lua %}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

local _M = {}
local modkey = RC.vars.modkey

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Key bindings

function _M.get(globalkeys)
  -- Bind all key numbers to tags.
  -- Be careful: we use keycodes to make it work on any keyboard layout.
  -- This should map on the top row of your keyboard, usually 1 to 9.
  for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
      -- View tag only.

      -- Toggle tag display.

      -- Move client to tag.
      
      -- Toggle tag on focused client.
    )
  end

  return globalkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { 
  __call = function(_, ...) return _M.get(...) end 
})
{% endhighlight %}

__.__

#### Calling Script

In main configuration:

{% highlight lua %}
-- Custom Local Library: Keys and Mouse Binding
local binding = {
  globalbuttons = require("binding.globalbuttons"),
  clientbuttons = require("binding.clientbuttons"),
  globalkeys    = require("binding.globalkeys"),
  clientkeys    = require("binding.clientkeys"),
  bindtotags    = require("binding.bindtotags")
}

-- Mouse and Key bindings
RC.globalkeys = binding.globalkeys()
RC.globalkeys = binding.bindtotags(RC.globalkeys)

-- Set root
root.buttons(binding.globalbuttons())
root.keys(RC.globalkeys)
{% endhighlight %}

#### View Tag only

{% highlight lua %}
      awful.key({ modkey }, "#" .. i + 9,
        function ()
          local screen = awful.screen.focused()
          local tag = screen.tags[i]
          if tag then
            tag:view_only()
          end
        end,
        {description = "view tag #"..i, group = "tag"}),
{% endhighlight %}

#### Toggle Tag Display

{% highlight lua %}
      awful.key({ modkey, "Control" }, "#" .. i + 9,
        function ()
          local screen = awful.screen.focused()
          local tag = screen.tags[i]
          if tag then
            awful.tag.viewtoggle(tag)
          end
        end,
        {description = "toggle tag #" .. i, group = "tag"}),
{% endhighlight %}

#### Move Client to Tag.

{% highlight lua %}
      awful.key({ modkey, "Shift" }, "#" .. i + 9,
        function ()
          if client.focus then
            local tag = client.focus.screen.tags[i]
            if tag then
              client.focus:move_to_tag(tag)
            end
          end
        end,
        {description = "move focused client to tag #"..i, group = "tag"}),
{% endhighlight %}

#### Toggle Tag on Focused Client.

{% highlight lua %}
      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
        function ()
          if client.focus then
            local tag = client.focus.screen.tags[i]
            if tag then
              client.focus:toggle_tag(tag)
            end
          end
        end,
        {description = "toggle focused client on tag #" .. i, group = "tag"})
{% endhighlight %}

-- -- --

What do you think ?


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/06' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3' %}

[image-ss-01-tree]:     {{ asset_path }}/01-tree-binding.png
[image-ss-01-layout]:   {{ asset_path }}/01-layout.png

[image-ss-01-help-simple]:  {{ asset_path }}/01-awesome-help-simple.png
[image-ss-01-help-standard]:{{ asset_path }}/01-awesome-help-standard.png

[dotfiles-config]:          {{ dotfiles }}/rc.lua
[dotfiles-bindtotags]:      {{ dotfiles }}/binding/bindtotags.lua
[dotfiles-clientbuttons]:   {{ dotfiles }}/binding/clientbuttons.lua
[dotfiles-clientkeys]:      {{ dotfiles }}/binding/clientkeys.lua
[dotfiles-globalbuttons]:   {{ dotfiles }}/binding/globalbuttons.lua
[dotfiles-globalkeys]:      {{ dotfiles }}/binding/globalkeys.lua
