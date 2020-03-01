---
layout     : post
title      :  "Awesome WM - Statusbar - Lain Library"
categories : desktop
date       : 2019-11-27 09:25:15 +0700
tags       : [awesome]
keywords   : [tiling, window manager, modularized, lua, lain]
author     : epsi
toc        : toc/2019/06/toc-awesome.html

opengraph:
  image: /assets/posts/desktop/2019/11/04-void-statusbar-arrow-1366x768.png

excerpt:
  Awesome WM statusbar step by step.
  Monitoring widget in statusbar using Lain Library.
---

-- -- --

### Preface

> Goal: Monitoring widget in statusbar using Lain Library.

Using Lain Library, we can manage monitoring widget, in statusbar.

![Awesome WM: Lain Statusbar in Gentoo (640x480)][image-ss-lain-640]{: .img-responsive }

#### Table of Content

* 1: Prerequisite

* 2: Simple Example

* 3: More Standard Example

* 4: Progressbar Widget

-- -- --

### 1: Prerequisite

#### rc.lua

Put the statusbar code at the end of the `rc.lua`,
and comment other statusbar.

*	[gitlab.com/.../dotfiles/.../awesome/4.3-statusbar/rc.lua][dotfiles-config]

{% highlight lua %}
-- Statusbar: Wibar
--local statusbar = require("statusbar.default.statusbar")
local statusbar = require("statusbar.lain.statusbar")
{% endhighlight %}

#### Prerequisite: The Lain Library

If your distribution does not support lain library,
you can clone `Lain` in AwesomeWM configuration directory.

{% highlight bash %}
$ git clone https://github.com/lcpz/lain
{% endhighlight %}

Now you can include the `lain` as any other library.

*	[gitlab.com/.../dotfiles/.../lain/helper-lain.lua][dotfiles-h-lain]

{% highlight lua %}
-- Standard awesome library
local awful     = require("awful")
local beautiful = require("beautiful")

-- Wibox handling library
local wibox = require("wibox")
local lain  = require("lain")

-- Custom Local Library
require("statusbar.lain.lain")
local gmc = require("themes.gmc")
{% endhighlight %}

#### Artefacts

We need additional `lain.lua`.

{% highlight lua %}
$ tree ~/.config/awesome/statusbar/lain
/home/epsi/.config/awesome/statusbar/lain
├── helper_default.lua
├── helper_lain.lua
├── lain-battery.lua
├── lain-diskfree.lua
├── lain.lua
├── lain-sound.lua
└── statusbar.lua
{% endhighlight %}

Here I refactor long configuration in separate files,
such as `battery`, `diskfree`, and `sound`.

![Awesome WM: Lain Statusbar Directory][image-ss-lain-dir]{: .img-responsive }

#### Arrow Decoration

This part can be skipped,
but I put it here anyway,
just in case anyone looking for it

{% highlight lua %}
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

-- -- --

### 2: Simple Example: Memory

The best manual for lain is in the [wiki][lain-wiki],
that you can read in its respective github.
For example this Memory:

{% highlight lua %}
local mymem = lain.widget.mem()
{% endhighlight %}

However the detail implementation might different from,
one dotfiles to another ricer.

#### lain.lua

Step by step, the `lain.lua`.

*	[gitlab.com/.../dotfiles/.../lain/lain.lua][dotfiles-lain]

{% highlight lua %}
-- Standard awesome library
local awful     = require("awful")
local beautiful = require("beautiful")

-- Wibox handling library
local wibox     = require("wibox")
local lain      = require("lain")

-- Custom Local Library
local gmc       = require("themes.gmc")
{% endhighlight %}

Since I have to separate files, I have these objects:

{% highlight lua %}
local W = {}
clone_widget_set = W           -- object name

local I = {}
clone_icon_set = I             -- object name
{% endhighlight %}

#### Formatting

Formatting code is cumbersome, so I made these two shortucts.

{% highlight lua %}
-- global for all splited
markup      = lain.util.markup
{% endhighlight %}

#### Configuring Memory Widget

Now my code is slightly different

{% highlight lua %}
W.mem = lain.widget.mem({
  settings = function()
    widget:set_markup(markup(gmc.color['blue900'], mem_now.used .. "M "))
  end
})
{% endhighlight %}

You can read more about `mem_now.used` in its wiki.

#### Helper

As usual we put `WB` object

*	[gitlab.com/.../dotfiles/.../lain/helper-lain.lua][dotfiles-h-lain]

{% highlight lua %}
-- Custom Local Library
require("statusbar.lain.lain")

local WB = wibox_package
{% endhighlight %}

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  local cws = clone_widget_set

  return {
    layout = wibox.layout.fixed.horizontal,

    --  time
    WB.arrow_rd,
    cws.mem,
  }
end
{% endhighlight %}

![Awesome WM: Lain Memory][image-ss-lain-mem]{: .img-responsive }

#### Icon

If you wish, you can add eye candy icon,
just like shown in the figure above.

{% highlight lua %}
I.mem = wibox.widget.imagebox(beautiful.widget_mem)
{% endhighlight %}

From `icons.lua` in theme directory.

{% highlight lua %}
theme.widget_mem          = icondir .. "memory.png"
{% endhighlight %}

And finally the helper

{% highlight lua %}
local WB = wibox_package
{% endhighlight %}

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  local cws = clone_widget_set
  local cis = clone_icon_set

  return {
    layout = wibox.layout.fixed.horizontal,

    --  time
    WB.arrow_rd,
    cis.mem,     cws.mem,
  }
end
{% endhighlight %}

-- -- --

### 3: More Standard Example

With those arrangement above we can setup more widget.
Step by step, the `lain.lua`.

*	[gitlab.com/.../dotfiles/.../lain/lain.lua][dotfiles-lain]

#### CPU

{% highlight lua %}
I.cpu = wibox.widget.imagebox()
I.cpu:set_image(beautiful.widget_cpu)

W.cpu = lain.widget.cpu({
  settings = function()
    widget:set_markup(markup(gmc.color['green900'], cpu_now.usage .. "% "))
  end
})
{% endhighlight %}

#### Text Clock

{% highlight lua %}
I.clock = wibox.widget.imagebox(beautiful.widget_clock)

W.textclock = awful.widget.textclock(
    markup(gmc.color['blue900'], "%A %d %B ")
        .. markup(gmc.color['black'], ">")
        .. markup(gmc.color['green900'], " %H:%M "))
{% endhighlight %}

#### Weather

{% highlight lua %}
I.weather = wibox.widget.imagebox(beautiful.widget_weather)

W.weather = lain.widget.weather({
    city_id = 1642911, -- http://openweathermap.org/city/1642911
    settings = function()
        descr = weather_now["weather"][1]["description"]:lower()
        units = math.floor(weather_now["main"]["temp"])
        local fg_color = "#000000" -- "#eca4c4"
        widget:set_markup(markup(fg_color, descr .. " @ " .. units .. "°C "))
    end
})
{% endhighlight %}

#### Network

{% highlight lua %}
I.netdown = wibox.widget.imagebox(beautiful.widget_netdown)
W.netdowninfo = wibox.widget.textbox()

W.netupinfo = lain.widget.net({
    settings = function()
        local fg_color_up   = "#000000" -- "#e54c62"
        local fg_color_down = "#000000" -- "#87af5f"
        widget:set_markup(markup(fg_color_up, net_now.sent .. " "))
        W.netdowninfo:set_markup(markup(fg_color_down, net_now.received .. " "))
    end
})
{% endhighlight %}


#### Battery

{% highlight lua %}
-- Battery from copycat-multicolor

I.bat = wibox.widget.imagebox(beautiful.widget_batt)

W.bat = lain.widget.bat({
    settings = function()
        if bat_now.perc == "N/A" then
            perc = "AC "
        else
            perc = bat_now.perc .. "% "
        end
        widget:set_markup(markup(gmc.color['blue900'], perc))
    end
})
{% endhighlight %}

#### File system

{% highlight lua %}
I.fs = wibox.widget.imagebox(beautiful.widget_fs)

-- Can't create more than one fs widget
W.fs = lain.widget.fs({
  settings  = function()
    widget:set_markup(markup(gmc.color['teal900'], fs_now["/"].percentage .. "% "))
  end
})
{% endhighlight %}

#### ALSA

{% highlight lua %}
-- ALSA volume from copycat-multicolor
I.volume = wibox.widget.imagebox(beautiful.widget_vol)

W.volume = lain.widget.alsa({
    settings = function()
        if volume_now.status == "off" then
            volume_now.level = volume_now.level .. "M"
        end

        widget:set_markup(markup(gmc.color['blue900'], volume_now.level .. "% "))
    end
})
{% endhighlight %}

#### Music Player Daemon

{% highlight lua %}
-- MPD from copycat-multicolor

I.mpd = wibox.widget.imagebox(beautiful.widget_note)
W.mpd = lain.widget.mpd({
    settings = function()
        mpd_notification_preset = {
            text = string.format("%s [%s] - %s\n%s", mpd_now.artist,
                   mpd_now.album, mpd_now.date, mpd_now.title)
        }

        if mpd_now.state == "play" then
            artist = mpd_now.artist .. " > "
            title  = mpd_now.title .. " "
            I.mpd:set_image(beautiful.widget_note_on)
        elseif mpd_now.state == "pause" then
            artist = "mpd "
            title  = "paused "
        else
            artist = ""
            title  = ""
            I.mpd:set_image(nil)
        end
        widget:set_markup(markup(gmc.color['blue900'], artist)
            .. markup(gmc.color['green900'], title))
    end
})
{% endhighlight %}

#### Example One

Consider show the wdiget above in this example below:

*	[gitlab.com/.../dotfiles/.../lain/helper-lain.lua][dotfiles-h-lain]

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  local cws = clone_widget_set
  local cis = clone_icon_set

  return {
    layout = wibox.layout.fixed.horizontal,

    --  net
    WB.arrow_rd,
    cis.netdown, cws.netdowninfo,
    cis.netup,   cws.netupinfo,

    --  mem, cpu, files system, temp, batt
    WB.arrow_dr,  WB.arrow_rd,
    cis.mem,     cws.mem,
    cis.cpu,     cws.cpu,
    cis.fs,      cws.fs,
    cis.bat,     cws.bat,
    
    --  wheather
    WB.arrow_dr,  WB.arrow_rd,
    cis.weather, cws.weather,

    -- at last   , you can ignore this line below
    WB.spacer,
  }
end
{% endhighlight %}


![Awesome WM: Lain Example 01][image-ss-lain-01]{: .img-responsive }

#### Example Two

Or this example below:

*	[gitlab.com/.../dotfiles/.../lain/helper-lain.lua][dotfiles-h-lain]

{% highlight lua %}
function WB.add_widgets_monitor_left (line, s)
  local cws = clone_widget_set
  local cis = clone_icon_set

  return {
    layout = wibox.layout.fixed.horizontal,

    --  time
    WB.arrow_rd,
    cis.clock,   cws.textclock,

    --  mpd
    WB.arrow_dr,  WB.arrow_rd,
    cis.volume,  cws.volume,
    cis.mpd,     cws.mpd,

    -- at last   , you can ignore this line below
    WB.spacer,
  }
end
{% endhighlight %}

![Awesome WM: Lain Example 02][image-ss-lain-02]{: .img-responsive }

-- -- --

### 4: Progressbar Widget

This is a little bit harder,
since we should dive into container and margin,
just to show widget progressbar.

#### Refactoring

For some reason I prefer to separate code so now we have this lines:

{% highlight lua %}
-- progress bar related widgets -- after global markup
local config_path = awful.util.getdir("config") .. "statusbar/lain/"
dofile(config_path .. "lain-diskfree.lua")
dofile(config_path .. "lain-battery.lua")
dofile(config_path .. "lain-sound.lua")
{% endhighlight %}

#### Battery Bar

The battery bar itself is `battery_bar` widget. 

*	[gitlab.com/.../dotfiles/.../lain/lain-battery.lua][dotfiles-lain-battery]

{% highlight lua %}
I.battery = wibox.widget.imagebox(beautiful.monitor_bat)

W.battery_bar = wibox.widget {
    forced_height    = dpi(1),
    forced_width     = dpi(59),
    color            = theme.fg_normal,
    background_color = theme.bg_normal,
    margins          = 1,
    paddings         = 1,
    ticks            = true,
    ticks_size       = dpi(6),
    widget           = wibox.widget.progressbar,
}

W.battery_margin = wibox.layout.margin(W.battery_bar, 2, 7)
W.battery_margin:set_top(6)
W.battery_margin:set_bottom(9)

W.batbg = wibox.container.background(
  W.battery_bar, gmc.color['grey500'], gears.shape.rectangle)
W.battery_bar_widget = wibox.container.margin(
  W.batbg, dpi(2), dpi(7), dpi(6), dpi(6))
{% endhighlight %}

#### Battery Updating

We utilize `lain.widget.bat` as a trigger to update the battery bar.

{% highlight lua %}
-- Update bar, also in widgets popups
local battery_widget_settings = function()
    if bat_now.status == "N/A" then return end

    perc = tonumber(bat_now.perc)
    if perc == nil then return end

    if bat_now.status == "Charging" then
        I.battery:set_image(beautiful.monitor_ac)
        if perc >= 98 then
            W.battery_bar:set_color(gmc.color['green300'])
        elseif perc > 50 then
            W.battery_bar:set_color(gmc.color['blue300'])
        elseif perc > 15 then
            W.battery_bar:set_color(beautiful.fg_normal)
        else
            W.battery_bar:set_color(gmc.color['red300'])
        end
    else
        if perc >= 98 then
            W.battery_bar:set_color(gmc.color['green300'])
        elseif perc > 50 then
            W.battery_bar:set_color(gmc.color['blue300'])
            I.battery:set_image(beautiful.monitor_bat)
        elseif perc > 15 then
            W.battery_bar:set_color(beautiful.fg_normal)
            I.battery:set_image(beautiful.monitor_bat_low)
        else
            W.battery_bar:set_color(gmc.color['red300'])
            I.battery:set_image(beautiful.monitor_bat_no)
        end
    end
    W.battery_bar:set_value(perc / 100)
end

W.battery_widget_update = lain.widget.bat({
    settings = battery_widget_settings
})

{% endhighlight %}

#### Disk Free Bar

The diskfree bar itself is `disk_bar` widget. 

*	[gitlab.com/.../dotfiles/.../lain/lain-diskfree.lua][dotfiles-lain-diskfree]

{% highlight lua %}
-- /home fs
I.disk = wibox.widget.imagebox(beautiful.monitor_disk)

W.disk_bar = wibox.widget {
    forced_height    = dpi(1),
    forced_width     = dpi(59),
    color            = theme.fg_normal,
    background_color = theme.bg_normal,
    margins          = 1,
    paddings         = 1,
    ticks            = true,
    ticks_size       = dpi(6),
    widget           = wibox.widget.progressbar,
}

W.disk_margin = wibox.layout.margin(W.disk_bar, 2, 7)
W.disk_margin:set_top(6)
W.disk_margin:set_bottom(9)

W.disk_bar_widget = wibox.widget.background(W.disk_margin)
W.disk_bar_widget:set_bgimage(beautiful.bar_bg_copland)

W.disk_bg = wibox.container.background(
  W.disk_bar, gmc.color['grey500'], gears.shape.rectangle)
W.disk_bar_widget = wibox.container.margin(
  W.disk_bg, dpi(2), dpi(7), dpi(6), dpi(6))
{% endhighlight %}

#### Disk Free Update

We utilize `lain.widget.fs` as a trigger to update the diskfree bar.

{% highlight lua %}
-- Update bar, also in widgets popups

local disk_widget_settings = function()
    if fs_now["/"].used < 90 then
        W.disk_bar:set_color(gmc.color['red300'])
    else
        W.disk_bar:set_color(gmc.color['blue300'])
    end
    W.disk_bar:set_value(fs_now["/"].percentage)
end

W.disk_widget = lain.widget.fs {
    notification_preset = { fg = theme.fg_normal, bg = theme.bg_normal, font = "Terminus 10.5" },
    settings  = disk_widget_settings
}
{% endhighlight %}

#### ALSA Bar

Lain has this `alsabar` widget

*	[gitlab.com/.../dotfiles/.../lain/lain-sound.lua][dotfiles-lain-sound]

{% highlight lua %}
local volume_wibox_settings = function()
    if volume_now.status == "off" then
        I.volume_dynamic:set_image(beautiful.monitor_vol_mute)
    elseif volume_now.level == 0 then
        I.volume_dynamic:set_image(beautiful.monitor_vol_no)
    elseif volume_now.level <= 50 then
        I.volume_dynamic:set_image(beautiful.monitor_vol_low)
    else
        I.volume_dynamic:set_image(beautiful.monitor_vol)
    end
end

local volume_wibox_colors = {
    background = beautiful.bg_normal,
    mute = gmc.color['red300'],
    unmute = gmc.color['blue300']
}

W.volume_wibox = lain.widget.alsabar({
  width = 55, ticks = true, ticks_size = 6, step = "2%",
  settings = volume_wibox_settings,
  colors = volume_wibox_colors
})
{% endhighlight %}

Now we can configure further

{% highlight lua %}
-- ALSA volume bar from copycat-copland
I.volume_dynamic = wibox.widget.imagebox(beautiful.monitor_vol)

W.volume_wibox.tooltip.wibox.fg = beautiful.fg_focus

W.volumebg = wibox.container.background(
  W.volume_wibox.bar, gmc.color['grey500'], gears.shape.rectangle)
W.volumewidget = wibox.container.margin(
  W.volumebg, dpi(2), dpi(7), dpi(6), dpi(6))
{% endhighlight %}

#### ALSA Update

We do not need. It is handled by Lain.

#### ALSA Volume Button

{% highlight lua %}
-- global terminal is required in alsabar, unfortunately
terminal = RC.vars.terminal

W.volume_wibox_buttons = my_table.join (
  awful.button({}, 1, function()
    awful.spawn(string.format("%s -e alsamixer", terminal))
  end),
  awful.button({}, 2, function()
    os.execute(string.format("%s set %s 100%%", W.volume_wibox.cmd, W.volume_wibox.channel))
    W.volume_wibox.update()
  end),
  awful.button({}, 3, function()
    os.execute(string.format("%s set %s toggle", W.volume_wibox.cmd, W.volume_wibox.togglechannel or W.volume_wibox.channel))
    W.volume_wibox.update()
    end),
  awful.button({}, 4, function()
    os.execute(string.format("%s set %s 1%%+", W.volume_wibox.cmd, W.volume_wibox.channel))
    W.volume_wibox.update()
  end),
    awful.button({}, 5, function()
    os.execute(string.format("%s set %s 1%%-", W.volume_wibox.cmd, W.volume_wibox.channel))
    W.volume_wibox.update()
  end)
)

W.volume_wibox.bar:buttons(W.volume_wibox_buttons)
{% endhighlight %}

#### Helper

Now here is the `alsa volume`, `disk free` and `battery`,
along with other widget

{% highlight lua %}
function WB.add_widgets_monitor_right (line, s)
  local cws = clone_widget_set
  local cis = clone_icon_set

  return {
    layout = wibox.layout.fixed.horizontal,

    --  volume
    WB.arrow_dl,         WB.arrow_ld,
    cis.volume_dynamic,  cws.volumewidget,
    
    --  disk
    WB.arrow_dl,         WB.arrow_ld,
    cis.disk,           cws.disk_bar_widget,

    --  battery
    WB.arrow_dl,         WB.arrow_ld,
    cis.battery,        cws.battery_bar_widget,

    -- at last
    WB.arrow_dl
  }
end
{% endhighlight %}

![Awesome WM: Lain Progressbar Widget][image-ss-progressbar]{: .img-responsive }

-- -- --

### What is Next ?

Now that we are done with `lain` statusbar,
we can go down with ricing using arrow, `lain` widget, and pallete colors.

![Awesome WM: Lain Statusbar in Void (1366x768)][image-ss-void-1366]{: .img-responsive }

Consider continue reading [ [Awesome WM - Statusbar - Arrow][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/11' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-statusbar' %}

[local-whats-next]: /desktop/2019/11/28/awesome-statusbar-arrow.html

[lain-wiki]:            https://github.com/lcpz/lain/wiki

[dotfiles-config]:      {{ dotfiles }}/rc.lua
[dotfiles-theme]:       {{ dotfiles }}/themes/clone/theme.lua
[dotfiles-statusbar]:   {{ dotfiles }}/statusbar/lain/statusbar.lua
[dotfiles-h-default]:   {{ dotfiles }}/statusbar/lain/helper_default.lua
[dotfiles-h-lain]:      {{ dotfiles }}/statusbar/lain/helper_lain.lua
[dotfiles-lain]:        {{ dotfiles }}/statusbar/lain/lain.lua
[dotfiles-lain-battery]:    {{ dotfiles }}/statusbar/lain/lain-battery.lua
[dotfiles-lain-diskfree]:   {{ dotfiles }}/statusbar/lain/lain-diskfree.lua
[dotfiles-lain-sound]:      {{ dotfiles }}/statusbar/lain/lain-sound.lua


[image-ss-lain-640]:    {{ asset_path }}/04-void-lain-640.png
[image-ss-lain-800]:    {{ asset_path }}/04-void-lain-800.png
[image-ss-void-1366]:   {{ asset_path }}/04-void-statusbar-arrow-1366x768.png
[image-ss-lain-dir]:    {{ asset_path }}/04-directory-lain.png
[image-ss-lain-mem]:    {{ asset_path }}/04-lain-memory.png

[image-ss-lain-01]:     {{ asset_path }}/04-lain-more-01.png
[image-ss-lain-02]:     {{ asset_path }}/04-lain-more-02.png
[image-ss-progressbar]: {{ asset_path }}/04-lain-progressbar.png
