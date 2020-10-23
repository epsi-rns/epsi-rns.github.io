---
layout: post
title:  "Conky with Lua Scripting"
categories: desktop
date      : 2019-06-11 20:19:15 +0700
tags      : [conky]
keywords  : [modularized]
author: epsi

opengraph:
  image: /assets/site/images/topics/lua.png

excerpt:
  This article explain how to convert old conky into lua format.
  Featuring Hijri date format.

---

<a name="preface"></a>

### Preface

> Goal: Modularized Conky Configuration Using Lua Script.

A member in dotfiles group once ask me,
how to change gregorian date in his conky, into hijri date.
The answer is simply, using external script.

The issue is, I like the new conky Lua format.
So here it is a step by step, conky with lua scripting.

#### Table of Content

* [Preface](#preface): Table of Content

* 1: [The Original Code](#original-code)

* 2: [Convert to Lua Code](#convert-to-lua)

* 3: [Modularized](#modularized)

* 4: [Finishing](#finishing)

* [Conclusion](#conclusion)

-- -- --

<a name="original-code"></a>

### 1: The Original Code

He gave a file in telegram group showing something like this one:

{% highlight conf %}
#Author url = http://dobbie03.deviantart.com/art/My-First-Conky-Config-327206399
#Modified by Akza

alignment top_middle
background no
border_margin 5
border_width 5
default_color ffffff  #413536 # grey 5f5f5f 3F3F3F 183149 3B3B3B 26211F
double_buffer yes
draw_borders no
draw_outline no
draw_shades no
gap_x 0
gap_y 150
maximum_width 2000
minimum_size 0 0
no_buffers yes
override_utf8_locale yes
own_window yes
own_window_title conky
own_window_hints undecorated,below,sticky,skip_taskbar,skip_pager
own_window_transparent yes
own_window_type conky
text_buffer_size 8000
total_run_times 0
update_interval 1
uppercase no
use_xft yes
xftalpha 1
xftfont Freesans:pixelsize=9

#				fonts
#	Blue Highway
#	Zegoe Light - U
#	Zekton
#	Calibri
#	Engebrechtre
#	Opeln2001
#	Pricedown

own_window_argb_value 0
own_window_argb_visual yes
own_window_colour 000000
TEXT
${font BankGothic Md BT:pixelsize=15}${alignc}${time [ %A, %I:%M:%S ]}${font}
${font BankGothic Md BT:pixelsize=15}${alignc}${time %d %B, %Y}${font}
${font BankGothic Md BT:pixelsize=45}${alignc}Be A Good Moslem or Die As Syuhada${font}
{% endhighlight %}

This is the old plain conky format.

#### Command Line

You can run this by issue this command:

{% highlight bash %}
$ conky -c conky/my.conkyrc 
{% endhighlight %}

Notice that he original code is longer than that above.

#### Source

From the link above, I can track that the original code is from Dobbie.

* <http://dobbie03.deviantart.com/art/My-First-Conky-Config-327206399>

-- -- --

<a name="convert-to-lua"></a>

### 2: Convert to Lua Code

My fist attempt is change the config, for minimal output.
Just to change if this works.
I also change the font to suit my environment.

*	[gitlab.com/.../dotfiles/.../dobbie/conkyrc-01.lua][dotfiles-rc-01]

{% highlight lua %}
-- Author url = http://dobbie03.deviantart.com/art/My-First-Conky-Config-327206399
-- Modified by Akza
-- 11 June 2019: Modified by epsi

-- vim: ts=4 sw=4 noet ai cindent syntax=lua

--[[
Conky, a system monitor, based on torsmo
]]

conky.config = {
-- common
  alignment = 'middle_middle',
  background = false,
  double_buffer = true,
  total_run_times = 0,
  update_interval = 1,
  default_color = '#ffffff',

-- window
  own_window = true,
  own_window_title = 'conky',
  own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',
  own_window_transparent = true,
  own_window_type = 'normal',
  own_window_argb_value = 0,
  own_window_argb_visual = true,
  own_window_colour = '#000000',
  
-- font  
  use_xft = true
}

conky.text = [[
${font CabinSketch-Bold:pixelsize=15}${alignc}${time [ %A, %I:%M:%S ]}${font}
${font CabinSketch-Bold:pixelsize=15}${alignc}${time %d %B, %Y}${font}
${font CabinSketch-Bold:pixelsize=45}${alignc}Let's Get Married!${font}
]]
{% endhighlight %}

This is the new Lua conky format.

#### Command Line

You can run this by issue this command:

{% highlight bash %}
$ conky -c conky/conkyrc-01.lua
{% endhighlight %}

#### Preview

![Conky: Lua Format][image-ss-01]{: .img-responsive }

If you see any shadow, this is because of my <code>compton</code> setup.

#### How does it Works?

All you nee to know is these two variables:

{% highlight lua %}
conky.config
{% endhighlight %}

{% highlight lua %}
conky.text
{% endhighlight %}

-- -- --

<a name="modularized"></a>

### 3: Modularized

#### Configuration

I like short code, and put long configuration code somewhere else.
Assuming this code will not be changed frequently.

*	[gitlab.com/.../dotfiles/.../dobbie/config.lua][dotfiles-config]

{% highlight lua %}
configuration = {
-- common
  alignment = 'middle_middle',
  background = false,
  double_buffer = true,
  total_run_times = 0,
  update_interval = 1,

-- border
  border_inner_margin = 5,
  border_outer_margin = 5,
  border_width = 5,

-- color
  -- #413536 # grey 5f5f5f 3F3F3F 183149 3B3B3B 26211F
  default_color = '#ffffff',

-- draw options
  draw_borders = false,
  draw_outline = false,
  draw_shades = false,

-- positioning
  gap_x = 0,
  gap_y = 0,
  maximum_width = 2000,
  minimum_height = 0,
  minimum_width = 0,

  no_buffers = true,
  override_utf8_locale = true,

-- window
  own_window = true,
  own_window_title = 'conky',
  own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',
  own_window_transparent = true,

-- 'normal', 'dock', 'panel', 'desktop', 'override'
  own_window_type = 'normal',

  own_window_argb_value = 0,
  own_window_argb_visual = true,
  own_window_colour = '#000000',

-- text
  text_buffer_size = 8000,
  uppercase = false,
  
-- font  
  use_xft = true,
  xftalpha = 1,
  xftfont = 'Freesans:pixelsize=9'

--				fonts
--	Blue Highway
--	Zegoe Light - U
--	Zekton
--	Calibri
--	Engebrechtre
--	Opeln2001
--	Pricedown
}
{% endhighlight %}

And in main file, we can call as below:

*	[gitlab.com/.../dotfiles/.../dobbie/conkyrc-02.lua][dotfiles-rc-02]

{% highlight lua %}
home = os.getenv("HOME")
dofile(home .. '/conky/config.lua')
conky.config = configuration
{% endhighlight %}

#### Text

Why don't I also put the long text code separately?
Because conky do not detect any changes outside that main file.
If I separate the text file, I have to change main code and save,
just to see the effect in conky. 
Of course you can freely refactor whenever your code get more complex.

Consider keep the text in main script.
But change each lines of the text into separate Lua variables.

{% highlight lua %}
home = os.getenv("HOME")
dofile(home .. '/conky/config.lua')
conky.config = configuration

datetime = ''
.. '${font CabinSketch-Bold:pixelsize=15}'
.. '${alignc}'
.. '${time [ %A, %I:%M:%S ]}'
.. '\n'
.. '${font CabinSketch-Bold:pixelsize=20}'
.. '${alignc}'
.. '${time %d %B, %Y}'
.. '\n'

message = ''
.. "${font CabinSketch-Bold:pixelsize=45}"
.. "${alignc}"
.. "Let's Get Married!"
.. "\n"

conky.text = ''
.. datetime
.. message 
{% endhighlight %}

#### Command Line

You can run this by issue this command:

{% highlight bash %}
$ conky -c conky/conkyrc-02.lua
{% endhighlight %}

#### Preview

The same as previous image.

-- -- --

<a name="finishing"></a>

### 4: Finishing

#### Path

How about relative path?
We can do this by using code below:

{% highlight lua %}
local dirname  = debug.getinfo(1).source:match("@?(.*/)")
dofile(dirname .. '/config.lua')
conky.config = configuration
{% endhighlight %}

#### Hijri Date

As I said before, use external script.

{% highlight lua %}
datetime = ''
.. '${font CabinSketch-Bold:pixelsize=15}'
.. '${alignc}'
.. '${time [ %A, %I:%M:%S ]}'
.. '\n'
.. '${font CabinSketch-Bold:pixelsize=20}'
.. '${alignc}'
.. '${exec ~/conky/conkyhijri.sh}'
.. '\n'
{% endhighlight %}

Notice the <code>exec ~/conky/conkyhijri.sh</code> code.

#### Hijri Script

Since this is not my script, I won't put it in my blog.
But you can have a look at my dotfiles.

*	[gitlab.com/.../dotfiles/.../dobbie/conkyhijri.sh][dotfiles-hijri]

#### Monitoring Code

I also port the rest code, to monitor the status of my system.
My intention is to have a more clear indentation for conditional script.

{% highlight lua %}
cpu_usage = 'CPU Usage: ${cpu}% - RAM Usage: ${mem}'

root = 'Root: ${fs_free /} / ${fs_size /}'

battery = 'Battery: ${battery_percent BAT0}% -'
.. ' Remaining Time: ${battery_time BAT0} '

user = 'User: ${exec users} - System Uptime: ${uptime_short}'

wlan = 'wlp3s0'

net_up = 'Net Up: '
.. '${if_existing /proc/net/route ' .. wlan .. '}'
..   '${upspeed ' .. wlan .. '}'
.. '${else}'
..   '${if_existing /proc/net/route eth0}'
..     '${upspeed ' .. wlan .. '}'
..   '${endif}'
.. '${endif}'

net_down = 'Net Down: '
.. '${if_existing /proc/net/route ' .. wlan .. '}'
..   '${downspeed ' .. wlan .. '}'
.. '${else}'
..   '${if_existing /proc/net/route eth0}'
..     '${downspeed ' .. wlan .. '}'
..   '${endif}'
.. '${endif}'

monitor = ''
.. '${font CabinSketch-Bold:pixelsize=12}'
.. '${alignc}[ ' .. cpu_usage .. ' ]\n'
.. '${alignc}[ ' .. root .. ' ]\n'
-- '${alignc}[ ' .. battery .. ' ]\n'
.. '${alignc}[ ' .. user .. ' ]\n'
.. '${alignc}[ ' .. net_up .. ' ' ..net_down .. ' ]\n'
{% endhighlight %}

Notice, how I manage the <code>wlan</code> name.
You may change it to suit your system.

#### The Text

Now you need to add this <code>monitor</code> variable to the text.

{% highlight lua %}
conky.text = ''
.. datetime
.. message 
.. ''
.. monitor
{% endhighlight %}

#### Preview

![Conky: Modularized Lua][image-ss-03]{: .img-responsive }

#### Command Line

You can run this by issue this command:

{% highlight bash %}
$ conky -c conky/conkyrc-03.lua
{% endhighlight %}

#### Complete Script

And finally, our summary:

{% highlight lua %}
-- Author url = http://dobbie03.deviantart.com/art/My-First-Conky-Config-327206399
-- Modified by Akza
-- 11 June 2019: Modified by epsi

-- vim: ts=4 sw=4 noet ai cindent syntax=lua

--[[
Conky, a system monitor, based on torsmo
]]

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
dofile(dirname .. '/config.lua')
conky.config = configuration

datetime = ''
.. '${font CabinSketch-Bold:pixelsize=15}'
.. '${alignc}'
.. '${time [ %A, %I:%M:%S ]}'
.. '\n'
.. '${font CabinSketch-Bold:pixelsize=20}'
.. '${alignc}'
.. '${exec ~/conky/conkyhijri.sh}'
.. '\n'

message = ''
.. "${font CabinSketch-Bold:pixelsize=45}"
.. "${alignc}"
.. "Let's Get Married!"
.. "\n"

cpu_usage = 'CPU Usage: ${cpu}% - RAM Usage: ${mem}'

root = 'Root: ${fs_free /} / ${fs_size /}'

battery = 'Battery: ${battery_percent BAT0}% -'
.. ' Remaining Time: ${battery_time BAT0} '

user = 'User: ${exec users} - System Uptime: ${uptime_short}'

wlan = 'wlp3s0'

net_up = 'Net Up: '
.. '${if_existing /proc/net/route ' .. wlan .. '}'
..   '${upspeed ' .. wlan .. '}'
.. '${else}'
..   '${if_existing /proc/net/route eth0}'
..     '${upspeed ' .. wlan .. '}'
..   '${endif}'
.. '${endif}'

net_down = 'Net Down: '
.. '${if_existing /proc/net/route ' .. wlan .. '}'
..   '${downspeed ' .. wlan .. '}'
.. '${else}'
..   '${if_existing /proc/net/route eth0}'
..     '${downspeed ' .. wlan .. '}'
..   '${endif}'
.. '${endif}'

monitor = ''
.. '${font CabinSketch-Bold:pixelsize=12}'
.. '${alignc}[ ' .. cpu_usage .. ' ]\n'
.. '${alignc}[ ' .. root .. ' ]\n'
-- '${alignc}[ ' .. battery .. ' ]\n'
.. '${alignc}[ ' .. user .. ' ]\n'
.. '${alignc}[ ' .. net_up .. ' ' ..net_down .. ' ]\n'

conky.text = ''
.. datetime
.. message 
.. ''
.. monitor
{% endhighlight %}

You can see that the whole code is looks completely different,
compared with the original code.

-- -- --

<a name="conclusion"></a>

### Conclusion

After all this is just a config.
You may modify this config to suit your needs.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets-desktop/2019/06' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/conky' %}

[image-ss-01]:      {{ asset_path }}/conkyrc-01.png
[image-ss-03]:      {{ asset_path }}/conkyrc-03.png
[dotfiles-config]:  {{ dotfiles }}/dobbie/config.lua
[dotfiles-rc-01]:   {{ dotfiles }}/dobbie/conkyrc-01.lua
[dotfiles-rc-02]:   {{ dotfiles }}/dobbie/conkyrc-02.lua
[dotfiles-rc-03]:   {{ dotfiles }}/dobbie/conkyrc-03.lua
[dotfiles-hijri]:   {{ dotfiles }}/dobbie/conkyhijri.sh

