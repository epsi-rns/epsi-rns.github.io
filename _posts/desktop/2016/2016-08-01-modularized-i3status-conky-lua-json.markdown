---
layout: post-sidemenu-wm
title:  "Modularized Conky Configuration for i3status"
categories: desktop
date      : 2016-08-01 20:19:15 +0700
tags      : [i3wm]
keywords  : [modularized, conky, i3status, json]
author: epsi

opengraph:
  image: /assets/site/images/topics/lua.png

excerpt:
  This article explain conky lua configuration
  for i3status using JSON.

related_link_ids:
  - 16080325  # Install i3 Arch
  - 16080224  # Install i3 Debian
  - 14120646  # i3 Window manager

---

<a name="preface"></a>

### Preface

I'm so excited with the release of Conky V1.10.
The latest conky configuration is actualy a Lua file.
It is very flexible, compared with the old plain text Conky Configuration.
We can separate the long old conky TEXT, into separate Lua function.

One application benefit from this Conky Lua Config is i3status.
Before we get too deep, let's see the old way of configuring i3status with Conky.

#### Table of Content

* [Preface](#preface): Table of Content

* [Screenshot](#screenshot)

* 1: [The i3 config](#i3-config)

* 2: [Using i3bar](#i3bar)

* 3: [Plain Conky](#plain-conky)

* 4: [Conky Config as Lua](#conky-lua)

* 5: [Conky as JSON Feed](#conky-json)

* 6: [Modular](#modular)

* 7: [Miscellanous](#misc)

* [Conclusion](#conclusion)

-- -- --

<a name="screenshot"></a>

### Screenshot

{% capture ss_content %}
<strong>OS</strong>: Arch<br/>
<strong>WM</strong>: i3-gaps<br/>
<br/>
  + <strong>Statusbar</strong>: i3bar<br/>
<br/>
  + Rofi<br/>
<br/>
  + Terminal: Termite<br/>
    - ViM: NERDTree<br/>
    - perl-term-extendedcolor<br/>
    - ls++<br/>
<br/>
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

[![i3-gaps: Conky Lua in dark i3status][image-ss-i3gaps-dark]{: .img-responsive }][photo-ss-i3gaps-dark]

-- -- --

<a name="i3-config"></a>

### 1: The i3 config

I assume that you have already familiar with i3 config.

Reading: 

* <http://i3wm.org/docs/userguide.html>

The only thing you need to concern is the <code>bar</code> section.
We will change the <code>status_command</code> few times.

{% highlight conf %}
bar {

# default i3 status config
    status_command i3status
    
    position       bottom  
    
    colors {
        background #000000
        statusline #c9c925
        separator  #ffffff
              
        # class            border    backgrd   text
        focused_workspace  #161616   #c9c925   #000000
        active_workspace   #161616   #5c5dad   #a5a5a5
        inactive_workspace #161616   #222222   #5c5dad
        urgent_workspace   #161616   #ad3737   #ffffff
    }
}
{% endhighlight %}

![i3status: workspace][image-i3-workspace]{: .img-responsive }

You can check my dotfiles here

* [i3 config][source-i3]

I put two configs, one the original i3-wm, and the other for i3-gaps.
Just copy one of them to <code class="code-file">~/.config/i3/config</code>

{% highlight bash %}
$ cat ~/.config/i3/config
{% endhighlight %}

There are others i3 variant, e.g sway for wayland,
and i3bgbar with powerline looks (deprecated).
I haven't explore the configuration yet.

In order to see change,
each time changing <code>status_command</code>,
you must restart i3 with <kbd>Mod+Shift+r</kbd>
or utilizing command line.

{% highlight conf %}
$ i3-msg restart
{% endhighlight %}

-- -- --

<a name="i3bar"></a>

### 2: Using i3bar

AFAIK there are three kind of configuration
that can be used as a feed to i3bar.

#### i3status

Let's copy the default i3status configuration.

{% highlight bash %}
$ mkdir ~/.config/i3/i3status
$ cp /etc/i3status.conf ~/.config/i3/i3status/default.conf
{% endhighlight %}

And make your own customization.

{% highlight conf %}
bar {
    status_command i3status --config ~/.config/i3/i3status/custom.conf
    }
{% endhighlight %}

[![i3status][image-i3-i3status]{: .img-responsive }][photo-i3-i3status]

#### i3blocks

Let's copy the default i3blocks configuration.

{% highlight bash %}
$ mkdir ~/.config/i3/i3blocks
$ cp /etc/i3blocks.conf ~/.config/i3/i3blocks/default.conf
{% endhighlight %}

And make your own customization.

{% highlight conf %}
bar {
   status_command i3blocks -c ~/.config/i3/i3blocks/custom.conf
    }
{% endhighlight %}

[![i3blocks][image-i3-i3blocks]{: .img-responsive }][photo-i3-i3blocks]

#### Conky

Example

{% highlight conf %}
bar {
     status_command conky -c ~/.config/i3/conkyrc
    }
{% endhighlight %}

We can have many i3bar in i3wm.
Each configured in different bar section.

-- -- --

<a name="plain-conky"></a>

### 3: Plain Conky

This is how we do conky in a very simple way,
before Conky v.1.10, and without JSON.

{% highlight conf %}
bar {
     status_command conky -c ~/.config/i3/conky/01.old/conkyrc
    }
{% endhighlight %}

And this is the <code class="code-file">conkyrc</code> file

{% highlight conf %}
out_to_x no
out_to_console yes
update_interval 1

TEXT
${if_mpd_playing}♫  ${mpd_artist} -  ${mpd_title} | ${endif}\
${time %H:%M:%S}    
{% endhighlight %}

![Conky: old][image-i3-conky-1-old]{: .img-responsive }

-- -- --

<a name="conky-lua"></a>

### Conky Config as Lua

Just like what I said before,
the latest Conky v1.10 configuration is actually a Lua file.

{% highlight conf %}
bar {
     status_command conky -c ~/.config/i3/conky/02.lua/conkyrc.lua
    }
{% endhighlight %}

And you see the difference in this <code class="code-file">conkyrc.lua</code> file.

{% highlight lua %}
conky.config = {
    background = false,
    out_to_x = false,
    out_to_console = true,
    update_interval = 1,
    total_run_times = 0,
    use_spacer = "none"
}

conky.text = [[ 
${if_mpd_playing}♫  ${mpd_artist} -  ${mpd_title} | ${endif}\
${time %H:%M:%S}    
]]
{% endhighlight %}

![Conky: lua][image-i3-conky-2-lua]{: .img-responsive }

Reading:

* <https://github.com/brndnmtthws/conky/wiki/Convert-to-new-1.10-syntax>

-- -- --

<a name="conky-json"></a>

### 4: Conky as JSON Feed

Once again, we have to change the <code>status_command</code>.
This time we are using a shell script to start json header.
And this script will call conky.

{% highlight conf %}
bar {
    status_command ~/.config/i3/conky/03.json/json.sh
    }
{% endhighlight %}

The <code class="code-file">conkyrc.lua</code> started 
to look a bit complicated right now. 
The <code>conky.config</code> part is the same as above.
But the <code>conky.text</code> is very different
to accomodate i3bar JSON protocol.

{% highlight lua %}
conky.text = [[ 
[ 
# MPD
${if_mpd_playing} 
    {"full_text":"", "color":"\#c9c925", 
     "separator":false, "separator_block_width":6},
    {"full_text":"${mpd_artist 20}", "color" : "\#5c5dad", 
     "separator" : false, "separator_block_width":3 },
    {"full_text":" - ", "color" : "\#909737", 
     "separator" : false, "separator_block_width":3 },
    {"full_text":"${mpd_title 30}", "color" : "\#545454", 
     "separator" : false, "separator_block_width":6 },
${else} 
    {"full_text":"", "color":"\#c92525",
     "separator" : false, "separator_block_width":6 },
${endif}

# Time:
    {"full_text":"|", "color":"\#545454", 
     "separator":false, "separator_block_width":6},
    {"full_text":"", "color":"\#c9c925", 
     "separator":false, "separator_block_width":6},
    {"full_text":"TIME", "color":"\#5c5dad", 
     "separator":false,"separator_block_width":6},
    {"full_text":"${time %H:%M }", "color":"\#aaaaaa", 
     "separator":false, "separator_block_width":6 }
],
]]
{% endhighlight %}

![Conky: JSON][image-i3-conky-3-json]{: .img-responsive }

This script is only using two monitoring widgets,
it is <code>mpd</code> and <code>time</code>.
For many monitoring widget, we should reduce the complexity.

Reading

* <https://i3wm.org/docs/user-contributed/conky-i3bar.html>

-- -- --

<a name="modular"></a>

### 5: Modular

You can make your own modular script for your own needs. This just a sample.

#### The i3 config

Mine using two bars. Top and Bottom.

{% highlight conf %}
bar {
    status_command ~/.config/i3/conky/04.modular/json.top.sh
    position       top
    workspace_buttons      no
}

bar {
    status_command ~/.config/i3/conky/04.modular/json.bottom.sh
    position       bottom  
}
{% endhighlight %}

#### The Lua Helper

Since we want to reduce complexity,
we create a function for each json parts 
in <code class="code-file">jsonhelper.lua</code>.

{% highlight lua %}
-- global
jsonhelper = {}

function jsonhelper.text(text, color)
  color = color or #5c5dad

  return [[  {
    "full_text":"]] .. text .. [[",
    "color":"\]] .. color .. [[",
    "separator":false,
    "separator_block_width":6}
  ]]
end
{% endhighlight %}

I provide few functions

* jsonhelper.separator()
* jsonhelper.icon()
* jsonhelper.text()
* jsonhelper.value()
* jsonhelper.common()

The last one is just a call to 
all other function at once [separator, icon, text, value].
So we call them in just one line.

{% highlight lua %}
parts.time = jsonhelper.common('', nil, '${time %H:%M }')
parts.mem  = jsonhelper.common('', 'RAM', '$mem/$memmax')
{% endhighlight %}

The dollar <code>$</code> syntax indicate conky variables.

You can check the rest of the file in github

* [conky/helper.lua][source-helper]

If you need more icon you can copy-paste from FontAwesome cheatsheet,
but be aware that it is not always work with other font.

* <http://fontawesome.io/cheatsheet/>

#### The Parts Lua

Now we need another file to define conky parts as functions.

{% highlight lua %}
parts = {}

-- user variables
local wlandev = 'wlan0'

-- shortcut
local _h = jsonhelper

-- Media Player Daemon
parts.mpd = [[
${if_mpd_playing} 
]] .. _h.icon('') .. [[,
    {"full_text":"${mpd_artist 20}", "color" : "\#5c5dad", 
     "separator" : false, "separator_block_width":3 },
    {"full_text":" - ", "color" : "\#909737", 
     "separator" : false, "separator_block_width":3 },
    {"full_text":"${mpd_title 30}", "color" : "\#545454", 
     "separator" : false, "separator_block_width":6 }
${else} 
    {"full_text":"", "color":"\#c92525", 
     "separator" : false, "separator_block_width":6 }
${endif} 
]]
{% endhighlight %}

You can check the rest of the file in github

* [conky/jsonparts.lua][source-parts]

#### Main Conkyrc Script

Now we can call this script in <code>conky.text</code> 
in main <code class="code-file">conkyrc.lua</code> file.

{% highlight lua %}
conky.text = [[ 
[ 
]] .. parts.date .. [[, 
]] .. parts.time .. [[ 
],
]]
{% endhighlight %}

You can check the rest of the file in github

* [conky/cokyrc.top.lua][source-top]

  [![i3status: Conky Lua Top][image-i3status-top]{: .img-responsive }][photo-i3status-top]

* [conky/cokyrc.bottom.lua][source-bottom]

  [![i3status: Conky Lua Bottom][image-i3status-bottom]{: .img-responsive }][photo-i3status-bottom]
  
-- -- --

<a name="misc"></a>

### 6: Miscellanous

#### Changing Color

This is a bonus parts.
For your convenience I put two colorschemes.
It is for dark and bright wallpaper.
Of course you can add your own colorscheme.

{% highlight lua %}
local color_preset_dark = {
  icon      = '#c9c925',
  text      = '#5c5dad',
  separator = '#545454',
  value     = '#aaaaaa'
}

local color_preset_bright = {
  icon      = '#5c5dad',
  text      = '#606040',
  separator = '#c9c925',
  value     = '#000000'
}

local color_preset = color_preset_bright
{% endhighlight %}

[![i3-gaps: Conky Lua in bright i3status][image-ss-i3gaps-bright]{: .img-responsive }][photo-ss-i3gaps-bright]  

#### Font Problem

Do not forget to install font to enable your icons.

{% highlight bash %}
$ sudo pacman -S ttf-font-icons
$ yaourt -S awesome-terminal-fonts
{% endhighlight %}

-- -- --

<a name="conclusion"></a>

### Conclusion

After all this is just a config.
You may modify this config to suit your needs.

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-i3gaps-dark]:   {{ site.url }}/assets-desktop/2016/08/i3gaps-dark.png
[image-ss-i3gaps-bright]: {{ site.url }}/assets-desktop/2016/08/i3gaps-bright.png

[image-i3-workspace]:     {{ site.url }}/assets-desktop/2016/08/i3-workspace.png
[image-i3-i3status]:      {{ site.url }}/assets-desktop/2016/08/i3-i3status.png
[image-i3-i3blocks]:      {{ site.url }}/assets-desktop/2016/08/i3-i3blocks.png

[image-i3-conky-1-old]:   {{ site.url }}/assets-desktop/2016/08/i3-conky-1-old.png
[image-i3-conky-2-lua]:   {{ site.url }}/assets-desktop/2016/08/i3-conky-2-lua.png
[image-i3-conky-3-json]:  {{ site.url }}/assets-desktop/2016/08/i3-conky-3-json.png

[image-i3status-top]:     {{ site.url }}/assets-desktop/2016/08/i3-conky-lua-json-top.png
[image-i3status-bottom]:  {{ site.url }}/assets-desktop/2016/08/i3-conky-lua-json-bottom.png

[photo-ss-i3gaps-dark]:   https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMXY1ouG45kbjW_OBvrgcDyVecU1Cz55flj0wnr
[photo-ss-i3gaps-bright]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMrVos0mLsbcSZR49ZkXZwWKFSkKfmTRJ5ar6Hf

[photo-i3-i3status]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPuF-zmeePj4kYj84O0OJnYyQ5xqQlNEGiWJBMV
[photo-i3-i3blocks]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPZ0TTZtgQfxoD0Mms5sURoAsLqZekKfYKxQJQf

[photo-i3status-top]:    https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipN89O6K1_nEO7RMlR2NQ00xT2FGLmLU1HUFJCrT
[photo-i3status-bottom]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipP_wqTnm_iRFFL28-nJ-lAiExnzeWz2jWTs7CvP

[source-i3]:     https://gitlab.com/epsi-rns/dotfiles/tree/master/i3
[source-helper]: https://gitlab.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/jsonhelper.lua
[source-parts]:  https://gitlab.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/jsonparts.lua
[source-top]:    https://gitlab.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/conkyrc.top.lua
[source-bottom]: https://gitlab.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/conkyrc.bottom.lua

