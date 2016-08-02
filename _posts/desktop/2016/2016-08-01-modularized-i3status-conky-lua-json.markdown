---
layout: post
title:  "Modularized Conky Configuration for i3status"
categories: desktop
date:   2016-08-01 20:19:15 +0700
tags: [i3]
author: epsi

excerpt:
  This article explain conky lua configuration
  for i3status using JSON.

related_link_ids:
  - 14120646  # i3 Window manager

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will move this page to Tutorial section later.
</div>

I'm so excited with the release of Conky V1.10.
The latest conky configuration is actualy a Lua file.
It is very flexible, compared with the old plain text Conky Configuration.
We can separate the long old conky TEXT, into separate Lua function.

One application benefit from this Conky Lua Config is i3status.
Before we get too deep, let's see the old way of configuring i3status with Conky.

-- -- --

## Screenshot

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

[![i3-gaps: Conky Lua in i3status][image-ss-i3gaps-dark]{: .img-responsive }][picasa-ss-i3gaps-dark]

-- -- --

## The i3 config

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

![i3status: workspace][image-workspace]{: .img-responsive }

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

## Using i3bar

AFAIK there are three kind of configuration
that can be used as a feed to i3bar.

### i3status

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

[![i3status][image-i3-i3status]{: .img-responsive }][picasa-i3-i3status]

### i3blocks

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

[![i3blocks][image-i3-i3blocks]{: .img-responsive }][picasa-i3-i3blocks]

### Conky

Example

{% highlight conf %}
bar {
     status_command conky -c ~/.config/i3/conkyrc
    }
{% endhighlight %}

We can have many i3bar in i3wm.
Each configured in different bar section.

-- -- --

## Plain Conky

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

## Conky Config as Lua

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

## Conky as JSON Feed

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

## Modular

You can make your own modular script for your own needs. This just a sample.

### The i3 config

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

### The Lua Helper

Since we want to reduce complexity,
we create a functio for each json parts 
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

### The Parts Lua

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


### Main Conkyrc Script

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

  [![i3status: Conky Lua Top][image-i3status-top]{: .img-responsive }][picasa-i3status-top]

* [conky/cokyrc.bottom.lua][source-bottom]

  [![i3status: Conky Lua Bottom][image-i3status-bottom]{: .img-responsive }][picasa-i3status-bottom]

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-i3gaps-dark]: {{ site.url }}/assets/posts/desktop/2016/08/i3gaps-dark.png

[image-i3-workspace]: {{ site.url }}/assets/posts/desktop/2016/08/i3-workspace.png
[image-i3-i3status]: {{ site.url }}/assets/posts/desktop/2016/08/i3-i3status.png
[image-i3-i3blocks]: {{ site.url }}/assets/posts/desktop/2016/08/i3-i3blocks.png

[image-i3-conky-1-old]: {{ site.url }}/assets/posts/desktop/2016/08/i3-conky-1-old.png
[image-i3-conky-2-lua]: {{ site.url }}/assets/posts/desktop/2016/08/i3-conky-2-lua.png
[image-i3-conky-3-json]: {{ site.url }}/assets/posts/desktop/2016/08/i3-conky-3-json.png

[image-i3status-top]: {{ site.url }}/assets/posts/desktop/2016/08/i3-conky-lua-json-top.png
[image-i3status-bottom]: {{ site.url }}/assets/posts/desktop/2016/08/i3-conky-lua-json-bottom.png

[picasa-ss-i3gaps-dark]: https://lh3.googleusercontent.com/-z2h94mqwszU/V59JN7KCTyI/AAAAAAAAAsI/Dj76UEcWbnkhowZUobnrj8uwC6aA-VcuwCCo/s0/i3gaps-dark.png

[picasa-i3-i3status]: https://lh3.googleusercontent.com/-rCE8C1AZe8E/V6CmyLvsNHI/AAAAAAAAAtc/sYzQbxCPuSMC_GYnLtBx7zHXidbm1GnkACCo/s0/i3-i3status.png
[picasa-i3-i3blocks]: https://lh3.googleusercontent.com/-BqXzPSz6fdU/V6CmyPLOKzI/AAAAAAAAAtc/iVJeU9TmRY80CItOLyGkZT3EEw3wpMP-ACCo/s0/i3-i3blocks.png

[picasa-i3status-top]: https://lh3.googleusercontent.com/-rUBIF6prl9M/V59hvuz00JI/AAAAAAAAAsg/_MkrH8oQLko9XY1wF4UWkrxrFV0aEwcVgCCo/s0/conky-lua-json-top.png
[picasa-i3status-bottom]: https://lh3.googleusercontent.com/-Ob6R5cfkNlE/V59hvg54EoI/AAAAAAAAAsg/IBBJhBRtZ54KljugJopeQCYac3rJwbJlgCCo/s0/conky-lua-json-bottom.png

[source-i3]: https://github.com/epsi-rns/dotfiles/tree/master/i3
[source-helper]: https://github.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/jsonhelper.lua
[source-parts]: https://github.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/jsonparts.lua
[source-top]: https://github.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/conkyrc.top.lua
[source-bottom]: https://github.com/epsi-rns/dotfiles/blob/master/i3/conky/04.modular/conkyrc.bottom.lua

