---
layout: post-sidemenu-wm
title:  "Standalone CLI System Monitoring Using Conky"
categories: desktop
date:   2017-04-12 00:25:15 +0700
tags: [ricing, console, conky]
author: epsi

excerpt:
  System monitoring using Text User Interface turn out to be not so difficult.
  We have three option here, using Watch, Bash, and Conky..
---

### Statusbar Tutorial

This tutorial/ guidance/ article is one of some parts.

**Statusbar**

*	[Standalone Statusbar Overview][local-overview]

*	[Standalone Dzen2 Statusbar Using BASH][local-dzen2-bash]

*	[Standalone Dzen2 Statusbar Using Conky][local-dzen2-conky]

*	[Standalone Lemonbar Using Conky][local-lemon-conky]

**Conky**

*	[Standalone CLI System Monitoring Using Conky][local-cli-conky]

*	[Debugging Conky][local-debug-conky]

**Complementary**

*	[Create XBM for your Dzen2][local-xbm-dzen2]

*	[Getting XLFD Font][local-xlfd-font]

-- -- --

### Preface

Ricing has many component, from original wallpaper,
window manager, panel, and mostly terminal based application.
So why not make your own CLI app running in your beloved terminal ?
And give any color as you like ?
We can use conky that already equipped with system monitoring tools.
And make some Lua tweak to make our conky easy to use.

**Reading**:<br/>
*	<https://github.com/brndnmtthws/conky>

This tutorial guide you step by step
from a very simple output using watch
then Bash, Simple Cokly and later Conky Lua.

-- -- --

### Using Bash and Watch

Coloring terminal require knowledge of ANSI escape codes.

This step also print some exotic character. 
You need both FontAwesome and PowerlineSymbol,
installed properly in your system.

If you desire to keep calling the script between interval,
there is <code>watch --color</code> command line,
that suit this requirement.

{% highlight bash %}
$ watch --color ~/Documents/standalone/cli/example/01.sh
{% endhighlight %}

**Source**:

*	[github.com/.../dotfiles/.../01.sh][dotfiles-example-01-bash]

![CLI: Using Bash and Watch][image-cli-01]{: .img-responsive }

{% highlight bash %}
#!/usr/bin/env bash

esc="\033"

      fgRed="${esc}[31m"
     fgBlue="${esc}[34m"
    fgWhite="${esc}[37m"
     bgRed="${esc}[41m"
     bgBlue="${esc}[44m"
    bgWhite="${esc}[47m"
complexBlue="${esc}[1;3;4;34m"
     boldOn="${esc}[1m"
    boldOff="${esc}[22m"
      reset="${esc}[0m"
     
date=$(date +'%a %b %d')
time=$(date +'%H:%M:%S')

arrow="${boldOff}"
awesome='                   '

text=""
text+="${fgBlue} ${date} "
text+="${complexBlue} ${time}"
text+="${reset}\n"
text+="${awesome}"
text+="${reset}\n"
text+="${bgBlue}${fgWhite}${arrow}"
text+="${bgBlue}${fgWhite}${boldOn} Right "
text+="${bgRed}${fgBlue}${arrow}"
text+="${bgRed}${fgWhite}${boldOn} Arrow "
text+="${reset}${fgRed}${arrow}"
text+="\n"

echo -e $text
{% endhighlight %}

-- -- --

### Using Bash Loop Only

We can use bash loop only, instead of using <code>watch</code>.

{% highlight bash %}
$ ~/Documents/standalone/cli/example/02.sh
{% endhighlight %}

Two things should be noted is this two commands.
<code>clear</code> clear the terminal,
and <code>tput</code> move cursor to top left
everytime the loop begin.

{% highlight bash %}
$  clear
{% endhighlight %}

{% highlight bash %}
$  tput cup 0 0
{% endhighlight %}

I also put a more complete ANSI escaping code in separate bash script.

**Source**:

*	[github.com/.../dotfiles/.../01.sh][dotfiles-example-02-bash]

*	[github.com/.../dotfiles/.../ansi.sh][dotfiles-example-02-ansi]

*	[github.com/.../dotfiles/.../helpercpu.sh][dotfiles-example-02-helpercpu]

*	[github.com/.../dotfiles/.../progressbar.sh][dotfiles-example-02-progressbar]

![CLI: Using Bash Loop Only][image-cli-02]{: .img-responsive }

{% highlight bash %}
#!/usr/bin/env bash

. ~/Documents/standalone/cli/example/ansi.sh
. ~/Documents/standalone/cli/example/helpercpu.sh
. ~/Documents/standalone/cli/example/progressbar.sh

initializeANSI

arrow="${boldOff}"

# hide cursor
tput civis  -- invisible
# type' tput cnorm' to show

clear

while :; do 
    helperCPU
    tput cup 0 0
    echo ""
      
    # cpu
    value=$cpu_util    
    progressbar $value  
    percent=$(printf "[ %3d%% ]" $value)
    
    diskText="${reset}"
    diskText+=" CPU    ${fgBlue}${boldOn}$percent${reset} "
    diskText+=$progressBarText
    echo -e "$diskText"
    
    # disk
    value=$(df /home -h | awk  'FNR == 2 {print $5}' | sed s/%//)    
    progressbar $value
    percent=$(printf "[ %3d%% ]" $value)
        
    diskText="${reset}"
    diskText+=" Disk   ${fgBlue}${boldOn}$percent${reset} "
    diskText+=$progressBarText
    echo -e "$diskText"
    
    # date time
    date=$(date +'%a %b %d')
    time=$(date +'%H:%M:%S')
    
    dateText="\n${reset} "
    dateText+="${bgRed}${fgWhite}${arrow}"
    dateText+="${bgRed}${fgWhite}${boldOn} ${date} "
    dateText+="${bgWhite}${fgRed}${arrow}"
    dateText+="${bgWhite}${fgRed} ${time} "
    dateText+="${reset}${fgWhite}${arrow}"
    dateText+="${reset}\n"
    echo -e "$dateText"
    
    # host
    
    value=$(uname -n)
    
    echo -e " ${fgRed}  ${reset}Host ${fgBlue}${boldOn}$value\n"

    sleep 2
done

{% endhighlight %}

Since it looks complicated, we have to move the complex part
somewhere else.


-- -- --

### A Very Simple Conky

This part move the output part from Bash to Conky Lua.
In Conky, printing Date and Time this should be as easy as this line.

{% highlight lua %}
conky.text = [[\
${time %a %b %d} - ${time %H:%M:%S}\
]]
{% endhighlight %}

The bash part only contain the <code>clear</code>.
Conky has capability to initialize stuff,
and run once before rendering text.
The issue is, it use Conky environment,
and it does not use terminal environment.
So we still need Bash to clear the terminal.

{% highlight bash %}
$ ~/Documents/standalone/cli/example/03.sh
{% endhighlight %}

**Source**:

*	[github.com/.../dotfiles/.../01.sh][dotfiles-example-03-bash]

*	[github.com/.../dotfiles/.../01.lua][dotfiles-example-03-lua]

![CLI: A Very Simple Conky][image-cli-03]{: .img-responsive }

{% highlight bash %}
#!/usr/bin/env bash

# hide cursor
# type' tput cnorm' to show
tput civis  -- invisible

clear

conky -c ~/Documents/standalone/cli/example/03.lua 
{% endhighlight %}

{% highlight lua %}
-- vim: ts=4 sw=4 noet ai cindent syntax=lua

--[[
Conky, a system monitor, based on torsmo
]]

conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

-- Lua Function Demo 
-- https://github.com/brndnmtthws/conky/issues/62

function exec(command)
    local file = assert(io.popen(command, 'r'))
    local s = file:read('*all')
    file:close()

    s = string.gsub(s, '^%s+', '') 
    s = string.gsub(s, '%s+$', '') 
    s = string.gsub(s, '[\n\r]+', ' ')

    return s
end


function gototopleft()
  return exec('tput cup 0 0') 
end

conky.text = gototopleft() .. [[\
  ${time %a %b %d} -  ${time %H:%M:%S}\
]]
{% endhighlight %}


-- -- --

### Using Conky Lua

I do not prefer complexity,
so I put the detail of system monitoring part in submodule libraries.

Now you can see in code below. That's tidy.

{% highlight bash %}
$ ~/Documents/standalone/cli/conky/main.sh
{% endhighlight %}

**Source**:

*	[github.com/.../dotfiles/.../main.sh][dotfiles-main]

*	[github.com/.../dotfiles/.../conky.lua][dotfiles-conky]

*	[github.com/.../dotfiles/.../ansi.lua][dotfiles-ansi]

*	[github.com/.../dotfiles/.../helper.lua][dotfiles-helper]

*	[github.com/.../dotfiles/.../parts.lua][dotfiles-parts]

![CLI: Using Conky Lua][image-cli-04]{: .img-responsive }

{% highlight lua %}
-- vim: ts=4 sw=4 noet ai cindent syntax=lua

--[[
Conky, a system monitor, based on torsmo
]]

conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

-- load subroutine
home = os.getenv("HOME")
path = '/Documents/standalone/cli/conky/'
dofile(home .. path .. 'ansi.lua')
dofile(home .. path .. 'helper.lua')
dofile(home .. path .. 'parts.lua')

-- shortcut
local _h = helper

--[[
-- if you care about performance, comment-out this variable.
disabled = ''
    .. parts.title     
    .. parts.newline    
    .. parts.newline
    .. parts.date
    .. parts.time    
    .. parts.newline 
    .. parts.mem    
]]

enabled = ''
    .. parts.newline 
    .. parts.newline

    .. parts.uptime
    .. parts.newline
    .. parts.host
    .. parts.machine    
    .. parts.newline
    
    .. parts.volume
    .. parts.newline
    .. parts.mpd
    .. parts.newline
    .. parts.newline 
       
    .. parts.datetime   
    .. parts.newline 
    .. parts.newline

    .. parts.cpu0        
    .. parts.newline
    .. parts.cputemp
    .. parts.newline
    .. parts.memory
    .. parts.newline
    .. parts.battery    
      
    .. parts.newline
    .. parts.newline
    .. parts.ssid
    .. parts.network
    .. parts.newline 
    .. parts.newline
    
conky.text = _h.gototopleft() .. [[\
]] .. enabled .. [[
]]

{% endhighlight %}

-- -- --

Coding is Fun.
Again, this is just a quick and dirty solution.
You can do better.

That is all for now.
Thank you for reading.

Have Fun



[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/cli/conky' %}
{% assign dotfiles_expath = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/cli/example' %}

[image-cli-01]: {{ asset_path }}/cli-01-bash-watch.png
[image-cli-02]: {{ asset_path }}/cli-02-bash-loop.png
[image-cli-03]: {{ asset_path }}/cli-03-conky-simple.png
[image-cli-04]: {{ asset_path }}/cli-04-conky-lua-pause.png

[dotfiles-example-01-bash]: {{ dotfiles_expath }}/01.sh
[dotfiles-example-02-bash]: {{ dotfiles_expath }}/02.sh
[dotfiles-example-03-bash]: {{ dotfiles_expath }}/03.sh
[dotfiles-example-03-lua]:  {{ dotfiles_expath }}/03.lua
[dotfiles-example-02-ansi]:        {{ dotfiles_expath }}/ansi.sh
[dotfiles-example-02-helpercpu]:   {{ dotfiles_expath }}/helpercpu.sh
[dotfiles-example-02-progressbar]: {{ dotfiles_expath }}/progressbar.sh

[dotfiles-main]:   {{ dotfiles_path }}/main.sh
[dotfiles-conky]:  {{ dotfiles_path }}/conky.lua
[dotfiles-ansi]:   {{ dotfiles_path }}/ansi.lua
[dotfiles-helper]: {{ dotfiles_path }}/helper.lua
[dotfiles-parts]:  {{ dotfiles_path }}/parts.lua

[local-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html
[local-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html
