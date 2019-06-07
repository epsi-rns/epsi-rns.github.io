---
layout: post
title:  "HerbstluftWM Tag Status in Lua"
date      : 2017-06-07 17:35:15 +0700
categories: desktop
tags      : [coding, lua, herbstluftwm, statusbar]
keywords  : [tag status, lemonbar, dzen2]
author: epsi

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in Lua script.

related_link_ids: 
  - 17060135  # Tag Status Overview
  - 17060235  # Tag Status BASH
  - 17060335  # Tag Status Perl
  - 17060435  # Tag Status Python
  - 17060535  # Tag Status Ruby
  - 17060635  # Tag Status PHP
  - 17060735  # Tag Status Lua
  - 17060835  # Tag Status Haskell
  - 17060935  # Tag Status Global Notes

---

### Preface

> Goal: Show the Herbstclient Tag.

	Focusing in "herbstclient tag_status". 

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

This tutorial cover Lemonbar, and in order to use Dzen2,
any reader could use the source code in github.

-- -- --

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[HerbstluftWM Tag Status Overview][local-overview]

*	[Modularized HerbstluftWM in Lua][local-lua-config]

*	[Piping and Forking in Lua][local-lua-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/][dotfiles-dzen2-lua]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/][dotfiles-lemon-lua]

-- -- --

{% include post/2017/06/herbstlustwm-tag-status-language.md %}

-- -- --

### Screenshot

Since window manager is out of topic in this tutorial,
I present **only panel** HerbstluftWM screenshot.

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
For both Dzen2 and Lemonbar, the structure are the same.
This figure will explain how it looks
in <code>Lua script</code> directory.

![Statusbar: Directory Structure][image-01-tree-lua]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

### Common Module

Lua is an embedding language. It is suppose to be light.
No wonder it lacks of standard function for daily coding,
such as <code>split</code>, <code>trim</code>, <code>sleep</code>,
that we are going to use in this project.

I don't like to grumble.
I'd better collect from some resurces in the internet.

<code class="code-file">common.lua</code>

{% highlight lua %}
local _M = {}

function _M.sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

function _M.split(inputstr, sep)
        if sep == nil then
                sep = "%s"
        end
        local t={} ; i=1 -- non zero based
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
end

function _M.trim1(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

function _M.has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/common.lua][dotfiles-dzen2-lua-common]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/common.lua][dotfiles-lemon-lua-common]

-- -- --

### Get Geometry

Let's have a look at <code class="code-file">helper.lua</code> in github.

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/helper.lua][dotfiles-dzen2-lua-helper]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/helper.lua][dotfiles-lemon-lua-helper]

{% include post/2017/06/herbstlustwm-tag-status-helper.md %}

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight bash %}
$ ./panel.lua 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.
Here it is our code in Lua.

<code class="code-file">helper.lua</code>

{% highlight lua %}
local _M = {}

function _M.get_monitor(arguments)
    -- no ternary operator, using expression
    return (#arguments > 0) and tonumber(arguments[1]) or 0
end
{% endhighlight %}

Calling external module in Lua using relative directory
need special tricks.

{% highlight lua %}
local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path   = package.path .. ';' .. dirname .. '?.lua;'

local helper      = require('.helper')
{% endhighlight %}

Now in main code we can call

{% highlight lua %}
local monitor = helper.get_monitor(arg)
print(monitor)
{% endhighlight %}

This will display <code>0</code> or else such as <code>1</code>,
depend on the script argument given.

{% highlight conf %}
0
{% endhighlight %}

#### Get Monitor Geometry

HerbstluftWM give this little tools to manage monitor geometry
by getting monitor rectangle.

{% highlight bash %}
$ herbstclient monitor_rect
{% endhighlight %}

This will show something similar to this.

{% highlight conf %}
0 0 1280 800
{% endhighlight %}

![HerbstluftWM: Monitor Rectangle][image-hlwm-02-monitor-rect]{: .img-responsive }

Consider wrap the code into function.
And get an array as function return.

<code class="code-file">helper.lua</code>

{% highlight lua %}
function _M.get_geometry(monitor)
    local command = 'herbstclient monitor_rect ' .. monitor
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close()

    if (result == nil or result == '') then
        print('Invalid monitor ' .. monitors)
        os.exit()
    end      
        
    local raw = common.trim1(result)  
    local geometry = common.split(raw, ' ')
    
    return geometry
end
{% endhighlight %}

Consider call this function from script later.
To print array in Lua,
we just have to wrap it in <code>table.concat(geometry, ' ')</code>.
Just remember that Lua array is not zero based.

{% highlight lua %}
local monitor  = helper.get_monitor(arg)
local geometry = helper.get_geometry(monitor)
print(table.concat(geometry, ' '))
{% endhighlight %}

This will produce

{% highlight conf %}
0 0 1280 800
{% endhighlight %}

#### Get Panel Geometry

The Panel geometry is completely depend on the user flavor and taste.
You can put it, on top, or bottom, or hanging somewhere.
You can create gap on both left and right.

Consider this example:
<code class="code-file">helper.lua</code>

{% highlight lua %}
function _M.get_bottom_panel_geometry(height, geometry)
    -- geometry has the format X Y W H
    return tonumber(geometry[1]) + 24, tonumber(geometry[4]) - height, 
           tonumber(geometry[3]) - 48, height
end
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight lua %}
local panel_height = 24
local monitor  = helper.get_monitor(arg)
local geometry = helper.get_geometry(monitor)
local xpos, ypos, width, height = helper.get_bottom_panel_geometry(
      panel_height, geometry)

print('Lemonbar geometry: ' 
    .. tostring(width) .. 'x' .. tostring(height) .. '+'
    .. tostring(xpos)  .. '+' .. tostring(ypos))
{% endhighlight %}

This will show something similar to this result,
depend on your monitor size.

{% highlight conf %}
Lemonbar geometry: 1280x24+24+776
{% endhighlight %}

#### Get Lemonbar Parameters

We almost done. 
This is the last step.
We wrap it all inside this function below.

<code class="code-file">helper.lua</code>

{% highlight lua %}
function _M.get_lemon_parameters(monitor, panel_height)
    -- calculate geometry
    local geometry = _M.get_geometry(monitor)
    local xpos, ypos, width, height = _M.get_bottom_panel_geometry(
        panel_height, geometry)

    -- geometry: -g widthxheight+x+y
    local geom_res = tostring(width) .. 'x' .. tostring(height)
           .. '+' .. tostring(xpos)  .. '+' .. tostring(ypos)

    -- color, with transparency    
    local bgcolor = "'#aa000000'"
    local fgcolor = "'#ffffff'"

    -- XFT: require lemonbar_xft_git 
    local font_takaop  = "takaopgothic-9"
    local font_bottom  = "monospace-9"
    local font_symbol  = "PowerlineSymbols-11"
    local font_awesome = "FontAwesome-9"
  
    local parameters = ""
        .. " -g "..geom_res.." -u 2"
        .. " -B "..bgcolor.." -F "..fgcolor
        .. " -f "..font_takaop
        .. " -f "..font_awesome
        .. " -f "..font_symbol
        
    return parameters
end
{% endhighlight %}

-- -- --

### Testing The Parameters

Consider this code <code class="code-file">01-testparams.lua</code>.
The script call the above function to get lemon parameters.

{% highlight lua %}
#!/usr/bin/lua

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path   = package.path .. ';' .. dirname .. '?.lua;'
  
local helper      = require('.helper')

-- initialize
local panel_height = 24
local monitor = helper.get_monitor(arg)

local lemon_parameters = helper.get_lemon_parameters(monitor, panel_height)
print(lemon_parameters)
{% endhighlight %}

This will produce output
something similar to this result

{% highlight conf %}
-g 1280x24+0+776 -u 2 -B '#aa000000' -F '#ffffff' 
-f takaopgothic-9 -f FontAwesome-9 -f PowerlineSymbols-11
{% endhighlight %}

Or in Dzen2 version:

{% highlight conf %}
-x 0 -y 0 -w 1280 -h 24 -ta l 
-bg '#000000' -fg '#ffffff' -title-name dzentop 
-fn '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*'
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/01-testparams.lua][dotfiles-dzen2-lua-testparams]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/01-testparams.lua][dotfiles-lemon-lua-testparams]

{% include post/2017/06/herbstlustwm-tag-status-testparams.md %}

-- -- --

### Adjusting the Desktop

Since we want to use panel, we have to adjust the desktop gap,
giving space at the top and bottom.

{% highlight bash %}
$ herbstclient pad 0 24 0 24 0
{% endhighlight %}

For more information, do <code>$ man herbsluftclient</code>,
and type <code>\pad</code> to search what it means.

In script, it looks like this below.

{% highlight lua %}
os.execute('herbstclient pad ' .. monitor .. ' ' 
    .. panel_height .. ' 0 ' .. panel_height .. ' 0')
{% endhighlight %}

-- -- --

### Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

<code class="code-file">gmc.lua</code>

{% highlight lua %}
_M.color = {
    ['white'] = '#ffffff',
    ['black'] = '#000000',

    ['grey50']  = '#fafafa',
    ['grey100'] = '#f5f5f5'
}
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/gmc.lua][dotfiles-dzen2-lua-gmc]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/gmc.lua][dotfiles-lemon-lua-gmc]

{% include post/2017/06/herbstlustwm-tag-status-gmc.md %}

-- -- --

### Preparing Output

Let's have a look at <code class="code-file">output.lua</code> in github.

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/output.lua][dotfiles-dzen2-lua-output]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/output.lua][dotfiles-lemon-lua-output]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

-- -- --

### Global Variable and Constant

Officialy there is a no way to define constant in Lua.
Lua does not differ between these two.

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.lua</code>
In script, we initialize the variable as below

{% highlight lua %}
_M.segment_windowtitle = '' -- empty string
_M.tags_status = {}         -- empty table
{% endhighlight %}

Each segment buffered.
And will be called while rendering the panel.

#### Global Constant: Tag Name 

Assuming that herbstclient tag status
only consist of nine number element.

{% highlight bash %}
$ herbstclient tag_status
	#1	:2	:3	:4	:5	.6	.7	.8	.9	
{% endhighlight %}

We can manage custom tag names,
consist of nine string element.
We can also freely using *unicode* string instead of plain one.

<code class="code-file">output.lua</code>

{% highlight lua %}
_M.tag_shows = {'一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'}
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.lua</code>
Decoration consist lemonbar formatting tag.

{% highlight lua %}
local gmc = require('.gmc')
local _M = {}

-- decoration
_M.separator = '%{B-}%{F' .. gmc.color['yellow500'] .. '}|%{B-}%{F-}'

-- Powerline Symbol
_M.right_hard_arrow = ""
_M.right_soft_arrow = ""
_M.left_hard_arrow  = ""
_M.left_soft_arrow  = ""

-- theme
_M.pre_icon    = '%{F' .. gmc.color['yellow500'] .. '}'
_M.post_icon   = '%{F-}'
{% endhighlight %}

-- -- --

### Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.lua</code>

{% highlight lua %}
function _M.set_tag_value(monitor)
    local command = 'herbstclient tag_status ' .. monitor
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close() 
        
    local raw = common.trim1(result)  
    _M.tags_status = common.split(raw, "\t")
end
{% endhighlight %}

This function above turn the tag status string
into array of tags for later use.

<code class="code-file">output.lua</code>

{% highlight lua %}
function _M.set_windowtitle(windowtitle)
    local icon = _M.pre_icon .. '' .. _M.post_icon

    if (windowtitle == nil) then windowtitle = '' end
      
    _M.segment_windowtitle = ' ' .. icon ..
        ' %{B-}%{F' .. gmc.color['grey700'] .. '} ' .. windowtitle
end
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And then returning string as result.

<code class="code-file">output.lua</code>

{% highlight lua %}
function _M.output_by_title()
    local text = _M.segment_windowtitle .. ' ' .. _M.separator .. '  '

    return text
end
{% endhighlight %}

-- -- --

### Decorating: Tag Status

This transform each plain tag such as <code>.2</code>,
to decorated tag names such as <code>二 ni</code>.
Note that it only process one tag.
We process all tags in a loop in other function.

This has some parts:

*	Pre Text: Color setting for Main Text
	(Background, Foreground, Underline).
	Arrow before the text, only for active tag.

*	Main Text: Tag Name by number,
	each with their tag state <code>#</code>, <code>+</code>,
	<code>.</code>, <code>|</code>, <code>!</code>,
	and each tag has clickable area setting.

*	Post Text: 
	Arrow after the text, only for active tag.

*	Color Reset: <code>%{B-}</code>,
	<code>%{F-}</code>, <code>%{-u}</code>
	(Background, Foreground, Underline).

<code class="code-file">output.lua</code>

{% highlight lua %}
function _M.output_by_tag(monitor, tag_status)
    local tag_index  = string.sub(tag_status, 2, 2)
    local tag_mark   = string.sub(tag_status, 1, 1)
    local index      = tonumber(tag_index)-- not a zero based array
    local tag_name   = _M.tag_shows[index]

    -- ----- pre tag

    local text_pre = ''
    if tag_mark == '#' then
        text_pre = '%{B' .. gmc.color['blue500'] .. '}'
                .. '%{F' .. gmc.color['black'] .. '}'
                .. '%{U' .. gmc.color['white'] .. '}%{+u}' 
                .. _M.right_hard_arrow
                .. '%{B' .. gmc.color['blue500'] .. '}'
                .. '%{F' .. gmc.color['white'] .. '}'
                .. '%{U' .. gmc.color['white'] .. '}%{+u}'
    elseif tag_mark == '+' then
        text_pre = '%{B' .. gmc.color['yellow500'] .. '}'
                .. '%{F' .. gmc.color['grey400'] .. '}'
    elseif tag_mark == ':' then
        text_pre = '%{B-}%{F' .. gmc.color['white'] .. '}'
                .. '%{U' .. gmc.color['red500'] .. '}%{+u}'
    elseif tag_mark == '!' then
        text_pre = '%{B' .. gmc.color['red500'] .. '}'
                .. '%{F' .. gmc.color['white'] .. '}'
                .. '%{U' .. gmc.color['white'] .. '}%{+u}'
    else
        text_pre = '%{B-}%{F' .. gmc.color['grey600'] .. '}%{-u}'
    end

    -- ----- tag by number

    -- clickable tags
    local text_name = '%{A:herbstclient focus_monitor '
                   .. '"' .. monitor .. '" && '
                   .. 'herbstclient use "' .. tag_index .. '":}'
                   .. ' ' .. tag_name ..' %{A} '
    
    -- non clickable tags
    -- local text_name = ' ' .. tag_name .. ' '

    -- ----- post tag

    local text_post = ""
    if (tag_mark == '#') then
        text_post = '%{B-}' 
                 .. '%{F' .. gmc.color['blue500'] .. '}' 
                 .. '%{U' .. gmc.color['red500'] .. '}%{+u}' 
                 .. _M.right_hard_arrow
    end

    text_clear = '%{B-}%{F-}%{-u}'

     
    return text_pre .. text_name .. text_post .. text_clear
end
{% endhighlight %}

-- -- --

### Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar is using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.lua</code>

{% highlight lua %}
function _M.get_statusbar_text(monitor)
    local text = ''
    
    -- draw tags, zero based
    text = text .. '%{l}'
    for index = 1, #(_M.tags_status) do
        text = text .. _M.output_by_tag(monitor, _M.tags_status[index])
    end
    
    -- draw window title 
    text = text .. '%{r}'
    text = text .. _M.output_by_title()
  
    return text
end
{% endhighlight %}

-- -- --

### Testing The Output

Consider this code <code class="code-file">02-testoutput.lua</code>.
The script using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight lua %}
#!/usr/bin/lua

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path   = package.path .. ';' .. dirname .. '?.lua;'
local helper      = require('.helper')

-- process handler
function test_lemon(monitor, parameters) 
    local output = require('.output')

    local command_out  = 'lemonbar ' .. parameters .. ' -p'
    local pipe_out = assert(io.popen(command_out, 'w'))
    
    -- initialize statusbar before loop
    output.set_tag_value(monitor)
    output.set_windowtitle('test')

    local text = output.get_statusbar_text(monitor)
    pipe_out:write(text .. "\n")
    pipe_out:flush()
        
    pipe_out:close()
end

-- initialize
local panel_height = 24
local monitor = helper.get_monitor(arg)
local lemon_parameters = helper.get_lemon_parameters(
    monitor, panel_height)

-- test
os.execute('herbstclient pad ' .. monitor .. ' ' 
    .. panel_height .. ' 0 ' .. panel_height .. ' 0')

test_lemon(monitor, lemon_parameters)
{% endhighlight %}

This will produce a panel on top.

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

The panel only contain the initialized version of the text.
It does not really interact with the HerbstluftWM event.

You can also click the clickable area to see it's result.
It only show text, not executed yet.

{% highlight bash %}
herbstclient focus_monitor "0" && herbstclient use "2"
herbstclient focus_monitor "0" && herbstclient use "3"
{% endhighlight %}

#### View Source File:

*	**dzen2**: 
	[gitlab.com/.../dotfiles/.../lua/01-testoutput.lua][dotfiles-dzen2-lua-testoutput]

*	**lemonbar**: 
	[gitlab.com/.../dotfiles/.../lua/01-testoutput.lua][dotfiles-lemon-lua-testoutput]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

-- -- --

### Coming up Next

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

*	[HerbstluftWM Event Idle in Haskell][local-lua-idle]

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-lua]:  {{ asset_path }}/hlwm-statusbar-01-tree-lua.png

[local-lua-config]: {{ site.url }}/desktop/2017/05/07/herbstlustwm-modularized-lua.html
[local-lua-pipe]:   {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-lua-idle]:   {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html

[dotfiles-dzen2-lua-testparams]:  {{ dotfiles_dzen2 }}/lua/01-testparams.lua
[dotfiles-dzen2-lua-testoutput]:  {{ dotfiles_dzen2 }}/lua/02-testoutput.lua
[dotfiles-dzen2-lua-panel]:       {{ dotfiles_dzen2 }}/lua/panel.lua
[dotfiles-dzen2-lua-gmc]:         {{ dotfiles_dzen2 }}/lua/gmc.lua
[dotfiles-dzen2-lua-common]:      {{ dotfiles_dzen2 }}/lua/common.lua
[dotfiles-dzen2-lua-helper]:      {{ dotfiles_dzen2 }}/lua/helper.lua
[dotfiles-dzen2-lua-output]:      {{ dotfiles_dzen2 }}/lua/output.lua
[dotfiles-dzen2-lua-pipehandler]: {{ dotfiles_dzen2 }}/lua/pipehandler.lua

[dotfiles-lemon-lua-testparams]:  {{ dotfiles_lemon }}/lua/01-testparams.lua
[dotfiles-lemon-lua-testoutput]:  {{ dotfiles_lemon }}/lua/02-testoutput.lua
[dotfiles-lemon-lua-panel]:       {{ dotfiles_lemon }}/lua/panel.lua
[dotfiles-lemon-lua-gmc]:         {{ dotfiles_lemon }}/lua/gmc.lua
[dotfiles-lemon-lua-common]:      {{ dotfiles_lemon }}/lua/common.lua
[dotfiles-lemon-lua-helper]:      {{ dotfiles_lemon }}/lua/helper.lua
[dotfiles-lemon-lua-output]:      {{ dotfiles_lemon }}/lua/output.lua
[dotfiles-lemon-lua-pipehandler]: {{ dotfiles_lemon }}/lua/pipehandler.lua
