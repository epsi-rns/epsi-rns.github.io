---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM in Lua"
date:   2017-05-07 17:35:15 +0700
categories: desktop
tags: [coding, lua, herbstluftwm]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using Lua.
  
---

### Preface

	Goal: Separate Main Flow, Code, and Data.

So anyone can focus to alter special customization in Main Script,
without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to know read this overview.

*	[Modularized HerbstluftWM Overview][local-overview]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[github.com/.../dotfiles/.../lua/][dotfiles-lua-directory]

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
This figure will explain how it looks 
in <code>Lua script</code> directory.

![HerbstluftWM: Directory Structure][image-lua-01-tree]{: .img-responsive }

-- -- --

### Modularizing in Lua

Lua is simple, except for importing module using relative path.
This require a workaround with <code>package.path</code>.

#### Declare a module

No need to explicitly define what to export.

{% highlight lua %}
local _M = {}

function _M.hc(arguments)
    os.execute("herbstclient " .. arguments)
end

return _M
{% endhighlight %}

#### Call a module

Note the dot <code>.</code>.

{% highlight lua %}
local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path = package.path .. ';' .. dirname .. '?.lua;'

local helper  = require(".helper")
{% endhighlight %}

-- -- --

### System Calls

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.lua</code>

{% highlight lua %}
function _M.hc(arguments)
    os.execute("herbstclient " .. arguments)
end
{% endhighlight %}

<code class="code-file">autostart.lua</code>

{% highlight lua %}
-- Read the manual in $ man herbstluftwm
helper.hc('emit_hook reload')

-- gap counter
os.execute("echo 35 > /tmp/herbstluftwm-gap")
{% endhighlight %}

-- -- --

### Array: Tag Names and Keys

Is it just me? Or didn't I do googling hard enough ?
I cannot find a way to define array by range in just one line.

<code class="code-file">config.lua</code>

{% highlight lua %}
_M.tag_names = {}; _M.tag_keys = {} -- new array

for i = 1,9 do _M.tag_names[i-1]=i end
for i = 1,9 do _M.tag_keys[i-1]=i  end

_M.tag_keys[9] = 0
{% endhighlight %}

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

-- -- --

### Hash: Color Schemes

Using **key-value pairs**, a simple data structure.

<code class="code-file">assets/gmc.lua</code>

{% highlight lua %}
_M.color = {
    ['white'] = '#ffffff',
    ['black'] = '#000000',

    ['grey50']  = '#fafafa',
    ['grey100'] = '#f5f5f5'
}
{% endhighlight %}

<code class="code-file">autostart.lua</code>

{% highlight lua %}
-- background before wallpaper
os.execute("xsetroot -solid '" .. gmc.color["blue500"] .. "'")
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../lua/assets/gmc.lua][dotfiles-lua-gmc]

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string concatenation all over the place.

<code class="code-file">config.lua</code>

{% highlight lua %}
-- Modifier variables
s = 'Shift';
c = 'Control';
m = 'Mod4';
a = 'Mod1';

_M.keybinds = {
  -- session
    [m .. '-' .. s .. '-q'] = 'quit',
    [m .. '-' .. s .. '-r'] = 'reload',
    [m .. '-' .. s .. '-c'] = 'close',
}
{% endhighlight %}

This config will be utilized in main script

<code class="code-file">autostart.lua</code>

{% highlight lua %}
helper.do_config('keybind',   config.keybinds)
helper.do_config('keybind',   config.tagskeybinds)
helper.do_config('mousebind', config.mousebinds)
helper.do_config('attr',      config.attributes)
helper.do_config('set',       config.sets)
helper.do_config('rule',      config.rules)
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../lua/config.lua][dotfiles-lua-config]

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.
I can see how simple and clean, Lua is.

<code class="code-file">helper.lua</code>

{% highlight lua %}
function _M.do_config(command, hash)
    -- loop over hash
    for key, value in pairs(hash) do 
        _M.hc(command .. ' ' .. key .. ' ' .. value)

        -- uncomment to debug in terminal
        -- print(command .. ' ' .. key .. ' ' .. value)
    end
end
{% endhighlight %}

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight lua %}
        print(command .. ' ' .. key .. ' ' .. value)
{% endhighlight %}

You can see the debugging result in figure below.

![HerbstluftWM: Tag Status][image-hlwm-03-debug-config]{: .img-responsive }

#### View Source File:

*	[github.com/.../dotfiles/.../lua/helper.lua][dotfiles-lua-helper]

-- -- --

### Setting the Tags

Nothing special here,
Ruby read all exported variable from modules.
And I define local <code>tag_names</code> to avoid long namespace.

<code class="code-file">helper.lua</code>

{% highlight lua %}
local config  = require(".config")

function _M.set_tags_with_name()
    local tag_names = config.tag_names
    local tag_keys = config.tag_keys    
    
    _M.hc("rename default '" .. tag_names[0] .. "' 2>/dev/null || true")
    
    for index, value in pairs(tag_names) do 
        _M.hc("add '" .. value .. "'");
        
        local key = tag_keys[index]
        if (not (key == nil or key == '') ) then
            _M.hc("keybind Mod4-"..key.." use_index '"..index.."'")
            _M.hc("keybind Mod4-Shift-"..key.." move_index '"..index.."'")
        end
    end
end
{% endhighlight %}

-- -- --

### Launch the Panel

Two more functions left, it is <code>do_panel</code>
and <code>startup_run</code>.

This two is longer in Lua, compared to another script.
<code class="code-file">helper.lua</code>

{% highlight lua %}
function _M.do_panel() 
    local dirname  = debug.getinfo(1).source:match("@?(.*/)")   
    local panel   = dirname .. "../bash/dzen2/panel.sh"
    if (not file_exists(panel)) then
        panel = "/etc/xdg/herbstluftwm/panel.sh"
    end

    command = 'herbstclient list_monitors | cut -d: -f1'
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close()
    
    local raw = trim1(result) 
    local monitors = lines(result)
    
    for i, monitor in pairs(monitors) do 
        if (not (monitor == nil or monitor == '') ) then
            -- start it on each monitor
            os.execute(panel .. " " .. monitor .." > /dev/null &")
        end
    end
end
{% endhighlight %}

#### Specific Lua Issue

Is it just me? Or didn't I do googling hard enough ?
I cannot find a way to make the script shorter.
It is almost three times longer than the BASH counterpart.

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.lua</code>

{% highlight lua %}
function _M.run()
    -- redirect stderror to stdout, then capture the result
    command = 'new_attr bool my_not_first_autostart'
    local handle = io.popen('herbstclient ' .. command .. ' 2>&1')
    local result = handle:read('*a')
    local exitcode = handle:close()
    
    if ((result == nil or result == '')) then
     -- non windowed app
        os.execute('compton &')
        os.execute('dunst &')
        os.execute('parcellite &')
        os.execute('nitrogen --restore &')
        os.execute('mpd &')

     -- windowed app
        os.execute('xfce4-terminal &')
        os.execute('sleep 1 && firefox &')
        os.execute('sleep 2 && geany &')
        os.execute('sleep 2 && thunar &')
    end
end
{% endhighlight %}

#### Specific Lua Issue

Is it just me? Or didn't I do googling hard enough ?
Instead of exitcode, I'm using standard error
to check whether it should be launch or not.

#### View Source File:

*	[github.com/.../dotfiles/.../lua/startup.lua][dotfiles-lua-startup]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.lua</code>

{% highlight lua %}
local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path = package.path .. ';' .. dirname .. '?.lua;'

local gmc     = require ".assets/gmc"
local helper  = require(".helper")
local config  = require(".config")
local startup = require(".startup")
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.lua</code>

{% highlight lua %}
-- background before wallpaper
os.execute("xsetroot -solid '" .. gmc.color["blue500"] .. "'")

-- Read the manual in $ man herbstluftwm
helper.hc('emit_hook reload')

-- gap counter
os.execute("echo 35 > /tmp/herbstluftwm-gap")

-- do not repaint until unlock
helper.hc("lock")

-- standard
helper.hc('keyunbind --all')
helper.hc('mouseunbind --all')
helper.hc('unrule -F')

helper.set_tags_with_name()

-- do hash config
helper.do_config('keybind',   config.keybinds)
helper.do_config('keybind',   config.tagskeybinds)
helper.do_config('mousebind', config.mousebinds)
helper.do_config('attr',      config.attributes)
helper.do_config('set',       config.sets)
helper.do_config('rule',      config.rules)

-- unlock, just to be sure
helper.hc("unlock")

-- launch statusbar panel (e.g. dzen2 or lemonbar)
helper.do_panel()

-- load on startup
startup.run()
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../lua/autostart.lua][dotfiles-lua-autostart]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html

[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]:  {{ asset_path }}/hlwm-03-debug-config.png

[image-lua-01-tree]:  {{ asset_path }}/hlwm-lua-01-tree.png

[dotfiles-lua-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/lua
[dotfiles-lua-autostart]: {{ dotfiles_path }}/lua/autostart.lua
[dotfiles-lua-gmc]:       {{ dotfiles_path }}/lua/assets/gmc.lua
[dotfiles-lua-config]:    {{ dotfiles_path }}/lua/config.lua
[dotfiles-lua-helper]:    {{ dotfiles_path }}/lua/helper.lua
[dotfiles-lua-startup]:   {{ dotfiles_path }}/lua/startup.lua
