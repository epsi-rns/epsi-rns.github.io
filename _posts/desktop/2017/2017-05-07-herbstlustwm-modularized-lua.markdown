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

> Goal: Separate Main Flow, Code, and Data.

So anyone can focus to alter special customization in Main Script,
without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[Modularized HerbstluftWM Overview][local-overview]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[github.com/.../dotfiles/.../lua/][dotfiles-lua-directory]

-- -- --

### Modularized HerbstluftWM in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Modularized Overview ]][local-overview]
[[ BASH ]][local-BASH]
[[ Perl ]][local-Perl]
[[ Python ]][local-python]
[[ Ruby ]][local-Ruby]
[[ PHP ]][local-PHP]
[[ Lua ]][local-Lua]
[[ Haskell ]][local-Haskell]

Source Code Directory:
[[ BASH ]][dotfiles-BASH]
[[ Perl ]][dotfiles-Perl]
[[ Python ]][dotfiles-python]
[[ Ruby ]][dotfiles-Ruby]
[[ PHP ]][dotfiles-PHP]
[[ Lua ]][dotfiles-Lua]
[[ Haskell ]][dotfiles-Haskell]

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

<code class="code-file">gmc.lua</code>

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

*	[github.com/.../dotfiles/.../lua/gmc.lua][dotfiles-lua-gmc]

Similar Code: 
[[ BASH Color ][dotfiles-bash-gmc]]
[[ Perl Color ][dotfiles-perl-gmc]]
[[ Python Color ][dotfiles-python-gmc]]
[[ Ruby Color ][dotfiles-ruby-gmc]]
[[ PHP Color ][dotfiles-php-gmc]]
[[ Lua Color ][dotfiles-lua-gmc]]
[[ Haskell Color ][dotfiles-haskell-gmc]]

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
as shown in the following code.

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

Similar Code: 
[[ BASH Config ][dotfiles-bash-config]]
[[ Perl Config ][dotfiles-perl-config]]
[[ Python Config ][dotfiles-python-config]]
[[ Ruby Config ][dotfiles-ruby-config]]
[[ PHP Config ][dotfiles-php-config]]
[[ Lua Config ][dotfiles-lua-config]]
[[ Haskell Config ][dotfiles-haskell-config]]

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

#### Debug Herbstclient Command

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight lua %}
        print(command .. ' ' .. key .. ' ' .. value)
{% endhighlight %}

You can see the debugging result in figure below.

[![HerbstluftWM: Debug Command][image-hlwm-03-debug-config]{: .img-responsive }][photo-hlwm-03-debug-config]

#### View Source File:

*	[github.com/.../dotfiles/.../lua/helper.lua][dotfiles-lua-helper]

Similar Code: 
[[ BASH Helper ][dotfiles-bash-helper]]
[[ Perl Helper ][dotfiles-perl-helper]]
[[ Python Helper ][dotfiles-python-helper]]
[[ Ruby Helper ][dotfiles-ruby-helper]]
[[ PHP Helper ][dotfiles-php-helper]]
[[ Lua Helper ][dotfiles-lua-helper]]
[[ Haskell Helper ][dotfiles-haskell-helper]]

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
    local panel   = dirname .. "panel-lemonbar.lua"
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
to determine whether it should be launch or not.

#### View Source File:

*	[github.com/.../dotfiles/.../lua/startup.lua][dotfiles-lua-startup]

Similar Code: 
[[ BASH Startup ][dotfiles-bash-startup]]
[[ Perl Startup ][dotfiles-perl-startup]]
[[ Python Startup ][dotfiles-python-startup]]
[[ Ruby Startup ][dotfiles-ruby-startup]]
[[ PHP Startup ][dotfiles-php-startup]]
[[ Lua Startup ][dotfiles-lua-startup]]
[[ Haskell Startup ][dotfiles-haskell-startup]]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.lua</code>

{% highlight lua %}
local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path = package.path .. ';' .. dirname .. '?.lua;'

local gmc     = require ".gmc"
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

Similar Code: 
[[ BASH autostart ][dotfiles-bash-autostart]]
[[ Perl autostart ][dotfiles-perl-autostart]]
[[ Python autostart ][dotfiles-python-autostart]]
[[ Ruby autostart ][dotfiles-ruby-autostart]]
[[ PHP autostart ][dotfiles-php-autostart]]
[[ Lua autostart ][dotfiles-lua-autostart]]
[[ Haskell autostart ][dotfiles-haskell-autostart]]

-- -- --

### Coming up Next

After the Window Manager, comes the Panel.

*	[HerbstluftWM Tag Status in Lua][local-lua-tag-status]

*	[HerbstluftWM Event Idle in Lua][local-lua-event-idle]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-lua-tag-status]:   {{ site.url }}/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
[local-lua-event-idle]:   {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html

[image-ss-hlwm-nopanel]: {{ asset_path }}/herbstluftwm-nopanel.png
[photo-ss-hlwm-nopanel]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM5QN3sl9KsZs3xlb87UivcHZeLGbSuk2Z8Jx0W?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-hlwm-02-tag-status]:   {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]: {{ asset_path }}/hlwm-03-debug-config-half.png
[photo-hlwm-03-debug-config]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMG-pilS2yhwarThAT23bBYV54z3rYcs2wnaL0E?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-lua-01-tree]:          {{ asset_path }}/hlwm-lua-01-tree.png

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html
[local-python]:   {{ site.url }}/desktop/2017/05/04/herbstlustwm-modularized-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/05/05/herbstlustwm-modularized-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/05/06/herbstlustwm-modularized-php.html
[local-lua]:      {{ site.url }}/desktop/2017/05/07/herbstlustwm-modularized-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/05/08/herbstlustwm-modularized-haskell.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell

[dotfiles-bash-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/bash
[dotfiles-bash-autostart]: {{ dotfiles_path }}/bash/autostart.sh
[dotfiles-bash-gmc]:       {{ dotfiles_path }}/bash/gmc.sh
[dotfiles-bash-config]:    {{ dotfiles_path }}/bash/config.sh
[dotfiles-bash-helper]:    {{ dotfiles_path }}/bash/helper.sh
[dotfiles-bash-startup]:   {{ dotfiles_path }}/bash/startup.sh

[dotfiles-perl-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/perl
[dotfiles-perl-autostart]: {{ dotfiles_path }}/perl/autostart.pl
[dotfiles-perl-gmc]:       {{ dotfiles_path }}/perl/gmc.pm
[dotfiles-perl-config]:    {{ dotfiles_path }}/perl/config.pm
[dotfiles-perl-helper]:    {{ dotfiles_path }}/perl/helper.pm
[dotfiles-perl-startup]:   {{ dotfiles_path }}/perl/startup.pm

[dotfiles-python-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/python
[dotfiles-python-autostart]: {{ dotfiles_path }}/python/autostart.py
[dotfiles-python-gmc]:       {{ dotfiles_path }}/python/gmc.py
[dotfiles-python-config]:    {{ dotfiles_path }}/python/config.py
[dotfiles-python-helper]:    {{ dotfiles_path }}/python/helper.py
[dotfiles-python-startup]:   {{ dotfiles_path }}/python/startup.py

[dotfiles-ruby-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/ruby
[dotfiles-ruby-autostart]: {{ dotfiles_path }}/ruby/autostart.rb
[dotfiles-ruby-gmc]:       {{ dotfiles_path }}/ruby/gmc.rb
[dotfiles-ruby-config]:    {{ dotfiles_path }}/ruby/config.rb
[dotfiles-ruby-helper]:    {{ dotfiles_path }}/ruby/helper.rb
[dotfiles-ruby-startup]:   {{ dotfiles_path }}/ruby/startup.rb

[dotfiles-php-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/php
[dotfiles-php-autostart]: {{ dotfiles_path }}/php/autostart.php
[dotfiles-php-gmc]:       {{ dotfiles_path }}/php/gmc.php
[dotfiles-php-config]:    {{ dotfiles_path }}/php/config.php
[dotfiles-php-helper]:    {{ dotfiles_path }}/php/helper.php
[dotfiles-php-startup]:   {{ dotfiles_path }}/php/startup.php

[dotfiles-lua-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/lua
[dotfiles-lua-autostart]: {{ dotfiles_path }}/lua/autostart.lua
[dotfiles-lua-gmc]:       {{ dotfiles_path }}/lua/gmc.lua
[dotfiles-lua-config]:    {{ dotfiles_path }}/lua/config.lua
[dotfiles-lua-helper]:    {{ dotfiles_path }}/lua/helper.lua
[dotfiles-lua-startup]:   {{ dotfiles_path }}/lua/startup.lua

[dotfiles-haskell-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/haskell
[dotfiles-haskell-autostart]: {{ dotfiles_path }}/haskell/autostart.hs
[dotfiles-haskell-gmc]:       {{ dotfiles_path }}/haskell/MyGMC.hs
[dotfiles-haskell-config]:    {{ dotfiles_path }}/haskell/MyConfig.hs
[dotfiles-haskell-helper]:    {{ dotfiles_path }}/haskell/MyHelper.hs
[dotfiles-haskell-startup]:   {{ dotfiles_path }}/haskell/MyStartup.hs

