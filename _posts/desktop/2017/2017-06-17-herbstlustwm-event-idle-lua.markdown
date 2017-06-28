---
layout: post-sidemenu-wm
title:  "HerbstluftWM Idle Event in Lua"
date:   2017-06-17 17:35:15 +0700
categories: desktop
tags: [coding, lua, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Event Idle using Dzen2 or Lemonbar.
  Modularized implementation in Lua script.
  An advance case of Pipe and Fork.

---

### Preface

> Goal: Manage Herbstclient process trigerred by idle event

	Focusing in "herbstclient --idle". 

![HerbstluftWM: Tag Status][image-hlwm-01-event-idle]{: .img-responsive }

This is the next Part, of the previous Tutorial.
This tutorial cover Lemonbar.
In order to use Dzen2, any reader could use the source code in github.

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
	[github.com/.../dotfiles/.../lua/][dotfiles-dzen2-lua]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/][dotfiles-lemon-lua]

#### The PipeHandler Source File:

Let's have a look at <code class="code-file">pipehandler.lua</code> in github.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../lua/pipehandler.lua][dotfiles-dzen2-lua-pipehandler]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/pipehandler.lua][dotfiles-lemon-lua-pipehandler]

-- -- --

### HerbstluftWM Idle Event in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Idle Event Overview ][local-overview]]
[[ BASH ][local-bash]]
[[ Perl ][local-perl]]
[[ Python ][local-python]]
[[ Ruby ][local-ruby]]
[[ PHP ][local-php]]
[[ Lua ][local-lua]]
[[ Haskell ][local-haskell]]

Dzen2 Source Code Directory:
[[ BASH ][dotfiles-dzen2-bash]]
[[ Perl ][dotfiles-dzen2-perl]]
[[ Python ][dotfiles-dzen2-python]]
[[ Ruby ][dotfiles-dzen2-ruby]]
[[ PHP ][dotfiles-dzen2-php]]
[[ Lua ][dotfiles-dzen2-lua]]
[[ Haskell ][dotfiles-dzen2-haskell]]

Lemonbar Source Code Directory:
[[ BASH ][dotfiles-lemon-bash]]
[[ Perl ][dotfiles-lemon-perl]]
[[ Python ][dotfiles-lemon-python]]
[[ Ruby ][dotfiles-lemon-ruby]]
[[ PHP ][dotfiles-lemon-php]]
[[ Lua ][dotfiles-lemon-lua]]
[[ Haskell ][dotfiles-lemon-haskell]]

-- -- --

### Statusbar Screenshot

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Without Idle event

Let's have a look at our main 
<code class="code-file">panel.lua</code> in github.
At the end of the script, we finally call lemonbar
with <code>detach_lemon</code> function.

{% highlight lua %}
-- remove all lemonbar instance
os.execute('pkill lemonbar')

-- run process in the background
pipehandler.detach_lemon(monitor, lemon_parameters)
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../lua/panel.lua][dotfiles-dzen2-lua-panel]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/panel.lua][dotfiles-lemon-lua-panel]


#### Run Lemon, Run !

This <code>detach_lemon</code> function.
is just a function that enable 
the lemonbar running process to be detached,
using <code>posix.fork()</code>.

{% highlight lua %}
function _M.detach_lemon(monitor, parameters)
    local pid_lemon = posix.fork()

    if pid_lemon == 0 then -- this is the child process
        _M.run_lemon(monitor, parameters)
    else                   -- this is the parent process
        -- nothing
    end
end
{% endhighlight %}

The real function is <code>run_lemon</code>.
You must be familiar with this <code>io.popen</code>.

{% highlight lua %}
function _M.run_lemon(monitor, parameters) 
    local command_out  = 'lemonbar ' .. parameters .. ' -p'
    local pipe_lemon_out = assert(io.popen(command_out, 'w'))
    
    _M.content_init(monitor, pipe_lemon_out)       
    pipe_lemon_out:close()
end
{% endhighlight %}

Note: that we want to ignore idle event for a while.
And append the <code>-p</code> for a while,
to make the statusbar persistent.

#### Statusbar Initialization

Here we have the <code>content_init</code>.
It is just an initialization of global variable.
We are going to have some loop later in different function,
to do the real works.

{% highlight lua %}
function _M.content_init(monitor, pipe_lemon_out)
    output.set_tag_value(monitor)
    output.set_windowtitle('')

    local text = output.get_statusbar_text(monitor)
    pipe_lemon_out:write(text .. "\n")
    pipe_lemon_out:flush()
end
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple version. No idle event. Only statusbar initialization.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/pipehandler.01-init.lua][dotfiles-lemon-lua-pipehandler-init]

-- -- --

### With Idle event

Consider this <code>content_walk</code> call,
after <code>content_init</code> call,
inside the <code>run_lemon</code>.

{% highlight lua %}
function _M.run_lemon(monitor, parameters) 
    local command_out  = 'lemonbar ' .. parameters
    local pipe_lemon_out = assert(io.popen(command_out, 'w'))
    
    _M.content_init(monitor, pipe_lemon_out)
    _M.content_walk(monitor, pipe_lemon_out) -- loop for each event
        
    pipe_lemon_out:close()
end
{% endhighlight %}

#### Wrapping Idle Event into Code

<code>content_walk</code> is the **heart** of this script.
We have to capture every event,
and process the event in event handler.

	Walk step by step, Process event by event

After the event handler,
we will get the statusbar text, in the same way,
we did in <code>content_init</code>.

{% highlight lua %}
function _M.content_walk(monitor, pipe_lemon_out)    
    -- start a pipe
    command_in = 'herbstclient --idle'
    local pipe_in  = assert(io.popen(command_in,  'r'))
    local text = ''
  
    -- wait for each event, trim newline
    for event in pipe_in:lines() do
        _M.handle_command_event(monitor, common.trim1(event))
    
        text = output.get_statusbar_text(monitor)
        pipe_lemon_out:write(text .. "\n")
        pipe_lemon_out:flush()
    end -- for loop
   
    pipein:close()
end
{% endhighlight %}

-- -- --

### The Event Handler

For each idle event, there are multicolumn string.
The first string define the event origin.

![HerbstluftWM: Tag Status][image-hlwm-04-event-origin]{: .img-responsive }

The origin is either <code>reload</code>, or <code>quit_panel</code>,
<code>tag_changed</code>, or <code>tag_flags</code>, 
or <code>tag_added</code>, or <code>tag_removed</code>,
or <code>focus_changed</code>, or <code>window_title_changed</code>.
More complete event, can be read in herbstclient manual.

All we need is to pay attention to this two function.
<code>set_tag_value</code> and <code>set_windowtitle</code>.

{% highlight lua %}
function _M.handle_command_event(monitor, event)
    -- find out event origin
    local column = common.split(event, "\t")
    local origin = column[1] -- non zero based

    local tag_cmds = {'tag_changed', 
         'tag_flags', 'tag_added', 'tag_removed'}
    local title_cmds = {'window_title_changed', 'focus_changed'}

    if origin == 'reload' then
        os.execute('pkill lemonbar')
    elseif origin == 'quit_panel' then
        os.exit()
    elseif common.has_value(tag_cmds, origin) then
        output.set_tag_value(monitor)
    elseif common.has_value(title_cmds, origin) then
        local title = (#column > 2) and (column[3]) or ''
        output.set_windowtitle(title)
    end
end
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

Be aware, that Lua is using non zero based array.
{% highlight lua %}
    origin = column[1]
{% endhighlight %}

#### View Source File:

With idle event. The **heart** of the script.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/pipehandler.02-idle.lua][dotfiles-lemon-lua-pipehandler-idle]

-- -- --

### Lemonbar Clickable Areas

This is specific issue for lemonbar,
that we don't have in dzen2.

Consider have a look at 
<code class="code-file">output.lua</code>.

{% highlight lua %}
    -- clickable tags
    local text_name = '%{A:herbstclient focus_monitor '
                   .. '"' .. monitor .. '" && '
                   .. 'herbstclient use "' .. tag_index .. '":}'
                   .. ' ' .. tag_name ..' %{A} 
{% endhighlight %}

**Issue**: Lemonbar put the output on terminal
instead of executing the command.

![HerbstluftWM: Tag Status][image-hlwm-05-clickable]{: .img-responsive }

Consider going back to
<code class="code-file">pipehandler.lua</code>.

We need to pipe the lemonbar output to shell.
It means Lemonbar read input and write output at the same time.
But the real issue is Lua does not support bidirectional pipe.

{% highlight lua %}
function _M.run_lemon(monitor, parameters) 
    -- no bidirectional in Lua, using shell pipe instead
    local command_out  = 'lemonbar ' .. parameters .. ' | sh'
    local pipe_lemon_out = assert(io.popen(command_out, 'w'))
    
    _M.content_init(monitor, pipe_lemon_out)
    _M.content_walk(monitor, pipe_lemon_out) -- loop for each event
        
    pipe_lemon_out:close()
end
{% endhighlight %}

There is complex solution using <code>posix.pipe</code>.
We are going to use <code>posix.pipe</code> for a simple case later,
the combined event case.

#### How does it work ?

	Using shell pipe.

{% highlight lua %}
    local command_out  = 'lemonbar ' .. parameters .. ' | sh'
{% endhighlight %}

I know that piping using <code> | sh</code> looks primitive.
But it does works. So why bother ?

#### View Source File:

Piping lemonbar output to shell, implementing lemonbar clickable area.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/pipehandler.03-clickable.lua][dotfiles-lemon-lua-pipehandler-clickable]

### Interval Based Event

We can put custom event other than idle event in statusbar panel.
This event, such as date event, called based on time interval in second.

It is a little bit tricky, because we have to make, 
a combined event that consist of,
idle event (asynchronous) and interval event (synchronous).
Merging two different paralel process into one.

This is an overview of what we want to achieve.

![HerbstluftWM: Custom Event][image-hlwm-06-event-custom]{: .img-responsive }

In real code later, we do not need the timestamp.
<code>interval</code> string is enough to trigger interval event.

#### View Testbed Source File:

Before merging combined event into main code,
consider this test in an isolated fashion.

*	[github.com/.../dotfiles/.../lua/11-testevents.lua][dotfiles-lemon-lua-testevents]

-- -- --

### Combined Event

#### Preparing The View

This is what it looks like, an overview of what we want to achieve.

![Statusbar: Event Screenshot][image-hlwm-ss-event]{: .img-responsive }

Consider make a progress in 
<code class="code-file">output.py</code>.

{% highlight lua %}
_M.segment_datetime    = '' -- empty string

function _M.get_statusbar_text(monitor)
    ...

    -- draw date and time
    text = text .. '%{c}'
    text = text .. _M.output_by_datetime()

    ...
end

function _M.output_by_datetime()
    return _M.segment_datetime
end

function _M.set_datetime()
    ...

    _M.segment_datetime = date_text .. '  ' .. time_text
end
{% endhighlight %}

And a few enhancement in 
<code class="code-file">pipehandler.lua</code>.

{% highlight lua %}
function _M.handle_command_event(monitor, event)
    ...

    if origin == 'reload' then
    ...
    elseif origin == 'interval' then
        output.set_datetime()
    end
end

function _M.content_init(monitor, pipe_lemon_out)
    ...
    output.set_windowtitle('')
    output.set_datetime()

    ...
end
{% endhighlight %}

#### Expanding The Event Controller

All we need to do is to split out <code>content_walk</code> into

*	<code>content_walk</code>: combined event,
	with the help of <code>posix.pipe()</code>.

*	<code>content_event_idle</code>: HerbstluftWM idle event. 
	Forked, as background processing.

*	<code>content_event_interval</code> : Custom date time event. 
	Forked, as background processing.

{% highlight lua %}
function _M.content_event_idle(pipe_cat_out)
    local pid_idle = posix.fork()

    if pid_idle == 0 then -- this is the child process
        -- start a pipe
        command_in = 'herbstclient --idle'
        local pipe_in  = assert(io.popen(command_in,  'r'))
  
        -- wait for each event 
        for event in pipe_in:lines() do
            posix.write(pipe_cat_out, event)
            io.flush()
        end -- for loop
   
        pipein:close()
    else             -- this is the parent process
        -- nothing
    end
end
{% endhighlight %}

{% highlight lua %}
function _M.content_event_interval(pipe_cat_out) 
    local pid_interval = posix.fork()

    if pid_interval == 0 then -- this is the child process
        while true do
            posix.write(pipe_cat_out, "interval\n")
            io.flush() 

            _M.os_sleep(1)
        end
    else             -- this is the parent process
        -- nothing
    end
end
{% endhighlight %}

{% highlight lua %}
function _M.content_walk(monitor, pipe_lemon_out)  
    rd, wr = posix.pipe()

    _M.content_event_idle(wr)
    _M.content_event_interval(wr)

    local bufsize = 4096
    local event = ''

    while true do
        -- wait for next event, trim newline
        event = common.trim1(posix.read(rd, bufsize))
        if event == nil or #event == 0 then break end
    
        _M.handle_command_event(monitor, event)    
    
        text = output.get_statusbar_text(monitor)
        pipe_lemon_out:write(text .. "\n")
        pipe_lemon_out:flush()
    end -- not using for loop

    posix.close(rd)
    posix.close(wr)
end
{% endhighlight %}

This above is the most complex part.
We are almost done.

#### View Source File:

Combined event consist of both,
synchronous interval event and asynchronous idle event.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/pipehandler.04-event.lua][dotfiles-lemon-lua-pipehandler-event]

-- -- --

### Dual Bar

The idea of this article comes from the fact
that <code>herbsclient --idle</code> is asynchronous event.
If you need another bar, just simply use <code>Conky</code> instead.

*	**Dzen2**: 
	![HerbstluftWM: Dzen2 Conky][image-hlwm-ss-dzen2-conky]{: .img-responsive }

*	**Lemonbar**: 
	![HerbstluftWM: Lemonbar Conky][image-hlwm-ss-lemon-conky]{: .img-responsive }

We only need one function to do this in
<code class="code-file">pipehandler.pm</code>.

{% highlight perl %}
function _M.detach_lemon_conky(parameters)
    local pid_conky = posix.fork()

    if pid_conky == 0 then -- this is the child process
        local cmd_out  = 'lemonbar ' .. parameters
        local pipe_out = assert(io.popen(cmd_out, 'w'))

        local dirname  = debug.getinfo(1).source:match("@?(.*/)")
        local path     = dirname .. "../conky"
        local cmd_in   = 'conky -c ' .. path .. '/conky.lua'
        local pipe_in  = assert(io.popen(cmd_in,  'r'))

        for line in pipe_in:lines() do
            pipe_out:write(line.."\n")
            pipe_out:flush()
        end -- for loop
   
        pipe_in:close()    
        pipe_out:close()
    else                   -- this is the parent process
        -- nothing
    end
end
{% endhighlight %}

And execute the function main script in
<code class="code-file">panel.pl</code>.

{% highlight perl %}
#!/usr/bin/lua

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path   = package.path .. ';' .. dirname .. '?.lua;'
  
local helper      = require('.helper')
local pipehandler = require('.pipehandler')

-- main

local panel_height = 24
local monitor = helper.get_monitor(arg)

os.execute('pkill lemonbar')
os.execute('herbstclient pad ' .. monitor .. ' ' 
    .. panel_height .. ' 0 ' .. panel_height .. ' 0')

-- run process in the background

local params_top = helper.get_params_top(monitor, panel_height)
pipehandler.detach_lemon(monitor, params_top)

local params_bottom = helper.get_params_bottom(monitor, panel_height)
pipehandler.detach_lemon_conky(params_bottom)
{% endhighlight %}

#### View Source File:

Dual Bar, <code>detach_lemon_conky</code> function.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../lua/pipehandler.05-conky.lua][dotfiles-lemon-lua-pipehandler-conky]

-- -- --

### Avoid Zombie Apocalypse

Zombie are scary, and fork does have a tendecy to become a zombie.
Application that utilize several forks should be aware of this threat.
The reason why I use fork instead of thread is,
because the original herbstluftwm configuration coming from bash,
and this bash script is using fork.

However, you can use this short script to reduce zombie population.
It won't kill all zombie, but works for most case.
You might still need <code>htop</code>,
and <code>kill -9</code> manually.

{% highlight lua %}
function _M.kill_zombie()
    os.execute('pkill dzen2')
    os.execute('pkill lemonbar')
    os.execute('pkill cat')
    os.execute('pkill conky')
    os.execute('pkill herbstclient')
end
{% endhighlight %}

-- -- --

### Putting Them All Together

**TBD**

{% highlight lua %}
{% endhighlight %}

-- -- --

#### Desktop Screenshot

Fullscreen, Dual Panel, Zero Gap.

[![HerbstluftWM: Screenshot Dual Panel][image-ss-hlwm-dualpanel]{: .img-responsive }][photo-ss-hlwm-dualpanel]

-- -- --

Enjoy the statusbar !
Enjoy the window manager !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-01-event-idle]:   {{ asset_path }}/herbstclient-01-event-idle.png
[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-04-event-origin]: {{ asset_path }}/herbstclient-04-event-origin.png
[image-hlwm-05-clickable]:    {{ asset_path }}/herbstclient-05-lemonbar-clickable-areas.png
[image-hlwm-06-event-custom]: {{ asset_path }}/herbstclient-06-event-custom.png

[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png
[image-hlwm-ss-event]: {{ asset_path }}/hlwm-event-ss.png
[image-hlwm-ss-dzen2-conky]: {{ asset_path }}/hlwm-dzen2-conky-ss.png
[image-hlwm-ss-lemon-conky]: {{ asset_path }}/hlwm-lemon-conky-ss.png

[image-ss-hlwm-dualpanel]: {{ asset_path }}/herbstluftwm-dualpanel.png
[photo-ss-hlwm-dualpanel]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPqMNt9e3_UypKHqASPs_njHBQPX7Kn8X_O9aTp?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[dotfiles-lemon-lua-testevents]:  {{ dotfiles_lemon }}/lua/11-testevents.lua

[local-lua-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-lua.html
[local-lua-pipe]:   {{ site.url }}/code/2017/04/16/lua-pipe-and-fork.html

[local-bash-config]: {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-bash-pipe]:   {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/12/herbstlustwm-event-idle-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/13/herbstlustwm-event-idle-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/16/herbstlustwm-event-idle-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html

[dotfiles-lemon-lua-pipehandler-init]:      {{ dotfiles_lemon }}/lua/pipehandler.01-init.lua
[dotfiles-lemon-lua-pipehandler-idle]:      {{ dotfiles_lemon }}/lua/pipehandler.02-idle.lua
[dotfiles-lemon-lua-pipehandler-clickable]: {{ dotfiles_lemon }}/lua/pipehandler.03-clickable.lua
[dotfiles-lemon-lua-pipehandler-event]:     {{ dotfiles_lemon }}/lua/pipehandler.04-event.lua
[dotfiles-lemon-lua-pipehandler-conky]:     {{ dotfiles_lemon }}/lua/pipehandler.05-conky.lua

[dotfiles-dzen2-bash]:    {{ dotfiles_dzen2 }}/bash
[dotfiles-dzen2-perl]:    {{ dotfiles_dzen2 }}/perl
[dotfiles-dzen2-python]:  {{ dotfiles_dzen2 }}/python
[dotfiles-dzen2-ruby]:    {{ dotfiles_dzen2 }}/ruby
[dotfiles-dzen2-php]:     {{ dotfiles_dzen2 }}/php
[dotfiles-dzen2-lua]:     {{ dotfiles_dzen2 }}/lua
[dotfiles-dzen2-haskell]: {{ dotfiles_dzen2 }}/haskell

[dotfiles-lemon-bash]:    {{ dotfiles_lemon }}/bash
[dotfiles-lemon-perl]:    {{ dotfiles_lemon }}/perl
[dotfiles-lemon-python]:  {{ dotfiles_lemon }}/python
[dotfiles-lemon-ruby]:    {{ dotfiles_lemon }}/ruby
[dotfiles-lemon-php]:     {{ dotfiles_lemon }}/php
[dotfiles-lemon-lua]:     {{ dotfiles_lemon }}/lua
[dotfiles-lemon-haskell]: {{ dotfiles_lemon }}/haskell

[dotfiles-dzen2-bash-panel]:       {{ dotfiles_dzen2 }}/bash/panel.sh
[dotfiles-dzen2-bash-helper]:      {{ dotfiles_dzen2 }}/bash/helper.sh
[dotfiles-dzen2-bash-output]:      {{ dotfiles_dzen2 }}/bash/output.sh
[dotfiles-dzen2-bash-pipehandler]: {{ dotfiles_dzen2 }}/bash/pipehandler.sh

[dotfiles-dzen2-perl-panel]:       {{ dotfiles_dzen2 }}/perl/panel.pl
[dotfiles-dzen2-perl-helper]:      {{ dotfiles_dzen2 }}/perl/helper.pm
[dotfiles-dzen2-perl-output]:      {{ dotfiles_dzen2 }}/perl/output.pm
[dotfiles-dzen2-perl-pipehandler]: {{ dotfiles_dzen2 }}/perl/pipehandler.pm

[dotfiles-dzen2-python-panel]:       {{ dotfiles_dzen2 }}/python/panel.py
[dotfiles-dzen2-python-helper]:      {{ dotfiles_dzen2 }}/python/helper.py
[dotfiles-dzen2-python-output]:      {{ dotfiles_dzen2 }}/python/output.py
[dotfiles-dzen2-python-pipehandler]: {{ dotfiles_dzen2 }}/python/pipehandler.py

[dotfiles-dzen2-ruby-panel]:       {{ dotfiles_dzen2 }}/ruby/panel.rb
[dotfiles-dzen2-ruby-helper]:      {{ dotfiles_dzen2 }}/ruby/helper.rb
[dotfiles-dzen2-ruby-output]:      {{ dotfiles_dzen2 }}/ruby/output.rb
[dotfiles-dzen2-ruby-pipehandler]: {{ dotfiles_dzen2 }}/ruby/pipehandler.rb

[dotfiles-dzen2-php-panel]:       {{ dotfiles_dzen2 }}/php/panel.php
[dotfiles-dzen2-php-helper]:      {{ dotfiles_dzen2 }}/php/helper.php
[dotfiles-dzen2-php-output]:      {{ dotfiles_dzen2 }}/php/output.php
[dotfiles-dzen2-php-pipehandler]: {{ dotfiles_dzen2 }}/php/pipehandler.php

[dotfiles-dzen2-lua-panel]:       {{ dotfiles_dzen2 }}/lua/panel.lua
[dotfiles-dzen2-lua-common]:      {{ dotfiles_dzen2 }}/lua/common.lua
[dotfiles-dzen2-lua-helper]:      {{ dotfiles_dzen2 }}/lua/helper.lua
[dotfiles-dzen2-lua-output]:      {{ dotfiles_dzen2 }}/lua/output.lua
[dotfiles-dzen2-lua-pipehandler]: {{ dotfiles_dzen2 }}/lua/pipehandler.lua

[dotfiles-dzen2-haskell-panel]:       {{ dotfiles_dzen2 }}/haskell/panel.hs
[dotfiles-dzen2-haskell-helper]:      {{ dotfiles_dzen2 }}/haskell/MyHelper.hs
[dotfiles-dzen2-haskell-output]:      {{ dotfiles_dzen2 }}/haskell/MyOutput.hs
[dotfiles-dzen2-haskell-pipehandler]: {{ dotfiles_dzen2 }}/haskell/MyPipeHandler.hs

[dotfiles-lemon-bash-panel]:       {{ dotfiles_lemon }}/bash/panel.sh
[dotfiles-lemon-bash-helper]:      {{ dotfiles_lemon }}/bash/helper.sh
[dotfiles-lemon-bash-output]:      {{ dotfiles_lemon }}/bash/output.sh
[dotfiles-lemon-bash-pipehandler]: {{ dotfiles_lemon }}/bash/pipehandler.sh

[dotfiles-lemon-perl-panel]:       {{ dotfiles_lemon }}/perl/panel.pl
[dotfiles-lemon-perl-helper]:      {{ dotfiles_lemon }}/perl/helper.pm
[dotfiles-lemon-perl-output]:      {{ dotfiles_lemon }}/perl/output.pm
[dotfiles-lemon-perl-pipehandler]: {{ dotfiles_lemon }}/perl/pipehandler.pm

[dotfiles-lemon-python-panel]:       {{ dotfiles_lemon }}/python/panel.py
[dotfiles-lemon-python-helper]:      {{ dotfiles_lemon }}/python/helper.py
[dotfiles-lemon-python-output]:      {{ dotfiles_lemon }}/python/output.py
[dotfiles-lemon-python-pipehandler]: {{ dotfiles_lemon }}/python/pipehandler.py

[dotfiles-lemon-ruby-panel]:       {{ dotfiles_lemon }}/ruby/panel.rb
[dotfiles-lemon-ruby-helper]:      {{ dotfiles_lemon }}/ruby/helper.rb
[dotfiles-lemon-ruby-output]:      {{ dotfiles_lemon }}/ruby/output.rb
[dotfiles-lemon-ruby-pipehandler]: {{ dotfiles_lemon }}/ruby/pipehandler.rb

[dotfiles-lemon-php-panel]:       {{ dotfiles_lemon }}/php/panel.php
[dotfiles-lemon-php-helper]:      {{ dotfiles_lemon }}/php/helper.php
[dotfiles-lemon-php-output]:      {{ dotfiles_lemon }}/php/output.php
[dotfiles-lemon-php-pipehandler]: {{ dotfiles_lemon }}/php/pipehandler.php

[dotfiles-lemon-lua-panel]:       {{ dotfiles_lemon }}/lua/panel.lua
[dotfiles-lemon-lua-common]:      {{ dotfiles_lemon }}/lua/common.lua
[dotfiles-lemon-lua-helper]:      {{ dotfiles_lemon }}/lua/helper.lua
[dotfiles-lemon-lua-output]:      {{ dotfiles_lemon }}/lua/output.lua
[dotfiles-lemon-lua-pipehandler]: {{ dotfiles_lemon }}/lua/pipehandler.lua

[dotfiles-lemon-haskell-panel]:       {{ dotfiles_lemon }}/haskell/panel.hs
[dotfiles-lemon-haskell-helper]:      {{ dotfiles_lemon }}/haskell/MyHelper.hs
[dotfiles-lemon-haskell-output]:      {{ dotfiles_lemon }}/haskell/MyOutput.hs
[dotfiles-lemon-haskell-pipehandler]: {{ dotfiles_lemon }}/haskell/MyPipeHandler.hs
