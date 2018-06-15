---
layout: post
title:  "Piping and Forking in Lua"
categories: code
date:   2017-04-20 17:35:15 +0700
tags: [coding, conky, lua]
author: epsi

excerpt:
  How to be a Lua Plumber.
  
related_link_ids: 
  - 17042335  # Pipe and Fork Overview
  - 17041535  # Pipe and Fork BASH
  - 17041635  # Pipe and Fork Perl
  - 17041735  # Pipe and Fork Python
  - 17041835  # Pipe and Fork Ruby
  - 17041935  # Pipe and Fork PHP
  - 17042035  # Pipe and Fork Lua
  - 17042135  # Pipe and Fork Haskell

---

### Lua Plumber.

Lua is a very Popular embedded script.
The only contender I know is Squirrel.

Lua script version for this Pipe and Fork tutorial 
is very similar with PHP script version.
Almost only syntax differences they have.
It would not be hard to understand between the two.

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky.

Before you dip your toe to scripting,
you might desire to know the reason by reading this overview.

**Reading**

*	[Piping and Forking in Linux Script][local-overview]

-- -- --

{% include post/2017/04/pipe-and-fork-language.md %}

-- -- --

### Pipe and Fork in Desktop Ricing

	Why Lua ?

Lua only used as embedded script. But I can see Lua's potential.
I have never seen anybody utilize Lua as main scripting tools in dotfiles,
except Conky, and AwesomeWM that use Lua as main scripting configuration.
Outside the world of Ricing, Lua also extensively used in Gaming..
I myself utilize a lot of Conky Lua a lot. So why not make a start ?

-- -- --

### Be Creative

Lua lack is so lightweight that it lacks some common commands.
We have to emulate these below:

*	sleep

*	_ dirname _

*	exec

Everything is fine now.

-- -- --

### Start Simple

Welcome to n00berland. Begin with simple script.
We will use this loop as a source feed to pipe.
This step won't introduce Pipe nor Fork.

This script only show an infinite loop showing local time.
Each updated in one second interval.
We manage this interval by delaying,
using <code>sleep</code> code.internal function.

The first issue with Lua is,
it does not have native <code>sleep</code> function.
So we either call it from system or emulate it with.


**Source**:

*	[gitlab.com/.../dotfiles/.../lua-01-basic.lua][dotfiles-lua-01-basic]

{% highlight lua %}
#!/usr/bin/lua

local timeformat = '%a %b %d %H:%M:%S'

function sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

while true do
    print(os.date(timeformat))
    sleep(1)
end
{% endhighlight %}

Call to this simple code would produce time marching,
one after another, below the command line prompt.

![Pipe: Basic][image-time-basic]{: .img-responsive }

{% include post/2017/04/pipe-and-fork-similar-01.md %}

-- -- --

### External Command as Source Feed

Beside previous simple loop that is used as Internal Command,
this tutorial also provide Conky as External Command
in <code class="code-file">asset</code> directory.
I made it as simple as possible.

**Source**:

*	[gitlab.com/.../dotfiles/.../conky.lua][dotfiles-conky]

{% highlight lua %}
conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

conky.text = [[\
${time %a %b %d %H:%M:%S}\
]]
{% endhighlight %}

-- -- --

### A Unidirectional Pipe Between External Command

This step is overview of Pipe between two external command.
Instead of system command, this utilize <code>popen</code>
using two handles: <code>pipein</code> and <code>pipeout</code>.

This is a very simple.  Just a few self explanatory lines.
This very short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

The second issue with Lua is,
it does not have native <code>_ dirname _</code> directive.
that's why emulate it with <code>debug.getinfo</code>


**Source**:

*	[gitlab.com/.../dotfiles/.../lua-02-popen.lua][dotfiles-lua-02-popen]

{% highlight lua %}
#!/usr/bin/lua

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
local path     = dirname .. "../assets"
local cmdin    = 'conky -c ' .. path .. '/conky.lua'
local cmdout   = 'less' -- or 'dzen2'

local pipein  = assert(io.popen(cmdin,  'r'))
local pipeout = assert(io.popen(cmdout, 'w'))
  
for line in pipein:lines() do
    pipeout:write(line.."\n")
    pipeout:flush()
end -- for loop
   
pipein:close()
pipeout:close()
{% endhighlight %}

You can see, how simple it is.
This would have <code>less</code> output similar to this below.

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

{% include post/2017/04/pipe-and-fork-similar-02.md %}

-- -- --

### How does it works ?

Lua act as middle man, anything read from
<code>pipein:lines()</code> written by <code>pipeout:write()</code>

{% highlight lua %}
for line in pipein:lines() do
    pipeout:write(line.."\n")
    pipeout:flush()
end
{% endhighlight %}

-- -- --

### A Unidirectional Pipe from Internal Function

Still with <code>popen</code>.
Instead of using <code>pipein</code> ,
we use internal function as source feed.
And <code>pipeout</code> to external command.

	Do not forget to flush.


**Source**:

*	[gitlab.com/.../dotfiles/.../lua-03-popen.lua][dotfiles-lua-03-popen]

{% highlight lua %}
#!/usr/bin/lua

local timeformat = '%a %b %d %H:%M:%S'

function sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

local cmdout   = 'less' -- or 'dzen2'
local pipeout = assert(io.popen(cmdout, 'w'))

while true do
    local datestr = os.date(timeformat).."\n"
    pipeout:write(datestr)
    pipeout:flush()
    
    sleep(1)
end

pipeout:close()
{% endhighlight %}

{% include post/2017/04/pipe-and-fork-similar-03.md %}

-- -- --

### How does it works ?

The same as previous.
But instead of reading from <code>pipein</code>,
it is managed by internal process using <code>pipeout:write()</code>.

{% highlight lua %}
    local datestr = os.date(timeformat).."\n"
    pipeout:write(datestr)
{% endhighlight %}

-- -- --

### Fork Overview

Fork in Lua is also simple.
But it require external <code>luaposix</code> Library.
Arch user should get it easy in AUR (Arch user Repository).

This step use internal function as source feed,
as continuation of previous step.

This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.


**Source**:

*	[gitlab.com/.../dotfiles/.../lua-05-fork.lua][dotfiles-lua-05-fork]

{% highlight lua %}
#!/usr/bin/lua

-- luaposix available in AUR
local posix = require "posix"

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- helper

function sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- application related function

function get_dzen2_parameters ()
    local xpos    = '0'
    local ypos    = '0'
    local width   = '640'
    local height  = '24'
    local fgcolor = '#000000'
    local bgcolor = '#ffffff'
    local font    = '-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'

    local parameters = ""
        .. " -x "..xpos.." -y "..ypos
        .. " -w "..width.." -h "..height
        .. " -fn '"..font.."'"
        .. " -ta c -bg '"..bgcolor.."' -fg '"..fgcolor.."'"
        .. " -title-name dzentop"

    return parameters
end

function generated_output (process)
    local timeformat = '%a %b %d %H:%M:%S'

    while true do
        local datestr = os.date(timeformat).."\n"
        process:write(datestr)
        process:flush()

        sleep(1)
    end
end

function run_dzen2 ()
    local cmdout  = 'dzen2 ' .. get_dzen2_parameters()
    local pipeout = assert(io.popen(cmdout, 'w'))
    
    generated_output(pipeout)
    
    pipeout:close()
end


function detach_dzen2()
    local pid = posix.fork()

    if pid == 0 then -- this is the child process
        run_dzen2()
    else             -- this is the parent process
        -- nothing
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

-- remove all dzen2 instance
os.execute('pkill dzen2')

-- run process in the background
detach_dzen2()

{% endhighlight %}

This step also add system command that kill
any previous dzen2 instance. So it will be guaranteed,
that the dzen2 shown is coming from the latest script.

{% include post/2017/04/pipe-and-fork-similar-05.md %}

-- -- --

### How does it works ?

Any code after the <code>posix.fork()</code>
executed in both parent and child.
The child process has been detached from parent process.
The only different is the <code>pid</code>.

{% highlight lua %}
function detach_dzen2()
    local pid = posix.fork()

    if pid == 0 then -- this is the child process
        run_dzen2()
    else             -- this is the parent process
        -- nothing
    end
end
{% endhighlight %}

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[gitlab.com/.../dotfiles/.../lua-07-conky.lua][dotfiles-lua-07-conky]

{% highlight lua %}
#!/usr/bin/lua

-- luaposix available in AUR
local posix = require "posix"

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- helper

function sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- application related function

function get_dzen2_parameters ()
    local xpos    = '0'
    local ypos    = '0'
    local width   = '640'
    local height  = '24'
    local fgcolor = '#000000'
    local bgcolor = '#ffffff'
    local font    = '-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'

    local parameters = ""
        .. " -x "..xpos.." -y "..ypos
        .. " -w "..width.." -h "..height
        .. " -fn '"..font.."'"
        .. " -ta c -bg '"..bgcolor.."' -fg '"..fgcolor.."'"
        .. " -title-name dzentop"

    return parameters
end

function generated_output (process)
    local dirname  = debug.getinfo(1).source:match("@?(.*/)")
    local path     = dirname .. "../assets"
    local cmdin    = 'conky -c ' .. path .. '/conky.lua'
    
    local pipein  = assert(io.popen(cmdin,  'r'))
  
    for line in pipein:lines() do
        process:write(line.."\n")
        process:flush()
    end -- for loop
   
    pipein:close()
end

function run_dzen2 ()
    local cmdout  = 'dzen2 ' .. get_dzen2_parameters()
    local pipeout = assert(io.popen(cmdout, 'w'))
    
    generated_output(pipeout)
    
    pipeout:close()
end


function detach_dzen2()
    local pid = posix.fork()

    if pid == 0 then -- this is the child process
        run_dzen2()
    else             -- this is the parent process
        -- nothing
    end
end

function detach_transset()
    local pid = posix.fork()

    if pid == 0 then -- this is the child process
        sleep(1)
        os.execute('transset .8 -n dzentop >/dev/null')        
    else             -- this is the parent process
        -- nothing
    end
end


-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

-- remove all dzen2 instance
os.execute('pkill dzen2')

-- run process in the background
detach_dzen2()

-- optional transparency
detach_transset()

{% endhighlight %}

This would have <code>dzen2</code> output similar to this below.

![Pipe: to Dzen2][image-time-dzen]{: .img-responsive }

	You may use transset-df instead of transset.

{% include post/2017/04/pipe-and-fork-similar-07.md %}

-- -- --

### How does it works ?

	Nothing new here.

-- -- --

### Lemonbar

I also provide Lemonbar, instead of Dzen2.
The code is very similar.

**Source**:
*	[gitlab.com/.../dotfiles/.../lua-17-conky.lua][dotfiles-lua-17-conky]

{% include post/2017/04/pipe-and-fork-similar-17.md %}

-- -- --

### Coming up Next

There already an advance case of Pipe and Fork.
Multitier, and bidirectional.

*	[HerbstluftWM Event Idle in Lua][local-lua-idle]

-- -- --

There above are some simple codes I put together. 
I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua
[local-lua-idle]:   {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html

[dotfiles-lua-01-basic]:   {{ dotfiles_path }}/lua/lua-01-basic.lua
[dotfiles-lua-02-popen]:   {{ dotfiles_path }}/lua/lua-02-popen.lua
[dotfiles-lua-03-popen]:   {{ dotfiles_path }}/lua/lua-03-popen.lua
[dotfiles-lua-05-fork]:    {{ dotfiles_path }}/lua/lua-05-fork-function.lua
[dotfiles-lua-07-conky]:   {{ dotfiles_path }}/lua/lua-07-fork-function.lua

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-lua.png

[local-overview]: {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
