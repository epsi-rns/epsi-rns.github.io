---
layout: post-sidemenu-wm
title:  "Piping and Forking in Lua"
categories: code
date:   2017-04-20 17:35:15 +0700
tags: [coding, conky, lua]
author: epsi

excerpt:
  How to be a Lua Plumber.
  
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

### Piping and Forking in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Pipe Overview ]][local-overview]
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

*	[github.com/.../dotfiles/.../lua-01-basic.lua][dotfiles-lua-01-basic]

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

Similar Code: 
[[ BASH basic ]][dotfiles-bash-01-basic]
[[ Perl basic ]][dotfiles-perl-01-basic]
[[ Python basic datetime ]][dotfiles-python-01-basic-date]
[[ Python basic time ]][dotfiles-python-01-basic-time]
[[ Ruby basic ]][dotfiles-ruby-01-basic]
[[ PHP basic ]][dotfiles-php-01-basic]
[[ Lua basic ]][dotfiles-lua-01-basic]
[[ Haskell basic ]][dotfiles-haskell-01-basic]

-- -- --

### External Command as Source Feed

Beside previous simple loop that is used as Internal Command,
this tutorial also provide Conky as External Command
in <code class="code-file">asset</code> directory.
I made it as simple as possible.

**Source**:

*	[github.com/.../dotfiles/.../conky.lua][dotfiles-conky]

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

*	[github.com/.../dotfiles/.../lua-02-popen.lua][dotfiles-lua-02-popen]

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

Similar Code: 
[[ BASH native ]][dotfiles-bash-02-native]
[[ Perl uni IO ]][dotfiles-perl-02-uni-io]
[[ Perl uni open ]][dotfiles-perl-02-uni-open]
[[ Perl IPC open2 ]][dotfiles-perl-02-uni-open2]
[[ Python subProcess]][dotfiles-python-02-subprocess]
[[ Ruby popen ]][dotfiles-ruby-02-popen]
[[ PHP popen ]][dotfiles-php-02-popen]
[[ PHP proc open ]][dotfiles-php-02-proc-open]
[[ Lua popen ]][dotfiles-lua-02-popen]
[[ Haskell createProcess ]][dotfiles-haskell-02-process]

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

*	[github.com/.../dotfiles/.../lua-03-popen.lua][dotfiles-lua-03-popen]

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

Similar Code: 
[[ BASH pipe ]][dotfiles-bash-03-pipe]
[[ Perl pipe open ]][dotfiles-perl-03-pipe-open]
[[ Perl pipe IO ]][dotfiles-perl-03-pipe-io]
[[ Perl IPC Open2 ]][dotfiles-perl-03-pipe-open2]
[[ Python subProcess ]][dotfiles-python-03-subprocess]
[[ Ruby pipe IO ]][dotfiles-ruby-03-pipe-io]
[[ Ruby popen ]][dotfiles-ruby-03-popen]
[[ Ruby open3 ]][dotfiles-ruby-03-open3]
[[ Ruby PTY ]][dotfiles-ruby-03-pty]
[[ PHP popen ]][dotfiles-php-03-popen]
[[ PHP proc open ]][dotfiles-php-03-proc-open]
[[ Lua popen ]][dotfiles-lua-03-popen]
[[ Haskell createProcess ]][dotfiles-haskell-03-process]

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

*	[github.com/.../dotfiles/.../lua-05-fork.lua][dotfiles-lua-05-fork]

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

Similar Code: 
[[ BASH fork ]][dotfiles-bash-05-fork]
[[ Perl fork ]][dotfiles-perl-05-fork]
[[ Python fork ]][dotfiles-python-05-fork]
[[ Ruby fork ]][dotfiles-ruby-05-fork]
[[ PHP fork ]][dotfiles-php-05-fork]
[[ Lua fork ]][dotfiles-lua-05-fork]
[[ Haskell fork ]][dotfiles-haskell-05-fork]

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

*	[github.com/.../dotfiles/.../lua-07-conky.lua][dotfiles-lua-07-conky]

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

Similar Code: 
[[ BASH conky ]][dotfiles-bash-07-conky]
[[ Perl conky ]][dotfiles-perl-07-conky]
[[ Python conky ]][dotfiles-python-07-conky]
[[ Ruby conky ]][dotfiles-ruby-07-conky]
[[ PHP conky ]][dotfiles-php-07-conky]
[[ Lua conky ]][dotfiles-lua-07-conky]
[[ Haskell conky ]][dotfiles-haskell-07-conky]

-- -- --

### How does it works ?

	Nothing new here.

-- -- --

### Lemonbar

I also provide Lemonbar, instead of Dzen2.
The code is very similar.

**Source**:
*	[github.com/.../dotfiles/.../lua-17-conky.lua][dotfiles-lua-17-conky]

Similar Code: 
[[ BASH lemon ]][dotfiles-bash-17-conky]
[[ Perl lemon ]][dotfiles-perl-17-conky]
[[ Python lemon ]][dotfiles-python-17-conky]
[[ Ruby lemon ]][dotfiles-ruby-17-conky]
[[ PHP lemon ]][dotfiles-php-17-conky]
[[ Lua lemon ]][dotfiles-lua-17-conky]
[[ Haskell lemon ]][dotfiles-haskell-17-conky]

-- -- --

There above are some simple codes I put together. 
I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[dotfiles-lua-01-basic]:   {{ dotfiles_path }}/lua/lua-01-basic.lua
[dotfiles-lua-02-popen]:   {{ dotfiles_path }}/lua/lua-02-popen.lua
[dotfiles-lua-03-popen]:   {{ dotfiles_path }}/lua/lua-03-popen.lua
[dotfiles-lua-05-fork]:    {{ dotfiles_path }}/lua/lua-05-fork-function.lua
[dotfiles-lua-07-conky]:   {{ dotfiles_path }}/lua/lua-07-fork-function.lua

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-lua.png

[local-overview]: {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
[local-BASH]:     {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-Perl]:     {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-python]:   {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-Ruby]:     {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-PHP]:      {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html
[local-Lua]:      {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-Haskell]:  {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell

[dotfiles-bash-01-basic]:   {{ dotfiles_path }}/bash/bash-01-basic.sh
[dotfiles-perl-01-basic]:     {{ dotfiles_path }}/perl/perl-01-basic.pl
[dotfiles-python-01-basic-date]: {{ dotfiles_path }}/python/python-01-basic-date.py
[dotfiles-python-01-basic-time]: {{ dotfiles_path }}/python/python-01-basic-time.py
[dotfiles-ruby-01-basic]:   {{ dotfiles_path }}/ruby/ruby-01-basic.rb
[dotfiles-php-01-basic]:   {{ dotfiles_path }}/php/php-01-basic.php
[dotfiles-lua-01-basic]:   {{ dotfiles_path }}/lua/lua-01-basic.lua
[dotfiles-haskell-01-basic]:   {{ dotfiles_path }}/haskell/haskell-01-basic.hs

[dotfiles-bash-02-native]:       {{ dotfiles_path }}/bash/bash-02-native.sh
[dotfiles-perl-02-uni-io]:       {{ dotfiles_path }}/perl/perl-02-uni-io.pl
[dotfiles-perl-02-uni-open]:     {{ dotfiles_path }}/perl/perl-02-uni-open.pl
[dotfiles-perl-02-uni-open2]:    {{ dotfiles_path }}/perl/perl-02-uni-open2.pl
[dotfiles-python-02-subprocess]: {{ dotfiles_path }}/python/python-02-subprocess-open.py
[dotfiles-ruby-02-popen]:        {{ dotfiles_path }}/ruby/ruby-02-popen.rb
[dotfiles-php-02-popen]:         {{ dotfiles_path }}/php/php-02-popen.php
[dotfiles-php-02-proc-open]:     {{ dotfiles_path }}/php/php-02-proc-open.php
[dotfiles-lua-02-popen]:         {{ dotfiles_path }}/lua/lua-02-popen.lua
[dotfiles-haskell-02-process]:   {{ dotfiles_path }}/haskell/haskell-02-process.hs

[dotfiles-bash-03-pipe]:         {{ dotfiles_path }}/bash/bash-03-pipe.sh
[dotfiles-perl-03-pipe-io]:      {{ dotfiles_path }}/perl/perl-03-pipe-io.pl
[dotfiles-perl-03-pipe-open]:    {{ dotfiles_path }}/perl/perl-03-pipe-open.pl
[dotfiles-perl-03-pipe-open2]:   {{ dotfiles_path }}/perl/perl-03-pipe-open2.pl
[dotfiles-python-03-subprocess]: {{ dotfiles_path }}/python/python-03-subprocess-simple.py
[dotfiles-ruby-03-open3]:        {{ dotfiles_path }}/ruby/ruby-03-open3.rb
[dotfiles-ruby-03-pipe-io]:      {{ dotfiles_path }}/ruby/ruby-03-pipe-io.rb
[dotfiles-ruby-03-popen]:        {{ dotfiles_path }}/ruby/ruby-03-popen.rb
[dotfiles-ruby-03-pty]:          {{ dotfiles_path }}/ruby/ruby-03-pty.rb
[dotfiles-php-03-popen]:         {{ dotfiles_path }}/php/php-03-popen.php
[dotfiles-php-03-proc-open]:     {{ dotfiles_path }}/php/php-03-proc-open.php
[dotfiles-lua-03-popen]:         {{ dotfiles_path }}/lua/lua-03-popen.lua
[dotfiles-haskell-03-process]:   {{ dotfiles_path }}/haskell/haskell-03-process.hs

[dotfiles-bash-05-fork]:    {{ dotfiles_path }}/bash/bash-05-fork.sh
[dotfiles-perl-05-fork]:    {{ dotfiles_path }}/perl/perl-05-fork-sub.pl
[dotfiles-python-05-fork]:  {{ dotfiles_path }}/python/python-05-fork-def.py
[dotfiles-ruby-05-fork]:    {{ dotfiles_path }}/ruby/ruby-05-fork-def.rb
[dotfiles-php-05-fork]:     {{ dotfiles_path }}/php/php-05-fork-function.php
[dotfiles-lua-05-fork]:     {{ dotfiles_path }}/lua/lua-05-fork-function.lua
[dotfiles-haskell-05-fork]: {{ dotfiles_path }}/haskell/haskell-05-fork.hs

[dotfiles-bash-07-conky]:    {{ dotfiles_path }}/bash/bash-07-conky.sh
[dotfiles-perl-07-conky]:    {{ dotfiles_path }}/perl/perl-07-fork-conky.pl
[dotfiles-python-07-conky]:  {{ dotfiles_path }}/python/python-07-fork-conky.py
[dotfiles-ruby-07-conky]:    {{ dotfiles_path }}/ruby/ruby-07-fork-conky.rb
[dotfiles-php-07-conky]:     {{ dotfiles_path }}/php/php-07-fork-conky.php
[dotfiles-lua-07-conky]:     {{ dotfiles_path }}/lua/lua-07-fork-function.lua
[dotfiles-haskell-07-conky]: {{ dotfiles_path }}/haskell/haskell-07-fork.hs

[dotfiles-bash-17-conky]:    {{ dotfiles_path }}/bash/bash-17-conky.sh
[dotfiles-perl-17-conky]:    {{ dotfiles_path }}/perl/perl-17-fork-conky.pl
[dotfiles-python-17-conky]:  {{ dotfiles_path }}/python/python-17-fork-conky.py
[dotfiles-ruby-17-conky]:    {{ dotfiles_path }}/ruby/ruby-17-fork-conky.rb
[dotfiles-php-17-conky]:     {{ dotfiles_path }}/php/php-17-fork-conky.php
[dotfiles-lua-17-conky]:     {{ dotfiles_path }}/lua/lua-17-fork-function.lua
[dotfiles-haskell-17-conky]: {{ dotfiles_path }}/haskell/haskell-17-fork.hs
