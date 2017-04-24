---
layout: post-sidemenu-wm
title:  "Piping and Forking in Haskell"
categories: code
date:   2017-04-21 17:35:15 +0700
tags: [coding, conky, haskell]
author: epsi

excerpt:
  How to be a Haskell Plumber.
  
---

### Haskell Plumber.

	Goal: A script that continuously show date and time,
	with Dzen2, and Conky.

-- -- --

### Piping and Forking in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
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

	Why Haskell? 

This is not a scripting language but compiled.
The reason is XMonad. Most people is still combined
the Haskell Configuration, with BASH, conky, even Perl.
Makes it looks like a Frankestein.
We need a more unified approach.

And of course you have total Control of the script,
e.g. color across the configuration,
when you call from just one Haskell script, instead of many language.

-- -- --

### Start Simple

Welcome to n00berland. Begin with simple script.
We will use this loop as a source feed to pipe.
This step won't introduce Pipe nor Fork.

This script only show an infinite loop showing local time.
Each updated in one second interval.
We manage this interval by delaying,
using <code>sleep</code> code.

A few things to be noted here:

*	Haskell does have <code>threadDelay</code> function.
	But since it is in microsecond.
	We have to wrap it in a new <code>sleep</code> function.

*	Haskell complex time library could be wrapped into simple function.

*	Haskell has no built in loop.
	But we can use Control, e.g. <code>Forever</code>.

**Source**:

*	[github.com/.../dotfiles/.../haskell-01-basic.hs][dotfiles-haskell-01-basic]

{% highlight haskell %}
import Data.Time.LocalTime
import Data.Time.Format

import Control.Concurrent
import Control.Monad

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- wrap Funktion

myTimeFormat = "%a %b %d %H:%M:%S"

wFormatTime :: FormatTime t => t -> String
wFormatTime myUtcTime = formatTime 
  Data.Time.Format.defaultTimeLocale myTimeFormat myUtcTime

wSleep :: Int -> IO ()
wSleep mySecond = threadDelay (1000000 * mySecond)

printDate = do
     now <- getZonedTime
     let nowFmt = wFormatTime now
     putStrLn nowFmt
     wSleep 1

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

main = forever $ printDate
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

### How does it works ?

Haskell has no built in loop.
Since we want infinite loop, we use Control <code>Forever</code>.

{% highlight haskell %}
main = forever $ printDate
{% endhighlight %}

There is this Dollar <code>$</code> operator.
You might consider reading my old post.

*	[How Haskell Syntax can Make Your Code Cleaner][local-haskell-dollar]

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

### Spawning Using System Shell

Using system shell is simple and straightforward.
But it does not have any ability,
to stream internal function process,
that required later on this article.

**Source**:

*	[github.com/.../dotfiles/.../haskell-02-system.hs][dotfiles-haskell-02-system]

{% highlight haskell %}
import System.Process
import System.Directory

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- wrap Funktion

-- source directory is irrelevant in Haskell
-- but we'll do it anyway for the sake of learning
wGetCmdIn :: String -> String
wGetCmdIn dirname = "conky -c " 
    ++ dirname ++ "/../assets" ++ "/conky.lua"

cmdout = "less" -- or dzen2

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

main = do
    dirname <- getCurrentDirectory
    let cmdin = wGetCmdIn dirname
    system $ cmdin ++ " | " ++ cmdout
{% endhighlight %}

Similar Code: 
[[ Perl system ]][dotfiles-perl-02-system]
[[ Python system ]][dotfiles-python-02-system]
[[ Python popen ]][dotfiles-python-02-popen]
[[ Ruby system ]][dotfiles-ruby-02-system]
[[ Ruby spawn ]][dotfiles-ruby-02-spawn]
[[ Ruby shell ]][dotfiles-ruby-02-shell]
[[ Haskell system ]][dotfiles-haskell-02-system]

-- -- --

### A Unidirectional Pipe Between External Command

This step is overview of Pipe between two external command.
This short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I add <code>dirname</code>, relative to the Haskell source,
to locate the conky script assets.
Source directory is irrelevant in Haskell,
since Haskell is compiled, and can be execute from anywhere.
But we'll do it anyway for the sake of learning

We use this amazingly cool Haskell's
<code>createProcess</code> mechanism.

**Source**:

*	[github.com/.../dotfiles/.../haskell-02-process.hs][dotfiles-haskell-02-process]

{% highlight haskell %}
import System.Process
import System.Directory

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- wrap Funktion

-- Source directory is irrelevant in Haskell
-- but we'll do it anyway for the sake of learning
wConkyFileName :: String -> String
wConkyFileName dirName = dirName ++ "/../assets" ++ "/conky.lua"

cmdin  = "conky"
cmdout = "less" -- or dzen2

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

main = do
    dirName <- getCurrentDirectory
    let conkyFileName = wConkyFileName dirName   
  
    (_, Just pipeout, _, _) <- 
        createProcess (proc cmdin ["-c", conkyFileName])
        { std_out = CreatePipe } 

    (_, _, _, ph)  <- 
        createProcess (proc cmdout []) 
        { std_in = UseHandle pipeout }
    
    -- just remove this waitForProcess line to detach dzen2
    _ <- waitForProcess ph

    putStr ""
{% endhighlight %}


You can see, how simple it is.
This would have <code>less</code> output similar to this below.

You can just remove this <code>waitForProcess</code> line
to detach dzen2. No need to fork

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

Similar Code: 
[[ BASH native ]][dotfiles-bash-02-native]
[[ Perl uni IO ]][dotfiles-perl-02-uni-io]
[[ Perl uni open ]][dotfiles-perl-02-uni-open]
[[ Python subProcess]][dotfiles-python-02-subprocess]
[[ Ruby popen ]][dotfiles-ruby-02-popen]
[[ PHP popen ]][dotfiles-php-02-popen]
[[ Lua popen ]][dotfiles-lua-02-popen]
[[ Haskell createProcess ]][dotfiles-haskell-02-process]

-- -- --

### How does it works ?

First process create a new stdout handle <code>std_out = CreatePipe</code>.
And the second process use it as stdin feed <code>std_in = UseHandle pipeout</code>.

{% highlight haskell %}
    (_, Just pipeout, _, _) <- 
        createProcess (proc cmdin ["-c", conkyFileName])
        { std_out = CreatePipe } 

    (_, _, _, ph)  <- 
        createProcess (proc cmdout []) 
        { std_in = UseHandle pipeout }
{% endhighlight %}

-- -- --

### A Unidirectional Pipe from Internal Function

Using internal function as source feed
to external command is straight forward.
This should be self explanatory.

	Do not forget to flush.

**Source**:

*	[github.com/.../dotfiles/.../haskell-03-process.hs][dotfiles-haskell-03-process]

{% highlight haskell %}
import System.Process
import System.IO

import Data.Time.LocalTime
import Data.Time.Format

import Control.Concurrent
import Control.Monad

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- wrap Funktion

wFormatTime :: FormatTime t => t -> String
wFormatTime myUtcTime = formatTime 
        Data.Time.Format.defaultTimeLocale myTimeFormat myUtcTime
    where myTimeFormat = "%a %b %d %H:%M:%S"

wSleep :: Int -> IO ()
wSleep mySecond = threadDelay (1000000 * mySecond)

generatedOutput pipein = do
     now <- getZonedTime
     let nowFmt = wFormatTime now

     hPutStrLn pipein nowFmt
     hFlush pipein

     wSleep 1

cmdout = "less" -- or dzen2

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

main = do
    (Just pipein, _, _, ph)  <- 
        createProcess (System.Process.proc cmdout []) 
        { std_in = CreatePipe }
    
    forever $ generatedOutput pipein
    hClose pipein
    
    putStr ""

{% endhighlight %}

Similar Code: 
[[ BASH pipe ]][dotfiles-bash-03-pipe]
[[ Perl pipe open ]][dotfiles-perl-03-pipe-open]
[[ Perl pipe IO ]][dotfiles-perl-03-pipe-io]
[[ Python subProcess ]][dotfiles-python-03-subprocess]
[[ Ruby pipe IO ]][dotfiles-ruby-03-pipe-io]
[[ Ruby popen ]][dotfiles-ruby-03-popen]
[[ Ruby open3 ]][dotfiles-ruby-03-open3]
[[ Ruby PTY ]][dotfiles-ruby-03-pty]
[[ PHP popen ]][dotfiles-php-03-popen]
[[ Lua popen ]][dotfiles-lua-03-popen]
[[ Haskell createProcess ]][dotfiles-haskell-03-process]

-- -- --

### How does it works ?

The same as previous.
But instead of reading from stdout <code>std_out</code>,
it is managed by internal process using <code>hPutStrLn</code>.

The second process use its own stdin <code>std_in = CreatePipe</code>.

{% highlight haskell %}
     now <- getZonedTime
     let nowFmt = wFormatTime now

     hPutStrLn pipein nowFmt
{% endhighlight %}

-- -- --

### Fork Overview

This step use internal function as source feed,
as continuation of previous step.

This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[github.com/.../dotfiles/.../haskell-05-fork.hs][dotfiles-haskell-05-fork]

{% highlight haskell %}
import System.Process
import System.IO
import System.Posix.Process

import Data.Time.LocalTime
import Data.Time.Format

import Control.Concurrent
import Control.Monad

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- wrap Funktion

cmdout = "dzen2"

getDzen2Parameters = [
        "-x", xpos,  "-y", ypos,
        "-w", width, "-h", height,
        "-fn", font,
        "-ta", "c",
        "-bg", bgcolor,
        "-fg", fgcolor,
        "-title-name", "dzentop"
     ]
    where    
        xpos    = "0"
        ypos    = "0"
        width   = "640"
        height  = "24"
        fgcolor = "#000000"
        bgcolor = "#ffffff"
        font    = "-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"


wFormatTime :: FormatTime t => t -> String
wFormatTime myUtcTime = formatTime 
        Data.Time.Format.defaultTimeLocale myTimeFormat myUtcTime
    where myTimeFormat = "%a %b %d %H:%M:%S"


wSleep :: Int -> IO ()
wSleep mySecond = threadDelay (1000000 * mySecond)

generatedOutput pipein = do
     now <- getZonedTime
     let nowFmt = wFormatTime now

     hPutStrLn pipein nowFmt
     hFlush pipein

     wSleep 1

runDzen2 = do
    (Just pipein, _, _, ph)  <- 
        createProcess (proc cmdout getDzen2Parameters) 
        { std_in = CreatePipe }
    
    forever $ generatedOutput pipein
    
    hClose pipein

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

main = do
    -- remove all dzen2 instance
    system "pkill dzen2"

    -- run process in the background
    -- forkProcess is required since we use forever control
    forkProcess $ runDzen2
    
    putStr ""

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

Since we use <code>forever</code> control, it will block the process forever.
So we do have to detach it using <code>forkProcess</code>.

{% highlight haskell %}
    forkProcess $ runDzen2
{% endhighlight %}

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[github.com/.../dotfiles/.../haskell-07-conky.hs][dotfiles-haskell-07-conky]

{% highlight haskell %}
import System.Process
import System.Directory
import System.IO
import System.Posix.Process

import Control.Concurrent
import Control.Monad

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- wrap Funktion

cmdin  = "conky"
cmdout = "dzen2"

-- Source directory is irrelevant in Haskell
-- but we'll do it anyway for the sake of learning
wConkyFileName :: String -> String
wConkyFileName dirName = dirName ++ "/../assets" ++ "/conky.lua"

getDzen2Parameters = [
        "-x", xpos,  "-y", ypos,
        "-w", width, "-h", height,
        "-fn", font,
        "-ta", "c",
        "-bg", bgcolor,
        "-fg", fgcolor,
        "-title-name", "dzentop"
     ]
    where    
        xpos    = "0"
        ypos    = "0"
        width   = "640"
        height  = "24"
        fgcolor = "#000000"
        bgcolor = "#ffffff"
        font    = "-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

wSleep :: Int -> IO ()
wSleep mySecond = threadDelay (1000000 * mySecond)

detachDzen2 = do
    dirName <- getCurrentDirectory
    let conkyFileName = wConkyFileName dirName  

    (_, Just pipeout, _, _) <- 
        createProcess (proc cmdin ["-c", conkyFileName])
        { std_out = CreatePipe } 

    (_, _, _, ph)  <- 
        createProcess (proc cmdout getDzen2Parameters) 
        { std_in = UseHandle pipeout }
      
    hClose pipeout

detachTransset = forkProcess $ do    
    wSleep 1
    system "transset .8 -n dzentop >/dev/null 2"
    putStr ""

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
-- main

main = do
    -- remove all dzen2 instance
    system "pkill dzen2"

    -- run process in the background
    detachDzen2

    -- optional transparency
    detachTransset
    
    putStr ""

{% endhighlight %}

This would have <code>dzen2</code> output similar to this below.

![Pipe: to Dzen2][image-time-dzen]{: .img-responsive }

-- -- --

### How does it works ?

Since we do not use <code>forever</code> control,
we do not need to detach dzen2 using <code>forkProcess</code>.

But we still have to do <code>forkProcess</code>
for <code>transset</code>.
So it does not wait for <code>wSleep 1</code>.
Or to be precise, delayed in the background.

{% highlight haskell %}
detachTransset = forkProcess $ do    
    wSleep 1
    system "transset .8 -n dzentop >/dev/null 2"
    putStr ""
{% endhighlight %}

-- -- --

There above are some simple codes I put together. 
I’m mostly posting codes so I won’t have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lang' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[dotfiles-haskell-01-basic]:   {{ dotfiles_path }}/haskell/haskell-01-basic.hs
[dotfiles-haskell-02-system]:  {{ dotfiles_path }}/haskell/haskell-02-system.hs
[dotfiles-haskell-02-process]: {{ dotfiles_path }}/haskell/haskell-02-process.hs
[dotfiles-haskell-03-process]: {{ dotfiles_path }}/haskell/haskell-03-process.hs
[dotfiles-haskell-05-fork]:    {{ dotfiles_path }}/haskell/haskell-05-fork.hs
[dotfiles-haskell-07-conky]:   {{ dotfiles_path }}/haskell/haskell-07-fork.hs

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-haskell.png

[local-BASH]:    {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-Perl]:    {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-python]:  {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-Ruby]:    {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-PHP]:     {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html
[local-Lua]:     {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-Haskell]: {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html

[local-Haskell-dollar]: {{ site.url }}/code/2016/05/14/haskell-dollar-syntax.html

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

[dotfiles-perl-02-system]:    {{ dotfiles_path }}/perl/perl-02-system-shell.pl
[dotfiles-python-02-system]:  {{ dotfiles_path }}/python/python-02-system-shell.py
[dotfiles-python-02-popen]:   {{ dotfiles_path }}/python/python-02-popen.py
[dotfiles-ruby-02-system]:    {{ dotfiles_path }}/ruby/ruby-02-system.rb
[dotfiles-ruby-02-spawn]:     {{ dotfiles_path }}/ruby/ruby-02-spawn.rb
[dotfiles-ruby-02-shell]:     {{ dotfiles_path }}/ruby/ruby-02-shell.rb
[dotfiles-haskell-02-system]: {{ dotfiles_path }}/haskell/haskell-02-system.hs

[dotfiles-bash-02-native]:       {{ dotfiles_path }}/bash/bash-02-native.sh
[dotfiles-perl-02-uni-io]:       {{ dotfiles_path }}/perl/perl-02-uni-io.pl
[dotfiles-perl-02-uni-open]:     {{ dotfiles_path }}/perl/perl-02-uni-open.pl
[dotfiles-python-02-subprocess]: {{ dotfiles_path }}/python/python-02-subprocess-open.py
[dotfiles-ruby-02-popen]:        {{ dotfiles_path }}/ruby/ruby-02-popen.rb
[dotfiles-php-02-popen]:         {{ dotfiles_path }}/php/php-02-popen.php
[dotfiles-lua-02-popen]:         {{ dotfiles_path }}/lua/lua-02-popen.lua
[dotfiles-haskell-02-process]:   {{ dotfiles_path }}/haskell/haskell-02-process.hs

[dotfiles-bash-03-pipe]:         {{ dotfiles_path }}/bash/bash-03-pipe.sh
[dotfiles-perl-03-pipe-io]:      {{ dotfiles_path }}/perl/perl-03-pipe-io.pl
[dotfiles-perl-03-pipe-open]:    {{ dotfiles_path }}/perl/perl-03-pipe-open.pl
[dotfiles-python-03-subprocess]: {{ dotfiles_path }}/python/python-03-subprocess-simple.py
[dotfiles-ruby-03-open3]:        {{ dotfiles_path }}/ruby/ruby-03-open3.rb
[dotfiles-ruby-03-pipe-io]:      {{ dotfiles_path }}/ruby/ruby-03-pipe-io.rb
[dotfiles-ruby-03-popen]:        {{ dotfiles_path }}/ruby/ruby-03-popen.rb
[dotfiles-ruby-03-pty]:          {{ dotfiles_path }}/ruby/ruby-03-pty.rb
[dotfiles-php-03-popen]:         {{ dotfiles_path }}/php/php-03-popen.php
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
