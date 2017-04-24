---
layout: post-sidemenu-wm
title:  "Piping and Forking in BASH"
categories: code
date:   2017-04-15 17:35:15 +0700
tags: [coding, conky, bash]
author: epsi

excerpt:
  How to be a Bashful Plumber.
  
---

### How not to be a Bashful Plumber.

Piping is a powerful concept in Linux and Unix.
Concept is deeper than just syntax.
As meaning is more important than grammar.

Piping is very common, and very easy to do it in BASH.
But we might need a little googling
when it comes to other language.
Most common method are using <code>popen</code>,
or <code>subProcess</code>.
but there are other mechanism as well,
depend on the language you deal with.

	Becoming plumber automagically.

I have made few example of Pipe Port in other language,
step by step for your convenience.
So now we have BASH, Perl, Python, Ruby, PHP, Lua, and Haskell.
These will give you overview on how
to flow your stream through pipe conduit.

	Dark art of daemonizing a process.

For each step I also add Fork example,
so your process can run in the background,
detached from console.

	Make the script simple, less problem.

Since we want to go for walk step by step,
I use <code>less</code> for Pipe target,
and later <code>dzen2</code> for Pipe Target.
And for feed, we are using conky,
and also function as a pipe source.

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

I have seen a lot of configuration in dotfiles.
Most utilize external tool written in BASH, Perl of Python.
Between taste and color of your language, it is just about preferences.
If you want to combine your preferred language to configure
Dzen2 or Lemonbar,or any tools in need of piping and fork,
then this tutorial is a good starter kit for you.

For historical reason.
BASH evolve to Perl, Perl Evolve to PHP and Python.
And here we are with Ruby. This is why I put these script.

I also add Lua.
Lua only used as embedded script. But I can see Lua's potential.
I have never seen anybody utilize Lua as main scripting tools in dotfiles,
except Conky, and AwesomeWM that use Lua as main scripting configuration.
I myself utilize a lot of Conky a lot. So why not make a start ?

How about Haskell? 
This is not a scripting language but compiled.
The reason is XMonad. Most people is still combined
the Haskell Configuration, with BASH, conky, even Perl.
Makes it looks like a Frankestein.
Once you know Haskell, this language is Sexy.

And of course you have total Control of the script,
e.g. color across the configuration,
when you call from just one script, instead of many language.

But again, it is a matter of preferences.

-- -- --

### A Very Bashful Start

Welcome to n00berland. Begin with simple script.
We will use this loop as a source feed to pipe.
This step won't introduce Pipe nor Fork.

This script only show an infinite loop showing local time.
Each updated in one second interval.
We manage this interval by delaying,
using <code>sleep</code> code.


**Source**:

*	[github.com/.../dotfiles/.../bash-01-basic.sh][dotfiles-bash-01-basic]

{% highlight bash %}
#!/usr/bin/env bash

# endless loop
while true; do 
    date +'%a %b %d %H:%M:%S'
    sleep 1
done
{% endhighlight %}

Call to this simple code would produce time marching,
one after another, below the command line prompt.

![Pipe: Basic][image-time-basic]{: .img-responsive }

-- -- --

### Port to other Language

I respect the reader, that you are smart.
You might be wondering, why I have to put 
this very basic script in this tutorial.
This script may looks simple in BASH,
and have very similar looks in most popular scripting language.
But it would have very different approach in other language.
As you can see in an obscure language below,
Haskell has no loop, but using <code>forever</code> control.

**Source**:

*	[github.com/.../dotfiles/.../haskell-01-basic.hs][dotfiles-haskell-01-basic]

{% highlight haskell %}
import Data.Time.LocalTime
import Data.Time.Format

import Control.Concurrent
import Control.Monad

-- ----- ----- ----- ----- -----
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

-- ----- ----- ----- ----- -----
-- main

main = forever $ printDate
{% endhighlight %}

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

### A Native Pipe Between External Command

This step is overview of Pipe between two external command.
This is a very simple. Only using <code>|</code> character.
This very short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I had made additional dirname function,
relative to the BASH source,
to locate the conky script assets.

**Source**:

*	[github.com/.../dotfiles/.../bash-02-native.sh][dotfiles-bash-02-native]

{% highlight bash %}
#!/usr/bin/env bash

dirname=$(dirname $(readlink -f "$0"))
path="$dirname/../assets"

cmdin="conky -c $path/conky.lua"
cmdout="less" # or dzen2

$cmdin | $cmdout
{% endhighlight %}

You can see, how simple it is.
This would have <code>less</code> output similar to this below.

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

-- -- --

### A Native Pipe from Internal Function

Using internal function as source feed
to external command is straight forward.
This should be self explanatory.

Other language has more complex mechanism for this.
Most common is using <code>subProcess</code> or <code>Popen</code>.
From this step forward, this would looks different in other language.

**Source**:

*	[github.com/.../dotfiles/.../bash-03-pipe.sh][dotfiles-bash-03-pipe]

{% highlight bash %}
#!/usr/bin/env bash

generated_output() {
    # endless loop
    while :; do 
      date +'%a %b %d %H:%M:%S'
      sleep 1
    done
}

cmdout="less" # or dzen2

generated_output | $cmdout
{% endhighlight %}

-- -- --

### Fork Overview

Fork in bash is also simple.
All it takes is jus <code>&</code> character.

This step use internal function as source feed,
as continuation of previous step.

This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[github.com/.../dotfiles/.../bash-05-fork.sh][dotfiles-bash-05-fork]

{% highlight bash %}
#!/usr/bin/env bash

generated_output() {
    # endless loop
    while :; do 
      date +'%a %b %d %H:%M:%S'
      sleep 1
    done
}

xpos=0
ypos=0
width=640
height=24
fgcolor="#000000"
bgcolor="#ffffff"
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

parameters="  -x $xpos -y $ypos -w $width -h $height" 
parameters+=" -fn $font"
parameters+=" -ta c -bg $bgcolor -fg $fgcolor"
parameters+=" -title-name dzentop"

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
pkill dzen2

generated_output | dzen2 $parameters &
{% endhighlight %}

This step also add system command that kill
any previous dzen2 instance. So it will be guaranteed,
that the dzen2 shown is coming from the latest script.

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.


**Source**:

*	[github.com/.../dotfiles/.../bash-07-conky.sh][dotfiles-bash-07-conky]

{% highlight bash %}
#!/usr/bin/env bash

generated_output() {
    dirname=$(dirname $(readlink -f "$0"))
    path="$dirname/../assets"
    conky -c "$path/conky.lua"
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# parameters

xpos=0
ypos=0
width=640
height=24
fgcolor="#000000"
bgcolor="#ffffff"
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

parameters="  -x $xpos -y $ypos -w $width -h $height" 
parameters+=" -fn $font"
parameters+=" -ta c -bg $bgcolor -fg $fgcolor"
parameters+=" -title-name dzentop"

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
pkill dzen2

# execute dzen
generated_output | dzen2 $parameters &

# optional transparency
sleep 1 && exec `(transset .8 -n dzentop >/dev/null 2>&1 &)` & 
{% endhighlight %}

This would have <code>dzen2</code> output similar to this below.

![Pipe: to Dzen2][image-time-dzen]{: .img-responsive }

-- -- --


There above are some simple codes I put together. 
I’m mostly posting codes so I won’t have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lang' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[dotfiles-bash-01-basic]:   {{ dotfiles_path }}/bash/bash-01-basic.sh
[dotfiles-bash-02-native]:  {{ dotfiles_path }}/bash/bash-02-native.sh
[dotfiles-bash-03-pipe]:    {{ dotfiles_path }}/bash/bash-03-pipe.sh
[dotfiles-bash-05-fork]:    {{ dotfiles_path }}/bash/bash-05-fork.sh
[dotfiles-bash-07-conky]:   {{ dotfiles_path }}/bash/bash-07-conky.sh
[dotfiles-haskell-01-basic]: {{ dotfiles_path }}/haskell/haskell-01-basic.hs

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-bash-basic.png

[local-BASH]:    {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-Perl]:    {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-python]:  {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-Ruby]:    {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-PHP]:     {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html
[local-Lua]:     {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-Haskell]: {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell
