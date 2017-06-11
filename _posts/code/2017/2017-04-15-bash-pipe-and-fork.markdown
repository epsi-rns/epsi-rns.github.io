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

### Bashful Plumber.

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

### Fork Overview

Fork in bash is also simple.
All it takes is just <code>&</code> character.

This step use internal function as source feed,
as continuation of previous step.

This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[github.com/.../dotfiles/.../bash-05-fork-simple.sh][dotfiles-bash-05-fork-simple]

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

### Fork Code in Function

Let's have a look again at the line below.
It is the heart above script.

{% highlight bash %}
generated_output | dzen2 $parameters &
{% endhighlight %}

In more complex situation that need more flexibility,
we can separate the pipe <code>|</code> in different function.

{% highlight bash %}
#!/usr/bin/env bash

function get_dzen2_parameters() { 
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
}

function generated_output() {
    # endless loop
    while :; do 
      date +'%a %b %d %H:%M:%S'
      sleep 1
    done
}

function run_dzen2() {
    get_dzen2_parameters    
    command_out="dzen2 $parameters"
    
    {
        generated_output 
    } | $command_out
}

function detach_dzen2() {    
    run_dzen2 &
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
pkill dzen2

# run process in the background
detach_dzen2
{% endhighlight %}

**Source**:

*	[github.com/.../dotfiles/.../bash-05-fork.sh][dotfiles-bash-05-fork]

Similar Code: 
[[ BASH fork ]][dotfiles-bash-05-fork]
[[ Perl fork ]][dotfiles-perl-05-fork]
[[ Python fork ]][dotfiles-python-05-fork]
[[ Ruby fork ]][dotfiles-ruby-05-fork]
[[ PHP fork ]][dotfiles-php-05-fork]
[[ Lua fork ]][dotfiles-lua-05-fork]
[[ Haskell fork ]][dotfiles-haskell-05-fork]

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we have two forks, dzen and transset.

{% highlight bash %}
# optional transparency
sleep 1 && exec `(transset .8 -n dzentop >/dev/null 2>&1 &)` & 
{% endhighlight %}

**Source**:

*	[github.com/.../dotfiles/.../bash-07-conky-simple.sh][dotfiles-bash-07-conky-simple]

Finally, we have this complete script.

{% highlight bash %}
#!/usr/bin/env bash

function get_dzen2_parameters() { 
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
}

function generated_output() {
    dirname=$(dirname $(readlink -f "$0"))
    path="$dirname/../assets"
    conky -c "$path/conky.lua"
}

function run_dzen2() {
    get_dzen2_parameters    
    command_out="dzen2 $parameters"
    
    {
        generated_output 
    } | $command_out
}

function detach_dzen2() {    
    run_dzen2 &
}

function detach_transset() { 
    {
        sleep 1
    
        # you may use either xorg-transset or transset-df
        # https://github.com/wildefyr/transset-df    
        exec `(transset .8 -n dzentop >/dev/null)`
    } &
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
pkill dzen2

# run process in the background
detach_dzen2

# optional transparency
detach_transset
{% endhighlight %}

**Source**:
*	[github.com/.../dotfiles/.../bash-07-conky.sh][dotfiles-bash-07-conky]


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

There above are some simple codes I put together. 
I’m mostly posting codes so I won’t have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

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

[dotfiles-bash-05-fork-simple]:  {{ dotfiles_path }}/bash/bash-05-fork-simple.sh
[dotfiles-bash-07-conky-simple]: {{ dotfiles_path }}/bash/bash-07-conky-simple.sh
