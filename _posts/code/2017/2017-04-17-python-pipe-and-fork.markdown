---
layout: post-sidemenu-wm
title:  "Piping and Forking in Python"
categories: code
date:   2017-04-17 17:35:15 +0700
tags: [coding, conky, perl]
author: epsi

excerpt:
  How to be a Python Plumber.
  
---

### Python Plumber.

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

### Start Simple

Welcome to n00berland. Begin with simple script.
We will use this loop as a source feed to pipe.
This step won't introduce Pipe nor Fork.

This script only show an infinite loop showing local time.
Each updated in one second interval.
We manage this interval by delaying,
using <code>sleep</code> code.

There at least two choices to show current time in Python.
Using <code>time</code> library, or using <code>datetime</code> library.

**Source**:

*	[github.com/.../dotfiles/.../python-01-basic-date.py][dotfiles-python-01-basic-date]

*	[github.com/.../dotfiles/.../python-01-basic-time.py][dotfiles-python-01-basic-time]

Using <code>time</code> library.

{% highlight python %}
#!/usr/bin/env python3

import time

timeformat = '%Y-%m-%d %H:%M:%S'

while True:
    timestr = time.strftime(timeformat)
    print(timestr)
    time.sleep(1)
{% endhighlight %}

Using <code>datetime</code> library.

{% highlight python %}
#!/usr/bin/env python3

import datetime
from time import sleep

timeformat = '{0:%Y-%m-%d %H:%M:%S}'

while True:
    datestr = timeformat.format(datetime.datetime.now())
    print(datestr)
    sleep(1)
{% endhighlight %}

Call to this simple code would produce time marching,
one after another, below the command line prompt.

![Pipe: Basic][image-time-basic]{: .img-responsive }

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

Using <code>os.system</code> shell is simple and straightforward.
But it does not have any ability, to stream internal function process,
that required later on this article.

I haven't explore <code>os.popen</code>,
since we are going to use <code>subprocess.Popen</code> anyway.

**Source**:

*	[github.com/.../dotfiles/.../python-02-system.py][dotfiles-python-02-system]

*	[github.com/.../dotfiles/.../python-02-popen.py][dotfiles-python-02-popen]

Using <code>os.system</code>:

{% highlight perl %}
#!/usr/bin/env python3

import os

dirname = os.path.dirname(os.path.abspath(__file__))
path    = dirname + "/../assets"
cmdin   = 'conky -c ' + path + '/conky.lua'

cmdout  = 'less' # or 'dzen2'

cmd     = cmdin + ' | ' + cmdout

os.system(cmd)
{% endhighlight %}

Using <code>os.popen</code>:

{% highlight perl %}
#!/usr/bin/env python3

import os
import subprocess

dirname = os.path.dirname(os.path.abspath(__file__))
path    = dirname + "/../assets"
cmdin   = 'conky -c ' + path + '/conky.lua'

cmdout  = 'dzen2' # no less

cmd     = cmdin + ' | ' + cmdout

process = os.popen(cmd, 'r')
{% endhighlight %}


-- -- --

### A Unidirectional Pipe Between External Command

This step is overview of Pipe between two external command.
This script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I add <code>dirname</code>, relative to the Python source,
to locate the conky script assets.

We use this awesomely cool Python's
<code>subprocess.Popen</code> mechanism.

**Source**:

*	[github.com/.../dotfiles/.../python-02-subprocess.py][dotfiles-python-02-subprocess]

{% highlight python %}
#!/usr/bin/env python3

# https://pymotw.com/2/subprocess/

import os
import subprocess

dirname = os.path.dirname(os.path.abspath(__file__))
path    = dirname + "/../assets"
cmdin   = 'conky -c ' + path + '/conky.lua'

cmdout  = 'less -K' # or 'dzen2'

pipein = subprocess.Popen(
        [cmdin], 
        stdout = subprocess.PIPE, 
        stderr = subprocess.STDOUT,
        shell  = True,
        universal_newlines = True
    )

pipeout = subprocess.Popen(
        [cmdout],
        stdin  = pipein.stdout,
        shell  = True,
        universal_newlines = True
    )

# http://kendriu.com/how-to-use-pipes-in-python-subprocesspopen-objects

pipein.stdout.close()
outputs, errors = pipeout.communicate()

# avoid zombie apocalypse
pipeout.wait()
{% endhighlight %}

You can see, how simple it is.
This would have <code>less</code> output similar to this below.

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

-- -- --

### A Unidirectional Pipe from Internal Function

Using internal function as source feed
to external command is straight forward.
This should be self explanatory.

	Do not forget to flush.

As previous example, we are using two mechanism,
<code>open</code> and <code>subprocess.Popen</code>.

A more complex try-except block also provided in link below.
But I would not make thos tutorial too complex.


**Source**:

*	[github.com/.../dotfiles/.../python-03-subprocess.py][dotfiles-python-03-subprocess]

*	[github.com/.../dotfiles/.../python-03-try-except.py][dotfiles-python-03-try-except]

{% highlight python %}
#!/usr/bin/env python3

# https://gist.github.com/waylan/2353749

import datetime
import time
import subprocess

timeformat = '{0:%Y-%m-%d %H:%M:%S}'

cmdout  = 'less -K' # or 'dzen2'

process = subprocess.Popen(
        [cmdout], 
        stdin  = subprocess.PIPE,
        shell  = True,
        universal_newlines=True
    )

while True:
    datestr = timeformat.format(datetime.datetime.now())
    
    process.stdin.write(datestr + '\n')
    process.stdin.flush()
    
    time.sleep(1)

process.stdin.close()
process.wait()

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

*	[github.com/.../dotfiles/.../python-05-fork.py][dotfiles-python-05-fork]

{% highlight python %}
#!/usr/bin/env python3

# https://gist.github.com/waylan/2353749

import datetime
import time
import subprocess
import os


def get_dzen2_parameters():
    xpos    = '0'
    ypos    = '0'
    width   = '640'
    height  = '24'
    fgcolor = '#000000'
    bgcolor = '#ffffff'
    font    = '-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'

    parameters  = '  -x '+xpos+' -y '+ypos+' -w '+width+' -h '+ height
    parameters += " -fn '"+font+"'"
    parameters += " -ta c -bg '"+bgcolor+"' -fg '"+fgcolor+"'"
    parameters += ' -title-name dzentop'

    return parameters;

def generated_output(process):
    timeformat = '{0:%Y-%m-%d %H:%M:%S}'

    while True:
        datestr = timeformat.format(datetime.datetime.now())
    
        process.stdin.write(datestr + '\n')
        process.stdin.flush()
    
        time.sleep(1)

def run_dzen2():
    cmdout  = 'dzen2 '+get_dzen2_parameters()

    pipeout = subprocess.Popen(
            [cmdout], 
            stdin  = subprocess.PIPE,
            shell  = True,
            universal_newlines=True
        )

    generated_output(pipeout)

    pipeout.stdin.close()
    
    # avoid zombie apocalypse
    pipeout.wait()

def detach_dzen2():
    pid = os.fork()
    
    if pid == 0:
        try:
            run_dzen2()
        finally:
            os.kill(pid, signal.SIGTERM)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
os.system('pkill dzen2')

detach_dzen2()
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

*	[github.com/.../dotfiles/.../python-07-conky.py][dotfiles-python-07-conky]

{% highlight python %}
#!/usr/bin/env python3

import datetime
import time
import subprocess
import os


def get_dzen2_parameters():
    xpos    = '0'
    ypos    = '0'
    width   = '640'
    height  = '24'
    fgcolor = '#000000'
    bgcolor = '#ffffff'
    font    = '-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'

    parameters  = '  -x '+xpos+' -y '+ypos+' -w '+width+' -h '+ height
    parameters += " -fn '"+font+"'"
    parameters += " -ta c -bg '"+bgcolor+"' -fg '"+fgcolor+"'"
    parameters += ' -title-name dzentop'

    return parameters;

def generated_output(pipeout):
    dirname = os.path.dirname(os.path.abspath(__file__))
    path    = dirname + "/../assets"
    cmdin   = 'conky -c ' + path + '/conky.lua'

    pipein = subprocess.Popen(
            [cmdin], 
            stdout = pipeout.stdin,
            stderr = subprocess.STDOUT,
            shell  = True,
            universal_newlines = True
        )

def run_dzen2():
    cmdout  = 'dzen2 '+get_dzen2_parameters()

    pipeout = subprocess.Popen(
            [cmdout], 
            stdin  = subprocess.PIPE,
            shell  = True,
            universal_newlines=True
        )

    generated_output(pipeout)

    pipeout.stdin.close()
    outputs, errors = pipeout.communicate()
    
    # avoid zombie apocalypse
    pipeout.wait()

def detach_dzen2():
    pid = os.fork()
    
    if pid == 0:
        try:
            run_dzen2()
            os._exit(1)
        finally:
            os.kill(pid, signal.SIGTERM)

def detach_transset():
    pid = os.fork()
    
    if pid == 0:
        try:
            time.sleep(1)
            os.system('transset .8 -n dzentop >/dev/null 2')
            os._exit(1)
        finally:
            os.kill(pid, signal.SIGTERM)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
os.system('pkill dzen2')

# run process in the background
detach_dzen2()

# optional transparency
detach_transset()
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

[dotfiles-python-01-basic-date]: {{ dotfiles_path }}/python/python-01-basic-date.py
[dotfiles-python-01-basic-time]: {{ dotfiles_path }}/python/python-01-basic-time.py
[dotfiles-python-02-popen]:      {{ dotfiles_path }}/python/python-02-popen.py
[dotfiles-python-02-subprocess]: {{ dotfiles_path }}/python/python-02-subprocess-open.py
[dotfiles-python-02-system]:     {{ dotfiles_path }}/python/python-02-system-shell.py
[dotfiles-python-03-subprocess]: {{ dotfiles_path }}/python/python-03-subprocess-simple.py
[dotfiles-python-03-try-except]: {{ dotfiles_path }}/python/python-03-subprocess-try-except.py
[dotfiles-python-05-fork]:       {{ dotfiles_path }}/python/python-05-fork-def.py
[dotfiles-python-07-conky]:      {{ dotfiles_path }}/python/python-07-fork-conky.py

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-python.png

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
