---
layout: post
title:  "Piping and Forking in Python"
categories: code
date:   2017-04-17 17:35:15 +0700
tags: [coding, conky, perl]
author: epsi

excerpt:
  How to be a Python Plumber.
  
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

### Python Plumber.

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky..

Before you dip your toe to scripting,
you might desire to know the reason by reading this overview.

**Reading**

*	[Piping and Forking in Linux Script][local-overview]

-- -- --

{% include post/2017/04/pipe-and-fork-language.md %}

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

*	[gitlab.com/.../dotfiles/.../python-01-basic-date.py][dotfiles-python-01-basic-date]

*	[gitlab.com/.../dotfiles/.../python-01-basic-time.py][dotfiles-python-01-basic-time]

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

### Spawning Using System Shell

Using <code>os.system</code> shell is simple and straightforward.
But it does not have any ability, to stream internal function process,
that required later on this article.

I haven't explore <code>os.popen</code>,
since we are going to use <code>subprocess.Popen</code> anyway.

**Source**:

*	[gitlab.com/.../dotfiles/.../python-02-system.py][dotfiles-python-02-system]

*	[gitlab.com/.../dotfiles/.../python-02-popen.py][dotfiles-python-02-popen]

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

{% include post/2017/04/pipe-and-fork-similar-02-system.md %}

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

*	[gitlab.com/.../dotfiles/.../python-02-subprocess.py][dotfiles-python-02-subprocess]

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

{% include post/2017/04/pipe-and-fork-similar-02.md %}

-- -- --

### How does it works ?

First process create a new stdout handle <code>pipein.stdout</code>.
And the second process use it as feed to <code>pipeout.stdin</code>.
<code>pipeout.communicate()</code> manage both.

{% highlight python %}
pipein = subprocess.Popen(
...
        stdout = subprocess.PIPE, 
    )

pipeout = subprocess.Popen(
...
        stdin  = pipein.stdout,
    )
{% endhighlight %}

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

*	[gitlab.com/.../dotfiles/.../python-03-subprocess.py][dotfiles-python-03-subprocess]

*	[gitlab.com/.../dotfiles/.../python-03-try-except.py][dotfiles-python-03-try-except]

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

{% include post/2017/04/pipe-and-fork-similar-03.md %}

-- -- --

### How does it works ?

The same as previous.
But instead of reading from <code>pipein.stdout</code>,
it is managed by internal process using <code>process.stdin.write()</code>.

{% highlight python %}
    process.stdin.write(datestr + '\n')
    process.stdin.flush()
{% endhighlight %}

-- -- --

### Fork Overview

This step use internal function as source feed,
as continuation of previous step.
To avoid complexity of longer script,
most code is written inside function


This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[gitlab.com/.../dotfiles/.../python-05-fork.py][dotfiles-python-05-fork]

{% highlight python %}
#!/usr/bin/env python3

# https://gist.github.com/waylan/2353749

import datetime
import time
import subprocess
import os
import signal

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

{% include post/2017/04/pipe-and-fork-similar-05.md %}

-- -- --

### How does it works ?

Any code after the <code>os.fork</code> executed in both parent and child.
The child process has been detached from parent process.
The only different is the <code>$pid</code>.

{% highlight python %}
def detach_dzen2():
    pid = os.fork()
    
    if pid == 0:
        run_dzen2()
{% endhighlight %}

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[gitlab.com/.../dotfiles/.../python-07-conky.py][dotfiles-python-07-conky]

{% highlight python %}
#!/usr/bin/env python3

import datetime
import time
import subprocess
import os
import signal

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
            os.system('transset .8 -n dzentop >/dev/null')
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
*	[gitlab.com/.../dotfiles/.../python-17-conky.py][dotfiles-python-17-conky]

{% include post/2017/04/pipe-and-fork-similar-17.md %}

-- -- --

### Coming up Next

There already an advance case of Pipe and Fork.
Multitier, and bidirectional.

*	[HerbstluftWM Event Idle in Python][local-python-idle]

-- -- --

There above are some simple codes I put together. 
I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua
[local-python-idle]:   {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html

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

[local-overview]: {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
