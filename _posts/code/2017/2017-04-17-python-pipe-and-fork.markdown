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

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky..

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

There above are some simple codes I put together. 
I’m mostly posting codes so I won’t have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

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
