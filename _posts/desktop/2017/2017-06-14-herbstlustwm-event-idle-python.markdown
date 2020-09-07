---
layout     : post
title      : "HerbstluftWM Idle Event in Python"
date       : 2017-06-14 17:35:15 +0700
categories : desktop
tags       : [coding, python, herbstluftwm, statusbar]
keywords   : [event idle, lemonbar, dzen2]
author     : epsi
toc        : toc/2017/06/herbstlustwm-idle-event.html

opengraph:
  image: /assets/site/images/topics/python.png

excerpt:
  HersbtluftWM Event Idle using Dzen2 or Lemonbar.
  Modularized implementation in Python script.
  An advance case of Pipe and Fork.

related_link_ids: 
  - 17061135  # Event Idle Overview
  - 17061235  # Event Idle BASH
  - 17061335  # Event Idle Perl
  - 17061435  # Event Idle Python
  - 17061535  # Event Idle Ruby
  - 17061635  # Event Idle PHP
  - 17061735  # Event Idle Lua
  - 17061835  # Event Idle Haskell

---

<a name="preface"></a>

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

*	[Modularized HerbstluftWM in Python][local-python-config]

*	[Piping and Forking in Python][local-python-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/][dotfiles-dzen2-python]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/][dotfiles-lemon-python]

#### The PipeHandler Source File:

Let's have a look at <code class="code-file">pipehandler.py</code> in github.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/pipehandler.py][dotfiles-dzen2-python-pipehandler]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/pipehandler.py][dotfiles-lemon-python-pipehandler]

-- -- --

### Statusbar Screenshot

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Without Idle event

Let's have a look at our main 
<code class="code-file">panel.py</code> in github.
At the end of the script, we finally call lemonbar
with <code>detach_lemon</code> function.

{% highlight python %}
# remove all lemonbar instance
os.system('pkill lemonbar')

# run process in the background
pipehandler.detach_lemon(monitor, lemon_parameters)
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/panel.py][dotfiles-dzen2-python-panel]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/panel.py][dotfiles-lemon-python-panel]


#### Run Lemon, Run !

This <code>detach_lemon</code> function.
is just a function that enable 
the lemonbar running process to be detached,
using <code>pid = os.fork()</code>.

{% highlight python %}
def detach_lemon(monitor, parameters):
    pid_lemon = os.fork()
    
    if pid_lemon == 0:
        try:
            run_lemon(monitor, parameters)
            os._exit(1)
        finally:
            import signal
            os.kill(pid_lemon, signal.SIGTERM)
{% endhighlight %}

The real function is <code>run_lemon</code>.
You must be familiar with this <code>subprocess.Popen</code>.
It is very flexible, and support bidirectional pipe that we need later.

{% highlight python %}
def run_lemon(monitor, parameters):  
    command_out  = 'lemonbar ' + parameters + ' -p'

    pipe_lemon_out = subprocess.Popen(
            [command_out], 
            stdin  = subprocess.PIPE, # for use with content processing
            shell  = True,
            universal_newlines=True
        )

    content_init(monitor, pipe_lemon_out)
    pipe_lemon_out.stdin.close()
{% endhighlight %}

Note: that we want to ignore idle event for a while.
And append the <code>-p</code> for a while,
to make the statusbar persistent.

#### Statusbar Initialization

Here we have the <code>content_init</code>.
It is just an initialization of global variable.
We are going to have some loop later in different function,
to do the real works.

{% highlight python %}
def content_init(monitor, pipe_lemon_out):
    output.set_tag_value(monitor)
    output.set_windowtitle('')
        
    text = output.get_statusbar_text(monitor)
    pipe_lemon_out.stdin.write(text + '\n')
    pipe_lemon_out.stdin.flush()
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple version. No idle event. Only statusbar initialization.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/pipehandler.01-init.py][dotfiles-lemon-python-pipehandler-init]

{% include toc/2017/06/herbstlustwm-event-idle-pipehandler.html %}

-- -- --

### With Idle event

Consider this <code>content_walk</code> call,
after <code>content_init</code> call,
inside the <code>run_lemon</code>.

{% highlight python %}
def run_lemon(monitor, parameters):  
    command_out  = 'lemonbar ' + parameters

    pipe_lemon_out = subprocess.Popen(
            [command_out], 
            stdin  = subprocess.PIPE, # for use with content processing
            shell  = True,
            universal_newlines=True
        )

    content_init(monitor, pipe_lemon_out)
    content_walk(monitor, pipe_lemon_out) # loop for each event

    pipe_lemon_out.stdin.close()
{% endhighlight %}

#### Wrapping Idle Event into Code

<code>content_walk</code> is the **heart** of this script.
We have to capture every event,
and process the event in event handler.

	Walk step by step, Process event by event

After the event handler,
we will get the statusbar text, in the same way,
we did in <code>content_init</code>.

{% highlight python %}
def content_walk(monitor, pipe_lemon_out):    
    # start a pipe
    command_in = 'herbstclient --idle'  
    pipe_idle_in = subprocess.Popen(
            [command_in], 
            stdout = subprocess.PIPE,
            stderr = subprocess.STDOUT,
            shell  = True,
            universal_newlines = True
        )
    
    # wait for each event, trim newline
    for event in pipe_cat.stdout:
        handle_command_event(monitor, event.strip())
        
        text = output.get_statusbar_text(monitor)
        pipe_lemon_out.stdin.write(text + '\n')
        pipe_lemon_out.stdin.flush()
    
    pipe_idle_in.stdout.close()
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

{% highlight python %}
def handle_command_event(monitor, event):  
    # find out event origin
    column = event.split("\t")
    origin = column[0]
    
    tag_cmds = ['tag_changed', 'tag_flags', 'tag_added', 'tag_removed']
    title_cmds = ['window_title_changed', 'focus_changed']

    if origin == 'reload':
        os.system('pkill lemonbar')
    elif origin == 'quit_panel':
        exit()
    elif origin in tag_cmds:
        output.set_tag_value(monitor)
    elif origin in title_cmds:
        title = column[2] if (len(column) > 2) else ''
        output.set_windowtitle(title)
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

#### View Source File:

With idle event. The **heart** of the script.

*	**Lemonbar**py
	[gitlab.com/.../dotfiles/.../python/pipehandler.02-idle.sh][dotfiles-lemon-python-pipehandler-idle]

-- -- --

### Lemonbar Clickable Areas

This is specific issue for lemonbar,
that we don't have in dzen2.

Consider have a look at 
<code class="code-file">output.py</code>.

{% highlight python %}
    # clickable tags
    text_name = '%{A:herbstclient focus_monitor "' \
              + str(monitor) + '" && ' + 'herbstclient use "' \
              + tag_index + '":} ' + tag_name + ' %{A} '
{% endhighlight %}

**Issue**: Lemonbar put the output on terminal
instead of executing the command.

![HerbstluftWM: Tag Status][image-hlwm-05-clickable]{: .img-responsive }

Consider going back to
<code class="code-file">pipehandler.py</code>.

We need to pipe the lemonbar output to shell.
It means Lemonbar read input and write output at the same time.

{% highlight python %}
def run_lemon(monitor, parameters):  
    command_out  = 'lemonbar ' + parameters

    pipe_lemon_out = subprocess.Popen(
            [command_out], 
            stdout = subprocess.PIPE, # for use with shell, note this
            stdin  = subprocess.PIPE, # for use with content processing
            shell  = True,
            universal_newlines=True
        )
    
    pipe_sh = subprocess.Popen(
            ['sh'], 
            stdin  = pipe_lemon_out.stdout,
            shell  = True,
            universal_newlines=True
        )

    content_init(monitor, pipe_lemon_out)
    content_walk(monitor, pipe_lemon_out) # loop for each event

    pipe_lemon_out.stdin.close()
    pipe_lemon_out.stdout.close()
{% endhighlight %}

#### How does it work ?

Just pay attention to these lines.
We are using <code>pipe_lemon_out.stdout</code>
as a feed to <code>pipe_sh.stdin</code>.

{% highlight python %}
    pipe_lemon_out = subprocess.Popen(
            stdout = subprocess.PIPE, # for use with shell, note this
            stdin  = subprocess.PIPE, # for use with content processing
        )

    pipe_sh = subprocess.Popen(
            stdin  = pipe_out.stdout,
        )
{% endhighlight %}

#### View Source File:

Piping lemonbar output to shell, implementing lemonbar clickable area.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/pipehandler.03-clickable.py][dotfiles-lemon-python-pipehandler-clickable]

-- -- --

### Interval Based Event

We can put custom event other than idle event in statusbar panel.
This event, such as date event, called based on time interval in second.
Luckily we can treat interval as event.

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

*	[gitlab.com/.../dotfiles/.../python/11-testevents.py][dotfiles-lemon-python-testevents]

-- -- --

### Combined Event

#### Preparing The View

This is what it looks like, an overview of what we want to achieve.

![Statusbar: Event Screenshot][image-hlwm-ss-event]{: .img-responsive }

Consider make a progress in 
<code class="code-file">output.py</code>.

{% highlight python %}
segment_datetime    = '' # empty string

def get_statusbar_text(monitor):
    ...

    # draw date and time
    text += '%{c}'
    text += output_by_datetime()

    ...

def output_by_datetime():
    return segment_datetime

def set_datetime():
    ...
    segment_datetime = date_text + '  ' + time_text
{% endhighlight %}

And a few enhancement in 
<code class="code-file">pipehandler.py</code>.

{% highlight python %}
def handle_command_event(monitor, event):  
    ...

    if origin == 'reload':
    ...
    elif origin == 'interval':
        output.set_datetime()

def content_init(monitor, pipe_lemon_out):
    ...
    output.set_windowtitle('')
    output.set_datetime()
        
    ...
{% endhighlight %}

#### Expanding The Event Controller

All we need to do is to split out <code>content_walk</code> into

*	<code>content_walk</code>: combined event,
	with the help of <code>cat</code> process.

*	<code>content_event_idle</code>: HerbstluftWM idle event. 
	Forked, as background processing.

*	<code>content_event_interval</code> : Custom date time event. 
	Forked, as background processing.

{% highlight python %}
def content_event_idle(pipe_cat_out):
    pid_idle = os.fork()
        
    if pid_idle == 0:
        try:
            # start a pipe
            command_in = 'herbstclient --idle'  
            pipe_idle_in = subprocess.Popen(
                    [command_in], 
                    stdout = subprocess.PIPE,
                    stderr = subprocess.STDOUT,
                    shell  = True,
                    universal_newlines = True
            )
            
            # wait for each event  
            for event in pipe_idle_in.stdout: 
                pipe_cat_out.stdin.write(event)
                pipe_cat_out.stdin.flush()
    
            pipe_idle_in.stdout.close()
        finally:
            import signal
            os.kill(pid_idle, signal.SIGTERM)
{% endhighlight %}

{% highlight python %}
def content_event_interval(pipe_cat_out):
    pid_interval = os.fork()

    if pid_interval == 0:
        try:
            while True:
                pipe_cat_out.stdin.write("interval\n")
                pipe_cat_out.stdin.flush()
    
                time.sleep(1)
        finally:
            import signal
            os.kill(pid_interval, signal.SIGTERM)
{% endhighlight %}

{% highlight python %}
def content_walk(monitor, pipe_lemon_out): 
    pipe_cat = subprocess.Popen(
            ['cat'], 
            stdin  = subprocess.PIPE,
            stdout = subprocess.PIPE,
            shell  = True,
            universal_newlines=True
        )

    content_event_idle(pipe_cat)
    content_event_interval(pipe_cat)

    # wait for each event, trim newline
    for event in pipe_cat.stdout:
        handle_command_event(monitor, event.strip())

        text = output.get_statusbar_text(monitor)
        pipe_lemon_out.stdin.write(text + '\n')
        pipe_lemon_out.stdin.flush()

    pipe_cat.stdin.close()
    pipe_cat.stdout.close()
{% endhighlight %}

This above is the most complex part.
We are almost done.

#### View Source File:

Combined event consist of both,
synchronous interval event and asynchronous idle event.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/pipehandler.04-event.py][dotfiles-lemon-python-pipehandler-event]

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
def detach_lemon_conky(parameters):
    pid_conky = os.fork()
    
    if pid_conky == 0:
        try:
            dirname  = os.path.dirname(os.path.abspath(__file__))
            path     = dirname + "/../conky"
            cmd_in   = 'conky -c ' + path + '/conky.lua'

            cmd_out  = 'lemonbar ' + parameters

            pipe_out = subprocess.Popen(
                    [cmd_out], 
                    stdin  = subprocess.PIPE,
                    shell  = True,
                    universal_newlines=True
                )

            pipe_in  = subprocess.Popen(
                    [cmd_in], 
                    stdout = pipe_out.stdin,
                    stderr = subprocess.STDOUT,
                    shell  = True,
                    universal_newlines = True
                )

            pipe_out.stdin.close()
            outputs, errors = pipe_out.communicate()
    
            # avoid zombie apocalypse
            pipe_out.wait()

            os._exit(1)
        finally:
            import signal
            os.kill(pid_conky, signal.SIGTERM)
{% endhighlight %}

And execute the function main script in
<code class="code-file">panel.pl</code>.

{% highlight perl %}
#!/usr/bin/env python3

import os
import sys

import helper
import pipehandler

# main

panel_height = 24
monitor = helper.get_monitor(sys.argv)

os.system('pkill lemonbar')
os.system('herbstclient pad ' + str(monitor) + ' ' 
    + str(panel_height) + ' 0 ' + str(panel_height) + ' 0')

# run process in the background

params_top    = helper.get_params_top(monitor, panel_height)
pipehandler.detach_lemon(monitor, params_top)

params_bottom = helper.get_params_bottom(monitor, panel_height)
pipehandler.detach_lemon_conky(params_bottom)
{% endhighlight %}

#### View Source File:

Dual Bar, <code>detach_lemon_conky</code> function.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/pipehandler.05-conky.py][dotfiles-lemon-python-pipehandler-conky]

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

{% highlight python %}
def kill_zombie():
    os.system('pkill -x dzen2')
    os.system('pkill -x lemonbar')
    os.system('pkill -x cat')
    os.system('pkill conky')
    os.system('pkill herbstclient')
{% endhighlight %}

-- -- --

### Putting Them All Together

I also created compact for version,
for use with main HerbstluftWM configuration,
in <code class="code-file">~/.config/herbstluftwm/</code> directory.
After reunification, they are not very long scripts after all.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/panel-dzen2.py][dotfiles-hlwm-python-dzen2-compact]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/panel-lemonbar.py][dotfiles-hlwm-python-lemon-compact]

{% include toc/2017/06/herbstlustwm-event-idle-panel.html %}

-- -- --

#### Desktop Screenshot

Fullscreen, Dual Panel, Zero Gap.

[![HerbstluftWM: Screenshot Dual Panel][image-ss-hlwm-dualpanel]{: .img-responsive }][photo-ss-hlwm-dualpanel]

-- -- --

<a name="conclusion"></a>

Enjoy the statusbar !
Enjoy the window manager !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}
{% assign dotfiles_hlwm  = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

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

[dotfiles-lemon-python-testevents]:  {{ dotfiles_lemon }}/python/11-testevents.py
[dotfiles-hlwm-python-dzen2-compact]: {{ dotfiles_hlwm }}/python/panel-dzen2.py
[dotfiles-hlwm-python-lemon-compact]: {{ dotfiles_hlwm }}/python/panel-lemonbar.py

[local-python-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-python.html
[local-python-pipe]:   {{ site.url }}/code/2017/04/16/python-pipe-and-fork.html

[local-bash-config]: {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-bash-pipe]:   {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html

[dotfiles-lemon-python-pipehandler-init]:      {{ dotfiles_lemon }}/python/pipehandler.01-init.py
[dotfiles-lemon-python-pipehandler-idle]:      {{ dotfiles_lemon }}/python/pipehandler.02-idle.py
[dotfiles-lemon-python-pipehandler-clickable]: {{ dotfiles_lemon }}/python/pipehandler.03-clickable.py
[dotfiles-lemon-python-pipehandler-event]:     {{ dotfiles_lemon }}/python/pipehandler.04-event.py
[dotfiles-lemon-python-pipehandler-conky]:     {{ dotfiles_lemon }}/python/pipehandler.05-conky.py

[dotfiles-dzen2-python-panel]:       {{ dotfiles_dzen2 }}/python/panel.py
[dotfiles-dzen2-python-helper]:      {{ dotfiles_dzen2 }}/python/helper.py
[dotfiles-dzen2-python-output]:      {{ dotfiles_dzen2 }}/python/output.py
[dotfiles-dzen2-python-pipehandler]: {{ dotfiles_dzen2 }}/python/pipehandler.py

[dotfiles-lemon-python-panel]:       {{ dotfiles_lemon }}/python/panel.py
[dotfiles-lemon-python-helper]:      {{ dotfiles_lemon }}/python/helper.py
[dotfiles-lemon-python-output]:      {{ dotfiles_lemon }}/python/output.py
[dotfiles-lemon-python-pipehandler]: {{ dotfiles_lemon }}/python/pipehandler.py

