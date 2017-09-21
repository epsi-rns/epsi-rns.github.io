---
layout: post
title:  "HerbstluftWM Idle Event in BASH"
date:   2017-06-12 17:35:15 +0700
categories: desktop
tags: [coding, bash, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Event Idle using Dzen2 or Lemonbar.
  Modularized implementation in BASH script.
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

*	[Modularized HerbstluftWM in BASH][local-bash-config]

*	[Piping and Forking in BASH][local-bash-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/][dotfiles-dzen2-bash]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/][dotfiles-lemon-bash]

#### The PipeHandler Source File:

Let's have a look at <code class="code-file">pipehandler.sh</code> in github.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/pipehandler.sh][dotfiles-dzen2-bash-pipehandler]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/pipehandler.sh][dotfiles-lemon-bash-pipehandler]

-- -- --

{% include post/2017/06/herbstlustwm-event-idle-language.md %}

-- -- --

### Statusbar Screenshot

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Without Idle event

Let's have a look at our main 
<code class="code-file">panel.sh</code> in github.
At the end of the script, we finally call lemonbar
with <code>detach_lemon</code> function.

{% highlight bash %}
# remove all lemonbar instance
pkill lemonbar

# run process in the background
detach_lemon $monitor $lemon_parameters
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/panel.sh][dotfiles-dzen2-bash-panel]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/panel.sh][dotfiles-lemon-bash-panel]


#### Run Lemon, Run !

This <code>detach_lemon</code> function.
is just a function that enable 
the lemonbar running process to be detached,
using fork <code>&</code>.

{% highlight bash %}
function detach_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    run_lemon $monitor $parameters &
}
{% endhighlight %}

The real function is <code>run_lemon</code>.

{% highlight bash %}
function run_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    command_out="lemonbar $parameters -p"
    
    {
       content_init $monitor
    } | $command_out
}
{% endhighlight %}

Note: that we want to ignore idle event for a while.
And append the <code>-p</code> for a while,
to make the statusbar persistent.

#### Statusbar Initialization

Here we have the <code>content_init</code>.
It is just an initialization of global variable.
We are going to have some loop later in different function,
to do the real works.

{% highlight bash %}
function content_init() {
    monitor=$1

    set_tag_value $monitor
    set_windowtitle ''

    get_statusbar_text $monitor
    echo $buffer
}
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple version. No idle event. Only statusbar initialization.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/pipehandler.01-init.sh][dotfiles-lemon-bash-pipehandler-init]

{% include post/2017/06/herbstlustwm-event-idle-pipehandler.md %}

-- -- --

### With Idle event

Consider this <code>content_walk</code> call,
after <code>content_init</code> call,
inside the <code>run_lemon</code>.

{% highlight bash %}
function run_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    command_out="lemonbar $parameters"
    
    {
       content_init $monitor
       content_walk $monitor # loop for each event
    } | $command_out
}
{% endhighlight %}

#### Wrapping Idle Event into Code

<code>content_walk</code> is the **heart** of this script.
We have to capture every event,
and process the event in event handler.

	Walk step by step, Process event by event

After the event handler,
we will get the statusbar text, in the same way,
we did in <code>content_init</code>.

{% highlight bash %}
function content_walk() {
    monitor=$1

    # start a pipe
    command_in='herbstclient --idle'

    # wait for each event     
    $command_in | while read event; do
        handle_command_event $monitor "$event"
        
        get_statusbar_text $monitor
        echo $buffer
    done    
}
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

{% highlight bash %}
function handle_command_event() {
    local monitor=$1
    shift
    local event=$@    
    
    # find out event origin
    IFS=$'\t' column=($event);
    origin=${column[0]}
    
    # find out event origin
    case $origin in
        reload)
            pkill lemonbar
            ;;
        quit_panel)
            exit
            ;;
        tag*)
            set_tag_value $monitor
            ;;
        focus_changed|window_title_changed)            
            [[ ${#column[@]} > 2 ]] && title=${column[2]} || title=''
            set_windowtitle "$title"
            ;;
    esac 
}
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

#### View Source File:

With idle event. The **heart** of the script.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/pipehandler.02-idle.sh][dotfiles-lemon-bash-pipehandler-idle]

-- -- --

### Lemonbar Clickable Areas

This is specific issue for lemonbar,
that we don't have in dzen2.

Consider have a look at 
<code class="code-file">output.sh</code>.

{% highlight bash %}
    # clickable tags
    local text_name=''
    text_name+="%{A:herbstclient focus_monitor \"$monitor\" && "
    text_name+="herbstclient use \"$tag_index\":} $tag_name %{A} "
{% endhighlight %}

**Issue**: Lemonbar put the output on terminal
instead of executing the command.

![HerbstluftWM: Tag Status][image-hlwm-05-clickable]{: .img-responsive }

Consider going back to
<code class="code-file">pipehandler.sh</code>.

We need to pipe the lemonbar output to shell.
It means Lemonbar read input and write output at the same time.

{% highlight bash %}
function run_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    command_out="lemonbar $parameters"
    
    {
       content_init $monitor
       content_walk $monitor # loop for each event
    } | $command_out | sh
}
{% endhighlight %}

#### How does it work ?

All we need to do is to pipe the output to <code>sh</code>.
And voilla !! The magic happened.
Now we have real clickable areas.

#### View Source File:

Piping lemonbar output to shell, implementing lemonbar clickable area.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/pipehandler.03-clickable.sh][dotfiles-lemon-bash-pipehandler-clickable]

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

*	[github.com/.../dotfiles/.../bash/11-testevents.sh][dotfiles-lemon-bash-testevents]

-- -- --

### Combined Event

#### Preparing The View

This is what it looks like, an overview of what we want to achieve.

![Statusbar: Event Screenshot][image-hlwm-ss-event]{: .img-responsive }

Consider make a progress in 
<code class="code-file">output.sh</code>.

{% highlight bash %}
segment_datetime='';    # empty string

function get_statusbar_text() {
    ...

    # draw date and time
    text+='%{c}'
    output_by_datetime
    text+=$buffer

    ...
}


function output_by_datetime() {
    buffer=$segment_datetime
}

function set_datetime() {
    ...

    segment_datetime="$date_text  $time_text"
}
{% endhighlight %}

And a few enhancement in 
<code class="code-file">pipehandler.sh</code>.

{% highlight bash %}
function handle_command_event() {
    ...

    case $origin in
        ...
        interval)
            set_datetime
            ;;
    esac 
}

function content_init() {
    ...
    set_windowtitle ''
    set_datetime

    ...
}
{% endhighlight %}

#### Expanding The Event Controller

All we need to do is to split out <code>content_walk</code> into

*	<code>content_walk</code>: combined event.

*	<code>content_event_idle</code>: HerbstluftWM idle event. 
	Forked, as background processing.

*	<code>content_event_interval</code> : Custom date time event. 
	Forked, as background processing.

{% highlight bash %}
function content_event_idle() {
    # wait for each event     
    herbstclient --idle
}
{% endhighlight %}

{% highlight bash %}
function content_event_interval() {
    # endless loop
    while :; do 
      echo "interval"
      sleep 1
    done
}
{% endhighlight %}

{% highlight bash %}
function content_walk() {
    monitor=$1
    
    {
        content_event_idle &
        pid_idle=$!
    
        content_event_interval &
        pid_interval=$!
    
    }  | while read event; do
            handle_command_event $monitor "$event"
        
            get_statusbar_text $monitor
            echo $buffer
        done
}
{% endhighlight %}

This above is the most complex part.
We are almost done.

#### View Source File:

Combined event consist of both,
synchronous interval event and asynchronous idle event.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/pipehandler.04-event.sh][dotfiles-lemon-bash-pipehandler-event]

-- -- --

### Dual Bar

The idea of this article comes from the fact
that <code>herbsclient --idle</code> is asynchronous event.
If you need another bar, just simply use <code>Conky</code>  instead.

*	**Dzen2**: 
	![HerbstluftWM: Dzen2 Conky][image-hlwm-ss-dzen2-conky]{: .img-responsive }

*	**Lemonbar**: 
	![HerbstluftWM: Lemonbar Conky][image-hlwm-ss-lemon-conky]{: .img-responsive }

We only need one function to do this in
<code class="code-file">pipehandler.pm</code>.

{% highlight perl %}
function detach_lemon_conky() {    
    parameters=$@

    command_out="lemonbar $parameters"
    
    {
        dirname=$(dirname $(readlink -f "$0"))
        path="$dirname/../conky"
        conky -c "$path/conky.lua"
    } | $command_out &
}
{% endhighlight %}

And execute the function main script in
<code class="code-file">panel.pl</code>.

{% highlight perl %}
#!/usr/bin/env bash

# libraries

DIR=$(dirname "$0")

. ${DIR}/gmc.sh
. ${DIR}/helper.sh
. ${DIR}/output.sh
. ${DIR}/pipehandler.sh

# main

panel_height=24
get_monitor ${@}

pkill lemonbar
herbstclient pad $monitor $panel_height 0 $panel_height 0

# run process in the background

get_params_top $monitor $panel_height
detach_lemon $monitor $lemon_parameters

get_params_bottom $monitor $panel_height
detach_lemon_conky $lemon_parameters
{% endhighlight %}

#### View Source File:

Dual Bar, <code>detach_lemon_conky</code> function.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/pipehandler.05-conky.sh][dotfiles-lemon-bash-pipehandler-conky]

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

{% highlight bash %}
function kill_zombie() {
    pkill -x dzen2
    pkill -x lemonbar
    pkill -x cat
    pkill conky
    pkill herbstclient
}
{% endhighlight %}

-- -- --

### Putting Them All Together

I also created compact for version,
for use with main HerbstluftWM configuration,
in <code class="code-file">~/.config/herbstluftwm/</code> directory.
After reunification, they are not very long scripts after all.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/panel-dzen2.sh][dotfiles-hlwm-bash-dzen2-compact]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/panel-lemonbar.sh][dotfiles-hlwm-bash-lemon-compact]

{% include post/2017/06/herbstlustwm-event-idle-panel.md %}

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
{% assign dotfiles_hlwm  = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

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

[dotfiles-lemon-bash-testevents]:  {{ dotfiles_lemon }}/bash/11-testevents.sh
[dotfiles-hlwm-bash-dzen2-compact]: {{ dotfiles_hlwm }}/bash/panel-dzen2.sh
[dotfiles-hlwm-bash-lemon-compact]: {{ dotfiles_hlwm }}/bash/panel-lemonbar.sh

[local-bash-config]: {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-bash-pipe]:   {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html

[dotfiles-lemon-bash-pipehandler-init]:      {{ dotfiles_lemon }}/bash/pipehandler.01-init.sh
[dotfiles-lemon-bash-pipehandler-idle]:      {{ dotfiles_lemon }}/bash/pipehandler.02-idle.sh
[dotfiles-lemon-bash-pipehandler-clickable]: {{ dotfiles_lemon }}/bash/pipehandler.03-clickable.sh
[dotfiles-lemon-bash-pipehandler-event]:     {{ dotfiles_lemon }}/bash/pipehandler.04-event.sh
[dotfiles-lemon-bash-pipehandler-conky]:     {{ dotfiles_lemon }}/bash/pipehandler.05-conky.sh

[dotfiles-dzen2-bash-panel]:       {{ dotfiles_dzen2 }}/bash/panel.sh
[dotfiles-dzen2-bash-helper]:      {{ dotfiles_dzen2 }}/bash/helper.sh
[dotfiles-dzen2-bash-output]:      {{ dotfiles_dzen2 }}/bash/output.sh
[dotfiles-dzen2-bash-pipehandler]: {{ dotfiles_dzen2 }}/bash/pipehandler.sh

[dotfiles-lemon-bash-panel]:       {{ dotfiles_lemon }}/bash/panel.sh
[dotfiles-lemon-bash-helper]:      {{ dotfiles_lemon }}/bash/helper.sh
[dotfiles-lemon-bash-output]:      {{ dotfiles_lemon }}/bash/output.sh
[dotfiles-lemon-bash-pipehandler]: {{ dotfiles_lemon }}/bash/pipehandler.sh
