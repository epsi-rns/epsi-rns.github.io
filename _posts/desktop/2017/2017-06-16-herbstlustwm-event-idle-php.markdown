---
layout: post
title:  "HerbstluftWM Idle Event in PHP"
date:   2017-06-16 17:35:15 +0700
categories: desktop
tags: [coding, php, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Event Idle using Dzen2 or Lemonbar.
  Modularized implementation in PHP script.
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

*	[Modularized HerbstluftWM in PHP][local-php-config]

*	[Piping and Forking in PHP][local-php-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../php/][dotfiles-dzen2-php]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/][dotfiles-lemon-php]

#### The PipeHandler Source File:

Let's have a look at <code class="code-file">pipehandler.php</code> in github.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../php/pipehandler.php][dotfiles-dzen2-php-pipehandler]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/pipehandler.php][dotfiles-lemon-php-pipehandler]

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
<code class="code-file">panel.php</code> in github.
At the end of the script, we finally call lemonbar
with <code>detach_lemon</code> function.

{% highlight php %}
# remove all lemonbar instance
system('pkill lemonbar')

# run process in the background
detach_lemon(monitor, lemon_parameters)
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../php/panel.php][dotfiles-dzen2-php-panel]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/panel.php][dotfiles-lemon-php-panel]


#### Run Lemon, Run !

This <code>detach_lemon</code> function.
is just a function that enable 
the lemonbar running process to be detached,
using <code>$pid = pcntl_fork();</code>.

{% highlight php %}
function detach_lemon($monitor, $parameters)
{ 
    $pid_lemon = pcntl_fork();
    
    switch($pid_lemon) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        run_lemon($monitor, $parameters); 
        break;
    default : // we are the parent             
        return $pid_lemon;
    }    
}
{% endhighlight %}

The real function is <code>run_lemon</code>.
Note that we are using <code>proc_open</code>
instead of <code>popen</code>,
to enable bidirectional pipe later.

{% highlight php %}
function run_lemon($monitor, $parameters) 
{ 
    $descriptorspec = array(
        0 => array('pipe', 'r'),  // stdin
        1 => array('pipe', 'w'),  // stdout
        2 => array('pipe', 'w',)  // stderr
    );
    
    $command_out = "lemonbar $parameters -p";
    $proc_lemon = proc_open($command_out, $descriptorspec, $pipe_lemon);
    
    content_init($monitor, $pipe_lemon[0]);
    pclose($pipe_lemon[0]);
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

{% highlight php %}
function content_init($monitor, $pipe_lemon_stdin)
{
    set_tag_value($monitor);
    set_windowtitle('');
        
    $text = get_statusbar_text($monitor);
    fwrite($pipe_lemon_stdin, $text."\n");
    flush();
}
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple version. No idle event. Only statusbar initialization.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/pipehandler.01-init.php][dotfiles-lemon-php-pipehandler-init]

{% include post/2017/06/herbstlustwm-event-idle-pipehandler.md %}

-- -- --

### With Idle event

Consider this <code>content_walk</code> call,
after <code>content_init</code> call,
inside the <code>run_lemon</code>.

{% highlight php %}
function run_lemon($monitor, $parameters) 
{ 
    $descriptorspec = array(
        0 => array('pipe', 'r'),  // stdin
        1 => array('pipe', 'w'),  // stdout
        2 => array('pipe', 'w',)  // stderr
    );
    
    $command_out = "lemonbar $parameters";
    $proc_lemon = proc_open($command_out, $descriptorspec, $pipe_lemon);
    
    content_init($monitor, $pipe_lemon[0]);
    content_walk($monitor, $pipe_lemon[0]); // loop for each event

    pclose($pipe_lemon[0]);
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
<code>popen</code> is sufficient for unidirectional pipe.

{% highlight php %}
function content_walk($monitor, $pipe_lemon_stdin)
{       
    // start a pipe
    $command_in    = 'herbstclient --idle';
    $pipe_idle_in  = popen($command_in,  'r'); // handle
    
    while(!feof($pipe_idle_in)) {
        # read next event
        $event = trim(fgets($pipe_idle_in));
        handle_command_event($monitor, $event);
        
        $text = get_statusbar_text($monitor);
        fwrite($pipe_lemon_stdin, $text."\n");
        flush();
    }
    
    pclose($pipe_idle_in);
}
{% endhighlight %}

We do not need to use <code>proc_open</code> for unidirectional pipe.

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

{% highlight php %}
function handle_command_event($monitor, $event)
{
    // find out event origin
    $column = explode("\t", $event);
    $origin = $column[0];

    switch($origin) {
    case 'reload':
        system('pkill lemonbar');
        break;
    case 'quit_panel':
        exit(1);
    case 'tag_changed':
    case 'tag_flags':
    case 'tag_added':
    case 'tag_removed':
        set_tag_value($monitor);
        break;
    case 'window_title_changed':
    case 'focus_changed':
        $title = count($column) > 2 ? $column[2] : '';
        set_windowtitle($title);
    }
}
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

#### View Source File:

With idle event. The **heart** of the script.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/pipehandler.02-idle.php][dotfiles-lemon-php-pipehandler-idle]

-- -- --

### Lemonbar Clickable Areas

This is specific issue for lemonbar,
that we don't have in dzen2.

Consider have a look at 
<code class="code-file">output.php</code>.

{% highlight php %}
    // clickable tags
    $text_name = "%{A:herbstclient focus_monitor \"${monitor}\" && " 
               . "herbstclient use \"${tag_index}\":} ${tag_name} %{A}";
{% endhighlight %}

**Issue**: Lemonbar put the output on terminal
instead of executing the command.

![HerbstluftWM: Tag Status][image-hlwm-05-clickable]{: .img-responsive }

Consider going back to
<code class="code-file">pipehandler.php</code>.

We need to pipe the lemonbar output to shell.
It means Lemonbar read input and write output at the same time.

{% highlight php %}
function run_lemon($monitor, $parameters) 
{ 
    $descriptorspec = array(
        0 => array('pipe', 'r'),  // stdin
        1 => array('pipe', 'w'),  // stdout
        2 => array('pipe', 'w',)  // stderr
    );
    
    $command_out  = "lemonbar $parameters";
    $proc_lemon = proc_open($command_out, $descriptorspec, $pipe_lemon);
    $proc_sh    = proc_open('sh', $descriptorspec, $pipe_sh);
    
    $pid_content = pcntl_fork();
    
    switch($pid_content) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        content_init($monitor, $pipe_lemon[0]);
        content_walk($monitor, $pipe_lemon[0]); // loop for each event
        break;
    default : // we are the parent
        while(!feof($pipe_lemon[1])) {
            $buffer = fgets($pipe_lemon[1]);
            fwrite($pipe_sh[0], $buffer);
        }
        return $pid_content;
    } 

    pclose($pipe_lemon[0]);
}
{% endhighlight %}

#### How does it work ?

	Forking solve this issue.

Seriously, we have to take care on where to put the loop,
without interfering the original loop in <code>content_walk</code>.

#### View Source File:

Piping lemonbar output to shell, implementing lemonbar clickable area.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/pipehandler.03-clickable.php][dotfiles-lemon-php-pipehandler-clickable]

### Interval Based Event

We can put custom event other than idle event in statusbar panel.
This event, such as date event, called based on time interval in second.

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

*	[github.com/.../dotfiles/.../php/11-testevents.php][dotfiles-lemon-php-testevents]

-- -- --

### Combined Event

#### Preparing The View

This is what it looks like, an overview of what we want to achieve.

![Statusbar: Event Screenshot][image-hlwm-ss-event]{: .img-responsive }

Consider make a progress in 
<code class="code-file">output.php</code>.

{% highlight php %}
$segment_datetime    = ''; # empty string

function get_statusbar_text($monitor)
{
    ...

    # draw date time
    $text .= '%{c}';
    $text .= output_by_datetime();

    ...
}

function output_by_datetime()
{
    global $segment_datetime; 
    return $segment_datetime;
}

function set_datetime() {
    ...

    $segment_datetime = "$date_text  $time_text";
}
{% endhighlight %}

And a few enhancement in 
<code class="code-file">pipehandler.php</code>.

{% highlight php %}
function handle_command_event($monitor, $event)
{
    ...

    switch($origin) {
    ...
    case 'interval':
        set_datetime();
    }
}

function content_init($monitor, $pipe_lemon_stdin)
{   
    ...
    set_windowtitle('');
    set_datetime();

    ...
}
{% endhighlight %}

#### Expanding The Event Controller

All we need to do is to split out <code>content_walk</code> into

*	<code>content_walk</code>: combined event,
	with the help of <code>cat</code> process.

*	<code>content_event_idle</code>: HerbstluftWM idle event. 
	Forked, as background processing.

*	<code>content_event_interval</code> : Custom date time event. 
	Forked, as background processing.

{% highlight php %}
function content_event_idle($pipe_cat_stdin) 
{
    $pid_idle = pcntl_fork();

    switch($pid_idle) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        // start a pipe
        $command_in    = 'herbstclient --idle';
        $pipe_idle_in  = popen($command_in,  'r'); // handle
    
        while(!feof($pipe_idle_in)) {
            # read next event
            $event = fgets($pipe_idle_in);
            fwrite($pipe_cat_stdin, $event);
            flush();
        }
    
        pclose($pipe_idle_in);

        break;
    default : // we are the parent
        // do nothing
        return $pid_idle;
    } 
}
{% endhighlight %}

{% highlight php %}
function content_event_interval($pipe_cat_stdin) 
{
    date_default_timezone_set("Asia/Jakarta");
    $pid_interval = pcntl_fork();

    switch($pid_interval) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        do {
            fwrite($pipe_cat_stdin, "interval\n");
            flush();
            sleep(1);
        } while (true);
        
        break;
    default : // we are the parent
        // do nothing
        return $pid_interval;
    } 
}
{% endhighlight %}

{% highlight php %}
function content_walk($monitor, $pipe_lemon_stdin)
{       
    $descriptorspec = array(
        0 => array('pipe', 'r'),  // stdin
        1 => array('pipe', 'w'),  // stdout
        2 => array('pipe', 'w',)  // stderr
    );
    
    $proc_cat = proc_open('cat', $descriptorspec, $pipe_cat);

    content_event_idle($pipe_cat[0]);
    content_event_interval($pipe_cat[0]);

    while(!feof($pipe_cat[1])) {
        $event = trim(fgets($pipe_cat[1]));
        handle_command_event($monitor, $event);
        
        $text = get_statusbar_text($monitor);
        fwrite($pipe_lemon_stdin, $text."\n");
        flush();
    }

    pclose($pipe_cat[1]);
}
{% endhighlight %}

This above is the most complex part.
We are almost done.

#### View Source File:

Combined event consist of both,
synchronous interval event and asynchronous idle event.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/pipehandler.04-event.php][dotfiles-lemon-php-pipehandler-event]

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
function detach_lemon_conky($parameters)
{ 
    $pid_conky = pcntl_fork();

    switch($pid_conky) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        $cmd_out  = 'lemonbar '.$parameters;
        $pipe_out = popen($cmd_out, "w");

        $path     = __dir__."/../conky";
        $cmd_in   = 'conky -c '.$path.'/conky.lua';
        $pipe_in  = popen($cmd_in,  "r");
    
        while(!feof($pipe_in)) {
            $buffer = fgets($pipe_in);
            fwrite($pipe_out, $buffer);
            flush();
        }
    
        pclose($pipe_in);
        pclose($pipe_out);

        break;
    default : // we are the parent             
        return $pid_conky;
    }  
}
{% endhighlight %}

And execute the function main script in
<code class="code-file">panel.pl</code>.

{% highlight perl %}
#!/usr/bin/php 
<?php # using PHP7

require_once(__DIR__.'/helper.php');
require_once(__DIR__.'/pipehandler.php');

// main

$panel_height = 24;
$monitor = get_monitor($argv);

system('pkill lemonbar');
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");

// run process in the background

$params_top = get_params_top($monitor, $panel_height);
detach_lemon($monitor, $params_top);

$params_bottom = get_params_bottom($monitor, $panel_height);
detach_lemon_conky($params_bottom);
{% endhighlight %}

#### View Source File:

Dual Bar, <code>detach_lemon_conky</code> function.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/pipehandler.05-conky.php][dotfiles-lemon-php-pipehandler-conky]

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

{% highlight php %}
function kill_zombie()
{
    system('pkill -x dzen2');
    system('pkill -x lemonbar');
    system('pkill -x cat');
    system('pkill conky');
    system('pkill herbstclient');
}
{% endhighlight %}

-- -- --

### Putting Them All Together

I also created compact for version,
for use with main HerbstluftWM configuration,
in <code class="code-file">~/.config/herbstluftwm/</code> directory.
After reunification, they are not very long scripts after all.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../php/panel-dzen2.php][dotfiles-hlwm-php-dzen2-compact]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../php/panel-lemonbar.php][dotfiles-hlwm-php-lemon-compact]

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

[dotfiles-lemon-php-testevents]:  {{ dotfiles_lemon }}/php/11-testevents.php
[dotfiles-hlwm-php-dzen2-compact]: {{ dotfiles_hlwm }}/php/panel-dzen2.php
[dotfiles-hlwm-php-lemon-compact]: {{ dotfiles_hlwm }}/php/panel-lemonbar.php

[local-php-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-php.html
[local-php-pipe]:   {{ site.url }}/code/2017/04/16/php-pipe-and-fork.html

[local-bash-config]: {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-bash-pipe]:   {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html

[dotfiles-lemon-php-pipehandler-init]:      {{ dotfiles_lemon }}/php/pipehandler.01-init.php
[dotfiles-lemon-php-pipehandler-idle]:      {{ dotfiles_lemon }}/php/pipehandler.02-idle.php
[dotfiles-lemon-php-pipehandler-clickable]: {{ dotfiles_lemon }}/php/pipehandler.03-clickable.php
[dotfiles-lemon-php-pipehandler-event]:     {{ dotfiles_lemon }}/php/pipehandler.04-event.php
[dotfiles-lemon-php-pipehandler-conky]:     {{ dotfiles_lemon }}/php/pipehandler.05-conky.php

[dotfiles-dzen2-php-panel]:       {{ dotfiles_dzen2 }}/php/panel.php
[dotfiles-dzen2-php-helper]:      {{ dotfiles_dzen2 }}/php/helper.php
[dotfiles-dzen2-php-output]:      {{ dotfiles_dzen2 }}/php/output.php
[dotfiles-dzen2-php-pipehandler]: {{ dotfiles_dzen2 }}/php/pipehandler.php

[dotfiles-lemon-php-panel]:       {{ dotfiles_lemon }}/php/panel.php
[dotfiles-lemon-php-helper]:      {{ dotfiles_lemon }}/php/helper.php
[dotfiles-lemon-php-output]:      {{ dotfiles_lemon }}/php/output.php
[dotfiles-lemon-php-pipehandler]: {{ dotfiles_lemon }}/php/pipehandler.php
