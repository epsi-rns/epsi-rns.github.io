---
layout: post-sidemenu-wm
title:  "HerbstluftWM Idle Event in Perl"
date:   2017-06-13 17:35:15 +0700
categories: desktop
tags: [coding, perl, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Event Idle using Dzen2 or Lemonbar.
  Modularized implementation in Perl script.
  An advance case of Pipe and Fork.

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

*	[Modularized HerbstluftWM in Perl][local-perl-config]

*	[Piping and Forking in Perl][local-perl-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/][dotfiles-dzen2-perl]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/][dotfiles-lemon-perl]

#### The PipeHandler Source File:

Let's have a look at <code class="code-file">pipehandler.pm</code> in github.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/pipehandler.pm][dotfiles-dzen2-perl-pipehandler]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.pm][dotfiles-lemon-perl-pipehandler]

-- -- --

### HerbstluftWM Idle Event in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Idle Event Overview ][local-overview]]
[[ BASH ][local-bash]]
[[ Perl ][local-perl]]
[[ Python ][local-python]]
[[ Ruby ][local-ruby]]
[[ PHP ][local-php]]
[[ Lua ][local-lua]]
[[ Haskell ][local-haskell]]

Dzen2 Source Code Directory:
[[ BASH ][dotfiles-dzen2-bash]]
[[ Perl ][dotfiles-dzen2-perl]]
[[ Python ][dotfiles-dzen2-python]]
[[ Ruby ][dotfiles-dzen2-ruby]]
[[ PHP ][dotfiles-dzen2-php]]
[[ Lua ][dotfiles-dzen2-lua]]
[[ Haskell ][dotfiles-dzen2-haskell]]

Lemonbar Source Code Directory:
[[ BASH ][dotfiles-lemon-bash]]
[[ Perl ][dotfiles-lemon-perl]]
[[ Python ][dotfiles-lemon-python]]
[[ Ruby ][dotfiles-lemon-ruby]]
[[ PHP ][dotfiles-lemon-php]]
[[ Lua ][dotfiles-lemon-lua]]
[[ Haskell ][dotfiles-lemon-haskell]]

-- -- --

### Statusbar Screenshot

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Without Idle event

Let's have a look at our main 
<code class="code-file">panel.pm</code> in github.
At the end of the script, we finally call lemonbar
with <code>detach_lemon</code> function.

{% highlight perl %}
# remove all lemonbar instance
system('pkill lemonbar');

# run process in the background
pipehandler::detach_lemon($monitor, $lemon_parameters);
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/panel.pl][dotfiles-dzen2-perl-panel]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/panel.pl][dotfiles-lemon-perl-panel]


#### Run Lemon, Run !

This <code>detach_lemon</code> function.
is just a function that enable 
the lemonbar running process to be detached,
using <code>my $pid = fork</code>.

{% highlight perl %}
sub detach_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $pid_lemon = fork;
    return if $pid_lemon;     # in the parent process
    
    run_lemon($monitor, $parameters);
    exit; 
}
{% endhighlight %}

The real function is <code>run_lemon</code>.
Note that we are using <code>IPC::Open2</code>
instead of <code>IO::Pipe</code>,
to enable bidirectional pipe later.

{% highlight perl %}
sub run_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $command_out = "lemonbar $parameters -p";
    my ($rh_lemon_out, $wh_lemon_out);
    my $pid_lemon_out = open2 (
            $rh_lemon_out, $wh_lemon_out, $command_out) 
        or die "can't pipe lemon out: $!";

    content_init($monitor, $wh_lemon_out);
    waitpid( $pid_lemon_out, 0 );
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

{% highlight perl %}
sub content_init {
    my $monitor = shift;
    my $pipe_lemon_out = shift;

    output::set_tag_value($monitor);
    output::set_windowtitle('');

    my $text = output::get_statusbar_text($monitor);
    print $pipe_lemon_out $text."\n";
    flush $pipe_lemon_out;
}
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple version. No idle event. Only statusbar initialization.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.01-init.pm][dotfiles-lemon-perl-pipehandler-init]

-- -- --

### With Idle event

Consider this <code>content_walk</code> call,
after <code>content_init</code> call,
inside the <code>run_lemon</code>.

{% highlight perl %}
sub run_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $command_out = "lemonbar $parameters";
    my ($rh_lemon_out, $wh_lemon_out);
    my $pid_lemon_out = open2 (
            $rh_lemon_out, $wh_lemon_out, $command_out) 
        or die "can't pipe lemon out: $!";

    content_init($monitor, $wh_lemon_out);
    content_walk($monitor, $wh_lemon_out); # loop for each event

    waitpid( $pid_lemon_out, 0 );
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
<code>IO::Pipe</code> is sufficient for unidirectional pipe.

{% highlight perl %}
sub content_walk {
    my $monitor = shift;
    my $pipe_lemon_out = shift; 
    
    # start a pipe
    my $pipe_idle_in = IO::Pipe->new();
    my $command = 'herbstclient --idle';
    my $handle  = $pipe_idle_in->reader($command);

    my $text = '';
    my $event = '';

    # wait for each event, trim newline
    while (chomp($event = <$pipe_idle_in>)) {
        handle_command_event($monitor, $event);
        
        $text = output::get_statusbar_text($monitor);     
        print $pipe_lemon_out $text."\n";
        flush $pipe_lemon_out;
    }
    
    $pipe_idle_in->close();
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

{% highlight perl %}
sub handle_command_event {
    my $monitor = shift;
    my $event = shift;
    
    # find out event origin
    my @column = split(/\t/, $event);
    my $origin = $column[0];

    if ($origin eq 'reload') {
        system('pkill lemonbar');
    } elsif ($origin eq 'quit_panel') {
        exit;
    } elsif (  # avoiding the unstable ~~ smartmatch operator
               ($origin eq 'tag_changed') 
            or ($origin eq 'tag_flags')
            or ($origin eq 'tag_added')
            or ($origin eq 'tag_removed')
            ) {
        output::set_tag_value($monitor);
    } elsif (  ($origin eq 'window_title_changed') 
            or ($origin eq 'focus_changed')
            ) {
        my $title = ($#column > 2) ? $column[2] : '';
        output::set_windowtitle($title);
    }    
}
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

#### View Source File:

With idle event. The **heart** of the script.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.02-idle.pm][dotfiles-lemon-perl-pipehandler-idle]

-- -- --

### Lemonbar Clickable Areas

This is specific issue for lemonbar,
that we don't have in dzen2.

Consider have a look at 
<code class="code-file">output.pm</code>.

{% highlight perl %}
    # clickable tags
    my $text_name = "%{A:herbstclient focus_monitor \"$monitor\" && "
                  . "herbstclient use \"$tag_index\":} $tag_name %{A} ";
{% endhighlight %}

**Issue**: Lemonbar put the output on terminal
instead of executing the command.

![HerbstluftWM: Tag Status][image-hlwm-05-clickable]{: .img-responsive }

Consider going back to
<code class="code-file">pipehandler.pm</code>.

We need to pipe the lemonbar output to shell.
It means Lemonbar read input and write output at the same time.

{% highlight perl %}
sub run_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $command_out = "lemonbar $parameters";
    my ($rh_lemon_out, $wh_lemon_out);
    my $pid_lemon_out = open2 (
            $rh_lemon_out, $wh_lemon_out, $command_out) 
        or die "can't pipe lemon out: $!";
        
    my ($rh_sh, $wh_sh);
    my $pid_sh = open2 ($rh_sh, $wh_sh, 'sh') 
        or die "can't pipe sh: $!";

    my $pid_content = fork;
    if ($pid_content) {
        # in the parent process
        my $line_clickable = '';
        while($line_clickable = <$rh_lemon_out>) {
            print $wh_sh $line_clickable;
            flush $wh_sh;
        }        
    } else {
        # in the child process
        content_init($monitor, $wh_lemon_out);
        content_walk($monitor, $wh_lemon_out); # loop for each event
    }

    waitpid( $pid_lemon_out, 0 );
    waitpid( $pid_sh, 0 );
}
{% endhighlight %}

#### How does it work ?

	Forking solve this issue.

Seriously, we have to take care on where to put the loop,
without interfering the original loop in <code>content_walk</code>.

#### View Source File:

Piping lemonbar output to shell, implementing lemonbar clickable area.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.03-clickable.pm][dotfiles-lemon-perl-pipehandler-clickable]

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

*	[github.com/.../dotfiles/.../perl/11-testevents.pl][dotfiles-lemon-perl-testevents]

-- -- --

### Combined Event

#### Preparing The View

This is what it looks like, an overview of what we want to achieve.

![Statusbar: Event Screenshot][image-hlwm-ss-event]{: .img-responsive }

Consider make a progress in 
<code class="code-file">output.pm</code>.

{% highlight perl %}
use Time::Piece;

my $segment_datetime    = ''; # empty string

sub get_statusbar_text {
    ...

    # draw date and time
    $text .= '%{c}';
    $text .= output_by_datetime();
    
    ...
}

sub output_by_datetime {
    return $segment_datetime;
}

sub set_datetime {    ...

    $segment_datetime = "$date_text  $time_text";
}
{% endhighlight %}

And a few enhancement in 
<code class="code-file">pipehandler.pm</code>.

{% highlight perl %}
sub handle_command_event {
    ...

    if ($origin eq 'reload') {
    ...
    } elsif ($origin eq 'interval') {
        output::set_datetime();
    }   
}

sub content_init {
    ...
    output::set_windowtitle('');
    output::set_datetime();

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

{% highlight perl %}
sub content_event_idle {
    my $pipe_cat_out = shift;
 
    my $pid_idle = fork;
    return if $pid_idle;     # in the parent process

    # start a pipe
    my $pipe_idle_in = IO::Pipe->new();
    my $command = 'herbstclient --idle';
    my $handle  = $pipe_idle_in->reader($command);

    # wait for each event
    my $event = '';
    while ($event = <$pipe_idle_in>) {
        print $pipe_cat_out $event;
        flush $pipe_cat_out;
    }
    
    $pipe_idle_in->close();
}
{% endhighlight %}

{% highlight perl %}
sub content_event_interval {
    my $pipe_cat_out = shift;

    my $pid_interval = fork;
    return if $pid_interval;     # in the parent process
    
    while(1) {         
        print $pipe_cat_out "interval\n";
        flush $pipe_cat_out;
        
        sleep 1;
    }
}
{% endhighlight %}

{% highlight perl %}
sub content_walk {
    my $monitor = shift;
    my $pipe_lemon_out = shift; 

    my ($rh_cat, $wh_cat);
    my $pid_cat = open2 ($rh_cat, $wh_cat, 'cat') 
        or die "can't pipe sh: $!";

    content_event_idle($wh_cat);
    content_event_interval($wh_cat);

    my $text  = '';
    my $event = '';

    # wait for each event, trim newline
    while (chomp($event = <$rh_cat>)) {
        handle_command_event($monitor, $event);
        
        $text = output::get_statusbar_text($monitor);     
        print $pipe_lemon_out $text."\n";
        flush $pipe_lemon_out;
    }

    waitpid( $pid_cat, 0 );
}
{% endhighlight %}

This above is the most complex part.
We are almost done.

#### View Source File:

Combined event consist of both,
synchronous interval event and asynchronous idle event.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.04-event.pm][dotfiles-lemon-perl-pipehandler-event]

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
sub detach_lemon_conky { 
    my $parameters = shift;

    my $pid_conky = fork;
    return if $pid_conky;     # in the parent process

    my $pipe_out = IO::Pipe->new();
    my $cmd_in   = "lemonbar " . $parameters;
    my $hnd_in   = $pipe_out->writer($cmd_in);

    my $pipe_in  = IO::Pipe->new();
    my $dirname  = dirname(__FILE__);
    my $path     = "$dirname/../conky";       
    my $cmd_out  = "conky -c $path/conky.lua";
    my $hnd_out  = $pipe_in->reader($cmd_out);

    while(<$pipe_in>) {
        print $pipe_out $_;
        flush $pipe_out;
    }

    $pipe_in->close();
    $pipe_out->close();
    exit; 
}
{% endhighlight %}

And execute the function main script in
<code class="code-file">panel.pl</code>.

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);

use helper;
use pipehandler;

# main

my $panel_height = 24;
my $monitor = helper::get_monitor(@ARGV);

system('pkill lemonbar');
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");

# run process in the background

my $params_top = helper::get_params_top($monitor, $panel_height);
pipehandler::detach_lemon($monitor, $params_top);

my $params_bottom = helper::get_params_bottom($monitor, $panel_height);
pipehandler::detach_lemon_conky($params_bottom);
{% endhighlight %}

#### View Source File:

Dual Bar, <code>detach_lemon_conky</code> function.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.05-conky.pm][dotfiles-lemon-perl-pipehandler-conky]

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

{% highlight perl %}
sub kill_zombie() {
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
	[github.com/.../dotfiles/.../perl/panel-dzen2.pl][dotfiles-hlwm-perl-dzen2-compact]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/panel-lemonbar.pl][dotfiles-hlwm-perl-lemon-compact]

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

[dotfiles-lemon-perl-testevents]:  {{ dotfiles_lemon }}/perl/11-testevents.pl
[dotfiles-hlwm-perl-dzen2-compact]: {{ dotfiles_hlwm }}/perl/panel-dzen2.pl
[dotfiles-hlwm-perl-lemon-compact]: {{ dotfiles_hlwm }}/perl/panel-lemonbar.pl

[local-perl-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html
[local-perl-pipe]:   {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/12/herbstlustwm-event-idle-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/13/herbstlustwm-event-idle-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/16/herbstlustwm-event-idle-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html

[dotfiles-lemon-perl-pipehandler-init]:      {{ dotfiles_lemon }}/perl/pipehandler.01-init.pm
[dotfiles-lemon-perl-pipehandler-idle]:      {{ dotfiles_lemon }}/perl/pipehandler.02-idle.pm
[dotfiles-lemon-perl-pipehandler-clickable]: {{ dotfiles_lemon }}/perl/pipehandler.03-clickable.pm
[dotfiles-lemon-perl-pipehandler-event]:     {{ dotfiles_lemon }}/perl/pipehandler.04-event.pm
[dotfiles-lemon-perl-pipehandler-conky]:     {{ dotfiles_lemon }}/perl/pipehandler.05-conky.pm

[dotfiles-dzen2-bash]:    {{ dotfiles_dzen2 }}/bash
[dotfiles-dzen2-perl]:    {{ dotfiles_dzen2 }}/perl
[dotfiles-dzen2-python]:  {{ dotfiles_dzen2 }}/python
[dotfiles-dzen2-ruby]:    {{ dotfiles_dzen2 }}/ruby
[dotfiles-dzen2-php]:     {{ dotfiles_dzen2 }}/php
[dotfiles-dzen2-lua]:     {{ dotfiles_dzen2 }}/lua
[dotfiles-dzen2-haskell]: {{ dotfiles_dzen2 }}/haskell

[dotfiles-lemon-bash]:    {{ dotfiles_lemon }}/bash
[dotfiles-lemon-perl]:    {{ dotfiles_lemon }}/perl
[dotfiles-lemon-python]:  {{ dotfiles_lemon }}/python
[dotfiles-lemon-ruby]:    {{ dotfiles_lemon }}/ruby
[dotfiles-lemon-php]:     {{ dotfiles_lemon }}/php
[dotfiles-lemon-lua]:     {{ dotfiles_lemon }}/lua
[dotfiles-lemon-haskell]: {{ dotfiles_lemon }}/haskell

[dotfiles-dzen2-bash-panel]:       {{ dotfiles_dzen2 }}/bash/panel.sh
[dotfiles-dzen2-bash-helper]:      {{ dotfiles_dzen2 }}/bash/helper.sh
[dotfiles-dzen2-bash-output]:      {{ dotfiles_dzen2 }}/bash/output.sh
[dotfiles-dzen2-bash-pipehandler]: {{ dotfiles_dzen2 }}/bash/pipehandler.sh

[dotfiles-dzen2-perl-panel]:       {{ dotfiles_dzen2 }}/perl/panel.pl
[dotfiles-dzen2-perl-helper]:      {{ dotfiles_dzen2 }}/perl/helper.pm
[dotfiles-dzen2-perl-output]:      {{ dotfiles_dzen2 }}/perl/output.pm
[dotfiles-dzen2-perl-pipehandler]: {{ dotfiles_dzen2 }}/perl/pipehandler.pm

[dotfiles-dzen2-python-panel]:       {{ dotfiles_dzen2 }}/python/panel.py
[dotfiles-dzen2-python-helper]:      {{ dotfiles_dzen2 }}/python/helper.py
[dotfiles-dzen2-python-output]:      {{ dotfiles_dzen2 }}/python/output.py
[dotfiles-dzen2-python-pipehandler]: {{ dotfiles_dzen2 }}/python/pipehandler.py

[dotfiles-dzen2-ruby-panel]:       {{ dotfiles_dzen2 }}/ruby/panel.rb
[dotfiles-dzen2-ruby-helper]:      {{ dotfiles_dzen2 }}/ruby/helper.rb
[dotfiles-dzen2-ruby-output]:      {{ dotfiles_dzen2 }}/ruby/output.rb
[dotfiles-dzen2-ruby-pipehandler]: {{ dotfiles_dzen2 }}/ruby/pipehandler.rb

[dotfiles-dzen2-php-panel]:       {{ dotfiles_dzen2 }}/php/panel.php
[dotfiles-dzen2-php-helper]:      {{ dotfiles_dzen2 }}/php/helper.php
[dotfiles-dzen2-php-output]:      {{ dotfiles_dzen2 }}/php/output.php
[dotfiles-dzen2-php-pipehandler]: {{ dotfiles_dzen2 }}/php/pipehandler.php

[dotfiles-dzen2-lua-panel]:       {{ dotfiles_dzen2 }}/lua/panel.lua
[dotfiles-dzen2-lua-common]:      {{ dotfiles_dzen2 }}/lua/common.lua
[dotfiles-dzen2-lua-helper]:      {{ dotfiles_dzen2 }}/lua/helper.lua
[dotfiles-dzen2-lua-output]:      {{ dotfiles_dzen2 }}/lua/output.lua
[dotfiles-dzen2-lua-pipehandler]: {{ dotfiles_dzen2 }}/lua/pipehandler.lua

[dotfiles-dzen2-haskell-panel]:       {{ dotfiles_dzen2 }}/haskell/panel.hs
[dotfiles-dzen2-haskell-helper]:      {{ dotfiles_dzen2 }}/haskell/MyHelper.hs
[dotfiles-dzen2-haskell-output]:      {{ dotfiles_dzen2 }}/haskell/MyOutput.hs
[dotfiles-dzen2-haskell-pipehandler]: {{ dotfiles_dzen2 }}/haskell/MyPipeHandler.hs

[dotfiles-lemon-bash-panel]:       {{ dotfiles_lemon }}/bash/panel.sh
[dotfiles-lemon-bash-helper]:      {{ dotfiles_lemon }}/bash/helper.sh
[dotfiles-lemon-bash-output]:      {{ dotfiles_lemon }}/bash/output.sh
[dotfiles-lemon-bash-pipehandler]: {{ dotfiles_lemon }}/bash/pipehandler.sh

[dotfiles-lemon-perl-panel]:       {{ dotfiles_lemon }}/perl/panel.pl
[dotfiles-lemon-perl-helper]:      {{ dotfiles_lemon }}/perl/helper.pm
[dotfiles-lemon-perl-output]:      {{ dotfiles_lemon }}/perl/output.pm
[dotfiles-lemon-perl-pipehandler]: {{ dotfiles_lemon }}/perl/pipehandler.pm

[dotfiles-lemon-python-panel]:       {{ dotfiles_lemon }}/python/panel.py
[dotfiles-lemon-python-helper]:      {{ dotfiles_lemon }}/python/helper.py
[dotfiles-lemon-python-output]:      {{ dotfiles_lemon }}/python/output.py
[dotfiles-lemon-python-pipehandler]: {{ dotfiles_lemon }}/python/pipehandler.py

[dotfiles-lemon-ruby-panel]:       {{ dotfiles_lemon }}/ruby/panel.rb
[dotfiles-lemon-ruby-helper]:      {{ dotfiles_lemon }}/ruby/helper.rb
[dotfiles-lemon-ruby-output]:      {{ dotfiles_lemon }}/ruby/output.rb
[dotfiles-lemon-ruby-pipehandler]: {{ dotfiles_lemon }}/ruby/pipehandler.rb

[dotfiles-lemon-php-panel]:       {{ dotfiles_lemon }}/php/panel.php
[dotfiles-lemon-php-helper]:      {{ dotfiles_lemon }}/php/helper.php
[dotfiles-lemon-php-output]:      {{ dotfiles_lemon }}/php/output.php
[dotfiles-lemon-php-pipehandler]: {{ dotfiles_lemon }}/php/pipehandler.php

[dotfiles-lemon-lua-panel]:       {{ dotfiles_lemon }}/lua/panel.lua
[dotfiles-lemon-lua-common]:      {{ dotfiles_lemon }}/lua/common.lua
[dotfiles-lemon-lua-helper]:      {{ dotfiles_lemon }}/lua/helper.lua
[dotfiles-lemon-lua-output]:      {{ dotfiles_lemon }}/lua/output.lua
[dotfiles-lemon-lua-pipehandler]: {{ dotfiles_lemon }}/lua/pipehandler.lua

[dotfiles-lemon-haskell-panel]:       {{ dotfiles_lemon }}/haskell/panel.hs
[dotfiles-lemon-haskell-helper]:      {{ dotfiles_lemon }}/haskell/MyHelper.hs
[dotfiles-lemon-haskell-output]:      {{ dotfiles_lemon }}/haskell/MyOutput.hs
[dotfiles-lemon-haskell-pipehandler]: {{ dotfiles_lemon }}/haskell/MyPipeHandler.hs
