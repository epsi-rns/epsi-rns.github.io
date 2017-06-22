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

![Statusbar: Dzen2 Screenpmot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenpmot][image-hlwm-ss-lemon]{: .img-responsive }

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

    my $pid = fork;
    return if $pid;     # in the parent process
    
    run_lemon($monitor, $parameters);
    exit; 
}
{% endhighlight %}

The real function is <code>run_lemon</code>.
Note that we are using <code>IPC::Open2</code>
instead of <code>IO::Popen</code>,
to enable bidirectional pipe later.

{% highlight perl %}
sub run_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $command_out = "lemonbar $parameters -p";
    my ($rh_out, $wh_out);
    my $pid_out = open2 ($rh_out, $wh_out, $command_out) 
        or die "can't pipe out: $!";

    init_content($monitor, $wh_out);

     waitpid( $pid_out, 0 );
}
{% endhighlight %}

Note: that we want to ignore idle event for a while.
And append the <code>-p</code> for a while,
to make the statusbar persistent.

#### Statusbar Initialization

Here we have the <code>init_content</code>.
It is just an initialization of global variable.
We are going to have some loop later in different function,
to do the real works.

{% highlight perl %}
sub init_content {
    my $monitor = shift;
    my $pipe_out = shift;

    # initialize statusbar before loop
    output::set_tag_value($monitor);
    output::set_windowtitle('');

    my $text = output::get_statusbar_text($monitor);
    print $pipe_out $text."\n";
    flush $pipe_out;
}
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple Version, No Idle event.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.simple.pl][dotfiles-lemon-perl-pipehandler-simple]

-- -- --

### With Idle event

Consider this <code>walk_content</code> call,
after <code>init_content</code> call,
inside the <code>run_lemon</code>.

{% highlight perl %}
sub run_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $command_out = "lemonbar $parameters";
    my ($rh_out, $wh_out);
    my $pid_out = open2 ($rh_out, $wh_out, $command_out) 
        or die "can't pipe out: $!";

    init_content($monitor, $wh_out);
    walk_content($monitor, $wh_out); # loop for each event

     waitpid( $pid_out, 0 );
}
{% endhighlight %}

#### Wrapping Idle Event into Code

<code>walk_content</code> is the **heart** of this script.
We have to capture every event,
and process the event in event handler.

	Walk step by step, Process event by event

After the event handler,
we will get the statusbar text, in the same way,
we did in <code>init_content</code>.

{% highlight perl %}
sub walk_content {
    my $monitor = shift;
    my $wh_out = shift;
    
    # start a pipe
    my $command_in = 'herbstclient --idle';
    
    my ($rh_in, $wh_in);
    my $pid_in  = open2 ($rh_in,  $wh_in,  $command_in)
        or die "can't pipe in: $!";

    my $text = '';
    my $event = '';

    while($event = <$rh_in>) {
        # wait for next event
        handle_command_event($monitor, $event);
        
        $text = output::get_statusbar_text($monitor);     
        print $wh_out $text."\n";
        flush $wh_out;
    }
    
    waitpid( $pid_in,  0 );
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
        output::set_windowtitle($column[2]);
    }    
}
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

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
    my ($rh_out, $wh_out);
    my $pid_out = open2 ($rh_out, $wh_out, $command_out) 
        or die "can't pipe out: $!";

    my ($rh_sh, $wh_sh);
    my $pid_sh = open2 ($rh_sh, $wh_sh, 'sh') 
        or die "can't pipe sh: $!";

    my $pid = fork;
    if ($pid) {
        # in the parent process
        my $lines = '';
        while($lines = <$rh_out>) {
            print $wh_sh $lines;
            flush $wh_sh;
        }        
    } else {
        # in the child process
        init_content($monitor, $wh_out);
        walk_content($monitor, $wh_out); # loop for each event
    }

     waitpid( $pid_out, 0 );
     waitpid( $pid_sh, 0 );
}
{% endhighlight %}

#### How does it work ?

	Forking solve this issue.

Seriously, we have to take care on where to put the loop,
without interfering the original loop in <code>walk_content</code>.

#### View Source File:

Shell Version, also with Idle event.

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/pipehandler.shell.pm][dotfiles-lemon-perl-pipehandler-shell]

-- -- --

### Interval Based Event

We can put other stuff other than idle event in statusbar panel.
This event, such as date event,
called based on interval, such as one second interval.
Luckily we can treat interval as event.
It is a little bit tricky, because we have to make,
a combined event, that consist of idle event and interval event.


**TBD**

-- -- --

### Putting Them All Together

**TBD**

-- -- --

### Desktop Screenshot

**TBD**

-- -- --

Enjoy the statusbar !
Enjoy the window manager !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-01-event-idle]:   {{ asset_path }}/herbstclient-01-event-idle.png
[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-04-event-origin]: {{ asset_path }}/herbstclient-04-event-origin.png
[image-hlwm-05-clickable]:    {{ asset_path }}/herbstclient-05-lemonbar-clickable-areas.png

[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

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

[dotfiles-lemon-perl-pipehandler-shell]:  {{ dotfiles_lemon }}/perl/pipehandler.shell.pl
[dotfiles-lemon-perl-pipehandler-simple]: {{ dotfiles_lemon }}/perl/pipehandler.simple.pl

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
