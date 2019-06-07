---
layout: post
title:  "HerbstluftWM Idle Event in Ruby"
date      : 2017-06-15 17:35:15 +0700
categories: desktop
tags      : [coding, ruby, herbstluftwm, statusbar]
keywords  : [event idle, lemonbar, dzen2]
author: epsi

excerpt:
  HersbtluftWM Event Idle using Dzen2 or Lemonbar.
  Modularized implementation in Ruby script.
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

*	[Modularized HerbstluftWM in Ruby][local-ruby-config]

*	[Piping and Forking in Ruby][local-ruby-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/][dotfiles-dzen2-ruby]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/][dotfiles-lemon-ruby]

#### The PipeHandler Source File:

Let's have a look at <code class="code-file">pipehandler.rb</code> in github.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.rb][dotfiles-dzen2-ruby-pipehandler]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.rb][dotfiles-lemon-ruby-pipehandler]

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
<code class="code-file">panel.rb</code> in github.
At the end of the script, we finally call lemonbar
with <code>detach_lemon</code> function.

{% highlight ruby %}
# remove all lemonbar instance
system('pkill lemonbar')

# run process in the background
detach_lemon(monitor, lemon_parameters)
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/panel.rb][dotfiles-dzen2-ruby-panel]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/panel.rb][dotfiles-lemon-ruby-panel]


#### Run Lemon, Run !

This <code>detach_lemon</code> function.
is just a function that enable 
the lemonbar running process to be detached,
using <code>pid = fork { ... }</code>.

{% highlight ruby %}
def detach_lemon(monitor, parameters)
  # warning: Signal.trap is application wide
  Signal.trap("PIPE", "EXIT")
    
  pid_lemon = fork { run_lemon(monitor, parameters) }
  Process.detach(pid_lemon)
end
{% endhighlight %}

The real function is <code>run_lemon</code>.
You must be familiar with this <code>IO.popen</code>.

{% highlight ruby %}
def run_lemon(monitor, parameters)
  command_out  = 'lemonbar ' + parameters + ' -p'

  IO.popen(command_out, 'w') do |io_lemon|  
    content_init(monitor, io_lemon)        
    io_lemon.close()
  end
end
{% endhighlight %}

Note: that we want to ignore idle event for a while.
And append the <code>-p</code> for a while,
to make the statusbar persistent.

#### Statusbar Initialization

Here we have the <code>content_init</code>.
It is just an initialization of global variable.
We are going to have some loop later in different function,
to do the real works.

{% highlight ruby %}
def content_init(monitor, lemon_stdin)
  set_tag_value(monitor)
  set_windowtitle('')
      
  text = get_statusbar_text(monitor)
  lemon_stdin.puts(text)
end
{% endhighlight %}

Now is time to try the panel, on your terminal.
**Note**: that we already reach this stage in our previous article.
These two functions, <code>set_tag_value</code>
and <code>set_windowtitle</code>, have already been discussed.

#### View Source File:

Simple version. No idle event. Only statusbar initialization.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.simple.rb][dotfiles-lemon-ruby-pipehandler-simple]
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.01-init.rb][dotfiles-lemon-ruby-pipehandler-init]

{% include post/2017/06/herbstlustwm-event-idle-pipehandler.md %}

-- -- --

### With Idle event

Consider this <code>content_walk</code> call,
after <code>content_init</code> call,
inside the <code>run_lemon</code>.

{% highlight ruby %}
def run_lemon(monitor, parameters)
  command_out  = 'lemonbar ' + parameters

  IO.popen(command_out, 'w') do |io_lemon| 
  
    content_init(monitor, io_lemon)
    content_walk(monitor, io_lemon) # loop for each event
        
    io_lemon.close()
  end
end
{% endhighlight %}

We are still using unidirectional pipe,
using write mode <code>w</code>.

#### Wrapping Idle Event into Code

<code>content_walk</code> is the **heart** of this script.
We have to capture every event,
and process the event in event handler.

	Walk step by step, Process event by event

After the event handler,
we will get the statusbar text, in the same way,
we did in <code>content_init</code>.

{% highlight ruby %}
def content_walk(monitor, lemon_stdin)
  # start an io
  command_in = 'herbstclient --idle'
  
  IO.popen(command_in, "r") do |io_idle|
    while io_idle do 
      # read next event, trim newline
      event = (io_idle.gets).strip
      handle_command_event(monitor, event)
        
      text = get_statusbar_text(monitor)
      lemon_stdin.puts(text)
    end
    io_idle.close()
  end
end
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

{% highlight ruby %}
def handle_command_event(monitor, event) 
  # find out event origin
  column = event.split("\t")
  origin = column[0]
    
  tag_cmds = ['tag_changed', 'tag_flags', 'tag_added', 'tag_removed']
  title_cmds = ['window_title_changed', 'focus_changed']

  case origin
  when 'reload'
    os.system('pkill lemonbar')
  when 'quit_panel'
    exit
  when *tag_cmds       # splat operator
    set_tag_value(monitor)
  when *title_cmds     # splat operator
    title = column.length > 2 ? column[2] : ''
    set_windowtitle(title)
  end
end
{% endhighlight %}

Actually that's all we need to have a functional lemonbar.
This is the minimum version.

#### View Source File:

With idle event. The **heart** of the script.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.02-idle.rb][dotfiles-lemon-ruby-pipehandler-idle]

-- -- --

### Lemonbar Clickable Areas

This is specific issue for lemonbar,
that we don't have in dzen2.

Consider have a look at 
<code class="code-file">output.rb</code>.

{% highlight ruby %}
  # clickable tags
  text_name = "%{A:herbstclient focus_monitor \"#{monitor}\" && " \
            + "herbstclient use \"#{tag_index}\":} #{tag_name} %{A} "
{% endhighlight %}

**Issue**: Lemonbar put the output on terminal
instead of executing the command.

![HerbstluftWM: Tag Status][image-hlwm-05-clickable]{: .img-responsive }

Consider going back to
<code class="code-file">pipehandler.rb</code>.

We need to pipe the lemonbar output to shell.
It means Lemonbar read input and write output at the same time.

{% highlight ruby %}
def run_lemon(monitor, parameters)
  command_out  = 'lemonbar ' + parameters

  # note the r+ mode
  IO.popen(command_out, 'r+') do |io_lemon| 

    pid_content = fork do 
      content_init(monitor, io_lemon)
      content_walk(monitor, io_lemon) # loop for each event
    end
    Process.detach(pid_content)  

    IO.popen('sh', 'w') do |io_sh|
      while io_lemon do
        io_sh.puts io_lemon.gets
      end
        
      io_sh.close()
    end
 
    io_lemon.close()
  end
end
{% endhighlight %}

#### How does it work ?

	Forking solve this issue.

Seriously, we have to take care on where to put the loop,
without interfering the original loop in <code>content_walk</code>.

In order to enable bidirectional pipe,
we open the IO in read and write mode <code>r+</code>.

{% highlight ruby %}
  IO.popen(command_out, 'r+') do |io_lemon|
    ...
  end
{% endhighlight %}

#### View Source File:

Piping lemonbar output to shell, implementing lemonbar clickable area.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.03-clickable.rb][dotfiles-lemon-ruby-pipehandler-clickable]

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

*	[gitlab.com/.../dotfiles/.../ruby/11-testevents.rb][dotfiles-lemon-ruby-testevents]

-- -- --

### Combined Event

#### Preparing The View

This is what it looks like, an overview of what we want to achieve.

![Statusbar: Event Screenshot][image-hlwm-ss-event]{: .img-responsive }

Consider make a progress in 
<code class="code-file">output.py</code>.

{% highlight ruby %}
@segment_datetime    = '' # empty string

def get_statusbar_text(monitor)
  ...

  # draw date and time
  text << '%{c}'
  text << output_by_datetime()
 
  ...
end

def output_by_datetime()
  @segment_datetime
end

def set_datetime()
    ...

    @segment_datetime = "#{date_text}  #{time_text}"
end
{% endhighlight %}

And a few enhancement in 
<code class="code-file">pipehandler.rb</code>.

{% highlight ruby %}
def handle_command_event(monitor, event) 
  ...

  case origin
  ...
  when 'interval'
    set_datetime()
  end
end

def content_init(monitor, lemon_stdin)
  ...
  set_windowtitle('')
  set_datetime()
      
  ...
end
{% endhighlight %}

#### Expanding The Event Controller

All we need to do is to split out <code>content_walk</code> into

*	<code>content_walk</code>: combined event,
	with the help of <code>cat</code> process.

*	<code>content_event_idle</code>: HerbstluftWM idle event. 
	Forked, as background processing.

*	<code>content_event_interval</code> : Custom date time event. 
	Forked, as background processing.

{% highlight ruby %}
def content_event_idle(cat_stdin)
  pid_idle = fork do 
    # start an io
    command_in = 'herbstclient --idle'
  
    IO.popen(command_in, "r") do |io_idle|
      while io_idle do 
         # read next event
        event = io_idle.gets
        cat_stdin.print(event)
      end
      io_idle.close()
    end
  end

  Process.detach(pid_idle)  
end
{% endhighlight %}

{% highlight ruby %}
def content_event_interval(cat_stdin)
  pid_interval = fork do 
    while true do
      cat_stdin.print "interval\n"
      sleep(1)
    end
  end

  Process.detach(pid_interval)  
end
{% endhighlight %}

{% highlight ruby %}
def content_walk(monitor, lemon_stdin)
  # note the r+ mode for bidirectional
  IO.popen('cat', 'r+') do |io_cat| 

    content_event_idle(io_cat)
    content_event_interval(io_cat)

    while io_cat do 
      # read next event, trim newline
      event = (io_cat.gets).strip
      handle_command_event(monitor, event)
        
      text = get_statusbar_text(monitor)
      lemon_stdin.puts(text)
    end
  
  io_cat.close()
  end
end
{% endhighlight %}

This above is the most complex part.
We are almost done.

#### View Source File:

Combined event consist of both,
synchronous interval event and asynchronous idle event.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.04-event.rb][dotfiles-lemon-ruby-pipehandler-event]

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
def detach_lemon_conky(parameters)
  # warning: Signal.trap is application wide
  Signal.trap("PIPE", "EXIT")
    
  pid_conky = fork do
    path    = __dir__+ "/../conky"
    cmd_in  = 'conky -c ' + path + '/conky.lua'
    cmd_out = 'lemonbar ' + parameters
  
    IO.popen(cmd_out, "w") do |io_lemon|
    IO.popen(cmd_in,  "r") do |io_conky| 
      while io_conky do
        io_lemon.puts io_conky.gets
      end

      io_conky.close()    
      io_lemon.close()
    end
    end
  end

  Process.detach(pid_conky)
end
{% endhighlight %}

And execute the function main script in
<code class="code-file">panel.pl</code>.

{% highlight perl %}
#!/usr/bin/ruby

require_relative 'helper'
require_relative 'pipehandler'

# main

panel_height = 24
monitor = get_monitor(ARGV)

system('pkill lemonbar')
system("herbstclient pad #{monitor} #{panel_height} 0 #{panel_height} 0")

# run process in the background

params_top = get_params_top(monitor, panel_height)
detach_lemon(monitor, params_top)

params_bottom = get_params_bottom(monitor, panel_height)
detach_lemon_conky(params_bottom)
{% endhighlight %}

	Relative in context of directory, not family.

#### View Source File:

Dual Bar, <code>detach_lemon_conky</code> function.

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/pipehandler.05-conky.rb][dotfiles-lemon-ruby-pipehandler-conky]

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

{% highlight ruby %}
def kill_zombie()
    system('pkill -x dzen2')
    system('pkill -x lemonbar')
    system('pkill -x cat')
    system('pkill conky')
    system('pkill herbstclient')
end
{% endhighlight %}

-- -- --

### Putting Them All Together

I also created compact for version,
for use with main HerbstluftWM configuration,
in <code class="code-file">~/.config/herbstluftwm/</code> directory.
After reunification, they are not very long scripts after all.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/panel-dzen2.rb][dotfiles-hlwm-ruby-dzen2-compact]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/panel-lemonbar.rb][dotfiles-hlwm-ruby-lemon-compact]

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

[dotfiles-lemon-ruby-testevents]:  {{ dotfiles_lemon }}/ruby/11-testevents.rb
[dotfiles-hlwm-ruby-dzen2-compact]: {{ dotfiles_hlwm }}/ruby/panel-dzen2.rb
[dotfiles-hlwm-ruby-lemon-compact]: {{ dotfiles_hlwm }}/ruby/panel-lemonbar.rb

[local-ruby-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-ruby.html
[local-ruby-pipe]:   {{ site.url }}/code/2017/04/16/ruby-pipe-and-fork.html

[local-bash-config]: {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-bash-pipe]:   {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html

[dotfiles-lemon-ruby-pipehandler-init]:      {{ dotfiles_lemon }}/ruby/pipehandler.01-init.rb
[dotfiles-lemon-ruby-pipehandler-idle]:      {{ dotfiles_lemon }}/ruby/pipehandler.02-idle.rb
[dotfiles-lemon-ruby-pipehandler-clickable]: {{ dotfiles_lemon }}/ruby/pipehandler.03-clickable.rb
[dotfiles-lemon-ruby-pipehandler-event]:     {{ dotfiles_lemon }}/ruby/pipehandler.04-event.rb
[dotfiles-lemon-ruby-pipehandler-conky]:     {{ dotfiles_lemon }}/ruby/pipehandler.05-conky.rb

[dotfiles-dzen2-ruby-panel]:       {{ dotfiles_dzen2 }}/ruby/panel.rb
[dotfiles-dzen2-ruby-helper]:      {{ dotfiles_dzen2 }}/ruby/helper.rb
[dotfiles-dzen2-ruby-output]:      {{ dotfiles_dzen2 }}/ruby/output.rb
[dotfiles-dzen2-ruby-pipehandler]: {{ dotfiles_dzen2 }}/ruby/pipehandler.rb

[dotfiles-lemon-ruby-panel]:       {{ dotfiles_lemon }}/ruby/panel.rb
[dotfiles-lemon-ruby-helper]:      {{ dotfiles_lemon }}/ruby/helper.rb
[dotfiles-lemon-ruby-output]:      {{ dotfiles_lemon }}/ruby/output.rb
[dotfiles-lemon-ruby-pipehandler]: {{ dotfiles_lemon }}/ruby/pipehandler.rb
