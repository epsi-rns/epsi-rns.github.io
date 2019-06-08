---
layout: post
title:  "HerbstluftWM Tag Status in Ruby"
date      : 2017-06-05 17:35:15 +0700
categories: desktop
tags      : [coding, ruby, herbstluftwm, statusbar]
keywords  : [tag status, lemonbar, dzen2]
author: epsi

opengraph:
  image: /assets/site/images/topics/ruby.png

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in Ruby script.

related_link_ids: 
  - 17060135  # Tag Status Overview
  - 17060235  # Tag Status BASH
  - 17060335  # Tag Status Perl
  - 17060435  # Tag Status Python
  - 17060535  # Tag Status Ruby
  - 17060635  # Tag Status PHP
  - 17060735  # Tag Status Lua
  - 17060835  # Tag Status Haskell
  - 17060935  # Tag Status Global Notes

---

### Preface

> Goal: Show the Herbstclient Tag.

	Focusing in "herbstclient tag_status". 

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

This tutorial cover Lemonbar, and in order to use Dzen2,
any reader could use the source code in github.

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

-- -- --

{% include post/2017/06/herbstlustwm-tag-status-language.md %}

-- -- --

### Screenshot

Since window manager is out of topic in this tutorial,
I present **only panel** HerbstluftWM screenshot.

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
For both Dzen2 and Lemonbar, the structure are the same.
This figure will explain how it looks
in <code>Ruby script</code> directory.

![Statusbar: Directory Structure][image-01-tree-ruby]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

### Get Geometry

Let's have a look at <code class="code-file">helper.rb</code> in github.

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/helper.rb][dotfiles-dzen2-ruby-helper]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/helper.rb][dotfiles-lemon-ruby-helper]

{% include post/2017/06/herbstlustwm-tag-status-helper.md %}

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight bash %}
$ ./panel.rb 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.

Here it is our code in Ruby.
We do not need to call return in Ruby,
the value of the last line will be a function return.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
# script arguments
def get_monitor(arguments)
  # ternary operator
  arguments.length > 0 ? arguments[0].to_i : 0
end
{% endhighlight %}

And in main code we can call

{% highlight ruby %}
require_relative 'helper'

monitor = get_monitor(ARGV)
puts(monitor)
{% endhighlight %}

This will display <code>0</code> or else such as <code>1</code>,
depend on the script argument given.

{% highlight conf %}
0
{% endhighlight %}

#### Get Monitor Geometry

HerbstluftWM give this little tools to manage monitor geometry
by getting monitor rectangle.

{% highlight bash %}
$ herbstclient monitor_rect
{% endhighlight %}

This will show something similar to this.

{% highlight conf %}
0 0 1280 800
{% endhighlight %}

![HerbstluftWM: Monitor Rectangle][image-hlwm-02-monitor-rect]{: .img-responsive }

Consider wrap the code into function.
And get an array as function return.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
def get_geometry(monitor)
  raw = IO.popen('herbstclient monitor_rect '+ monitor.to_s).read()

  if raw.to_s.empty?
    print('Invalid monitor ' + monitor.to_s)
    exit(1)
  end
    
  raw.split(' ')
end
{% endhighlight %}

Consider call this function from script later.
To print array in Ruby,
we just have to wrap it in <code>geometry.join(' ')</code>.

{% highlight ruby %}
monitor = get_monitor(ARGV)
geometry = get_geometry(monitor)
puts(geometry.join(' '))
{% endhighlight %}

This will produce

{% highlight conf %}
0 0 1280 800
{% endhighlight %}

#### Get Panel Geometry

The Panel geometry is completely depend on the user flavor and taste.
You can put it, on top, or bottom, or hanging somewhere.
You can create gap on both left and right.

Consider this example:
<code class="code-file">helper.rb</code>

{% highlight ruby %}
def get_bottom_panel_geometry(height, geometry)
  # geometry has the format X Y W H
  return geometry[0].to_i + 24, (geometry[3].to_i - height), 
         geometry[2].to_i - 48, height
end
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight ruby %}
panel_height = 24
monitor = get_monitor(ARGV)
geometry = get_geometry(monitor)
xpos, ypos, width, height = get_bottom_panel_geometry(
    panel_height, geometry)

puts('Lemonbar geometry: ' \
      + "#{width}x#{height}+#{xpos}+#{ypos}")
{% endhighlight %}

This will show something similar to this result,
depend on your monitor size.

{% highlight conf %}
Lemonbar geometry: 1280x24+24+776
{% endhighlight %}

#### Get Lemonbar Parameters

We almost done. 
This is the last step.
We wrap it all inside this function below.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
def get_lemon_parameters(monitor, panel_height)
  # calculate geometry
  geometry = get_geometry(monitor)
  xpos, ypos, width, height = get_top_panel_geometry(
    panel_height, geometry)

  # geometry: -g widthxheight+x+y
  geom_res = "#{width}x#{height}+#{xpos}+#{ypos}"

  # color, with transparency    
  bgcolor = "'#aa000000'"
  fgcolor = "'#ffffff'"

  # XFT: require lemonbar_xft_git 
  font_takaop  = "takaopgothic-9"
  font_bottom  = "monospace-9"
  font_symbol  = "PowerlineSymbols-11"
  font_awesome = "FontAwesome-9"

  parameters  = "  -g #{geom_res} -u 2" \
                   " -B #{bgcolor} -F #{fgcolor}" \
                   " -f #{font_takaop} -f #{font_awesome} -f #{font_symbol}"
end
{% endhighlight %}

-- -- --

### Testing The Parameters

Consider this code <code class="code-file">01-testparams.rb</code>.
The script call the above function to get lemon parameters.

{% highlight ruby %}
#!/usr/bin/ruby
require_relative 'helper'

# initialize
panel_height = 24
monitor = get_monitor(ARGV)

lemon_parameters = get_lemon_parameters(monitor, panel_height)
puts(lemon_parameters)
{% endhighlight %}

This will produce output
something similar to this result

{% highlight conf %}
-g 1280x24+0+0 -u 2 -B '#aa000000' -F '#ffffff' 
-f takaopgothic-9 -f FontAwesome-9 -f PowerlineSymbols-11
{% endhighlight %}

Or in Dzen2 version:

{% highlight conf %}
-x 0 -y 0 -w 1280 -h 24 -ta l 
-bg '#000000' -fg '#ffffff' -title-name dzentop 
-fn '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*'
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/01-testparams.rb][dotfiles-dzen2-ruby-testparams]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/01-testparams.rb][dotfiles-lemon-ruby-testparams]

{% include post/2017/06/herbstlustwm-tag-status-testparams.md %}

-- -- --

### Adjusting the Desktop

Since we want to use panel, we have to adjust the desktop gap,
giving space at the top and bottom.

{% highlight bash %}
$ herbstclient pad 0 24 0 24 0
{% endhighlight %}

For more information, do <code>$ man herbsluftclient</code>,
and type <code>\pad</code> to search what it means.

In script, it looks like this below.

{% highlight ruby %}
system("herbstclient pad #{monitor} #{panel_height} 0 #{panel_height} 0")
{% endhighlight %}

-- -- --

### Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

<code class="code-file">gmc.rb</code>

{% highlight ruby %}
module GMC
  Color = {
    'white' => '#ffffff',
    'black' => '#000000',

    'grey50'  => '#fafafa',
    'grey100' => '#f5f5f5',
  }
end
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/gmc.rb][dotfiles-dzen2-ruby-gmc]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/gmc.rb][dotfiles-lemon-ruby-gmc]

{% include post/2017/06/herbstlustwm-tag-status-gmc.md %}

-- -- --

### Preparing Output

Let's have a look at <code class="code-file">output.rb</code> in github.

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/output.rb][dotfiles-dzen2-ruby-output]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/output.rb][dotfiles-lemon-ruby-output]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

-- -- --

### Global Variable and Constant

Constant in Ruby start with capital case.
The <code>@</code> before the variable means it is an instance variable.

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.py</code>
In script, we initialize the variable as below

{% highlight ruby %}
segment_windowtitle = '' # empty string
tags_status = []         # empty array
{% endhighlight %}

Each segment buffered.
And will be called while rendering the panel.

#### Global Constant: Tag Name 

Assuming that herbstclient tag status
only consist of nine number element.

{% highlight bash %}
$ herbstclient tag_status
	#1	:2	:3	:4	:5	.6	.7	.8	.9	
{% endhighlight %}

We can manage custom tag names,
consist of nine string element.
We can also freely using *unicode* string instead of plain one.

<code class="code-file">output.py</code>

{% highlight ruby %}
@TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū']
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.py</code>
Decoration consist lemonbar formatting tag.

{% highlight ruby %}
require_relative 'gmc'
include GMC

# decoration
@SEPARATOR = "%{B-}%{F#{COLOR['yellow500']}}|%{B-}%{F-}"

# Powerline Symbol
@RIGHT_HARD_ARROW = ""
@RIGHT_SOFT_ARROW = ""
@LEFT_HARD_ARROW  = ""
@LEFT_SOFT_ARROW  = ""

# theme
@PRE_ICON    = "%{F#{COLOR['yellow500']}}"
@POST_ICON   = "%{F-}"
{% endhighlight %}

-- -- --

### Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.py</code>

{% highlight ruby %}
def set_tag_value(monitor)
  raw = IO.popen('herbstclient tag_status ' + monitor.to_s).read()
  @tags_status = raw.strip.split("\t")
end
{% endhighlight %}

This function above turn the tag status string
into array of tags for later use.

<code class="code-file">output.py</code>

{% highlight ruby %}
def set_windowtitle(windowtitle)
  icon = @PRE_ICON  + '' + @POST_ICON 

  @segment_windowtitle = " #{icon} %{B-}" \
    + "%{F#{COLOR['grey700']}} #{windowtitle}"
end
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And then returning string as result.

<code class="code-file">output.py</code>

{% highlight ruby %}
def output_by_title()
  text = "#{@segment_windowtitle} #{@SEPARATOR}  ";
end
{% endhighlight %}

-- -- --

### Decorating: Tag Status

This transform each plain tag such as <code>.2</code>,
to decorated tag names such as <code>二 ni</code>.
Note that it only process one tag.
We process all tags in a loop in other function.

This has some parts:

*	Pre Text: Color setting for Main Text
	(Background, Foreground, Underline).
	Arrow before the text, only for active tag.

*	Main Text: Tag Name by number,
	each with their tag state <code>#</code>, <code>+</code>,
	<code>.</code>, <code>|</code>, <code>!</code>,
	and each tag has clickable area setting.

*	Post Text: 
	Arrow after the text, only for active tag.

*	Color Reset: <code>%{B-}</code>,
	<code>%{F-}</code>, <code>%{-u}</code>
	(Background, Foreground, Underline).

<code class="code-file">output.py</code>

{% highlight ruby %}
def output_by_tag(monitor, tag_status)
  tag_index  = tag_status[1..1]
  tag_mark   = tag_status[0..0]
  tag_name   = @TAG_SHOWS[tag_index.to_i - 1] # zero based

  # ----- pre tag
    
  case tag_mark
  when '#'
    text_pre = "%{B#{COLOR['blue500']}}%{F#{COLOR['black']}}" \
             + "%{U#{COLOR['white']}}%{+u}#{@RIGHT_HARD_ARROW}" \
             + "%{B#{COLOR['blue500']}}%{F#{COLOR['white']}}" \
             + "%{U#{COLOR['white']}}%{+u}"
  when '+'
    text_pre = "%{B#{COLOR['yellow500']}}%{F#{COLOR['grey400']}}"
  when ':'
    text_pre = "%{B-}%{F#{COLOR['white']}}" \
             + "%{U#{COLOR['red500']}}%{+u}"
  when '!'
    text_pre = "%{B#{COLOR['red500']}}%{F#{COLOR['white']}}" \
             + "%{U#{COLOR['white']}}%{+u}"
  else
    text_pre = "%{B-}%{F#{COLOR['grey600']}}%{-u}"
  end

  # ----- tag by number

  # clickable tags
  text_name = "%{A:herbstclient focus_monitor \"#{monitor}\" && " \
            + "herbstclient use \"#{tag_index}\":} #{tag_name} %{A} "
    
  # non clickable tags
  # text_name = " #{tag_name} "
    
  # ----- post tag

  if tag_mark == '#'
    text_post = "%{B-}%{F#{COLOR['blue500']}}" \
              + "%{U#{COLOR['red500']}}%{+u}" \
              + @RIGHT_HARD_ARROW;
  else
    text_post = ""        
  end
  
  text_clear = '%{B-}%{F-}%{-u}'
     
  text_pre + text_name + text_post + text_clear
end
{% endhighlight %}

-- -- --

### Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar is using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.py</code>

{% highlight ruby %}
def get_statusbar_text(monitor)
  text = ''

  # draw tags
  #text << '%{l}'
  @tags_status.each { |tag_status| 
    text << output_by_tag(monitor, tag_status) }
    
  # draw window title
  text << '%{r}'
  text << output_by_title()
end
{% endhighlight %}

-- -- --

### Testing The Output

Consider this code <code class="code-file">02-testoutput.rb</code>.
The script using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight ruby %}
#!/usr/bin/ruby
require_relative 'helper'

# process handler
def test_lemon(monitor, parameters)
  require_relative 'output'

  command_out  = 'lemonbar ' + parameters + ' -p'
  IO.popen(command_out, 'w') do |f|   
    # initialize statusbar
    set_tag_value(monitor)
    set_windowtitle('test')
      
    text = get_statusbar_text(monitor)
    f.puts(text)
        
    f.close()    
  end
end

# initialize
panel_height = 24
monitor = get_monitor(ARGV)
lemon_parameters = get_lemon_parameters(monitor, panel_height)

# test
system("herbstclient pad #{monitor} #{panel_height} 0 #{panel_height} 0")
test_lemon(monitor, lemon_parameters)
{% endhighlight %}

This will produce a panel on top.

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

The panel only contain the initialized version of the text.
It does not really interact with the HerbstluftWM event.

You can also click the clickable area to see it's result.
It only show text, not executed yet.

{% highlight bash %}
herbstclient focus_monitor "0" && herbstclient use "2"
herbstclient focus_monitor "0" && herbstclient use "3"
{% endhighlight %}

#### View Source File:

*	**dzen2**: 
	[gitlab.com/.../dotfiles/.../ruby/01-testoutput.rb][dotfiles-dzen2-ruby-testoutput]

*	**lemonbar**: 
	[gitlab.com/.../dotfiles/.../ruby/01-testoutput.rb][dotfiles-lemon-ruby-testoutput]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

-- -- --

### Coming up Next

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

*	[HerbstluftWM Event Idle in Haskell][local-ruby-idle]

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-ruby]:  {{ asset_path }}/hlwm-statusbar-01-tree-ruby.png

[local-ruby-config]: {{ site.url }}/desktop/2017/05/05/herbstlustwm-modularized-ruby.html
[local-ruby-pipe]:   {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-ruby-idle]:   {{ site.url }}/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html

[dotfiles-dzen2-ruby-testparams]:  {{ dotfiles_dzen2 }}/ruby/01-testparams.rb
[dotfiles-dzen2-ruby-testoutput]:  {{ dotfiles_dzen2 }}/ruby/02-testoutput.rb
[dotfiles-dzen2-ruby-panel]:       {{ dotfiles_dzen2 }}/ruby/panel.rb
[dotfiles-dzen2-ruby-gmc]:         {{ dotfiles_dzen2 }}/ruby/gmc.rb
[dotfiles-dzen2-ruby-helper]:      {{ dotfiles_dzen2 }}/ruby/helper.rb
[dotfiles-dzen2-ruby-output]:      {{ dotfiles_dzen2 }}/ruby/output.rb
[dotfiles-dzen2-ruby-pipehandler]: {{ dotfiles_dzen2 }}/ruby/pipehandler.rb

[dotfiles-lemon-ruby-testparams]:  {{ dotfiles_lemon }}/ruby/01-testparams.rb
[dotfiles-lemon-ruby-testoutput]:  {{ dotfiles_lemon }}/ruby/02-testoutput.rb
[dotfiles-lemon-ruby-panel]:       {{ dotfiles_lemon }}/ruby/panel.rb
[dotfiles-lemon-ruby-gmc]:         {{ dotfiles_lemon }}/ruby/gmc.rb
[dotfiles-lemon-ruby-helper]:      {{ dotfiles_lemon }}/ruby/helper.rb
[dotfiles-lemon-ruby-output]:      {{ dotfiles_lemon }}/ruby/output.rb
[dotfiles-lemon-ruby-pipehandler]: {{ dotfiles_lemon }}/ruby/pipehandler.rb
