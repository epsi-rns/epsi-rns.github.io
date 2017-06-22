---
layout: post-sidemenu-wm
title:  "HerbstluftWM Tag Status in BASH"
date:   2017-06-02 17:35:15 +0700
categories: desktop
tags: [coding, bash, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in BASH script.

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

*	[Modularized HerbstluftWM in BASH][local-bash-config]

*	[Piping and Forking in BASH][local-bash-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/][dotfiles-dzen2-bash]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/][dotfiles-lemon-bash]

-- -- --

### HerbstluftWM Tag Status in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Tag Status Overview ][local-overview]]
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
in <code>BASH script</code> directory.

![Statusbar: Directory Structure][image-01-tree-bash]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

### Get Geometry

Let's have a look at <code class="code-file">helper.sh</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/helper.sh][dotfiles-dzen2-bash-helper]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/helper.sh][dotfiles-lemon-bash-helper]

Similar Code (Dzen2): 
[[ BASH Helper ][dotfiles-dzen2-bash-helper]]
[[ Perl Helper ][dotfiles-dzen2-perl-helper]]
[[ Python Helper ][dotfiles-dzen2-python-helper]]
[[ Ruby Helper ][dotfiles-dzen2-ruby-helper]]
[[ PHP Helper ][dotfiles-dzen2-php-helper]]
[[ Lua Helper ][dotfiles-dzen2-lua-helper]]
[[ Haskell Helper ][dotfiles-dzen2-haskell-helper]]

Similar Code (Lemonbar): 
[[ BASH Helper ][dotfiles-lemon-bash-helper]]
[[ Perl Helper ][dotfiles-lemon-perl-helper]]
[[ Python Helper ][dotfiles-lemon-python-helper]]
[[ Ruby Helper ][dotfiles-lemon-ruby-helper]]
[[ PHP Helper ][dotfiles-lemon-php-helper]]
[[ Lua Helper ][dotfiles-lemon-lua-helper]]
[[ Haskell Helper ][dotfiles-lemon-haskell-helper]]

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight bash %}
$ ./panel.sh 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.

The original herbstluftwm example, handle this in oneliner fashion.

{% highlight bash %}
monitor=${1:-0}
{% endhighlight %}

Since we want to port to other language, we need a more clear code.
And for that reason, we wrapped them in function in module.

Here it is our code, with similar result.
I know BASH is cryptic. But we have good  manual in TLDP.
BASH do not have a variable return capability, 
so we have to use global variable <code>$monitor</code>.

<code class="code-file">helper.sh</code>

{% highlight bash %}
# script arguments
function get_monitor() {
    local argument=("$@")
    local num_args=${#argument[@]}

    # ternary operator
    [[ $num_args > 0 ]] && monitor=${argument[0]} || monitor=0
}
{% endhighlight %}

And in main code we can call

{% highlight bash %}
DIR=$(dirname "$0")
. ${DIR}/helper.sh

get_monitor ${@}
echo $monitor
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
And use <code>$geometry</code> as global variable.

<code class="code-file">helper.sh</code>

{% highlight bash %}
function get_geometry() {
    local monitor=$1;
    geometry=( $(herbstclient monitor_rect "$monitor") )
    if [ -z "$geometry" ] ;then
        echo "Invalid monitor $monitor"
        exit 1
    fi
}
{% endhighlight %}

Consider call this function from script later.
It is a little bit tricky,
because the variable <code>$geometry"</code> has space in it,
we have to wrap it in <code>"${geometry[@]}"</code>.

{% highlight bash %}
get_monitor ${@}
get_geometry $monitor
echo ${geometry[@]}
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
<code class="code-file">helper.sh</code>

{% highlight bash %}
function get_bottom_panel_geometry() {
   local panel_height=$1
   shift
   local geometry=("$@")
   
   # geometry has the format X Y W H
     xpos=$(( ${geometry[0]} + 24 ))
     ypos=$(( ${geometry[3]} - $panel_height ))
    width=$(( ${geometry[2]} - 48 ))
   height=$panel_height
}
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight bash %}
panel_height=24
get_monitor ${@}
get_geometry $monitor
get_bottom_panel_geometry $panel_height "${geometry[@]}"

echo "Lemonbar geometry: ${width}x${height}+${xpos}+${ypos}"
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

<code class="code-file">helper.sh</code>

{% highlight bash %}
function get_lemon_parameters() {  
    # parameter: function argument
    local monitor=$1
    local panel_height=$2

    # calculate geometry
    get_geometry $monitor
    get_top_panel_geometry $panel_height "${geometry[@]}"
    
    # geometry: -g widthxheight+x+y
    geom_res="${width}x${height}+${xpos}+${ypos}"
    
    # color, with transparency
    local bgcolor="#aa000000"
    local fgcolor="#ffffff"
    
    # XFT: require lemonbar_xft_git 
    local font_takaop="takaopgothic-9"
    local font_bottom="monospace-9"
    local font_symbol="PowerlineSymbols-11"
    local font_awesome="FontAwesome-9"

    # finally
    lemon_parameters="  -g $geom_res -u 2"
    lemon_parameters+=" -B $bgcolor -F $fgcolor" 
    lemon_parameters+=" -f $font_takaop -f $font_awesome -f $font_symbol" 
}
{% endhighlight %}

-- -- --

### Testing The Parameters

Consider this code <code class="code-file">01-testparams.sh</code>.
The script call the above function to get lemon parameters.

{% highlight bash %}
#!/usr/bin/env bash

# libraries
DIR=$(dirname "$0")
. ${DIR}/helper.sh

# initialize
panel_height=24
get_monitor ${@}

get_lemon_parameters $monitor $panel_height
echo $lemon_parameters 
{% endhighlight %}

This will produce output
something similar to this result

{% highlight conf %}
-g 1280x24+0+0 -u 2 -B #aa000000 -F #ffffff 
-f takaopgothic-9 -f FontAwesome-9 -f PowerlineSymbols-11
{% endhighlight %}

Or in Dzen2 version:

{% highlight conf %}
-x 0 -y 0 -w 1280 -h 24 -ta l 
-bg #000000 -fg #ffffff -title-name dzentop 
-fn -*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/01-testparams.sh][dotfiles-dzen2-bash-testparams]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/01-testparams.sh][dotfiles-lemon-bash-testparams]

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

{% highlight bash %}
herbstclient pad $monitor $panel_height 0 $panel_height 0
{% endhighlight %}

-- -- --

### Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

<code class="code-file">gmc.sh</code>

{% highlight bash %}
declare -A color=(
    ['white']='#ffffff'
    ['black']='#000000'

    ['grey50']='#fafafa'
    ['grey100']='#f5f5f5'
)
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/gmc.sh][dotfiles-dzen2-bash-gmc]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/gmc.sh][dotfiles-lemon-bash-gmc]

Similar Code (Dzen2): 
[[ BASH Color ][dotfiles-dzen2-bash-gmc]]
[[ Perl Color ][dotfiles-dzen2-perl-gmc]]
[[ Python Color ][dotfiles-dzen2-python-gmc]]
[[ Ruby Color ][dotfiles-dzen2-ruby-gmc]]
[[ PHP Color ][dotfiles-dzen2-php-gmc]]
[[ Lua Color ][dotfiles-dzen2-lua-gmc]]
[[ Haskell Color ][dotfiles-dzen2-haskell-gmc]]

Similar Code (Lemon): 
[[ BASH Color ][dotfiles-lemon-bash-gmc]]
[[ Perl Color ][dotfiles-lemon-perl-gmc]]
[[ Python Color ][dotfiles-lemon-python-gmc]]
[[ Ruby Color ][dotfiles-lemon-ruby-gmc]]
[[ PHP Color ][dotfiles-lemon-php-gmc]]
[[ Lua Color ][dotfiles-lemon-lua-gmc]]
[[ Haskell Color ][dotfiles-lemon-haskell-gmc]]

-- -- --

### Preparing Output

Let's have a look at <code class="code-file">output.sh</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../bash/output.sh][dotfiles-dzen2-bash-output]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../bash/output.sh][dotfiles-lemon-bash-output]

Similar Code (Dzen2): 
[[ BASH Output ][dotfiles-dzen2-bash-output]]
[[ Perl Output ][dotfiles-dzen2-perl-output]]
[[ Python Output ][dotfiles-dzen2-python-output]]
[[ Ruby Output ][dotfiles-dzen2-ruby-output]]
[[ PHP Output ][dotfiles-dzen2-php-output]]
[[ Lua Output ][dotfiles-dzen2-lua-output]]
[[ Haskell Output ][dotfiles-dzen2-haskell-output]]

Similar Code (Lemonbar): 
[[ BASH Output ][dotfiles-lemon-bash-output]]
[[ Perl Output ][dotfiles-lemon-perl-output]]
[[ Python Output ][dotfiles-lemon-python-output]]
[[ Ruby Output ][dotfiles-lemon-ruby-output]]
[[ PHP Output ][dotfiles-lemon-php-output]]
[[ Lua Output ][dotfiles-lemon-lua-output]]
[[ Haskell Output ][dotfiles-lemon-haskell-output]]

-- -- --

### Global Variable and Constant

There are ways to define constant in BASH.
Constant in PHP may begin with the word <code>readonly</code>.

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.sh</code>
In script, we initialize the variable as below

{% highlight bash %}
segment_windowtitle=''; # empty string
tags_status=();         # empty array
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

<code class="code-file">output.sh</code>

{% highlight bash %}
readonly tag_shows=( "一 ichi" "二 ni" "三 san" "四 shi" 
  "五 go" "六 roku" "七 shichi" "八 hachi" "九 kyū" "十 jū")
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.sh</code>
Decoration consist lemonbar formatting tag.

{% highlight bash %}
readonly separator="%{B-}%{F${color['yellow500']}}|%{B-}%{F-}"

# powerline symbol
readonly right_hard_arrow=""
readonly right_soft_arrow=""
readonly  left_hard_arrow=""
readonly  left_soft_arrow=""

# theme
readonly  pre_icon="%{F${color['yellow500']}}"
readonly post_icon="%{F-}
{% endhighlight %}

-- -- --

### Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.sh</code>

{% highlight bash %}
function set_tag_value() {
  IFS=$'\t' read -ra tags_status <<< "$(herbstclient tag_status $monitor)"
}
{% endhighlight %}

This IFS above turn the tag status string
into array of tags for later use.

<code class="code-file">output.sh</code>

{% highlight bash %}
function set_windowtitle() {
    local windowtitle=$1
    local icon="$pre_icon$post_icon"
    
    segment_windowtitle=" $icon %{B-}%{F${color['grey700']}} $windowtitle"
}
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And put the result in <code>$buffer</code>
because BASH can't return a string.

<code class="code-file">output.sh</code>

{% highlight bash %}
function output_by_title() {
    local text="$segment_windowtitle $separator  "
    buffer=$text
}
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

<code class="code-file">output.sh</code>

{% highlight bash %}
function output_by_tag() {
    local    monitor=$1    
    local tag_status=$2    
        
    local  tag_index=${tag_status:1:1}
    local   tag_mark=${tag_status:0:1}
    local   tag_name=${tag_shows[$tag_index - 1]}; # zero based

    # ----- pre tag

    local text_pre=''
    case $tag_mark in
        '#') text_pre+="%{B${color['blue500']}}%{F${color['black']}}"
             text_pre+="%{U${color['white']}}%{+u}$right_hard_arrow"
             text_pre+="%{B${color['blue500']}}%{F${color['white']}}"
             text_pre+="%{U${color['white']}}%{+u}"
        ;;
        '+') text_pre+="%{B${color['yellow500']}}%{F${color['grey400']}}"
        ;;
        ':') text_pre+="%{B-}%{F${color['white']}}"
             text_pre+="%{U${color['red500']}}%{+u}"
        ;;
        '!') text_pre+="%{B${color['red500']}}%{F${color['white']}}"
             text_pre+="%{U${color['white']}}%{+u}"
        ;;
        *)   text_pre+="%{B-}%{F${color['grey600']}}%{-u}"
        ;;
    esac

    # ----- tag by number
    
    # clickable tags
    local text_name=''
    text_name+="%{A:herbstclient focus_monitor \"$monitor\" && "
    text_name+="herbstclient use \"$tag_index\":} $tag_name %{A} "
  
    # non clickable tags
    # local text_name=" $tag_name "
    
    # ----- post tag

    local text_post=''
    if [ $tag_mark = '#' ]
    then        
        text_post+="%{B-}%{F${color['blue500']}}"
        text_post+="%{U${color['red500']}}%{+u}${right_hard_arrow}";
    fi
    
    text_clear='%{B-}%{F-}%{-u}'
     
    buffer="$text_pre$text_name$text_post$text_clear"
}
{% endhighlight %}

-- -- --

### Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar is using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.sh</code>

{% highlight bash %}
function get_statusbar_text() {
    local monitor=$1
    local text=''

    # draw tags
    text+='%{l}'
    for tag_status in "${tags_status[@]}"
    do
        output_by_tag $monitor $tag_status
        text+=$buffer
    done
    
    # draw window title
    text+='%{r}'
    output_by_title    
    text+=$buffer
    
    buffer=$text
}
{% endhighlight %}

-- -- --

### Testing The Output

Consider this code <code class="code-file">02-testoutput.sh</code>.
The script using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight bash %}
#!/usr/bin/env bash

# libraries
DIR=$(dirname "$0")

. ${DIR}/gmc.sh
. ${DIR}/helper.sh
. ${DIR}/output.sh

# process handler

function test_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    command_out="lemonbar $parameters -p"
    
    {
      # initialize statusbar
      set_tag_value $monitor
      set_windowtitle 'test'

      get_statusbar_text $monitor
      echo $buffer
    } | $command_out

}

# initialize

panel_height=24
get_monitor ${@}
get_lemon_parameters $monitor $panel_height

# test
herbstclient pad $monitor $panel_height 0 $panel_height 0
test_lemon $monitor $lemon_parameters
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
	[github.com/.../dotfiles/.../bash/01-testoutput.sh][dotfiles-dzen2-bash-testoutput]

*	**lemonbar**: 
	[github.com/.../dotfiles/.../bash/01-testoutput.sh][dotfiles-lemon-bash-testoutput]

-- -- --

### Coming up Next

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

*	[HerbstluftWM Event Idle in Haskell][local-bash-idle]

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-bash]:  {{ asset_path }}/hlwm-statusbar-01-tree-bash.png

[local-bash-config]: {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-bash-pipe]:   {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-bash-idle]:   {{ site.url }}/desktop/2017/06/12/herbstlustwm-event-idle-bash.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/02/herbstlustwm-tag-status-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/03/herbstlustwm-tag-status-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/04/herbstlustwm-tag-status-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/06/herbstlustwm-tag-status-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html

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

[dotfiles-dzen2-bash-testparams]:  {{ dotfiles_dzen2 }}/bash/01-testparams.sh
[dotfiles-dzen2-bash-testoutput]:  {{ dotfiles_dzen2 }}/bash/02-testoutput.sh
[dotfiles-dzen2-bash-panel]:       {{ dotfiles_dzen2 }}/bash/panel.sh
[dotfiles-dzen2-bash-gmc]:         {{ dotfiles_dzen2 }}/bash/gmc.sh
[dotfiles-dzen2-bash-helper]:      {{ dotfiles_dzen2 }}/bash/helper.sh
[dotfiles-dzen2-bash-output]:      {{ dotfiles_dzen2 }}/bash/output.sh
[dotfiles-dzen2-bash-pipehandler]: {{ dotfiles_dzen2 }}/bash/pipehandler.sh

[dotfiles-dzen2-perl-testparams]:  {{ dotfiles_dzen2 }}/perl/01-testparams.pl
[dotfiles-dzen2-perl-testoutput]:  {{ dotfiles_dzen2 }}/perl/02-testoutput.pl
[dotfiles-dzen2-perl-panel]:       {{ dotfiles_dzen2 }}/perl/panel.pl
[dotfiles-dzen2-perl-gmc]:         {{ dotfiles_dzen2 }}/perl/gmc.pm
[dotfiles-dzen2-perl-helper]:      {{ dotfiles_dzen2 }}/perl/helper.pm
[dotfiles-dzen2-perl-output]:      {{ dotfiles_dzen2 }}/perl/output.pm
[dotfiles-dzen2-perl-pipehandler]: {{ dotfiles_dzen2 }}/perl/pipehandler.pm

[dotfiles-dzen2-python-testparams]:  {{ dotfiles_dzen2 }}/python/01-testparams.py
[dotfiles-dzen2-python-testoutput]:  {{ dotfiles_dzen2 }}/python/02-testoutput.py
[dotfiles-dzen2-python-panel]:       {{ dotfiles_dzen2 }}/python/panel.py
[dotfiles-dzen2-python-gmc]:         {{ dotfiles_dzen2 }}/python/gmc.py
[dotfiles-dzen2-python-helper]:      {{ dotfiles_dzen2 }}/python/helper.py
[dotfiles-dzen2-python-output]:      {{ dotfiles_dzen2 }}/python/output.py
[dotfiles-dzen2-python-pipehandler]: {{ dotfiles_dzen2 }}/python/pipehandler.py

[dotfiles-dzen2-ruby-testparams]:  {{ dotfiles_dzen2 }}/ruby/01-testparams.rb
[dotfiles-dzen2-ruby-testoutput]:  {{ dotfiles_dzen2 }}/ruby/02-testoutput.rb
[dotfiles-dzen2-ruby-panel]:       {{ dotfiles_dzen2 }}/ruby/panel.rb
[dotfiles-dzen2-ruby-gmc]:         {{ dotfiles_dzen2 }}/ruby/gmc.rb
[dotfiles-dzen2-ruby-helper]:      {{ dotfiles_dzen2 }}/ruby/helper.rb
[dotfiles-dzen2-ruby-output]:      {{ dotfiles_dzen2 }}/ruby/output.rb
[dotfiles-dzen2-ruby-pipehandler]: {{ dotfiles_dzen2 }}/ruby/pipehandler.rb

[dotfiles-dzen2-php-testparams]:  {{ dotfiles_dzen2 }}/php/01-testparams.php
[dotfiles-dzen2-php-testoutput]:  {{ dotfiles_dzen2 }}/php/02-testoutput.php
[dotfiles-dzen2-php-panel]:       {{ dotfiles_dzen2 }}/php/panel.php
[dotfiles-dzen2-php-gmc]:         {{ dotfiles_dzen2 }}/php/gmc.php
[dotfiles-dzen2-php-helper]:      {{ dotfiles_dzen2 }}/php/helper.php
[dotfiles-dzen2-php-output]:      {{ dotfiles_dzen2 }}/php/output.php
[dotfiles-dzen2-php-pipehandler]: {{ dotfiles_dzen2 }}/php/pipehandler.php

[dotfiles-dzen2-lua-testparams]:  {{ dotfiles_dzen2 }}/lua/01-testparams.lua
[dotfiles-dzen2-lua-testoutput]:  {{ dotfiles_dzen2 }}/lua/02-testoutput.lua
[dotfiles-dzen2-lua-panel]:       {{ dotfiles_dzen2 }}/lua/panel.lua
[dotfiles-dzen2-lua-gmc]:         {{ dotfiles_dzen2 }}/lua/gmc.lua
[dotfiles-dzen2-lua-common]:      {{ dotfiles_dzen2 }}/lua/common.lua
[dotfiles-dzen2-lua-helper]:      {{ dotfiles_dzen2 }}/lua/helper.lua
[dotfiles-dzen2-lua-output]:      {{ dotfiles_dzen2 }}/lua/output.lua
[dotfiles-dzen2-lua-pipehandler]: {{ dotfiles_dzen2 }}/lua/pipehandler.lua

[dotfiles-dzen2-haskell-testparams]:  {{ dotfiles_dzen2 }}/haskell/01-testparams.hs
[dotfiles-dzen2-haskell-testoutput]:  {{ dotfiles_dzen2 }}/haskell/02-testoutput.hs
[dotfiles-dzen2-haskell-panel]:       {{ dotfiles_dzen2 }}/haskell/panel.hs
[dotfiles-dzen2-haskell-gmc]:         {{ dotfiles_dzen2 }}/haskell/MyGMC.hs
[dotfiles-dzen2-haskell-helper]:      {{ dotfiles_dzen2 }}/haskell/MyHelper.hs
[dotfiles-dzen2-haskell-output]:      {{ dotfiles_dzen2 }}/haskell/MyOutput.hs
[dotfiles-dzen2-haskell-pipehandler]: {{ dotfiles_dzen2 }}/haskell/MyPipeHandler.hs

[dotfiles-lemon-bash-testparams]:  {{ dotfiles_lemon }}/bash/01-testparams.sh
[dotfiles-lemon-bash-testoutput]:  {{ dotfiles_lemon }}/bash/02-testoutput.sh
[dotfiles-lemon-bash-panel]:       {{ dotfiles_lemon }}/bash/panel.sh
[dotfiles-lemon-bash-gmc]:         {{ dotfiles_lemon }}/bash/gmc.sh
[dotfiles-lemon-bash-helper]:      {{ dotfiles_lemon }}/bash/helper.sh
[dotfiles-lemon-bash-output]:      {{ dotfiles_lemon }}/bash/output.sh
[dotfiles-lemon-bash-pipehandler]: {{ dotfiles_lemon }}/bash/pipehandler.sh

[dotfiles-lemon-perl-testparams]:  {{ dotfiles_lemon }}/perl/01-testparams.pl
[dotfiles-lemon-perl-testoutput]:  {{ dotfiles_lemon }}/perl/02-testoutput.pl
[dotfiles-lemon-perl-panel]:       {{ dotfiles_lemon }}/perl/panel.pl
[dotfiles-lemon-perl-gmc]:         {{ dotfiles_lemon }}/perl/gmc.pm
[dotfiles-lemon-perl-helper]:      {{ dotfiles_lemon }}/perl/helper.pm
[dotfiles-lemon-perl-output]:      {{ dotfiles_lemon }}/perl/output.pm
[dotfiles-lemon-perl-pipehandler]: {{ dotfiles_lemon }}/perl/pipehandler.pm

[dotfiles-lemon-python-testparams]:  {{ dotfiles_lemon }}/python/01-testparams.py
[dotfiles-lemon-python-testoutput]:  {{ dotfiles_lemon }}/python/02-testoutput.py
[dotfiles-lemon-python-panel]:       {{ dotfiles_lemon }}/python/panel.py
[dotfiles-lemon-python-gmc]:         {{ dotfiles_lemon }}/python/gmc.py
[dotfiles-lemon-python-helper]:      {{ dotfiles_lemon }}/python/helper.py
[dotfiles-lemon-python-output]:      {{ dotfiles_lemon }}/python/output.py
[dotfiles-lemon-python-pipehandler]: {{ dotfiles_lemon }}/python/pipehandler.py

[dotfiles-lemon-ruby-testparams]:  {{ dotfiles_lemon }}/ruby/01-testparams.rb
[dotfiles-lemon-ruby-testoutput]:  {{ dotfiles_lemon }}/ruby/02-testoutput.rb
[dotfiles-lemon-ruby-panel]:       {{ dotfiles_lemon }}/ruby/panel.rb
[dotfiles-lemon-ruby-gmc]:         {{ dotfiles_lemon }}/ruby/gmc.rb
[dotfiles-lemon-ruby-helper]:      {{ dotfiles_lemon }}/ruby/helper.rb
[dotfiles-lemon-ruby-output]:      {{ dotfiles_lemon }}/ruby/output.rb
[dotfiles-lemon-ruby-pipehandler]: {{ dotfiles_lemon }}/ruby/pipehandler.rb

[dotfiles-lemon-php-testparams]:  {{ dotfiles_lemon }}/php/01-testparams.php
[dotfiles-lemon-php-testoutput]:  {{ dotfiles_lemon }}/php/02-testoutput.php
[dotfiles-lemon-php-panel]:       {{ dotfiles_lemon }}/php/panel.php
[dotfiles-lemon-php-gmc]:         {{ dotfiles_lemon }}/php/gmc.php
[dotfiles-lemon-php-helper]:      {{ dotfiles_lemon }}/php/helper.php
[dotfiles-lemon-php-output]:      {{ dotfiles_lemon }}/php/output.php
[dotfiles-lemon-php-pipehandler]: {{ dotfiles_lemon }}/php/pipehandler.php

[dotfiles-lemon-lua-testparams]:  {{ dotfiles_lemon }}/lua/01-testparams.lua
[dotfiles-lemon-lua-testoutput]:  {{ dotfiles_lemon }}/lua/02-testoutput.lua
[dotfiles-lemon-lua-panel]:       {{ dotfiles_lemon }}/lua/panel.lua
[dotfiles-lemon-lua-gmc]:         {{ dotfiles_lemon }}/lua/gmc.lua
[dotfiles-lemon-lua-common]:      {{ dotfiles_lemon }}/lua/common.lua
[dotfiles-lemon-lua-helper]:      {{ dotfiles_lemon }}/lua/helper.lua
[dotfiles-lemon-lua-output]:      {{ dotfiles_lemon }}/lua/output.lua
[dotfiles-lemon-lua-pipehandler]: {{ dotfiles_lemon }}/lua/pipehandler.lua

[dotfiles-lemon-haskell-testparams]:  {{ dotfiles_lemon }}/haskell/01-testparams.hs
[dotfiles-lemon-haskell-testoutput]:  {{ dotfiles_lemon }}/haskell/02-testoutput.hs
[dotfiles-lemon-haskell-panel]:       {{ dotfiles_lemon }}/haskell/panel.hs
[dotfiles-lemon-haskell-gmc]:         {{ dotfiles_lemon }}/haskell/MyGMC.hs
[dotfiles-lemon-haskell-helper]:      {{ dotfiles_lemon }}/haskell/MyHelper.hs
[dotfiles-lemon-haskell-output]:      {{ dotfiles_lemon }}/haskell/MyOutput.hs
[dotfiles-lemon-haskell-pipehandler]: {{ dotfiles_lemon }}/haskell/MyPipeHandler.hs
