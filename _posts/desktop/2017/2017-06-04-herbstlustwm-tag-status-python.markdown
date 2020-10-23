---
layout     : post
title      : "HerbstluftWM Tag Status in Python"
date       : 2017-06-04 17:35:15 +0700
categories : desktop
tags       : [coding, python, herbstluftwm, statusbar]
keywords   : [tag status, lemonbar, dzen2]
author     : epsi
toc        : toc/2017/06/herbstlustwm-tag-status.html

opengraph:
  image: /assets/site/images/topics/python.png

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in Python script.

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

<a name="preface"></a>

### Preface

> Goal: Show the Herbstclient Tag.

	Focusing in "herbstclient tag_status". 

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

This tutorial cover Lemonbar, and in order to use Dzen2,
any reader could use the source code in github.

#### Table of Content

* [Preface](#preface): Table of Content

* [Reference](#reference)

* [Screenshot](#screenshot)

* 1: [Directory Structure](#directory-structure)

* 2: [Get Geometry](#get-geometry)

* 3: [Testing The Parameters](#testing-params)

* 4: [Adjusting the Desktop](#adjusting-desktop)

* 5: [Color Schemes](#color-schemes)

* 6: [Preparing Output](#preparing-output)

* 7: [Global Variable and Constant](#global-vars)

* 8: [Segment Variable](#segment-var)

* 9: [Decorating: Window Title](#windows-title)

* 10: [Decorating: Tag Status](#tag-status)

* 11: [Combine The Segments](#combine-segments)

* 12: [Testing The Output](#testing-output)

* [Coming up Next](#whats-next)

-- -- --

<a name="reference"></a>

### Reference

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[HerbstluftWM Tag Status Overview][local-overview]

*	[Modularized HerbstluftWM in Python][local-python-config]

*	[Piping and Forking in Python][local-python-pipe]

#### All The Source Code

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/][dotfiles-dzen2-python]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/][dotfiles-lemon-python]

-- -- --

<a name="screenshot"></a>

### Screenshot

Since window manager is out of topic in this tutorial,
I present **only panel** HerbstluftWM screenshot.

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

<a name="directory-structure"></a>

### 1: Directory Structure

Directory Structure has been explained in preface. 
For both Dzen2 and Lemonbar, the structure are the same.
This figure will explain how it looks
in <code>Python script</code> directory.

![Statusbar: Directory Structure][image-01-tree-python]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

<a name="get-geometry"></a>

### 2: Get Geometry

Let's have a look at <code class="code-file">helper.py</code> in github.

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/helper.py][dotfiles-dzen2-python-helper]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/helper.py][dotfiles-lemon-python-helper]

{% include toc/2017/06/herbstlustwm-tag-status-helper.html %}

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight bash %}
$ ./panel.py 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.
Here it is our code in Python.

<code class="code-file">helper.py</code>

{% highlight python %}
# script arguments
def get_monitor(arguments):
    # ternary operator
    monitor = int(arguments[1]) if (len(arguments) > 1) else 0

    return monitor
{% endhighlight %}

And in main code we can call

{% highlight python %}
import sys
import helper

monitor  = helper.get_monitor(sys.argv)
print(monitor)
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

<code class="code-file">helper.py</code>

{% highlight python %}
def get_geometry(monitor):
    raw = os.popen('herbstclient monitor_rect '+ str(monitor)).read()

    if not raw: 
        print('Invalid monitor ' + str(monitor))
        exit(1)
    
    geometry = raw.rstrip().split(' ')
    
    return geometry
{% endhighlight %}

Consider call this function from script later.
To print array in Python,
we just have to wrap it in <code>' '.join(geometry)</code>.

{% highlight python %}
monitor  = helper.get_monitor(sys.argv)
geometry = helper.get_geometry(monitor)
print(' '.join(geometry))
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
<code class="code-file">helper.py</code>

{% highlight python %}
def get_bottom_panel_geometry(height, geometry):
    # geometry has the format X Y W H
    return (int(geometry[0]) + 24, int(geometry[3])-height, 
            int(geometry[2]) - 48, height )
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight python %}
panel_height = 24
monitor  = helper.get_monitor(sys.argv)
geometry = helper.get_geometry(monitor)
xpos, ypos, width, height = helper.get_bottom_panel_geometry(
       panel_height, geometry)

print('Lemonbar geometry: ' + 
    str(width)+'x'+str(height)+'+'+str(xpos)+'+'+str(ypos) )
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

<code class="code-file">helper.py</code>

{% highlight python %}
def get_lemon_parameters(monitor, panel_height):  
    # calculate geometry
    geometry = get_geometry(monitor)
    xpos, ypos, width, height = get_top_panel_geometry(
       panel_height, geometry)

    # geometry: -g widthxheight+x+y
    geom_res = str(width)+'x'+str(height)+'+'+str(xpos)+'+'+str(ypos)

    # color, with transparency    
    bgcolor = "'#aa000000'"
    fgcolor = "'#ffffff'"
    
    # XFT: require lemonbar_xft_git 
    font_takaop  = "takaopgothic-9"
    font_bottom  = "monospace-9"
    font_symbol  = "PowerlineSymbols-11"
    font_awesome = "FontAwesome-9"

    # finally
    parameters  = ' -g '+geom_res+' -u 2 ' \
                + ' -B '+bgcolor+' -F '+fgcolor \
                + ' -f '+font_takaop+' -f '+font_awesome+' -f '+font_symbol

    return parameters
{% endhighlight %}

-- -- --

<a name="testing-params"></a>

### 3: Testing The Parameters

Consider this code <code class="code-file">01-testparams.py</code>.
The script call the above function to get lemon parameters.

{% highlight python %}
#!/usr/bin/env python3

import os
import sys
import helper

# initialize
panel_height = 24
monitor = helper.get_monitor(sys.argv)

lemon_parameters = helper.get_lemon_parameters(monitor, panel_height)
print(lemon_parameters)
{% endhighlight %}

This will produce output
something similar to this result

{% highlight conf %}
-g 1280x24+0+0 -u 2  -B '#aa000000' -F '#ffffff' 
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
	[gitlab.com/.../dotfiles/.../python/01-testparams.py][dotfiles-dzen2-python-testparams]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/01-testparams.py][dotfiles-lemon-python-testparams]

{% include toc/2017/06/herbstlustwm-tag-status-testparams.html %}

-- -- --

<a name="adjusting-desktop"></a>

### 4: Adjusting the Desktop

Since we want to use panel, we have to adjust the desktop gap,
giving space at the top and bottom.

{% highlight bash %}
$ herbstclient pad 0 24 0 24 0
{% endhighlight %}

For more information, do <code>$ man herbsluftclient</code>,
and type <code>\pad</code> to search what it means.

In script, it looks like this below.

{% highlight python %}
os.system('herbstclient pad ' + str(monitor) + ' ' 
    + str(panel_height) + ' 0 ' + str(panel_height) + ' 0'
{% endhighlight %}

-- -- --

<a name="color-schemes"></a>

### 5: Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

<code class="code-file">gmc.py</code>

{% highlight python %}
color = {
    'white' : '#ffffff', 
    'black' : '#000000',

    'grey50'      : '#fafafa',
    'grey100'     : '#f5f5f5'
}
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/gmc.py][dotfiles-dzen2-python-gmc]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/gmc.py][dotfiles-lemon-python-gmc]

{% include toc/2017/06/herbstlustwm-tag-status-gmc.html %}

-- -- --

<a name="preparing-output"></a>

### 6: Preparing Output

Let's have a look at <code class="code-file">output.py</code> in github.

#### View Source File:

*	**Dzen2**: 
	[gitlab.com/.../dotfiles/.../python/output.py][dotfiles-dzen2-python-output]

*	**Lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/output.py][dotfiles-lemon-python-output]

{% include toc/2017/06/herbstlustwm-tag-status-output.html %}

-- -- --

<a name="global-vars"></a>

### 7: Global Variable and Constant

Officialy there is a no way to define constant in Python.
Python does not differ between these two,
for that reason we distinguish global constant with capital case.

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.py</code>
In script, we initialize the variable as below

{% highlight python %}
segment_windowtitle = '' # empty string
tags_status = []         # empty list
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

{% highlight python %}
TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
    '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū']
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.py</code>
Decoration consist lemonbar formatting tag.

{% highlight python %}
from gmc import color

# decoration
SEPARATOR = '%{B-}%{F' + color['yellow500'] + '}|%{B-}%{F-}'

# Powerline Symbol
RIGHT_HARD_ARROW = ""
RIGHT_SOFT_ARROW = ""
LEFT_HARD_ARROW  = ""
LEFT_SOFT_ARROW  = ""

# theme
PRE_ICON    = '%{F' + color['yellow500'] + '}'
POST_ICON   = '%{F-}'
{% endhighlight %}

-- -- --

<a name="segment-var"></a>

### 8: Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.py</code>

{% highlight python %}
def set_tag_value(monitor):
    global tags_status

    raw = os.popen('herbstclient tag_status ' + str(monitor)).read()
    raw = raw.strip()
    tags_status = raw.split("\t")
{% endhighlight %}

This function above turn the tag status string
into array of tags for later use.

<code class="code-file">output.py</code>

{% highlight python %}
def set_windowtitle(windowtitle):
    global segment_windowtitle
    icon = PRE_ICON + '' + POST_ICON

    segment_windowtitle = ' ' + icon + \
        ' %{B-}%{F' + color['grey700'] + '} ' + windowtitle
{% endhighlight %}

We will call these two functions later.

-- -- --

<a name="windows-title"></a>

### 9: Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And then returning string as result.

<code class="code-file">output.py</code>

{% highlight python %}
def output_by_title():
    text = segment_windowtitle + ' ' + SEPARATOR + '  '

    return text
{% endhighlight %}

-- -- --

<a name="tag-status"></a>

### 10: Decorating: Tag Status

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

{% highlight python %}
def output_by_tag(monitor, tag_status):
    tag_index  = tag_status[1:2]
    tag_mark   = tag_status[0:1]
    tag_name   = TAG_SHOWS[int(tag_index) - 1] # zero based

    # ----- pre tag

    if tag_mark == '#':
        text_pre = '%{B' + color['blue500'] + '}' \
                   '%{F' + color['black'] + '}' \
                   '%{U' + color['white'] + '}%{+u}' \
                 + RIGHT_HARD_ARROW \
                 + '%{B' + color['blue500'] + '}' \
                   '%{F' + color['white'] + '}' \
                   '%{U' + color['white'] + '}%{+u}'
    elif tag_mark == '+':
        text_pre = '%{B' + color['yellow500'] + '}' \
                   '%{F' + color['grey400'] + '}'
    elif tag_mark == ':':
        text_pre = '%{B-}%{F' + color['white'] + '}' \
                   '%{U' + color['red500'] + '}%{+u}'
    elif tag_mark == '!':
        text_pre = '%{B' + color['red500'] + '}' \
                   '%{F' + color['white'] + '}' \
                   '%{U' + color['white'] + '}%{+u}'
    else:
        text_pre = '%{B-}%{F' + color['grey600'] + '}%{-u}'

    # ----- tag by number
    
    # clickable tags
    text_name = '%{A:herbstclient focus_monitor "' \
              + str(monitor) + '" && ' + 'herbstclient use "' \
              + tag_index + '":} ' + tag_name + ' %{A} '

    # non clickable tags
    # text_name = ' ' + tag_name + ' '
    
    # ----- post tag

    if tag_mark == '#':
        text_post = '%{B-}' \
                    '%{F' + color['blue500'] + '}' \
                    '%{U' + color['red500'] + '}%{+u}' \
                  + RIGHT_HARD_ARROW
    else: 
        text_post = ''
    
    text_clear = '%{B-}%{F-}%{-u}';
     
    return (text_pre + text_name + text_post + text_clear)
{% endhighlight %}

-- -- --

<a name="combine-segments"></a>

### 11: Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar is using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.py</code>

{% highlight python %}
def get_statusbar_text(monitor):
    text = ''

    # draw tags
    text += '%{l}'
    for tag_status in tags_status:
        text += output_by_tag(monitor, tag_status)
    
    # draw window title    
    text += '%{r}'
    text += output_by_title()
    
    return text
{% endhighlight %}

-- -- --

<a name="testing-output"></a>

### 12: Testing The Output

Consider this code <code class="code-file">02-testoutput.py</code>.
The script using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight python %}
#!/usr/bin/env python3

import os
import sys
import helper

# process handler
def test_lemon(monitor, parameters): 
    import subprocess
    import output
 
    command_out  = 'lemonbar ' + parameters + ' -p'

    pipe_out = subprocess.Popen(
            [command_out], 
            stdin  = subprocess.PIPE,
            shell  = True,
            universal_newlines=True
        )

    # initialize statusbar
    output.set_tag_value(monitor)
    output.set_windowtitle('test')
        
    text = output.get_statusbar_text(monitor)
    pipe_out.stdin.write(text + '\n')
    pipe_out.stdin.flush()

    pipe_out.stdin.close()

# initialize
panel_height = 24
monitor = helper.get_monitor(sys.argv)
lemon_parameters = helper.get_lemon_parameters(monitor, panel_height)

# test
os.system('herbstclient pad ' + str(monitor) + ' ' 
    + str(panel_height) + ' 0 ' + str(panel_height) + ' 0')

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
	[gitlab.com/.../dotfiles/.../python/01-testoutput.py][dotfiles-dzen2-python-testoutput]

*	**lemonbar**: 
	[gitlab.com/.../dotfiles/.../python/01-testoutput.py][dotfiles-lemon-python-testoutput]

{% include toc/2017/06/herbstlustwm-tag-status-output.html %}

-- -- --

<a name="whats-next"></a>

### Coming up Next

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

*	[HerbstluftWM Event Idle in Haskell][local-python-idle]

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets-desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-python]:  {{ asset_path }}/hlwm-statusbar-01-tree-python.png

[local-python-config]: {{ site.url }}/desktop/2017/05/04/herbstlustwm-modularized-python.html
[local-python-pipe]:   {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-python-idle]:   {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html

[dotfiles-dzen2-python-testparams]:  {{ dotfiles_dzen2 }}/python/01-testparams.py
[dotfiles-dzen2-python-testoutput]:  {{ dotfiles_dzen2 }}/python/02-testoutput.py
[dotfiles-dzen2-python-panel]:       {{ dotfiles_dzen2 }}/python/panel.py
[dotfiles-dzen2-python-gmc]:         {{ dotfiles_dzen2 }}/python/gmc.py
[dotfiles-dzen2-python-helper]:      {{ dotfiles_dzen2 }}/python/helper.py
[dotfiles-dzen2-python-output]:      {{ dotfiles_dzen2 }}/python/output.py
[dotfiles-dzen2-python-pipehandler]: {{ dotfiles_dzen2 }}/python/pipehandler.py

[dotfiles-lemon-python-testparams]:  {{ dotfiles_lemon }}/python/01-testparams.py
[dotfiles-lemon-python-testoutput]:  {{ dotfiles_lemon }}/python/02-testoutput.py
[dotfiles-lemon-python-panel]:       {{ dotfiles_lemon }}/python/panel.py
[dotfiles-lemon-python-gmc]:         {{ dotfiles_lemon }}/python/gmc.py
[dotfiles-lemon-python-helper]:      {{ dotfiles_lemon }}/python/helper.py
[dotfiles-lemon-python-output]:      {{ dotfiles_lemon }}/python/output.py
[dotfiles-lemon-python-pipehandler]: {{ dotfiles_lemon }}/python/pipehandler.py

[dotfiles-dzen2-python]:             {{ dotfiles_dzen2 }}/python/
[dotfiles-lemon-python]:             {{ dotfiles_lemon }}/python/
