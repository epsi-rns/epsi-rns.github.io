---
layout: post-sidemenu-wm
title:  "HerbstluftWM Tag Status using Dzen2 or Lemonbar in PHP"
date:   2017-06-06 17:35:15 +0700
categories: desktop
tags: [coding, php, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in PHP script.

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

*	[Modularized HerbstluftWM in PHP][local-php-config]

*	[Piping and Forking in PHP][local-php-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[github.com/.../dotfiles/.../php/][dotfiles-php-directory]

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

Source Code Directory:
[[ BASH ][dotfiles-bash]]
[[ Perl ][dotfiles-perl]]
[[ Python ][dotfiles-python]]
[[ Ruby ][dotfiles-ruby]]
[[ PHP ][dotfiles-php]]
[[ Lua ][dotfiles-lua]]
[[ Haskell ][dotfiles-haskell]]

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
in <code>PHP script</code> directory.

![Statusbar: Directory Structure][image-01-tree-php]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

### Get Geometry

Let's have a look at <code class="code-file">helper.php</code> in github.

#### View Source File:

*	[github.com/.../dotfiles/.../php/helper.php][dotfiles-php-helper]

Similar Code: 
[[ BASH Helper ][dotfiles-bash-helper]]
[[ Perl Helper ][dotfiles-perl-helper]]
[[ Python Helper ][dotfiles-python-helper]]
[[ Ruby Helper ][dotfiles-ruby-helper]]
[[ PHP Helper ][dotfiles-php-helper]]
[[ Lua Helper ][dotfiles-lua-helper]]
[[ Haskell Helper ][dotfiles-haskell-helper]]

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight bash %}
$ ./panel.php 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.
Here it is our code in PHP.

<code class="code-file">helper.php</code>

{% highlight php %}
function get_monitor($arguments)
{
    // ternary operator
    return count($arguments) > 0 ? (int)$arguments[0] : 0;
}
{% endhighlight %}

And in main code we can call

{% highlight php %}
require_once(__DIR__.'/helper.php');

$monitor = get_monitor($argv);
echo $monitor;
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

<code class="code-file">helper.php</code>

{% highlight php %}
function get_geometry($monitor)
{
    $raw = shell_exec('herbstclient monitor_rect '.$monitor);

    if (empty($raw)) {
        print('Invalid monitor '.$monitors);
        exit(1);
    }
    
    return explode(' ', trim($raw));
}
{% endhighlight %}

Consider call this function from script later.
To print array in PHP,
we just have to wrap it in <code>implode(' ', $geometry)</code>.

{% highlight php %}
$monitor  = get_monitor($argv);
$geometry = get_geometry($monitor);
echo implode(' ', $geometry)."\n";
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
<code class="code-file">helper.php</code>

{% highlight php %}
function get_bottom_panel_geometry($height, $geometry)
{
    // geometry has the format X Y W H
    return array(
        $geometry[0] + 24, ($geometry[3]-$height), 
        $geometry[2] - 48, $height);
}
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight php %}
$panel_height = 24;
$monitor  = get_monitor($argv);
$geometry = get_geometry($monitor);
list($xpos, $ypos, $width, $height) = get_bottom_panel_geometry(
    $panel_height, $geometry);

echo "Lemonbar geometry: ${width}x${height}+${xpos}+${ypos}\n"
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

<code class="code-file">helper.php</code>

{% highlight php %}
function get_lemon_parameters($monitor, $panel_height)
{
    # calculate geometry
    $geometry = get_geometry($monitor);
    list($xpos, $ypos, $width, $height) = get_top_panel_geometry(
        $panel_height, $geometry);


    # geometry: -g widthxheight+x+y
    $geom_res = "${width}x${height}+${xpos}+${ypos}";
    
    # color, with transparency    
    $bgcolor = "'#aa000000'";
    $fgcolor = "'#ffffff'";

    # XFT: require lemonbar_xft_git 
    $font_takaop  = "takaopgothic-9";
    $font_bottom  = "monospace-9";
    $font_symbol  = "PowerlineSymbols-11";
    $font_awesome = "FontAwesome-9";

    # finally  
    $parameters  =  "  -g $geom_res -u 2"
                 . " -B $bgcolor -F $fgcolor"
                 . " -f $font_takaop -f $font_awesome -f $font_symbol"; 

    return $parameters;
}
{% endhighlight %}

-- -- --

### Testing The Parameters

Consider this code <code class="code-file">01-testparams.php</code>.
The script call the above function to get lemon parameters.

{% highlight php %}
#!/usr/bin/php 
<?php # using PHP7

require_once(__DIR__.'/helper.php');

# initialize
$panel_height = 24;
$monitor = get_monitor($argv);

$lemon_parameters = get_lemon_parameters($monitor, $panel_height);
echo $lemon_parameters."\n";
{% endhighlight %}

This will produce output
something similar to this result

{% highlight conf %}
-g 1280x24+0+0 -u 2 -B '#aa000000' -F '#ffffff' 
-f takaopgothic-9 -f FontAwesome-9 -f PowerlineSymbols-11
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../php/01-testparams.php][dotfiles-php-testparams]

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

{% highlight php %}
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");
{% endhighlight %}

-- -- --

### Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

<code class="code-file">gmc.php</code>

{% highlight php %}
$color = array(
    'white' => '#ffffff',
    'black' => '#000000',

    'grey50'  => '#fafafa',
    'grey100' => '#f5f5f5',
);
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../php/gmc.sh][dotfiles-php-gmc]

Similar Code: 
[[ BASH Color ][dotfiles-bash-gmc]]
[[ Perl Color ][dotfiles-perl-gmc]]
[[ Python Color ][dotfiles-python-gmc]]
[[ Ruby Color ][dotfiles-ruby-gmc]]
[[ PHP Color ][dotfiles-php-gmc]]
[[ Lua Color ][dotfiles-lua-gmc]]
[[ Haskell Color ][dotfiles-haskell-gmc]]

-- -- --

### Preparing Output

Let's have a look at <code class="code-file">output.php</code> in github.

#### View Source File:

*	[github.com/.../dotfiles/.../php/output.php][dotfiles-php-output]

Similar Code: 
[[ BASH Output ][dotfiles-bash-output]]
[[ Perl Output ][dotfiles-perl-output]]
[[ Python Output ][dotfiles-python-output]]
[[ Ruby Output ][dotfiles-ruby-output]]
[[ PHP Output ][dotfiles-php-output]]
[[ Lua Output ][dotfiles-lua-output]]
[[ Haskell Output ][dotfiles-haskell-output]]

-- -- --

### Global Variable and Constant

Constant in PHP begin with the word <code>const</code>.

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.php</code>
In script, we initialize the variable as below

{% highlight php %}
$segment_windowtitle = ''; # empty string
$tags_status = [];         # empty array
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

<code class="code-file">output.php</code>
{% highlight php %}
const TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'];
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.php</code>
Decoration consist lemonbar formatting tag.

{% highlight php %}
require_once(__DIR__.'/gmc.php');

// decoration
const SEPARATOR = "%{B-}%{F".COLOR['yellow500']."}|%{B-}%{F-}";

// Powerline Symbol
const RIGHT_HARD_ARROW = "";
const RIGHT_SOFT_ARROW = "";
const LEFT_HARD_ARROW  = "";
const LEFT_SOFT_ARROW  = "";

// theme
const PRE_ICON    = "%{F".COLOR['yellow500']."}";
const POST_ICON   = "%{F-}";
{% endhighlight %}

-- -- --

### Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.php</code>

{% highlight php %}
function set_tag_value($monitor)
{
    global $tags_status;

    $raw = shell_exec("herbstclient tag_status $monitor");
    $tags_status = explode("\t", trim($raw));
}
{% endhighlight %}

This function above turn the tag status string
into array of tags for later use.

<code class="code-file">output.php</code>

{% highlight php %}
function set_windowtitle($windowtitle)
{
    global $segment_windowtitle;

    $icon = PRE_ICON."".POST_ICON;
    
    $windowtitle = trim($windowtitle);
      
    $segment_windowtitle = " ${icon} %{B-}"
        . "%{F".COLOR['grey700']."} ${windowtitle}";
}
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And then returning string as result.

<code class="code-file">output.php</code>

{% highlight php %}
function output_by_title()
{
    global $segment_windowtitle;    
    $text  = "$segment_windowtitle ".SEPARATOR."  ";
    
    return $text;
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

<code class="code-file">output.php</code>

{% highlight php %}
function output_by_tag($monitor, $tag_status)
{
    $tag_index  = substr($tag_status, 1, 1);
    $tag_mark   = substr($tag_status, 0, 1);
    $tag_name   = TAG_SHOWS[(int)$tag_index - 1]; # zero based

    # ----- pre tag

    switch ($tag_mark) {
    case "#":
        $text_pre = "%{B".COLOR['blue500']."}%{F".COLOR['black']."}"
                  . "%{U".COLOR['white']."}%{+u}".RIGHT_HARD_ARROW
                  . "%{B".COLOR['blue500']."}%{F".COLOR['white']."}"
                  . "%{U".COLOR['white']."}%{+u}";
        break;
    case "+":
        $text_pre = "%{B".COLOR['yellow500']."}%{F".COLOR['grey400']."}";
        break;
    case ":":
        $text_pre = $text_pre = "%{B-}%{F".COLOR['white']."}"
                  . "%{U".COLOR['red500']."}%{+u}";
        break;
    case "!":
        $text_pre = "%{B".COLOR['red500']."}%{F".COLOR['white']."}"
                  . "%{U".COLOR['white']."}%{+u}";
        break;
    default:
        $text_pre = "%{B-}%{F".COLOR['grey600']."}%{-u}";
    }

    # ----- tag by number
    
    // clickable tags
    $text_name = "%{A:herbstclient focus_monitor \"${monitor}\" && " 
               . "herbstclient use \"${tag_index}\":} ${tag_name} %{A}";
    
    # non clickable tags
    #$text_name = " $tag_name ";

    # ----- post tag

    if ($tag_mark == '#')
        $text_post = "%{B-}%{F".COLOR['blue500']."}"
                   . "%{U".COLOR['red500']."}%{+u}"
                   . RIGHT_HARD_ARROW;
    else
        $text_post = "";

    $text_clear = '%{B-}%{F-}%{-u}';
     
    return $text_pre . $text_name . $text_post . $text_clear;
}
{% endhighlight %}

-- -- --

### Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.php</code>

{% highlight php %}
function get_statusbar_text($monitor)
{
    global $tags_status;
    $text = '';
    
    // draw tags
    $text .= '%{l}';
    foreach ($tags_status as $tag_status) {
        $text .= output_by_tag($monitor, $tag_status);
     }
    
    // draw window title
    $text .= '%{r}';
    $text .= output_by_title();

    return $text;
}
{% endhighlight %}

-- -- --

### Testing The Output

Consider this code <code class="code-file">02-testoutput.php</code>.
The script is using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight php %}
#!/usr/bin/php 
<?php # using PHP7

require_once(__DIR__.'/helper.php');
require_once(__DIR__.'/output.php');

# process handler
function test_lemon($monitor, $parameters) 
{
    $command_out  = "lemonbar $parameters -p";
    $pipe_out = popen($command_out, 'w');

    // initialize statusbar
    set_tag_value($monitor);
    set_windowtitle('test');
        
    $text = get_statusbar_text($monitor);
    fwrite($pipe_out, $text."\n");
    flush();

    pclose($pipe_out);
}

# initialize
$panel_height = 24;
$monitor = get_monitor($argv);
$lemon_parameters = get_lemon_parameters($monitor, $panel_height);

# test
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");
test_lemon($monitor, $lemon_parameters);
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

*	[github.com/.../dotfiles/.../php/01-testoutput.php][dotfiles-php-testoutput]

-- -- --

### Continue on Next Tutorial

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-php]:  {{ asset_path }}/hlwm-statusbar-01-tree-php.png

[local-php-config]: {{ site.url }}/desktop/2017/05/06/herbstlustwm-modularized-php.html
[local-php-pipe]:   {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/02/herbstlustwm-tag-status-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/03/herbstlustwm-tag-status-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/04/herbstlustwm-tag-status-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/06/herbstlustwm-tag-status-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html

[dotfiles-bash]:    {{ dotfiles_path }}/bash
[dotfiles-perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-php]:     {{ dotfiles_path }}/php
[dotfiles-lua]:     {{ dotfiles_path }}/lua
[dotfiles-haskell]: {{ dotfiles_path }}/haskell

[dotfiles-bash-directory]:   {{ dotfiles_path }}/bash
[dotfiles-bash-testparams]:  {{ dotfiles_path }}/bash/01-testparams.sh
[dotfiles-bash-testoutput]:  {{ dotfiles_path }}/bash/02-testoutput.sh
[dotfiles-bash-panel]:       {{ dotfiles_path }}/bash/panel.sh
[dotfiles-bash-gmc]:         {{ dotfiles_path }}/bash/gmc.sh
[dotfiles-bash-helper]:      {{ dotfiles_path }}/bash/helper.sh
[dotfiles-bash-output]:      {{ dotfiles_path }}/bash/output.sh
[dotfiles-bash-pipehandler]: {{ dotfiles_path }}/bash/pipehandler.sh

[dotfiles-perl-directory]:   {{ dotfiles_path }}/perl
[dotfiles-perl-testparams]:  {{ dotfiles_path }}/perl/01-testparams.pl
[dotfiles-perl-testoutput]:  {{ dotfiles_path }}/perl/02-testoutput.pl
[dotfiles-perl-panel]:       {{ dotfiles_path }}/perl/panel.pl
[dotfiles-perl-gmc]:         {{ dotfiles_path }}/perl/gmc.pm
[dotfiles-perl-helper]:      {{ dotfiles_path }}/perl/helper.pm
[dotfiles-perl-output]:      {{ dotfiles_path }}/perl/output.pm
[dotfiles-perl-pipehandler]: {{ dotfiles_path }}/perl/pipehandler.pm

[dotfiles-python-directory]:   {{ dotfiles_path }}/python
[dotfiles-python-testparams]:  {{ dotfiles_path }}/python/01-testparams.py
[dotfiles-python-testoutput]:  {{ dotfiles_path }}/python/02-testoutput.py
[dotfiles-python-panel]:       {{ dotfiles_path }}/python/panel.py
[dotfiles-python-gmc]:         {{ dotfiles_path }}/python/gmc.py
[dotfiles-python-helper]:      {{ dotfiles_path }}/python/helper.py
[dotfiles-python-output]:      {{ dotfiles_path }}/python/output.py
[dotfiles-python-pipehandler]: {{ dotfiles_path }}/python/pipehandler.py

[dotfiles-ruby-directory]:   {{ dotfiles_path }}/ruby
[dotfiles-ruby-testparams]:  {{ dotfiles_path }}/ruby/01-testparams.rb
[dotfiles-ruby-testoutput]:  {{ dotfiles_path }}/ruby/02-testoutput.rb
[dotfiles-ruby-panel]:       {{ dotfiles_path }}/ruby/panel.rb
[dotfiles-ruby-gmc]:         {{ dotfiles_path }}/ruby/gmc.rb
[dotfiles-ruby-helper]:      {{ dotfiles_path }}/ruby/helper.rb
[dotfiles-ruby-output]:      {{ dotfiles_path }}/ruby/output.rb
[dotfiles-ruby-pipehandler]: {{ dotfiles_path }}/ruby/pipehandler.rb

[dotfiles-php-directory]:   {{ dotfiles_path }}/php
[dotfiles-php-testparams]:  {{ dotfiles_path }}/php/01-testparams.php
[dotfiles-php-testoutput]:  {{ dotfiles_path }}/php/02-testoutput.php
[dotfiles-php-panel]:       {{ dotfiles_path }}/php/panel.php
[dotfiles-php-gmc]:         {{ dotfiles_path }}/php/gmc.php
[dotfiles-php-helper]:      {{ dotfiles_path }}/php/helper.php
[dotfiles-php-output]:      {{ dotfiles_path }}/php/output.php
[dotfiles-php-pipehandler]: {{ dotfiles_path }}/php/pipehandler.php

[dotfiles-lua-directory]:   {{ dotfiles_path }}/lua
[dotfiles-lua-testparams]:  {{ dotfiles_path }}/lua/01-testparams.lua
[dotfiles-lua-testoutput]:  {{ dotfiles_path }}/lua/02-testoutput.lua
[dotfiles-lua-panel]:       {{ dotfiles_path }}/lua/panel.lua
[dotfiles-lua-gmc]:         {{ dotfiles_path }}/lua/gmc.lua
[dotfiles-lua-common]:      {{ dotfiles_path }}/lua/common.lua
[dotfiles-lua-helper]:      {{ dotfiles_path }}/lua/helper.lua
[dotfiles-lua-output]:      {{ dotfiles_path }}/lua/output.lua
[dotfiles-lua-pipehandler]: {{ dotfiles_path }}/lua/pipehandler.lua

[dotfiles-haskell-directory]:   {{ dotfiles_path }}/lua
[dotfiles-haskell-testparams]:  {{ dotfiles_path }}/haskell/01-testparams.hs
[dotfiles-haskell-testoutput]:  {{ dotfiles_path }}/haskell/02-testoutput.hs
[dotfiles-haskell-panel]:       {{ dotfiles_path }}/haskell/panel.hs
[dotfiles-haskell-gmc]:         {{ dotfiles_path }}/haskell/MyGMC.hs
[dotfiles-haskell-helper]:      {{ dotfiles_path }}/haskell/MyHelper.hs
[dotfiles-haskell-output]:      {{ dotfiles_path }}/haskell/MyOutput.hs
[dotfiles-haskell-pipehandler]: {{ dotfiles_path }}/haskell/MyPipeHandler.hs
