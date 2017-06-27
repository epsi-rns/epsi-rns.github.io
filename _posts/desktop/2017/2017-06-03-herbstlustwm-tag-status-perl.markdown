---
layout: post-sidemenu-wm
title:  "HerbstluftWM Tag Status in Perl"
date:   2017-06-03 17:35:15 +0700
categories: desktop
tags: [coding, perl, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in Perl script.

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

*	[Modularized HerbstluftWM in Perl][local-perl-config]

*	[Piping and Forking in Perl][local-perl-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/][dotfiles-dzen2-perl]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/][dotfiles-lemon-perl]


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
in <code>Perl script</code> directory.

![Statusbar: Directory Structure][image-01-tree-perl]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

### Get Geometry

Let's have a look at <code class="code-file">helper.pm</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/helper.pm][dotfiles-dzen2-perl-helper]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/helper.pm][dotfiles-lemon-perl-helper]

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
$ ./panel.pm 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.
Here it is our code in Perl.

<code class="code-file">helper.pm</code>

{% highlight perl %}
# script arguments
sub get_monitor {
    my @arguments = @_; 
    my $num_args  = $#arguments;
   
    # ternary operator
    my $monitor = ($num_args > 0) ? $arguments[0] : 0;
    
    return $monitor;
}
{% endhighlight %}

And in main code we can call

{% highlight perl %}
use File::Basename;
use lib dirname(__FILE__);
use helper;

my $monitor = helper::get_monitor(@ARGV);
print $monitor."\n";
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

<code class="code-file">helper.pm</code>

{% highlight perl %}
sub get_geometry {
    my $monitor = shift;

    my $geometry_qx = qx(herbstclient monitor_rect "$monitor");
    if ($geometry_qx eq "") { 
        print "Invalid monitor $monitor\n";
        exit 1
    }
    
    my @geometry = split / /, $geometry_qx;
    
    return @geometry;
}
{% endhighlight %}

Consider call this function from script later.
To print array in Perl,
we just have to wrap it in <code>"@geometry"</code>.

{% highlight perl %}
my $panel_height = 24;
my $monitor  = helper::get_monitor(@ARGV);
my @geometry = helper::get_geometry($monitor);
print "@geometry";
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
<code class="code-file">helper.pm</code>

{% highlight perl %}
sub get_bottom_panel_geometry {
    my $height = shift;
    my @geometry = @_;

    # geometry has the format X Y W H
    return ($geometry[0] + 24, $geometry[3] - $height, 
            $geometry[2] - 48, $height);
}
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight perl %}
my $panel_height = 24;
my $monitor  = helper::get_monitor(@ARGV);
my @geometry = helper::get_geometry($monitor);
my ($xpos, $ypos, $width, $height) = 
        helper::get_bottom_panel_geometry($panel_height, @geometry);

print "Lemonbar geometry: ${width}x${height}+${xpos}+${ypos}\n";
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

<code class="code-file">helper.pm</code>

{% highlight perl %}
sub get_lemon_parameters {   
    # parameter: function argument
    my $monitor = shift;
    my $panel_height = shift;

    # calculate geometry
    my @geometry = get_geometry($monitor);
    my ($xpos, $ypos, $width, $height) = 
       get_top_panel_geometry($panel_height, @geometry); 

    # geometry: -g widthxheight+x+y
    my $geom_res = "${width}x${height}+${xpos}+${ypos}";
    
    # color, with transparency
    my $bgcolor = '#aa000000';
    my $fgcolor = '#ffffff';
    
    # XFT: require lemonbar_xft_git 
    my $font_takaop  = "takaopgothic-9";
    my $font_bottom  = "monospace-9";
    my $font_symbol  = "PowerlineSymbols-11";
    my $font_awesome = "FontAwesome-9";

    # finally
    my $parameters = "  -g $geom_res -u 2"
                   . " -B $bgcolor -F $fgcolor"
                   . " -f $font_takaop -f $font_awesome -f $font_symbol";

    return $parameters;
}
{% endhighlight %}

-- -- --

### Testing The Parameters

Consider this code <code class="code-file">01-testparams.pl</code>.
The script call the above function to get lemon parameters.

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);

use helper;

# initialize
my $panel_height = 24;
my $monitor = helper::get_monitor(@ARGV);

my $lemon_parameters = helper::get_lemon_parameters(
    $monitor, $panel_height);

print $lemon_parameters."\n";
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
-bg '#000000' -fg '#ffffff' -title-name dzentop 
-fn '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*'
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/01-testparams.sh][dotfiles-dzen2-perl-testparams]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/01-testparams.sh][dotfiles-lemon-perl-testparams]

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

{% highlight perl %}
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");
{% endhighlight %}

-- -- --

### Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

<code class="code-file">gmc.pm</code>

{% highlight perl %}
our %color = (
    'white' => '#ffffff',
    'black' => '#000000',

    'grey50'  => '#fafafa',
    'grey100' => '#f5f5f5'
);
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/gmc.pm][dotfiles-dzen2-perl-gmc]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/gmc.pm][dotfiles-lemon-perl-gmc]

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

Let's have a look at <code class="code-file">output.pm</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/output.pm][dotfiles-dzen2-perl-output]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/output.pm][dotfiles-lemon-perl-output]

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

There are a several ways to define constant in Perl.
Constant in Perl may begin with the word <code>use const</code>.

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.pm</code>
In script, we initialize the variable as below

{% highlight perl %}
my $segment_windowtitle = ''; # empty string
my @tags_status = [];         # empty array
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

<code class="code-file">output.pm</code>

{% highlight perl %}
use constant TAG_SHOWS => ['一 ichi', '二 ni', '三 san', '四 shi', 
    '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'];
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.pm</code>
Decoration consist lemonbar formatting tag.

{% highlight perl %}
use gmc;

# decoration
use constant SEPARATOR => "%{B-}%{F$color{'yellow500'}}|%{B-}%{F-}";

# Powerline Symbol
use constant RIGHT_HARD_ARROW => "";
use constant RIGHT_SOFT_ARROW => "";
use constant LEFT_HARD_ARROW  => "";
use constant LEFT_SOFT_ARROW  => "";

# theme
use constant PRE_ICON  => "%{F$color{'yellow500'}}";
use constant POST_ICON => "%{F-}";
{% endhighlight %}

-- -- --

### Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.pm</code>

{% highlight perl %}
sub set_tag_value {
    my $monitor = shift;
    
    my $tag_status_qx = qx(herbstclient tag_status $monitor);
       $tag_status_qx =~ s/^\s+|\s+$//g;
    @tags_status = split(/\t/, $tag_status_qx);
}
{% endhighlight %}

This function above turn the tag status string
into array of tags for later use.

<code class="code-file">output.pm</code>

{% highlight perl %}
sub set_windowtitle {
    my $windowtitle = shift;  
    my $icon = PRE_ICON."".POST_ICON;
      
    $segment_windowtitle = " $icon "
                         . "%{B-}%{F$color{'grey700'}} $windowtitle";
}
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And then returning string as result.

<code class="code-file">output.pm</code>

{% highlight perl %}
sub output_by_title {
    my $text = "$segment_windowtitle ".SEPARATOR."  ";

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

<code class="code-file">output.pm</code>

{% highlight perl %}
sub output_by_tag {
    my $monitor = shift;    
    
    my $tag_status = shift;
    my $tag_index  = substr($tag_status, 1, 1);
    my $tag_mark   = substr($tag_status, 0, 1);
    my $tag_name   = TAG_SHOWS->[$tag_index - 1]; # zero based

    # ----- pre tag

    my $text_pre = '';
    if ($tag_mark eq '#') {
        $text_pre = "%{B$color{'blue500'}}%{F$color{'black'}}"
                  . "%{U$color{'white'}}%{+u}".RIGHT_HARD_ARROW
                  . "%{B$color{'blue500'}}%{F$color{'white'}}"
                  . "%{U$color{'white'}}%{+u}";
    } elsif ($tag_mark eq '+') {
        $text_pre = "%{B$color{'yellow500'}}%{F$color{'grey400'}}";
    } elsif ($tag_mark eq ':') {
        $text_pre = "%{B-}%{F$color{'white'}}"
                  . "%{U$color{'red500'}}%{+u}";
    } elsif ($tag_mark eq '!') {
        $text_pre = "%{B$color{'red500'}}%{F$color{'white'}}"
                  . "%{U$color{'white'}}%{+u}";
    } else {
        $text_pre = "%{B-}%{F$color{'grey600'}}%{-u}";
    }

    # ----- tag by number

    # clickable tags
    my $text_name = "%{A:herbstclient focus_monitor \"$monitor\" && "
                  . "herbstclient use \"$tag_index\":} $tag_name %{A} ";
                  
    # non clickable tags
    # my $text_name = " $tag_name ";

    # ----- post tag
    
    my $text_post = "";
    if ($tag_mark eq '#') {
        $text_post = "%{B-}%{F$color{'blue500'}}"
                   . "%{U$color{'red500'}}%{+u}"
                   . RIGHT_HARD_ARROW;
    }
    
    my $text_clear = '%{B-}%{F-}%{-u}';
     
    return $text_pre . $text_name . $text_post . $text_clear;
}
{% endhighlight %}

-- -- --

### Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar is using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.pm</code>

{% highlight perl %}
sub get_statusbar_text {
    my $monitor = shift;   
    my $text = '';

    # draw tags
    $text .= '%{l}';
    foreach my $tag_status (@tags_status) {
        $text .= output_by_tag($monitor, $tag_status);
    }
    
    # draw window title
    $text .= '%{r}';
    $text .= output_by_title();
    
    return $text;
}
{% endhighlight %}

-- -- --

### Testing The Output

Consider this code <code class="code-file">02-testoutput.pl</code>.
The script using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);

use helper;

# process handler
sub test_lemon { 
    use IO::Pipe;
    use output;

    my $monitor = shift;
    my $parameters = shift;
    
    my $pipe_out = IO::Pipe->new();
    my $command = "lemonbar $parameters -p";
    my $handle = $pipe_out->writer($command);

    # initialize statusbar
    output::set_tag_value($monitor);
    output::set_windowtitle('test');

    my $text = output::get_statusbar_text($monitor);
    print $pipe_out $text."\n";
    flush $pipe_out;

    $pipe_out->close();
}

# initialize
my $panel_height = 24;
my $monitor = helper::get_monitor(@ARGV);
my $lemon_parameters = helper::get_lemon_parameters(
    $monitor, $panel_height);

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

*	**dzen2**: 
	[github.com/.../dotfiles/.../perl/01-testoutput.pl][dotfiles-dzen2-perl-testoutput]

*	**lemonbar**: 
	[github.com/.../dotfiles/.../perl/01-testoutput.pl][dotfiles-lemon-perl-testoutput]

-- -- --

### Coming up Next

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

*	[HerbstluftWM Event Idle in Haskell][local-perl-idle]

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

[image-01-tree-perl]:  {{ asset_path }}/hlwm-statusbar-01-tree-perl.png

[local-perl-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html
[local-perl-pipe]:   {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-perl-idle]:   {{ site.url }}/desktop/2017/06/13/herbstlustwm-event-idle-perl.html

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
