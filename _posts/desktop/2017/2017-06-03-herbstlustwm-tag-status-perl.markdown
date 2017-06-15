---
layout: post-sidemenu-wm
title:  "HerbstluftWM Tag Status using Dzen2 or Lemonbar in Perl"
date:   2017-06-03 17:35:15 +0700
categories: desktop
tags: [coding, bash, herbstluftwm]
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

*	[github.com/.../dotfiles/.../perl/][dotfiles-perl-directory]

-- -- --

### HerbstluftWM Tag Status in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Tag Status Overview ][local-overview]]
[[ BASH ][local-BASH]]
[[ Perl ][local-Perl]]
[[ Python ][local-python]]
[[ Ruby ][local-Ruby]]
[[ PHP ][local-PHP]]
[[ Lua ][local-Lua]]
[[ Haskell ][local-Haskell]]

Source Code Directory:
[[ BASH ][dotfiles-BASH]]
[[ Perl ][dotfiles-Perl]]
[[ Python ][dotfiles-python]]
[[ Ruby ][dotfiles-Ruby]]
[[ PHP ][dotfiles-PHP]]
[[ Lua ][dotfiles-Lua]]
[[ Haskell ][dotfiles-Haskell]]

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

*	[github.com/.../dotfiles/.../perl/helper.pm][dotfiles-perl-helper]

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight perl %}
$ ./panel.pm 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.
Here it is our code in Perl.

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
And use <code>$geometry</code> as global variable.

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

#### View Source File:

*	[github.com/.../dotfiles/.../perl/01-testparams.pl][dotfiles-perl-testparams]

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

### Preparing Output

Let's have a look at <code class="code-file">output.sh</code> in github.
Before explain deep about Function,
we need to define two things that live in output module:

*	global variable: mutable state, must be initialized.

*	global constant: it is usually defined as a global variable,
	except that, the value won't be altered during script execution.
	The value is defined at the beginning of program.

#### View Source File:

*	[github.com/.../dotfiles/.../perl/output.pm][dotfiles-perl-output]

#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

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

{% highlight perl %}
use constant TAG_SHOWS => ['一 ichi', '二 ni', '三 san', '四 shi', 
    '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'];
{% endhighlight %}

#### Global Constant: Decoration

Decoration consist lemonbar formatting tag.

{% highlight perl %}
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

{% highlight perl %}
sub set_windowtitle {
    my $windowtitle = shift;  
    my $icon = PRE_ICON."".POST_ICON;

    # trim both ends
    $windowtitle =~ s/^\s+|\s+$//g;
      
    $segment_windowtitle = " $icon "
                         . "%{B-}%{F$color{'grey700'}} $windowtitle";
}
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
Ans then returning string as result.

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
Lemonbar using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

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

system("herbstclient pad $monitor $panel_height 0 $panel_height 0");

my $lemon_parameters = helper::get_lemon_parameters(
    $monitor, $panel_height);

# test
test_lemon($monitor, $lemon_parameters);
{% endhighlight %}

This will produce a panel on top.

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

The panel only contain the initialized version of the text.
It does not really interact with the Herbstluft event.

You can also click the clickable area to see it's result.
It only show text, not executed yet.

{% highlight bash %}
herbstclient focus_monitor "0" && herbstclient use "2"
herbstclient focus_monitor "0" && herbstclient use "3"
{% endhighlight %}


#### View Source File:

*	[github.com/.../dotfiles/.../perl/01-testoutput.pl][dotfiles-perl-testoutput]

-- -- --

### Continue on Next Tutorial

It is already a long tutorial.
It is time to take a break.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-perl]:  {{ asset_path }}/hlwm-statusbar-01-tree-perl.png

[local-perl-config]: {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html
[local-perl-pipe]:   {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/02/herbstlustwm-tag-status-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/03/herbstlustwm-tag-status-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/04/herbstlustwm-tag-status-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/06/herbstlustwm-tag-status-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell

[dotfiles-perl-directory]:   https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/perl
[dotfiles-perl-testparams]:  {{ dotfiles_path }}/perl/01-testparams.pl
[dotfiles-perl-testoutput]:  {{ dotfiles_path }}/perl/02-testoutput.pl
[dotfiles-perl-panel]:       {{ dotfiles_path }}/perl/panel.pl
[dotfiles-perl-gmc]:         {{ dotfiles_path }}/perl/assets/gmc.pm
[dotfiles-perl-helper]:      {{ dotfiles_path }}/perl/helper.pm
[dotfiles-perl-output]:      {{ dotfiles_path }}/perl/output.pm
[dotfiles-perl-pipehandler]: {{ dotfiles_path }}/perl/pipehandler.pm

