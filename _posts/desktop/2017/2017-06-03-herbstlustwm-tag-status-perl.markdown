---
layout: post
title:  "HerbstluftWM Tag Status in Perl"
date:   2017-06-03 17:35:15 +0700
categories: desktop
tags: [coding, perl, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in Perl script.

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

*	[Modularized HerbstluftWM in Perl][local-perl-config]

*	[Piping and Forking in Perl][local-perl-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/][dotfiles-dzen2-perl]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/][dotfiles-lemon-perl]


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

{% include post/2017/06/herbstlustwm-tag-status-helper.md %}

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

{% include post/2017/06/herbstlustwm-tag-status-gmc.md %}

-- -- --

### Preparing Output

Let's have a look at <code class="code-file">output.pm</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../perl/output.pm][dotfiles-dzen2-perl-output]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../perl/output.pm][dotfiles-lemon-perl-output]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

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

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

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

[dotfiles-dzen2-perl-testparams]:  {{ dotfiles_dzen2 }}/perl/01-testparams.pl
[dotfiles-dzen2-perl-testoutput]:  {{ dotfiles_dzen2 }}/perl/02-testoutput.pl
[dotfiles-dzen2-perl-panel]:       {{ dotfiles_dzen2 }}/perl/panel.pl
[dotfiles-dzen2-perl-gmc]:         {{ dotfiles_dzen2 }}/perl/gmc.pm
[dotfiles-dzen2-perl-helper]:      {{ dotfiles_dzen2 }}/perl/helper.pm
[dotfiles-dzen2-perl-output]:      {{ dotfiles_dzen2 }}/perl/output.pm
[dotfiles-dzen2-perl-pipehandler]: {{ dotfiles_dzen2 }}/perl/pipehandler.pm

[dotfiles-lemon-perl-testparams]:  {{ dotfiles_lemon }}/perl/01-testparams.pl
[dotfiles-lemon-perl-testoutput]:  {{ dotfiles_lemon }}/perl/02-testoutput.pl
[dotfiles-lemon-perl-panel]:       {{ dotfiles_lemon }}/perl/panel.pl
[dotfiles-lemon-perl-gmc]:         {{ dotfiles_lemon }}/perl/gmc.pm
[dotfiles-lemon-perl-helper]:      {{ dotfiles_lemon }}/perl/helper.pm
[dotfiles-lemon-perl-output]:      {{ dotfiles_lemon }}/perl/output.pm
[dotfiles-lemon-perl-pipehandler]: {{ dotfiles_lemon }}/perl/pipehandler.pm
