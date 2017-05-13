---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM in Perl"
date:   2017-05-03 17:35:15 +0700
categories: desktop
tags: [coding, perl, herbstluftwm]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using Perl.
  
---

### Preface

	Goal: Separate Main Flow, Code, and Data.
	So anyone can focus to alter special customization in Main Script,
	without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to know read this overview.

*	[Modularized HerbstluftWM Script][local-overview]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[github.com/.../dotfiles/.../perl/][dotfiles-perl-directory]

#### Influence

For a more sophisticated HerbstluftWM configuration in Perl.
You may also take a look at this script.
I am also inspired by this configuration script.
This give influence to my script.

*	<https://github.com/ypnos/hlwm/blob/master/autostart>

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
This figure will explain how it looks 
in <code>Perl script</code> directory.

![HerbstluftWM: Directory Structure][image-perl-01-tree]{: .img-responsive }

-- -- --

### Modularizing in Perl

While main script using <code>.pl</code> extension (perl library),
module use <code>.pm</code>. extension (perl module).

#### Declare a module

We have to explicitly define what to export.
Anything exported become public in caller script.
And the rest is private to module.

Here we export hc function variable from helper module.

{% highlight perl %}
package helper;

use warnings;
use strict;

use config;

use Exporter;
our @ISA = 'Exporter';
our @EXPORT = qw(hc);
{% endhighlight %}

#### Call a module

{% highlight perl %}
use helper;
{% endhighlight %}

-- -- --

### System Calls

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.pm</code>

{% highlight perl %}
function hc() {
    system("herbstclient @_");
}
{% endhighlight %}

<code class="code-file">autostart.pl</code>

{% highlight perl %}
# Read the manual in $ man herbstluftwm
hc('emit_hook reload');

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap");
{% endhighlight %}

-- -- --

### Array: Tag Names and Keys

<code class="code-file">config.pm</code>

{% highlight perl %}
our @tag_names = (1..9);
our @tag_keys  = (1..9, 0);
{% endhighlight %}

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

-- -- --

### Hash: Color Schemes

We do not need many colors in Tiling Window Manager.
So this module is not mandatory required.
I like google Material Color to create custom Wallpaper in Inkscape.
I also use it in Dzen2 Statusbar, or Lemonbar.
It is nice to reuse the same resources for Window Manager.

	The reason is testing new data structure

The reason why I put this is Data Structure.
Everytime I try new language, I need to test the data structure.

*	It is simple, no need any string interpolation.

*	It is easy to test color, and dump in terminal.
	Changing color won't harm system, nor window manager.

After it is proven does well,
I can use it as a model to herbstluft config.
Both are similar, they use **key-value pairs**.

<code class="code-file">assets/gmc.pm</code>

{% highlight perl %}
our %color = (
    'white' => '#ffffff',
    'black' => '#000000',

    'grey50'  => '#fafafa',
    'grey100' => '#f5f5f5',
);
{% endhighlight %}

<code class="code-file">autostart.pl</code>

{% highlight perl %}
# background before wallpaper
system("xsetroot -solid '$color{'blue500'}'");
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../perl/assets/gmc.pm][dotfiles-perl-gmc]

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string interpolation all over the place.

<code class="code-file">config.pm</code>

{% highlight perl %}
# Modifier variables
my $s = 'Shift';
my $c = 'Control';
my $m = 'Mod4';
my $a = 'Mod1';


# resizing frames
my $resizestep = 0.05;

our %keybinds = (
  # session
    "$m-$s-q" => 'quit',
    "$m-$s-r" => 'reload',
    "$m-$s-c" => 'close',
);
{% endhighlight %}

This config will be utilized in main script

<code class="code-file">autostart.pl</code>

{% highlight perl %}
helper::do_config("keybind",   %config::keybinds);
helper::do_config("keybind",   %config::tagskeybinds);
helper::do_config("mousebind", %config::mousebinds);
helper::do_config("attr",      %config::attributes);
helper::do_config("set",       %config::sets);
helper::do_config("rule",      %config::rules);
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../perl/config.pm][dotfiles-perl-config]

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.
Passing hash argument in Perl is rather cryptic.

<code class="code-file">helper.pm</code>

{% highlight perl %}
sub do_config($\%) {
    my ($command, $ref2hash) = @_;
    my %hash = %$ref2hash;

    # loop over hash
    while(my ($key, $value) = each %hash) { 
        hc("$command $key $value");
        
        # uncomment to debug in terminal
        # print("$command $key $value \n")
    }
}
{% endhighlight %}

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight perl %}
        print("$command $key $value \n")
{% endhighlight %}

You can see the debugging result in figure below.

![HerbstluftWM: Tag Status][image-hlwm-03-debug-config]{: .img-responsive }

#### View Source File:

*	[github.com/.../dotfiles/.../perl/helper.pm][dotfiles-perl-helper]

-- -- --

### Setting the Tags

Nothing special here,
Perl read all exported variable from modules.

<code class="code-file">helper.pm</code>

{% highlight perl %}
sub set_tags_with_name() {
    hc("rename default '$tag_names[0]' 2>/dev/null || true");
    
    for my $index (0 .. $#tag_names) {
        hc("add '$tag_names[$index]'");
        
        # uncomment to debug in terminal
        # print $index."\n";

        my $key = $tag_keys[$index];
        if ("$key" ne "") {
            hc("keybind Mod4-$key use_index '$index'");
            hc("keybind Mod4-Shift-$key move_index '$index'");
        }
    }
}
{% endhighlight %}

-- -- --

### Launch the Panel

Two more functions left, it is <code>do_panel</code>
and <code>startup_run</code>.

This two also be easy to do in Perl.
<code class="code-file">helper.pm</code>

{% highlight perl %}
sub do_panel() {
    my $dirname = dirname(__FILE__);
    my $panel   = "$dirname/../bash/dzen2/panel.sh";
    if (not -x $panel) { $panel = "/etc/xdg/herbstluftwm/panel.sh"; }

    my $monitor_qx = qx(herbstclient list_monitors | cut -d: -f1);
    my @monitors = split /\n/, $monitor_qx;

    for my $monitor (@monitors) {
        # start it on each monitor
        system("$panel $monitor &");
    }
}
{% endhighlight %}

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.pm</code>

{% highlight perl %}
sub run() {
    my $command = 'silent new_attr bool my_not_first_autostart';

    my $not_first_qx = qx(herbstclient $command);
    my $exitcode = $?;

    if ($exitcode == 0) {
      # non windowed app
        system("compton &");
        system("dunst &");
        system("parcellite &");
        system("nitrogen --restore &");
        system("mpd &");
    
      # windowed app
        system("xfce4-terminal &");
        system("sleep 1 && firefox &");
        system("sleep 2 && geany &");
        system("sleep 2 && thunar &");
    }
}
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../perl/startup.pm][dotfiles-perl-startup]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

<code class="code-file">Header Part: autostart.pl</code>

{% highlight perl %}
use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);

use assets::gmc;
use helper;
use config;
use startup;
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.pl</code>

{% highlight perl %}
# background before wallpaper
system("xsetroot -solid '$color{'blue500'}'");

# Read the manual in $ man herbstluftwm
hc('emit_hook reload');

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap");

# do not repaint until unlock
hc("lock");

# standard
# remove all existing keybindings
hc('keyunbind --all');
hc("mouseunbind --all");
hc("unrule -F");

helper::set_tags_with_name();

# do hash config

helper::do_config("keybind",   %config::keybinds);
helper::do_config("keybind",   %config::tagskeybinds);
helper::do_config("mousebind", %config::mousebinds);
helper::do_config("attr",      %config::attributes);
helper::do_config("set",       %config::sets);
helper::do_config("rule",      %config::rules);

# unlock, just to be sure
hc("unlock");

# launch statusbar panel
helper::do_panel();

# load on startup
startup::run();

{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../perl/autostart.pl][dotfiles-perl-autostart]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-perl]:     {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html

[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]:  {{ asset_path }}/hlwm-03-debug-config.png

[image-perl-01-tree]:  {{ asset_path }}/hlwm-perl-01-tree.png

[dotfiles--directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/perl
[dotfiles-perl-autostart]: {{ dotfiles_path }}/perl/autostart.pl
[dotfiles-perl-gmc]:       {{ dotfiles_path }}/perl/assets/gmc.pm
[dotfiles-perl-config]:    {{ dotfiles_path }}/perl/config.pm
[dotfiles-perl-helper]:    {{ dotfiles_path }}/perl/helper.pm
[dotfiles-perl-startup]:   {{ dotfiles_path }}/perl/startup.pm
