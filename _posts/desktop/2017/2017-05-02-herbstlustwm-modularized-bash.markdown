---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM in BASH"
date:   2017-05-02 17:35:15 +0700
categories: desktop
tags: [coding, bash, herbstluftwm]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using BASH.
  
---

### Preface

	Goal: Separate Main Flow, Code, and Data.
	So anyone can focus to alter special customization in Main Script,
	without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to know read this overview.

*	[Modularized HerbstluftWM Overview][local-overview]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[github.com/.../dotfiles/.../bash/][dotfiles-bash-directory]

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
This figure will explain how it looks
in <code>BASH script</code> directory.

![HerbstluftWM: Directory Structure][image-bash-01-tree]{: .img-responsive }

-- -- --

### Modularizing in BASH

Here we are talking how to create module and call module.
It is very straightforward in BASH. No dark magic required.

#### Declare a module

Nothing to declare, just do not forget the shebang <code>#!</code>
and executable permission.

{% highlight bash %}
#!/usr/bin/env bash
{% endhighlight %}

#### Call a module

{% highlight bash %}
. ~/.config/herbstluftwm/bash/helper.sh
{% endhighlight %}

-- -- --

### System Calls

There is no such thing as system calls in bash.
Everything in bash is command called in system environment.
That is the most advantage of using BASH to configure HLWM.

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.sh</code>

{% highlight bash %}
function hc() {
    herbstclient "$@"
}
{% endhighlight %}

<code class="code-file">autostart.sh</code>

{% highlight bash %}
# Read the manual in $ man herbstluftwm
hc emit_hook reload

# gap counter
echo 35 > /tmp/herbstluftwm-gap
{% endhighlight %}

-- -- --

### Array: Tag Names and Keys

<code class="code-file">config.sh</code>

{% highlight bash %}
tag_names=( {1..9} )
tag_keys=( {1..9} 0 )
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

<code class="code-file">assets/gmc.sh</code>

{% highlight bash %}
declare -A color=(
    ['white']='#ffffff'
    ['black']='#000000'

    ['grey50']='#fafafa'
    ['grey100']='#f5f5f5'
)
{% endhighlight %}

<code class="code-file">autostart.sh</code>

{% highlight bash %}
# background before wallpaper
xsetroot -solid "${color['blue500']}"
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../bash/assets/gmc.sh][dotfiles-bash-gmc]

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string interpolation all over the place.

<code class="code-file">config.sh</code>

{% highlight bash %}
# Modifier variables
s=Shift
c=Control
m=Mod4
a=Mod1

declare -A keybinds=(
  # session
    ["$m-$s-q"]='quit'
    ["$m-$s-r"]='reload'
    ["$m-$s-c"]='close'
)
{% endhighlight %}

Be aware of this one. Specific BASH only issue.
Since BASH directly interpret the interpolation in system environment.
Changing double quote to single quote prevent the terminal spawning,
even when you double quote it later.

{% highlight bash %}
declare -A keybinds=(
    ["$m-Return"]="spawn ${TERMINAL:-xfce4-terminal}"
)
{% endhighlight %}

This config will be utilized in main script

<code class="code-file">autostart.sh</code>

{% highlight bash %}
do_config "keybind"   "$(declare -p keybinds)"
do_config "keybind"   "$(declare -p tagskeybinds)"
do_config "mousebind" "$(declare -p mousebinds)"
do_config "attr"      "$(declare -p attributes)"
do_config "set"       "$(declare -p sets)"
do_config "rule"      "$(declare -p rules)"
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../bash/config.sh][dotfiles-bash-config]

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.
Since BASH does not have built support for passing hash argument,
this require a little hack and a few cryptic character.

<code class="code-file">helper.sh</code>

{% highlight bash %}
function do_config()
{
    local command="${1}"
    shift
    
    # associative array hack
    eval "declare -A hash="${1#*=}

    # loop over hash    
    for key in "${!hash[@]}"; do
        local value=${hash[$key]}        
        hc $command $key $value
        
        # uncomment to debug in terminal
        # echo $command $key $value
    done
}
{% endhighlight %}

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight bash %}
        echo $command $key $value
{% endhighlight %}

You can see the debugging result in figure below.

![HerbstluftWM: Tag Status][image-hlwm-03-debug-config]{: .img-responsive }

#### View Source File:

*	[github.com/.../dotfiles/.../bash/helper.sh][dotfiles-bash-helper]

-- -- --

### Setting the Tags

This should be done before doing any config rules.
I did **copy-paste** part of the original configuration,
turn the code into function, and make just a few modification.

Nothing special here,
BASH read all global variable from other files.

<code class="code-file">helper.sh</code>

{% highlight bash %}
function set_tags_with_name() {
    hc rename default "${tag_names[0]}" 2>/dev/null || true
    
    for index in ${!tag_names[@]} ; do
        hc add "${tag_names[$index]}"
        
        # uncomment to debug in terminal
        # echo $index

        local key="${tag_keys[$index]}"
        if ! [ -z "$key" ] ; then
            hc keybind "$m-$key" use_index "$index"
            hc keybind "$m-Shift-$key" move_index "$index"
        fi
    done
}
{% endhighlight %}

-- -- --

### Launch the Panel

Two more functions left, it is <code>do_panel</code>
and <code>startup_run</code>.
Again, I did **copy-paste** part of the original configuration,
turn the code into function, and make just a few modification.

This two should be very easy to do in BASH.
Nothing special in BASH.
But this could be complex task, in other language, such as Lua.

<code class="code-file">helper.sh</code>

{% highlight bash %}
function do_panel() {
    local panel=~/.config/herbstluftwm/bash/dzen2/panel.sh
    [ -x "$panel" ] || panel=/etc/xdg/herbstluftwm/panel.sh
    for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
        # start it on each monitor
        "$panel" $monitor &
    done
}
{% endhighlight %}

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.sh</code>

{% highlight bash %}
startup_run() {
    command="silent new_attr bool my_not_first_autostart"
    
    if hc $command ; then
      # non windowed app
        compton &
        dunst &
        parcellite &
        nitrogen --restore &
        mpd &
    
      # windowed app
        xfce4-terminal &
        sleep 1 && firefox &
        sleep 2 && geany &
        sleep 2 && thunar &
    fi
}
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../bash/startup.sh][dotfiles-bash-startup]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

<code class="code-file">Header Part: autostart.sh</code>

{% highlight bash %}
. ~/.config/herbstluftwm/bash/assets/gmc.sh
. ~/.config/herbstluftwm/bash/config.sh
. ~/.config/herbstluftwm/bash/helper.sh
. ~/.config/herbstluftwm/bash/startup.sh
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.sh</code>

{% highlight bash %}
# background before wallpaper
xsetroot -solid "${color['blue500']}"

# Read the manual in $ man herbstluftwm
hc emit_hook reload

# gap counter
echo 35 > /tmp/herbstluftwm-gap

# do not repaint until unlock
hc lock

# standard
hc keyunbind --all
hc mouseunbind --all
hc unrule -F

set_tags_with_name

# do hash config
do_config "keybind"   "$(declare -p keybinds)"
do_config "keybind"   "$(declare -p tagskeybinds)"
do_config "mousebind" "$(declare -p mousebinds)"
do_config "attr"      "$(declare -p attributes)"
do_config "set"       "$(declare -p sets)"
do_config "rule"      "$(declare -p rules)"

# unlock, just to be sure
hc unlock

# launch statusbar panel
do_panel

# load on startup
startup_run
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../bash/autostart.sh][dotfiles-bash-autostart]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html

[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]:  {{ asset_path }}/hlwm-03-debug-config.png

[image-bash-01-tree]:  {{ asset_path }}/hlwm-bash-01-tree.png

[dotfiles-bash-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/bash
[dotfiles-bash-autostart]: {{ dotfiles_path }}/bash/autostart.sh
[dotfiles-bash-gmc]:       {{ dotfiles_path }}/bash/assets/gmc.sh
[dotfiles-bash-config]:    {{ dotfiles_path }}/bash/config.sh
[dotfiles-bash-helper]:    {{ dotfiles_path }}/bash/helper.sh
[dotfiles-bash-startup]:   {{ dotfiles_path }}/bash/startup.sh
