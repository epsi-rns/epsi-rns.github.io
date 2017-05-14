---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM in Python"
date:   2017-05-04 17:35:15 +0700
categories: desktop
tags: [coding, python, herbstluftwm]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using Python.
  
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

*	[github.com/.../dotfiles/.../python/][dotfiles-python-directory]

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
This figure will explain how it looks 
in <code>Python script</code> directory.

![HerbstluftWM: Directory Structure][image-python-01-tree]{: .img-responsive }

-- -- --

### Modularizing in Python

	Nothing to say here. Python is simple

#### Declare a module

No need to explicitly define what to export.

Here we export <code>hc</code> function variable from helper module.

	Nothing to write in Python Module

#### Call a module

{% highlight python %}
import helper
from helper import hc
{% endhighlight %}

-- -- --

### System Calls

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.py</code>

{% highlight python %}
def hc(arguments):
    os.system("herbstclient "+arguments)
{% endhighlight %}

<code class="code-file">autostart.py</code>

{% highlight python %}
# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
os.system("echo 35 > /tmp/herbstluftwm-gap");
{% endhighlight %}

-- -- --

### List: Tag Names and Keys

<code class="code-file">config.py</code>

{% highlight python %}
tag_names = list(range(1, 10))
tag_keys  = list(range(1, 10)) + [0]
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
I can use it as a model for herbstluft config.
Both are similar, they use **key-value pairs**.

<code class="code-file">assets/gmc.py</code>

{% highlight python %}
color = {
    'white' : '#ffffff', 
    'black' : '#000000',

    'grey50'      : '#fafafa',
    'grey100'     : '#f5f5f5'
}
{% endhighlight %}

<code class="code-file">autostart.py</code>

{% highlight python %}
# background before wallpaper
os.system("xsetroot -solid '"+color['blue500']+"'")
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../python/assets/gmc.py][dotfiles-python-gmc]

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string interpolation all over the place.

<code class="code-file">config.py</code>

{% highlight python %}
# Modifier variables
s = 'Shift'
c = 'Control'
m = 'Mod4'
a = 'Mod1'

keybinds = {
  # session
    m+'-'+s+'-q' : 'quit',
    m+'-'+s+'-r' : 'reload',
    m+'-'+s+'-c' : 'close'
}
{% endhighlight %}

This config will be utilized in main script

<code class="code-file">autostart.py</code>

{% highlight python %}
helper.do_config("keybind",   config.keybinds)
helper.do_config("keybind",   config.tagskeybinds)
helper.do_config("mousebind", config.mousebinds)
helper.do_config("attr",      config.attributes)
helper.do_config("set",       config.sets)
helper.do_config("rule",      config.rules)
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../python/config.py][dotfiles-python-config]

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.
I can see how simple and clean, Python is.

<code class="code-file">helper.py</code>

{% highlight python %}
def do_config(command, dictionary):
    # loop over dictionary
    for key, value in dictionary.items():
        hc(command+' '+key+' '+value)

        # uncomment to debug in terminal
        # print(command+' '+key+' '+value)
{% endhighlight %}

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight python %}
        print(command+' '+key+' '+value)
{% endhighlight %}

You can see the debugging result in figure below.

![HerbstluftWM: Tag Status][image-hlwm-03-debug-config]{: .img-responsive }

#### View Source File:

*	[github.com/.../dotfiles/.../python/helper.py][dotfiles-python-helper]

-- -- --

### Setting the Tags

Python use <code>from import</code> mechanism.

<code class="code-file">helper.py</code>

{% highlight python %}
import config
from config import tag_names, tag_keys
{% endhighlight %}

{% highlight python %}
def set_tags_with_name():
    hc("rename default '" + str(tag_names[0]) + "' 2>/dev/null || true")
    
    for index, tag_name in enumerate(tag_names):
        hc("add '" + str(tag_names[index]) + "'")
        
        # uncomment to debug in terminal
        # print(index)

        key = tag_keys[index];
        if key:
            hc("keybind Mod4-" + str(key) 
                + " use_index '" + str(index) + "'")
            hc("keybind Mod4-Shift-" + str(key) 
                + " move_index '" + str(index) + "'")
{% endhighlight %}

-- -- --

### Launch the Panel

Two more functions left, it is <code>do_panel</code>
and <code>startup_run</code>.

This two also be easy to do in Python.
<code class="code-file">helper.py</code>

{% highlight python %}
def do_panel():
    dirname = os.path.dirname(os.path.abspath(__file__))
    panel   = dirname + "/../bash/dzen2/panel.sh"

    if not os.path.isfile(panel) and os.access(panel, os.X_OK):
        panel = "/etc/xdg/herbstluftwm/panel.sh"
    
    raw = os.popen('herbstclient list_monitors | cut -d: -f1').read()
    monitors = raw.split("\n")

    for monitor in (monitors):
        os.system(panel + ' ' + str(monitor) + ' &');
{% endhighlight %}

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.py</code>

{% highlight python %}
def run():
    command = 'silent new_attr bool my_not_first_autostart'
    exitcode = os.system('herbstclient ' + command)

    if exitcode == 0:
      # non windowed app
        os.system("compton &")
        os.system("dunst &")
        os.system("parcellite &")
        os.system("nitrogen --restore &")
        os.system("mpd &")

      # windowed app
        os.system("xfce4-terminal &")
        os.system("sleep 1 && firefox &")
        os.system("sleep 2 && geany &")
        os.system("sleep 2 && thunar &")
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../python/startup.py][dotfiles-python-startup]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.py</code>

{% highlight python %}
import os

from assets.gmc import color

import helper
from helper import hc

import config
from config import tag_names

import startup
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.py</code>

{% highlight python %}
# background before wallpaper
os.system("xsetroot -solid '"+color['blue500']+"'")

# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
os.system("echo 35 > /tmp/herbstluftwm-gap");

# do not repaint until unlock
hc("lock");

# standard
hc('keyunbind --all')
hc("mouseunbind --all")
hc("unrule -F")

helper.set_tags_with_name()

# do hash config
helper.do_config("keybind",   config.keybinds)
helper.do_config("keybind",   config.tagskeybinds)
helper.do_config("mousebind", config.mousebinds)
helper.do_config("attr",      config.attributes)
helper.do_config("set",       config.sets)
helper.do_config("rule",      config.rules)

# unlock, just to be sure
hc("unlock")

# launch statusbar panel
helper.do_panel()

# load on startup
startup.run()
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../python/autostart.py][dotfiles-python-autostart]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-python]:     {{ site.url }}/desktop/2017/05/04/herbstlustwm-modularized-python.html

[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]:  {{ asset_path }}/hlwm-03-debug-config.png

[image-python-01-tree]:  {{ asset_path }}/hlwm-python-01-tree.png

[dotfiles-python-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/python
[dotfiles-python-autostart]: {{ dotfiles_path }}/python/autostart.py
[dotfiles-python-gmc]:       {{ dotfiles_path }}/python/assets/gmc.py
[dotfiles-python-config]:    {{ dotfiles_path }}/python/config.py
[dotfiles-python-helper]:    {{ dotfiles_path }}/python/helper.py
[dotfiles-python-startup]:   {{ dotfiles_path }}/python/startup.py
