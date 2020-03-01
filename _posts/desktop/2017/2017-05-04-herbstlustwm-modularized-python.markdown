---
layout     : post
title      : "Modularized HerbstluftWM in Python"
date       : 2017-05-04 17:35:15 +0700
categories : desktop
tags       : [coding, python, herbstluftwm]
keywords   : [modularized]
author     : epsi
toc        : toc/2017/05/herbstlustwm-modularized.html

opengraph:
  image: /assets/site/images/topics/python.png

excerpt:
  Doing Hersbtluft WM Config using Python.
  
related_link_ids: 
  - 17050135  # HerbstluftWM Overview
  - 17050235  # HerbstluftWM BASH
  - 17050335  # HerbstluftWM Perl
  - 17050435  # HerbstluftWM Python
  - 17050535  # HerbstluftWM Ruby
  - 17050635  # HerbstluftWM PHP
  - 17050735  # HerbstluftWM Lua
  - 17050835  # HerbstluftWM Haskell

---

### Preface

> Goal: Separate Main Flow, Code, and Data.

So anyone can focus to alter special customization in Main Script,
without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[Modularized HerbstluftWM Overview][local-overview]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[gitlab.com/.../dotfiles/.../python/][dotfiles-python-directory]

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

Using **key-value pairs**, a simple data structure.

<code class="code-file">gmc.py</code>

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

*	[gitlab.com/.../dotfiles/.../python/gmc.py][dotfiles-python-gmc]

{% include toc/2017/05/herbstlustwm-modularized-gmc.html %}

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string concatenation all over the place.

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
as shown in the following code.

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

*	[gitlab.com/.../dotfiles/.../python/config.py][dotfiles-python-config]

{% include toc/2017/05/herbstlustwm-modularized-config.html %}

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

#### Debug Herbstclient Command

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight python %}
        print(command+' '+key+' '+value)
{% endhighlight %}

You can see the debugging result in figure below.

[![HerbstluftWM: Debug Command][image-hlwm-03-debug-config]{: .img-responsive }][photo-hlwm-03-debug-config]

#### View Source File:

*	[gitlab.com/.../dotfiles/.../python/helper.py][dotfiles-python-helper]

{% include toc/2017/05/herbstlustwm-modularized-helper.html %}

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
    panel   = dirname + "/panel-lemonbar.py"

    if not os.path.isfile(panel) and os.access(panel, os.X_OK):
        panel = "/etc/xdg/herbstluftwm/panel.sh"
    
    raw = os.popen('herbstclient list_monitors | cut -d: -f1').read()
    monitors = raw.strip().split("\n")

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

*	[gitlab.com/.../dotfiles/.../python/startup.py][dotfiles-python-startup]

{% include toc/2017/05/herbstlustwm-modularized-startup.html %}

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.py</code>

{% highlight python %}
import os

from gmc import color

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

*	[gitlab.com/.../dotfiles/.../python/autostart.py][dotfiles-python-autostart]

{% include toc/2017/05/herbstlustwm-modularized-autostart.html %}

-- -- --

### Coming up Next

After the Window Manager, comes the Panel.

*	[HerbstluftWM Tag Status in Python][local-python-tag-status]

*	[HerbstluftWM Event Idle in Python][local-python-event-idle]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-python-tag-status]:   {{ site.url }}/desktop/2017/06/04/herbstlustwm-tag-status-python.html
[local-python-event-idle]:   {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html

[image-ss-hlwm-nopanel]: {{ asset_path }}/herbstluftwm-nopanel.png
[photo-ss-hlwm-nopanel]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM5QN3sl9KsZs3xlb87UivcHZeLGbSuk2Z8Jx0W?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-hlwm-02-tag-status]:   {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]: {{ asset_path }}/hlwm-03-debug-config-half.png
[photo-hlwm-03-debug-config]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMG-pilS2yhwarThAT23bBYV54z3rYcs2wnaL0E?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-python-01-tree]:       {{ asset_path }}/hlwm-python-01-tree.png

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html

[dotfiles-python-directory]: https://gitlab.com/epsi-rns/dotfiles/tree/master/herbstluftwm/python
[dotfiles-python-autostart]: {{ dotfiles_path }}/python/autostart.py
[dotfiles-python-gmc]:       {{ dotfiles_path }}/python/gmc.py
[dotfiles-python-config]:    {{ dotfiles_path }}/python/config.py
[dotfiles-python-helper]:    {{ dotfiles_path }}/python/helper.py
[dotfiles-python-startup]:   {{ dotfiles_path }}/python/startup.py
