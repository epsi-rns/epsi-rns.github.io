---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM in Ruby"
date:   2017-05-05 17:35:15 +0700
categories: desktop
tags: [coding, ruby, herbstluftwm]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using Ruby.
  
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

*	[github.com/.../dotfiles/.../ruby/][dotfiles-ruby-directory]

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
This figure will explain how it looks 
in <code>Ruby script</code> directory.

![HerbstluftWM: Directory Structure][image-ruby-01-tree]{: .img-responsive }

-- -- --

### Modularizing in Ruby

	Nothing to say here. Ruby is simple

#### Declare a module

No need to explicitly define what to export.

Here we export <code>hc</code> function variable from helper script.

	Nothing to write in helper Script

You can wrap the script in module.

{% highlight ruby %}
module Config
    Tag_names = Array (1..9)
    Tag_keys  = (Array (1..9)) << 0
end
{% endhighlight %}

#### Call a module

{% highlight ruby %}
require_relative 'helper'
require_relative 'config'
{% endhighlight %}

-- -- --

### System Calls

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
def hc(arguments)
    system("herbstclient #{arguments}");
end
{% endhighlight %}

<code class="code-file">autostart.rb</code>

{% highlight ruby %}
# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap");
{% endhighlight %}

-- -- --

### Array: Tag Names and Keys

<code class="code-file">config.rb</code>

{% highlight ruby %}
    Tag_names = Array (1..9)
    Tag_keys  = (Array (1..9)) << 0
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

<code class="code-file">assets/gmc.rb</code>

{% highlight ruby %}
module GMC
    Color = {
        'white' => '#ffffff',
        'black' => '#000000',

        'grey50'  => '#fafafa',
        'grey100' => '#f5f5f5',
    }
end
{% endhighlight %}

<code class="code-file">autostart.rb</code>

{% highlight ruby %}
# background before wallpaper
system("xsetroot -solid '#{GMC::Color['blue500']}'")
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../ruby/assets/gmc.rb][dotfiles-ruby-gmc]

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string interpolation all over the place.

<code class="code-file">config.rb</code>

{% highlight ruby %}
module Config
    Keybinds = {
      # session
        "#{m}-#{s}-q" => 'quit',
        "#{m}-#{s}-r" => 'reload',
        "#{m}-#{s}-c" => 'close'
    }
end
{% endhighlight %}

This config will be utilized in main script

<code class="code-file">autostart.rb</code>

{% highlight ruby %}
helper::do_config("keybind",   %config::keybinds);
do_config("keybind",   Config::Keybinds)
do_config("keybind",   Config::Tagskeybinds)
do_config("mousebind", Config::Mousebinds)
do_config("attr",      Config::Attributes)
do_config("set",       Config::Sets)
do_config("rule",      Config::Rules)
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../ruby/config.rb][dotfiles-ruby-config]

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.
I can see how simple and clean, Ruby is.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
def do_config(command, hash)
    # loop over hash   
    hash.each do |key, value|
        hc(command+' '+key+' '+value)

        # uncomment to debug in terminal
        # puts(command+' '+key+' '+value)
    end
end
{% endhighlight %}

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight ruby %}
        puts(command+' '+key+' '+value)
{% endhighlight %}

You can see the debugging result in figure below.

![HerbstluftWM: Tag Status][image-hlwm-03-debug-config]{: .img-responsive }

#### View Source File:

*	[github.com/.../dotfiles/.../ruby/helper.rb][dotfiles-ruby-helper]

-- -- --

### Setting the Tags

Nothing special here,
Ruby read all exported variable from modules.
And we defined local <code>tag_names</code> to avoid long namespace.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
def set_tags_with_name()
    tag_names = Config::Tag_names
    tag_keys = Config::Tag_keys

    hc("rename default '#{tag_names[0]}' 2>/dev/null || true")
    
    tag_names.each_with_index do |tag_name, index|
        hc("add '#{tag_names[index]}'")
        
        # uncomment to debug in terminal
        # puts i

        key = tag_keys[index]                
        unless key.to_s.empty?
            hc("keybind Mod4-#{key} use_index '#{index}'");
            hc("keybind Mod4-Shift-#{key} move_index '#{index}'");
        end
    end
end
{% endhighlight %}

-- -- --

### Launch the Panel

Two more functions left, it is <code>do_panel</code>
and <code>startup_run</code>.

This two also be easy to do in Ruby.
<code class="code-file">helper.rb</code>

{% highlight ruby %}
def do_panel()
    panel = __dir__ + "/../bash/dzen2/panel.sh"

    if not File.exist?(panel) and File.executable?(panel)
        panel = "/etc/xdg/herbstluftwm/panel.sh"
    end
   
    raw = IO.popen('herbstclient list_monitors | cut -d: -f1').read()
    monitors = raw.split("\n")

    for monitor in (monitors)
        system("#{panel} #{monitor} &");
    end
end
{% endhighlight %}

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.rb</code>

{% highlight ruby %}
def startup_run()
    command = 'silent new_attr bool my_not_first_autostart'
    system("herbstclient #{command}")
    exitcode = $?.exitstatus

    if (exitcode == 0)
      # non windowed app
        system("compton &")
        system("dunst &")
        system("parcellite &")
        system("nitrogen --restore &")
        system("mpd &")

      # windowed app
        system("xfce4-terminal &")
        system("sleep 1 && firefox &")
        system("sleep 2 && geany &")
        system("sleep 2 && thunar &")
    end
end
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../ruby/startup.rb][dotfiles-ruby-startup]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

<code class="code-file">Header Part: autostart.rb</code>

{% highlight ruby %}
require_relative 'assets/gmc'
require_relative 'helper'
require_relative 'config'
require_relative 'startup'
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.rb</code>

{% highlight ruby %}
# background before wallpaper
system("xsetroot -solid '#{GMC::Color['blue500']}'")

# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap");

# do not repaint until unlock
hc("lock");

# standard
hc('keyunbind --all')
hc("mouseunbind --all")
hc("unrule -F")

set_tags_with_name()

# do hash config
do_config("keybind",   Config::Keybinds)
do_config("keybind",   Config::Tagskeybinds)
do_config("mousebind", Config::Mousebinds)
do_config("attr",      Config::Attributes)
do_config("set",       Config::Sets)
do_config("rule",      Config::Rules)

# unlock, just to be sure
hc("unlock")

# launch statusbar panel (e.g. dzen2 or lemonbar)
do_panel()

# load on startup
startup_run
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../ruby/autostart.rb][dotfiles-ruby-autostart]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-ruby]:     {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-ruby.html

[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]:  {{ asset_path }}/hlwm-03-debug-config.png

[image-ruby-01-tree]:  {{ asset_path }}/hlwm-ruby-01-tree.png

[dotfiles-ruby-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/ruby
[dotfiles-ruby-autostart]: {{ dotfiles_path }}/ruby/autostart.rb
[dotfiles-ruby-gmc]:       {{ dotfiles_path }}/ruby/assets/gmc.rb
[dotfiles-ruby-config]:    {{ dotfiles_path }}/ruby/config.rb
[dotfiles-ruby-helper]:    {{ dotfiles_path }}/ruby/helper.rb
[dotfiles-ruby-startup]:   {{ dotfiles_path }}/ruby/startup.rb