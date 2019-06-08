---
layout: post
title:  "Modularized HerbstluftWM in Ruby"
date      : 2017-05-05 17:35:15 +0700
categories: desktop
tags      : [coding, ruby, herbstluftwm]
keywords  : [modularized]
author: epsi

opengraph:
  image: /assets/site/images/topics/ruby.png

excerpt:
  Doing Hersbtluft WM Config using Ruby.
  
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

*	[gitlab.com/.../dotfiles/.../ruby/][dotfiles-ruby-directory]

-- -- --

{% include post/2017/05/herbstlustwm-modularized-language.md %}

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
  system("herbstclient #{arguments}")
end
{% endhighlight %}

<code class="code-file">autostart.rb</code>

{% highlight ruby %}
# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap")
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

Using **key-value pairs**, a simple data structure.

<code class="code-file">gmc.rb</code>

{% highlight ruby %}
module GMC
  COLOR = {
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
system("xsetroot -solid '#{GMC::COLOR['blue500']}'")
{% endhighlight %}

#### View Source File:

*	[gitlab.com/.../dotfiles/.../ruby/gmc.rb][dotfiles-ruby-gmc]

{% include post/2017/05/herbstlustwm-modularized-gmc.md %}

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string interpolation all over the place.

<code class="code-file">config.rb</code>

{% highlight ruby %}
module Config
  # Modifier variables
  s = 'Shift'
  c = 'Control'
  m = 'Mod4'
  a = 'Mod1'

  Keybinds = {
  # session
    "#{m}-#{s}-q" => 'quit',
    "#{m}-#{s}-r" => 'reload',
    "#{m}-#{s}-c" => 'close'
  }
end
{% endhighlight %}

This config will be utilized in main script
as shown in the following code.

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

*	[gitlab.com/.../dotfiles/.../ruby/config.rb][dotfiles-ruby-config]

{% include post/2017/05/herbstlustwm-modularized-config.md %}

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

#### Debug Herbstclient Command

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment this line to see what happened.

{% highlight ruby %}
    puts(command+' '+key+' '+value)
{% endhighlight %}

You can see the debugging result in figure below.

[![HerbstluftWM: Debug Command][image-hlwm-03-debug-config]{: .img-responsive }][photo-hlwm-03-debug-config]

#### View Source File:

*	[gitlab.com/.../dotfiles/.../ruby/helper.rb][dotfiles-ruby-helper]

{% include post/2017/05/herbstlustwm-modularized-helper.md %}

-- -- --

### Setting the Tags

Nothing special here,
Ruby read all exported variable from modules.
And I define local <code>tag_names</code> to avoid long namespace.

<code class="code-file">helper.rb</code>

{% highlight ruby %}
def set_tags_with_name()
  tag_names = Config::Tag_names
  tag_keys = Config::Tag_keys

  hc("rename default '#{tag_names[0]}' 2>/dev/null || true")
    
  tag_names.each_with_index do |tag_name, index|
    hc("add '#{tag_names[index]}'")
        
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
  panel = __dir__ + "/panel-lemonbar.rb"

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

*	[gitlab.com/.../dotfiles/.../ruby/startup.rb][dotfiles-ruby-startup]

{% include post/2017/05/herbstlustwm-modularized-startup.md %}

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.rb</code>

{% highlight ruby %}
require_relative 'gmc'
require_relative 'helper'
require_relative 'config'
require_relative 'startup'
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.rb</code>

{% highlight ruby %}
# background before wallpaper
system("xsetroot -solid '#{GMC::COLOR['blue500']}'")

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

*	[gitlab.com/.../dotfiles/.../ruby/autostart.rb][dotfiles-ruby-autostart]

{% include post/2017/05/herbstlustwm-modularized-autostart.md %}

-- -- --

### Coming up Next

After the Window Manager, comes the Panel.

*	[HerbstluftWM Tag Status in Ruby][local-ruby-tag-status]

*	[HerbstluftWM Event Idle in Ruby][local-ruby-event-idle]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-ruby-tag-status]:   {{ site.url }}/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
[local-ruby-event-idle]:   {{ site.url }}/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html

[image-ss-hlwm-nopanel]: {{ asset_path }}/herbstluftwm-nopanel.png
[photo-ss-hlwm-nopanel]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM5QN3sl9KsZs3xlb87UivcHZeLGbSuk2Z8Jx0W?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-hlwm-02-tag-status]:   {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]: {{ asset_path }}/hlwm-03-debug-config-half.png
[photo-hlwm-03-debug-config]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMG-pilS2yhwarThAT23bBYV54z3rYcs2wnaL0E?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ruby-01-tree]:         {{ asset_path }}/hlwm-ruby-01-tree.png

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html

[dotfiles-ruby-directory]: https://gitlab.com/epsi-rns/dotfiles/tree/master/herbstluftwm/ruby
[dotfiles-ruby-autostart]: {{ dotfiles_path }}/ruby/autostart.rb
[dotfiles-ruby-gmc]:       {{ dotfiles_path }}/ruby/gmc.rb
[dotfiles-ruby-config]:    {{ dotfiles_path }}/ruby/config.rb
[dotfiles-ruby-helper]:    {{ dotfiles_path }}/ruby/helper.rb
[dotfiles-ruby-startup]:   {{ dotfiles_path }}/ruby/startup.rb
