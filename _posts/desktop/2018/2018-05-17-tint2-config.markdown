---
layout     : post
title      : "Tint2 - Panel Items"
categories : desktop
date       : 2018-05-17 09:25:15 +0700
tags       : [openbox]
keywords   : [tutorial, configuration, tint2]
author     : epsi
toc        : toc/2018/05/toc-openbox-config.html

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  A brief guidance about Tint2 theme.
---

### Tutor 07: Common Items

We need to concentrate on common component, other than taskbar.

* C: Clock

* L: Launcher

* S: System tray (notification area)

#### Panel

CLTS: Clock, Launcher, Taskbar, and System Tray

{% highlight conf %}
panel_items = CLTS
panel_size = 90% 30
{% endhighlight %}

This time we need to adjust the panel size to 90%,
so we have space to click on menu when application in maximized state.
If you have wide display, you may consider adjusting the panel size to only 50%,

#### Clock

Using (almost) default configuration.

{% highlight conf %}
time1_format = %H:%M
time2_format = %A %d %B
time1_timezone = 
time2_timezone = 
clock_font_color = #222222 100
clock_padding = 2 0
clock_background_id = 1
clock_tooltip = 
clock_tooltip_timezone = 
clock_lclick_command = 
clock_rclick_command = orage
clock_mclick_command = 
clock_uwheel_command = 
clock_dwheel_command = 
{% endhighlight %}

#### Launcher

Using (almost) default configuration.
Except, I put my own favorite application.

{% highlight conf %}
launcher_padding = 2 4 2
launcher_background_id = 4
launcher_icon_background_id = 0
launcher_icon_size = 24
launcher_icon_asb = 100 0 0
launcher_icon_theme_override = 0
startup_notifications = 1
launcher_tooltip = 1

launcher_item_app = /usr/share/applications/tint2conf.desktop
launcher_item_app = /usr/share/applications/rxvt-unicode.desktop 
launcher_item_app = /usr/share/applications/firefox.desktop
launcher_item_app = /usr/share/applications/thunderbird.desktop 
# launcher_item_app = /usr/share/applications/thunar.desktop 
{% endhighlight %}

#### System tray (notification area)

Using default configuration.

{% highlight conf %}
systray_padding = 0 4 2
systray_background_id = 4
systray_sort = ascending
systray_icon_size = 32
systray_icon_asb = 100 0 0
systray_monitor = 1
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 07][image-ss-tutor-07]{: .img-responsive }

#### Background

I set bright background, the same with taskbar.

{% highlight conf %}
clock_font_color = #222222 100
clock_background_id = 1
{% endhighlight %}

I do not set bright background for all the item,
for aestethic reason, I like underline better.

{% highlight conf %}
launcher_background_id = 4
{% endhighlight %}

and this one

{% highlight conf %}
systray_background_id = 4
{% endhighlight %}

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-07</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-07
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../tint2rc-tutor-07][dotfiles-tutor-07]

-- -- --

### Tutor 08: Additional Items

Not so common component

* E: Executor

* P: Button

#### Executor: MPC Controller.

You can have as many <code>executor</code> as you need.
It means, you can make a monitoring system in tint2, such as CPU, memory and stuff.

{% highlight conf %}
execp = new
execp_command = mpc current --format '%title%\n%artist%' | head -n 2
execp_interval = 40
execp_has_icon = 0
execp_cache_icon = 1
execp_continuous = 0
execp_markup = 1
execp_tooltip = 
execp_lclick_command = mpc next
execp_rclick_command = mpc prev
execp_mclick_command = mpc toggle
execp_uwheel_command = mpc play
execp_dwheel_command = mpc stop
execp_font = xos4 Terminess Powerline 8
execp_font_color = #444444 100
execp_padding = 8 0
execp_background_id = 2
execp_centered = 0
execp_icon_w = 0
execp_icon_h = 0
{% endhighlight %}

Now you've got yourself <code>mpc</code> controller,
only using tint2 and mouse, without the need to open terminal.

{% highlight conf %}
execp_lclick_command = mpc next
execp_rclick_command = mpc prev
execp_mclick_command = mpc toggle
execp_uwheel_command = mpc play
execp_dwheel_command = mpc stop
{% endhighlight %}

I also have a chance to use Terminus.

{% highlight conf %}
execp_font = xos4 Terminess Powerline 8
{% endhighlight %}

#### Button

You can have as many <code>button</code> as you need.
Not just URL launcher as below, but any command is fine.

We need <code>png</code> image for each button icon,
or just text.

{% highlight conf %}
button = new
button_icon = /home/epsi/.config/tint2/logo-orb-spiral.png
button_text = 
button_lclick_command = firefox epsi-rns.github.io
button_rclick_command = 
button_mclick_command = 
button_uwheel_command = 
button_dwheel_command = 
button_font_color = #000000 100
button_padding = 0 0
button_background_id = 4
button_centered = 0
button_max_icon_size = 0
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 08][image-ss-tutor-08]{: .img-responsive }

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-08</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-08
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../tint2rc-tutor-08][dotfiles-tutor-08]

-- -- --

### Tutor 09: Finishing

To complete this guidance, this is the complete panel.

{% highlight conf %}
panel_items = CLTSEP
{% endhighlight %}

We are done. Finished.

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-09</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-09
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../tint2rc-tutor-09][dotfiles-tutor-09]

-- -- --

### Autostart

Do not forget to put your <code>tint2</code> at <code>autostart</code>.

{% highlight conf %}
tint2
{% endhighlight %}

Or for dual panel

{% highlight conf %}
tint2 -c ~/.config/tint2/tint2rc-top &
tint2 -c ~/.config/tint2/tint2rc-bottom &
{% endhighlight %}

![Tint2: dual panel][image-ss-dual-panel]{: .img-responsive }

#### Source

Have a look at the <code class="code-file">tint2rc-top</code>,
and <code class="code-file">tint2rc-bottom</code>:

*	[gitlab.com/.../dotfiles/.../tint2rc-top][dotfiles-top]

*	[gitlab.com/.../dotfiles/.../tint2rc-bottom][dotfiles-bottom]

-- -- --

### What's Next

We are finished with tint2 configuration.
Consider continue reading [ [Install: openbox, tint2][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/tint2' %}

[dotfiles-tutor-07]:    {{ dotfiles }}/tint2rc-tutor-07
[dotfiles-tutor-08]:    {{ dotfiles }}/tint2rc-tutor-08
[dotfiles-tutor-09]:    {{ dotfiles }}/tint2rc-tutor-09
[dotfiles-top]:         {{ dotfiles }}/tint2rc-top
[dotfiles-bottom]:      {{ dotfiles }}/tint2rc-bottom

[local-part-config]:  /desktop/2018/07/15/openbox-install.html

[image-ss-tutor-07]:    {{ asset_path }}/tint2rc-tutor-07.png
[image-ss-tutor-08]:    {{ asset_path }}/tint2rc-tutor-08.png
[image-ss-dual-panel]:  {{ asset_path }}/tint2rc-dual-panel.png
