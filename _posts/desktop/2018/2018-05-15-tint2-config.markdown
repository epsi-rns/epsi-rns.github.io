---
layout: post
title:  "Tint2 - Simple Taskbar"
categories: desktop
date:   2018-05-15 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  A brief guidance about Tint2 theme.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Explaining tint2 configuration in general

You must have heard about tint2.
This is an example of what we want to achieve.

![Tint2: Top Wide][image-ss-top-wide]{: .img-responsive }

#### Reading

Always official documentation and the holy archwiki:

*	[https://gitlab.com/o9000/tint2](https://gitlab.com/o9000/tint2)

*	[https://wiki.archlinux.org/index.php/tint2](https://wiki.archlinux.org/index.php/tint2)

#### Gallery

> Salute

I would like to show respect and admiration to this **Adhi Pambudi**.
Who made incredible collection,
all with the config and very nice screenshot,
so we can learn easier.

*	White Cat: [addy-dclxvi/tint2-theme-collections][cat-white]

#### Virtual Desktop

Remember in last **rc.xml**, I use greek name for virtual desktop:

*	1:<code>α</code>, 2:<code>β</code>, 3:<code>γ</code>, 4:<code>δ</code>.

#### GUI Tools

There is, however a GUI tools called <code>tint2conf</code>.

![Tint2: tint2conf GUI tools][image-ss-tint2conf]{: .img-responsive }

But we are not going to use it.
So prepare your text favorite text editor, sucah as geany or ViM.

#### Dotfiles Document

Config is available at:

* [github.com/epsi-rns/dotfiles/.../tint2/config][dotfiles-tutor]

-- -- --

### Tutor 01: Minimalist

This minimalist **tint2** panel has these config

* Backgrounds: Only one background, the taskbar.

* Panel: Only one panel, the taskbar.

* Taskbar: Single Desktop

* Task: with no maximum width

#### Background

You can have as many background as you want.
But this time you only need to define one background.

{% highlight conf %}
# Background 1: Taskbar
rounded = 0
border_width = 0
background_color =         #000000 10
border_color =             #ffffff 10  # white
{% endhighlight %}

A black background with a very thin transparency,
along with white border.

#### Panel

{% highlight conf %}
panel_items = T
panel_size = 100% 30
panel_margin = 0 0
panel_padding = 2 0 2
panel_background_id = 0
wm_menu = 1
panel_dock = 0
panel_position = top center horizontal
{% endhighlight %}

There are many option for <code>panel_items</code> such as 

{% highlight conf %}
panel_items = CLTSEP
{% endhighlight %}

But this time we only use <code>T</code>, the taskbar.

#### Taskbar

{% highlight conf %}
taskbar_mode = single_desktop
taskbar_padding = 8 2 8
taskbar_background_id = 1
taskbar_active_background_id = 1
taskbar_name = 1
taskbar_hide_inactive_tasks = 0
taskbar_hide_different_monitor = 0
taskbar_always_show_all_desktop_tasks = 0
taskbar_name_padding = 4 2

taskbar_name_active_background_id = 0
taskbar_name_active_font_color = #ffffff 100
{% endhighlight %}

This is where we put the background defined above.

{% highlight conf %}
taskbar_background_id = 1
taskbar_active_background_id = 1
{% endhighlight %}

#### Task

{% highlight conf %}
task_text = 1
task_icon = 1
task_font = Monospace 8
task_centered = 1
urgent_nb_of_blink = 100000
task_padding = 2 2 4
task_tooltip = 0
task_font_color = #eeeeee 100
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 01][image-ss-tutor-01]{: .img-responsive }

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-01</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-01
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../tint2rc-tutor-01][dotfiles-tutor-01]

-- -- --

### Tutor 02: More Config

This **tint2** panel has these config

* Backgrounds: Two backgrounds, 1: the taskbar, and 2: the tasks.

* Panel.

* Taskbar.

* Task: with maximum width of 150px.

#### Background

This time we define two backgrounds.

{% highlight conf %}
# Background 1: Taskbar
rounded = 0
border_width = 0
border_sides = TBLR
background_color =         #000000 10  # black
border_color =             #ffffff 10  # white

# Background 2: Any task
rounded = 4
border_width = 0
border_sides = TBLR
background_color =         #ffffff 20  # white
border_color =             #000000 20  # black
{% endhighlight %}


#### Panel

A more complete panel configuration.

{% highlight conf %}
panel_items = T
panel_size = 100% 30
panel_margin = 0 0
panel_padding = 2 0 2
panel_background_id = 0
wm_menu = 1
panel_dock = 0
panel_position = top center horizontal
panel_layer = top
panel_monitor = all
primary_monitor_first = 0
autohide = 0
autohide_show_timeout = 0
autohide_hide_timeout = 0.5
autohide_height = 2
strut_policy = follow_size
panel_window_name = tint2
disable_transparency = 1
mouse_effects = 1
font_shadow = 0
mouse_hover_icon_asb = 100 0 10
mouse_pressed_icon_asb = 100 0 0
{% endhighlight %}

#### Taskbar

A more complete taskbar configuration.

{% highlight conf %}
taskbar_mode = single_desktop
taskbar_padding = 8 2 8
taskbar_background_id = 1
taskbar_active_background_id = 1
taskbar_name = 1
taskbar_hide_inactive_tasks = 0
taskbar_hide_different_monitor = 0
taskbar_always_show_all_desktop_tasks = 0
taskbar_name_padding = 4 2
taskbar_name_background_id = 0
taskbar_name_active_background_id = 0
taskbar_name_font_color = #cccccc 100
taskbar_name_active_font_color = #ffffff 100
taskbar_distribute_size = 0
taskbar_sort_order = none
task_align = left
{% endhighlight %}

#### Task

A more complete task configuration.

{% highlight conf %}
task_text = 1
task_icon = 1
task_font = Monospace 8
task_centered = 1
urgent_nb_of_blink = 100000
task_maximum_size = 150 40
task_padding = 2 2 4
task_tooltip = 0
task_font_color = #eeeeee 100
task_background_id = 2
task_active_background_id = 2
task_urgent_background_id = 2
task_iconified_background_id = 2
mouse_left = toggle_iconify
mouse_middle = none
mouse_right = close
mouse_scroll_up = toggle
mouse_scroll_down = iconify
{% endhighlight %}

The second background defined above:

{% highlight conf %}
task_background_id = 2
task_active_background_id = 2
task_urgent_background_id = 2
task_iconified_background_id = 2
{% endhighlight %}

And limit the width task to 150px:

{% highlight conf %}
task_maximum_size = 150 40
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 02][image-ss-tutor-02]{: .img-responsive }

We are done with basic configuration.
After this we only need a few lines to change the configuration.

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-02</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-02
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../tint2rc-tutor-02][dotfiles-tutor-02]

-- -- --

### Tutor 03: Multi Desktop

This **tint2** panel has these config

* Taskbar: Multi Desktop

* Task: Icon Only, with maximum width 40px.

#### Taskbar

{% highlight conf %}
taskbar_mode = multi_desktop
{% endhighlight %}

#### Task

{% highlight conf %}
task_text = 0
task_icon = 1
task_maximum_size = 40 40
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 03][image-ss-tutor-03]{: .img-responsive }

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-03</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-03
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../tint2rc-tutor-03][dotfiles-tutor-03]

-- -- --

### What's Next

Consider continue reading [ [Tint2: More Backgrounds][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/tint2' %}

[cat-white]:       https://github.com/addy-dclxvi/tint2-theme-collections

[dotfiles-tutor]:       {{ dotfiles }}
[dotfiles-tutor-01]:    {{ dotfiles }}/tint2rc-tutor-01
[dotfiles-tutor-02]:    {{ dotfiles }}/tint2rc-tutor-02
[dotfiles-tutor-03]:    {{ dotfiles }}/tint2rc-tutor-03

[local-part-config]:    /desktop/2018/05/16/tint2-config.html

[image-ss-top-wide]:    {{ asset_path }}/tint2-top-wide.png
[image-ss-tint2conf]:   {{ asset_path }}/tint2conf.png
[image-ss-tutor-01]:    {{ asset_path }}/tint2rc-tutor-01.png
[image-ss-tutor-02]:    {{ asset_path }}/tint2rc-tutor-02.png
[image-ss-tutor-03]:    {{ asset_path }}/tint2rc-tutor-03.png

