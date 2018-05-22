---
layout: post
title:  "Tint2 - More Backgrounds"
categories: desktop
date:   2018-05-16 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  A brief guidance about Tint2 theme.

---

{% include post/2018/05/toc-openbox-config.html %}

### Tutor 04: Background for Each Tasks

This minimalist **tint2** panel has these config

* Backgrounds: Four more background : 3, 4, 5, 6

* Taskbar: Single Desktop

* Task: Text and Icon

#### Taskbar

{% highlight conf %}
taskbar_mode = single_desktop
{% endhighlight %}

#### Task: Text and Icon

{% highlight conf %}
task_text = 1
task_icon = 1
task_maximum_size = 150 40
{% endhighlight %}

#### Background: Default task

Default task:

{% highlight conf %}
# Background 2: Default task
rounded = 2
border_width = 2
border_sides = B
background_color =         #bbdefb 0   # blue100
border_color =             #1976d2 100 # blue700
background_color_hover =   #64b5f6 30  # blue300
border_color_hover =       #2196f3 100 # blue500
background_color_pressed = #2196f3 60  # blue500
border_color_pressed =     #0d47a1 100 # blue900
{% endhighlight %}

We only use the bottom border,
and no background color (zero transparency).

{% highlight conf %}
# Background 2: Default task
border_sides = B
background_color =         #bbdefb 0   # blue100
{% endhighlight %}

This background also utilize google material color, to make an effect.
When you hover and press, the background become darker and darker.

#### Background: Other task

Active task:

{% highlight conf %}
# Background 3: Active task
rounded = 2
border_width = 2
border_sides = B
background_color =         #ffffff 30  # white
border_color =             #ffeb3b 100 # yellow500
background_color_hover =   #fff176 45  # yellow300
border_color_hover =       #fbc02d 100 # yellow700
background_color_pressed = #ffeb3b 60  # yellow500
border_color_pressed =     #f57f17 100 # yellow900
{% endhighlight %}

Urgent task:

{% highlight conf %}
# Background 4: Urgent task
rounded = 2
border_width = 2
border_sides = B
background_color =         #ffcdd2 0   # red100
border_color =             #d32f2f 100 # red700
background_color_hover =   #e57373 30  # red300
border_color_hover =       #f44336 100 # red500
background_color_pressed = #f44336 60  # red500
border_color_pressed =     #b71c1c 100 # red900
{% endhighlight %}

Iconified task:

{% highlight conf %}
# Background 5: Iconified task
rounded = 2
border_width = 2
border_sides = B
background_color =         #f5f5f5 0   # grey100
border_color =             #9e9e9e 60  # grey500
background_color_hover =   #e0e0e0 30  # grey300
border_color_hover =       #616161 100 # grey700
background_color_pressed = #9e9e9e 60  # grey500
border_color_pressed =     #212121 100 # grey900
{% endhighlight %}

#### Task: Background Color

And this is where we set the predefined background above.

{% highlight conf %}
task_font_color = #eeeeee 100
task_background_id = 2
task_active_background_id = 3
task_urgent_background_id = 4
task_iconified_background_id = 5
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 04][image-ss-tutor-04]{: .img-responsive }

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-04</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-04
{% endhighlight %}

*	[github.com/.../dotfiles/.../tint2rc-tutor-04][dotfiles-tutor-04]

-- -- --

### Tutor 05: Other Background

Consider having other background.

* Backgrounds: Two more backgrounds, 6: tooltip, and 7: active taskbar

* Tooltip

#### Background: Other task

Active task:

{% highlight conf %}
# Background 7: Tooltip
rounded = 2
border_width = 1
background_color =         #64b5f6 100 # blue300
border_color =             #000000 100 # white

# Background 7: Active Taskbar
rounded = 2
border_width = 1
border_sides = TBLR
background_color =         #e57373 50  # red300
border_color =             #ffffff 30  # white
background_color_hover =   #f44336 50  # red500
border_color_hover =       #ffffff 30  # white
background_color_pressed = #d32f2f 50  # red700
border_color_pressed =     #ffffff 30  # white
{% endhighlight %}

#### Taskbar

Change the plain taskbar name background color to a colorful one.

{% highlight conf %}
taskbar_name_active_background_id = 7
{% endhighlight %}

#### Task: Tooltip

Enable the tooltip.

{% highlight conf %}
task_tooltip = 1
{% endhighlight %}

#### Tooltip

A new section here.

{% highlight conf %}
tooltip_show_timeout = 0.5
tooltip_hide_timeout = 0.1
tooltip_padding = 2 2
tooltip_background_id = 6
tooltip_font_color = #222222 100
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 05][image-ss-tutor-05]{: .img-responsive }

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-05</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-05
{% endhighlight %}

*	[github.com/.../dotfiles/.../tint2rc-tutor-05][dotfiles-tutor-05]

-- -- --

### Tutor 06: Bright Background

Consider changing from dark panel to bright panel.

* Backgrounds: Change background, 1: taskbar

* Font Color: dark (for single desktop with text).

* Taskbar: Multi Desktop

* Task: Icon Only, with maximum width 40px.

#### Background: Taskbar

Default task:

{% highlight conf %}
# Background 1: Taskbar
rounded = 4
border_width = 1
border_sides = TBLR
background_color =         #ffffff 50
border_color =             #444444 30
{% endhighlight %}

And make a slight modification.
Removing background, by using zero transparency.

{% highlight conf %}
# Background 3: Active task
rounded = 2
border_width = 2
border_sides = B
background_color =         #ffffff 0  # white
border_color =             #ffeb3b 100 # yellow500
...
{% endhighlight %}

#### Taskbar

Multi Desktop:

{% highlight conf %}
taskbar_mode = multi_desktop
{% endhighlight %}

Background:

{% highlight conf %}
taskbar_background_id = 1
taskbar_active_background_id = 1
{% endhighlight %}

Font Color:

{% highlight conf %}
taskbar_name_font_color = #444444 100
taskbar_name_active_font_color = #222222 100
{% endhighlight %}

#### Task

{% highlight conf %}
task_text = 0
task_icon = 1
task_maximum_size = 40 40
{% endhighlight %}

Font Color:

{% highlight conf %}
task_font_color = #222222 100
{% endhighlight %}

#### Screenshot

![Tint2: Tutor 06][image-ss-tutor-06]{: .img-responsive }

We are done with backgrounds.

#### Source

Have a look at the <code class="code-file">tint2rc-tutor-06</code>:

{% highlight bash %}
$ tint2 -c ~/.config/tint2/tint2rc-tutor-06
{% endhighlight %}

*	[github.com/.../dotfiles/.../tint2rc-tutor-06][dotfiles-tutor-06]

-- -- --

### What's Next

Consider continue reading [ [Tint2: Panel Items][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/openbox/tint2' %}

[dotfiles-tutor-04]:    {{ dotfiles }}/tint2rc-tutor-04
[dotfiles-tutor-05]:    {{ dotfiles }}/tint2rc-tutor-05
[dotfiles-tutor-06]:    {{ dotfiles }}/tint2rc-tutor-06

[local-part-config]:  /desktop/2018/05/17/tint2-config.html

[image-ss-tutor-04]:    {{ asset_path }}/tint2rc-tutor-04.png
[image-ss-tutor-05]:    {{ asset_path }}/tint2rc-tutor-05.png
[image-ss-tutor-06]:    {{ asset_path }}/tint2rc-tutor-06.png
