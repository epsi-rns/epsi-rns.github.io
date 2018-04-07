---
layout: post
title:  "Fluxbox Theme - Style Part"
categories: desktop
date:   2018-04-07 09:25:15 +0700
tags: [fluxbox, theme, inkscape]
author: epsi

excerpt:
  A brief explanation about Fluxbox theme (or style).
  Step by step, using Inkscape, one SVG file to create each XPM part.

---

### Preface

> Goal: Explaining Fluxbox Style Configuration

After XFWM4 Theme, here comes my curiosity about Fluxbox Style.

#### Reading

Before you begin.
This is a nice resource:

*	[fluxbox-wiki.org](http://fluxbox-wiki.org/category/howtos/en/)

#### Managing Configuration

Based on <code>ArchBlu</code> configuration,
the Fluxbox config has four parts:

* Fonts

* Window

* Menu

* Toolbar

This how this article arranged.

#### Where to put style configuration

Just in case you are new to fluxbox config,
here we I put my style.

	/home/epsi/.fluxbox/styles/exilorate/

#### Configuration Source

<code>theme.cfg</code> is available at:

* [github.com/epsi-rns/dotfiles/.../exilorate][dotfiles-tutor]

-- -- --

### Fonts

> Terminus

I can't find a better replacement for this Terminus font.

{% highlight conf %}
menu.title.font:                  terminus-8
menu.frame.font:                  terminus-8
toolbar.clock.font:               terminus-8
toolbar.workspace.font:           terminus-8
toolbar.iconbar.focused.font:     terminus-8
toolbar.iconbar.unfocused.font:   terminus-8
window.font:                      terminus-8
{% endhighlight %}

-- -- --

### Window

Here maping, between the XPM and button required by Fluxbox.

#### Common

This is the common config, before we go into button detail.

{% highlight conf %}
window.bevelWidth:                0
window.shade:                     true
window.borderWidth:               1
window.borderColor:               #616161

window.justify:                   center

window.title.height:              24

window.handleWidth:               3
{% endhighlight %}

#### Focused

{% highlight conf %}
window.title.focus.pixmap:        title-active.xpm

window.label.focus.pixmap:        label-active.xpm
window.label.focus.textColor:     #000000

window.stick.pixmap:              stick-active.xpm
window.stuck.pixmap:              stuck-active.xpm
window.close.pixmap:              close-active.xpm
window.iconify.pixmap:            iconify-active.xpm
window.maximize.pixmap:           maximize-active.xpm

window.handle.focus:              flat
window.handle.focus.color:        #000000

window.grip.focus:                flat
window.grip.focus.color:          #ffeb3b
{% endhighlight %}

![fluxbox Style: exilorate focused window][image-ss-focused]{: .img-responsive }

#### Pressed

Can you see the <code>x</code> on close icon ?

{% highlight conf %}
window.stick.pressed.pixmap:      stick-pressed.xpm
window.stuck.pressed.pixmap:      stuck-pressed.xpm
window.close.pressed.pixmap:      close-pressed.xpm
window.iconify.pressed.pixmap:    iconify-pressed.xpm
window.maximize.pressed.pixmap:   maximize-pressed.xpm
{% endhighlight %}

#### Unfocused

{% highlight conf %}
window.title.unfocus.pixmap:      title-inactive.xpm

window.label.unfocus.pixmap:      label-inactive.xpm
window.label.unfocus.textColor:   #1976d2

window.stick.unfocus.pixmap:      stick-inactive.xpm
window.stuck.unfocus.pixmap:      stuck-inactive.xpm
window.close.unfocus.pixmap:      close-inactive.xpm
window.iconify.unfocus.pixmap:    iconify-inactive.xpm
window.maximize.unfocus.pixmap:   maximize-inactive.xpm

window.handle.unfocus:            flat
window.handle.unfocus.color:      #ffffff

window.grip.unfocus:              flat
window.grip.unfocus.color:        #1976d2
{% endhighlight %}

![fluxbox Style: exilorate unfocused window][image-ss-unfocused]{: .img-responsive }

#### Complete

{% highlight conf %}
window.bevelWidth:                0
window.shade:                     true
window.borderWidth:               1
window.borderColor:               #616161

window.justify:                   center

window.title.height:              24

window.title.focus.pixmap:        title-active.xpm
window.title.unfocus.pixmap:      title-inactive.xpm

window.label.focus.pixmap:        label-active.xpm
window.label.focus.textColor:     #000000
window.label.unfocus.pixmap:      label-inactive.xpm
window.label.unfocus.textColor:   #1976d2

window.stick.pixmap:              stick-active.xpm
window.stick.unfocus.pixmap:      stick-inactive.xpm
window.stick.pressed.pixmap:      stick-pressed.xpm

window.stuck.pixmap:              stuck-active.xpm
window.stuck.unfocus.pixmap:      stuck-inactive.xpm
window.stuck.pressed.pixmap:      stuck-pressed.xpm

window.close.pixmap:              close-active.xpm
window.close.unfocus.pixmap:      close-inactive.xpm
window.close.pressed.pixmap:      close-pressed.xpm

window.iconify.pixmap:            iconify-active.xpm
window.iconify.unfocus.pixmap:    iconify-inactive.xpm
window.iconify.pressed.pixmap:    iconify-pressed.xpm

window.maximize.pixmap:           maximize-active.xpm
window.maximize.unfocus.pixmap:   maximize-inactive.xpm
window.maximize.pressed.pixmap:   maximize-pressed.xpm

window.handle.focus:              flat
window.handle.focus.color:        #000000
window.handle.unfocus:            flat
window.handle.unfocus.color:      #ffffff
window.handleWidth:               3

window.grip.focus:                flat
window.grip.focus.color:          #ffeb3b
window.grip.unfocus:              flat
window.grip.unfocus.color:        #1976d2
{% endhighlight %}

-- -- --

### Menu

#### Pixmap

There are some XPM utilized here.

{% highlight conf %}
menu.submenu.pixmap:              bullet.xpm
menu.selected.pixmap:             selected.xpm
menu.unselected.pixmap:           unselected.xpm

menu.title:                       flat
menu.title.pixmap:                menu-active.xpm

menu.hilite:                      flat
menu.hilite.pixmap:               menu-hilite.xpm  
{% endhighlight %}

![fluxbox Style: exilorate menu][image-ss-menu]{: .img-responsive }

#### Complete

It is self explanatory.

{% highlight conf %}
menu.bevelWidth:                  3

menu.itemHeight:                  24
menu.titleHeight:                 24

menu.borderColor:                 #e0e0e0
menu.borderWidth:                 1

menu.bullet.position:             left
menu.bullet:                      flat
menu.submenu.pixmap:              bullet.xpm
menu.selected.pixmap:             selected.xpm
menu.unselected.pixmap:           unselected.xpm

menu.title:                       flat
menu.title.pixmap:                menu-active.xpm
menu.title.justify:               center
menu.title.textColor:             #000000

menu.frame:                       flat gradient diagonal
menu.frame.pixmap:                
menu.frame.justify:               left
menu.frame.color:                 #e0e0e0
menu.frame.colorTo:               #9e9e9e
menu.frame.textColor:             #000000
menu.frame.disableColor:          #e0e0e0

menu.hilite:                      flat
menu.hilite.pixmap:               menu-hilite.xpm                
menu.hilite.textColor:            #ffffff
{% endhighlight %}

-- -- --

### Toolbar

#### Pixmap

Consider reuse, already baked XPM.

{% highlight conf %}
toolbar:                          flat
toolbar.pixmap:                   title-active.xpm

toolbar.clock:                    flat
toolbar.clock.pixmap:             menu-inactive.xpm

toolbar.workspace:                flat
toolbar.workspace.pixmap:         menu-active.xpm

toolbar.button:                   flat 
toolbar.button.pixmap:            menu-active.xpm

toolbar.iconbar.empty:                flat
toolbar.iconbar.empty.pixmap:         label-inactive.xpm

toolbar.iconbar.focused:              flat
toolbar.iconbar.focused.pixmap:       title-active.xpm

toolbar.iconbar.unfocused:            flat
toolbar.iconbar.unfocused.pixmap:     title-inactive.xpm
{% endhighlight %}

#### Preview

I separate the toolbar image into two: Left, and Right. Because it is a long toolbar.

![fluxbox Style: exilorate toolbar left][image-ss-toolbar-left]{: .img-responsive }

And

![fluxbox Style: exilorate toolbar right][image-ss-toolbar-right]{: .img-responsive }

#### Complete

It is self explanatory.

{% highlight conf %}
toolbar.shaped:                   false
toolbar.bevelWidth:               0

toolbar.borderWidth:              1
toolbar.borderColor:              #e0e0e0

toolbar.height:                   24

toolbar.justify:                  center

toolbar:                          flat
toolbar.pixmap:                   title-active.xpm

toolbar.clock:                    flat
toolbar.clock.pixmap:             menu-inactive.xpm
toolbar.clock.justify:            center
toolbar.clock.textColor:          #000000

toolbar.workspace:                flat
toolbar.workspace.pixmap:         menu-active.xpm
toolbar.workspace.justify:        center
toolbar.workspace.textColor:      #000000

toolbar.button:                   flat 
toolbar.button.pixmap:            menu-active.xpm

toolbar.iconbar.empty:                flat
toolbar.iconbar.empty.pixmap:         label-inactive.xpm
toolbar.iconbar.focused:              flat
toolbar.iconbar.focused.justify:      center
toolbar.iconbar.focused.pixmap:       title-active.xpm
toolbar.iconbar.focused.textColor:    #000000
toolbar.iconbar.unfocused:            flat
toolbar.iconbar.unfocused.justify:    center
toolbar.iconbar.unfocused.pixmap:     title-inactive.xpm
toolbar.iconbar.unfocused.textColor:  #1976d2
{% endhighlight %}

-- -- --

### Conclusion

This is only the Style Part.
Do not forget to read the [ [Inkscape Part][local-part-inkscape] ]

I think we are done with Fluxbox style configuration.
Have fun with Fluxbox theming.



[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/04' %}

[dotfiles-tutor]:  https://github.com/epsi-rns/dotfiles/tree/master/fluxbox/styles/exilorate

[local-part-inkscape]: /desktop/2018/04/06/fluxbox-style.html

[image-ss-focused]:    {{ asset_path }}/fluxbox-exilorate-focused.png
[image-ss-unfocused]:  {{ asset_path }}/fluxbox-exilorate-unfocused.png
[image-ss-menu]:       {{ asset_path }}/fluxbox-exilorate-menu.png
[image-ss-toolbar-left]:  {{ asset_path }}/fluxbox-exilorate-toolbar-left.png
[image-ss-toolbar-right]: {{ asset_path }}/fluxbox-exilorate-toolbar-right.png
