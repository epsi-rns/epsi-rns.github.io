---
layout: post
title:  "Fluxbox Config - General"
categories: desktop
date      : 2018-04-03 09:25:15 +0700
tags      : [fluxbox]
keywords  : [tutorial, configuration]
author: epsi

opengraph:
  image: /assets/site/images/topics/fluxbox.png

excerpt:
  A brief explanation about Fluxbox config.

---

{% include post/2018/04/toc-fluxbox.html %}

### Preface

> Goal: Explaining Fluxbox Configuration

### Config Directory

Fluxbox config directory contain a bunch of config files.

{% highlight bash %}
$ ls ~/.fluxbox
apps           lastwallpaper  startup
backgrounds    menu           styles
fbrun_history  overlay        windowmenu
init           pixmaps
keys           slitlist
{% endhighlight %}

![fluxbox Config: directory][image-ss-directory]{: .img-responsive }

#### Dotfiles Document

Config is available at:

* [github.com/epsi-rns/dotfiles/.../fluxbox/config][dotfiles-tutor]

-- -- --

### Config: init

Consider start with <code class="code-file">~/.fluxbox/init</code> file.

*	[fluxbox-wiki.org/.../Editing_the_init_file.html](http://fluxbox-wiki.org/category/howtos/en/Editing_the_init_file.html)

You can either edit the init file directly, or you can change using fluxbox menu.

{% highlight conf %}
session.screen0.toolbar.autoHide:	true
session.screen0.toolbar.widthPercent:	90
{% endhighlight %}

![fluxbox Config: Menu Init][image-ss-menu-init]{: .img-responsive }

You can examine some sub-setting in respective config files.

{% highlight conf %}
session.screen0.windowMenu: ~/.fluxbox/windowmenu
session.keyFile:            ~/.fluxbox/keys
session.appsFile:           ~/.fluxbox/apps
session.styleFile:          ~/.fluxbox/styles/exilorate
session.menuFile:           ~/.fluxbox/menu
session.slitlistFile:       ~/.fluxbox/slitlist
session.styleOverlay:       ~/.fluxbox/overlay
{% endhighlight %}

#### Source

*	[gitlab.com/.../dotfiles/.../init][dotfiles-init]

-- -- --

### Config: Startup

Most my <code class="code-file">~/.fluxbox/startup</code> file only configure,
common non-windowed-application in window manager.

#### Wallpaper

You can set wallpaper using either <code>fbsetbg</code>,
or <code>feh --bgscale</code>, or <code>nitrogen</code>.

{% highlight bash %}
fbsetbg -f ~/Pictures/landscape_forest_mountain.jpg &
{% endhighlight %}

{% highlight bash %}
nitrogen --restore &
{% endhighlight %}

#### Internet

{% highlight bash %}
nm-applet &
{% endhighlight %}

#### Other Background Application

If you want, you can add other background application,
such as notification, using dunst or else.

{% highlight bash %}
compton &
dunst &
parcellite &
mpd &
{% endhighlight %}

#### Complete

{% highlight bash %}
#!/bin/sh

nitrogen --restore &
nm-applet &

compton &
# dunst &
parcellite &
mpd &

# And last but not least we start fluxbox.
# Because it is the last app you have to run it with ''exec'' before it.

exec fluxbox
{% endhighlight %}

#### Source

*	[gitlab.com/.../dotfiles/.../startup][dotfiles-startup]

-- -- --

### Workspace Name

You can have nice workspace name

{% highlight conf %}
session.screen0.workspaceNames:	1:α, 2:β, 3:γ, 4:δ,
{% endhighlight %}

-- -- --

### Config: Key Binding

This <code class="code-file">~/.fluxbox/keys</code> file is self explanatory.
No need to be a wm-hacker to configure.

{% highlight conf %}
# change to a specific workspace
Control F1 :Workspace 1
Control F2 :Workspace 2
Control F3 :Workspace 3
Control F4 :Workspace 4
{% endhighlight %}

#### More Custom Key Binding.

Mostly borrowed from my i3 configuration.

{% highlight conf %}
# custom keybind
Mod4 Return  :Exec xfce4-terminal
Mod4 d       :Exec i3-dmenu-desktop
Mod4 Mod1 d  :Exec dmenu_run
Mod4 Shift d :Exec rofi -show run -opacity 90
Mod4 Ctrl d  :Exec rofi -show window -opacity 90
Mod4 t       :ToggleDecor
{% endhighlight %}

#### Source

*	[gitlab.com/.../dotfiles/.../keys][dotfiles-keys]

-- -- --

### Rules

This <code class="code-file">~/.fluxbox/apps</code> file, govern each application.

*	[fluxbox-wiki.org/.../Editing_the_apps_file.html](http://fluxbox-wiki.org/category/howtos/en/Editing_the_apps_file.html)

For example, we can set each urxvt to be launched,
in the center of the window,
with specific width and height,
and put it in specific workspace.

{% highlight conf %}
[app] (name=urxvt)
  [Position]    (CENTER)    {0 0}
  [Dimensions]  {800 480}
  [Deco]        {NONE}
[end]
{% endhighlight %}

Or do automatic grouping.

{% highlight conf %}
[group]  (workspace=[current])
  [app] (name=xterm) (class=XTerm)
[end]
{% endhighlight %}

#### Source

*	[gitlab.com/.../dotfiles/.../apps][dotfiles-apps]

-- -- --

### Panel

#### Hide Toolbar

Toolbar is not the only option.
You can autohide it.

{% highlight conf %}
session.screen0.toolbar.autoHide:	true
{% endhighlight %}

#### Other Panel: tint2

You can use other panel as well.
For example: double tint2 panel (top and bottom), as figure below.

[![fluxbox Config: tint2 panel][image-ss-tint2]{: .img-responsive }][photo-ss-tint2]

Please click the screenshot to see the original size.

-- -- --

### What's Next

This is configuration in general,
we need to go deep with [ [Config: Menu Part][local-part-menu] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/fluxbox/config' %}

[dotfiles-tutor]:   {{ dotfiles }}
[dotfiles-init]:    {{ dotfiles }}/init
[dotfiles-startup]: {{ dotfiles }}/startup
[dotfiles-apps]:    {{ dotfiles }}/apps

[local-part-menu]:  /desktop/2018/04/04/fluxbox-config.html

[image-ss-directory]:     {{ asset_path }}/fluxbox-directory.png
[image-ss-menu-init]:     {{ asset_path }}/fluxbox-menu-init.png

[image-ss-tint2]:         {{ asset_path }}/fluxbox-tint2.png
[photo-ss-tint2]:         https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNUYbPsqONpSXNXURca7MESyvLE2u1XPOV3NlKz?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

