---
layout: post
title:  "Openbox Config - General rc.xml"
categories: desktop
date      : 2018-05-03 09:25:15 +0700
tags      : [openbox]
keywords  : [tutorial, configuration]
author: epsi

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  A brief explanation about Openbox rc.xml configuration in general.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Explaining Openbox rc.xml configuration in general

#### Reading

<code class="code-file">rc.xml</code> has been comprehensively covered in this article.

*	[http://openbox.org/wiki/Help:Configuration](http://openbox.org/wiki/Help:Configuration)

Therefore, this article only discuss about changes in **rc.xml** and stuff.

That article above clearly defined 13 sections of xml:
Resistance, Focus, Placement, Theme, Desktops, Resize (and move),
Applications, Keyboard, Mouse, Margins, Menu, Dock, and Coordinates.

#### OBConf

There is a GUI tools to edit the **rc.xml**, called <code>obconf</code>.

![openbox Config: theme][image-ss-obconf]{: .img-responsive }

#### Source

*	[gitlab.com/.../dotfiles/.../rc.xml][dotfiles-rc-xml]

-- -- --

### Theme

The representative xml of theme configuration,
would looks similar as example below.

{% highlight xml %}
  <theme>
    <name>flatypuss</name>
    <titleLayout>NLSDIMC</titleLayout>
    ...
    <keepBorder>yes</keepBorder>
    <animateIconify>yes</animateIconify>
    <font place="ActiveWindow">
      ...
    </font>
  </theme>
{% endhighlight %}

Where the character means:

*	N: window icon

*	L: window label (AKA title).

*	I: iconify

*	M: maximize

*	C: close

*	S: shade (roll up/down)

*	D: omnipresent (on all desktops).

-- -- --

### Desktop

I like to use, geeky greek character to identify my desktop.

{% highlight xml %}
  <desktops>
    <number>4</number>
    <firstdesk>1</firstdesk>
    <names>
      <name>α</name>
      <name>β</name>
      <name>γ</name>
      <name>δ</name>
    </names>
    <popupTime>875</popupTime>
  </desktops>
{% endhighlight %}

Now you can see the result in <code>tint2</code> as below.

![openbox Config: desktops tint2][image-ss-config-desktops]{: .img-responsive }

-- -- --

### Gaps

You can set desktop margins.
Note that I put 40 on top,
because I have my tint2 panel.
You might want different setting.

{% highlight xml %}
  <margins>
    <top>40</top>
    <bottom>10</bottom>
    <left>10</left>
    <right>10</right>
  </margins>
{% endhighlight %}

![openbox Config: desktops margin][image-ss-config-margin]{: .img-responsive }

-- -- --

### Startup

There is this <code class="code-file">~/.config/openbox/autostart</code> file,
that manage the startup.

I change my autostart from time to time.
It all depen on your need, mood, and the weather on your city.

#### Current autostart

{% highlight conf %}
# -- non windowed app --

nitrogen --restore &
nm-applet &

compton &
# dunst &
parcellite &
mpd &

thunar --daemon &

xautolock -locker i3lock -time 7 &

# -- panel --

tint2 -c ~/.config/openbox/tint2/tint2rc-top &
tint2 -c ~/.config/openbox/tint2/tint2rc-tutor-05-solid &

# -- windowed app --

geany &
thunar &
urxvt &
urxvt &
firefox &
{% endhighlight %}

This non **windowed-app**, will be discussed later in rules section,
in the next article.

#### Old autostart

My old startup file contain these lines.

{% highlight conf %}
### panel
tint2 &

## Menu Transparency
compton -m 0.9 -e 0.7 &

## Volume control for systray
(sleep 2s && pnmixer) &

## Volume keys daemon
xfce4-volumed &

## Enable power management
xfce4-power-manager &

## Start Thunar Daemon
thunar --daemon &

## Start xscreensaver
xscreensaver -no-splash &

## Start Clipboard manager
(sleep 3s && clipit) &
{% endhighlight %}

#### Source

*	[gitlab.com/.../dotfiles/.../autostart][dotfiles-autostart]


-- -- --

### What's Next

Actually there is not many to say here,
because I splitted the article to: key/mouse binding, rules, and menu.

Consider continue reading [ [Config: Key and Mouse Binding][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]: {{ dotfiles }}/rc.xml
[dotfiles-autostart]: {{ dotfiles }}/autostart

[local-part-config]:  /desktop/2018/05/04/openbox-config.html

[image-ss-obconf]:          {{ asset_path }}/openbox-obconf.png
[image-ss-config-desktops]: {{ asset_path }}/openbox-config-desktops-tint2.png
[image-ss-config-margin]:   {{ asset_path }}/openbox-config-margin.png
