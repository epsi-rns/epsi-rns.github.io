---
layout     : post
title      : "Openbox Config - Overview"
categories : desktop
date       : 2018-05-01 09:25:15 +0700
tags       : [openbox]
keywords   : [tutorial, configuration]
author     : epsi
toc        : toc/2018/05/toc-openbox-config.html

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Overview about Openbox in configuration.
---

<a name="preface"></a>

### Overview

> Goal: Openbox Configuration Overview

#### Table of Content

This guidance structured into separated articles:

*	Overview (this article)

*	Config: rc.xml (general, binding, rules)

*	Config: menu.xml (static and sub menus, dynamic, obmenu-generator)

*	Tint2: Taskbar, Background, Panel Items

We have already discuss about theme last months.

After a few discussion, I decide to put a section, about installation.
This is not, a part of config. But if you wish, here it is.

*	Install: Solving difficulties about installation [Debian, Fedora, openSUSE, Gentoo]

#### Window Manager Features

As most of any other Window Manager, Openbox has these features:

*	Themes (window decoration)

*	Menu

*	Startup

*	Config: Key Binding

-- -- --

### Additional Resources

#### Reading

There are some good resources.
I always start with the official one.
I found that the holy archwiki is a also good start.


*	[http://openbox.org/wiki/Help:Contents](http://openbox.org/wiki/Help:Contents)

*	[archlinux.org/index.php/openbox](https://wiki.archlinux.org/index.php/openbox)

#### Dotfiles Document

Config is available at:

* [github.com/epsi-rns/dotfiles/.../openbox/config][dotfiles-tutor]

#### Ricing

These are some good openbox galleries on deviantart:

*	[nixiepro.deviantart.com](https://nixiepro.deviantart.com/gallery/46455703/Screenshots)

*	[fikriomar16.deviantart.com](https://fikriomar16.deviantart.com/art/Break-The-Glass-747098702)

*	[addy-dclxvi.deviantart.com](https://addy-dclxvi.deviantart.com/art/Openbox-Alchemy-733909333)

I can't tell you how I adore these galleries.

-- -- --

### Stacking Window Manager

> Update 2020

![Illustration: Why Stacking Window Manager?][illustration-custom-stacking]

-- -- --

<a name="whats-next"></a>

### What's Next

Consider continue reading [ [Config: General rc.xml][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-tutor]:  {{ dotfiles }}

[local-part-config]:  /desktop/2018/05/03/openbox-config.html

[illustration-custom-stacking]: {{ site.url }}/assets/posts/desktop/2020/customization-stacking.png
