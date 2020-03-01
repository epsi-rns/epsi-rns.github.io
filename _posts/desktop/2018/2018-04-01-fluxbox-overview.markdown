---
layout     : post
title      : "Fluxbox Overview"
categories : desktop
date       : 2018-04-01 09:25:15 +0700
tags       : [fluxbox]
keywords   : [tutorial]
author     : epsi
toc        : toc/2018/04/toc-fluxbox.html 

opengraph:
  image: /assets/site/images/topics/fluxbox.png

excerpt:
  A brief explanation about Fluxbox in general.
---

### Overview

> Goal: Fluxbox Overview in General

#### Table of Content

This guidance structured into separated articles:

*	Overview (this article)

*	Config (two articles): General, and Menu

*	Themes (two articles): Inkscape Part, and Style Part

#### Window Manager Features

As most of any other Window Manager, Fluxbox has these features:

*	Themes (window decoration)

*	Menu (and Window Menu)

*	Startup

*	Key Binding

*	Rule (window setting for each application)

*	Feature: Toolbar (panel), Slit, and Tabs

#### Reading

There are some good resources.
I always start with the official one.
I found that the holy archwiki is a also good start.
But there are also other reading material as well, from my first googling attempt.

*	[http://fluxbox.sourceforge.net/docbook/en/html/](http://fluxbox.sourceforge.net/docbook/en/html/)

*	[archlinux.org/index.php/fluxbox](https://wiki.archlinux.org/index.php/fluxbox)

*	[slackwiki.com/Fluxbox_Newbie](https://www.slackwiki.com/Fluxbox_Newbie)

*	[linuxcritic/.../fluxbox-in-depth](https://linuxcritic.wordpress.com/2009/08/03/fluxbox-in-depth-mad-customization-and-other-tips/)

-- -- --

### Features

As a window manager, Fluxbox has a unique feature.
It is the <code>tabs</code> feature.

This guidance doesn't explain these features below.
I rarely use it.
Ity is just, I always think that this is interesting.

#### Toolbar

Fluxbox has built in panel, fluxbox use toolbar terminology for this panel.
You can select the placement, of toolbar.

*	[fluxbox.sourceforge.net/.../chap-toolbar.html](http://fluxbox.sourceforge.net/docbook/en/html/chap-toolbar.html)

![fluxbox Feature: Toolbar][image-ss-toolbar]{: .img-responsive }

#### Tabs

This is basically just grouping application into a bar.

*	[fluxbox-wiki.org/.../Tabs.html](http://fluxbox-wiki.org/category/howtos/en/Tabs.html)

You can group windows into tabs using <code>ctrl - left_click</code>.

![fluxbox Feature: Tabs in Title Bar][image-ss-tabs-title]{: .img-responsive }

There are also tabs placement.

![fluxbox Feature: Tabs using placement][image-ss-tabs-place]{: .img-responsive }

### Slit

This is the place for dockable application, such as <code>gkrellm</code>.
You can also select the placement.

*	[fluxbox.sourceforge.net/.../chap-slit.html](http://fluxbox.sourceforge.net/docbook/en/html/chap-slit.html)

![fluxbox Feature: Slit: gkrellm -w][image-ss-slit-gkrellm]{: .img-responsive }

-- -- --

### What's Next

This is only an overview.
Consider continue reading [ [Config: General Part][local-part-config] ].



[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}

[dotfiles-tutor]:  https://gitlab.com/epsi-rns/dotfiles/tree/master/fluxbox/config

[local-part-config]:  /desktop/2018/04/03/fluxbox-config.html

[image-ss-tabs-title]:    {{ asset_path }}/fluxbox-tabs-intitlebar.png
[image-ss-tabs-place]:    {{ asset_path }}/fluxbox-tabs-placement.png
[image-ss-slit-gkrellm]:  {{ asset_path }}/fluxbox-slit-gkrellm.png
[image-ss-toolbar]:       {{ asset_path }}/fluxbox-toolbar.png

