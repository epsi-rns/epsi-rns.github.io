---
layout     : post
title      : "Awesome WM - Overview"
categories : desktop
date       : 2019-06-15 09:25:15 +0700
tags       : [awesome]
keywords   : [tiling, window manager, modularized, lua]
author     : epsi
toc        : toc/2019/06/toc-awesome.html

opengraph:
  image: /assets-desktop/2019/06/01-menu-custom.png

excerpt:
  Awesome WM customization step by step.
  A brief overview.
  
related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome
---

### Preface

> Goal: A brief overview of Awesome WM customization.

Awesome WM is ready to use tiling window manager for beginner.
It has been three years since my last Awesome WM configuration,
now Awesome WM version has turned to 4.3 series.
It is more easier for beginner, than before.
The official site also has huge documentation.

![Awesome WM: GhostBSD][image-ss-stacked-640]{: .img-responsive }

The tiling window manager itself make it easy for user
that need workflow for daily basis.
And also most tiling parts can be automated.
What tiling manager is the best, is vary.
Just choose what suitable for you.

With Lua, Awesome configuration is also highly configurable.
So flexible, that the limit is your creativity.

#### Issue

> Nothing someone say before the word **but** really counts.

Now here comes the issue.
Awesome configuration is long.
Not easy to talk with in forum, without separate the code.

Our challenge before making any customization is,
modularization. So here below are our topics:

* Modularization

* Customization: Functionality

* Customization: Statusbar and Looks

* Theme: Main, title, Layout Icons, Statusbar Icons

* Statusbar: Default, Stacked, Vicious, Lain, Arrow

I think that is all for today.
This article will grow.
so yes, this is not my final thought.

#### Why Tiling Window Manager?

> Update 2020

![Illustration: Why Tiling Window Manager?][illustration-custom-tiling]

#### Why Awesome WM?

> Update 2020

![Illustration: Why Awesome WM?][illustration-custom-awesome]

### Lua in CLI

You might want to know Lua, by examining how the code works in CLI.

* [Lua - Playing with Records][lua-records]

Some code might looks like a mess.
But you'll get the idea quickly, of Lua capability,
and also its limitation.
Then realize of, how amazing Lua language,
as a configuration tools, just like this AwesomeWM.
It has been very interesting to work with Lua.
I'm looking forward to explore more Lua.

-- -- --

### What is Next ?

Consider continue reading [ [Awesome WM - Modularized Structure][local-whats-next] ].
There are, some interesting topic,
about refactoring<code>Awesome WM</code> using <code>Lua</code>.

What do you think ?


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets-desktop/2019/11' %}

[local-whats-next]: /desktop/2019/06/16/awesome-modularized-structure.html

[lua-records]: https://epsi.bitbucket.io/lambda/2020/11/16/playing-with-records-lua-01/

[image-ss-stacked-640]: {{ asset_path }}/04-ghostbsd-awesome-stacked.png

[illustration-custom-tiling]:   {{ site.url }}/assets-desktop/2020/customization-tiling.png
[illustration-custom-awesome]:  {{ site.url }}/assets-desktop/2020/customization-awesome.png
