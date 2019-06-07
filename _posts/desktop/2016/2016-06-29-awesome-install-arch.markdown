---
layout: post-sidemenu-wm
title:  "Install Awesome WM in Arch Linux based Distribution"
categories: desktop
date      : 2016-06-29 23:34:15 +0700
tags      : [awesome, install]
keywords  : [modularized, vicious, lain]
author: epsi

excerpt:
  Awesome is the easiest Tiling Window Manager.
  I recommend Awesome WM for first time Tiling Window Manager.
  It is not intimidating, as it still has built in menu.

related_link_ids: 
  - 16071350  # Preparing Modularized
  - 16070650  # Modularized Structure  
  - 16063052  # Install Awesome Debian
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

Awesome is the easiest Tiling Window Manager.
I recommend Awesome WM for first time Tiling Window Manager.
It is not intimidating, as it still has built in menu.

**Reading**

* New: <http://awesomeWM.github.io>, or: <http://new.awesomewm.org/>

* Current: <https://awesomewm.org/>, or: <https://awesome.naquadah.org/>

* Wiki: <https://awesomewm.org/wiki/Main_Page>

[![Awesome Install Summary Screenshot][image-ss-awesome]{: .img-responsive }][photo-ss-awesome]

However it needs a little knowledge of Lua Programming to configure Awesome.
Learning Lua will will give some insight into what the configuration actually does.
In fact I learnt Lua, by configuring Awesome.

-- -- --

## Install Awesome WM

Installing Awesome is straightforward.
It doesnt have many dependency, except Lua.
Just issue Pacman command, and it will take care the rest.

{% highlight bash %}
$ sudo pacman -S awesome
{% endhighlight %}
 
[![Install Awesome and Lua Dependency][image-awesome-lua]{: .img-responsive }][photo-awesome-lua]

You can see how lightweight Awesome WM from figure above.
The installation footprint of both Awesome and Lua are less than five megabytes.
What a perfect combination between Window Manager and Scripting Language.

-- -- --

## Running Awesome WM

To switch to Awesome WM after installation completed,
you can logout your current DE/WM (Desktop Environment or Window Manager).
In your DM (Display Manager), login with Awesome WM Session.

If you need to make sure, that Awesome is in you DM list.
You can check xsession directory.

{% highlight bash %}
$ ls /usr/share/xsessions/
awesome.desktop        gnome.desktop  plasma.desktop
enlightenment.desktop  hidden         xfce.desktop
gnome-classic.desktop  i3.desktop     xmonad.desktop
{% endhighlight %}

-- -- --

## Additional Packages

There are useful additional packages, 
to make your awesome become more awesome. 

* Lain: <https://awesomewm.org/wiki/Lain>

* Vicious: <https://awesomewm.org/wiki/Vicious>

-- -- --

## Install Vicious

Install Vicious in Arch is also very easy,
as it is in official repository.

{% highlight bash %}
$ sudo pacman -S vicious
{% endhighlight %}

[![Install Awesome Additional Package][image-awesome-vicious]{: .img-responsive }][photo-awesome-vicious]

-- -- --

## Install Lain Module

Lain is a fork of Vain Module.
Both are only available in AUR (Arch User Repository).
In order to install Lain, you must use AUR Helper,
e.g. yaourt, aura, packer, pacaur, or else.

{% highlight bash %}
$ pacaur -y lain-git
{% endhighlight %}

[![Install Awesome Lain Module Using AUR][image-awesome-lain-git]{: .img-responsive }][photo-awesome-lain-git]

-- -- --

## Sample Configuration

Awesome provide sample rc in their installation package.
Just copy them to <code class="code-file">~/.config/awesome/</code>

{% highlight bash %}
$ pacman -Ql awesome | grep rc.lua
awesome /etc/xdg/awesome/rc.lua
{% endhighlight %}

{% highlight bash %}
$ mkdir ~/.config/awesome/
$ cp /etc/xdg/awesome/rc.lua ~/.config/awesome/rc.lua
{% endhighlight %}
 
Now you can login to your Awesome Window Manager

-- -- --

## Modularized Configuration

You can modify your <code class="code-file">rc.lua</code>,
and make your awesome WM more awesome

My configuration dotfiles is here.
You can copy for your own needs.

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome>

Since this configuration is already modularized,
it is easier to learn part by part.

I realized that it needs further refactoring,
but now I don't have much time to do it.

-- -- --

## Source Code

Surprisingly, Awesome WM is still in an active development

* <https://github.com/awesomeWM/awesome>

Another website migrate to github, (but not jekyll)

* <https://github.com/awesomeWM/awesomeWM.github.io>

Even the documentation:

* <http://new.awesomewm.org/doc/api/>

* <http://new.awesomewm.org/apidoc/>

It is a good start to learn Lua programming language.

-- -- --

## What's next ?

There are nice customizations beyond scope of this article.
This site might worth to look at.

* <https://github.com/copycat-killer/awesome-copycats>

-- -- --

Thank you for reading

Good night.

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-awesome]: {{ site.url }}/assets/posts/desktop/2016/06/arch-awesome-screenshot-summary.png
[photo-ss-awesome]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNAAts6ck3-Zsza0A2ha_zN0xtLWqByOdEo5gOg

[image-awesome-lua]: {{ site.url }}/assets/posts/desktop/2016/06/arch-awesome-awesome-lua-half.png
[photo-awesome-lua]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOJod_w5oRsR4ekEUM34oOkcZSK0Q6kG2ASlF5u

[image-awesome-vicious]: {{ site.url }}/assets/posts/desktop/2016/06/arch-awesome-vicious-half.png
[photo-awesome-vicious]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOuxKFNrl357TARUZoV6QIc0guG1xMhiPjBKesV

[image-awesome-lain-git]: {{ site.url }}/assets/posts/desktop/2016/06/arch-awesome-lain-git-half.png
[photo-awesome-lain-git]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNmztpdX8svy21B1fXtPlyhXjEeF05p-oPrMelo
 
