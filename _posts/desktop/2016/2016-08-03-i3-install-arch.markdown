---
layout: post
title:  "Install i3WM in Arch Linux based Distribution"
categories: desktop
date:   2016-08-03 19:25:15 +0700
tags: [i3wm]
author: epsi

excerpt:
  i3 is also an easy WM for beginner.
  It doesn't have need complex configuration.
  i3 is actually a very comfortable Tiling Window Manager,
  as long as you read the fine manual.

related_link_ids:
  - 16080119  # Modularized i3status Conky Lua JSON
  - 14120646  # i3 Window manager

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will move this page to Tutorial section later.
</div>

i3 is also an easy WM for beginner.
It doesn't have need complex configuration.
You can just install it, and use it, the tiling rule is simple.
Except that it start with intimidating black screen,
so you have to read the manual to learn the keystrokes.
But once you get along with i3,
i3 is actually a very comfortable Tiling Window Manager.

**Reading**:

Guidance in The Fine Manual is enough.

* User Guide: <https://i3wm.org/docs/userguide.html>

[![i3 Install Summary Screenshot][image-ss-i3]{: .img-responsive }][picasa-ss-i3]

-- -- --

## Install i3 WM

Installing i3 will also intsall three packages,
it is i3-wm, i3lock and i3status.
You can issue either one of this.

{% highlight bash %}
$ sudo pacman -S i3
$ sudo pacman -S i3-wm i3lock i3status 
{% endhighlight %}
 
[![Install i3 (i3-wm i3lock i3status )][image-i3-i3wm]{: .img-responsive }][picasa-i3-i3wm]

You can see how lightweight Awesome WM from figure above.
The installation footprint of both Awesome and Lua are less than five megabytes.
What a perfect combination between Window Manager and Scripting Language.

-- -- --

## Running i3 WM

To switch to i3 WM after installation completed,
you can logout your current DE/WM (Desktop Environment or Window Manager).
In your DM (Display Manager), login with i3 WM Session.

If you need to make sure, that i3 is in you DM list.
You can check xsession directory.

{% highlight bash %}
$ ls /usr/share/xsessions/
awesome.desktop        gnome.desktop  plasma.desktop
enlightenment.desktop  hidden         xfce.desktop
gnome-classic.desktop  i3.desktop     xmonad.desktop
{% endhighlight %}

On first run, i3-config-wizard will create ~/.config/i3/config.

-- -- --

## Install i3 Blocks

There is an i3blocks tool to help you getting pretty i3statusbar.
It is a useful additional package.

i3-gaps is available in AUR (Arch User Repository).
In order to install i3, you must use AUR Helper,
e.g. yaourt, aura, packer, pacaur, or else.

{% highlight bash %}
$ pacaur -y i3blocks
{% endhighlight %}

[![Install AUR i3-blocks][image-i3-i3blocks]{: .img-responsive }][picasa-i3-i3blocks]

-- -- --

## Install i3 Gaps

There is an Unofficial Fork of i3 from AirBlader.
The most interesting feature is Gaps between window.

i3-gaps is available in AUR (Arch User Repository).
In order to install i3, you must use AUR Helper,
e.g. yaourt, aura, packer, pacaur, or else.

Since it is an i3 fork, you can't have both installed.
This command will install i3-gaps and uninstall i3-wm.

{% highlight bash %}
$ pacaur -y i3-gaps
{% endhighlight %}

[![Install AUR i3-gaps][image-i3-i3gaps]{: .img-responsive }][picasa-i3-i3gaps]

To go back from i3-gaps, install i3-wm.

{% highlight bash %}
$ sudo pacman -S i3-wm 
{% endhighlight %}

-- -- --

## Sample Configuration

If the default config is not suitable enough,
you can customize the config for your needs.
I prefer to put my i3status and i3blocks
inside my i3 config directory,
and create my custom config later from copy of the default one.

{% highlight bash %}
$ cat ~/.config/i3/config

$ mkdir ~/.config/i3/i3status
$ cp /etc/i3status.conf ~/.config/i3/i3status/default.conf

$ mkdir ~/.config/i3/i3blocks
$ cp /etc/i3blocks.conf ~/.config/i3/i3blocks/default.conf
{% endhighlight %}

-- -- --

## Modularized i3status Configuration

Although i3 config is very simple,
however i3status using conky-lua-json is complex.

My configuration dotfiles is here.
You can copy for your own needs.

* <https://github.com/epsi-rns/dotfiles/tree/master/i3>

Since this configuration is already modularized,
it is easier to learn part by part.

-- -- --

## Additional Package

For your convenience,
you can choose to use either dmenu or rofi.
Both provide the same purpose to run command,
except rofi has more feature.

{% highlight bash %}
$ sudo pacman -S dmenu
$ sudo pacman -S rofi
{% endhighlight %}

[![i3-gaps: Conky Lua in dark i3status][image-ss-i3gaps-dark]{: .img-responsive }][picasa-ss-i3gaps-dark]

-- -- --

## Source Code

Window Manager

* <https://github.com/i3/i3>

* <https://github.com/Airblader/i3>

Statusbar

* <https://github.com/i3/i3status>

* <https://github.com/vivien/i3blocks>

Jekyll Site Source.

* <https://github.com/i3/i3.github.io>


-- -- --

Thank you for reading and visiting.


[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-i3]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-summary.png
[picasa-ss-i3]: https://lh3.googleusercontent.com/-1rU0U1ffYO4/V6HYA15Q5TI/AAAAAAAAAuI/a14r84TgDaguRESpK5eWVULRhWWPGTL5QCCo/s0/i3-arch-install-fullscreen.png

[image-i3-i3wm]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-i3wm-half.png
[picasa-i3-i3wm]: https://lh3.googleusercontent.com/-xmEJ0jNXmso/V6HYBFt-InI/AAAAAAAAAuI/yQtOpXbqBV4Z_8CwzyFappNPWQ6ZhanuQCCo/s0/i3-arch-install-i3wm-full.png

[image-i3-i3blocks]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-i3blocks-half.png
[picasa-i3-i3blocks]: https://lh3.googleusercontent.com/-3f4WBLUMM5g/V6HYA96BUzI/AAAAAAAAAuI/0S7E3Lu3c7MN6Ho0Naa9Pik8D3fvQ6_UwCCo/s0/i3-arch-install-i3blocks-full.png

[image-i3-i3gaps]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-i3gaps-half.png
[picasa-i3-i3gaps]: https://lh3.googleusercontent.com/-HmYhhfjP1dY/V6HYAYhygJI/AAAAAAAAAuI/EpohYgBEk2YzpkgfFZLGKtaWooIqhYOQQCCo/s0/i3-arch-install-i3gaps-full.png

[image-ss-i3gaps-dark]: {{ site.url }}/assets/posts/desktop/2016/08/i3gaps-dark.png
[picasa-ss-i3gaps-dark]: https://lh3.googleusercontent.com/-z2h94mqwszU/V59JN7KCTyI/AAAAAAAAAsI/Dj76UEcWbnkhowZUobnrj8uwC6aA-VcuwCCo/s0/i3gaps-dark.png
