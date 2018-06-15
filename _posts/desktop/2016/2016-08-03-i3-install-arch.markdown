---
layout: post-sidemenu-wm
title:  "Install i3 WM in Arch Linux based Distribution"
categories: desktop
date:   2016-08-03 19:25:15 +0700
tags: [i3wm, install]
author: epsi

excerpt:
  i3 is also an easy WM for beginner.
  It doesn't have need complex configuration.
  i3 is actually a very comfortable Tiling Window Manager,
  as long as you read the fine manual.

related_link_ids:
  - 16080224  # Install i3 Debian
  - 16080119  # Modularized i3status Conky Lua JSON
  - 14120646  # i3 Window manager

---

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

[![i3 Install Summary Screenshot][image-ss-i3]{: .img-responsive }][photo-ss-i3]

-- -- --

## Install i3 WM

Installing <code>i3</code> will also install three packages,
it is <code>i3-wm</code>, <code>i3lock</code> and <code>i3status</code>.
You can issue either one of this.

{% highlight bash %}
$ sudo pacman -S i3
$ sudo pacman -S i3-wm i3lock i3status 
{% endhighlight %}
 
[![Install i3 (i3-wm i3lock i3status )][image-i3-i3wm]{: .img-responsive }][photo-i3-i3wm]

<code>i3status</code> use <code>confuse</code> library.
It will be installed automatically.

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

On first run, <code>i3-config-wizard</code> will
create <code class="code-file">~/.config/i3/config</code>.

-- -- --

## Install i3 Blocks

There is an i3blocks tool to help you getting pretty i3statusbar.
It is a useful additional package.

<code>i3blocks</code> is available in AUR (Arch User Repository).
In order to install i3, you must use AUR Helper,
e.g. yaourt, aura, packer, pacaur, or else.

{% highlight bash %}
$ pacaur -y i3blocks
{% endhighlight %}

or

{% highlight bash %}
$ yaourt -S i3blocks
{% endhighlight %}

[![Install AUR i3-blocks][image-i3-i3blocks]{: .img-responsive }][photo-i3-i3blocks]

-- -- --

## Install i3 Gaps

There is an Unofficial Fork of i3 from AirBlader.
The most interesting feature is Gaps between window.

<code>i3-gaps</code> is available in AUR (Arch User Repository).
In order to install i3, you must use AUR Helper,
e.g. yaourt, aura, packer, pacaur, or else.

Since it is an i3 fork, you can't have both installed.
This command will install <code>i3-gaps</code>,
and also uninstall <code>i3-wm</code>.

{% highlight bash %}
$ pacaur -y i3-gaps
{% endhighlight %}

or

{% highlight bash %}
$ yaourt -S i3-gaps
{% endhighlight %}

[![Install AUR i3-gaps][image-i3-i3gaps]{: .img-responsive }][photo-i3-i3gaps]

To go back from <code>i3-gaps</code>,
install <code>i3-wm</code>.

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

## i3status with Conky

To enable, of course you have to install <code>conky</code>.
The <code>lua</code> package is needed as a mandatory dependency for conky since v1.10.
Since Conky is optional. It is beyond this scope 
to explain conky installation in detail.

{% highlight bash %}
$ sudo pacman -S lua conky
{% endhighlight %}

-- -- --

## Modularized i3status Configuration

Although i3 config is very simple,
however i3status using conky-lua-json is complex.

My configuration dotfiles is here.
You can copy for your own needs.

* <https://gitlab.com/epsi-rns/dotfiles/tree/master/i3>

Since this configuration is already modularized,
it is easier to learn part by part.

-- -- --

## Additional Package

For your convenience,
you can choose to install either <code>dmenu</code> or <code>rofi</code>.
Both provide the same purpose to run command,
except rofi has more feature.

{% highlight bash %}
$ sudo pacman -S dmenu
$ sudo pacman -S rofi
{% endhighlight %}

[![i3-gaps: Conky Lua in dark i3status][image-ss-i3gaps-dark]{: .img-responsive }][photo-ss-i3gaps-dark]

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
[photo-ss-i3]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipP4x8DBep9URZHKJl7rrpaE9b8GvXXz9_RPH56J

[image-i3-i3wm]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-i3wm-half.png
[photo-i3-i3wm]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMy7qxBD_jR-En1LVmEO2pa_M4K2uNfC-kt5xjv

[image-i3-i3blocks]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-i3blocks-half.png
[photo-i3-i3blocks]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPgQWe8icQgf3XQ7Rrtb56aUPqWyEZ02TSb9u8K

[image-i3-i3gaps]: {{ site.url }}/assets/posts/desktop/2016/08/i3-arch-install-i3gaps-half.png
[photo-i3-i3gaps]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPsf9GBtpYyOd6pRAWaUj0j7x_H1mqCn59_U8SV

[image-ss-i3gaps-dark]: {{ site.url }}/assets/posts/desktop/2016/08/i3gaps-dark.png
[photo-ss-i3gaps-dark]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMXY1ouG45kbjW_OBvrgcDyVecU1Cz55flj0wnr
