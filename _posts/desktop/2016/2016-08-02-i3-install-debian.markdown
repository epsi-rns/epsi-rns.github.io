---
layout: post
title:  "Install i3 WM in Debian"
categories: desktop
date:   2016-08-02 22:24:15 +0700
tags: [i3wm]
author: epsi

excerpt:
  i3 is also an easy WM for beginner.
  It doesn't have need complex configuration.
  Installing official i3wm in Debian is also easy,
  but if you need i3-gaps, you have to git-clone i3-gaps manually.

related_link_ids:
  - 16080325  # Install i3 Arch
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
It is true that i3 start black screen,
it is intimidating that you have to read the manual first,
just to learn the keystrokes.
Once you get along with i3,
you can find that i3 is a very comfortable Tiling Window Manager.

Installing official i3wm in Debian is also easy,
but if you need i3-gaps, you have to git-clone i3-gaps manually.
Don't worry about it, this unofficial fork is optional.

**Reading**:

Guidance in The Fine Manual is enough.

* User Guide: <https://i3wm.org/docs/userguide.html>

[![i3 Install Summary Screenshot][image-ss-i3]{: .img-responsive }][picasa-ss-i3]

-- -- --

## Install i3 WM

Installing <code>i3</code> will also install three packages,
it is <code>i3</code>, <code>i3lock</code> and <code>i3status</code>.
You can issue either one of this.

{% highlight bash %}
$ sudo apt install i3
{% endhighlight %}
 
[![Install i3 (i3 i3lock i3status )][image-i3-i3wm]{: .img-responsive }][picasa-i3-i3wm]

-- -- --

## Running i3 WM

To switch to i3 WM after installation completed,
you can logout your current DE/WM (Desktop Environment or Window Manager).
In your DM (Display Manager), login with i3 WM Session.

If you need to make sure, that i3 is in you DM list.
You can check xsession directory.

{% highlight bash %}
$ ls /usr/share/xsessions/
awesome.desktop                 i3.desktop                plasma.desktop
gnome-classic.desktop           i3-with-shmlog.desktop    twm.desktop
gnome.desktop                   lightdm-xsession.desktop  xfce.desktop
gnome-flashback-xmonad.desktop  openbox.desktop           xmonad.desktop
{% endhighlight %}

On first run, <code>i3-config-wizard</code> will
create <code class="code-file">~/.config/i3/config</code>.

-- -- --

## Install i3 Blocks

There is an i3blocks tool to help you getting pretty i3statusbar.
It is a useful additional package and it is available in Debian repository.

{% highlight bash %}
$ sudo apt install i3blocks
{% endhighlight %}

[![Install i3-blocks][image-i3-i3blocks]{: .img-responsive }][picasa-i3-i3blocks]

-- -- --

## Install i3 Gaps

There is an Unofficial Fork of i3 from AirBlader.
The most interesting feature is Gaps between window.

Unfortunately <code>i3-gaps</code> is not available in official repository.
So you have to compile and install it manually.

Reading

* <https://github.com/Airblader/i3/wiki/Compiling-&-Installing>

### Debian Stretch Dependency

{% highlight bash %}
$ sudo apt install libxcb-keysyms1-dev libpango1.0-dev \
  libxcb-util0-dev xcb libxcb1-dev libxcb-icccm4-dev libyajl-dev \
  libev-dev libxcb-xkb-dev libxcb-cursor-dev libxkbcommon-dev \
  libxcb-xinerama0-dev libxkbcommon-x11-dev \
  libstartup-notification0-dev libxcb-randr0-dev libxcb-xrm-dev
{% endhighlight %}

### Clone form Github

{% highlight bash %}
$ git clone https://www.github.com/Airblader/i3 i3-gaps
$ cd i3-gaps/
$ git checkout gaps && git pull
{% endhighlight %}

### Make and Install

{% highlight bash %}
$ cd i3-gaps/
$ make
$ sudo make install
{% endhighlight %}

[![Install AUR i3-gaps][image-i3-i3gaps]{: .img-responsive }][picasa-i3-i3gaps]

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
$ sudo apt install lua conky
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
you can choose to install either <code>dmenu</code> or <code>rofi</code>.
Both provide the same purpose to run command,
except rofi has more feature.

{% highlight bash %}
$ sudo apt install dmenu
$ sudo apt install rofi
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

[image-ss-i3]: {{ site.url }}/assets/posts/desktop/2016/08/i3-debian-install-summary.png
[picasa-ss-i3]: https://lh3.googleusercontent.com/-w-Lulan3AEI/V6IKgI5K5oI/AAAAAAAAAuo/yKPdrukomZMkZsZOJaVa3vm1734ZPNkDwCCo/s0/i3-debian-install-summary.png

[image-i3-i3wm]: {{ site.url }}/assets/posts/desktop/2016/08/i3-debian-install-i3wm-half.png
[picasa-i3-i3wm]: https://lh3.googleusercontent.com/-YohfQLQgM08/V6IKl7la0LI/AAAAAAAAAuo/p9Y2xdnnbMcFnagKhGZNcCcMgqZZsaglwCCo/s0/i3-debian-install-i3wm-full.png

[image-i3-i3blocks]: {{ site.url }}/assets/posts/desktop/2016/08/i3-debian-install-i3blocks-half.png
[picasa-i3-i3blocks]: https://lh3.googleusercontent.com/-R43C8kWbB88/V6IKlhyrbEI/AAAAAAAAAuo/7qb2HShmaUUvA_6-nZX_ThW2A6-KerN1ACCo/s0/i3-debian-install-i3blocks-full.png

[image-i3-i3gaps]: {{ site.url }}/assets/posts/desktop/2016/08/i3-debian-install-i3gaps-half.png
[picasa-i3-i3gaps]: https://lh3.googleusercontent.com/-7AAudqElnBk/V6IKl3XikzI/AAAAAAAAAuo/_Hbotu_UFkkaP3zOfYTpBb23aK_9yEugACCo/s0/i3-debian-install-i3gaps-full.png

[image-ss-i3gaps-dark]: {{ site.url }}/assets/posts/desktop/2016/08/i3gaps-dark.png
[picasa-ss-i3gaps-dark]: https://lh3.googleusercontent.com/-z2h94mqwszU/V59JN7KCTyI/AAAAAAAAAsI/Dj76UEcWbnkhowZUobnrj8uwC6aA-VcuwCCo/s0/i3gaps-dark.png
