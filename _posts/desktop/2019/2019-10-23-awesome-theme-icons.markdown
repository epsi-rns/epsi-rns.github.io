---
layout: post
title:  "Awesome WM - Theme - Statusbar Icons"
categories: desktop
date      : 2019-10-23 09:25:15 +0700
tags      : [awesome]
keywords  : [tiling, window manager, modularized, lua]
author: epsi

opengraph:
  image: /assets/posts/desktop/2019/10/clone-layout.png

excerpt:
  Awesome WM theme step by step.
  Custom Multicolor Layout Icons, Minimalist PNG Button.
  Using Inkscape, one SVG file to create each PNG resource.

related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

{% include post/2019/06/toc-awesome.html %}

-- -- --

### Preface

> Goal: Custom Flat Design Layout Icon, Minimalist PNG Button

Statusbar Icon use different approach.
Instead of making original image, we can use image from Font.
Most ricing use stlarch or siji.
If you are lucky enough you can grab someone's dotfiles,
and copy paste the icon you need. But most of the case,
you lost one or two specific icons that you need.

#### Automation in Converting

Why don't we take other approach.
First by finding the source image first, and then convert later. 
There are two steps in converting.

* The first step is the source font image,
  You have various options, such as FontAwesome or other iconic font.

* The second step is converting from font, such as SVG to PNG.
  We can do it by using inkscape.

* The third step is converting to the right size of PNG, by imagemagick.

* Finally, put it in theme configuration.

The interesting part is, all can be automated.

#### Table of Content

* 1: Prepare

* 2: Getting the Font Source

* 3: Converting Format

* 4: Converting Size

* 5: Configuration

-- -- --

### 1: Prepare

Suppose that you use FontAwesome,
you can prepare the directory first.

{% highlight bash %}
% cd ~/.config/awesome/themes/clone/icons/fontawesome/
{% endhighlight %}

Make this three directory.

{% highlight bash %}
% tree -d
.
├── png20
├── png40
└── svg
{% endhighlight %}

So we these directory:

* svg: The SVG source

* png20: The SVG convert to PNG result for (20px, 20px)

* png40: The convert to the right size (40px, 40px)

You can use diferent size.
This is just an example.

-- -- --

### 2: Getting the Font Source

Assuming that you use FontAwesome,
you can clone first somewhere outside your config directory.

{% highlight bash %}
% cd ~
% git clone https://github.com/FortAwesome/Font-Awesome 
{% endhighlight %}

And then copy-paste all the icon that you need,
into one place, tohesvg directory.

![Awesome WM: Thunar File Manager: AwesomeFont][image-thunar-awesome]{: .img-responsive }

-- -- --

### 3: Converting Format

The next step is to convert to PNG.
This is very easy with command line using inkscape.
For example, using (20px, 20px) size.

{% highlight bash %}
% inkscape -z -e png20/angle-down.png -w 20 -h 20 svg/angle-down.svg
{% endhighlight %}

We can automate this, batch process many images, at once.
Consider name this script `svg2png.sh`.

{% highlight bash %}
#!/usr/bin/env bash

esc=""
boldon="${esc}[1m";
boldoff="${esc}[22m"
redf="${esc}[31m";
bluef="${esc}[34m";
reset="${esc}[0m"

i=0
while read line
do
  line=${line##*/}
  array[ $i ]=${line%.svg}
  (( i++ ))
done < <(ls svg/*.svg)

for i in ${array[@]};
do
  echo "${boldon}${redf}inkscape${boldoff} ${bluef}-z -e png20/$i.png -w 20 -h 20 svg/$i.svg${reset}"
  inkscape -z -e png20/$i.png -w 20 -h 20 svg/$i.svg
done
{% endhighlight %}

![Awesome WM: Bash Script Converting SVG to PNG][image-vim-svg2png]{: .img-responsive }

Now have a look at the result.
We are going to discuss statusbar later, this is just an example.

![Awesome WM: Statusbar with image without margin][image-icons-20x20]{: .img-responsive }

These icons looks ugly because the are stretched all the height of statusbar.
We need margin, this lead to the next step.

-- -- --

### 4: Converting Size

My approach of making margin is, to combine two images.
One transparent image  using (40px, 40px) size at the bottom,
and icon using (20px, 20px) size,
**centering** on top of the transparent image.

The SVG source of transparent is available at the repository,
although you ca do it yourself with inkscape.
Now how about the combining parts ?

We can combine images with GIMP, icon by icon, but this is tedious task.
Luckily, this task is also pretty easy with command line.

{% highlight bash %}
% convert transparent-40x40.png png20/angle-down.png -gravity center -compose over -composite png40/angle-down.png
{% endhighlight %}

We can automate this, batch process many images, at once.
Consider name this script `png2center.sh`.

{% highlight bash %}
#!/usr/bin/env bash

esc=""
boldon="${esc}[1m";
boldoff="${esc}[22m"
redf="${esc}[31m";
bluef="${esc}[34m";
reset="${esc}[0m"

i=0
while read line
do
  line=${line##*/}
  array[ $i ]=${line%.png}
  (( i++ ))
done < <(ls png20/*.png)

for i in ${array[@]};
do
  echo "${boldon}${redf}convert${boldoff} ${bluef}transparent-40x40.png png20/$i.png -gravity center -compose over -composite png40/$i.png${reset}"
  convert transparent-40x40.png png20/$i.png -gravity center -compose over -composite png40/$i.png
done
{% endhighlight %}

![Awesome WM: Bash Script Converting SVG to PNG][image-vim-png2center]{: .img-responsive }

Again, have a look at the result.
It is prettier.

![Awesome WM: Statusbar with image with margin][image-icons-40x40]{: .img-responsive }

#### Source

All source available at:

* [github.com/epsi-rns/dotfiles/.../icons/fontawesome][dotfiles-fontawesome]

-- -- --

### 5: Configuration

We need to map from generated PNG to Awesome predefined variable names,
for later to be used in statusbar.

*	[gitlab.com/.../dotfiles/.../clone/icons.lua][dotfiles-icons]

{% highlight lua %}
local icondir = ""

-- Fort Awesome

icondir = theme_path .. "icons/fontawesome/png40/"

theme.widget_wifi         = icondir .. "wifi"
theme.widget_temp         = icondir .. "thermometer-half"
theme.widget_uptime       = icondir .. "charging-station.png"
theme.widget_cpu          = icondir .. "microchip.png"
theme.widget_fs           = icondir .. "hdd.png"
theme.widget_mem          = icondir .. "memory.png"
theme.widget_note         = icondir .. "music.png"
theme.widget_batt         = icondir .. "battery-full.png"
theme.widget_clock        = icondir .. "clock.png"
theme.widget_vol          = icondir .. "volume-up.png"
theme.widget_weather      = icondir .. "cloud.png"
theme.widget_netdown      = icondir .. "download.png"
theme.widget_netup        = icondir .. "upload.png"

theme.monitor_disk        = icondir .. "hdd.png"
theme.monitor_vol         = icondir .. "volume-up.png"
theme.monitor_vol_low     = icondir .. "volume-down.png"
theme.monitor_vol_no      = icondir .. "volume-off.png"
theme.monitor_vol_mute    = icondir .. "volume-mute..png"

theme.monitor_ac          = icondir .. "charging-station.png"
theme.monitor_bat         = icondir .. "battery-three-quarters.png"
theme.monitor_bat_low     = icondir .. "battery-quarters.png"
theme.monitor_bat_no      = icondir .. "battery-empty.png"
{% endhighlight %}

In statusbar, we call the variable
by using `beautiful` instead of `theme`',
for example.

{% highlight lua %}
-- Textclock
I.clock = wibox.widget.imagebox(beautiful.widget_clock)
{% endhighlight %}

It is all for now.

-- -- --

### Conclusion

> I'll be back!

After this Awesome WM modularization
there will be another article,
about Awesome WM panel customization.

What do you think ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/desktop/2019/10' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/awesome/4.3-statusbar' %}

[local-xfwm4-theme]:    /desktop/2018/03/21/xfwm4-theme.html

[dotfiles-icons]:       {{ dotfiles }}/themes/clone/icons.lua
[dotfiles-fontawesome]: {{ dotfiles }}/themes/clone/icons/fontawesome

[image-vim-svg2png]:    {{ asset_path }}/03-gentoo-awesome-vim-svg2png.png
[image-vim-png2center]: {{ asset_path }}/03-gentoo-awesome-vim-png2center.png
[image-thunar-awesome]: {{ asset_path }}/03-gentoo-awesome-thunar-awesome.png
[image-icons-40x40]:    {{ asset_path }}/03-statusbar-icons-40x40.png
[image-icons-20x20]:    {{ asset_path }}/03-statusbar-icons-20x20.png
