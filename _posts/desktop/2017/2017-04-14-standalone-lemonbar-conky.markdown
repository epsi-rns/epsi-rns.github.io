---
layout: post-sidemenu-wm
title:  "Standalone Lemonbar Using Conky"
categories: desktop
date:   2017-04-14 06:25:15 +0700
tags: [ricing, statusbar, conky]
author: epsi

excerpt:
  Lemonbar is also easy when you have guidance.
  This tutorial guide you to write Lemonbar in Conky.
  As a continuation of the previous dzen2 in conky tutorial.
  
---

### Why Lemonbar Conky Guidance

Because I can. 

Just Kidding. I like to learn. And I also like to share what I learn. 
In Short. I don't even know, why I wrote this guidance, in the first place.
Or maybe I'm just to tied to write any preface as an eyecatching entry point.
I don't know what to write. I'm more in push people to read code than
explaining stuff. So All I've got is this paragraph.

Allright. Let me try.

Lemonbar is great, and the creation
of this cool statusbar can be made easier with Conky.
There is a lot of dotfile out there showing amazing Lemonbar case.
But very few tutorial, so hard for beginner to step in 
to this ricing n00berland.
So yeah. We need a guidance, rather than scattered case.

How about conky? Conky is getting more interesting after
Lua binding. It makes the configuration very flexible, 
because it can be programmed.
With this article, I can say that Conky is proven
to be used as multipurpose system monitor tools,
as long as it takes text feed.
Name it, from conky itself, i3bar, dzen2, lemonbar,
and even command line interface.

**Reading**

*	<https://wiki.archlinux.org/index.php/Lemonbar>

*	<https://github.com/LemonBoy/bar>

*	<https://github.com/brndnmtthws/conky>

-- -- --

### Lemonbar and Dzen2

Lemonbar has a more different compared to Dzen2.

1.	The ability to align: left, center, and right, in the same panel.
	With Dzen2, we have to create three panels,
	or just one panel with dzen-textwidth calculation.

2.	Font support. The original Lemonbar only support XDFL.
	But you can install lemonbar-xft-git, that support XFD.
	
3.	Unlike Dzen2, Lemonbar does not support Image. Nor .xbm or other.
	It is also designed to be lightweight in source code.

4.	Underline Feature: It is an important feature,
	to show tagged desktop very nicely, in Tiling Window Manager.

5.	Native transparency support.

6.	Lemonbar has manual page.


-- -- --

### Using XLFD.

Yesterday, I wrote another article about XLFD font.
I recommend you to read that article before using Lemonbar.

-- -- --

### Simple Piping to Lemonbar 

Our very first script, a very simple example,
<code class="code-file">example/01.sh</code>.
This example describe parameters supported,
related to lemonbar configuration.
It is set from external lemonbar.

{% highlight bash %}
$ ~/Documents/standalone/lemon/example/01.sh
{% endhighlight %}

For more information you can use the manual.

{% highlight bash %}
$ man lemonbar
{% endhighlight %}

**Source**:<br/>

*	[github.com/.../dotfiles/.../01.sh][dotfiles-example-01-bash]

Figure below show a transparent lemonbar on top.
With three different Font, in one panel.

![Lemonbar Example: Introduction][image-example-01]{: .img-responsive }

{% highlight bash %}
#!/usr/bin/env bash

colWhite='ffffff'
colBlack='000000'

generated_output() {
    homeicon_awesome='\uf015' 
    timeicon_siji='\ue018'  

    # endless loop
    while true; do 
      echo -en " $homeicon_awesome "
      echo -en " $timeicon_siji "
      date +'%a %b %d %H:%M:%S'
      sleep 1
    done
}

# settings
position="640x24+0+0"
background="#aa$colWhite"
foreground="#ff$colBlack"

# XLFD
font_fixed='-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'
font_siji='-wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1'
font_awesome='-*-fontawesome-medium-*-*-*-17-*-*-*-*-*-*-*'

generated_output | lemonbar \
    -g $position -u 2 -B $background -F $foreground \
    -f $font_fixed -f $font_awesome -f $font_siji

{% endhighlight %}

Here we are using XFLD font, and unicode number.
This is important part, as you cannot use XFT with original Lemonbar.

{% highlight bash %}
homeicon_awesome='\uf015' 
timeicon_siji='\ue018' 

font_fixed='-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'
font_siji='-wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1'
font_awesome='-*-fontawesome-medium-*-*-*-17-*-*-*-*-*-*-*'
{% endhighlight %}

I also add alpha transparency. Have a look at the source.


-- -- --

### Decorating Lemonbar with BASH

The next step is feature related to text formatting in lemonbar.
It is set internally by lemonbar. I separate main script
<code class="code-file">example/02-main.sh</code> that configure
external parameter. And generated output by loop
<code class="code-file">example/02-output.sh</code> that set 
internal lemonbar formatting. I also put all colors in one place,
<code class="code-file">example/gmc.sh</code>.
GMC stand for Google Material Color.

In this step, I'm using XFT Font.
So we need lemonbar-xft-git, replacing the original lemonbar.

{% highlight bash %}
$ ~/Documents/standalone/lemon/example/02-main.sh
{% endhighlight %}

**Source**:<br/>

*	[github.com/.../dotfiles/.../02-main.sh][dotfiles-example-02-main-bash]

*	[github.com/.../dotfiles/.../02-output.sh][dotfiles-example-02-output-bash]

You can see in figure below, how alignment made in lemonbar.
The home icon is on the right side of the statusbar panel.
It lso has very nice underline.

![Lemonbar Example: Bash][image-example-02]{: .img-responsive }

{% highlight bash %}
#!/usr/bin/env bash

# include
. ~/Documents/standalone/lemon/example/gmc.sh
. ~/Documents/standalone/lemon/example/02-output.sh

# settings
position="640x24+0+0"
background="#aa$colWhite"
foreground="#ff$colBlack"

# XFT
# require lemonbar_xft_git 
font_symbol="PowerlineSymbols-10"
font_awesome="FontAwesome-10"
font_monospace="monospace-10"


generated_output | lemonbar \
    -g $position -u 2 -B $background -F $foreground \
    -f $font_monospace -f $font_awesome -f $font_symbol
{% endhighlight %}

This step, require knowledge, of a few lemonbar tags.

*	Foreground Color: <code>%{F}</code>

*	Background Color: <code>%{B}</code>

*	Underline Color: <code>%{U}</code>

*	Alignment: <code>%{l}</code>, <code>%{c}</code>, 
	and <code>%{r}</code>


{% highlight bash %}
#!/usr/bin/env bash

# Char glyps for powerline fonts
   sep_left=""    # Powerline separator left
  sep_right=""    # Powerline separator right
 sep_l_left=""    # Powerline light separator left
sep_l_right=""    # Powerline light sepatator right

homeicon_awesome=''

generated_output() {
    # endless loop
    while :; do      
        local date=$(date +'%a %b %d')
        local time=$(date +'%H:%M:%S')

        local text=""

        text+="%{r} "
        text+="%{B-}%{F-}%{-u}"
        text+="$homeicon_awesome "
        
        # Lemon Feature
        # Font Test using XFT
        text+="%{c}"
        text+="%{B-}%{F-}%{-u}  "
        text+="%{B#cc$colBlue500}%{U#$colGrey900}%{+u}"
        text+="%{F#$colBlue800} $sep_right"
        text+="%{F#$colGrey900} "
        text+="%{F#$colGrey200} $date  "
        text+="%{B-}%{F-}%{-u}"
        text+="%{B#cc$colGreen500}%{U#$colGrey200}%{+u}"
        text+="%{F#$colGreen200} $sep_right"
        text+="%{F#$colGrey200} "
        text+="%{F#$colGrey900} $time  "
        text+="%{B-}%{F-}%{-u}  "
        
        echo $text 
      
      sleep 1
    done
}
{% endhighlight %}

It is just an example, we need a more clear format,
than typing <code>text+=</code> over and over again.
And Conky is just the right tools comes for us.

-- -- --

### Composing Lemonbar in Conky Format

Just like the previous example, I separate lemonbar parameter
<code class="code-file">example/03-main.sh</code>  and formatting.
This time in Lua. <code class="code-file">example/03-output.lua</code>

{% highlight bash %}
$ ~/Documents/standalone/lemon/example/03-main.sh
{% endhighlight %}

**Source**:<br/>

*	[github.com/.../dotfiles/.../03-main.sh][dotfiles-example-03-main-bash]

*	[github.com/.../dotfiles/.../03-output.sh][dotfiles-example-03-output-lua]

![Lemonbar Example: Conky][image-example-03]{: .img-responsive }

The only different of the main bash script is the Conky loader.

{% highlight bash %}
generated_output() {
    conky -c ~/Documents/standalone/lemon/example/03-output.lua
}
{% endhighlight %}

You can debug your Conky output via command line interface.

{% highlight bash %}
$ conky -c ~/Documents/standalone/lemon/example/03-output.lua
{% endhighlight %}

Conky configuration itself is in fact a Lua script.
It contain two main parts. 
<code>conky.config</code> and <code>conky.text</code>.
The first part, persist accros this tutorial.
But we need more attention to the second part.

{% highlight lua %}
conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

colGrey900  = '\\#212121'
colGrey200  = '\\#eeeeee'

colAlphaBlue500  = '\\#aa2196f3'
colGreen500 = '\\#4caf50'
colRed500   = '\\#f44336'

conky.text = [[\
%{r} \
%{B-}%{F-}%{-u}\
%{F]] .. colRed500 ..[[}\
  \
%{l} \
%{B-}%{F-}%{-u}\
  \
%{F]] .. colRed500 ..[[}\
%{c}\
%{B-}%{F-}%{-u}\
%{F]] .. colGreen500 ..[[}\
%{B]] .. colGreen500 ..[[}%{U]] .. colGrey200 ..[[}%{+u}\
%{F]] .. colGrey200 ..[[}  \
%{B]] .. colAlphaBlue500 .. [[}%{U]] .. colGrey900 ..[[}%{+u}\
%{F]] .. colGreen500 .. [[}\
%{F]] .. colGrey900 ..[[} \
%{F]] .. colGrey200 ..[[} ${time %a %b %d}  \
%{F]] .. colGreen500 ..[[}\
\
%{B-}%{F-}%{-u}\
%{B]] .. colGreen500 ..[[}%{U]] .. colGrey200 ..[[}%{+u}\
%{F]] .. colGrey200 ..[[} \
%{F]] .. colGrey900 ..[[} ${time %H:%M:%S}  \
%{B-}%{F-}%{-u}\
%{F]] .. colGreen500 ..[[}\
\
]]
{% endhighlight %}


-- -- --

### Real Life Lemonbar with Lua in Conky

	"Let's be tidy and get organized."

I have made quick-and dirty-scripts. Just an example.
It is never intended to be a perfect script.
And you can write better than mine later.

Le'ts see the file structure. It contains:

*	Main BASH script, top pipe Conky to Dzen2:
	<code class="code-file">main.sh</code>.

*	A Conky Configuration Resource, as a script entry point:
	<code class="code-file">conky.lua</code>.
	You can freely modify this conky.

*	Two submodule libraries:
	<code class="code-file">helper.lua</code> and
	<code class="code-file">parts.lua</code>.

*	One asset contain Google Material Color for convenience:
	<code class="code-file">gmc.lua</code>.

*	One Themable File:
	<code class="code-file">assembly.lua</code>.
	This assembly combine some segments,
	and put them together into panel.
	You can make your own assembly, and change the looks.

{% highlight bash %}
$ ~/Documents/standalone/lemon/conky/main.sh
{% endhighlight %}


**Source**:<br/>

*	[github.com/.../dotfiles/.../01.sh][dotfiles-example-01-bash]

![Real Life Lemonbar: Conky Lua][image-conky-01]{: .img-responsive }

The entry point conky.lua relatively is simple.
I have put all the stuff in libraries.

{% highlight lua %}
home = os.getenv("HOME")
path = '/Documents/standalone/lemon/conky/'
dofile(home .. path .. 'gmc.lua')
dofile(home .. path .. 'helper.lua')
dofile(home .. path .. 'parts.lua')
dofile(home .. path .. 'assembly.lua')


-- shortcut
local _h = helper
lf = helper.lemonForeground
lb = helper.lemonBackground
la = helper.lemonBackgroundAlpha
lu = helper.lemonUnderline
lr = helper.lemonReset

conky.text = [[\
%{r} \
]] .. lr() .. lf(colRed500) .. [[  \
%{l} \
]] .. lr() .. lf(colRed500) .. [[  \
%{c}\
]] .. lr() .. lf(colGreen300) .. [[\
]] .. enabled
   .. lr() .. lf(colBlue400) .. [[\
]]

{% endhighlight %}

Arrrghh... What is this enabled variable?
Well, this script assembly each segment parts to build a nice statusbar.
Unneeded segments, I put on disabled variable.
This is the best parts about using conky as a lua programming language.
The assemby process looks neat and tidy.

You can change the color and segment without a lot of effort.
Just edit the conky.lua, and save.
The lemonbar statusbar panel will updated automatically

{% highlight lua %}
--[[
disabled = ''
-- .. parts.host      (colBlue400)
-- .. parts.volume    (colBlue300)
-- .. parts.separator (colBlue300, colGreen400, colBlue500)
-- .. parts.battery   (colBlue400)   
-- .. parts.mem       (colBlue400)
-- .. parts.ssid      (colBlue400)
-- .. parts.network   (colBlue400)
-- .. parts.mpd       (colBlue400)
-- .. parts.date      (colBlue400)
-- .. parts.time      (colBlue400)
-- .. parts.machine   (colBlue400)
]]


enabled = ''

   .. parts.uptime    (colGreen400)
   .. parts.separator (colGreen400, colBlue500, colGreen200)
   .. parts.cpu0      (colBlue500)
   .. parts.separator (colBlue500, colGreen500, colBlue300)
   .. parts.cputemp   (colGreen500)
   .. parts.separator (colGreen500, colBlue400, colGreen300)
   .. parts.memory    (colBlue400)
{% endhighlight %}

There is no need to make this script simpler.
No need any optimization.
It is designed to be modified.
Quick and Dirty.

-- -- --

Coding is Fun.
Especially when it comes to lemonbar-conky-lua tiers.

That is all for now.
Thank you for reading.

Have Fun



[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon/conky' %}
{% assign dotfiles_expath = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon/example' %}

[image-example-01]: {{ asset_path }}/lemon-01-example-01.png
[image-example-02]: {{ asset_path }}/lemon-01-example-02.png
[image-example-03]: {{ asset_path }}/lemon-01-example-03.png
[image-conky-01]: {{ asset_path }}/lemon-02-conky.png

[dotfiles-example-01-bash]:        {{ dotfiles_expath }}/01.sh
[dotfiles-example-02-main-bash]:   {{ dotfiles_expath }}/02-main.sh
[dotfiles-example-02-output-bash]: {{ dotfiles_expath }}/02-output.sh
[dotfiles-example-03-main-bash]:   {{ dotfiles_expath }}/03-main.sh
[dotfiles-example-03-output-lua]:  {{ dotfiles_expath }}/03-output.lua
[dotfiles-example-02-gmc]:         {{ dotfiles_expath }}/gmc.sh

[dotfiles-main]:   {{ dotfiles_path }}/main.sh
[dotfiles-conky]:  {{ dotfiles_path }}/conky.lua
[dotfiles-gmc]:    {{ dotfiles_path }}/gmc.lua
[dotfiles-helper]: {{ dotfiles_path }}/helper.lua
[dotfiles-parts]:  {{ dotfiles_path }}/parts.lua
