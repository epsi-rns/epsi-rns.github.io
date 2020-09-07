---
layout     : post
title      : "Standalone Dzen2 Statusbar Using Conky"
categories : desktop
date       : 2017-04-11 00:05:15 +0700
tags       : [ricing, statusbar, conky]
keywords   : [standalone, dzen2, lua]
author     : epsi
toc        : toc/2017/04/toc-standalone.html

opengraph:
  image: /assets/site/images/topics/lua.png

excerpt:
  Dzen2 is easy when you have guidance.
  This tutorial guide you to write Dzen2 in Conky.
  As a continuation of the previous dzen2 in bash tutorial.

---

<a name="preface"></a>

### Preface

> Goal: A guide how to write Dzen2 in Conky.

This is the continuation of dzen2 guidance.
I case you have been missed it.
Last week I have wrote a basic dzen tutorial using bash.
And now we are going to continue with conky. 
It is much easier.

**Article**:

*	[Standalone Dzen2 Statusbar Using BASH][local-dzen2-bash]

**Reading**:<br/>

*	<https://github.com/brndnmtthws/conky>

#### Table of Content

* [Preface](#preface): Table of Content

* 1: [Hello World in Command Line](#hello-world)

* 2: [Simple conky-dzen2 in Script](#conky-dzen2)

* 3: [Decorating dzen2](#decorate-basic)

* 4: [Create Decoration for Your own Suite](#decorate-custom)

* 5: [Dzen2 with Modularized Lua in Conky](#modularized-lua)

* 6: [Assembly the Parts](#assembly-parts)

* 7: [Real Life Dzen2 with Lua in Conky](#real-life)

* [Conclusion](#conclusion)

-- -- --

<a name="hello-world"></a>

### 1: Hello World in Command Line

Conky has evolved. Now conky configuration is a Lua script.
So we are going to talk about Lua programming language here.
And forget about the old conky style. 

**Reading**:<br/>
*	<https://github.com/brndnmtthws/conky/wiki/Convert-to-new-1.10-syntax>

Let's do our very first conky. 

**Source**:<br/>
*	[gitlab.com/.../dotfiles/.../01.lua][dotfiles-example-01-lua]

{% highlight lua %}
conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

conky.text = [[\
${time %a %b %d %H:%M:%S}\
]]
{% endhighlight %}

There are two parts on that script above.
The <code>conky.config</code> and the <code>conky.text</code>.
The config will be persistent during this guidance
since we only need conky capability to dump text.
And we are going to do a lot with this text.

Now let's run this conky.
Conky will dump a formatted time text every one second interval.
on the command line.

{% highlight bash %}
$ conky -c ~/Documents/standalone/dzen2/conky-example/01.lua
{% endhighlight %}
 
And see how the output goes through the nice command line interface.

![Dzen2 Conky CLI Example][image-11-example-00]{: .img-responsive }

It means you can debug your output via command line interface.

-- -- --

<a name="conky-dzen2"></a>

### 2: Simple conky-dzen2 in Script

Let's show a simple dzen2 statusbar by put this conky
in a script <code class="code-file">conky-example/01.sh</code>.

**Source**:<br/>
*	[gitlab.com/.../dotfiles/.../01.sh][dotfiles-example-01-bash]

{% highlight bash %}
#!/usr/bin/env bash

generated_output() {
    DIR=$(dirname "$0")
    conky -c ${DIR}/01.lua
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# parameters

xpos=0
ypos=0
width=640
height=24
fgcolor="#000000"
bgcolor="#ffffff"
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

parameters="  -x $xpos -y $ypos -w $width -h $height" 
parameters+=" -fn $font"
parameters+=" -ta c -bg $bgcolor -fg $fgcolor"
parameters+=" -title-name dzentop"

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
pkill dzen2

# execute dzen
generated_output | dzen2 $parameters &

# optional transparency
sleep 1 && exec `(transset .8 -n dzentop >/dev/null 2>&1 &)` & 
{% endhighlight %}

Each code lines in script should be self explanatory.
If you do not understand what each line does,
you should go back to our previous lesson.
There few things covered:

*	Pipeline in command line interface

*	Dzen2 parameters

*	BASH scripting

*	Optional transparency using either transset or transset-df


Since it require knowledge of dzen2 parameters,
I suggest you this good reading.

*	<https://github.com/robm/dzen>


After all, this is the output. 
You can see the amazingly simple dzen2 statusbar on top of the screen.
With updated time every one second interval.

![Dzen2 Simple Conky Dzen2 Example][image-11-example-01]{: .img-responsive }

-- -- --

<a name="decorate-basic"></a>

### 3: Decorating dzen2

This require knowledge of a few dzen2 tags.

*	Foreground Color: <code>^fg()</code>

*	Background Color: <code>^bg()</code>

*	Font: <code>^fn()</code>
	We are going to use AwesomeFont to show Icons.
	You need to get AwseomeFont installed properly in your system.
	Or maybe or just put proper font in your .font directory.

*	Decoration: <code>^i()</code>
	This tag is used to show Icon from Image Glyph.
	But we are going to use it to make a better Powerline like statusbar.

**Reading**:<br/>
*	<https://github.com/robm/dzen>

Here is a working example:
<code class="code-file">conky-example/02.sh</code> and
<code class="code-file">conky-example/02.lua</code>.

**Source**:<br/>

*	[gitlab.com/.../dotfiles/.../02.sh][dotfiles-example-02-bash]

*	[gitlab.com/.../dotfiles/.../02.lua][dotfiles-example-02-lua]

{% highlight lua %}
-- vim: ts=4 sw=4 noet ai cindent syntax=lua

--[[
Conky, a system monitor, based on torsmo
]]

conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

--[[
Common Variables
]]

-- base color
-- escape hash (#) character in conky with slash (\#)
colWhite = '\\#ffffff'
colBlack = '\\#000000'

-- also using google material
-- https://material.io/guidelines/style/color.html
colRed500    = '\\#f44336'
colYellow500 = '\\#ffeb3b'
colBlue500   = '\\#2196f3'
colGrey500   = '\\#9e9e9e'
colGreen500  = '\\#4caf50'
colPink500   = '\\#e91e63'
colOrange500 = '\\#ff9800'
colIndigo500 = '\\#3f51b5'
colCyan500   = '\\#00bcd4'

-- http://fontawesome.io/
FontAwesome = '^fn(FontAwesome-9)'

-- icon 
preIcon  = '^fg(' .. colYellow500 .. ')' .. FontAwesome
postIcon = '^fn()^fg()'

-- glyph icon decoration
decoPath = 'Documents/standalone/dzen2/assets/xbm'

-- diagonal corner
decoCornerTopLeft     = '^i(' .. decoPath .. '/dc-024-tl.xbm)'
decoCornerTopRight    = '^i(' .. decoPath .. '/dc-024-tr.xbm)'
decoCornerBottomLeft  = '^i(' .. decoPath .. '/dc-024-bl.xbm)'
decoCornerBottomRight = '^i(' .. decoPath .. '/dc-024-br.xbm)'

-- single arrow and double arrow
decoSingleArrowLeft  = '^i(' .. decoPath .. '/sa-024-l.xbm)'
decoSingleArrowRight = '^i(' .. decoPath .. '/sa-024-r.xbm)'
decoDoubleArrowLeft  = '^i(' .. decoPath .. '/da-024-l.xbm)'
decoDoubleArrowRight = '^i(' .. decoPath .. '/da-024-r.xbm)'

--[[
Segment
]]

-- date
dateIcon     = ' ' .. preIcon .. '' .. postIcon .. ' '
datePre      = '^bg(' .. colIndigo500 .. ')'
    .. '^fg(' .. colYellow500 .. ')'
dateCommand  = '^fg(' .. colWhite .. ')${time %a %b %d} '
datePost     = '^bg()^fg()'

dateDecoPre  = '^bg(' .. colIndigo500 .. ')'
    .. '^fg(' .. colWhite .. ')' .. decoCornerTopLeft
dateDecoPost = '^bg(' .. colWhite .. ')'
    .. '^fg(' .. colIndigo500 .. ')' .. decoCornerBottomLeft

dateText     = dateDecoPre .. datePre .. dateIcon 
    .. dateCommand .. datePost .. dateDecoPost

-- time
timeIcon     = ' ' .. preIcon .. '' .. postIcon .. ' '
timePre      = '^bg(' .. colPink500 .. ')'
timeCommand  = '^fg(' .. colWhite .. ')${time %H:%M:%S} '
timePost     = '^bg()'

timeDecoPre  = '^bg(' .. colWhite .. ')'
    .. '^fg(' .. colPink500 .. ')' .. decoDoubleArrowLeft
timeDecoPost = '^bg(' .. colWhite .. ')'
    .. '^fg(' .. colPink500 .. ')' .. decoDoubleArrowRight

timeText     = timeDecoPre .. timePre .. timeIcon 
    .. timeCommand .. timePost .. timeDecoPost

--[[
Execute Conky
]]

conky.text = [[\
]] .. dateText .. [[\
  \
]] .. timeText ..[[\
]]
{% endhighlight %}


Just run it, and now you can see the output.
Not a very pretty decoration. But I guess you get my point.
Dzen output is amazingly very flexible to be configured.

![Dzen2 Decorated Conky Dzen2 Example][image-11-example-02]{: .img-responsive }

-- -- --

<a name="decorate-custom"></a>

### 4: Create Decoration for Your own Suite

Everybody has different requirement, taste and style.
Instead of giving <code class="code-file">.xbm</code> files,
I'd better give the Source Image. And explain the creation process.

![Image Source: Diagonal and Arrow][image-source-shapes]{: .img-responsive }

**Article**:

*	[Create XBM for your Dzen2][local-xbm-dzen2]

The creation process has been explained in previous chapter.
I only use use Inkscape and GIMP with very few steps.
And yeah, very simple process.

**XBM**:<br/>

*	[github.com/.../xbm/...][dotfiles-xbm]

*	[github.com/.../xbm-source/...][dotfiles-xbm-source]

-- -- --

<a name="modularized-lua"></a>

### 5: Dzen2 with Modularized Lua in Conky

> "Let's be tidy and get organized."

We are going to make this Lua script themable
with flexible segment monitoring statusbar.
I have made quick-and dirty-scripts.
It means you are free to accustomize the script
for your quick-and-dirty needs.
It is never intended to be a perfect script.
It just works.

Le'ts see the file structure. It contains:

*	Main BASH script, top pipe Conky to Dzen2:
	<code class="code-file">main.sh</code>.

*	A Conky Configuration Resource, as a script entry point:
	<code class="code-file">conky.lua</code>.
	You can freely modify this conky
	by changing theme (preset and assembly).

*	Two submodule libraries:
	<code class="code-file">helper.lua</code> and
	<code class="code-file">parts.lua</code>.

*	One asset contain Google Material Color for convenience:
	<code class="code-file">gmc.lua</code>.

*	Two Themable Paths:
	<code class="code-file">presets</code> for color and
	<code class="code-file">assemblies</code>.
	You can make your own preset and assembly.

![Lua Script Structure][image-12-example-01]{: .img-responsive }

The entry point conky.lua is very simple.
I have put all the stuff in libraries.

**Source**:<br/>
*	[gitlab.com/.../dotfiles/.../conky.lua][dotfiles-conky]

{% highlight lua %}
conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

presetName  = 'bright-background'
assemblyName = 'back-arrow'

local dirname  = debug.getinfo(1).source:match("@?(.*/)")

dofile(dirname .. 'gmc.lua')
dofile(dirname .. 'presets/' .. presetName .. '.lua')
dofile(dirname .. 'helper.lua')
dofile(dirname .. 'parts.lua')
dofile(dirname .. 'assemblies/' .. assemblyName .. '.lua')

conky.text = [[\
]] .. enabled .. [[\
]]
{% endhighlight %}

The color preset is just simpler.

**Source**:<br/>
*	[gitlab.com/.../dotfiles/.../presets/bright-background.lua][dotfiles-preset]

{% highlight lua %}
colorPreset = {
  icon       = colYellow500,
  label      = colPink700,
  separator  = colBlue900,
  value      = colBlack,
  background = colBlue500
}
{% endhighlight %}

Before we go to far, and got you a headache,
let's see how the dzen2 output goes.

![Dzen2: Modularized Conky Lua][image-13-theme-01]{: .img-responsive }

-- -- --

<a name="assembly-parts"></a>

### 6: Assembly the Parts

Let's assembly each segment parts to build a nice statusbar.
This is the best parts about using conky as a lua programming language.
The assemby process looks neat and tidy.

The image above is simply an output of 
this <code class="code-file">assemblies/separator.lua</code> configuration.

**Source**:<br/>
*	[gitlab.com/.../dotfiles/.../assemblies/separator.lua][dotfiles-assembly-separator]

{% highlight lua %}
enabled = ''
    .. parts.host()
    .. parts.volume()
    .. parts.cpu0()
    .. parts.ssid()
    .. parts.network()
    .. helper.separator()
{% endhighlight %}

You can change the color preset and dzen parameter
to achieve other theme without a lot of effort.
Just edit the conky.lua, and save.
The dzen2 statusbar panel will updated automatically
following the selected theme.

![Lua Script Structure][image-13-theme-02]{: .img-responsive }

Here is another sample to create a bright back arrow without making a lot of effort.
<code class="code-file">assemblies/back-arrow.lua</code> configuration.

**Source**:<br/>
*	[gitlab.com/.../dotfiles/.../assemblies/back-arrow.lua][dotfiles-assembly-back-arrow]

{% highlight lua %}
deco = helper.decoDoubleArrowLeft

enabled = ''
    .. parts.transition(deco, colWhite, colGreen400)
    .. parts.host(colGreen400)
    .. parts.transition(deco, colGreen400, colGreen500)
    .. parts.volume(colGreen500)
    .. parts.transition(deco, colGreen500, colGreen600)
    .. parts.cpu0(colGreen600)
    .. parts.transition(deco, colGreen600, colBlue500)
    .. parts.ssid(colBlue500)
    .. parts.transition(deco, colBlue500, colBlue600)
    .. parts.network(colBlue600)
    .. parts.transition(deco, colBlue600, colWhite)
{% endhighlight %}

![Lua Script Structure][image-13-theme-03]{: .img-responsive }

You can create you own theme based on this.
And for me, there is a lot of things
about dzen2 and conky that I haven't covered yet.
This deserve a specific article focused on each topic.
I simply do not have time to optimize the theme.
It is just a quick and dirty solution.

-- -- --

<a name="real-life"></a>

### 7: Real Life Dzen2 with Lua in Conky

<div class="alert alert-dismissible alert-success">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <span class="fa fa-check pull-right"></span>
  <strong>Update:</strong> Additional guidance.
</div>

Now the Final part. The dzen2 statusbar example for your desktop.

{% highlight bash %}
$ ~/Documents/standalone/dzen2/multi/main.sh
{% endhighlight %}

Dzen2 need six bars, while lemonbar only need two bars.
For code comparation you should see both source code yourself.

**Source**:

*	[gitlab.com/.../dotfiles/...dzen2.../main.sh][dotfiles-multi-dzen2]

*	[gitlab.com/.../dotfiles/...lemon.../main.sh][dotfiles-multi-lemon]

[![Real Life Dzen2: Conky Lua][image-ss-real-dzen2]{: .img-responsive }][photo-ss-real-dzen2]

-- -- --

<a name="conclusion"></a>

### Conclusion

Coding is Fun.
Especially when it comes to dzen2-conky-lua tiers.
Now I have to move on to compose the next tutorial.

That is all for now.
Thank you for reading.

Have Fun

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/conky' %}
{% assign dotfiles_expath = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/conky-example' %}
{% assign dotfiles_assets = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/assets' %}

[image-11-example-00]: {{ asset_path }}/dzen2-conky-11-example-00-cli.png
[image-11-example-01]: {{ asset_path }}/dzen2-conky-11-example-01-bash.png
[image-11-example-02]: {{ asset_path }}/dzen2-conky-11-example-02-deco.png
[image-12-example-01]: {{ asset_path }}/dzen2-conky-12-main-01-tree.png

[image-source-shapes]: {{ asset_path }}/xbm-source-shapes.png

[image-13-theme-01]: {{ asset_path }}/dzen2-conky-13-theming-01-bright-colorful.png
[image-13-theme-02]: {{ asset_path }}/dzen2-conky-13-theming-02-dark-colorful.png
[image-13-theme-03]: {{ asset_path }}/dzen2-conky-13-theming-03-bright-arrow.png

[image-ss-real-dzen2]: {{ asset_path }}/ss-dzen2-real-life.png
[photo-ss-real-dzen2]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPL1k-5g1b9F-RZRl10pXDLzgqhXPJfIO2xvTXA?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[dotfiles-example-01-lua]:        {{ dotfiles_expath }}/01.lua
[dotfiles-example-01-bash]:       {{ dotfiles_expath }}/01.sh
[dotfiles-example-02-lua]:        {{ dotfiles_expath }}/02.lua
[dotfiles-example-02-bash]:       {{ dotfiles_expath }}/02.sh

[dotfiles-xbm]:        {{ dotfiles_assets }}/xbm/
[dotfiles-xbm-source]: {{ dotfiles_assets }}/xbm-source/

[dotfiles-conky]:      {{ dotfiles_path }}/conky.lua
[dotfiles-preset]:     {{ dotfiles_path }}/presets/bright-background.lua
[dotfiles-assembly-separator]:  {{ dotfiles_path }}/assemblies/separator.lua
[dotfiles-assembly-back-arrow]: {{ dotfiles_path }}/assemblies/back-arrow.lua

[dotfiles-multi-dzen2]: https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/multi/
[dotfiles-multi-lemon]: https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon/multi/

[local-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html
[local-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html
