---
layout: post-sidemenu-wm
title:  "Standalone Dzen2 Statusbar Using BASH"
categories: desktop
date:   2017-04-01 01:35:15 +0700
tags: [ricing]
author: epsi

excerpt:
  Dzen2 is easy when you have guidance.

---

### Preface

It was first April 2017 when I configure my dzen setup,
and I'll speak the truth about dzen2.
Dzen is actually simple, the issue is, there was nobody to help me.
I could not find any good tutorial, sufficient to ease my troubled mind.
There was nothing I could do, but read as many dotshare sample as I could.

I can give you an illustration, a story you can imagine

	"It was a frozen winter in cold war era.
	We are two men, a boy, two women, a husky, and two shotguns.
	After three weeks, we finally configure dzen2.
	But we lost our beloved husky before we finally made it.
	Now, every january, we remember our husky,
	that helped all of us to survive."

Now that the hard day was over,
this tutorial is for you.
Dzen2 is easy when you have guidance.

-- -- --

### Why Learn Dzen2 ?

As a complement of Tiling Window Manager. 
People use standalone panel (or statusbar),
e.g. Dzen2, Lemonbar, and stuff.

Let us see this preview image to see what can be achieved by Dzen2.

![Dzen2 Preview][image-01-preview]{: .img-responsive }

The thing is, Tiling Window Manager itself is also not complicated.
Once you get the idea of one Tiling Window Manager,
it is going to be easy to switch beetwen any of them.
Just learn some keystrokes, and everything is functional.
And later you can go deep into configuration if you want.

Further than this, especially for ricing,
the hardest part is to make a beautiful panel/ statusbar setup.
Most of the time, the panel part, is the longest configuration part.

There are a few consideration in building an acceptable panel.

1.	Make the panel show the shape, as you wish.

2.	Make the panel, in a structured script, flexible enough to be themed.

3.	Make the panel, not resource hungry. Not hogging the CPU proccess.

Here we are going to cover the first and second part using BASH.

With understanding of dzen2 statusbar,
learning Tiling Window Manager, become easier.
You can focus on the Window Manager instead of statusbar stuff.

-- -- --

### Hello World in Command Line

In order to keep dzen2 running,
we need to echo output in endless loop, and pipe it to dzen2.
Otherwise, dzen2 will dissappear, as the script stop.

This command will show you 'Hello World' at the top of your desktop.

{% highlight bash %}
$ while sleep 1; do echo "Hello World"; done | dzen2 -w 640
{% endhighlight %}

![Dzen2 Command Line Example][image-02-example-00]{: .img-responsive }


Let's do it for a more dynamic input using date that changed every second.
Dzen will be shown at bottom right corner.

{% highlight bash %}
$ while sleep 1; do date +'%a %b %d %H:%M:%S'; done | \
  dzen2 -ta r -h 25 -y -30 -w 200 -x -200
{% endhighlight %}

**Reading**:<br/>

Further information about dzen parameters,
you can read in manual page in github.
I mean not in <code>man dzen2</code>, from the command line.

*	<https://github.com/robm/dzen>

*	<https://wiki.archlinux.org/index.php/Dzen>

-- -- --

### Simple dzen2 in Script

Let's put this dzen in a script <code class="code-file">example-01.sh</code>.

**Source**:<br/>
*	[github.com/.../example-01.sh][dotfiles-example-01]

{% highlight bash %}
#!/usr/bin/env bash

generated_output() {
    # endless loop
    while true; do 
      date +'%a %b %d %H:%M:%S'
      sleep 1
    done
}

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

generated_output | dzen2 $parameters 
{% endhighlight %}

And examine the output. 

![Dzen2 BASH Script Example][image-02-example-01]{: .img-responsive }

### Coloring dzen2

Again, let's give some color in generated output,
using <code>^bg()</code> and <code>^fg()</code>.
And let's also refactor between the panel and output,
<code class="code-file">example-02-main.sh</code> and
<code class="code-file">example-02-output.sh</code>.

**Source**:<br/>
*	[github.com/.../example-02-main.sh][dotfiles-example-02-main]

{% highlight bash %}
#!/usr/bin/env bash

# include
. ~/Documents/standalone/dzen2/bash/example-02-output.sh

# dzen2

xpos=0
ypos=0
width=640
height=24
fgcolor=$colBlack
bgcolor=$colWhite
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

parameters="  -x $xpos -y $ypos -w $width -h $height" 
parameters+=" -fn $font"
parameters+=" -ta c -bg $bgcolor -fg $fgcolor"
parameters+=" -title-name dzentop"

generated_output | dzen2 $parameters &

sleep 1 && exec `(transset-df .8 -n dzentop >/dev/null 2>&1 &)` &
{% endhighlight %}

**Source**:<br/>
*	[github.com/.../example-02-output.sh][dotfiles-example-02-output]

{% highlight bash %}
#!/usr/bin/env bash

# color, 
colWhite='#ffffff'
colBlack='#000000'

# also using google material
colRed500='#f44336'
colYellow500='#ffeb3b'
colBlue500='#2196f3'

generated_output() {
    # endless loop
    while :; do      
        local date=$(date +'%a %b %d')
        local time=$(date +'%H:%M:%S')

        local text=""
        text+="^bg($colBlue500)^fg($colYellow500)  $date  "
        text+="^bg()^fg()  "
        text+="^bg($colRed500)  $time  ^bg()"
      
        echo $text 
      
      sleep 1
    done
}
{% endhighlight %}


And again examine the output, with color and transparency.

![Dzen2 Coloring Example][image-02-example-02]{: .img-responsive }

-- -- --

### Font in dzen2

Font <code>^fn()</code> can be used to show

*	Unicode Character, e.g. Japanese Number

*	Eye Candy Icons

*	Powerline Style Arrow


Here is a working example:
<code class="code-file">example-03-main.sh</code> and
<code class="code-file">example-03-output.sh</code>.


**Source**:<br/>

*	[github.com/.../example-03-main.sh][dotfiles-example-03-main]

*	[github.com/.../example-03-output.sh][dotfiles-example-03-output]


{% highlight bash %}
#!/usr/bin/env bash

# color, 
colWhite='#ffffff'
colBlack='#000000'

# also using google material
colRed500='#f44336'
colYellow500='#ffeb3b'
colBlue500='#2196f3'
colGrey500='#9e9e9e'

# http://fontawesome.io/
FontAwesome="^fn(FontAwesome-9)"

# icon 
preIcon="^fg($colYellow500)$FontAwesome"
postIcon="^fn()^fg()"

# Powerline Symbol
arrow="^fn(powerlinesymbols-14)^fn()"


generated_output() {
    local iconDate="$preIcon$postIcon"
    local iconTime="$preIcon$postIcon"

    # endless loop
    while :; do   
        local date=$(date +'%a %b %d')
        local time=$(date +'%H:%M:%S')
        
        local text=""
        text+="^bg($colBlue500)^fg($colWhite)$arrow "
        text+="^bg($colBlue500) $iconDate ^fg()  $date  "
        text+="^bg($colWhite)^fg($colBlue500)$arrow "
        text+="^bg()^fg()  "
        text+="^bg($colRed500)^fg($colWhite)$arrow "
        text+="^bg($colRed500) $iconTime ^fg() $time  ^bg()"
        text+="^bg($colWhite)^fg($colRed500)$arrow "
             
        echo -n $text 
        echo
      
      sleep 1
    done
}

{% endhighlight %}

![Dzen2 Font and Arrow Example][image-02-example-03]{: .img-responsive }

-- -- --

### Graphic Decoration in dzen2

Icon <code>^i()</code> can be used to show

*	Graphic Decoration, more than Powerline Style Arrow

*	Eye Candy Glyph Icons

Here is a working example:
<code class="code-file">example-04-main.sh</code> and
<code class="code-file">example-04-output.sh</code>.


**Source**:<br/>

*	[github.com/.../example-04-main.sh][dotfiles-example-04-main]

*	[github.com/.../example-04-output.sh][dotfiles-example-04-output]

Dzen2 can read <code class="code-file">.xbm</code> image format.
For your convenience, I have made some eight glyph icons.
For each has height of 24px, the same height as dzen panel.

**XBM**:<br/>

*	[github.com/.../xbm/...][dotfiles-xbm]

*	[github.com/.../xbm-source/...][dotfiles-xbm-source]


{% highlight bash %}
#!/usr/bin/env bash

# color, 
colWhite='#ffffff'
colBlack='#000000'

# also using google material
colRed500='#f44336'
colYellow500='#ffeb3b'
colBlue500='#2196f3'
colGrey500='#9e9e9e'

# http://fontawesome.io/
FontAwesome="^fn(FontAwesome-9)"

# icon 
preIcon="^fg($colYellow500)$FontAwesome"
postIcon="^fn()^fg()"

# Glyph Icon Decoration
decopath="Documents/standalone/dzen2/assets/xbm"

# diagonal corner
deco_dc_tl="^i($decopath/dc-024-tl.xbm)"
deco_dc_tr="^i($decopath/dc-024-tr.xbm)"
deco_dc_bl="^i($decopath/dc-024-bl.xbm)"
deco_dc_br="^i($decopath/dc-024-br.xbm)"

# single arrow and double arrow
deco_sa_l="^i($decopath/sa-024-l.xbm)"
deco_sa_r="^i($decopath/sa-024-r.xbm)"
deco_da_l="^i($decopath/da-024-l.xbm)"
deco_da_r="^i($decopath/da-024-r.xbm)"

generated_output() {
    local iconDate="$preIcon$postIcon"
    local iconTime="$preIcon$postIcon"

    # endless loop
    while :; do   
        local date=$(date +'%a %b %d')
        local time=$(date +'%H:%M:%S')
        
        local text=""
        text+="^bg($colBlue500)^fg($colWhite)$deco_dc_tl "
        text+="^bg($colBlue500) $iconDate ^fg()  $date  "
        text+="^bg($colWhite)^fg($colBlue500)$deco_da_r"
        text+="^bg()^fg() "
        text+="^bg($colWhite)^fg($colRed500)$deco_da_l"
        text+="^bg($colRed500) $iconTime ^fg() $time  ^bg()"
        text+="^bg($colWhite)^fg($colRed500)$deco_dc_bl "
             
        echo -n $text 
        echo
      
      sleep 1
    done
}
{% endhighlight %}

![Dzen2 Graphics Decoration Icon Example][image-02-example-04]{: .img-responsive }

-- -- --

### Monitoring Panel

You can use Dzen2 as monitoring panel.
Let's make a more complete segment, than just a date.

**Source**:<br/>

*	[github.com/.../main.sh][dotfiles-main]

*	[github.com/.../segments.sh][dotfiles-segments]

-- -- --

### Theming

For convenience you can make your script themable.
I just made a quick and dirty after midnight script, that maybe useful for dzen2 theming.
I do not claim that my script here is a good script, just because it is just works.
But you should get my point, that with a little tweak, theming is possible.
In fact, I desire to find a better script, and I wait for you to write it.

Let's see how the variation goes

![Dzen2 Theme: Dark Colorful ][image-03-theme-dark-colorful]{: .img-responsive }

![Dzen2 Theme: Bright Colorful][image-03-theme-bright-colorful]{: .img-responsive }

![Dzen2 Theme: Bright Arrow][image-03-theme-red-arrow]{: .img-responsive }

![Dzen2 Theme: Dark Arrow][image-03-theme-blue-arrow]{: .img-responsive }

![Dzen2 Theme: Dark Arrow][image-03-theme-mix-arrow]{: .img-responsive }

**Source**:<br/>

*	[github.com/.../themes][dotfiles-themes]

After all, it depends on your imagination.

	"Use the source Luke"

-- -- --

### Combine Dzen with Tiling Window Manager

This is outside of the basic scope of configuring Dzen.
But you can see my unfinished code here for my HerbstluftWM.

**Source**:<br/>

*	[github.com/.../herbstluftwm/panel.sh][dotfiles-hlwm-dzen2]

-- -- --

### Using Dzen2 with Conky Lua

This is a very interesting topic.
Conky is less complicated, and Lua can help you more.
But since we need to leave BASH for Lua.
This deserve an article of its own.

-- -- --

I think that's all.
Thank you for reading.
Sorry for my english.
I know how terrible it is.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/bash' %}
{% assign dotfiles_assets = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/assets' %}

[image-01-preview]: {{ asset_path }}/dzen2-01-preview.png
[image-02-example-00]: {{ asset_path }}/dzen2-02-example-00.png
[image-02-example-01]: {{ asset_path }}/dzen2-02-example-01.png
[image-02-example-02]: {{ asset_path }}/dzen2-02-example-02.png
[image-02-example-03]: {{ asset_path }}/dzen2-02-example-03.png
[image-02-example-04]: {{ asset_path }}/dzen2-02-example-04.png

[image-03-theme-dark-colorful]: {{ asset_path }}/dzen2-03-theme-dark-colorful.png
[image-03-theme-bright-colorful]: {{ asset_path }}/dzen2-03-theme-bright-colorful.png
[image-03-theme-red-arrow]: {{ asset_path }}/dzen2-03-theme-red-arrow.png
[image-03-theme-blue-arrow]: {{ asset_path }}/dzen2-03-theme-blue-arrow.png
[image-03-theme-mix-arrow]: {{ asset_path }}/dzen2-03-theme-mix-arrow.png

[dotfiles-example-01]: {{ dotfiles_path }}/example-01.sh
[dotfiles-example-02-main]:   {{ dotfiles_path }}/example-02-main.sh
[dotfiles-example-02-output]: {{ dotfiles_path }}/example-02-output.sh
[dotfiles-example-03-main]:   {{ dotfiles_path }}/example-03-main.sh
[dotfiles-example-03-output]: {{ dotfiles_path }}/example-03-output.sh
[dotfiles-example-04-main]:   {{ dotfiles_path }}/example-04-main.sh
[dotfiles-example-04-output]: {{ dotfiles_path }}/example-04-output.sh

[dotfiles-xbm]: {{ dotfiles_assets }}/xbm/
[dotfiles-xbm-source]: {{ dotfiles_assets }}/xbm-source/

[dotfiles-main]: {{ dotfiles_path }}/main.sh
[dotfiles-segments]: {{ dotfiles_path }}/segments.sh
[dotfiles-themes]: {{ dotfiles_path }}/themes

[dotfiles-hlwm-dzen2]: https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm/bash/dzen2/panel.sh
