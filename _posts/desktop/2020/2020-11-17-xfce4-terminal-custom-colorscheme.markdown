---
layout: post
title:  "XFCE4 Terminal - Custom Colorscheme"
categories: desktop
date      : 2020-11-15 09:25:15 +0700
tags      : [terminal, shell]
keywords  : [xfce4-terminal, colorscheme, bright]
author: epsi

opengraph:
  image: /assets/site/images/topics/bash.png

excerpt:
  Create custom bright colorscheme for XFCE4 Terminal.

related_link_ids:
  - 16032117 # powerline
  - 16032038 # Console Pixel Art
  - 17122635 # Iterate Oh My BASH Theme
  - 17041225 # Standalone CLI System Monitoring Using Conky
  - 16051900 # midnight commander
  - 16091915 # install termite

---

<a name="preface"></a>

### Preface

> Goal: Create custom bright colorscheme for XFCE4 Terminal.

It has been more than two years after my last terminal article.
I have been craved to use bright mode,
and having issue with colorscheme with bright color.
This time I found the solution.

#### Table of Content

* [Preface](#preface): Table of Content

* [Configuration](#configuration)

* [Conclusion](#conclusion)

-- -- --

<a name="configuration"></a>

### Configuration

#### Directory

First of all, where are the configuration files are placed?

* System wide: `/usr/share/xfce4/terminal/colorschemes/`

* Per user: `~/.local/share/xfce4/terminal/colorschemes/`

* Current setting: `~/.config/xfce4/terminal/terminalrc/`

#### Why Bright Colorscheme?

I know some people hate bright colorscheme.
But bright colorscheme might be compelling in a few situation.
For example in my case, I wrote a book and I require a bright colorscheme.
The issue comes when need a screenshot for my free e-book.
This book comes in PDF, and must be allowed to be printed by the reader.
In this context, `dark mode` is not a choice.
I have to use `bright mode`, to reduce ink while printing.

#### Dark Pastel

Consider look at, this `xfce4-terminal` below.
This terminal looks good with `dark-pastel`.

![XFCE4 Terminal - Colorscheme - Dark Pastel][image-dark]

This should be no problem with daily basis.
But how about bright colorscheme?

#### Black on White

The bright mode choice is already exist,
with `black on white` colorscheme in `xfce4-terminal`.

![XFCE4 Terminal - Colorscheme - Black on White][image-bright]

The issue is not all the colors looks visible.
The color is shown in `size` and `build time` in terminal above.

#### Custom Colorscheme

I copy a file from:

* `/usr/share/xfce4/terminal/colorschemes/black-on-white.theme`

Then `copy` to this directory:

* `~/.local/share/xfce4/terminal/colorschemes`

And rename to file to `custom-black-on-white.theme`.

After this, I change the colorscheme name to `Custom Black on White`.

{% highlight bash %}
[Scheme]
Name=Custom Black on White
ColorForeground=#000000
ColorBackground=#ffffff
{% endhighlight %}

Now we have this colorscheme in `xfce4-terminal`'s preset.

![XFCE4 Terminal - Colorscheme - Presets][image-presets]

I will modify the content later.

#### Black on White with Solarized Text

The easier way to solve this is,
using colorscheme from `solarized (light)`.

* `/usr/share/xfce4/terminal/colorschemes/solarized-light.theme`

Now we have configuration as below:

{% highlight bash %}
[Scheme]
Name=Custom Black on White
ColorForeground=#000000
ColorBackground=#ffffff
ColorPalette=#073642;#dc322f;#859900;#b58900;#268bd2;#d33682;#2aa198;#eee8d5;#002b36;#cb4b16;#586e75;#657b83;#839496;#6c71c4;#93a1a1;#fdf6e3
{% endhighlight %}

![XFCE4 Terminal - Colorscheme - Black on White][image-solarized]

#### Black on White with Custom Solarized Text

If you need enough contrast, then you can change the `RGB` color.
Consider, have a look at, the configuration here:

{% highlight bash %}
ColorPalette=...;#839496;#6c71c4;#93a1a1;#fdf6e3
{% endhighlight %}

With just guess (or trial an error),
I find the color responsible fot this is `#93a1a1`.

Consider to change the `#93a1a1` to  `#435151`,
or any color that you want.
So we have this configuration

{% highlight bash %}
ColorPalette=...;#839496;#6c71c4;#435151;#fdf6e3
{% endhighlight %}

Voila. We have the result as below:

![XFCE4 Terminal - Colorscheme - Custom Color][image-custom]

Since this bright terminal has been solved.
Now I can concentrate to my blog material.
I can go back, busy with scribus, writing an ebook.

🙏🏽.

-- -- --

<a name="conclusion"></a>

### Conclusion

That is all.

Thank you for reading and visiting.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = 'https://epsi-rns.github.io/assets-desktop/2020/11' %}

[image-presets]:    {{ asset_path }}/xfce4-terminal-presets.png
[image-dark]:       {{ asset_path }}/xfce4-terminal-dark.png
[image-bright]:     {{ asset_path }}/xfce4-terminal-bright.png
[image-solarized]:  {{ asset_path }}/xfce4-terminal-bright-solarized.png
[image-custom]:     {{ asset_path }}/xfce4-terminal-bright-custom.png
