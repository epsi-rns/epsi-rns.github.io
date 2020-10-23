---
layout     : post
title      : "Openbox Theme - Overview"
categories : desktop
date       : 2018-04-15 09:25:15 +0700
tags       : [xfwm4, theme, inkscape]
keywords   : [tutorial, free SVG, custom theme]
author     : epsi
toc        : toc/2018/04/toc-openbox-theme.html

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Openbox theme step by step, covering two theme styles.
  Using Inkscape, one SVG file to create each XBM part.
---

### Preface

> Goal: Explaining Openbox Theme Step by Step

Good day ? How are you doing ?
We meet again in Window Manager guidance.
This time, we will cover Openbox.
openbox is a good start before you start to learn other Window Manager.

-- -- --

### Reading

Before you begin.
Reading this, is a must:

*	[wiki/Help:Themes](http://openbox.org/wiki/Help:Themes)

All material in this article based from above link.

### Table of Content

Here, I present an Openbox Tutorial, step by step, for beginners.

*	matclue - common tricks: (SVG Inkscape and Theme Configuration):

	![openbox Theme: matclue - Metallic Material][image-ss-matclue-term]{: .img-responsive }

*	flatypuss - special tricks: (SVG Inkscape and Theme Configuration):

	![openbox Theme: flatypuss - Flat Soft Color][image-ss-flatypuss-term]{: .img-responsive }

#### Unified Graphic Material

> Also: Reusable SVG, as a base for XFWM4 theme.

As usual, we will utilize inkscape SVG as our tools, to create images.
In this case XBM.

![openbox Theme: openbox SVG slices][image-ss-ob-slices]{: .img-responsive }

	This guidance applied for any distribution.

Hint: Use Inkscape's Batch Export, to export all shapes at once.

-- -- --

#### SVG Source

SVG source available at:

* [github.com/epsi-rns/dotfiles/.../themes][dotfiles-tutor]

-- -- --

### Using Inkscape XML

I have made my SVG using Inkscape in my PC.
I also have made the target for each export for my particular home directory.
And of course, you have your own home directory, that is sifferent than mine.

{% highlight xml %}
<svg
   ...
   inkscape:export-filename="/home/epsi/.themes/flatypuss/flatypuss.png"
   inkscape:export-xdpi="90"
   inkscape:export-ydpi="90">
{% endhighlight %}

You can easily open the SVG file using any text editor,
and do such search and replace to suit your own needs.

{% highlight xml %}
<svg
   ...
   inkscape:export-filename="/home/sapi/.themes/flatypuss/flatypuss.png"
   inkscape:export-xdpi="96"
   inkscape:export-ydpi="96">
{% endhighlight %}

![xfwm4 Theme: SVG XML Search and Replace][image-ss-search-replace]{: .img-responsive }

I mostly use text editor to fix dpi issue.

-- -- --

### Button Color

Openbox button use XBM format.
XBM is a binary image. 
Hence we do not need any color. 
Black and White is sufficient.

-- -- --

### Menu Transparency

You can set with compton.

{% highlight xml %}
compton -m 0.8
{% endhighlight %}

-- -- --

### Theme Color

I'm using Google's material color for theming.

* [material.io/guidelines/style/color.html](https://material.io/guidelines/style/color.html)

-- -- --

### Design by Pixel

Due to tiny size of openbox button, designed made per pixel.

-- -- --

### Dotfiles Resources

From dotcats telegram channel and groups. We are mostly cats.

#### Theme

Known dotfiles resources containing XFWM4 theme.

*	Fikri Omar: [fikriomar16/OBTheme-Collections][cat-omar-01]

*	White Cat: [addy-dclxvi/openbox-theme-collections][cat-white]

*	Hard Cat: [epsi-rns/dotfiles/../themes][cat-hard]

*	Black Cat: [noirecat/arc-ob-theme][cat-black]

#### Tools

Known dotfiles resources containing openbox tools.

*	openbox Theme Generator - Create your own Openbox-theme from .Xresources <br/>
	Fikri Omar: [fikriomar16/obtgen][cat-omar-02]

*	Patches for Openbox with Rounded Corners<br/>
	Nanda Vera: [yuune/openbox-patched][cat-vera]

-- -- --

<a name="whats-next"></a>

### What's Next

Tutor One, PNG Solid Border.
Consider start reading [ [Common Inkscape Trick][local-part-svg] ].
Let's get it started.


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets-desktop/2018/04' %}

[dotfiles-tutor]:  https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/themes

[local-part-svg]:  /desktop/2018/04/16/openbox-theme.html

[cat-omar-01]:     https://github.com/fikriomar16/OBTheme-Collections
[cat-omar-02]:     https://github.com/fikriomar16/obtgen

[cat-white]:       https://github.com/addy-dclxvi/openbox-theme-collections
[cat-hard]:        https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/themes
[cat-vera]:        https://github.com/yuune/openbox-patched
[cat-black]:       https://github.com/noirecat/arc-ob-theme

[image-ss-search-replace]: {{ asset_path }}/openbox-svg-search-and-replace.png

[image-ss-matclue-term]:   {{ asset_path }}/openbox-matclue-overview.png
[image-ss-flatypuss-term]: {{ asset_path }}/openbox-flatypuss-overview.png
[image-ss-ob-slices]:      {{ asset_path }}/openbox-slices.png


