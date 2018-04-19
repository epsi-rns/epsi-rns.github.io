---
layout: post
title:  "Openbox Theme - Common Inkscape Trick"
categories: desktop
date:   2018-04-16 09:25:15 +0700
tags: [openbox, theme, inkscape]
author: epsi

excerpt:
  A brief explanation about Openbox theme.
  Step by step, using Inkscape, one SVG file to create each XBM part.

---

{% include post/2018/04/toc-openbox-theme.html %}

-- -- --
### Preface

> Goal: Reusable SVG, as a base for Openbox theme.

![openbox Theme: matclue window preview][image-ss-window]{: .img-responsive }

I will go straight to Inkscape's SVG,
and discuss about the style configuration later.

	This guidance applied for any linux distribution.

#### Previous Article

Most Inkscape related article has been discussed in previous XFWM4 article.

*	[XFWM4 Theme Overview][local-overview]

-- -- --

### Unified Graphic Material

	This is the Inkscape Part.

#### Inkscape Document

SVG source is available at:

* [github.com/epsi-rns/dotfiles/.../matclue][dotfiles-tutor]

#### Get the SVG

You may make from scratch or get my <code>matclue.svg</code> from my dotfiles repository at github.

#### Slices

Before you begin this the slices that we want to achieve.

![openbox Theme: matclue SVG slices][image-ss-svg]{: .img-responsive }

#### Layers

I utilize some more layers, we are going to build this step by step.

![openbox Theme: matclue SVG layers][image-ss-layers]{: .img-responsive }

#### Button Size

We are using, only [W*H = 10px * 10px] button for this style.
For each I give an imaginary narrow border of 1px wide, in border layer.

-- -- --

### Building the XBM

	This is also the Inkscape Part.

Since we are going to use XBM only.

#### Exporting Button

> Hint: Use Inkscape's Batch Export, to export all shapes at once.

Consider set these layers before export:

*	(You may) Hide: name, border, background

*	Locked: button

*	Unlocked: frame

Now the tricky part, for each border box parts.
We must select the box first in <code>frame</code> layer,
so we can have the export target filename such as <code>shade-pressed.png</code>.

![openbox Theme: matclue SVG export][image-ss-export]{: .img-responsive }

Unhide the <code>frame</code> layer, to select other box.

#### Mogrify

Openbox is using XBM, while Inkscape export to PNG.
Hence, we should convert from PNG to XBM.
We can utilize <code>mogrify</code> to do this task.

{% highlight bash %}
$ mogrify -format xbm *.png
{% endhighlight %}

![openbox Theme: matclue mogrify *.xbm to *.png][image-ss-mogrify]{: .img-responsive }

-- -- --

### Button

	This is still also the Inkscape Part.

#### Specification

*	Size: W*H = 10px * 10px.  Each box button has this size.

*	Colors: Using Black and White only. 

#### Design by Pixel

Due to tiny size of Openbox's button, designed made per pixel.
It is actually easy to do it in inkscape.
Just use create rectangular or box shape,
and then set the width/height size in pixel.

![openbox Theme: matclue inkscape pixel][image-ss-pixel]{: .img-responsive }

The rest for each box is self explanatory.

-- -- --

### What's Next

This is only the Inkscape Part,
Consider continue reading [ [Common Config Trick][local-part-style] ]


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/04' %}

[dotfiles-tutor]:  https://github.com/epsi-rns/dotfiles/tree/master/openbox/themes/matclue

[local-overview]:    /desktop/2018/03/20/xfwm4-theme.html
[local-part-style]:  /desktop/2018/04/17/openbox-theme.html

[image-ss-window]:   {{ asset_path }}/openbox-matclue.png

[image-ss-svg]:      {{ asset_path }}/openbox-matclue-slices.png
[image-ss-layers]:   {{ asset_path }}/openbox-matclue-layers.png
[image-ss-export]:   {{ asset_path }}/openbox-matclue-export.png
[image-ss-mogrify]:  {{ asset_path }}/openbox-matclue-mogrify.png
[image-ss-pixel]:    {{ asset_path }}/openbox-matclue-pixel.png
