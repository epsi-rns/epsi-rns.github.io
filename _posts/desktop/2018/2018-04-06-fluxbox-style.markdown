---
layout: post
title:  "Fluxbox Theme - Inkscape Part"
categories: desktop
date:   2018-04-06 09:25:15 +0700
tags: [fluxbox, theme, inkscape]
author: epsi

excerpt:
  A brief explanation about Fluxbox theme (or style).
  Step by step, using Inkscape, one SVG file to create each XPM part.

---

### Preface

> Goal: Reusable SVG, as a base for Fluxbox theme.

I will go straight to Inkscape's SVG,
and discuss about the style configuration later.

	This guidance applied for any linux distribution.

#### Previous Article

Most Inkscape related article has been discussed in previous XFWM4 article.

*	[XFWM4 Theme Overview][local-overview]

-- -- --

### Inkscape Document

	This is the Inkscape Part.

#### SVG Source

SVG source is available at:

* [github.com/epsi-rns/dotfiles/.../exilorate][dotfiles-tutor]

#### Get the SVG

You may make from scratch or get my <code>exilorate.svg</code> from my dotfiles repository at github.

#### Slices

Before you begin this the slices that we want to achieve.

![fluxbox Style: exilorate SVG slices][image-ss-svg]{: .img-responsive }

#### Layers

I utilize some more layers, we are going to build this step by step.

![fluxbox Style: exilorate SVG layers][image-ss-layers]{: .img-responsive }

#### Button Size

We are using, only [W*H = 24px * 24px] button for this style.
Some other image might have different width and height.

-- -- --

### Building the XPM

	This is also the Inkscape Part.

Since we are going to use XPM only.

#### Exporting Button

	This is a tricky part.

Consider set these layers before export:

*	Locked: icon, icon border, button, icon shadow, highlight, gradient, border

*	Unlocked: Frame

*	Hide: name, background

Now the tricky part, for each border box parts.
We must select the box first in <code>frame</code> layer,
so we can have the export target filename such as <code>close-active.png</code>.

![fluxbox Style: exilorate SVG export][image-ss-export]{: .img-responsive }

For image with transparency, such as <code>selected</code> menu button,
We have to hide the <code>frame</code> layer, because we want empty space as transparent.
The next thing to do is to click the export button to export PNG.

Unhide the <code>frame</code> layer, to select other box.

#### Mogrify

Fluxbox is using XPM, while Inkscape export to PNG.
Hence, we should convert from PNG to XPM.
We can utilize <code>mogrify</code> to do this task.

{% highlight bash %}
$ mogrify -format xpm *.png
{% endhighlight %}

-- -- --

### Gradient and Highlight

	This is still the Inkscape Part.

#### Layer

I utilize some more layers, such as this two

*	gradient

*	highlight

#### Gradient Node

Remember that, the button has 24px height.

![fluxbox Style: exilorate SVG gradient][image-ss-gradient]{: .img-responsive }

On <code>gradient</code> layer, below the icon layers:
I put a box, 24 px height, filled with gradient with four handles as shown above.

*	Handle 0: rgba = (0, 0, 0, 80)

*	Handle 1: rgba = (0, 0, 0, 48)

*	Handle 2: rgba = (0, 0, 0, 24)

*	Handle 3: rgba = (0, 0, 0, 64)

On <code>highlight</code> layer, over the top border: 8px vertical offset of the previous box
I put a box, 16 px height, filled with gradient with two handles.

*	Handle 0: rgba = (255, 255, 255, 255)

*	Handle 1: rgba = (255, 255, 255, 40)

Neither gradient or highlight on unfocused button. It is just simply flat.

#### Export

Now that we are done, we can export each PNG boxes.

-- -- --

### Button

	This is still also the Inkscape Part.

#### Specification

*	Size: W*H = 24px * 24px.  Each box button has this size.

*	Colors: Using Google Material: Red, Green , Blue, Orange. 
	Each box button has different color.
	Stick button and Stuck button, have the same color.

You may have different, to suit your needs.

#### Layout

We are going to make a uniform shape for each button.

![fluxbox Style: exilorate SVG layout][image-ss-layout]{: .img-responsive }

#### Usability

The issue with minimalist color is that,
it is good in aestethic issue, but lack of functional aspect.
In order to solve this usability issue, I put functional icon on keypressed,
such as the <code>x</code> icon for close.
So that user won't click the wrong button.

#### Details

There are four details, for each use the same spec as these below

*	Focused: 

	*	layer icon: circle of 12 px radius with red300 color

	*	layer icon-border: circle of 16 px radius with white color

	![fluxbox Style: exilorate SVG - focused button][image-ss-active]{: .img-responsive }

* Unfocused:

	*	layer icon: box of 12 px radius with red100 color

	*	layer icon-border: box of 16 px radius with white color

	![fluxbox Style: exilorate SVG - ufocused button][image-ss-inactive]{: .img-responsive }

* Pressed, give an icon to solve usability issue.

	*	layer icon: circle of 12 px radius with white color

	*	layer icon-border: circle of 16 px radius with red300 color

	*	layer button: cut circle of 24 px radius with red700 color

	*	layer icon: <code>x</code> shape with red900 color

	![fluxbox Style: exilorate SVG - pressed button][image-ss-pressed]{: .img-responsive }

-- -- --

### What's Next

This is only the Inkscape Part,
Consider continue reading [ [Style Part][local-part-style] ]



[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/04' %}

[dotfiles-tutor]:  https://github.com/epsi-rns/dotfiles/tree/master/fluxbox/styles/exilorate

[local-overview]:    /desktop/2018/03/20/xfwm4-theme.html
[local-part-style]:  /desktop/2018/04/07/fluxbox-style.html

[image-ss-svg]:      {{ asset_path }}/fluxbox-exilorate-svg.png
[image-ss-layers]:   {{ asset_path }}/fluxbox-exilorate-layers.png
[image-ss-export]:   {{ asset_path }}/fluxbox-exilorate-export.png

[image-ss-gradient]: {{ asset_path }}/fluxbox-exilorate-gradient.png
[image-ss-layout]:   {{ asset_path }}/fluxbox-exilorate-layout.png
[image-ss-active]:   {{ asset_path }}/fluxbox-exilorate-close-active.png
[image-ss-inactive]: {{ asset_path }}/fluxbox-exilorate-close-inactive.png
[image-ss-pressed]:  {{ asset_path }}/fluxbox-exilorate-close-pressed.png
