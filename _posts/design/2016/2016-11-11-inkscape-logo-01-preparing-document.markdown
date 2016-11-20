---
layout: post
title:  "Inkscape Logo Creation Part One"
date:   2016-11-11 15:00:15 +0700
categories: design
tags: [inkscape, design]
author: epsi

excerpt:
  Complete Detail Sample of Logo Creation.
  First Part, of Three Articles.

---

## Part One: Preparing Document

### Preface

I've been learning to implement some logo tutorials while setting up new company.
For each logo, I inject my own concept until I found one suited for the company.
I did make one nice logo, and for a while I couldn't change my mood to try another tutorial.
It was too much fun, and this articel will show you what I've learnt in detail.

Most of the tutorial comes from a designer, it is easy to spot how the designer works in youtube.
The designer just do what they desire, the ideas flow, but they don't really care about precision.
On the contrary, I need a reusable logo that can be fitted to any document for the long run.
I'm considering company stamp, paper, employee card, websites, and so on.
Just like a CAD document, I need an exact size that can be reproducible.

Beside above consideration, The logo should be simple enough, but also flexible to be decorated.
The color should consist nomore than three color for the new company. The simpler the better.
But for the sake of fun I give you another logo. A colorful one as a sample for public.
And, keep my own humble logo as a private one.

The final version would be as this preview:

![Part One: Final Preview][image-p01-final]{: .img-responsive }

I consider myself as a non-designer person, so forgive me for the lack of art.
And I'm also a beginner in inkscape, I'm still in need to learn some trick, so please accept my apology if I'm doing it wrong.
And sorry for my english.


### Table of Content

There are four parts of this article

* First part: is containing document preparation, also introduction to some dock tools e.g. Layers, Align, Fill and Stroke.

* Second part: is containing one sample of shape creation.

* Third part: is containing coloring and pallete. And also about decoration, e.g. Shadow/ Glow, Glossy Shine, Gradient, Pattern, 3D Effect.

### Sub Topic

In this our very first part, we will create a general document template.
This contain some steps:

* Document Properties Setup

* Background and Layer

* Guidance and Snap

* Document ID.

Let's get started

-- -- --

## Documents

### Document Properties

Open a new inkscape document and for this logo create a box.
A box document with same width and height would looks nice
in your whatsapp, and facebook, so your friend can give opinion easily.
It can be done from menu (<kbd>File - Document Properties</kbd>).
Or just press (<kbd>Shift+Ctrl+D</kbd>).

* Default Units: px

* Units: px

* Width: 1000 px

* Height: 1000 px

* Let the background color be transparent.

![Part One: Document Properties][image-p01-doc-props]{: .img-responsive }

Let's cick menu (<kbd>View - Zoom - Page</kbd>).
Or just press (<kbd>5</kbd>).
You're going to need this keystrokes a lot.

### File Naming

Save this file as you desire. 
Let's say the name would be 'logo-template (01).svg'.
So when we have revision, we could have any numbering e.g. 'logo-template (03b) - with watermark.svg'.

### Layers

Prepare Three Layers.
Open Layers Dock (<kbd>Shift+Ctrl+L</kbd>).

* Content: by renaming the current 'Layer 1'. This is our final working area.

* Background: It contains sub Layer, you can grow later this later.

	* BG White: We will duplicate it later to other layer such as BG Black, or BG Gradient.

* Document ID

* Basic Shape: This is our finding-shape-process working area. It contains sub Layer, you can grow later this later.

	* Guidance

![Part One: Layers][image-p01-layers]{: .img-responsive }

-- -- --

## Background

### Box Background

Let's make 'BG White' our current working space layer.

I always put background, so I won't get PNG with transparency while I'm sending it via WhatsApp.
My favorite is off course White, so let's make box covered all the document area.
An 1000px width x 1000px height rectangle.

For this session I will always use RGBA. 
I know it is very basic, I must assume that the reader of this article,
know all the stuff step by step before we go on to more advance topic.

Open Fill and Stroke Dock by Pressing (<kbd>Shift+Ctrl+F</kbd>).
Fill it with #ffffff, or in RGBA (#255, #255, #255, #255).
Make sure no stroke paint. 
And make sure it has 100% transparency.
Make sure it right top rectangle is sharp right at the corner so you don't get rounded corner.

If you forget to set 'BG White' as a working space layer,
move the rectangle object to 'BG White',
just to be sure it is in the proper layer.

![Part One: Background Rectangle][image-p01-bg-box]{: .img-responsive }


### White Background

I'm not completely honest with you.
When I said White background, it won't be pure white,
but I'd rather have radial greyish white.

So let's change, the fill of the fill color using radial gradient
from center #ffffffff to outer #e0e0e0ff.
And Let's rename the Gradient to 'BG-White'
Remember that the color is really up to your flavor.

And Lock The 'BG White' Layer.

![Part One: Background White][image-p01-bg-white]{: .img-responsive }

### Black Background

I rarely use Black Background actually, so you can skip this step.
Sometimes I just have a need to test the Background against dark background color.

You can have as many background as you want.
The easiest way to do it is to Duplicate 'BG White' Layer.
Let's Duplicate Current Layer, and rename from 'BG White copy' to 'BG Black'.
And temporarily hide the visibility of layer 'BG White' by click the eye icon.

Unlock The 'BG Black' Layer and choose the Rectangle.

Open Fill and Stroke Dock by Pressing (<kbd>Shift+Ctrl+F</kbd>).
Create a Duplicate Gradient from 'BG-White' so our new gradient won't interfere the last one.

Rename it to 'BG-Black' and edit radial gradient
from center #202020ff to outer #000000ff.
Remember that the color is really up to your personal taste.
This is no exact science.

![Part One: Gradient Name][image-p01-grad-name]{: .img-responsive }

Don't forget to Lock and Hide, The 'BG Black' Layer.

### Rainbow Background

Let's Duplicate 'BG White' Layer again.
Rename to 'BG Gradient'. Unlock and set the Visibility.

Just in case you need to test the logo against varying color,
you can create as much gradient as you need,
from simple gradient to complex gradient.

Here is just a sample. I got this pallete from gnuplotting.
Each pallete has nice smooth transition of 9 colors. 
Let's make some gradients and saved it using it's pallete name, e.g. Magma, Viridis.

* http://www.gnuplotting.org/tag/palette/

* https://github.com/Gnuplotting/gnuplot-palettes

![Part One: Gnu Plot Pallete][image-p01-gnuplot]{: .img-responsive }


Apply 'magma' linear gradient to background rectangle
with about 45 degree from left top to right bottom,
using Snap bounding corners.
And if you wish, you can apply one of the many inkscape effect.
In this case, click menu (<kbd>Filters- Bumps - Plaster</kbd>).

![Part One: Background Magma Rainbow][image-p01-bg-rainbow]{: .img-responsive }

### Default Background

Let's go back to White Background.
Set the Visibility of 'BG White' layer.
Hide Visibility of all other background.
We are going to use White Background for most of our logo.

Lock all Background Layers.
We are going to do something else.

-- -- --

## Guidance

This just a sample, of skill you need to have,
rather than document preparation.
No need to attach guide in any layer.
The guidance layer above, only needed in complex shape.
Guide is also depend on the shape we want to make.
Generally it is in center of the page.

Turn on Page Grid, and Guides.
Drag Guide from Left Ruler to 500px horizontal position.
Drag Guide from Top Ruler to 500px vertical position.

![Part One: Guide and Page Grid][image-p01-guide]{: .img-responsive }

You should also set the snaps on snaps toolbar.

-- -- --

## File ID

Let's make 'File ID' our current working space layer.

One of the most common things in design is, unfortunately revision.
And sometimes we have to tell people that our document is not a final version,
thus cannot be published in public.
That's why we need this File ID. 
This File ID can be as  simple as a 'Example' text warning,
or a document number e.g. 'Draft 01'.

### Typography

Let's type text 'Draft-01'.

It is all up to you.
But I like to use 'Capture-It' font.

Rotate it about 45 degrees by Transform tools.

Set Color by #00000080

### Alignment

I'll show you here because we are going to use alignment a lot in the next part.
And I don't want to bloat the complex part with tool introduction.

Choose the text, and then choose Background Box from unlocked 'BG White' Layer.
Click (Center on Vertical Axis). And Click (Center on Horizontal Axis)

![Part One: Text Alignment on Center Page][image-p01-align]{: .img-responsive }

Our example of logo preview with File ID would be like this.

![Part One: File ID Example][image-p01-file-id]{: .img-responsive }

Right now, we are going to hide File ID Layers by turning off its visibility.

-- -- --

### SVG Source

Save the Templates. We are going to reuse it.

You can download this very small size template, if you wish.

* [epsi-rns.github.io/.../logo-template-01.svg.gz][dotfiles-p01]

-- -- --

## Template

If you are sure, you can put your template
in <code class="code-file">~/.config/inkscape/templates/</code> for convenience.
So you can use it later from menu (<kbd>Open - Templates</kbd>)

-- -- --

To be continued.


[//]: <> ( -- -- -- links below -- -- -- )


[image-p01-final]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-final-preview.png
[image-p01-doc-props]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-document-properties.png
[image-p01-layers]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-layers.png
[image-p01-bg-box]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-bg-box.png
[image-p01-bg-white]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-bg-white.png
[image-p01-grad-name]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-gradient-name.png
[image-p01-gnuplot]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-gnuplot.png
[image-p01-bg-rainbow]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-bg-rainbow.png
[image-p01-guide]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-guide.png
[image-p01-align]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-align.png
[image-p01-file-id]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-01-file-id.png

[dotfiles-p01]: {{ site.url }}/assets/posts/design/2016/11/logo-template-01.svg.gz
