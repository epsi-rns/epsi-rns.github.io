---
layout: post
title:  "Inkscape Logo Creation Part Two"
date:   2016-11-11 19:10:15 +0700
categories: design
tags: [inkscape, design]
author: epsi

excerpt:
  Complete Detail Sample of Logo Creation.
  Part Two, of Four Article.

---

## Part Two: Making Shape Example

This is the hardest part of logo creation, define logo that you need,
break down the shape parts in your mind, and decide how to do it properly,
in the right sequence

### Find a Shape to be Our Example

I've been learning some tutorial from youtube,
and I've been a great fans of 'Irfan Prastiyanto'.
He has made some awesome Tutorial,
so we can pick one of his logo shapes.

* <https://www.youtube.com/user/desainew>

* <http://irfanow.blogspot.co.id/>


There are other choices, of course.

* Nick Saporito Channel: <https://www.youtube.com/channel/UCEQXp_fcqwPcqrzNtWJ1w9w>

* Yoga Perdana: <https://dribbble.com/yoga>


My choice would be a modification of Telkom Vision Logo
that is easy enough to implement as an example.
I'm not a pirate of somebody's hardworl so we have to modify the logo.
While this originl logo using five corners,
our new logo is going to use seven corners.

* <https://www.youtube.com/watch?v=YtTz430h0RA>

### Preview

The final version of this article would be similar to this preview:

![Part Two: Final Preview][image-p02-final]{: .img-responsive }

-- -- --

## The Polygon Layer

The earliest step would be copying our last template,
and renaming it from 'logo-template (01).svg'
to 'logo-creation-shape (01).svg' 
After that. Open the new 'logo-creation-shape (01).svg' file.

Let's make a new 'Polygon' layer inside 'Basic Shape' layer. 
And choose that layer as our current working layer.

### Septagonal Shape

Create a transparent circle as a guidance.
We need the circle to transparent 
so we can see the position of each shape,
one on top the other, later.

* Position: 0px horizontal, 0px vertical.

* Size: 1000px width x 1000px height

* Color: Black

* Opacity: 20%

Create a Polygon with 7 corners.
Start click, snapping from center of document (handle guide to origin).
And release at top most of the circle.
The snap will place at top most of the circle.

![Part Two: Polygon][image-p02-polygon]{: .img-responsive }

### Divider Shape

Create a Rectangle, to make a divider line.

* Position: 500px horizontal, 500px vertical.

* Size: 10px width x 500px height

We need to move the rotation center. So prepare snap toolbar to enable Rotation Center.

click the rectangle. Zoom at selection, 
click again so we can see the rotation center (+). 
Move the rectangle rotation center to circle object center.

![Part Two: First Divider Line][image-p02-div-line-01]{: .img-responsive }

We need to enable snap corners to do below operation.

Duplicate this divider line,
click so we can see the rotation tool.
Click the top-left rotation,
and drag to first left corner of the septagon.
Release when you see (x) marking on the corner of the septagon.

![Part Two: Second Divider Line][image-p02-div-line-02]{: .img-responsive }

Continue until the seventh corner, select all seven divider line and group them at once.
Move the divider group rotatin center to center of the circle.

![Part Two: Divider Lines Rotation Center][image-p02-div-lrot-cen]{: .img-responsive }

Mirror the group, by duplicate it first, then click menu (<kbd>Object - Flip Horizontal</kbd>)

## Umbrella Layer

### Inner Shape

Create 'Umbrella' layer inside the 'Content' layer.

Back to 'Polygon' layer, choose three objects,
duplicate, and move it to 'Umbrella' layer.

* Polygon

* Divider Line

* Mirrored Didivder Line

Back to 'Umbrella' layer as current working layer.
Disable visibility of 'Polygon' layer.

In 'Umbrella' layer, choose those two divider line groups, and Ungroup.
Then click menu (<kbd>Path - Union</kbd>).

Select Polygon and Unioned Divider,
then click menu (<kbd>Path - Difference</kbd>).

Now we get the umbrella like shape.

![Part Two: Umbrella][image-p02-umbrella]{: .img-responsive }

### Outer Shape

Let's disable visibility of the 'Umbrella' layer.
Enable visibility of the 'Polygon' layer.
And go back to 'Polygon' layer.

Select The any of Divider Group, Duplicate, and Move it to 'Umbrella' layer.

Let's go back to 'Umbrella' layer.
Disable visibility of the 'Polygon' layer.
And enable visibility of the 'Umbrella' layer.

Select Umbrella shape, and set opacity to 30% or else
to differ it from the divider lines. 
And click menu (<kbd>Path - Break Apart</kbd>).

Create Inner circle.

* Position: 300px horizontal, 300px vertical.

* Size: 400px width x 400px height

Select divider lines group, then click menu (<kbd>Object - Flip Vertical</kbd>)
Intersection of the circle and the divider lines would our new guidance.

![Part Two: Inner Circle][image-p02-inner-circle]{: .img-responsive }

Select one of the umbrella leaf, duplicate, and snap the corner to inner circle intersection.
Then do this for all the other six leaves.

![Part Two: Umbrella Leaf][image-p02-leaf]{: .img-responsive }

For each leaf, choose inner and outer, and click menu (<kbd>Path - Difference</kbd>).

Now you can safely remove the divider line group.

And move the inner circle to 'Polygon' layer.

![Part Two: Star Shape][image-p02-star-circle]{: .img-responsive }

We're almost done with this star shape result.

### Elliptical Star

Select all leaf from star shape,
and click menu (<kbd>Path - Union</kbd>).

Create Ellipse from Circle in the middle of document.
And Rotate 45 degree using Transform Tool.
 
* Position: 50px horizontal, 150px vertical.

* Size: 900px width x 700px height

Select both Star Shape and Ellipse, 
and click menu (<kbd>Path - Union</kbd>).

![Part Two: Star Shape][image-p02-star-ellipse]{: .img-responsive }

-- -- --

### SVG Source

We're done with making the shape,
but there are still more steps to go.

You can review our tutorial in this SVG source.

* [epsi-rns.github.io/.../logo-shape-01.svg.gz][dotfiles-p02]

-- -- --

To be continued.


[//]: <> ( -- -- -- links below -- -- -- )


[image-p02-final]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-final-preview.png
[image-p02-polygon]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-polygon.png
[image-p02-div-line-01]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-divider-line-01.png
[image-p02-div-line-02]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-divider-line-02.png
[image-p02-div-rot-cen]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-divider-rotation-center.png
[image-p02-umbrella]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-umbrella.png
[image-p02-inner-circle]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-inner-circle.png
[image-p02-leaf]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-leaf.png
[image-p02-star-circle]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-star-circle.png
[image-p02-star-ellipse]: {{ site.url }}/assets/posts/design/2016/11/logo-creation-02-star-ellipse.png

[dotfiles-p02]: {{ site.url }}/assets/posts/design/2016/11/logo-shape-01.svg.gz
