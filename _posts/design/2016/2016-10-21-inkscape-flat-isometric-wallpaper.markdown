---
layout: post
title:  "Inkscape: Flat Wallpaper with Isometric Looks"
date:   2016-10-03 02:39:15 +0700
categories: design
tags: [inkscape, design]
author: epsi

excerpt:
  Let's make an Inkscape Wallpaper.
  It is actually a 2D Flat Style,
  with help of 3D Isometric Projection.

related_link_ids:
  - 16111957  # Manjaro Logo

---

What!! 3D Isometric in 2D ?
Yeps, it is not a 3D Projection at all,
We just need the Perspective, to create a balance Grid.
So your final result looks neat.

Without Isometric style, all you need is rotating shape and text.
But with Isometric you need to Skew, before Rotate the object.
Skewing has a more realistic looks as its advantage,
as you can see in this preview.

[![Preview Wallpaper][image-preview]{: .img-responsive }][hires-final]

-- -- --

## Template

First create a new document.

### Page

Change the document properties
to match extra large wallpaper screen size:

* Default Units: px

* Width: 1920 px

* Height: 1080 px

![Document Properties: Page][image-dialog-page]{: .img-responsive }

### Grid

Open the document properties, and create new Axonometric Grid.

* Grid Units: px

* Spacing Y: 32

* Angle X: 30

* Angle Z: 30

* Major Grid: 4

Check the menu option (<kbd>View - Page Grid</kbd>) to see the grid in your page.
Note that the Spacing and Major Grid, is flexible, you can change later.
The rule is, just pick what suitable for your project.
Optionally, you can also change the XML and set the grid name as 'Isonometric'.

![Document Properties: Grids][image-dialog-grids]{: .img-responsive }

### Palette

Prepare your Pallete. 
Here we are going to use Google Material Pallete.

![Pallete: Google Material][image-pallete]{: .img-responsive }

### Snaps

Prepare your Snaps. By clicking the representative icons.

* Snap Bounding Boxes.

* Snap Nodes, Path and handles.

### Layer

Create Three new Layers.

* Crop

* Wallpaper

* Background

![Dock: Layers][image-dock-layers]{: .img-responsive }

### Background

Use Background Layer as current working space.
Zoom Page or click (<kbd>5</kbd>), and zoom out few times.
We are going to make an isometric background shape
using Freehand Lines (<kbd>F6</kbd>).

Draw Path by Pressing (<kbd>F6</kbd>), and snap to 'Handle to Grid Intersection',
right in the first major grid from about left-bottom of the page.
The idea is, the background should cover all the page area.

![Snap Handle][image-snap-handle]{: .img-responsive }

Click from node to node until you get your first diamond shape.
Now you are having about 18 box x 19 box tiling.
Fill with any color you desire.

Lock The Layer, after you finished.

### Crop

Use Crop Layer as current working space.
We need to visualize while working,
on how the final result of the wallpaper,
without actual cropping.

Create a rectangular inner box with dimension the same as your page.
Make sure that it has no stroke color.

* X: 0px, Y: 0px, W: 1920px, H: 1080px

![Crop: Inner Box][image-box-inner]{: .img-responsive }

Create a rectangular outer box with dimension the twice as previous inner box.
I'd rather duplicate, and change the dimension.

* X: -960px, Y: -540px, W: 3840px, H: 2160px

Lower Object under the inner box (<kbd>Page Down</kbd>).
Choose both boxes, and click menu (<kbd>Path - Difference</kbd>)
to create one hollow box.

Now that you have a hollow boxes,
change its opacity to 50%.

![Crop: Outer Box][image-box-outer]{: .img-responsive }

And do not forget to lock this layer.

### Save your works

Congratulation now you have a new template.
Let's save this as new document,
named '<code class="code-file">flat-isometric-looks.svg</code>'.

-- -- --

## Basic Shape

Here is our example, inspired from
this Brilliant [Material Design ArtWork][link-inspired-by].
But this time we are going to use isometric looks.

Let's use Wallpaper Layer as current working space.

### First Bar

Draw Path use Freehand Lines by Pressing (<kbd>F6</kbd>),
and create an isometric rectangle using major grid snap,
about 18 box x 2 box, of major tiling.
Make sure there is no stroke color.

Draw the inner Path, about 18 major box x 6 minor box.
No stroke color.

Select both, and group them,
we need the outer box as snap guidance.

![Bar: First][image-bar-first]{: .img-responsive }

### All Bars

Set 'Snap Bounding Box Corner',
and duplicate the first bar to fill all the backgound area.

Select all, and ungroup.
Then delete all bigger bars.

![Bar: All][image-bar-all]{: .img-responsive }

### Cross Bars

Do the same thing, by duplicate and mirror.
I'd rather do it from scratch.

Remove unnecessary bar.
I'd rather move than remove to left or right by precise pixel,
e.g 2000px right or -2000px left, just in case I need it later.

![Bar: Cross][image-bar-cross]{: .img-responsive }

-- -- --

## Coloring

### Background

First thing first, the background, use pure white.

### Bars

The rest I'm going to use Google Material Color.
I like the soft color of this pallete.

Left Bar, use Grey, the inner most is the darkest.
And you can use any color accent as you desire.
1: grey 500,
2: grey 600,
3: grey 700,
4: grey 800,
5: lightblue 900,
6: grey 800,
7: grey 700,
8: grey 600,
9: grey 500,
10: grey 400.

Right Bar. 
1: grey 900,
2: grey 700,
3: grey 900.

![Bar: Color][image-bar-color]{: .img-responsive }

### Shadow

Select all bars, and apply filter by
click menu (<kbd>Filters - Shadows and Glows - Drop Shadow</kbd>)

![Dialog: Shadow][image-dialog-shadow]{: .img-responsive }

* Blur: 10px

* Horizontal Offset: 20px

* Vertical Offset: 20px

* Shadow Type: Outer

* Color: RGBA #00000080

Also change the object arrangement by select object
and press (<kbd>Pg Up</kbd>) or (<kbd>Pg Down</kbd>).

![Bar: Shadow][image-bar-shadow]{: .img-responsive }

-- -- --

## Text

This is a hard part for me,
since I'm not good at poetry.
I have to look on my past,
just to find good words combination.

### Typography

Im using Oswald Font, Bold, size 80px, and grey 50 color.
You can use any font setting as you desire.

### Isometric

The rule is simple.

* Skew: 30 Degree, then Rotate: 30 Degree, in this sequence or.

* Skew: -30 Degree, then Rotate: -30 Degree, in this sequence.

![Dock: Transform][image-dock-transform]{: .img-responsive }

Put the text on the bar using 'Bounding box corner to grid intersection'.

![Text: Snap][image-text-snap]{: .img-responsive }

-- -- --

## Final Result

Export to PNG using 90dpi to achieved 1920px width x 1080 px height.

Don't forget to save your document.

### Preview

Please Click for higher resolution.

[![Final Wallpaper][image-final]{: .img-responsive }][hires-final]

### SVG Source

You can review our tutorial in this SVG source.

* [epsi-rns.github.io/.../flat-isometric-looks.svg.gz][dotfiles-final]

-- -- --

## Beyond Flat

If you want a more realistic, leaving flat philosophy,
you can add use filter to create effect,
this would make the bar looks like ribbon.

You need to create a duplicate for each ribbon,
so we have bottom and top ribbon.

* Bottom: Filter - Shadows and Glows - Drop Shadow. 

* Top: Morphology - Posterized Blur

You should also darken bottom ribbon color.
Suppose the ribbon have Red 800 color,
the bottom color should be rised to Red 900.

Please Click for higher resolution.

[![Ribbon Wallpaper][image-ribbon]{: .img-responsive }][hires-ribbon]



### DeviantArt

I also put two links, other examples in DeviantArt.
Each with downloadable SVGs.

* [White Blue][deviant-sample-1]

* [Black Red][deviant-sample-2]

* [White Multicolor][deviant-sample-3]

-- -- --

Enjoy.


[//]: <> ( -- -- -- links below -- -- -- )

[link-inspired-by]: https://plus.google.com/photos/+ArtentDesignsPlus/albums/6131117606440340913/6131118364711497282?pid=6131118364711497282&oid=117401759762633574877

[image-pallete]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-accent700.png

[image-dialog-page]:    {{ site.url }}/assets/posts/design/2016/10/flat-isometric-dialog-page.png
[image-dialog-grids]:   {{ site.url }}/assets/posts/design/2016/10/flat-isometric-dialog-grids.png
[image-dialog-shadow]:  {{ site.url }}/assets/posts/design/2016/10/flat-isometric-dialog-shadow.png
[image-dock-layers]:    {{ site.url }}/assets/posts/design/2016/10/flat-isometric-dock-layers.png
[image-dock-transform]: {{ site.url }}/assets/posts/design/2016/10/flat-isometric-dock-transform.png
[image-snap-handle]:    {{ site.url }}/assets/posts/design/2016/10/flat-isometric-snap-handle.png
[image-box-inner]:      {{ site.url }}/assets/posts/design/2016/10/flat-isometric-box-inner.png
[image-box-outer]:      {{ site.url }}/assets/posts/design/2016/10/flat-isometric-box-outer.png
[image-bar-first]:      {{ site.url }}/assets/posts/design/2016/10/flat-isometric-bar-first.png
[image-bar-all]:        {{ site.url }}/assets/posts/design/2016/10/flat-isometric-bar-all.png
[image-bar-cross]:      {{ site.url }}/assets/posts/design/2016/10/flat-isometric-bar-cross.png
[image-bar-shadow]:     {{ site.url }}/assets/posts/design/2016/10/flat-isometric-bar-shadow.png
[image-bar-color]:      {{ site.url }}/assets/posts/design/2016/10/flat-isometric-bar-color.png
[image-text-snap]:      {{ site.url }}/assets/posts/design/2016/10/flat-isometric-text-snap.png
[image-preview]:        {{ site.url }}/assets/posts/design/2016/10/flat-isometric-preview.png
[image-final]:          {{ site.url }}/assets/posts/design/2016/10/flat-isometric-final.png
[image-ribbon]:         {{ site.url }}/assets/posts/design/2016/10/flat-isometric-ribbon.png

[hires-preview]: https://lh3.googleusercontent.com/GYPXUpUEFWq8g9Tn9k91wHV5XIWHq5IdcHNmYLWkhyFiybLXNbglpgrp9ut8eQLLNrmMbA_K8hVtlTQJ4bBjvvoMOed5mnoh2eUOiyIOKYBEkdUQJ8mXPXNqi74kspTUncJUhRduBzr_bf3gsFRiwHg-k2DI3Hke_LLHmyJu-NszGaOgDhN2de8iVzgAEkavwWwErT48nrp1FuTnBE3Dl4nUlbtZX83VIOrRRmoXZYK7P8tceHFn1OY4YpZ3eGdusH1fOSnCvBe9SXRlfjuPtJSAZ_b8hRCcBwHhkZibRlw0GQSUycCGq3ss4Gz2E1YuH3CyDvVmVapH0R1Z47p8dkcv-351PBkmlJPmRDjfa_utRThq1eudH32UQfKjT-5CE9_QccXQ33H_uAR33yeZ-BdbUd-3lSXaIjTwOVzt4QbBc7ZpXNnb3PvvTD8C_L852KOx6dCxX5532_UfDDodCXTLHNU02kSX0gYBWo5EofDAE6F5qwVao88ruwoBhFkP71Jmnxci_fiFUwqzMjlTIkskaJbTuLgkBAIC0sNpAPBWR3fVrImPgz8zsPtEoTo1AlJx0Rjc8Q4SRZOCYgE5IV_uuHeQGv65nOGZ3bp0kPWZUzc4=w0
[hires-final]:   https://lh3.googleusercontent.com/SDi3WfEYcQgAVeYlZrHbXaf8Rso9vhQzxap8ekr3gChLpjRnSAOqWQDRGyNCFUH5FEXGnj4aXo7BwasbGirzPRcNPu84ddck9lnqj0pJ8Tzhx1XgWublM7Zxy1rJCYDSLYxpyukUdPQWNTnIAfWdpl3z50o8u1IL0tlGLIEAiBjcSL-QzSvgUgQlHxmPz6MrJkC06SetQC6-uQCixRCIsxHduEht-u1Fh8YNneIC7VjJ6cqFwHBf_Sek5p49-P_EwNuaS0TXbnp-VdJRBd9Jx9-AkGduHcT2faP3pO13s25VIa-qdIr4antUWqEWe5dhyZMYF7Z5-thHfNIJFqFCmwNG1VuGAU-SnX0Map02x5GQnlysssogoKjVh0xJimKq4Kd-PO9_OiObAwCcrnjaB6upMMN_qETPYc3GhKg57H0Pf8F0PC-82qk2V5aI8dkq_ISEP68_ELvYkubw08eiQM-_noJEVqUcrtNMxV2rH6HwqkEdWQcw8r6FgzFWzB5v0bRbRE-kDoNLClWus2TN3CQIIl4-lWOb5zXvJoZ6b9paMESlrXoemePCp28VfRsjhNULyIBOxKoB92wdrcsN8uh_hoMq6bXIMmyCDFfT1bW1LYQK=w0
[hires-ribbon]:  https://lh3.googleusercontent.com/PgqjQX65sueEbb1-1Uq2hYDpsNeqAmIRDO5y-ycy0CPJsgbrN2U9SfKuY-27rovjU8fG-OkQN6VzMepbgYh5J57ri5XOZNScU81HGGaOf5feHqgt-0_CbopR8DeIG3R7H7Ejpoc1r3MBk28pjjQlqAPN2kmFJ4XKeWsCHxuq852O07Hl7DX2SHtKATTjC-V24qTrxt5oLezSeSEtaRkJjk3WKkQojWX_pV_FfAeGlGHTzvMx4-bHWh2I_qQgNAgyoNi9cBpQuj5ZgLGx7KZDlWPYN10zUSRO0GpeiXuHgY9pROlnotqKVgOHEDTL3IC74mtEazKTbRDNtznlRdogVmni4g8wLkmjtqzog1I7uNGgd2EQXYuo4LOcYceAFdo2CEPdTDXJVh686itlWIpkAJ9HpwAL1QipWwhDiQOIyiwNMrmremNvZOmUnBRZ7N1feHiRn0Z0kyMO5mQvYbT8ciXlHukQJbJat2-s9-9EpLLwyoWPQnMKxlCZr_SFiS6B768kBgRTaKG4qgnzKUKEAvDgkMLDCKRSBucsvsH_Px2Rz7mYJj8mACneOn5xPeyZVD8cO0zT8_Nip0jyU0wah9XqlzZy7GGGHqB-pWcE13Lid7-e=w0

[dotfiles-final]:       {{ site.url }}/assets/posts/design/2016/10/flat-isometric-looks.svg.gz

[deviant-sample-1]: http://nurwijayadi.deviantart.com/art/Anies-Sandiaga-Hashtag-646646564
[deviant-sample-2]: http://nurwijayadi.deviantart.com/art/Material-Design-Wallpaper-646576299
[deviant-sample-3]: http://nurwijayadi.deviantart.com/art/Flat-Wallpaper-with-Isometric-Looks-646980074
