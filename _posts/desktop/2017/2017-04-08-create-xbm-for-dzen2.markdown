---
layout: post-sidemenu-wm
title:  "Create XBM for your Dzen2"
categories: desktop
date:   2017-04-08 13:35:15 +0700
tags: [ricing, statusbar, inkscape, gimp]
author: epsi

excerpt:
  Create Decoration to suit your own dzen2,
  just take a few steps.

---

### Statusbar Tutorial

This tutorial/ guidance/ article is one of some parts.

**Statusbar**

*	[Standalone Statusbar Overview][local-overview]

*	[Standalone Dzen2 Statusbar Using BASH][local-dzen2-bash]

*	[Standalone Dzen2 Statusbar Using Conky][local-dzen2-conky]

*	[Standalone Lemonbar Using Conky][local-lemon-conky]

**Conky**

*	[Standalone CLI System Monitoring Using Conky][local-cli-conky]

*	[Debugging Conky][local-debug-conky]

**Complementary**

*	[Create XBM for your Dzen2][local-xbm-dzen2]

*	[Getting XLFD Font][local-xlfd-font]

-- -- --

Dzen2 can read <code class="code-file">.xbm</code> image format.
For your convenience, I have made some eight glyph icons.
For each has height of 24px, the same height as dzen panel example in this blog.

![Image Source: Diagonal and Arrow][image-source-shapes]{: .img-responsive }

**XBM**:<br/>

*	[github.com/.../xbm/...][dotfiles-xbm]

*	[github.com/.../xbm-source/...][dotfiles-xbm-source]

Everybody has different requirement, taste and style.
Instead of giving <code class="code-file">.xbm</code> files,
I'd better give the Source Image.
And explain the creation process.

### Summary

Create Decoration to suit your own dzen2, just take a few steps.
It is easier when you have Inkscape and GIMP.

Here is the Proces Flow for Each Image.

*	Inkscape: Create Vector

*	Inkscape: Export Page

*	GIMP: Indexed Mode

*	GIMP: Scale

*	GIMP: Export

*	Repeat Process: Next Image


### Inkscape: Create Vector

Create Inkscape <code class="code-file">.svg</code> with size 100x100px Page size.
Create the simple shape as needed, e.g. triangle at bottom right to create a diagonal corner.
And <kbd>Save</kbd> this <code class="code-file">diagonal-corner.svg</code> file.

![XBM: Inkscape: Create Vector][image-xbm-inkscape-vector]{: .img-responsive }

### Inkscape: Export Page

Export Page to <code class="code-file">.png</code> with 96 dpi.
e.g. <code class="code-file">diagonal-corner.png</code> file.

![XBM: Inkscape: Export Page][image-xbm-inkscape-export]{: .img-responsive }

### GIMP: Indexed Mode

Open <code class="code-file">.png</code> with GIMP,
Transformed it into Black and White.
Using <kbd>Image - Mode - Indexed - 1 bit Palette</kbd>.
And <kbd>Save</kbd> this <code class="code-file">.png</code> file.

Close The File.

![XBM: GIMP: Indexed Mode][image-xbm-gimp-index]{: .img-responsive }

### GIMP: Scale

<kbd>Scale Image - Height 24</kbd>.
Or the same height as your panel.
Do not forget to keep the ratio.

![XBM: GIMP: Scale Image][image-xbm-gimp-scale]{: .img-responsive }

### GIMP: Export

<kbd>Export As - X Bitmap Image</kbd>.
e.g. <code class="code-file">dc-24br-.xbm</code> file.
Do not use X10 Format Bitmap.

![XBM: GIMP: Export Image][image-xbm-gimp-export]{: .img-responsive }

### Repeat Process

You may use the same process, or just use GIMP Image Transform,
e.g. <kbd>Image - Transform - Rotate</kbd>,
or <kbd>Image - Transform - Flip</kbd>.

![XBM: Nautilus PReview][image-xbm-nautilus]{: .img-responsive }

-- -- --

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}
{% assign dotfiles_assets = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2/assets' %}

[image-source-shapes]: {{ asset_path }}/xbm-source-shapes.png

[dotfiles-xbm]:        {{ dotfiles_assets }}/xbm/
[dotfiles-xbm-source]: {{ dotfiles_assets }}/xbm-source/

[image-xbm-gimp-export]:     {{ asset_path }}/xbm-gimp-export.png
[image-xbm-gimp-index]:      {{ asset_path }}/xbm-gimp-index.png
[image-xbm-inkscape-vector]: {{ asset_path }}/xbm-inkscape-vector.png
[image-xbm-inkscape-export]: {{ asset_path }}/xbm-inkscape-export.png
[image-xbm-gimp-scale]:      {{ asset_path }}/xbm-gimp-scale.png
[image-xbm-nautilus]:        {{ asset_path }}/xbm-nautilus.png

[local-overview]:    {{ site.url }}/desktop/2017/04/15/standalone-overview.html
[local-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html
