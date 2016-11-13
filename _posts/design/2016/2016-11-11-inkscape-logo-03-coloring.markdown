---
layout: post
title:  "Inkscape Logo Creation Part Three"
date:   2016-11-11 22:30:15 +0700
categories: design
tags: [inkscape, design]
author: epsi

excerpt:
  Complete Detail Sample of Logo Creation.
  Last Part, of Three Articles.

---

## Part Three: Decoration

### Pallete

I'm not good at color. All I can do is sharing what I know.

Inkscape has enough pallete to be used for your project.
One of my favorites is material design pallete.
Just click the right most pallete color, and you will see pallete choice.

![Part Three: Pallete Choice][image-p03-pallete-choice]{: .img-responsive }

I'm curious, how to get a bright color for this logo.
After having some trial error with google material pallete.
I finally found it in accent pallete. In this case a700.

![Part Three: Accent Pallete 700][image-p03-accent-700]{: .img-responsive }

### Coloring

Let's open our last project.
Rename it from 'logo-shape (01).svg' to 'logo-color (01).svg' 
Then open the new 'logo-color (01).svg' file.

We are going to save this elliptical star logo to other place,
because coloring require the path to broken apart.
Create a new 'Star' layer inside 'Basic Shape' layer.
Duplicate logo, and move to 'Star' layer.

Go to 'Star' layer. Click the logo object and make sure it has 100% opacity.
Then go back to 'umbrella' layer. Hide 'Star' layer disability.
We are still working in current 'umbrella' layer.

Click the logo path, 
change the opacity to 100%,
as we don't play with shape anymor.

Click the logo path again, 
and click menu (<kbd>Path - Break Apart</kbd>).

I colored them with: only blue from google material [a300 .. a900]. I really love the soft looks.

![Part Three: Using Blue from Google Material][image-p03-only-material]{: .img-responsive }

Remember that we need to mimic Telkom Vision Logo, and we have seven leaves, so we need seven colors.
Let's pick rainbow color that has seven colors: Red, Orange, Yellow, Green, Blue, Indigo, Violet.

Actually I decide to have a different color than rainbow. 
I colored them with: Red a700, Orange a700, Yellow a700, Green a700, Blue a700, Indigo a700, Violet a700.

![Part Three: Pure Accent 700 Combination][image-p03-accent-pure]{: .img-responsive }

-- -- --

## Shadow and Glow

We are going to need a new layer called 'Shadow and Glow'.
Since the object will be under 'Content' layer.
Let's put the layer below 'Content' layer.

We need four copy  our 'Star' layer. Each renamed to

* 'Shadow Filter' layer.

* 'Glow Filter' layer.

* 'Shadow Blur' layer.

* 'Glow Blur' layer.

All should be unlock and visibility should be disabled.

![Part Three: Layer Shadow and Glow][image-p03-layer-shadow]{: .img-responsive }

We also need to lock the 'Umbrella' layer.

I'm amateur here ini creating glow, so any critics is welcomed.

### Shadow by Filter

Go to 'Shadow Filter' layer, and set show the visibility. 
Select logo and click menu (<kbd>Filters - Shadows and Glows - Drop Shadow</kbd>)

* Blur Color: #00000080.

* Shadow Type: Shadow Only

* Blur: 10px

* Horizontal Offset: 20px

* Vertical Offset: 20px

![Part Three: Filter Drop Shadow][image-p03-drop-shadow]{: .img-responsive }

Let's set show visibility on 'Umbrella' layer and get the result.

![Part Three: With Filter Drop Shadow][image-p03-with-drop-shadow]{: .img-responsive }

### Shadow by Manual Blurring

Go to 'Shadow Blur' layer, and set show the visibility.

There's another method.
We can create manual shadow by resize the object and blur later.

* Horizontal Position: -20px

* Vertical Position: -20px

* Width: +40 px

* Height: +40 px

From Original Size

![Part Three: Original Size][image-p03-blur-shadow-1]{: .img-responsive }

To Modified Size

![Part Three: Modified Size][image-p03-blur-shadow-2]{: .img-responsive }

And Blur about 3% with #00000080 color and get the result.

![Part Three: With Manual Blur Size][image-p03-with-blur-shadow]{: .img-responsive }

### Glow by Filter

With the same method as Shadow by Filter, but using #ffffff80 color.
And put it against Gradient Background.

### Glow by Blur

With the same method as Shadow by Blur, but using #ffffff80 color.
And put it against Gradient Background.

### Combination

Yes you can combine any method above,
also playing with each object opacity.

-- -- --

## Decorations

We are going to need a new layer called 'Decorations'.
Since the object will be on top of 'Content' layer.
Let's put the layer above 'Content' layer.

We need three copy  our 'Star' layer. Each renamed to

* 'Glossy Shine' layer.

* 'Gradient' layer.

* 'Pattern' layer.

And also one empty layer, renamed to

* 'Cool Effect' layer.

![Part Three: Layer Decorations][image-p03-layer-deco]{: .img-responsive }


### Gradient

I know sometimes flat is boring.
We need to make the logo object more realistic
by adding gradient to emulate height against lightning.

Select Logo in 'Gradient' layer.
And Click Menu (<kbd>Object - Fill and Strokes</kbd>).
Click Radial Gradient and add five nodes.
Besides RGBA, we are going to use HSLA.

This is just an example, you may play with other gradient.
I maintain the middle with full transparency to conserve the color,
(1) #ffffffa0
(2) #00000014
(3) #00000000
(4) #ffffff14
(5) #000000a0

![Part Three: Gradient Decoration][image-p03-deco-gradient]{: .img-responsive }

Let's set show visibility on 'Umbrella' layer and get the result.

![Part Three: With Gradient][image-p03-with-gradient]{: .img-responsive }


### Glossy Shine

This is a famous old effect in between the year 2000 and 2010.

Go to 'Glossy Shine' layer. And Set show visibility of this layer.

Create Ellipse from Circle in the middle of document.

* Position: 50px horizontal, 200px vertical.

* Size: 900px width x 600px height

Open Transform Tool

* Rotate 45 Degree

* Move Horizontal -200px

* Move Vertical 200px

Select both Star Shape and Ellipse, 
and click menu (<kbd>Path - Intersection</kbd>).

Select the result object and click Radial Gradient.
Edit Gradient and move the two nodes diagonally.
From top right #ffffff00 to bottom left #ffffff40.
You can also add middle node,
and let's give it almost transparent color #ffffff04.

![Part Three: Glossy Decoration][image-p03-deco-glossy]{: .img-responsive }

Let's set show visibility on 'Umbrella' layer and get the result.

![Part Three: With Glossy][image-p03-with-glossy]{: .img-responsive }


### Cool Filter Effect

This Filter would make your logo a 3D looks.

Go to 'Umbrella' layer. 
Group all Umbrella Leaves. 
Duplicate and move to 'Cool Effect' layer. 

Go to 'Cool Effect' layer. And Set show visibility of this layer.

Select Logo in 'Cool Effect' layer.
And Click Menu (<kbd>Filters - Morphology - Cool Outside</kbd>).
And get the result.

![Part Three: With Cool Effect][image-p03-with-cool-effect]{: .img-responsive }

### Pattern

Long time ago I found this Pattern in Vectezzy

* <https://www.vecteezy.com/patterns/22080-crazy-circles-free-seamless-pattern>

I still desire to create my own pattern.
Since I don't now the license of this pattern,
I won't include thia pattern in the SVG file.
But I'll show you how it can improve your logo.

Go to 'Pattern' layer. And Set show visibility of this layer.
Select Logo in 'Pattern' layer.
And Click Menu (<kbd>Object - Fill and Strokes</kbd>).
And fill with Pattern, with 20% Opacity.

And get the result.

![Part Three: With Pattern][image-p03-with-pattern]{: .img-responsive }

Of course you can experiment with hundreds of cool free pattern from the internet.
Packed Circle from Inkscape also cool.

-- -- --

## Final Result

### Rainbow

Let's Combine all your hardwork into one cool logo.

Please Click for higher resolution.

[![Part Three: Final Image Rainbow][image-p03-final-rainbow]{: .img-responsive }][hires-p03-final-rainbow]

### Google Material

This one using only blue from google material [a100 .. a900]. 

Please Click for higher resolution.

[![Part Three: Final Image Material][image-p03-final-material]{: .img-responsive }][hires-p03-final-material]

### Beyond Logo

After all, there is no limit for creativity.

For final touch I did intersection from the Umbrella Path Against a Splatter Path.
You must ungroup each object and union each, before you do both.

I found this Cool Splatter here.
It was made by 'Bonang Respati Satmoko'. Using GIMP and later Inkscape.
Since the spallter is not mine, I can't put it in the SVG source.
I want to keep the SVG source original.

* <https://archive.org/details/SplatterCat>

I also did (<kbd>Filters - Bevels - Button</kbd>).

I'm using the Rainbow Colored Logo.
The background, I'm using Blue Google Material in Gradient. 
Duplicate it first, Rotate 90 Degree, and reduce opacity to 50%.

Please Click for higher resolution.

[![Part Three: Final Image Beyond][image-p03-final-beyond]{: .img-responsive }][hires-p03-final-beyond]

### Evolve

Dare to Change.
In previous logo, we translate the word Vision to a shape.
I translate this logo to AV (Alumni Vision).
But the thing is, a different requirement need different concept.
For the sake of fun, let's change the AV word to AU word (Auto Update).
so let's modify our basic shape and start over.

While conservatives logo use their name outside the logo,
so the text won't interfere the shape.
Most profesional, blend their logo with their shape.
As you can see in figure below, the text is inside the logo.

Less is more. Simple color has stronger impact than the colourful one.

Please Click for higher resolution.

[![Part Three: Final Image Evolve][image-p03-final-evolve]{: .img-responsive }][hires-p03-final-evolve]

-- -- --

### SVG Source

We're done with making the shape,
but there are still more steps to go.

You can review our tutorial in this SVG source.

* [epsi-rns.github.io/.../logo-color-01.svg.gz][dotfiles-p03]


-- -- --

I know I'm a pathetic fool.

I'll see you again.


[//]: <> ( -- -- -- links below -- -- -- )


[image-p03-pallete-choice]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-pallete-choice.png
[image-p03-accent-700]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-accent700.png
[image-p03-only-material]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-only-material.png
[image-p03-accent-pure]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-pure-accent.png
[image-p03-layer-shadow]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-layer-shadow.png
[image-p03-drop-shadow]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-drop-shadow.png
[image-p03-with-drop-shadow]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-with-drop-shadow.png
[image-p03-blur-shadow-1]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-manual-shadow-original-size.png
[image-p03-blur-shadow-2]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-manual-shadow-modified-size.png
[image-p03-with-blur-shadow]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-with-blur-shadow.png
[image-p03-layer-deco]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-layer-decorations.png
[image-p03-deco-gradient]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-deco-gradient.png
[image-p03-with-gradient]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-with-gradient.png
[image-p03-deco-glossy]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-deco-glossy.png
[image-p03-with-glossy]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-with-glossy.png
[image-p03-with-cool-effect]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-with-cool-effect.png
[image-p03-with-pattern]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-with-pattern.png
[image-p03-final-rainbow]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-final-rainbow.png
[image-p03-final-material]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-final-material.png
[image-p03-final-beyond]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-final-beyond.png
[image-p03-final-evolve]: {{ site.url }}/assets/posts/design/2016/11/logo-deco-03-final-evolve.png

[hires-p03-final-rainbow]: https://lh3.googleusercontent.com/YzLOyHmXUzuQABatw9gjBcECnrMCGXrdvKEozt8ERGmLJbtlKWW6e98oDbUgrYAx9NypNjCM734GDcYCIdgzQBwrdbipdkmHsnvmgGMotI4qawC4JaoVNKHeFO1qZVghEMqEv58i107zQxj_V0woFEbW84SJ8AaRbUrD4FtdrVoH06KkrqSsMZzFlYXaezfdhFglmnTL1pRe9pDFdXZONeJGFIADaHNmt5NUaDrP15-Ubl7tq4ZuUfj5oB1QmsS8bW_sh88DHvUF4dtBsWIJD5Ihm40Q2VNN65AjqWKCDyHA2I-YQDoEIMbYKYQADUUXUGNOFKLdz7YnWA0g7RqDMPk_Kv7F31p43nyBBmJmX9yaKho8Q7QxV7a0PwdEXfjHddcc1avnoN36UCYp4EpO9iX2ONYCDPHP6uaEXzK2H3eQBqXjq1HUyvZLi_Yz_YX2FJzsV9B-xrAGfWsKFFDDE8H-m1OxdEqhDrMlUlY_4abR7ziM9hJ7g73b5Tx3pzWCNh4NeJ9-Lu2V6aB_Ak2p0kqgXIYQILiloZaGrisFX50Ftr4JeMcL2ZbH6z_sM_A_n2FbvJ4wlAvndtd6ZH6r-jfsa_Rfr6E_4XeAZRRuQ6sStw60=s0
[hires-p03-final-material]: https://lh3.googleusercontent.com/jrQB9lvgpBNfiwTJZd0HDlI3Yu6L9gb4Lugm3U4cGjvW2Mhxq6IzFX7r97PTdgpHvr6RQrewEUunHLusKTlNiK7KViFYh_c78QVXoMSalNsg4WSht24aYwDcwYlD4KojyeKlJWRdcgNkHx7tZGKwSSyp1Qt3Wq2I9Y2rWSrHJvLbLrjwGGlIRC6d1nXCfizsXkqBwrHXPfEbunziClKdTHuuaijJbM_qqOYH1GZ7ZYO0WL8TM2qmBKVCXiNyNLLs6zyn3yZqCZQGPynDB1Ey4FxBHnzR1GdlRYNnE4TCEFNmER-ffYILqqON69oGLFdcrP5FfPwIPimaVW7CPQgGF9SX-QFenmv6kzFaA93LdWRzRUsbhSgUQjhv5ptMPeyusMnO_rK41nsch9lU6dxF2AxYOyjsfX8BDwzGR9fRtFojLfXANXPUAcOCnMC5qHWtGObNU_UwQ4Cjkzb35SeKQn3NVKzJgXbuWx4sh31O7NZ_36AEBa5esW07JteTnngBkRsfaPKVnAEm6gQyzJFsh2kdsSKMTcHCDgR05Uafl-yWlF8_Uyqh37sKzxlpS2DilQUrml_P1Kf6Y9OdN6lIvGH2JU7A452JPWJIr23Ai2HpqgeA=s0
[hires-p03-final-beyond]: https://lh3.googleusercontent.com/WEa5xOPKDjrmegsI8QXUqY0aWGs3DzNFu5kM_d_FRkGCE-t7t5_zD9Mes0cwJfiYT2VFJ92rgsS2WvUR9u-IHF_Yi4pgbJPxvbVjmAkvLnUQK6cmCRR8bCxaDeey4XEgkwFIrHUnLvMYbghkd6hmMqLJTxlMbNMn8xaAhhDvyEaBEIuTrvG_9I3I6xseHq2ruKFswmkzI8k3wrZq8Jj8QTQ7RP1dDlK6ZWMGGTr1IuXI-JfHglFhJ2NuLraV8JSvFpSRPA4BPYTC9VQH3Wgs2whyiLamynTyn9AyL31afef-9p_ol1ANuiQUqFbYlCYeAvpTxLTs_cVs-oj-cy5veXoa2JqYIsJQvPa1WUwCDV5qjSr9KqgEc3JiKgdfNUHbg1LGpfpyWI285Cv704y5ihzgmUsW0_YmNusvvRfGPoJOm8IzIaJhXYwToYKLOSMung5K-M5vsw3A7B1q8Nm74ZhWwRMPhjtLp_CrJZsOTcCfqXvfeLvXpeUar63R9xOAM94qWmDk0coJ-Hu9XxRAKX0uaWeu4i4JJAvsVi5ufXWP0SkwKp4y0D1MD9F8vWd2v5J3b-E78x8mYzLN4Sabi3szJ9XcUrt-lfkCZg-sZoip2945=s0
[hires-p03-final-evolve]: https://lh3.googleusercontent.com/0GpXOyNZaUEXmWKxK0BqYx45GJTejoZhJFIQV3Z1PI_Fjbt0XpzudqK0D9tW1T0W5Nfh91kGDiyUlWSk41wGuKd8OD0__GbVYfxzQGvoc-6YhBogk3pGU6xIFgSk8Wt8i8VsxexWCI49NKIfSTBDszQDGwuGPDwwcustG37DQuFIBboolHdLfm-AqM2gU6pJe366beFxxcdUsdZlkQLoHWSP-IYR6QgJqTo1CTTsNHg3mnjRqjzuHX7JD6sVhS6Kf9kHCsTN5ponHWHEdDwNj6hCLGouhEapcMNs-JeHnjEWH-6Y_UjpX6fFYMS5Tff6u43u1AnB_WLGE17yniL2Ky97j0SpHIhuGC0RWpos2udr8Z-4kjtFGO3sDVIWn28VAJuDRWinEP0eloJ1Kg5-8FOz8L5yzXwCRio703Xb2QIeJIEioZqcaoDBuOLkfQvaaTFGl659urn5z5FUM9YPosg7ZTo-Ff_v48ClIEZIUVbhomB_8-qXg2EszsXEPMOVxFmugQaW-RrNXv9FbE3jEIekgd979I_OcBkDARN6gnT479xv5BK2DXu-bg-9ZvF-5c7N7s7jDzzKVQKWIpY3Z9CzMrhOj0kp1lDDeAKpskAW7Q3W=s0

[dotfiles-p03]: {{ site.url }}/assets/posts/design/2016/11/logo-color-01.svg.gz
