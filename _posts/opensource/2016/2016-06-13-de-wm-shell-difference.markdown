---
layout: post
title:  "The difference between DE, WM and a Shell"
categories: opensource
date:   2016-06-13 23:16:15 +0700
tags: [thought]
author: epsi

excerpt:
  There is a standalone WM, and WM tighted to a DE.
  Stacking WM, Tiling WM, Dynamic WM.
  Just try every popular DE.
  XFCE4, gnome-shell, KDE, LXQT, Cinnamon.
  And later WM, Awesome, i3 and XMonad.

---

The difference between DE, WM and a Shell.

GNU/Linux is very modular,
you can find a bunch of different distro 
based on GNU/Linux in distrowatch. 
Even Android is GNU/Linux based.

I'm not textbook guy, 
I don't know exactly what is the different
and I won't give a definition here either.

![Cinnamon 3D Switcher][image-ss-cinnamon-3d]{: .img-responsive }


-- -- --

## Window Manager Perspective

There is a standalone WM, and WM tighted to a DE.

If you don't know what is DE and WM are all about,
here is the abbreviation.

* DE: Desktop Environment
* WM: Window Manager

-- -- --


### WM tighted to a DE

Here is a list of DE with specific WM.


GTK+3 based

* gnome-shell is using Mutter

* Cinnamon is using Muffin

* Pantheon is using Gala

* Mate is using Marco


GTK based

* XFCE4 is using XFWM4


QT based

* KDE is using either kwin_x11 or kwin_wayland
  KWin here is also a compositor. 
  Before you asked. I will tell you that I don't understand.


New Contender
 
* Deepin is using Deepin-WM

* Budgie is using Window-WM


### DE with choice of WM

* LXQT can utilize openbox or xfwm4 or kwin

-- -- --

### Standalone Window Manager

There are many kind of Window Manager

* Stacking WM: openbox, fluxbox

* Tiling WM: Awesome, i3, XMonad

* Dynamic WM: You can switch from tiling to floating

Note that you should read from wiki for more comprehensive knowledge.

-- -- --

### Living on the Edge

Nowadays there is a tendency to 
replace X Display Server with Wayland Weston.

So we have a WM made for Wayland Only

* Velox, I never tried this. So I don't know.

And sadly, some cool WMs made for X only.

* Openbox 

Openbox cannot be ported to Wayland
because it used internal X something.

-- -- --

### Switch beetwen WM

Note that you can experiment replacing 
standard WM in a DE with this standalone WM. 
Prepare for weird result.

My favorite is using KWin in XFCE4
because I like to use Ghost Deco from KWin.

Let's try in XFCE4.

{% highlight bash %}
 $ kwin_x11 --replace &
{% endhighlight %}

or

{% highlight bash %}
 $ openbox --replace &
{% endhighlight %}

and you can switch back to 

{% highlight bash %}
 $ xfwm4 --replace &
{% endhighlight %}

-- -- --

How do I learn ?

For a beginner,
let's try every popular DE.

* XFCE4, gnome-shell, KDE, LXQT, Cinnamon.

And later Window Manager

* Awesome, because it still have menu, it is easy

* i3, intimidating for n00bs. But it is still easy because it only needs a configuration file.

* XMonad. This one need coding, in Haskell. But once you got it running, you'll love it.



-- -- --

I think that's enough for today.
Correct me If I wrong.

Thank you for reading





[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-cinnamon-3d]: {{ site.url }}/assets/posts/opensource/2016/06/cinnamon-3d.png

