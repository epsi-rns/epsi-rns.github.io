---
layout: post-sidemenu-wm
title:  "The Difference Between DE, Shell, WM and Compositor"
categories: desktop
date      : 2016-06-13 23:16:15 +0700
tags      : [thought]
keywords  : [desktop manager, window manager, shell, compositor]
author: epsi

excerpt:
  There is a standalone WM, and WM tighted to a DE.
  Stacking WM, Tiling WM, Dynamic WM.
  Just try every popular DE.
  XFCE4, gnome-shell, KDE, LXQT, Cinnamon.
  And later WM, Awesome, i3 and XMonad.

related_link_ids: 
  - 14110202  # Openbox SWM
  - 14110804  # Fluxbox SWM
  - 14113019  # Awesome TWM Beginner
  - 14120646  # i3 TWM
  - 14121259  # XMonad Tiling

---

Linux is very modular,
you can find a bunch of different distro 
based on GNU/Linux in distrowatch. 
Even Android is using Linux kernel.
Although Android doesn't utilize GNU tools.

I'm not textbook guy, 
I don't know exactly what is the different
and I won't give a definition here either.

[![Cinnamon 3D Switcher][image-ss-cinnamon-3d]{: .img-responsive }][photo-ss-cinnamon-3d]


-- -- --

## Window Manager Perspective

There is a standalone WM, and WM tighted to a DE.

If you don't know what is DE and WM are all about,
here is the abbreviations.

* DE: Desktop Environment

* WM: Window Manager

-- -- --


### WM tighted to a DE

Here is a list of DE with specific WM.


GTK+3 based

*	[gnome-shell][gnome-shell] (C, Meson) is using [Mutter][mutter]

*	[Cinnamon][cinnamon] (C) is using [Muffin][muffin]

*	Elementary's Pantheon (Vala) is using [Gala][gala]

*	Mate is using [Marco][marco]

*	[Unity][unity] is using [Compiz][compiz]


GTK based

*	[XFCE4][xfce4] (C) is using [XFWM4][xfwm4]


QT5 based

*	KDE Plasma5 (C++, CMake) 
	[[framework][plasma-framework], [dekstop][plasma-desktop], [workspace][plasma-workspace]]
	is using either [kwin_x11][kwin] or [kwin_wayland][kwin].

### DE with choice of WM

*	LXQT (C++, CMake) can utilize openbox or xfwm4 or kwin


New Contender
 
*	Deepin (Vala, Go) is using [Deepin-WM][deepin-wm], utilizing HTML5.

*	[Budgie][budgie] (Vala, Meson) is using Window-WM,
	Moving from (GTK) to (QT, QML)

*	BlankOn's [Manokwari][manokwari] (Vala, Utilize Meson Soon)

*	[Liri][liri-shell] (C++, QML), formerly Hawaii

*	[Orbital][orbital] (Rust, Cargo)

-- -- --

### Standalone Window Manager

There are many kind of Window Manager

*	Stacking WM: 
	
	*	[Openbox][openbox] (C)
	
	*	[Fluxbox][fluxbox] (C++)

*	Tiling WM: 

	*	[Awesome][awesome] (C), Lua Configuration
	
	*	[i3][i3]  or [i3-gaps][i3-gaps] (C), Text based Configuration
	
	*	[XMonad][xmonad] (Haskell)
	
	*	[HerbstluftWM][hlwm] (C),
		BASH Configuration. But any language is welcomed.
	
	*	[BSPWM][bspwm] (C)
	
	*	[DWM][dwm] (C)

*	Dynamic WM: You can switch from tiling to floating

Note that you should read from wiki for more comprehensive knowledge.

-- -- --

## Compositor

There is one more Category, The Compositor
Before you ask. I will tell you 
that I don't understand what it means.

* KWin

* Compiz


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

### Switch between WM

Each DE can use different WMs.
You can experiment replacing 
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

### How do I learn ?

For a beginner,
let's try every popular DE.

* XFCE4, gnome-shell, KDE, LXQT, Cinnamon.

And later Window Manager

* Awesome, because it still have menu, it is easy

* i3, intimidating for n00bs. But it is still easy because it only needs a configuration file.

* XMonad. This one need coding, in Haskell. But once you got it running, you'll love it.

-- -- --

### Good Watch

I found a super duper good cast from Aline Abler in youtube.

* <https://www.youtube.com/watch?v=Api6dFMlxAA>

-- -- --

### DE Customization

> Update 2020

![Illustration: Desktop Environment Customization][illustration-custom-de]

-- -- --

### WM Customization

> Update 2020

![Illustration: Window Manager Customization][illustration-custom-wm]

-- -- --

I think that's enough for today.
Correct me If I wrong.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-cinnamon-3d]: {{ site.url }}/assets/posts/desktop/2016/06/cinnamon-3d.png
[photo-ss-cinnamon-3d]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMxkI2MO3-Uamt4onaDbVI1KNW2yxR2E6QtKF-r

[gnome-shell]:	https://github.com/GNOME/gnome-shell
[mutter]:		https://github.com/GNOME/mutter
[cinnamon]:	https://github.com/linuxmint/Cinnamon
[muffin]:	https://github.com/linuxmint/muffin
[gala]:		https://github.com/elementary/gala
[marco]:	https://github.com/mate-desktop/marco
[unity]:	https://code.launchpad.net/unity
[compiz]:	https://launchpad.net/compiz
[xfce4]:	https://github.com/xfce-mirror/xfdesktop
[xfwm4]:	https://github.com/xfce-mirror/xfwm4

[plasma-framework]:	https://github.com/KDE/plasma-framework
[plasma-desktop]:	https://github.com/KDE/plasma-desktop
[plasma-workspace]:	https://github.com/KDE/plasma-workspace
[kwin]:	https://github.com/KDE/kwin

[deepin-wm]:	https://github.com/linuxdeepin/deepin-wm
[budgie]:		https://github.com/budgie-desktop/budgie-desktop
[manokwari]:	https://github.com/BlankOn/manokwari
[liri-shell]:	https://github.com/lirios/shell
[orbital]:		https://github.com/redox-os/orbital
[openbox]:	https://github.com/danakj/openbox
[fluxbox]:	https://github.com/fluxbox/fluxbox

[awesome]:	https://github.com/awesomeWM/awesome
[i3]:		https://github.com/i3/i3
[i3-gaps]:	https://github.com/Airblader/i3
[xmonad]:	https://github.com/xmonad/xmonad	
[hlwm]:		https://github.com/herbstluftwm/herbstluftwm
[bspwm]:	https://github.com/baskerville/bspwm
[dwm]:		https://github.com/sr/dwm

[illustration-custom-de]:   {{ site.url }}/assets/posts/desktop/2020/customization-desktop.png
[illustration-custom-wm]:   {{ site.url }}/assets/posts/desktop/2020/customization-window.png
