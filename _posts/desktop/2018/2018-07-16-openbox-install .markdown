---
layout     : post
title      : "Openbox Install - Common Utility"
categories : desktop
date       : 2018-07-16 09:25:15 +0700
tags       : [openbox]
keywords   : [tutorial, install]
author     : epsi
toc        : toc/2018/05/toc-openbox-config.html

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Common utility for most window manager.
  Still for Dummies.
---

<a name="preface"></a>

### Overview

> Goal: Common utility for most window manager.

A must have knowledge, for people who just jump to WM land.
Example in Debian, Fedora, openSUSE, Gentoo.

#### scrot

{% highlight bash %}
$ scrot -sbcd5
{% endhighlight %}

![scrot -sbcd5][image-ss-scrot]{: .img-responsive }

This is a utility to get a acreenshot.
If you have an error and asking for help in a group,
please give a screenshot or error messages to that group.
We are not sidekick, we don't know what happened in your notebook.

#### feh

{% highlight bash %}
$ feh --bg-scale /usr/share/backgrounds/gnome/Mirror.jpg 
{% endhighlight %}

#### nitrogen

{% highlight bash %}
$ nitrogen /usr/share/backgrounds/
{% endhighlight %}

![nitrogen /usr/share/backgrounds/][image-ss-nitrogen]{: .img-responsive }

To get your dekstop back to your last wallpapaer choice

{% highlight bash %}
$ nitrogen --restore
{% endhighlight %}

-- -- --

#### Debian

In Debian/Ubuntu based, installing is as easy as.

{% highlight bash %}
$ sudo apt install scrot feh nitrogen
{% endhighlight %}

You may click the figure below for fullscreen screenshot.

[![Debian: apt install scrot feh nitrogen][image-ss-apt]{: .img-responsive }][photo-ss-apt]

-- -- --

#### Fedora

{% highlight bash %}
$ sudo dnf install scrot feh nitrogen
{% endhighlight %}

![Fedora: dnf install scrot feh nitrogen][image-ss-dnf]{: .img-responsive }

-- -- --

#### openSUSE

{% highlight bash %}
$ sudo zypper install scrot feh nitrogen
{% endhighlight %}

![openSUSE: zypper install scrot feh nitrogen][image-ss-zypper]{: .img-responsive }


-- -- --

#### Gentoo

Gentoo portage means patience.
But installing these two is fast.

{% highlight bash %}
$ sudo emerge --ask scrot feh nitrogen
{% endhighlight %}

![Gentoo: emerge --ask scrot feh nitrogen][image-ss-emerge]{: .img-responsive }

-- -- --

<a name="whats-next"></a>

### What's Next

Consider continue reading [ [Install: oblogout][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/07' %}

[local-part-config]:  /desktop/2018/07/17/openbox-install.html

[image-ss-apt]:      {{ asset_path }}/02-apt-install-scrot-feh-nitrogen.png
[image-ss-dnf]:      {{ asset_path }}/02-dnf-install-scrot-feh-nitrogen.png
[image-ss-emerge]:   {{ asset_path }}/02-portage-emerge-scrot-feh-nitrogen.png
[image-ss-zypper]:   {{ asset_path }}/02-zypper-in-scrot-feh-nitrogen.png

[photo-ss-apt]:      https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipM2B_0wzcTiXTdpkHcQx1UOPIPW_Ctu1aj2_0X0?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-nitrogen]: {{ asset_path }}/02-nitrogen-example.png
[image-ss-scrot]:    {{ asset_path }}/02-scrot-example.png
