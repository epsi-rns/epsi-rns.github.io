---
layout: post
title:  "Openbox Install - Common Utility"
categories: desktop
date:   2018-07-16 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  Common utility for most window manager.
  Still for Dummies.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Common utility for most window manager.

A must have knowledge, for people who jut jump to WM land.
For Debian, Fedora, openSUSE, Gentoo.

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

### What's Next

Consider continue reading [ [Config: General rc.xml][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/07' %}

[local-part-config]:  /desktop/2018/05/03/openbox-config.html

[image-ss-apt]:      {{ asset_path }}/02-apt-install-scrot-feh-nitrogen.png
[image-ss-dnf]:      {{ asset_path }}/02-dnf-install-scrot-feh-nitrogen.png
[image-ss-emerge]:   {{ asset_path }}/02-portage-emerge-scrot-feh-nitrogen.png
[image-ss-zypper]:   {{ asset_path }}/02-zypper-in-scrot-feh-nitrogen.png

[photo-ss-apt]:      https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipM2B_0wzcTiXTdpkHcQx1UOPIPW_Ctu1aj2_0X0?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
