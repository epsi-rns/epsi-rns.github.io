---
layout: post
title:  "Openbox Install - Basic"
categories: desktop
date      : 2018-07-15 09:25:15 +0700
tags      : [openbox]
keywords  : [tutorial, install]
author: epsi

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Openbox install for Dummies.
  Also tint2.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Some case of installing openbox

Installing openbox iseasy for most users,
however some people need a more detail guidance.

#### Table of content

This is not the only install article.
Here are the list:

*	Window Manager and Panel: openbox and tint2

*	Common WM Utility: scrot, feh, nitrogen

*	Specific Openbox Utility: oblogut, obmenu-generator


These specific openbox utility require some sill,
such as git clone, using cpan, and such.

*	oblogout

*	obmenu-generator

#### Distribution

So here below are my screenshots as install log for:

*	Debian (for Debian/Ubuntu based)

*	Fedora

*	openSUSE

*	Gentoo

I won't give guidance for Arch user,
as I guess they have already mastered about installing stuff.

I also use slackware, but I don't have time to make a screenshot.

And the last one, the gentoo screenshot is not a sarcasm.
I just love making screenshot okay!

-- -- --

#### Debian

In Debian/Ubuntu based, installing is as easy as.

{% highlight bash %}
$ sudo apt install openbox tint2
{% endhighlight %}

You may click the figure below for fullscreen screenshot.

[![Debian: apt install openbox tint2][image-ss-apt]{: .img-responsive }][photo-ss-apt]

-- -- --

#### Fedora

{% highlight bash %}
$ sudo dnf install openbox tint2
{% endhighlight %}

![Fedora: dnf install openbox tint2][image-ss-dnf]{: .img-responsive }

-- -- --

#### openSUSE

{% highlight bash %}
$ sudo zypper install openbox tint2
{% endhighlight %}

![openSUSE: zypper install openbox tint2][image-ss-zypper]{: .img-responsive }

-- -- --

#### Gentoo

Gentoo portage means patience.
But installing these two is fast.

{% highlight bash %}
$ sudo emerge --ask openbox tint2
{% endhighlight %}

![Gentoo: emerge --ask openbox tint2][image-ss-emerge]{: .img-responsive }

-- -- --

### What's Next

Consider continue reading [ [Install: scrot, feh, nitrogen][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/07' %}

[local-part-config]:  /desktop/2018/07/16/openbox-install.html

[image-ss-apt]:      {{ asset_path }}/01-apt-install-openbox-tint2.png
[image-ss-dnf]:      {{ asset_path }}/01-dnf-install-openbox-tint2.png
[image-ss-emerge]:   {{ asset_path }}/01-emerge-openbox-tint2.png
[image-ss-zypper]:   {{ asset_path }}/01-zypper-in-openbox-tint2.png

[photo-ss-apt]:      https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipN1g5eax2b3G5T1ss84ig_5JVKFxARK3I7P6Ewh?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR


