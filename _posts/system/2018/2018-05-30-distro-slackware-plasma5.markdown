---
layout: post
title: "Distribution - Slackware Plasma 5"
date: 2018-05-30 09:45:15 +0700
categories: system
tags: [slackware, distro, package manager]
author: epsi

excerpt:
  My Slackware Experience,
  install KDE Plasma 5 using ktown.

related_link_ids: 
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration

---

### Preface: Courage the Cowardly Dog

>	I'm not a brave guy

I have installed Slackware for the first time about five months ago.
For some peculiar reason, I haven't installed KDE Plasma 5.
I still use KDE 4 foor a little while.

The documentation is actually simple.

*	[https://alien.slackbook.org/blog/kde-5_16-11-available-for-slackware-14-2-and-current/](https://alien.slackbook.org/blog/kde-5_16-11-available-for-slackware-14-2-and-current/)

But unfortunately I read this first which is scary.

*	[http://bear.alienbase.nl/mirrors/alien-kde/current/latest/README](http://bear.alienbase.nl/mirrors/alien-kde/current/latest/README)

After a few months later, I search again, and successfully made it.

{% highlight bash %}
$ slackpkg update
$ slackpkg install ktown
$ slackpkg install-new
$ slackpkg upgrade ktown
$ slackpkg upgrade-all 
{% endhighlight %}

I'm still using openbox while doing all these above.

-- -- --

### Config: Enable ktown

Modify <code class="code-file">/etc/slackpkg/slackpkgplus.conf</code> to enable ktown.

{% highlight conf %}
PKGS_PRIORITY=( ktown )
MIRRORPLUS['ktown']=http://bear.alienbase.nl/mirrors/alien-kde/14.2/5/x86_64/
{% endhighlight %}

![slackpkgplus: ktown][image-ss-slackpkgplus]{: .img-responsive }

-- -- --

### Step by Step

#### Update

This is very common. No need for further explanation.

{% highlight bash %}
$ slackpkg update
{% endhighlight %}

#### Install ktown

This take you to a dialog.

{% highlight bash %}
$ slackpkg install ktown
{% endhighlight %}

[![slackpkg: install ktown dialog][image-ss-ktown-dialog]{: .img-responsive }][photo-ss-ktown-dialog]

Folowed by common slackpkg process.

[![slackpkg: install ktown process][image-ss-ktown-cli]{: .img-responsive }][photo-ss-ktown-cli]

#### Install new

Just another step.

{% highlight bash %}
$ slackpkg install-new
{% endhighlight %}

#### Upgrade ktown

This take you to a dialog.

{% highlight bash %}
$ slackpkg upgrade ktown
{% endhighlight %}

[![slackpkg: upgrade ktown screenshot][image-ss-ktown-upgrade]{: .img-responsive }][photo-ss-ktown-upgrade]

Please click image above to enlarge.


#### Upgrade All

Just another step.

{% highlight bash %}
$ slackpkg upgrade-all
{% endhighlight %}

-- -- --

### Switch Desktop

Now that we are done with those simple process above,
we can log out, and safely login to KDE Plasma 5.

[![Slackware 14.2: KDE Plasma 5][image-ss-kde-plasma5]{: .img-responsive }][photo-ss-kde-plasma5]

Voila !
No magic required.

	All I need is courage.

-- -- --

### Conclusion

That's all

	Finally Plasma 5.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/system/2018/05' %}

[image-ss-slackpkgplus]:   {{ asset_path }}/slackpkgplus-config-ktown.png

[image-ss-ktown-dialog]:   {{ asset_path }}/slackpkg-install-ktown-dialog.png
[photo-ss-ktown-dialog]:   https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipPyYA9sZ5vHUGXMqw0BQ111J4UIv2hJ_dnxI8-w?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-ktown-cli]:      {{ asset_path }}/slackpkg-install-ktown-cli.png
[photo-ss-ktown-cli]:      https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOX49GhQyilbQ_J74zfaOsIkjEa-gFE4DUZ0eo1?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-ktown-upgrade]:  {{ asset_path }}/slackpkg-install-ktown-upgrade.png
[photo-ss-ktown-upgrade]:  https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNrtuozKc4djR_65lyE_ugtpUrpD6Ohmgj_drvd?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-kde-plasma5]:    {{ asset_path }}/slackware-plasma5.png
[photo-ss-kde-plasma5]:    https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOZtReYWf-qwhXmZm6KGA9nlJ4Bmi_kcOiXWH5d?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
