---
layout: post
title:  "GhostBSD - Driver"
date      : 2019-03-25 09:45:15 +0700
categories: system
tags      : [bsd]
keywords  : [ghostbsd, wireless driver, display driver]
author: epsi

opengraph:
  image: /assets/site/images/topics/ghostbsd.png

excerpt:
  GhostBSD driver sounds scary at first.
  But once you get along it make sense.
---

{% include post/2019/03/toc-ghostbsd.html %}

### Preface

From the perspective of user,
installing OS for desktop these days just require two things:

* Wireless Driver:
  Notebook has leave DVD device behind, and replace with USB installation.
  Wireless also pretty common, no need any cable.

* Display Driver:
  Because, we are using it as desktop.

Other stuff such as audio can be added later.

#### Wireless Issue

It turned out that GhostBSD does not support the internal broadcomm BCM4313.
It is not that bad, linux can use BCM4313 well to get internet from my router,
but fail miserably to get internet from android tethering.

The recommended hardware is atheros or realtek.

#### Wireless Solution.

The solution is simple.
Buy external wireless.
USB dongle is suitable for my situation.
This little stuff works for me, and also have strong signal.

![USB Dongle: WN725N][image-usb-dongle]{: .img-responsive }

Be aware not to use Broadcomm.

#### Display Issue

GhostBSD detect display driver as a charmed.

But TrueOS server doesn't detect my i915.
Of course, TrueOS server does no come with desktop.
I have to download manualy using cable,
and enable it in config first.

-- -- --

### Wireless Configuration

#### Load Driver

First, I need to load the module

{% highlight bash %}
$ sudo kldload rtwn_usb_load
{% endhighlight %}

{% highlight bash %}
$ sudo ifconfig wlan0 create wlandev rtwn0
$ sudo ifconfig wlan0 up
$ sudo ifconfig
{% endhighlight %}

#### Permanent

I make it permanent by using this config.

`/boot/loader.conf`

{% highlight config %}
rtwn_load="YES"
if_rtwn_usb_load="YES"
{% endhighlight %}

`/etc/rc.conf`

{% highlight config %}
wlans_rtwn0="wlan0"
ifconfig_wlan0="WPA DHCP"
{% endhighlight %}

#### Unplugging and Replugging

After unplug, and then replug the dongle,
the internet would not connect automatically,
because it require sequence of service.

We can enable the internet by restart the network.

{% highlight bash %}
$ service network restart
{% endhighlight %}

![service network restart][image-network-restart]{: .img-responsive }

-- -- --

### Display in TrueOS

This is not a GhostBSD issue,
but rather a TrueOS issue.
Just in case I need it.

I have already succeeded installing TrueOS,
But I decided to use GhostBSD,
because I need my notebook to be ready for as soon as possible.
TrueOS takes manual configuration.
As a beginner, I prefer GhostBSD.

#### Display Driver

{% highlight config %}
$ sudo pkg install drm-kmod
$ kldload i915kms
{% endhighlight %}

And make it permanent in `/etc/rc.conf` if necessary.

{% highlight config %}
kld_list="/boot/modules/i915kms.ko"
{% endhighlight %}

#### Display Manager

Just like any linux with openRC.

{% highlight config %}
$ rc-update add lightdm default
$ service lightdm start
{% endhighlight %}

-- -- --

### What's Next ?

Thank you for reading this article.

Consider continue reading [ [GhostBSD - Migraion from Linux][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/system' %}

[local-part-config]:       /system/2019/03/30/ghostbsd-migration.html

[image-usb-dongle]:         {{ system_path }}/2019/03/WN725N.jpg
[image-network-restart]:    {{ system_path }}/2019/03/trident-service-network-restart.jpg
