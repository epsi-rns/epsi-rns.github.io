---
layout: post
title:  "Wireless in Command Line"
date:   2014-03-13 18:31:15 +0700
categories: opensource
tags: [thought, network]
author: epsi
excerpt:
  Even if you have installed your linux successfully 
  you still need to know your own hardware. 
  This article also useful to debug networking issue, 
  especially for first time linux install.

---

This is a remake of my facebook note with a similar title.

* [facebook.com/notes/.../433920080087341][facebook-note]

{% highlight bash %}
{% endhighlight %}

Even if you have installed your linux successfully 
you still need to know your own hardware. 
This article also useful to debug networking issue, 
especially for first time linux install.

![Wireless Cover][image-cover]{: .img-responsive }

-- -- --

## Device 

First  thing to do is knowing where your device plugged in, pci or usb.

It can be done with this simple command

{% highlight bash %}
># lspci 
{% endhighlight %}

[![Wireless lspci][image-1-lspci]{: .img-responsive }][picasa-1-lspci]

This command requires some privileges, so it might be different in other distro.

For Ubuntu, add the word sudo.

{% highlight bash %}
 $ sudo lspci 
{% endhighlight %}

[![Wireless lspci controller][image-1-lspci-controller]{: .img-responsive }][picasa-1-lspci-controller]

If you have no luck with this command, maybe it is plugged-in by usb connexion.

Image below shows a Realtek 8187 in 

{% highlight bash %}
># lsusb 
{% endhighlight %}

[![Wireless lsusb][image-1-lsusb]{: .img-responsive }][picasa-1-lsusb]

Le'ts see what related device with 802.11 standard

{% highlight bash %}
># lsmod | grep 802 
{% endhighlight %}

[![Wireless lsmod][image-1-lsmod]{: .img-responsive }][picasa-1-lsmod]


Well... it is also nice to know this command.
It shows what happened to my realtek 8187 device in boot time.

{% highlight bash %}
># dmesg | grep 8187 
{% endhighlight %}

[![Wireless dmesg][image-1-dmesg]{: .img-responsive }][picasa-1-dmesg]

Some wireless card need to be turned on manually. So let's check it out with rfkill command

{% highlight bash %}
># rfkill list 
{% endhighlight %}

[![Wireless rfkill][image-1-rfkill]{: .img-responsive }][picasa-1-rfkill]

-- -- --

## Network

Now we need to list our device. Physical name could be different.

Since iwconfig has been deprecated, we are using iw.

{% highlight bash %}
># iw dev 
{% endhighlight %}

[![Wireless iw][image-2-iw]{: .img-responsive }][picasa-2-iw]

And scan our available essid for our wireless

{% highlight bash %}
># iw dev wlan0 scan | grep -i ssid 
{% endhighlight %}

[![Wireless iw scan][image-2-iw-scan]{: .img-responsive }][picasa-2-iw-scan]

From this step, we may choose method to connect to your wireless. 
There are many methods, but you can only chooses one method at a time. 
This will show you NetworkManager using 'nmcli' command, and other method using 'iw' command.

-- -- --

## Using 'nmcli' 

NetworkManager is installed automatically
because it is a dependency for gnome-shell,
and maybe also cinnamon and unity. 
So it is the common preference method 
unless you are not using those desktop environment. 
Some linux prefer other method, 
e.g Arch let the user choose any method, 
and Mageia using an applet to manage network. 
Again, let's do it with command line.

{% highlight bash %}
># nmcli dev wifi connect mataharani 
{% endhighlight %}

You can use iw to check if it is works.

{% highlight bash %}
># iw dev wlan0 link 
{% endhighlight %}

And ping google to make sure.

{% highlight bash %}
># ping google.com -c 2 
{% endhighlight %}

Finally, you may disconnect,

{% highlight bash %}
># nmcli dev disconnect iface wlan0 
{% endhighlight %}

[![Wireless nmcli connect][image-3-nmcli]{: .img-responsive }][picasa-3-nmcli]

The 'wifi connect' command only supported after Netwok Manager 0.96. 
Kali Linux 1.x series is still using 0.9.4 series of Netwok Manager. 
It does not support connect command. 
So it is a little bit different.

[![Wireless nmcli con up][image-3-nmcli-up]{: .img-responsive }][picasa-3-nmcli-up]

-- -- --

## Stop using Network Manager 

I like the way Arch Linux giving me option what kind of method that I 
can use to connect my wireless with. it require more steps, and it needs
'dhcpcd'. To experiment this with my current notebook, I also need to 
shutdown my NetworkManager service form systemd. Luckily we can apply 
the same method to other distro.

Let's see how we do it with SysV, systemd and upstart.
There are other interesting init. e.g. OpenRC,
but it is beyond this scope. Still, you can see 
example of OpenRC in [lamp setup here][local-lamp].

Before you begin, please stop your currently running NetworManager service

### Using SysV

{% highlight bash %}
># service network-manager stop 
{% endhighlight %}

### Using systemd

{% highlight bash %}
># systemctl disable NetworkManager.service 
{% endhighlight %}

{% highlight bash %}
># systemctl stop NetworkManager.service 
{% endhighlight %}

### Using upstart

{% highlight bash %}
># sudo service network-manager stop 
{% endhighlight %}

Don't forget to enable later, after you finished with your experiment.

-- -- --

## Using 'iw dev' 

Now we can start some experiment with oher method.

Note: Tested with Debian Jessie, Arch, Mageia 3 and Kali 1.06.  
But I still got no luck with Ubuntu Trusty beta in my notebook. 

Sometimes you also need to use 'ip link' command to activate your device.
The ifconfig command has been deprecated and replaced by ip command.

{% highlight bash %}
># ip link show wlan0 
{% endhighlight %}

{% highlight bash %}
># ip link set wlan0 up 
{% endhighlight %}

Then you can use 'iw dev'

{% highlight bash %}
># iw dev wlan0 connect mataharani 
{% endhighlight %}

Again.. check your connexion.

{% highlight bash %}
># iw dev wlan0 link 
{% endhighlight %}

And reserved your IP with dhcpcd

{% highlight bash %}
># dhclient wlan0 
{% endhighlight %}

Now you can ping

{% highlight bash %}
># ping google.com -c 2 
{% endhighlight %}

[![Wireless ip][image-4-summary]{: .img-responsive }][picasa-4-summary]

-- -- --

## Reading 

[Arch Wiki: Wireless Network Configuration][link-archwiki]



[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/opensource/2014/03' %}

[image-cover]: {{ asset_path }}/wireless-cover.png

[image-1-lspci]: {{ asset_path }}/wireless-1-lspci.png
[picasa-1-lspci]: https://lh3.googleusercontent.com/-zzHixWPiXGA/Vz_gqW3HakI/AAAAAAAAATQ/y6klqF6UcLElnbdlKZ2cut1NoJzsyhv0gCCo/s0/wireless-1-lspci.png
[image-1-lspci-controller]: {{ asset_path }}/wireless-1-lspci-controller.png
[picasa-1-lspci-controller]: https://lh3.googleusercontent.com/-gP8xmBTq0os/Vz_gpzBWA1I/AAAAAAAAATQ/4dirqdOLBPEPZ3dMeHpBCQDopMafWdiZQCCo/s0/wireless-1-lspci-controller.png
[image-1-lsusb]: {{ asset_path }}/wireless-1-lsusb.png
[picasa-1-lsusb]: https://lh3.googleusercontent.com/-9pXeWEdT6gY/Vz_gqh1pRvI/AAAAAAAAATQ/9Ekxayme46Qx-GjmjKgIWq14ZznFPLh4ACCo/s0/wireless-1-lsusb.png
[image-1-lsmod]: {{ asset_path }}/wireless-1-lsmod.png
[picasa-1-lsmod]: https://lh3.googleusercontent.com/-9hThNUzx5dA/Vz_gpomwzsI/AAAAAAAAASk/MraFatPPLGgVQgel8DZUy_9TBRgZp_hKgCCo/s0/wireless-1-lsmod.png
[image-1-dmesg]: {{ asset_path }}/wireless-1-dmesg.png
[picasa-1-dmesg]: https://lh3.googleusercontent.com/-7ydgeiqmZUU/Vz_gp9cmCoI/AAAAAAAAASo/syFJcxrjo38ahTOh7UBqg7dVVk3APnZiQCCo/s0/wireless-1-dmesg.png
[image-1-rfkill]: {{ asset_path }}/wireless-1-rfkill.png
[picasa-1-rfkill]: https://lh3.googleusercontent.com/-LcKNiBOKb-w/Vz_gqkv9g7I/AAAAAAAAATQ/6SuOi3dz2gIrRHfeLhgV7b9a9DZrGQnowCCo/s0/wireless-1-rfkill.png

[image-2-iw]: {{ asset_path }}/wireless-2-iw.png
[picasa-2-iw]: https://lh3.googleusercontent.com/-7YaoFOSsKDg/Vz_grNbKA8I/AAAAAAAAATQ/kQON8ydLg3MVKDuGteVw0AMULVe6a-3HwCCo/s0/wireless-2-iw.png
[image-2-iw-scan]: {{ asset_path }}/wireless-2-iw-scan.png
[picasa-2-iw-scan]: https://lh3.googleusercontent.com/-tGKVWVhRF0c/Vz_gq3SOkUI/AAAAAAAAATQ/XqmwWRQCZyQelsfSjCO_1_EcUxklwEuJwCCo/s0/wireless-2-iw-scan.png

[image-3-nmcli]: {{ asset_path }}/wireless-3-nmcli-connect.png
[picasa-3-nmcli]: https://lh3.googleusercontent.com/-Dra9B0_7Y_g/Vz_grmk1lOI/AAAAAAAAATQ/VxB5tuwUFF8RQxiiZjey8FsBsJXuLsBQgCCo/s0/wireless-3-nmcli-connect.png
[image-3-nmcli-up]: {{ asset_path }}/wireless-3-nmcli-con-up.png
[picasa-3-nmcli-up]: https://lh3.googleusercontent.com/-MCk7_XD4_8w/Vz_grd5wqKI/AAAAAAAAATQ/3H7y8XcvcCMB7zBw7EElXgnIYrdWsUH7ACCo/s0/wireless-3-nmcli-con-up.png

[image-4-summary]: {{ asset_path }}/wireless-4-summary.png
[picasa-4-summary]: https://lh3.googleusercontent.com/-CC7dh4vI_oI/Vz_grwKIj6I/AAAAAAAAATQ/SVmIokkX5Qwa4eyaOjrDQegDHrvSlZ-oACCo/s0/wireless-4-summary.png

[facebook-note]: https://www.facebook.com/notes/epsi-r-nurwijayadi/command-line-wireless/433920080087341
[link-archwiki]: https://wiki.archlinux.org/index.php/Wireless_network_configuration
[local-lamp]: {{ site.url }}/opensource/2015/10/16/lamp-stack-manjaro-openrc.html
