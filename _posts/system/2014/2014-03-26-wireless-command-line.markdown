---
layout: post
title:  "Wireless Command Line, a must have Knowledge"
date      : 2014-03-13 18:32:15 +0700
categories: system
tags      : [thought, network]
keywords  : [command line interface, cli, wireless, ip, iw, nmcli]
author: epsi

excerpt:
  Even if you have installed your linux successfully 
  you still need to know your own hardware. 
  This article also useful to debug networking issue, 
  especially for first time linux install.

related_link_ids: 
  - 14040246  # Arch Install

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

[![Wireless lspci][image-1-lspci]{: .img-responsive }][photo-1-lspci]

This command requires some privileges, so it might be different in other distro.

For Ubuntu, add the word sudo.

{% highlight bash %}
 $ sudo lspci 
{% endhighlight %}

[![Wireless lspci controller][image-1-lspci-controller]{: .img-responsive }][photo-1-lspci-controller]

If you have no luck with this command, maybe it is plugged-in by usb connexion.

Image below shows a Realtek 8187 in 

{% highlight bash %}
># lsusb 
{% endhighlight %}

[![Wireless lsusb][image-1-lsusb]{: .img-responsive }][photo-1-lsusb]

Le'ts see what related device with 802.11 standard

{% highlight bash %}
># lsmod | grep 802 
{% endhighlight %}

[![Wireless lsmod][image-1-lsmod]{: .img-responsive }][photo-1-lsmod]


Well... it is also nice to know this command.
It shows what happened to my realtek 8187 device in boot time.

{% highlight bash %}
># dmesg | grep 8187 
{% endhighlight %}

[![Wireless dmesg][image-1-dmesg]{: .img-responsive }][photo-1-dmesg]

Some wireless card need to be turned on manually. 
So let's check it out with <code class="code-command">rfkill</code> command

{% highlight bash %}
># rfkill list 
{% endhighlight %}

[![Wireless rfkill][image-1-rfkill]{: .img-responsive }][photo-1-rfkill]

-- -- --

## Network

Now we need to list our device. Physical name could be different.

Since iwconfig has been deprecated, we are using iw.

{% highlight bash %}
># iw dev 
{% endhighlight %}

[![Wireless iw][image-2-iw]{: .img-responsive }][photo-2-iw]

And scan our available essid for our wireless

{% highlight bash %}
># iw dev wlan0 scan | grep -i ssid 
{% endhighlight %}

[![Wireless iw scan][image-2-iw-scan]{: .img-responsive }][photo-2-iw-scan]

From this step, we may choose method to connect to your wireless. 
There are many methods, but you can only chooses one method at a time. 
This will show you NetworkManager using <code class="code-command">nmcli</code> command, 
and other method using <code class="code-command">iw</code> command.

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

[![Wireless nmcli connect][image-3-nmcli]{: .img-responsive }][photo-3-nmcli]

The <code class="code-command">wifi connect</code> command 
only supported after Netwok Manager 0.96. 
Kali Linux 1.x series is still using 0.9.4 series of Netwok Manager. 
It does not support connect command. 
So it is a little bit different.

[![Wireless nmcli con up][image-3-nmcli-up]{: .img-responsive }][photo-3-nmcli-up]

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

Sometimes you also need to use 
<code class="code-command">ip link</code> command to activate your device.
The <code class="code-command">ifconfig</code> command has been deprecated and replaced by ip command.

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

And reserved your IP with <code>dhcpcd</code>

{% highlight bash %}
># dhclient wlan0 
{% endhighlight %}

Now you can ping

{% highlight bash %}
># ping google.com -c 2 
{% endhighlight %}

[![Wireless ip][image-4-summary]{: .img-responsive }][photo-4-summary]

-- -- --

## Reading 

[Arch Wiki: Wireless Network Configuration][link-archwiki]

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2014/03' %}

[image-cover]: {{ asset_path }}/wireless-cover.png

[image-1-lspci]: {{ asset_path }}/wireless-1-lspci.png
[photo-1-lspci]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipMxKL1R60tjBiY0YSQ9MNIMljRRpcrOLP5eheTN
[image-1-lspci-controller]: {{ asset_path }}/wireless-1-lspci-controller.png
[photo-1-lspci-controller]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipOTEMmuVbVw23bD19Le_bF_jgDDTFKwix2zW3MR
[image-1-lsusb]: {{ asset_path }}/wireless-1-lsusb.png
[photo-1-lsusb]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipNOclJzB3GCGO8TMiWfshduoJVbBcsKa-ssiQYH
[image-1-lsmod]: {{ asset_path }}/wireless-1-lsmod.png
[photo-1-lsmod]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPMsAD1Mof3dgfRcFY7Rvft-wJUACQa8aezaSr5
[image-1-dmesg]: {{ asset_path }}/wireless-1-dmesg.png
[photo-1-dmesg]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipOvy4CUR8GnZpBIR_FL9Y0WYlEQOC6CyWKBcuKL
[image-1-rfkill]: {{ asset_path }}/wireless-1-rfkill.png
[photo-1-rfkill]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipMFOrJMMfH15l4gp5ct-tP_LWk2Qw2bN5zrW1bp

[image-2-iw]: {{ asset_path }}/wireless-2-iw.png
[photo-2-iw]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipMrLGZ4puUBJpAEb3UvHtnnQgLclrfYxdUpDhTR
[image-2-iw-scan]: {{ asset_path }}/wireless-2-iw-scan.png
[photo-2-iw-scan]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPTV2HWWtpowaukjhmEfekak--ddK7mKgQyGw6V

[image-3-nmcli]: {{ asset_path }}/wireless-3-nmcli-connect.png
[photo-3-nmcli]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipNr3Su0WM1fkDqniyMp2_K8SqK14aveSVQLGAHp
[image-3-nmcli-up]: {{ asset_path }}/wireless-3-nmcli-con-up.png
[photo-3-nmcli-up]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipP-uHUMT1NA6uqi3zaynaNXiLxbIC5iOpehs7_O

[image-4-summary]: {{ asset_path }}/wireless-4-summary.png
[photo-4-summary]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipOvGRF_yM0P_iPwWRZqF8vRmDM_Ag4Q67MmKjVP

[facebook-note]: https://www.facebook.com/notes/epsi-r-nurwijayadi/command-line-wireless/433920080087341
[link-archwiki]: https://wiki.archlinux.org/index.php/Wireless_network_configuration
[local-lamp]: {{ site.url }}/opensource/2015/10/16/lamp-stack-manjaro-openrc.html

