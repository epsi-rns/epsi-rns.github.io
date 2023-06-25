---
layout    : post
title     : "Wireless: Network Manager"
categories: system
date      : 2023-05-07 09:25:15 +0700
tags      : [network]
keywords  : [vanilla arch, lenovo, network manager, nmcli, nmtui]
author: epsi
toc        : toc/2023/05/toc-install.html

excerpt:
  Examine wireless in system, device, driver, and interface.

opengraph:
  image: /assets/posts/system/2023/05/041-iw-dev-wlan0-ssid.png

---

### Preface

> Goal: Examine wireless in system: device, driver, and interface.

First thing to do in my post setup
in my vanilla Arch install is `NetworkManager`.
`NetworkManager` is the common network manager in most linux distribution.

-- -- --

### Backend Consideration

#### WPA Supplicant

Before 2020, the only backend for `NetworkManager` is `wpa_supplicant`.

{% highlight bash %}
❯ sudo systemctl enable wpa_supplicant
Created symlink /etc/systemd/system/dbus-fi.w1.wpa_supplicant1.service → /usr/lib/systemd/system/wpa_supplicant.service.
Created symlink /etc/systemd/system/multi-user.target.wants/wpa_supplicant.service → /usr/lib/systemd/system/wpa_supplicant.service.
{% endhighlight %}

I'm still using this backend.

#### INet Wireless Daemon

The fact that arch linux setup has `iwd`,
does not mean that you should use `iwd` along with `nmcli`.

You can use it, but this require additional settings.

#### Required Service

As an overview, 
in order to connect to the internet,
we required these services.

{% highlight bash %}
❯ sudo systemctl start wpa_supplicant.service
❯ sudo systemctl start NetworkManager.service
❯ sudo systemctl start dhcpcd.service
{% endhighlight %}

Do not forget to enable all.

{% highlight bash %}
❯ sudo systemctl enable wpa_supplicant.service
❯ sudo systemctl enable NetworkManager.service
❯ sudo systemctl enable dhcpcd.service
{% endhighlight %}

You should be familiar with these command,
just in case you need to troubleshoot your network.

-- -- --

### Controlling The Service

How does it looks like, managing the NetworkManager service.

#### Enable and Disable

Enabling service is just making a symbolic link.

{% highlight bash %}
❯ sudo systemctl enable NetworkManager
Created symlink /etc/systemd/system/multi-user.target.wants/NetworkManager.service → /usr/lib/systemd/system/NetworkManager.service.
Created symlink /etc/systemd/system/dbus-org.freedesktop.nm-dispatcher.service → /usr/lib/systemd/system/NetworkManager-dispatcher.service.
Created symlink /etc/systemd/system/network-online.target.wants/NetworkManager-wait-online.service → /usr/lib/systemd/system/NetworkManager-wait-online.service.
{% endhighlight %}

![Network Manager: Enable][061-nm-enable]

And so, disabling service is just removing a symbolic link.

{% highlight bash %}
❯ sudo systemctl disable NetworkManager.service
Removed "/etc/systemd/system/multi-user.target.wants/NetworkManager.service".
Removed "/etc/systemd/system/dbus-org.freedesktop.nm-dispatcher.service".
Removed "/etc/systemd/system/network-online.target.wants/NetworkManager-wait-online.service".
{% endhighlight %}

![Network Manager: Disable][061-nm-disable]

You may use `su` to get into root, or using `sudo`,
so the command line won't ask you about which privilege to use.

{% highlight bash %}
❯ systemctl enable NetworkManager.service
==== AUTHENTICATING FOR org.freedesktop.systemd1.manage-unit-files ====
Authentication is required to manage system service or unit files.
Multiple identities can be used for authentication:
 1.  epsi
 2.  rizqi
Choose identity to authenticate as (1-2):
{% endhighlight %}

![Network Manager: No Sudo][061-nm-user-nosu]

Note that enable service,
does not start the service.

#### Status

Consider go deep into status as well.

Inactive service would looks like below output.

{% highlight bash %}
❯ sudo systemctl status NetworkManager.service
○ NetworkManager.service - Network Manager
     Loaded: loaded (/usr/lib/systemd/system/NetworkManager.service;>
     Active: inactive (dead)
       Docs: man:NetworkManager(8)

Jun 15 09:14:28 utama NetworkManager[5226]: <info>  [1686795268.1714>
Jun 15 09:14:28 utama NetworkManager[5226]: <info>  [1686795268.1715>
Jun 15 09:14:28 utama NetworkManager[5226]: <warn>  [1686795268.1722>
Jun 15 09:14:28 utama NetworkManager[5226]: <info>  [1686795268.1722>
{% endhighlight %}

![Network Manager: Status Inactive][062-nm-status]

You should start the `NetworkManager` to activate the service.

{% highlight bash %}
❯ sudo systemctl start NetworkManager
{% endhighlight %}

So the output would looks like below:

{% highlight bash %}
❯ sudo systemctl status NetworkManager.service
● NetworkManager.service - Network Manager
     Loaded: loaded (/usr/lib/systemd/system/NetworkManager.service;>
     Active: active (running) since Thu 2023-06-15 09:22:51 WIB; 54s>
       Docs: man:NetworkManager(8)
   Main PID: 6613 (NetworkManager)
      Tasks: 4 (limit: 18838)
     Memory: 5.7M
        CPU: 366ms
     CGroup: /system.slice/NetworkManager.service
             └─6613 /usr/bin/NetworkManager --no-daemon

Jun 15 09:23:24 utama NetworkManager[6613]: <info>  [1686795804.5763>
Jun 15 09:23:24 utama NetworkManager[6613]: <info>  [1686795804.5764>
Jun 15 09:23:24 utama NetworkManager[6613]: <info>  [1686795804.9693>
{% endhighlight %}

![Network Manager: Status Inactive][062-nm-status-act]

#### Start and Stop

You can play with start and stop.

{% highlight bash %}
❯ sudo systemctl start NetworkManager
❯ sudo systemctl stop NetworkManager
{% endhighlight %}

![Network Manager: Start Stop][062-nm-start-stop]

Nothing complex.

-- -- --

### DHCP Service

If you have trouble connecting on the first time,
you might also require `dhcpcd` temporarily.
Alternatively you can use `dhclient`, instead of `dhcpcd`.

#### IP Leasing

`NetworkManager` can connect to an SSID,
but you still require DHCP client daemon to obtain an IP lease.
Without DHCPCD, the ping might failed as below:

{% highlight bash %}
❯ ping archlinux.org -c 2
ping: archlinux.org: Temporary failure in name resolution
{% endhighlight %}

#### Enable and Disable

We should enable the service,
so we can have this service automatically started at boot.

{% highlight bash %}
❯ sudo systemctl enable dhcpcd.service
Created symlink /etc/systemd/system/multi-user.target.wants/dhcpcd.service → /usr/lib/systemd/system/dhcpcd.service.
{% endhighlight %}

![Network Manager: Enable DHCPCD][063-dhcp-enable]

And you can try the opposite command.

{% highlight bash %}
❯ sudo systemctl disable dhcpcd.service
Removed "/etc/systemd/system/multi-user.target.wants/dhcpcd.service".
{% endhighlight %}

![Network Manager: Disable DHCPCD][063-dhcp-disable]

#### Status

Active service would looks like below output.

{% highlight bash %}
❯ sudo systemctl status dhcpcd.service
● dhcpcd.service - DHCP/ IPv4LL/ IPv6RA/ DHCPv6 client on all interf>
     Loaded: loaded (/usr/lib/systemd/system/dhcpcd.service; enabled>
     Active: active (running) since Thu 2023-06-15 09:27:59 WIB; 29s>
   Main PID: 7480 (dhcpcd)
      Tasks: 5 (limit: 18838)
     Memory: 1.7M
        CPU: 65ms
     CGroup: /system.slice/dhcpcd.service
             ├─7480 "dhcpcd: [manager] [ip4] [ip6]"
             ├─7481 "dhcpcd: [privileged proxy]"
             ├─7482 "dhcpcd: [network proxy]"
             ├─7483 "dhcpcd: [control proxy]"
             └─7513 "dhcpcd: [BPF ARP] wlan0 192.168.0.67"

Jun 15 09:27:59 utama dhcpcd[7481]: wlan0: IAID 2d:0c:49:00
Jun 15 09:27:59 utama dhcpcd[7481]: wlan0: adding address fe80::c4db>
Jun 15 09:27:59 utama dhcpcd[7481]: wlan0: soliciting a DHCP lease
Jun 15 09:28:00 utama dhcpcd[7481]: wlan0: soliciting an IPv6 router
Jun 15 09:28:06 utama dhcpcd[7481]: wlan0: offered 192.168.0.67 from>
Jun 15 09:28:06 utama dhcpcd[7481]: wlan0: probing address 192.168.0>
Jun 15 09:28:11 utama dhcpcd[7481]: wlan0: leased 192.168.0.67 for 7>
Jun 15 09:28:11 utama dhcpcd[7481]: wlan0: adding route to 192.168.0>
Jun 15 09:28:11 utama dhcpcd[7481]: wlan0: adding default route via >
Jun 15 09:28:13 utama dhcpcd[7481]: wlan0: no IPv6 Routers available
{% endhighlight %}

![Network Manager: DHCPCD Status][064-dhcp-status]

#### Start and Stop

You can also play with start and stop.

{% highlight bash %}
❯ sudo systemctl start dhcpcd.service
❯ sudo systemctl stop dhcpcd.service
{% endhighlight %}

![Network Manager: DHCPCD Start Stop][064-dhcp-st-st]

-- -- --

### NMTUI

Beginner should start with `nm-applet`.
The problem is, `nm-applet` reqquire GUI.
Most vanilla arch post-setup comes with no GUI.
So we need other cool tool.

Terminal user would love the easy to use `nmtui`.
First it show the main menu.

![Network Manager: nmtui][065-nmtui-01]

With edit dialog.
we can edit an already made connection.
Or make a new one. Or forget a connection by deleting it.

![Network Manager: nmtui][065-nmtui-02]

Normally you would go straight to connect to a network,
by choosing SSID name.
This can be done in activate dialog.

![Network Manager: nmtui][065-nmtui-03]

While choosing SSID,
there will be notification,
showing that `nmtui`  is trying to connect to your chosen SSID.

![Network Manager: nmtui][065-nmtui-05]

After choosing a connection,
you would see star to the connected SSID.

![Network Manager: nmtui][065-nmtui-04]

That is all.

-- -- --

### Testing The Connection

If all done well, we can test the connection.

{% highlight bash %}
❯ ping 8.8.8.8 -c 2
PING 8.8.8.8 (8.8.8.8) 56(84) bytes of data.
64 bytes from 8.8.8.8: icmp_seq=1 ttl=115 time=17.4 ms
64 bytes from 8.8.8.8: icmp_seq=2 ttl=115 time=17.2 ms

--- 8.8.8.8 ping statistics ---
2 packets transmitted, 2 received, 0% packet loss, time 1002ms
rtt min/avg/max/mdev = 17.218/17.285/17.353/0.067 ms
{% endhighlight %}

![Network Manager: ping 8.8.8.8][065-ping-8888]

{% highlight bash %}
❯ ping archlinux.org -c 2
PING archlinux.org (95.217.163.246) 56(84) bytes of data.
64 bytes from 95.217.163.246: icmp_seq=1 ttl=48 time=276 ms
64 bytes from 95.217.163.246: icmp_seq=2 ttl=48 time=229 ms

--- archlinux.org ping statistics ---
2 packets transmitted, 2 received, 0% packet loss, time 10287ms
rtt min/avg/max/mdev = 228.719/252.346/275.974/23.627 ms
{% endhighlight %}

![Network Manager: ping archlinux.org][065-ping-arch-org]

### NM CLI

Sometimes it helps to also know, the command line.
Not just for curious people,
but sometimes for troubleshooting.
The low level connection is provided by `nmcli`.

Let's start by this example.

{% highlight bash %}
❯ nmcli dev wifi connect "E.R. Nurwijayadi"
Error: NetworkManager is not running.
❯ sudo systemctl start NetworkManager
{% endhighlight %}

This means, you need to start `NetworkManager.service` by `systemctl`.
Let's try again.

{% highlight bash %}
❯ nmcli dev wifi connect "E.R. Nurwijayadi"
Error: Connection activation failed: Secrets were required, but not provided.
{% endhighlight %}

This means, you need to start `wpa_supplicant.service` by `systemctl`.
Let's try again.

{% highlight bash %}
❯ sudo systemctl start wpa_supplicant
{% endhighlight %}

For first time user connection,
you need to provide password.

{% highlight bash %}
❯ nmcli dev wifi connect "E.R. Nurwijayadi" password oyenyebus
Device 'wlan0' successfully activated with '4525cdf8-a007-446e-b3c2-cd5172ce44df'.
{% endhighlight %}


{% highlight bash %}
❯ sudo wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)
Successfully initialized wpa_supplicant
Failed to open config file '/proc/self/fd/13', error: No such file or directory
Failed to read or parse configuration '/proc/self/fd/13'.
: CTRL-EVENT-DSCP-POLICY clear_all
{% endhighlight %}


-- -- --

### iwd backend

iwgtk

iwd applet

-- -- --

<a name="conclusion"></a>

### Conclusion

I should know what's inside my notebook.
So I can be ready for Gentoo.
We will continue to NetworkManager



Thank you for reading and visiting.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/05wli' %}

[ref-bandit]:       https://bandithijo.dev/blog/mudah-mengkonfigurasi-wifi-dengan-iwd



[061-nm-disable]:   {{ asset_path }}/061-nm-disable.png
[061-nm-enable]:    {{ asset_path }}/061-nm-enable.png
[061-nm-user-nosu]: {{ asset_path }}/061-nm-user-nosudo.png
[062-nm-start-stop]:{{ asset_path }}/062-nm-start-stop.png
[062-nm-status]:    {{ asset_path }}/062-nm-status.png
[062-nm-status-act]:{{ asset_path }}/062-nm-status-active.png
[063-dhcp-disable]: {{ asset_path }}/063-dhcp-disable.png
[063-dhcp-enable]:  {{ asset_path }}/063-dhcp-enable.png
[064-dhcp-st-st]:   {{ asset_path }}/064-dhcp-start-stop.png
[064-dhcp-status]:  {{ asset_path }}/064-dhcp-status.png

[065-nmtui-01]:     {{ asset_path }}/065-nmtui-01.png
[065-nmtui-02]:     {{ asset_path }}/065-nmtui-02.png
[065-nmtui-03]:     {{ asset_path }}/065-nmtui-03.png
[065-nmtui-04]:     {{ asset_path }}/065-nmtui-04.png
[065-nmtui-05]:     {{ asset_path }}/065-nmtui-05.png
[065-ping-8888]:    {{ asset_path }}/065-ping-8888.png
[065-ping-arch-org]:{{ asset_path }}/065-ping-archlinux.png

enable network manager