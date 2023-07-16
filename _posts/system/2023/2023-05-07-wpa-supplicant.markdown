---
layout    : post
title     : "Wireless: WPA Supplicant"
categories: system
date      : 2023-05-07 09:25:15 +0700
tags      : [network]
keywords  : [vanilla arch, lenovo, wpa-supplicant, wpa-cli]
author: epsi
toc        : toc/2023/05/toc-install.html

excerpt:
  Examine wireless in system, device, driver, and interface.

opengraph:
  image: /assets/posts/system/2023/05wli/050-summary.png

---

### Preface

> Goal: Examine wireless in system: device, driver, and interface.

Before we get into the popular NetworkManager,
we should know that before 2020 NetworkManager depend on WPA Supplicant.
`wpa_supplicant` was the only backend for `NetworkManager`.
This is an old school tools, and we would explore this tools,
so we do not get confused when we need to use it later on.

#### Reference

There is better reference for you:

* [Mudah Mengkonfigurasi Wi-Fi dengan wpa_supplicant][ref-bandit]

-- -- --

### WPA Passphrase

#### Generate Config

We can generate the config using command line.

{% highlight bash %}
❯ wpa_passphrase "E.R. Nurwijayadi" oyenyebus
network={
	ssid="E.R. Nurwijayadi"
	#psk="oyenyebus"
	psk=50a047c8ba8ff1f14bc7651ff1b6e4bb6e6682d4c218181461a3ae22b014b7ad
}
{% endhighlight %}

![WPA: Passphrase][071-passphrase]

#### Passing Config Through Command

Then using root account,
you can pass this config directly to `wpa_supplicant` command.

{% highlight bash %}
❯# wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)
Successfully initialized wpa_supplicant
{% endhighlight %}

![WPA: Passphrase: Complete][072-pp-complete]

#### Hardcoded Configuration

It is a good idea to save the configuration,
to make it reusable.

{% highlight bash %}
❯ cat /etc/wpa_supplicant/wpa_supplicant.conf
ctrl_interface=/run/wpa_supplicant GROUP=wheel
update_config=1

network={
  ssid="E.R. Nurwijayadi"
  # psk="oyenyebus"
  psk=50a047c8ba8ff1f14bc7651ff1b6e4bb6e6682d4c218181461a3ae22b014b7ad
  # mesh_fwding=1
}
{% endhighlight %}

![WPA: Configuration][077-bat-config]

So this setting can be called anytime,
by using either command line or as a service.

{% highlight bash %}
❯ sudo wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf
Successfully initialized wpa_supplicant
{% endhighlight %}

![WPA: wpa_supplicant: using config with root][077-wpa-cfg-root]

Beware of the privileges.

{% highlight bash %}
❯ wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf
Successfully initialized wpa_supplicant
Could not set interface wlan0 flags (UP): Operation not permitted
nl80211: Could not set interface 'wlan0' UP
nl80211: deinit ifname=wlan0 disabled_11b_rates=0
wlan0: Failed to initialize driver interface
wlan0: CTRL-EVENT-DSCP-POLICY clear_all
{% endhighlight %}

![WPA: wpa_supplicant: using config without root][077-wpa-cfg-user]

Just do not forget the magic `sudo` word.

#### Privilege

Beware with piping when using sudo.

{% highlight bash %}
❯ sudo wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)
Successfully initialized wpa_supplicant
Failed to open config file '/proc/self/fd/13', error: No such file or directory
Failed to read or parse configuration '/proc/self/fd/13'.
: CTRL-EVENT-DSCP-POLICY clear_all
{% endhighlight %}

![WPA: wpa_supplicant: sudo pipe fail][073-sudo-pipe-f]

We can solve this issue with shell.

{% highlight bash %}
❯ sudo sh -c 'wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)'
Successfully initialized wpa_supplicant
{% endhighlight %}

![WPA: wpa_supplicant: sh sudo pipe][073-sh-sudo-pipe]

Or you can use root account as well.

{% highlight bash %}
❯# wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)
Successfully initialized wpa_supplicant
{% endhighlight %}

![WPA: wpa_supplicant: su root pipe][073-su-root-pipe]

#### Already Configured

When you have `iwd` or `NetworkManager` running,
you will have this message.

{% highlight bash %}
❯# wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)
Successfully initialized wpa_supplicant
nl80211: kernel reports: Match already configured
nl80211: kernel reports: Match already configured
nl80211: kernel reports: Match already configured
nl80211: kernel reports: Match already configured
nl80211: kernel reports: Match already configured
{% endhighlight %}

![WPA: Passphrase: NM CLI][072-pp-with-nmcli]

-- -- --

### Running WPA Supplicant

We can either run WPA supplicant using `wpa_supplicant` command,
or using init such as `systemd`.

#### Using systemd

As usual command.

{% highlight bash %}
❯ sudo systemctl enable wpa_supplicant
Created symlink /etc/systemd/system/dbus-fi.w1.wpa_supplicant1.service → /usr/lib/systemd/system/wpa_supplicant.service.
Created symlink /etc/systemd/system/multi-user.target.wants/wpa_supplicant.service → /usr/lib/systemd/system/wpa_supplicant.service.
{% endhighlight %}

![WPA: wpa_supplicant: systemctl enable][074-sysctl-enable]

And also start.

{% highlight bash %}
❯ sudo systemctl start wpa_supplicant
{% endhighlight %}

![WPA: wpa_supplicant: systemctl start][074-sysctl-start]

You can also check the status of the wpa_supplicant service.

{% highlight bash %}
❯ sudo systemctl status wpa_supplicant
● wpa_supplicant.service - WPA supplicant
     Loaded: loaded (/usr/lib/systemd/system/wpa_supplicant.service; enabled; preset: disabled)
     Active: active (running) since Sun 2023-07-16 05:09:07 WIB; 34s ago
   Main PID: 487 (wpa_supplicant)
      Tasks: 1 (limit: 18837)
     Memory: 3.2M
        CPU: 2ms
     CGroup: /system.slice/wpa_supplicant.service
             └─487 /usr/bin/wpa_supplicant -u -s -O /run/wpa_supplicant

Jul 16 05:09:07 utama systemd[1]: Starting WPA supplicant...
Jul 16 05:09:07 utama systemd[1]: Started WPA supplicant.
Jul 16 05:09:07 utama wpa_supplicant[487]: Successfully initialized wpa_supplicant
{% endhighlight %}

![WPA: wpa_supplicant: systemctl status][074-sysctl-status]

#### Using Command

We have seen before.

{% highlight bash %}
❯ sudo sh -c 'wpa_supplicant -B -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)'
Successfully initialized wpa_supplicant
{% endhighlight %}

![WPA: wpa_supplicant: sh sudo pipe][073-sh-sudo-pipe]

#### Debugging

This command line can be useful for debugging.
For example this command.
Would produce very long message.

{% highlight bash %}
❯ sudo sh -c 'wpa_supplicant -B -d -i wlan0 -c <(wpa_passphrase "E.R. Nurwijayadi" oyenyebus)'
wpa_supplicant v2.10
Successfully initialized wpa_supplicant
Initializing interface 'wlan0' conf '/dev/fd/63' driver 'default' ctrl_interface 'N/A' bridge 'N/A'
Configuration file '/dev/fd/63' -> '/dev/fd/63'
Reading configuration file '/dev/fd/63'
Priority group 0
   id=0 ssid='E.R. Nurwijayadi'
nl80211: TDLS supported
nl80211: TDLS external setup
...
MBO: Update non-preferred channels, non_pref_chan=N/A
wlan0: Added interface wlan0
wlan0: State: DISCONNECTED -> DISCONNECTED
nl80211: Set wlan0 operstate 0->0 (DORMANT)
netlink: Operstate: ifindex=3 linkmode=-1 (no change), operstate=5 (IF_OPER_DORMANT)
Daemonize..
{% endhighlight %}

![WPA: wpa_supplicant: debugging][075-sudo-wpa-01]

![WPA: wpa_supplicant: debugging][075-sudo-wpa-02]

![WPA: wpa_supplicant: debugging][075-sudo-wpa-03]

![WPA: wpa_supplicant: debugging][075-sudo-wpa-04]

-- -- --

### WPA CLI

This section is under construction.

#### Summary

As a summary, here is the command to activate a network.

{% highlight bash %}
> scan
> scan_results

> add_network
> list_networks

> set_network 0 ssid "E.R. Nurwijayadi"
> set_network 0 psk "oyenyebus"

> select_network 0
> enable_network 0
{% endhighlight %}

#### Running

{% highlight bash %}
❯ sudo wpa_cli
wpa_cli v2.10
Copyright (c) 2004-2022, Jouni Malinen <j@w1.fi> and contributors

This software may be distributed under the terms of the BSD license.
See README for more details.


Selected interface 'wlan0'

Interactive mode

>
{% endhighlight %}

![WPA CLI: Running][081-sudo-wpa-cli]

{% highlight bash %}

{% endhighlight %}

![WPA CLI: Not Running][081-sudo-wpa-clin]

#### Scan

{% highlight bash %}
> scan
OK
<3>CTRL-EVENT-SCAN-STARTED 
<3>CTRL-EVENT-SCAN-RESULTS 
{% endhighlight %}

![WPA CLI: Scan][082-scan]

{% highlight bash %}
> scan_results
bssid / frequency / signal level / flags / ssid
60:7e:cd:d2:1a:e8	2427	-66	[WPA-PSK-CCMP+TKIP][WPA2-PSK-CCMP+TKIP][ESS]	HUAWEI-yVkA
80:3f:5d:ca:fb:2b	2412	-89	[WPA-PSK-CCMP][WPA2-PSK+FT/PSK-CCMP][ESS]	WTWTWT
b4:0f:3b:4a:c5:e1	2422	-85	[WPA-PSK-CCMP][WPA2-PSK-CCMP][ESS]	cotomakassar
90:91:64:1c:1d:25	2437	-85	[WPA2-PSK-CCMP][WPS][ESS]	ambonmania-EXT
3c:84:6a:2a:6e:6b	2462	-57	[WPA2-PSK-CCMP+TKIP][ESS]	Whitewood_EXT
a0:f4:79:87:8f:a6	2462	-61	[WPA-PSK-CCMP+TKIP][WPA2-PSK-CCMP+TKIP][WPS][ESS]	Whitewood
44:55:b1:aa:0f:6c	2412	-80	[WPA-PSK-CCMP+TKIP][WPA2-PSK-CCMP+TKIP][ESS]	HUAWEI-HTAs
c0:bf:c0:19:08:0c	2442	-90	[WPA-PSK-CCMP+TKIP][WPA2-PSK-CCMP+TKIP][ESS]	RumahAspirasiMaluku
3c:84:6a:2a:6e:6a	5180	-72	[WPA2-PSK-CCMP+TKIP][ESS]	Whitewood_5GEXT
80:3f:5d:ca:f9:0b	2462	-90	[WPA-PSK-CCMP][WPA2-PSK+FT/PSK-CCMP][ESS]	WTWTWT
58:6d:8f:8f:3a:d4	2462	-94	[WPA2-PSK-CCMP][WPS][ESS]	EDA
30:23:03:c2:ab:2b	2467	-92	[WPA-PSK-CCMP+TKIP][WPA2-PSK-CCMP+TKIP][WPS][ESS]	merapi1
80:3f:5d:ca:e5:c7	2452	-94	[WPA-PSK-CCMP][WPA2-PSK+FT/PSK-CCMP][ESS]	WTWTWT
1e:77:f6:4c:0d:31	2442	-32	[WPA2-PSK-CCMP][WPS][ESS]	E.R. Nurwijayadi
24:9e:ab:ef:46:c0	2427	-91	[WPA-PSK-CCMP+TKIP][WPA2-PSK-CCMP+TKIP][ESS]	RAKHA
{% endhighlight %}

![WPA CLI: Scan Result][082-scan-result]

#### Network Setting

{% highlight bash %}
> list_networks
network id / ssid / bssid / flags
0	E.R. Nurwijayadi	any
{% endhighlight %}

![WPA CLI: List Network 0][083-list-net-0]

{% highlight bash %}
> add_network
1
<3>CTRL-EVENT-NETWORK-ADDED 1
{% endhighlight %}

![WPA CLI: Add Network][083-add-network]

{% highlight bash %}
> list_networks
network id / ssid / bssid / flags
0	E.R. Nurwijayadi	any	[CURRENT]
1		any	[DISABLED]
{% endhighlight %}

![WPA CLI: List Network 1][083-list-net-1]

#### Connexion Setting

{% highlight bash %}
> set_network 0 ssid "E.R. Nurwijayadi"
OK
{% endhighlight %}

![WPA CLI: Set SSID][084-set-net-ssid]

{% highlight bash %}
> set_network 0 psk "oyenyebus"
OK
{% endhighlight %}

![WPA CLI: Set PSK][084-set-net-psk]

#### Activate Network

{% highlight bash %}
> select_network 0
OK
{% endhighlight %}

![WPA CLI: ][085-enable-net-0]

{% highlight bash %}
> enable_network 0
OK
{% endhighlight %}

![WPA CLI: ][085-select-net-0]

{% highlight bash %}
> enable_network 0
OK

<3>CTRL-EVENT-SIGNAL-CHANGE above=0 signal=-89 noise=9999 txrate=104000
<3>CTRL-EVENT-SCAN-STARTED 
<3>CTRL-EVENT-SIGNAL-CHANGE above=1 signal=-64 noise=9999 txrate=104000
<3>CTRL-EVENT-SCAN-RESULTS 
<3>CTRL-EVENT-DO-ROAM cur_bssid=a0:f4:79:87:8f:a6 cur_freq=2462 cur_level=-66 cur_est=62400 sel_bssid=1e:77:f6:4c:0d:31 sel_freq=2442 sel_level=-32 sel_est=65000
<3>SME: Trying to authenticate with 1e:77:f6:4c:0d:31 (SSID='E.R. Nurwijayadi' freq=2442 MHz)
<3>CTRL-EVENT-REGDOM-CHANGE init=CORE type=WORLD
<3>Trying to associate with 1e:77:f6:4c:0d:31 (SSID='E.R. Nurwijayadi' freq=2442 MHz)
<3>Associated with 1e:77:f6:4c:0d:31
<3>CTRL-EVENT-SUBNET-STATUS-UPDATE status=0
<3>WPA: Key negotiation completed with 1e:77:f6:4c:0d:31 [PTK=CCMP GTK=CCMP]
<3>CTRL-EVENT-CONNECTED - Connection to 1e:77:f6:4c:0d:31 completed [id=0 id_str=]
<3>CTRL-EVENT-SIGNAL-CHANGE above=1 signal=-33 noise=9999 txrate=52000
{% endhighlight %}

![WPA CLI: ][085-select-net-1]

#### Disconnect

{% highlight bash %}
> disconnect
OK
<3>CTRL-EVENT-DISCONNECTED bssid=1e:77:f6:4c:0d:31 reason=3 locally_generated=1
<3>CTRL-EVENT-DSCP-POLICY clear_all
<3>CTRL-EVENT-REGDOM-CHANGE init=CORE type=WORLD
> 
{% endhighlight %}

![WPA CLI: ][086-disconnect]

{% highlight bash %}

{% endhighlight %}

![WPA CLI: ][086-set-network]

#### Running

{% highlight bash %}

{% endhighlight %}

![WPA CLI: ][087-sudo-wpa-nm]

-- -- --

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

After the `WPA Supplicant` in post arch setup.
We can go right away to `network manager` for easy to use in daily basis.

Consider continue reading [ [Wireless: Network Manager][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/05wli' %}

[ref-bandit]:       https://bandithijo.dev/blog/mudah-mengkonfigurasi-wifi-dengan-wpa_supplicant

[local-whats-next]: /system/2023/05/09/network-manager.html

[071-passphrase]:   {{ asset_path }}/071-wpa-passphrase.png
[072-pp-complete]:  {{ asset_path }}/072-wpa-complete.png
[072-pp-with-nmcli]:{{ asset_path }}/072-wpa-with-nmcli.png
[073-sh-sudo-pipe]: {{ asset_path }}/073-wpa-sh-sudo-pipe.png
[073-sudo-pipe-f]:  {{ asset_path }}/073-wpa-sudo-pipe-fail.png
[073-su-root-pipe]: {{ asset_path }}/073-wpa-su-root-pipe.png
[074-sysctl-enable]:{{ asset_path }}/074-systemctl-enable.png
[074-sysctl-start]: {{ asset_path }}/074-systemctl-start.png
[074-sysctl-status]:{{ asset_path }}/074-systemctl-status.png
[075-sudo-wpa-01]:  {{ asset_path }}/075-sudo-wpa-supplicant-01.png
[075-sudo-wpa-02]:  {{ asset_path }}/075-sudo-wpa-supplicant-02.png
[075-sudo-wpa-03]:  {{ asset_path }}/075-sudo-wpa-supplicant-03.png
[075-sudo-wpa-04]:  {{ asset_path }}/075-sudo-wpa-supplicant-04.png
[077-bat-config]:   {{ asset_path }}/077-bat-config.png
[077-wpa-cfg-user]: {{ asset_path }}/077-wpa-config-user.png
[077-wpa-cfg-root]: {{ asset_path }}/077-wpa-config-root.png

[081-sudo-wpa-cli]: {{ asset_path }}/081-sudo-wpa-cli.png
[081-sudo-wpa-clin]:{{ asset_path }}/081-sudo-wpa-cli-not.png
[082-scan]:         {{ asset_path }}/082-scan.png
[082-scan-result]:  {{ asset_path }}/082-scan-result.png
[083-add-network]:  {{ asset_path }}/083-add-network.png
[083-list-net-0]:   {{ asset_path }}/083-list-networks-0.png
[083-list-net-1]:   {{ asset_path }}/083-list-networks-1.png
[084-set-net-psk]:  {{ asset_path }}/084-set-network-psk.png
[084-set-net-ssid]: {{ asset_path }}/084-set-network-ssid.png
[085-enable-net-0]: {{ asset_path }}/085-enable-network-0.png
[085-select-net-0]: {{ asset_path }}/085-select-network-0.png
[085-select-net-1]: {{ asset_path }}/085-select-network-1.png
[086-disconnect]:   {{ asset_path }}/086-disconnect.png
[086-set-network]:  {{ asset_path }}/086-set-network-fail.png
[087-sudo-wpa-nm]:  {{ asset_path }}/087-sudo-wpa-cli-after-nm.png
