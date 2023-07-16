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