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
`wpa_Supplicant` was the only backend for `NetworkManager`.
This is an old school tools, and we would explore this tools,
so we do not get confused when we need to use it later on.

#### Reference

There is better reference for you:

* [Mudah Mengkonfigurasi Wi-Fi dengan wpa_supplicant][ref-bandit]

-- -- --

### WPA Passphrase

-- -- --

### Running WPA Supplicant

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

After the `WPA Supplicant` in post arch setup.
We can go right away to `network manager` for easy to use in dailt basis.

Consider continue reading [ [Wireless: Network Manager][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/05wli' %}

[ref-bandit]:       https://bandithijo.dev/blog/mudah-mengkonfigurasi-wifi-dengan-wpa_supplicant

[local-whats-next]: /system/2023/05/09/network-manager.html
