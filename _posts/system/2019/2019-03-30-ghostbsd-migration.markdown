---
layout: post
title:  "GhostBSD - Migration from Linux"
date:   2019-03-30 09:45:15 +0700
categories: system
tags: [bsd, package manager]
author: epsi

excerpt:
  This article contain common issues,
  for migrating from linux to BSD.

---

### Desktop Customization

Masochist dekstop aka ricer would love this GhostBSD.
You just can copy paste your linux dotfiles, and most of all works,
just like you move dotfiles from one distribution to other notebook.

Any DE/WM run on slackware would run on GhostBSD.
It means DE with high dependency with systemd such as Pantheon,
would not run on GhostBSD.

#### Screenshot

GhostBSD + openbox.

![ghostbsd: screenfetch][image-ghosdtbsd-openbox]{: .img-responsive }

#### Device name

Of course you still have to change device name such as disk and wireless.

#### Terminal Issue

The only annoying thing is,
`reset` in terminal does not work in my HP notebook.
`clean` also do not clean any text.
So if you scroll, all the text is still there.
But it can be solved by typing this below.

{% highlight config %}
$ printf '\033\143'
{% endhighlight %}

The issue is solved by now.

-- -- --

#### Init

> Pefer nosystemd? just jump to BSD!

GhostBSD is using openRC.

-- -- --

### Directory name

GhostBSD use different path

* `home`, under `\usr\home`.
  Example: `/usr/home/epsi`.

* Every package outside the base system lay on `/usr/local/`.
  Example `/usr/local/bin`.
  So hypotethically, we can remove all stuff in `/usr/local/`,
  and start over without damaging base system.
  Although in real life `pkg` wooul still think that the package is still there.

-- -- --

### Discussion

Telegram is a good place to talk about BSD

#### English

* [TrueOS Community](https://t.me/TrueOSCommunity)

* [Project Trident](https://t.me/ProjectTrident)

* [GhostBSD](https://t.me/ghostbsd)

* [BSD](https://t.me/unitedbsd)

* [Lumina Desktop](https://t.me/luminadesktop)

#### Local (Indonesia)

* [Laskar Setan Merah](https://t.me/setanmerahID)

* [Free BSD Indonesia](https://t.me/freebsdid)

* [Pegel BSD](https://t.me/pegelbsd)

-- -- --

### What's Next ?

Thank you for reading this article.

Consider continue reading [ [GhostBSD - Ports][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/system/2019/03' %}

[local-part-config]:       /system/2019/04/02/ghostbsd-ports.html

[image-ghosdtbsd-openbox]:  {{ asset_path }}/neofetch-ghostbsd-fullscreen.png
