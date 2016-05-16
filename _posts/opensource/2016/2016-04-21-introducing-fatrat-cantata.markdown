---
layout: post
title:  "Introducing FatRat and Cantata"
date:   2016-04-21 21:39:15 +0700
categories: opensource
tags: [app, kde]
author: epsi
excerpt:
  If you got bored with popular application,
  compiling and installing this two can be fun.
  FatRat is almost as cool as Transmission.
  And this Cantata MPD Client is actually have a very nice looks.
---

# Introducing 2 KDE Applications

1. FatRat Downloader.
2. Cantata Player (MPD Client).

* * *

# Screenshot

**OS**: Debian<br>
**DE**: Plasma<br>
**WM**: XMonad<br>

[![Introducing FatRat Cantata][image-ss-xmonad]][picasa-ss-xmonad]

* * *

## Installing FatRat in Debian 

{% highlight bash %}
 $ git clone git://git.dolezel.info/fatrat.git

 $ cd fatrat

 $ sudo apt install qtbase5-dev libqt5svg5-dev libboost-dev \
   libtorrent-rasterbar-dev libboost-thread-dev libqt5webkit5-dev

 $ cmake . -DWITH_BITTORRENT=ON -DWITH_SFTP=ON

 $ make

 $ sudo make install
{% endhighlight %}

**Reading**:<br>
* [fatrat.dolezel.info][site-fatrat]

**Source Code**:<br>
* Clone from git above

* * *

## Installing Cantata in Debian

{% highlight bash %}
># apt instal cantata
{% endhighlight %}

**Reading**:<br>
* [mpd.wikia.com/wiki/Client:Cantata][mpd-fatrat]

**Source Code**:<br>
* [github.com/cdrummond/cantata][github-fatrat]


[site-fatrat]: http://fatrat.dolezel.info/
[mpd-fatrat]: http://mpd.wikia.com/wiki/Client:Cantata
[github-fatrat]: https://github.com/cdrummond/cantata
[image-ss-xmonad]: {{ site.url }}/assets/posts/opensource/2016/04/introducing-fatrat-cantata.png
[picasa-ss-xmonad]: https://lh3.googleusercontent.com/-zzlpA_0blio/VzmdWMGIWwI/AAAAAAAAAMg/mC4uMoJdlWYJV376Nk3AT8CVFYOOnNeywCCo/s0/introducing-fatrat-cantata.png

