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
  And this Cantata MPD Client have a very nice looks.
  
---

## Introducing 2 KDE Applications

1. FatRat Downloader.
2. Cantata Player (MPD Client).

* * *

## Screenshot

{% capture ss_content %}
<strong>OS</strong>: Debian<br>
<strong>DE</strong>: Plasma<br>
<strong>WM</strong>: XMonad<br>
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

[![Introducing FatRat Cantata: debian+plasma+xmonad+cmake][image-ss-xmonad]{: .img-responsive }][photo-ss-xmonad]

* * *

### Installing FatRat in Debian 

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

### Installing Cantata in Debian

Surprisingly Cantata have a very nice looks.

[![Cantata mpd player have a very nice looks][image-ss-cantata]{: .img-responsive }][photo-ss-cantata]


{% highlight bash %}
># apt instal cantata
{% endhighlight %}


**Reading**:<br>
* [mpd.wikia.com/wiki/Client:Cantata][mpd-fatrat]

**Source Code**:<br>
* [github.com/cdrummond/cantata][github-fatrat]



[//]: <> ( -- -- -- links below -- -- -- )


[site-fatrat]: http://fatrat.dolezel.info/
[mpd-fatrat]: http://mpd.wikia.com/wiki/Client:Cantata
[github-fatrat]: https://github.com/cdrummond/cantata
[image-ss-xmonad]: {{ site.url }}/assets/posts/opensource/2016/04/introducing-fatrat-cantata.png
[photo-ss-xmonad]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMuzaI_fAd5cqbp4Xsb1Sgcj8m97fcTu5Mz-vRC
[image-ss-cantata]: {{ site.url }}/assets/posts/opensource/2016/04/cantata.png
[photo-ss-cantata]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipN67dDllxbRB2X3tEsao5CWm1VzpiXk0qbWwECk
