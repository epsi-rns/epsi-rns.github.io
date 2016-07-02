---
layout: post
title:  "Install Yaourt, the AUR Helper"
date:   2016-06-21 05:27:15 +0700
categories: system
tags: [package manager]
author: epsi

excerpt:
  One of arch linux based advantage is access to AUR (Arch User Repository).
  To install AUR package we need tool, the AUR helpers.
  So how do we install an AUR helper, 
  in a clean system without AUR helper ?
  Manual install with makepkg is the answer.
  And there is also an alternative workaround.

related_link_ids:
  - 14122608  # Unbundling AUR Process 
  - 14040246  # Arch Install
  - 16020803  # Update Arch no Bloated  
  - 14042750  # BlackArch as Repository
  - 14122758  # Selectively BlackArch Tools" 

---

One of arch linux based advantage is access to AUR (Arch User Repository).
To install AUR package we need tool, the AUR helpers.

There are many AUR Helpers.
The most popular AUR helper is yaourt.
The problem is that, yaourt is not available in official package.
yoaurt itself is an AUR package.

So how do we install an AUR helper, without AUR helper ?
Manual install, makepkg is the way.

Reading:

* <https://archlinux.fr/yaourt-en>

* <https://github.com/archlinuxfr/yaourt/issues/209>

Let me quote from link above:

> The global consensus is that 
> Arch users should learn to build manually 
> with makepkg before using an AUR helper.

-- -- --

## Base Devel

Yaourt do automatic compilation of source code given from AUR.
Since we are dealing with development here, before you begin,
make sure you have base-devel installed.

$ sudo pacman -S base-devel

-- -- --

## The Package Query

AUR Helper need one required package as a dependency.
It is package-query

* <https://aur.archlinux.org/packages/package-query/>

You can either

Download the snapshot

{% highlight bash %}
$ wget -c https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz
$ tar -xzvf package-query.tar.gz 
{% endhighlight %}

or clone

{% highlight bash %}
$ git clone https://aur.archlinux.org/package-query.git
{% endhighlight %}

Then

{% highlight bash %}
$ cd package-query
$ makepkg -i
$ cd ..
{% endhighlight %}

Always RTFM

{% highlight bash %}
$ man package-query
{% endhighlight %}

Play with this tool

{% highlight bash %}
$ package-query -Ai package-query
{% endhighlight %}

![Package Query Command in Shell][image-yaourt-package-query]{: .img-responsive }

-- -- --

## The Yaourt

With the same procedure:

* <https://aur.archlinux.org/packages/yaourt/>

You can either

Download the snapshot

{% highlight bash %}
$ wget -c https://aur.archlinux.org/cgit/aur.git/snapshot/yaourt.tar.gz
$ tar -xzvf yaourt.tar.gz
{% endhighlight %}

or clone

{% highlight bash %}
$ git clone https://aur.archlinux.org/yaourt.git
{% endhighlight %}

Then

{% highlight bash %}
$ cd yaourt
$ makepkg -i
$ cd ..
{% endhighlight %}

[![Yaourt Manual Compilation][image-yaourt-makepkg]{: .img-responsive }][picasa-yaourt-makepkg]

Always RTFM

{% highlight bash %}
$ man yaourt
{% endhighlight %}

-- -- --

## Using pacman

There's another workaround if you don't want to do automatic compilation.

{% highlight bash %}
 $ sudo nano pacman.conf
{% endhighlight %}

and write these three lines.

{% highlight conf %}
[archlinuxfr]
SigLevel = PackageOptional
Server = http://repo.archlinux.fr/$arch
{% endhighlight %}

[![YaourtFr Repository Sync][image-yaourtfr-sync]{: .img-responsive }][picasa-yaourt-sync]

Do sync your repo, and install both

{% highlight bash %}
$ pacman -Syu
$ sudo pacman -S package-query yaourt
{% endhighlight %}

[![Yaourt Install Using Pacman][image-yaourt-pacman]{: .img-responsive }][picasa-yaourt-pacman]

After both package installed,
you can safely remove the archlinuxfr section from pacman.conf.

-- -- --

## Using yaourt

After yaourt installed,
you can use yaourt to query your package.

{% highlight bash %}
$ yaourt yaourt
{% endhighlight %}

You can see the result in image below

[![Yaourt Querying Yaourt Package][image-yaourt-yaourt]{: .img-responsive }][picasa-yaourt-yaourt]

-- -- --

## Configuration

Yaourt comes with config file.
Just copy them to your home directory

{% highlight bash %}
$ cat /etc/yaourtrc
$ cp /etc/yaourtrc ~/.yaourtrc
{% endhighlight %}

I left my yauort config as below to avoid
too many confirmation question from yaourt.

{% highlight conf %}
# SUDO
SUDONOVERIF=1      # Avoid multiple sudo checks when timestamp_timeout=0

# Prompt
BUILD_NOCONFIRM=1  # Only prompt for editing files
EDITFILES=0
{% endhighlight %}

This configuration is very helpful
if you maintain your AUR upgrade regularly
with a lot of Syua option in yaourt.
This command will update all your AUR at once.

{% highlight bash %}
$ yaourt -Syua
{% endhighlight %}

-- -- --

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

[image-yaourt-package-query]: {{ site.url }}/assets/posts/system/2016/06/yaourt-package-query.png
[image-yaourt-makepkg]: {{ site.url }}/assets/posts/system/2016/06/yaourt-makepkg.png
[image-yaourt-pacman]: {{ site.url }}/assets/posts/system/2016/06/yaourt-pacman-half.png
[image-yaourtfr-sync]: {{ site.url }}/assets/posts/system/2016/06/yaourtfr-sync.png
[image-yaourt-yaourt]: {{ site.url }}/assets/posts/system/2016/06/yaourt-yaourt.png

[picasa-yaourt-makepkg]: https://lh3.googleusercontent.com/-25HANVvRrqY/V2pLjzeDxHI/AAAAAAAAAW4/1_WsdVmXzUoXq6dJGZb-Hy-fk4aiSIG2gCCo/s0/yaourt-makepkg.png
[picasa-yaourt-pacman]: https://lh3.googleusercontent.com/-rJzch1p332w/V2pLjiN1eVI/AAAAAAAAAW4/99NGo2q-XZEfeawaRPebkmG3pQ9PhbFmACCo/s0/yaourt-pacman-full.png
[picasa-yaourt-sync]: https://lh3.googleusercontent.com/-kQEz48z02J4/V2pLlZ6MVVI/AAAAAAAAAW4/HSbsQ7OWAi8r053LdBQmcg9t2-3w1mH7QCCo/s0/yaourtfr-sync.png
[picasa-yaourt-yaourt]: https://lh3.googleusercontent.com/-mz3zNQtpRkw/V2pLkwdriqI/AAAAAAAAAW4/42xPu4hEauMS43wtWpscnjL49yID-7nTwCCo/s0/yaourt-yaourt.png

