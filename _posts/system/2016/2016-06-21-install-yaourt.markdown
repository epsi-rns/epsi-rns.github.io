---
layout: post
title:  "Install Yaourt, the AUR Helper"
date      : 2016-06-21 05:27:15 +0700
categories: system
tags      : [package manager]
keywords  : [aur, yaourt]
author: epsi

opengraph:
  image: /assets/site/images/topics/arch.png

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

## Manjaro User

Yaourt comes pre-installed with Manjaro so you normally won't have to install it.

-- -- --

## Base Devel

Yaourt do automatic compilation of source code given from AUR.
Since we are dealing with development here, before you begin,
make sure you have <code>base-devel</code> installed.

$ sudo pacman -S base-devel

-- -- --

## The Package Query

AUR Helper need one required package as a dependency.
It is <code>package-query</code>

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

[![Yaourt Manual Compilation][image-yaourt-makepkg]{: .img-responsive }][photo-yaourt-makepkg]

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

[![YaourtFr Repository Sync][image-yaourtfr-sync]{: .img-responsive }][photo-yaourt-sync]

Do sync your repo, and install both

{% highlight bash %}
$ pacman -Syu
$ sudo pacman -S package-query yaourt
{% endhighlight %}

[![Yaourt Install Using Pacman][image-yaourt-pacman]{: .img-responsive }][photo-yaourt-pacman]

After both package installed,
you can safely remove the archlinuxfr section from <code class="code-file">/etc/pacman.conf</code>.

-- -- --

## Using yaourt

After yaourt installed,
you can use yaourt to query your package.

{% highlight bash %}
$ yaourt yaourt
{% endhighlight %}

You can see the result in image below

[![Yaourt Querying Yaourt Package][image-yaourt-yaourt]{: .img-responsive }][photo-yaourt-yaourt]

-- -- --

## Configuration

Yaourt comes with config file.
Just copy them to your home directory

{% highlight bash %}
$ cat /etc/yaourtrc
$ cp /etc/yaourtrc ~/.yaourtrc
{% endhighlight %}

I left my <code class="code-file">~/.yauortrc</code> config as below to avoid
too many confirmation question from yaourt.

{% highlight conf %}
# SUDO
SUDONOVERIF=1      # Avoid multiple sudo checks when timestamp_timeout=0

# Prompt
BUILD_NOCONFIRM=1  # Only prompt for editing files
EDITFILES=0

# Command
MAKEPKG="makepkg --skippgpcheck"
{% endhighlight %}

This configuration is very helpful
if you maintain your AUR upgrade regularly
with a lot of Syua option in yaourt.
This command will update all your AUR at once.

Make sure you know what you are doing when skipping PGP Verification.

{% highlight bash %}
$ yaourt -Syua
{% endhighlight %}

-- -- --

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/assets-system' %}

[image-yaourt-package-query]: {{ system_path }}/2016/06/yaourt-package-query.png
[image-yaourt-makepkg]: {{ system_path }}/2016/06/yaourt-makepkg.png
[image-yaourt-pacman]:  {{ system_path }}/2016/06/yaourt-pacman-half.png
[image-yaourtfr-sync]:  {{ system_path }}/2016/06/yaourtfr-sync.png
[image-yaourt-yaourt]:  {{ system_path }}/2016/06/yaourt-yaourt.png

[photo-yaourt-makepkg]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOVqc1s0UzjD3vCFTQsH60Ixy2Wuqv0imn9G-My
[photo-yaourt-pacman]:  https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMtI5c045Eo7JdxrLeT_V6f4I_OL7QuCPp9Fj1G
[photo-yaourt-sync]:    https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMcfesFCPt3TXMTVR9ga0gmh7K0YWYnOwG_9Q-k
[photo-yaourt-yaourt]:  https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOS04CSeZ8i6hibmsqeaObz7O-sveNtsiolkbe1


