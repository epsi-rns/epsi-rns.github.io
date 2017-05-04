---
layout: post-sidemenu-wm
title:  "Install Termite"
categories: desktop
date:   2016-09-19 20:15:15 +0700
tags: [ricing, install]
author: epsi

excerpt:
  Installing Termite in Arch based distribution is easy,
  the thing get tough when installing vte3-ng in Debian based.
  And installing termite require vte-ng as dependency.

---

Termite is my latest favorite terminal, it is simple,
it supports themable text colors in configuration. 
It is lightweight, looks good for ricing.
It is also very suitable in combination with tiling window manager.
It has mono Dependency, but that's okay for me.

Installing Termite in Arch based distribution
is easy since it is in community repository.

Installing Termite in Debian distribution require
manual compilation from git since it is not in official repository.
Manual compilation is not a hard thing to do.

But thing get tough when installing vte3-ng in Debian.
And installing termite require vte-ng as dependency.
There is no guidance about vte-ng compilation dependency.

-- -- --

### Sample Configuration

You can check my dotfiles here. 

* [Termite Configuration][source-termite]

I don't really write my own config and themes.
Actually I copied them form many sources. 

-- -- --

### Arch Install

Very simple. Termite is in community package. 
No need to touch any AUR. This will also install vte3-ng.

{% highlight bash %}
$ sudo pacman -S termite
{% endhighlight %}

This show bright theme

[![Termite Install Arch Bright][image-arch-bright]{: .img-responsive }][photo-arch-bright]

-- -- --

### Manjaro Install

Similar with Arch. 

{% highlight bash %}
$ sudo pacman -S termite
{% endhighlight %}

This show dark theme

[![Termite Install Manjaro Dark][image-manjaro-dark]{: .img-responsive }][photo-manjaro-dark]

-- -- --

### Install VTE-NG in Debian

In order to install termite in Debian, you need install vte-ng from git.

Since we desire to install manually from git,
we shall prepare the directory.

{% highlight bash %}
$ mkdir ~/git-src
$ cd ~/git-src
{% endhighlight %}


Before you compile vte-ng you should install required dependency.

{% highlight bash %}
$ sudo apt install gtk-doc-tools valac libgirepository1.0-dev /
  libgtk-3-dev libgnutls28-dev libxml2-utils gperf
{% endhighlight %}

-- -- --
  
After this you can safely run

{% highlight bash %}
$ git clone https://github.com/thestinger/vte-ng.git
{% endhighlight %}

![VTE-NG git clone][image-debian-vte-ng-git-clone]{: .img-responsive }

{% highlight bash %}
$ cd vte-ng
{% endhighlight %}

{% highlight bash %}
$ ./autogen.sh 
{% endhighlight %}

[![VTE-NG autogen][image-debian-vte-ng-autogen]{: .img-responsive }][photo-debian-vte-ng-autogen]

{% highlight bash %}
$ make 
{% endhighlight %}

[![VTE-NG make][image-debian-vte-ng-make]{: .img-responsive }][photo-debian-vte-ng-make]

{% highlight bash %}
$ sudo make install 
{% endhighlight %}

[![VTE-NG make install][image-debian-vte-ng-make-install]{: .img-responsive }][photo-debian-vte-ng-make-install]

-- -- --

### VTE-NG Dependency

Here is the detail of Dependency requirement

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
./autogen.sh: 11: ./autogen.sh: gtkdocize: not found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install gtk-doc-tools
{% endhighlight %}


-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
src/Makefile.am:205: error: ENABLE_VAPIGEN does not appear in AM_CONDITIONAL
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install valac
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
src/Makefile.am:178: error: HAVE_INTROSPECTION does not appear in AM_CONDITIONAL
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libgirepository1.0-dev
{% endhighlight %}

This will also install g++

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
No package 'gtk+-3.0' found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libgtk-3-dev
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
No package 'gnutls' found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libgnutls28-dev
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
configure: error: xmllint not found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libxml2-utils
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ make
../missing: 81: ../missing: gperf: not found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install gperf
{% endhighlight %}

-- -- --

### Install Termite in Debian

Let's get back to our git directory

{% highlight bash %}
$ cd ~/git-src
{% endhighlight %}

{% highlight bash %}
$ git clone --recursive https://github.com/thestinger/termite.git
{% endhighlight %}

![Termite git clone][image-debian-termite-git-clone]{: .img-responsive }

{% highlight bash %}
$ cd termite 
{% endhighlight %}

{% highlight bash %}
$ make 
{% endhighlight %}

[![Termite make][image-debian-termite-make]{: .img-responsive }][photo-debian-termite-make]

{% highlight bash %}
$ sudo make install
{% endhighlight %}

![Termite make][image-debian-termite-make-install]{: .img-responsive }

No need to install either libglib3.0-cil-dev or gnutls-bin

Let's test

{% highlight bash %}
$ termite -v
termite v11-33-g7a7021f
{% endhighlight %}

![Termite Version][image-debian-termite-version]{: .img-responsive }

-- -- --

### Issue with Debian

If you have trouble with xfce4-terminal, simply remove symbolic link, or any vte generated in /usr/local/lib .

-- -- --

I think that's all.


[//]: <> ( -- -- -- links below -- -- -- )

[source-termite]: https://github.com/epsi-rns/dotfiles/tree/master/config/termite

[image-arch-bright]: {{ site.url }}/assets/posts/desktop/2016/09/arch-termite-bright.png
[photo-arch-bright]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNQAwvxM9LVDwDIe2JrjlHcUwzxaf6j_f8gpaRj

[image-manjaro-dark]: {{ site.url }}/assets/posts/desktop/2016/09/manjaro-termite-dark.png
[photo-manjaro-dark]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipO97BRUu5lXJAVJBkP-XYVDISf9cD1FyoT63iva 

[image-debian-vte-ng-git-clone]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-git-clone.png

[image-debian-vte-ng-autogen]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-autogen-half.png
[photo-debian-vte-ng-autogen]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNXqGy4osnyvw-Prj0gEStK1t6abyUB2Uiwk7Ap

[image-debian-vte-ng-make]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-make-half.png
[photo-debian-vte-ng-make]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNps2MW3RDaF-0QNLebcP5F5Gi1IKNJKc670xHT

[image-debian-vte-ng-make-install]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-make-install-half.png
[photo-debian-vte-ng-make-install]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNVdlF_p1qDDXJmDgqM1gXRMf3G0feOR3jiGyP0

[image-debian-termite-git-clone]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-git-clone.png

[image-debian-termite-make]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-make-half.png
[photo-debian-termite-make]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPiEb2bQvPaeRfG8TGOw95JSUYzNuPhJRDwh6ce

[image-debian-termite-make-install]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-make-install.png

[image-debian-termite-version]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-version.png
