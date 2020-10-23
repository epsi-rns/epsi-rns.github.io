---
layout     : post
title      : "Openbox Install - oblogout"
categories : desktop
date       : 2018-07-17 09:25:15 +0700
tags       : [openbox]
keywords   : [tutorial, install]
author     : epsi
toc        : toc/2018/05/toc-openbox-config.html

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Specific tools for openbox.
  Not really for idiots.
---

<a name="preface"></a>

### Overview

> Goal: Specific tools for openbox.

In oblogout this case, I will give an example in Fedora.
You can do it in other distribution as well, such as Debian or Gentoo.

But I will give different example for openSUSE

-- -- --

### Compile Step by Step

This is an example for any distribution that does not support oblogout package.
You have to compile it yourself. But do not worry. It is easy.

#### git clone

I'm using this fork

*	[github.com/Cloudef/oblogout-fork](https://github.com/Cloudef/oblogout-fork)

First I went to a directory that stored all my clone.
I have already had many repository in there.
You might have difeerent directory.

{% highlight bash %}
$ cd /media/Works/2018\ -\ Build/
{% endhighlight %}

Now it is a good time to clone.

{% highlight bash %}
$ git clone https://github.com/Cloudef/oblogout-fork
{% endhighlight %}

![fedora28 oblogout: git clone][image-ss-f28-git-clone]{: .img-responsive }

#### python-distutils-extra

Most distro require this <code>python-distutils-extra</code>.
Because we need to run <code>setup.py</code> later on.

{% highlight bash %}
$ sudo dnf install python-distutils-extra
{% endhighlight %}

![fedora28 oblogout: distutils][image-ss-f28-distutils]{: .img-responsive }

#### setup.py

{% highlight bash %}
$ cd /media/Works/2018\ -\ Build/oblogout-fork
$ sudo ./setup.py install
{% endhighlight %}

![fedora28 oblogout: setup-py][image-ss-f28-setup-py]{: .img-responsive }

Now you can run from anywhere you like in your OS.

#### oblogout command

Running oblogout is easy.

{% highlight bash %}
$ oblogout &
{% endhighlight %}

![fedora28 oblogout: command][image-ss-f28-command]{: .img-responsive }

#### oblogout preview

And this is the preview.
You may click to see original resolution.

![fedora28 oblogout: fullscreen preview][image-ss-f28-preview]{: .img-responsive }

-- -- --

### openSUSE

How about distribution that support oblogout package.
Here an example, since I'm using openSUSE 15.

{% highlight bash %}
$ zypper addrepo https://download.opensuse.org/repositories/home:AndnoVember:test/openSUSE_Leap_15.0/home:AndnoVember:test.repo
$ zypper refresh
$ zypper install oblogout
{% endhighlight %}

#### addrepo

![openSUSE oblogout: zypper addrepo][image-ss-zypper-addrepo]{: .img-responsive }

#### refresh

![openSUSE oblogout: zypper refresh][image-ss-zypper-refresh]{: .img-responsive }

#### install

![openSUSE oblogout: install][image-ss-zypper-install]{: .img-responsive }

Now you can launch oblogout.

-- -- --

#### Debian

Just in case, there is no oblogout,
in your Debian/Ubuntu based distribution, 
compiling is the same.
Here is my screenshot.

{% highlight bash %}
$ sudo apt install python-distutils-extra
{% endhighlight %}

![debian oblogout: distutils][image-ss-debian-distutils]{: .img-responsive }

{% highlight bash %}
$ cd /media/Works/2018\ -\ Build/oblogout-fork
$ sudo ./setup.py install
{% endhighlight %}

![debian oblogout: setup-py][image-ss-debian-setup-py]{: .img-responsive }

Exactly the same steps with Fedora.

-- -- --

#### Gentoo

I just want to show my Gentoo screenshot.
I just can't help it.

{% highlight bash %}
$ emerge --ask python-distutils-extra
{% endhighlight %}

Yo may click the image below for more verbose compilation.

[![gentoo oblogout: distutils][image-ss-gentoo-distutils]{: .img-responsive }][photo-ss-gentoo-emerge]

{% highlight bash %}
$ sudo ./setup.py install
{% endhighlight %}

![gentoo oblogout: setup-py][image-ss-gentoo-setup-py]{: .img-responsive }

I really just can't help it.

-- -- --

<a name="whats-next"></a>

### What's Next

Consider continue reading [ [Install: obmenu-generator][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets-desktop/2018/07' %}

[local-part-config]:  /desktop/2018/07/18/openbox-install.html

[photo-ss-gentoo-emerge]:    https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNIJ3emTGQYMi-LpdndCLAsgOxbAbG6jyCEhjkT?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-f28-git-clone]:    {{ asset_path }}/03-fedora28-git-clone-oblogout-fork.png
[image-ss-f28-distutils]:    {{ asset_path }}/03-fedora28-dnf-install-python-distutils-extra.png
[image-ss-f28-setup-py]:     {{ asset_path }}/03-fedora28-setup-py-install.png
[image-ss-f28-command]:      {{ asset_path }}/03-fedora28-oblogout-command.png
[image-ss-f28-preview]:      {{ asset_path }}/03-fedora28-oblogout-fullscreen.png

[image-ss-zypper-addrepo]:   {{ asset_path }}/03-zypper-addrepo-oblogout.png
[image-ss-zypper-install]:   {{ asset_path }}/03-zypper-in-oblogout.png
[image-ss-zypper-refresh]:   {{ asset_path }}/03-zypper-refresh.png

[image-ss-debian-setup-py]:  {{ asset_path }}/03-debian-oblogout-setup-py-install.png
[image-ss-debian-distutils]: {{ asset_path }}/03-debian-apt-install-python-distutils-extra.png

[image-ss-gentoo-distutils]: {{ asset_path }}/03-gentoo-emerge-python-distutils-extra.png
[image-ss-gentoo-setup-py]:  {{ asset_path }}/03-gentoo-oblogout-setup-py-install.png

