---
layout: post
title:  "Openbox Menu - XDG Applications"
categories: desktop
date:   2018-05-09 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  Using openbox-menu script as Dynamic Openbox menu.

---

{% include post/2018/05/toc-openbox-config.html %}

### openbox-menu Script

> Goal: Using openbox-menu Script as Dynamic Openbox menu.

My friend, named Addy DCVLXVI, as always giving me a good suggestion.
<code>openbox-menu</code>, a very useful trick,
to make an openbox menu without sweating.

	Respect

#### XDG menus

If you haven't notice, there are these files 
in <code>/etc/xdg/menus</code>.

![openbox-menu: /etc/xdg/menus][image-ss-xdg-menus]{: .img-responsive }

We are going to utilize this.
For this example let use use this <code>lxde-applications.menu</code>.

#### Sub Menu

Create the <code class="code-file">~/.config/openbox/menu.applications.xml</code>.

{% highlight bash %}
$ openbox-menu lxde-applications.menu > ~/.config/openbox/menu.applications.xml
{% endhighlight %}

Sometime there are icon errors.
you can surpress with redirecting to <code>/dev/null</code>.

{% highlight bash %}
$ openbox-menu lxde-applications.menu > ~/.config/openbox/menu.applications.xml 2> /dev/null
{% endhighlight %}

![openbox-menu: pipe to file][image-ss-pipe-file]{: .img-responsive }

#### Main Menu

To create dynamic menu, we utilize script,
and use <code>menu execute=""</code>.

Modify the <code class="code-file">~/.config/openbox/menu.xml</code>.

{% highlight xml %}
    <menu execute="cat /home/epsi/.config/openbox/menu.applications.xml" 
        id="lxde-apps" label="LXDE Applications"/>
{% endhighlight %}

Consider reconfigure openbox. And have a look at the result.

{% highlight bash %}
$ openbox --reconfigure
{% endhighlight %}

![openbox-menu: lxde-applications.menu][image-ss-menu-lxde]{: .img-responsive }

Voila... It is so easy.

-- -- --

### Install openbox-menu

The downside of this method is, sometimes it is not easy to setup.
Debian based, and Gentoo play well with this <code>openbox-menu</code>.
Other than that, we might need to clone from repository.

#### Debian-based

Install in Debian-based is as easy as:

{% highlight bash %}
$ sudo apt install openbox-menu lxmenu-data
{% endhighlight %}

![openbox-menu: apt install][image-ss-apt-install]{: .img-responsive }

#### Gentoo

Install in Gentoo is as easy as:

{% highlight bash %}
$ emerge --ask openbox-menu lxmenu-data
{% endhighlight %}

![openbox-menu: emerge lxmenu-data][image-ss-emerge-lxmenu-data]{: .img-responsive }

Allright, now you can apply the <code>USE</code> by applying.

{% highlight bash %}
$ etc-update
{% endhighlight %}

And run it, once again.

{% highlight bash %}
$ emerge --ask openbox-menu lxmenu-data
{% endhighlight %}

![openbox-menu: emerge openbox-menu][image-ss-emerge-openbox-menu]{: .img-responsive }

#### Mercurial Clone

The official documentation is here:

*	[fabrice.thiroux.free.fr/openbox-menu_en.html](http://fabrice.thiroux.free.fr/openbox-menu_en.html)

We need to know where the source:

*	[bitbucket.org/fabriceT/openbox-menu](https://bitbucket.org/fabriceT/openbox-menu)

This is a <code>mercurial</code> repository.
Not a <code>git</code> repository.
And here is the magical command:

{% highlight bash %}
$ hg clone https://bitbucket.org/fabriceT/openbox-menu
{% endhighlight %}

![openbox-menu: hg clone (mercurial)][image-ss-hg-pull]{: .img-responsive }

#### make and make install

As usual common installation from source.

{% highlight bash %}
$ make
{% endhighlight %}

![openbox-menu: make][image-ss-make]{: .img-responsive }

{% highlight bash %}
$ sudo make install
{% endhighlight %}

![openbox-menu: sudo make install][image-ss-make-install]{: .img-responsive }

Now you can run from anywhere in your linux box.

{% highlight bash %}
$ openbox-menu
{% endhighlight %}

#### openSUSE Dependency

<code>lxmenu-data</code> is available in openSUSE repository

{% highlight bash %}
$ sudo zypper in lxmenu-data
{% endhighlight %}

In order to compile the source above properly,
you need to install the development package,
as the compile time dependency.

*	gtk2-devel

*	menu-cache-devel

{% highlight bash %}
$ sudo zypper install gtk2-devel
{% endhighlight %}

![openbox-menu: zypper gtk2-devel][image-ss-zypper-gtk2-devel]{: .img-responsive }

{% highlight bash %}
$ sudo zypper install menu-cache-devel
{% endhighlight %}

![openbox-menu: zypper menu-cache][image-ss-zypper-menu-cache]{: .img-responsive }

#### Fedora Dependency

<code>lxmenu-data</code> is also available in repository

{% highlight bash %}
$ sudo dnf install lxmenu-data
{% endhighlight %}

![openbox-menu: dnf gtk2-devel][image-ss-dnf-lxmenu-data]: .img-responsive }

In order to compile the source above properly,
you need to install the development package,
as the compile time dependency.

{% highlight bash %}
$ sudo dnf install gtk2-devel
{% endhighlight %}

![openbox-menu: dnf gtk2-devel][image-ss-dnf-gtk2-devel]{: .img-responsive }

{% highlight bash %}
$ sudo dnf install menu-cache-devel
{% endhighlight %}

![openbox-menu:dnf menu-cache][image-ss-dnf-menu-cache]{: .img-responsive }

-- -- --

### What's Next

Consider continue reading [ [Menu: Generator][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-bin]:   {{ dotfiles }}/bin/
[dotfiles-bunsenlabs]: {{ dotfiles }}/menu.bunsenlabs.xml
[dotfiles-pipemenus]:  {{ dotfiles }}/pipemenus.rc

[local-part-config]:  /desktop/2018/05/10/openbox-config.html

[image-ss-xdg-menus]:    {{ asset_path }}/om-opensuse-etc-xdg-menus.png
[image-ss-pipe-file]:    {{ asset_path }}/om-opensuse-pipe-to-file.png
[image-ss-menu-lxde]:    {{ asset_path }}/openbox-menu-lxde-applications.png

[image-ss-apt-install]:    {{ asset_path }}/omenu-apt-debian-install.png

[image-ss-emerge-lxmenu-data]:  {{ asset_path }}/om-emerge-lxmenu-data.png
[image-ss-emerge-openbox-menu]: {{ asset_path }}/om-emerge-openbox-menu.png

[image-ss-hg-pull]:        {{ asset_path }}/om-opensuse-hg-pull.png
[image-ss-make]:           {{ asset_path }}/om-opensuse-make.png
[image-ss-make-install]:   {{ asset_path }}/om-opensuse-make-install.png

[image-ss-zypper-gtk2-devel]: {{ asset_path }}/om-zypper-gtk2-devel.png
[image-ss-zypper-menu-cache]: {{ asset_path }}/om-zypper-menu-cache-devel.png

[image-ss-dnf-lxmenu-data]:   {{ asset_path }}/om-f28-dnf-lxmenu-data.png
[image-ss-dnf-gtk2-devel]:    {{ asset_path }}/om-f28-dnf-gtk2-devel.png
[image-ss-dnf-menu-cache]:    {{ asset_path }}/om-f28-dnf-menu-cache-devel.png
