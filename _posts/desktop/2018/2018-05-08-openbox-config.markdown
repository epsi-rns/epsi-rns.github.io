---
layout: post
title:  "Openbox Menu - Dynamic"
categories: desktop
date      : 2018-05-08 09:25:15 +0700
tags      : [openbox]
keywords  : [tutorial, configuration, menu]
author: epsi

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Using Bunsenslabs Script as Dynamic Openbox menu.

---

{% include post/2018/05/toc-openbox-config.html %}

### BunsenLabs Script

> Goal: Using Bunsenslabs Script as Dynamic Openbox menu.

I'm not using BunsenLabs, but I found these script useful.

	Respect

#### Format

To create dynamic menu, we utilize script,
and use <code>menu execute=""</code>.

{% highlight xml %}
    <menu execute="/home/epsi/.config/openbox/bin/bl-tint2-pipemenu" 
        id="bl-tint2-pipemenu" label="Tint2"/>
{% endhighlight %}

#### Preparation

You need a file called <code class="code-file">bl-include.cfg</code>.

{% highlight bash %}
$ git clone https://github.com/BunsenLabs/bunsen-common
{% endhighlight %}

Put it under <code class="code-file">~/.config/openbox/bin</code>.

#### BunsenLabs or ArcoLinux

There are choices:

*	[github.com/BunsenLabs/bunsen-pipemenus](https://github.com/BunsenLabs/bunsen-pipemenus)

*	[github.com/arcolinux/arcolinux-pipemenus](https://github.com/arcolinux/arcolinux-pipemenus)

It is all, up to you. But in this tutorial, I prefer BunsenLabs.

#### Getting the Scripts

First clone this:

{% highlight bash %}
$ git clone https://github.com/BunsenLabs/bunsen-pipemenus
{% endhighlight %}

And copy the whole file from <code class="code-file">bin</code> directory,
under <code class="code-file">~/.config/openbox/bin</code>.

There will be a bunch of file:

*	bl-compositor, bl-conky-pipemenu, bl-dropbox-pipemenu, bl-graphics-pipemenu,
	bl-help-pipemenu, bl-kb-pipemenu, bl-libreoffice-pipemenu, bl-multimedia-pipemenu,
	bl-places-pipemenu, bl-printing-pipemenu, bl-recent-files-pipemenu,
	bl-remote-desktop-pipemenu, bl-sshconfig-pipemenu, bl-tint2-pipemenu,
	bl-x-www-browser-pipemenu.

#### Modify the Scripts

Open all the pipemenu scripts, and do a search and replace.

*	[gitlab.com/.../dotfiles/.../bin/][dotfiles-bin]

Change from

{% highlight bash %}
BL_COMMON_LIBDIR='/usr/lib/bunsen/common'
{% endhighlight %}

To

{% highlight bash %}
BL_COMMON_LIBDIR='/home/epsi/.config/openbox/bin/'
{% endhighlight %}

Do not forget to add executable mode for all scripts.

{% highlight bash %}
$ chmod +x /home/epsi/.config/openbox/bin/
{% endhighlight %}

#### Main Menu

Modify the <code class="code-file">~/.config/openbox/menu.xml</code>.

{% highlight bash %}
        ...
        <separator label="Dynamic Script"/>
        <menu execute="cat /home/epsi/.config/openbox/menu.bunsenlabs.xml" 
            id="bunsenlabs-menu" label="BunsenLabs"/>
        ...
{% endhighlight %}

#### Sub Menu

Create the <code class="code-file">~/.config/openbox/menu.bunsenlabs.xml</code>.

*	[gitlab.com/.../dotfiles/.../menu.bunsenlabs.xml][dotfiles-bunsenlabs]

{% highlight bash %}
<openbox_pipe_menu>
    <separator label="BunsenLabs Script"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-compositor" 
        id="bl-compositor" label="Compositor"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-conky-pipemenu" 
        id="bl-conky-pipemenu" label="Conky"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-dropbox-pipemenu" 
        id="bl-dropbox-pipemenu" label="Dropbox"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-graphics-pipemenu" 
        id="bl-graphics-pipemenu" label="Graphics"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-help-pipemenu" 
        id="bl-help-pipemenu" label="Help"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-kb-pipemenu" 
        id="bl-kb-pipemenu" label="Keybinds"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-libreoffice-pipemenu" 
        id="bl-libreoffice-pipemenu" label="LibreOffice"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-multimedia-pipemenu" 
        id="bl-multimedia-pipemenu" label="Multimedia"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-places-pipemenu" 
        id="bl-places-pipemenu" label="Places"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-printing-pipemenu" 
        id="bl-printing-pipemenu" label="Printing"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-recent-files-pipemenu" 
        id="bl-recent-files-pipemenu" label="Files"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-remote-desktop-pipemenu" 
        id="bl-remote-desktop-pipemenu" label="Remote Desktop"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-sshconfig-pipemenu" 
        id="bl-sshconfig-pipemenu" label="SSH"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-tint2-pipemenu" 
        id="bl-tint2-pipemenu" label="Tint2"/>
    <menu execute="/home/epsi/.config/openbox/bin/bl-x-www-browser-pipemenu" 
        id="bl-x-www-browser-pipemenu" label="Browser"/>
</openbox_pipe_menu>
{% endhighlight %}

Consider reconfigure openbox. And have a look at the result.

![openbox Menu: Bunsenlabs Dynamic Script][image-ss-menu-bunsenlabs]{: .img-responsive }

#### Additional Configuration

Some menu need a configuration file called
<code class="code-file">~/.config/openbox/pipemenus.rc</code>.

*	[gitlab.com/.../dotfiles/.../pipemenus.rc][dotfiles-pipemenus]

I grab this from a Kali user, and change a bit, as below.

{% highlight conf %}
# pipemenu.rc holds the applications which appear in the Openbox pipemenus
# Edit the arrays to add or remove package names, making sure there are spaces
# between each entry.

# Multimedia pipemenu (bl-multimedia-pipemenu)
MM_APPS=( 'vlc' 'audacious' 'smplayer' 'gnome-mplayer' 'mplayer' 'clementine')
MM_EDITORS=('mhwaveedit' 'audacity' 'openshot' )
MM_UTILS=('xfburn' 'gtk-recordmydesktop' )

# Graphics pipemenu (bl-graphics-pipemenu)
GRAPHICS_APPS=( 'mirage' 'gimp' 'blender' 'inkscape' )
GRAPHICS_SCROTS=('xfce4-screenshooter' 'scrot' 'spectacle')

# Browsers pipemenu (bl-x-www-browser-pipemenu)
BROWSERS=('chromium' 'iceweasel' 'midori' 'google-chrome-stable' 'opera' )
{% endhighlight %}

-- -- --

### What's Next

Consider continue reading [ [Menu: XDG Applications][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-bin]:   {{ dotfiles }}/bin/
[dotfiles-bunsenlabs]: {{ dotfiles }}/menu.bunsenlabs.xml
[dotfiles-pipemenus]:  {{ dotfiles }}/pipemenus.rc

[local-part-config]:  /desktop/2018/05/09/openbox-config.html

[image-ss-menu-bunsenlabs]:    {{ asset_path }}/openbox-menu-bl-scripts.png
