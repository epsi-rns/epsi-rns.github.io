---
layout: post
title:  "Fluxbox Config - Menu"
categories: desktop
date:   2018-04-04 09:25:15 +0700
tags: [fluxbox]
author: epsi

excerpt:
  A brief explanation about Fluxbox menu configuration.

---

{% include post/2018/04/toc-fluxbox.html %}

### Config: Menu

> Goal: Simple Example of Custom Fluxbox Menu Configuration

#### Syntax

The syntax is:

{% highlight conf %}
[exec] (Label) {command} < optional path to .xpm icon >
{% endhighlight %}

#### Default

This is the default fluxbox menu in openSUSE.
You can find it in <code>~/.fluxbox/menu</code> file.

{% highlight conf %}
# Version 0.2   07.12.2004 - hvogel@hennevogel.de

[begin] (Fluxbox Menu)
    [exec] (xterm) {xterm}
    [include] (~/.fluxbox/menu.xdg)
        [submenu] (Fluxbox Configuration) {}
                [config] (Config)
                [workspaces] (Workspace)
                [submenu] (System-Styles) {Choose a style...}
                        [stylesdir] (/usr/share/fluxbox/styles)
                [end]
                [submenu] (User-Styles) {Choose a style...}
                        [stylesdir] (~/.fluxbox/styles)
                [end]
        [end]
    [exec] (Run Command) {fbrun}
    [exec] (Lock Screen) {xlock}
    [restart] (Restart) {}
    [exit] (Logout)
[end]
{% endhighlight %}

![fluxbox Feature: Default Vanilla Menu][image-ss-menu-default]{: .img-responsive }

#### Source

*	[github.com/.../dotfiles/.../menu][dotfiles-menu]

We will learn to customize later.

#### Window Menu

There are other menu, used for window context,
<code>~/.fluxbox/windowmenu</code> file.
The window menu is actually customizable.

*	[fluxbox-wiki.org/.../Editing_the_windowmenu.html](http://fluxbox-wiki.org/category/howtos/en/Editing_the_windowmenu.html)

![fluxbox Feature: Window Menu][image-ss-windowmenu]{: .img-responsive }

#### Other Distribution

This is the default fluxbox menu in Fedora.
You can find it in <code>~/.fluxbox/menu</code> file.

{% highlight conf %}
[begin] (Fluxbox-1.3.7)
[encoding] {UTF-8}
      [exec] (xterm) {xterm}
      [exec] (firefox) {}
[submenu] (Fluxbox menu)
      [config] (Configure)
[submenu] (System Styles) {Choose a style...}
      [stylesdir] (/usr/share/fluxbox/styles)
[end]
[submenu] (User Styles) {Choose a style...}
      [stylesdir] (~/.fluxbox/styles)
[end]
      [workspaces] (Workspace List)
      [commanddialog] (Fluxbox Command)
      [reconfig] (Reload config)
      [restart] (Restart)
      [exec] (About) {(fluxbox -v; fluxbox -info | sed 1d) | xmessage -file - -center}
      [separator]
      [exit] (Exit)
[end]
[endencoding]
[end]
{% endhighlight %}

#### Include

This is what I found in my Debian, as an example.

{% highlight conf %}
[begin] (fluxbox)
[include] (/etc/X11/fluxbox/fluxbox-menu)
[end]
{% endhighlight %}

On other distribution, I prefer creating XDG menu.

-- -- --

### XDG Menu

We can create xdg menu for fluxbox using this command:

{% highlight bash %}
$ xdg_menu --format fluxbox  --root-menu /etc/xdg/menus/applications.menu > ~/.fluxbox/menu.xdg
{% endhighlight %}

*	[wiki.archlinux.org/index.php/Xdg-menu](https://wiki.archlinux.org/index.php/Xdg-menu#FluxBox)

Depend on your setup,
the result of <code>~/.fluxbox/menu.xdg</code> would looks similar like this one:

{% highlight conf %}
[submenu] (SUSE Menu)
 [submenu] (Development)
  [submenu] (Debugger)
      [exec] (Qt 5 D-Bus Viewer) {/usr/bin/qdbusviewer-qt5}
  [end] # (Debugger)
  ...
 [end] # (Development)
 ...
[end] # (SUSE Menu)
{% endhighlight %}

Now you can see the openSUSE's XDG menu.

![fluxbox Feature: openSUSE XDG Menu][image-ss-menu-xdg-suse]{: .img-responsive }

#### Source

*	[github.com/.../dotfiles/.../menu.opensuse.xdg][dotfiles-xdg]

-- -- --

### Custom Menu

I would rather create my own menu <code>~/.fluxbox/menu.favorites</code>,
based on my favorites application.

It is either the minimalist version.

{% highlight conf %}
[submenu] (Favorites)
    [exec] (URxvt) {urxvt}
    [exec] (XFCE4 Terminal) {xfce4-terminal}
    [separator]
    [exec] (Geany) {geany}
    [exec] (Thunar ) {thunar}
    [exec] (Firefox) {firefox}
    [exec] (Thunderbird) {thunderbird}
    [separator]
    [exec] (Inkscape) {inkscape}
    [exec] (GIMP) {gimp}
    [exec] (Clementine) {clementine}
    [exec] (scrot -d 5) {scrot -d 5}
[end] # (Favorites)
{% endhighlight %}

Or a more structured one:

{% highlight conf %}
[submenu] (Favorites)
    [submenu] (Terminal) {}
        [exec] (URxvt) {urxvt}
        [exec] (XFCE4 Terminal) {xfce4-terminal}
    [end]
    [submenu] (File Manager) {}
        [exec] (Thunar ) {thunar}
        [exec] (PCMan FM) {pcmanfm-qt}
    [end]
    [submenu] (Internet) {}
        [exec] (Firefox) {firefox}
        [exec] (Chromium) {chromium}
        [exec] (Midori) {midori}
        [exec] (Thunderbird) {thunderbird}
        [exec] (Transmission) {transmission-qt}
    [end]
    [submenu] (Graphics and Media) {}
        [exec] (Inkscape) {inkscape}
        [exec] (GIMP) {gimp}
        [separator]
        [exec] (VLC) {vlc}
        [exec] (Clementine) {clementine}
    [end]
    [exec] (Libreoffice) {libreoffice}
    [exec] (Geany) {geany}
    [exec] (scrot -d 5) {scrot -d 5}
[end] # (Favorites)
{% endhighlight %}

![fluxbox Feature: Custom Favorites Menu][image-ss-menu-favorites]{: .img-responsive }

And the main menu <code>~/.fluxbox/menu</code> would be:

{% highlight conf %}
[begin] (Fluxbox Menu)
    [include] (~/.fluxbox/menu.xdg)
    [include] (~/.fluxbox/menu.favorites)
    [separator]
    [submenu] (Fluxbox Configuration) {}
            [config] (Config)
            [workspaces] (Workspace)
            [submenu] (System-Styles) {Choose a style...}
                    [stylesdir] (/usr/share/fluxbox/styles)
            [end]
            [submenu] (User-Styles) {Choose a style...}
                    [stylesdir] (~/.fluxbox/styles)
            [end]
    [end]
    [submenu] (Window Manager) {}
            [exec] (Run Command) {fbrun}
            [exec] (Lock Screen) {xlock}
            # [reconfig] (Reload config)
            [exec] (Shutdown) {/usr/bin/sudo /sbin/shutdown -h now} <>
            [restart] (Restart) {}
    [end]
    [exit] (Logout)
[end]
{% endhighlight %}

#### Source

*	[github.com/.../dotfiles/.../menu.favorites][dotfiles-fav]

-- -- --

### Generated Menu

There are also built in tools to generate menu,
based on application installed on your system.

{% highlight conf %}
$ fluxbox-generate_menu
{% endhighlight %}

-- -- --

### What's Next

We are done with configuration.
Consider having fun with theme by continue reading [ [Theme: Inkscape Part][local-part-style] ]



[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/fluxbox/config' %}

[dotfiles-tutor]:  {{ dotfiles }}
[dotfiles-menu]:   {{ dotfiles }}/menu
[dotfiles-xdg]:    {{ dotfiles }}/menu.opensuse.xdg
[dotfiles-fav]:    {{ dotfiles }}/menu.favorites

[local-overview]:    /desktop/2018/03/20/xfwm4-theme.html
[local-part-style]:  /desktop/2018/04/06/fluxbox-style.html

[image-ss-menu-default]:   {{ asset_path }}/fluxbox-menu-default.png
[image-ss-windowmenu]:     {{ asset_path }}/fluxbox-windowmenu.png
[image-ss-menu-xdg-suse]:  {{ asset_path }}/fluxbox-menu-xdg-suse.png
[image-ss-menu-favorites]: {{ asset_path }}/fluxbox-menu-favorites.png


