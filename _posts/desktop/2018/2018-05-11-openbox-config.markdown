---
layout: post
title:  "Openbox Menu - Exit"
categories: desktop
date      : 2018-05-11 09:25:15 +0700
tags      : [openbox]
keywords  : [tutorial, configuration, menu]
author: epsi

opengraph:
  image: /assets/site/images/topics/openbox.png

excerpt:
  Openbox logging out menu for dummies.  

---

{% include post/2018/05/toc-openbox-config.html %}

### OB Menu Generator

> Goal: Explaining how to log out from openbox using menu.

-- -- --

### Basic

#### Sub menu systemd

Using systemd exit is as easy as:

{% highlight bash %}
$ systemctl suspend
$ systemctl hibernate
$ systemctl reboot
$ systemctl poweroff
{% endhighlight %}

And the respective openbox menu would be <code>menu.systemd.xml</code>.

{% highlight xml %}
<openbox_pipe_menu>
    <item label="Logout">
        <action name="Exit" />
    </item>
    <item label="Suspend">
        <action name="Execute"><execute>systemctl suspend</execute></action>
    </item>
    <item label="Hibernate">
        <action name="Execute"><execute>systemctl hibernate</execute></action>
    </item>
    <item label="Reboot">
        <action name="Execute"><execute>systemctl reboot</execute></action>
    </item>
    <item label="Shutdown">
        <action name="Execute"><execute>systemctl poweroff</execute></action>
    </item>
</openbox_pipe_menu>
{% endhighlight %}

I have test this in my Fedora, openSUSE and Debian.

*	[gitlab.com/.../dotfiles/.../menu.systemd.xml][dotfiles-menu-systemd-xml]

#### Sub menu other than systemd

This could be like this in other system (such as openrc).
But may vary depend on your setup.

{% highlight bash %}
$ pm-suspend
$ pm-hibernate
$ reboot
$ poweroff
{% endhighlight %}

It needs workaround to setup this in openbox menu.

#### Main Menu

And in mainmenu add this.

{% highlight xml %}
<?xml version="1.0" encoding="utf-8"?>
<openbox_menu xmlns="http://openbox.org/3.4/menu">
    <menu id="system-menu" label="System">
        ...
    </menu>
    <menu id="root-menu" label="Openbox 3">
        ...
        <menu id="system-menu"/>
        <separator/>
        <menu execute="cat /home/epsi/.config/openbox/menu.systemd.xml" 
            id="exit-menu" label="Exit" >
    </menu>
</openbox_menu>
{% endhighlight %}

The result is as simply as this one.

![openbox menu: exit menu][image-ss-menu-main]{: .img-responsive }

-- -- --

### Forcing Password

The issue with systemctl is, **any user can shutdown**.
There are some workaround,
such as using <code>gksu</code> or <code>sudo</code>.
Just edit your xml menu config.

#### Using gksu

{% highlight bash %}
$ gksu systemctl poweroff
{% endhighlight %}

![openbox menu: gksu][image-ss-exit-gksu]{: .img-responsive }

Now your menu could be

{% highlight xml %}
<openbox_pipe_menu>
    <item label="Logout">
        <action name="Exit" />
    </item>
    <item label="Suspend">
        <action name="Execute"><execute>gksu systemctl suspend</execute></action>
    </item>
    <item label="Hibernate">
        <action name="Execute"><execute>gksu systemctl hibernate</execute></action>
    </item>
    <item label="Reboot">
        <action name="Execute"><execute>gksu systemctl reboot</execute></action>
    </item>
    <item label="Shutdown">
        <action name="Execute"><execute>gksu systemctl poweroff</execute></action>
    </item>
</openbox_pipe_menu>
{% endhighlight %}

![openbox menu: gksu][image-ss-menu-gksu]{: .img-responsive }

#### Using urxvt sudo

{% highlight bash %}
$ urxvt -e sh -c 'sudo systemctl poweroff'
{% endhighlight %}

![openbox menu: urxvt sudo][image-ss-urxvt-sudo]{: .img-responsive }

#### Using xterm sudo

{% highlight bash %}
$ xterm -e 'sudo systemctl poweroff'
{% endhighlight %}

![openbox menu: xterm sudo][image-ss-xterm-sudo]{: .img-responsive }

#### Setting up sudoers

In order this <code>sudo</code> to work you,
first you must setup <code>/etc/sudoers</code>.

{% highlight conf %}
root ALL=(ALL) ALL
epsi ALL=(ALL) ALL
{% endhighlight %}

Or setup <code>wheel</code> group.

{% highlight conf %}
%wheel ALL=(ALL) ALL
{% endhighlight %}

Or in openSUSE, there is something like this:

{% highlight conf %}
Defaults targetpw
ALL   ALL=(ALL) ALL
{% endhighlight %}

-- -- --

### Forcing no Password

Now consider to see non-systemd again.

{% highlight bash %}
$ pm-suspend
$ pm-hibernate
$ reboot
$ poweroff
{% endhighlight %}

The respective openbox menu would be <code>menu.openrc.xml</code>.

{% highlight xml %}
<openbox_pipe_menu>
    <item label="Logout">
        <action name="Exit" />
    </item>
    <item label="Suspend">
        <action name="Execute"><execute>xterm -e sudo pm-suspend</execute></action>
    </item>
    <item label="Hibernate">
        <action name="Execute"><execute>xterm -e sudo pm-hibernate</execute></action>
    </item>
    <item label="Reboot">
        <action name="Execute"><execute>xterm -e sudo reboot</execute></action>
    </item>
    <item label="Shutdown">
        <action name="Execute"><execute>xterm -e sudo poweroff</execute></action>
    </item>
</openbox_pipe_menu>
{% endhighlight %}

![openbox menu: xml xterm sudo][image-ss-menu-xterm]{: .img-responsive }

And set in <code>/etc/sudoers</code>,
to make it behaves like systemctl.
Use no password.

{% highlight xml %}
%wheel ALL=(ALL) NOPASSWD: ALL
{% endhighlight %}

I have test this in my Gentoo.

*	[gitlab.com/.../dotfiles/.../menu.openrc.xml][dotfiles-menu-openrc-xml]

-- -- --

### Custom Dialog

There is, however, this old good trick from urukrama,
using gxmessage. gxmessage is not very common, and not available in all distro.

*	[urukrama.wordpress.com/.../confirm-to-shut-down/](https://urukrama.wordpress.com/2007/12/03/confirm-to-shut-down-reboot-or-log-out-in-openbox/)

Let me rewrite the good script for a modern days systemd style.

*	[gitlab.com/.../dotfiles/.../gxmessage-exit.sh][dotfiles-gxmessage-exit-sh]

{% highlight bash %}
#!/bin/bash

gxmessage "Are you sure you want to shut down your computer?" \
  -center -title "Take action" \
  -font "Sans bold 10" \
  -default "Cancel" \
  -buttons "_Cancel":1,"_Log out":2,"_Reboot":3,"_Shut down":4 \
  >/dev/null

case $? in
  1) echo "Exit";;
  2) openbox --exit;;
  3) systemctl reboot;;
  4) systemctl poweroff;;
esac
{% endhighlight %}

![openbox menu: gxmessage exit dialog][image-ss-gxmessage]{: .img-responsive }

Or the ugly one.

![openbox menu: xmessage exit dialog][image-ss-xmessage]{: .img-responsive }

-- -- --

### What's Next

We are almost finished with openbox configuration.

Consider continue reading [ [Openbox: Exit][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-menu-systemd-xml]:  {{ dotfiles }}/menu.systemd.xml
[dotfiles-menu-openrc-xml]:   {{ dotfiles }}/menu.openrc.xml
[dotfiles-gxmessage-exit-sh]: {{ dotfiles }}/gxmessage-exit.sh

[local-part-config]:  /desktop/2018/05/12/openbox-exit.html

[image-ss-menu-main]:    {{ asset_path }}/openbox-menu-exit-systemctl.png
[image-ss-exit-gksu]:    {{ asset_path }}/openbox-terminal-exit-gksu.png

[image-ss-urxvt-sudo]:   {{ asset_path }}/openbox-terminal-urxvt-sudo.png
[image-ss-xterm-sudo]:   {{ asset_path }}/openbox-terminal-xterm-sudo.png

[image-ss-menu-gksu]:    {{ asset_path }}/openbox-menu-exit-systemctl-xml-gksu.png
[image-ss-menu-xterm]:   {{ asset_path }}/openbox-menu-openrc-xterm.png

[image-ss-gxmessage]:    {{ asset_path }}/openbox-gxmessage.png
[image-ss-xmessage]:     {{ asset_path }}/openbox-xmessage.png
