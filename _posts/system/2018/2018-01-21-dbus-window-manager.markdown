---
layout: post
title: "dbus in Window Manager"
date: 2018-01-21 09:45:15 +0700
categories: system
tags: [etc]
author: epsi

excerpt:
  Solving dbus session in non-systemd environment,
  in application level, session level, and system wide.

---

### Preface: Issue in Figure

If you are moving to non-systemd environment,
you might notice that your window manager might behave differently.
I mean, there is no such different with Desktop Environment,
but Window Manager under Desktop Manager does not contain dbus session.
In short dbus session is not working under certain circumstances,

* no removable drive, 

* other partition not shown, 

* operation permitted for root only,

* and so on.

It will be easier to figure out the issue with figures.

#### Thunar without dbus

![thunar: no dbus][image-ss-thunar-none]{: .img-responsive }

#### Thunar with dbus

![thunar: with dbus][image-ss-thunar-with]{: .img-responsive }

#### Solution

There are at least three solutions to run dbus to suit your needs.

* Application level

* Window Manager Session

* System Wide, under Desktop Manager

	This is not really a desktop tutorial, but more like a system case.

-- -- --

### Application Level

All you need is to type <code>dbus-launch</code>.

{% highlight bash %}
% dbus-launch thunar &
{% endhighlight %}

![dbus-launch: rofi][image-ss-rofi]{: .img-responsive }

But typing repetitively over and over again, could be cumbersome.

-- -- --

### Window Manager Session

Another temporary workaround is to mess <code>*.desktop</code> files,
that is located in <code>/usr/share/xsessions/</code>,
by adding <code>dbus-launch</code> in <code>Exec</code> line.

![dbus-launch: *.desktop][image-ss-desktop]{: .img-responsive }

This is also not the best practice,
since I have to edit every Window Manager after installation procedure.

-- -- --

### Init 3: Using startx

dbus run without any issue in this init.
I can run <code>startx</code> in <code>init 3</code> after login.
And voila, the mighty thunar will show my storage device.

In Slackware you can use <code>xwmconfig</code>,
that will create <code>.xsession</code> in home directory.

Here is an example of <code>xinitrc</code>.
It also has <code>dbus-launch</code> in it.

![dbus: xinitrc][image-ss-xinitrc]{: .img-responsive }

No need to alter anything.
Everything works well here.

-- -- --

### Init 4: Using Desktop Manager

This is where the most issue happened.
The solution depends on the Desktop Manager.

Intead of different DM, I also give different tiers [DM - distro - init]:

* LightDM -- Slackware - SysVinit. 

* SDDM - Artix - OpenRC

* SLiM - Devuan - SysVinit

#### LightDM on Slackware

We can simply use LightDM's Xsession wrapper,
by altering the <code>/etc/lightdm/Xsession</code>,
and add <code>dbus-launch</code> in <code>exec</code> line
(the last line).

![dbus: lightdm Xsession wrapper][image-ss-lightdm]{: .img-responsive }

I'm using LightDM - Slackware - SysVinit. 

* [idnux lightdm](https://github.com/idnux/idnux_slackbuilds/tree/master/lightdm)

You can switch to <code>init 3</code> and switchback to <code>init 4</code>.

{% highlight bash %}
% init 3

% init 4

{% endhighlight %}

Or just kill the DM.

{% highlight bash %}
% pkill lightdm
{% endhighlight %}

#### SDDM on Artix

There is also similar file in SDDM,
in <code>/usr/share/sddm/scripts/Xsession</code>.

I'm using SDDM - Artix - OpenRC.

* [idnux lightdm](https://github.com/idnux/idnux_slackbuilds/tree/master/lightdm)

Restart using daemon manager.

{% highlight bash %}
% rc-service xdm restart
{% endhighlight %}

Note that this sddm run under <code>/etc/conf.d/xdm</code>

{% highlight bash %}
$ cat /etc/conf.d/xdm
# We always try and start X on a static VT.
CHECKVT=7

# What display manager do you use ? 
DISPLAYMANAGER="sddm"
{% endhighlight %}

Related Daemon

{% highlight bash %}
$ rc-service dbus status
 * status: started
{% endhighlight %}

-- -- --

#### SLiM (or XDM) on Devuan

There is not much to say with this Devuan.
dbus with WMs works properly without further configuration.

Unless you turn off the dbus daemon.
Your dbus will be fine.

{% highlight bash %}
$ sudo service dbus status
[ ok ] dbus is running.
{% endhighlight %}

-- -- --

### Credits

Thank you to folks at Slackware Indonesia Telegram Group,
for being a good company while solving these dbus issues.

	Your ideas pointing to right direction.

### Conclusion

That's all

	It works.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/01' %}

[image-ss-desktop]:     {{ asset_path }}/dbus-launch-herbstluftwm.png
[image-ss-lightdm]:     {{ asset_path }}/dbus-lightdm-wrapper.png
[image-ss-sddm]:        {{ asset_path }}/dbus-sddm-wrapper.png
[image-ss-rofi]:        {{ asset_path }}/dbus-rofi-dbus-launch.png
[image-ss-thunar-none]: {{ asset_path }}/dbus-thunar-no-dbus.png
[image-ss-thunar-with]: {{ asset_path }}/dbus-thunar-with-dbus.png
[image-ss-xinitrc]:     {{ asset_path }}/dbus-xinitrc.png
