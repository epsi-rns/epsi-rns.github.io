---
layout: post
title: "dbus in Window Manager"
date      : 2018-01-18 09:45:15 +0700
categories: system
tags      : [etc]
keywords  : [window manager, dbus, session, non-systemd]
author: epsi

opengraph:
  image: /assets/site/images/topics/tux.png

excerpt:
  Solving dbus session in non-systemd environment,
  in application level, session level, and system wide.

---

<a name="preface"></a>

### Preface:

> Goal: Solving dbus session in non-systemd environment

#### Table of Content

* [Preface](#preface): Table of Content

* [Issue in Figure](#issue)

* 1: [Application Level](#application)

* 2: [Window Manager Session](#wm-session)

* 3: [Init 3: Using startx](#startx)

* 4: [Init 4: Using Desktop Manager](#desktop-manager)

* 5: [Session](#session)

* [Credits](#credits)

* [Conclusion](#conclusion)

-- -- --

<a name="issue"></a>

### Issue in Figure

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

<a name="application"></a>

### 1: Application Level

All you need is to type <code>dbus-launch</code>.

{% highlight bash %}
% dbus-launch thunar &
{% endhighlight %}

![dbus-launch: rofi][image-ss-rofi]{: .img-responsive }

But typing repetitively over and over again, could be cumbersome.

-- -- --

<a name="wm-session"></a>

### 2: Window Manager Session

Another temporary workaround is to mess <code>*.desktop</code> files,
that is located in <code>/usr/share/xsessions/</code>,
by adding <code>dbus-launch</code> in <code>Exec</code> line.

![dbus-launch: *.desktop][image-ss-desktop]{: .img-responsive }

This is also not the best practice,
since I have to edit every Window Manager after installation procedure.

-- -- --

<a name="startx"></a>

### 3: Init 3: Using startx

dbus run without any issue in this init.
I can run <code>startx</code> in <code>init 3</code> after login.
And voila, the mighty thunar will show my storage device.

In Slackware you can use <code>xwmconfig</code>,
that will create <code>.xsession</code> in home directory.

	Do not forget to remove .xsession if you want to use init 4

Here is an example of <code>xinitrc</code>.
It also has <code>dbus-launch</code> in it.

![dbus: xinitrc][image-ss-xinitrc]{: .img-responsive }

No need to alter anything.
Everything works well here.

-- -- --

<a name="desktop-manager"></a>

### 4: Init 4: Using Desktop Manager

This is where the most issue happened.
The solution depends on the Desktop Manager.

Intead of different DM, I also give different tiers [DM - distro - init]:

* LightDM -- Slackware - SysVinit. 

* SDDM - Artix - OpenRC

* SLiM - Devuan - SysVinit

Before you begin, make sure dbus service is actively running.

#### LightDM on Slackware

Activate the messagebus daemon first.

{% highlight bash %}
% sudo chmod +x /etc/rc.d/rc.messagebus

% sudo /etc/rc.d/rc.messagebus start
Starting system message bus:  /usr/bin/dbus-uuidgen --ensure ; /usr/bin/dbus-daemon --system

% sudo /etc/rc.d/rc.messagebus status
System dbus-daemon is running.
{% endhighlight %}

![dbus: messagebus daemon][image-ss-messagebus]{: .img-responsive }

Now we can simply use LightDM's Xsession wrapper,
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

![dbus: sddm Xsession wrapper][image-ss-sddm]{: .img-responsive }

I'm using SDDM - Artix - OpenRC.

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

Unless you do not turn off the dbus daemon,
your dbus will be fine.

{% highlight bash %}
$ sudo service dbus status
[ ok ] dbus is running.
{% endhighlight %}

-- -- --

<a name="session"></a>

### 5: Session

If you have policy issue with message 'not authorized to perform'
while mounting device form Thunar,
you can run <code>lxsession</code> from terminal to enable it.

![policy: not authorized to perform][image-ss-notauthorized]{: .img-responsive }

-- -- --

<a name="credits"></a>

### Credits

Thank you to folks at Slackware Indonesia Telegram Group,
for being a good company while solving these dbus issues.

> Your ideas pointing to right direction.

-- -- --

<a name="conclusion"></a>

### Conclusion

That's all

> It works.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/assets-system' %}

[image-ss-desktop]:     {{ system_path }}/2018/01/dbus-launch-herbstluftwm.png
[image-ss-lightdm]:     {{ system_path }}/2018/01/dbus-lightdm-wrapper.png
[image-ss-sddm]:        {{ system_path }}/2018/01/dbus-sddm-wrapper.png
[image-ss-rofi]:        {{ system_path }}/2018/01/dbus-rofi-dbus-launch.png
[image-ss-thunar-none]: {{ system_path }}/2018/01/dbus-thunar-no-dbus.png
[image-ss-thunar-with]: {{ system_path }}/2018/01/dbus-thunar-with-dbus.png
[image-ss-xinitrc]:     {{ system_path }}/2018/01/dbus-xinitrc.png
[image-ss-messagebus]:  {{ system_path }}/2018/01/dbus-messagebus.png
[image-ss-notauthorized]: {{ system_path }}/2018/01/not-authorized-to-perform.png
