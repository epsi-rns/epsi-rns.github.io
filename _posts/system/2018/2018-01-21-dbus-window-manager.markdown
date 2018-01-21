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
In short dbus session is not working under certain circumstances,
no removable drive, other partition not shown, and so on.

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

#### LightDM

We can simply use LightDM's Xsession wrapper,
by altering the <code>/etc/lightdm/Xsession</code>,
and add <code>dbus-launch</code> in <code>exec</code> line
(the last line).

![dbus: lightdm Xsession wrapper][image-ss-lightdm]{: .img-responsive }

-- -- --

### Conclusion

That's all

	It works.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/01' %}

[image-ss-desktop]:     {{ asset_path }}/dbus-launch-herbstluftwm.png
[image-ss-lightdm]:     {{ asset_path }}/dbus-lightdm-wrapper.png
[image-ss-rofi]:        {{ asset_path }}/dbus-rofi-dbus-launch.png
[image-ss-thunar-none]: {{ asset_path }}/dbus-thunar-no-dbus.png
[image-ss-thunar-with]: {{ asset_path }}/dbus-thunar-with-dbus.png
[image-ss-xinitrc]:     {{ asset_path }}/dbus-xinitrc.png
