---
layout: post
title: "Distribution - Manjaro OpenRC Issues"
date: 2017-08-01 09:45:15 +0700
categories: system
tags: [debian, devuan, sysvinit, distro, package manager]
author: epsi

excerpt:
  Solving Manjaro Open RC Setup Issues.

---

### Preface

After almost a half year abandoning this Manjaro OpenRC in my partition.
I finally have time to face a few issues with my Manjaro OpenRC setup.
This issue are scattered, I mean there is no particular topic.
I just want to clean up bugs on my systems.
All are solved, except SDDM avatar.

These are just my personal notes. 
I mostly documenting note so I can find the solution later
whenever I have similar issue.

-- -- --

### 1. using elogind service 

<code>[solved]</code>

Issue: cannot halt or reboot from user

{% highlight bash %}
$ sudo loginctl 
Failed to list sessions: Launch helper exited with unknown return code 1
{% endhighlight %}

Problem Definition

*	elogind not started as service

Solution

{% highlight bash %}
$ sudo rc-update add elogind default
 * service elogind added to runlevel default
$ sudo rc-service elogind start
 * Starting elogind ... 
{% endhighlight %}

This note is also for any non-systemd user.
 
-- -- --

### 2. Network Manager in GUI failed

<code>[solved]</code>

Error message: "<code>no authorized to control networking</code>"

But nmcli works well in terminal.

{% highlight bash %}
$ nmcli dev wifi connect "myssid" password "mypassword"
{% endhighlight %}

Solution: use elogind in pam.d session

{% highlight bash %}
$ cat /etc/pam.d/system-login
...
session    optional   pam_elogind.so

$ cat /etc/pam.d/sddm-greeter
...
session		optional pam_elogind.so
{% endhighlight %}

This note is also for any non-systemd user.

-- -- --

### 3. Restart SDDM with OpenRC 

<code>[solved]</code>

{% highlight bash %}
$ sudo /etc/init.d/xdm start
{% endhighlight %}

instead of 

{% highlight bash %}
$ sudo rc-service sddm restart
{% endhighlight %}

This note is also for any non-systemd user.

-- -- --

### 4. SDDM Avatar not shown

<code>[not solved]</code>

Manjaro OpenRC is still using 0.14.

SDDM 0.14 released with the bug, and SDDM 0.14.1, which had the fix, never got released. 

[![Manjaro SDDM No Face][image-ss-manjaro-sddm]{: .img-responsive }][photo-ss-manjaro-sddm]

-- -- --

### 5. compton

<code>[solved]</code>

{% highlight bash %}
$ compton
glx_init(): No GLX extension.
{% endhighlight %}

As a temporary workaround I'm using xcompmgr instead.

Solution:

Upgrade xorg-server.

-- -- --

### 6. Managing gtktheme in i3 

<code>[solved]</code>

Use this one

{% highlight bash %}
$ lxappearance
{% endhighlight %}

-- -- --

### 7. HerbstluftWM Conky 

<code>[solved]</code>

It has "<code>shared memfd open() failed</code>" error messages.
It turn out that it comes from amixer.

If it appears. It is something related to the Linux kernel settings, 
which cannot be alternated inside the current running kernel. 

{% highlight bash %}
$ amixer get Master 
shared memfd open() failed: Function not implemented
Simple mixer control 'Master',0
  Capabilities: pvolume pvolume-joined pswitch pswitch-joined
  Playback channels: Mono
  Limits: Playback 0 - 64
  Mono: Playback 62 [97%] [-2.00dB] [on]
{% endhighlight %}
  
Solution

{% highlight bash %}
$ amixer get Master 2> /dev/null
{% endhighlight %}

-- -- --

### 8. Pacman Mirror Issue 

<code>[solved]</code>

I put it here, because of this reccuring problem is annoying.

{% highlight bash %}
$ sudo pacman-mirrors -g
{% endhighlight %}

-- -- --

### 9. No tty login 

<code>[solved]</code>

Solution: install xf86-video-fbdev

-- -- --

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[image-ss-manjaro-sddm]: {{ asset_path }}/manjaro-sddm-no-face.png
[photo-ss-manjaro-sddm]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM9ac3IrRfSiuq6nC7-kO-ogWYSUynaqPIA4iCD?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
