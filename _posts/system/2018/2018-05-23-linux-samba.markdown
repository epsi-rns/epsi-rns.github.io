---
layout: post
title:  "Linux Network - Samba"
date:   2018-05-23 09:25:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.

related_link_ids: 
  - 14010246  # Debian Install
  - 14040246  # Arch Install

---

### Overview

This is more like a network topic rather than a multiboot topic,
I gather this samba article here, because of my multiboot situation.


#### Samba Configuration

<code>smb.conf</code> for each distribution is available at:

* [github.com/epsi-rns/dotfiles/.../multiboot][dotfiles-multiboot]


-- -- --

### Basic Config

The basic <code>smb.conf</code> is similar.
Because I copy pastef from my Debian to other distribution.

	I made it that way

#### Debian, Fedora, openSUSE, KaOSx

{% highlight conf %}
[Samba]
   path = /media/Works/Samba/
   available = yes
   valid users = epsi
   read only = no
   browseable = yes
   public = yes
   writeable = yes
{% endhighlight %}




-- -- --

{% highlight conf %}
{% endhighlight %}

{% highlight conf %}
{% endhighlight %}


{% highlight conf %}
{% endhighlight %}


{% highlight conf %}
{% endhighlight %}


{% highlight conf %}
{% endhighlight %}


-- -- --

### What's next

Samba ?

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/multiboot/pc-01' %}

[basic-multiboot]: http://localhost:4000/system/2014/03/13/linux-multiboot.html

[dotfiles-smb-debian]:   {{ dotfiles }}/smb.debian.conf
[dotfiles-smb-fedora]:   {{ dotfiles }}/smb.fedora.conf
[dotfiles-smb-opensuse]: {{ dotfiles }}/smb.opensuse.conf
[dotfiles-smb-kaosx]:    {{ dotfiles }}/smb.kaosx.conf
