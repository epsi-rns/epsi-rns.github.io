---
layout: post
title:  "Install BlackArch as Repository"
date:   2014-04-27 14:50:15 +0700
categories: opensource
tags: [security,blackarch,package manager]
author: epsi
excerpt:
  There is no need to install BlackArch or ArchAssault as a full distribution.
  With any distribution utilized pacman, any of these can be instaled as a repository. 
  So you can have BlackArch, or BlackManjaro or ManjaroAssault or BlackAntergos.
---

There is no need to install BlackArch or ArchAssault as a full distribution.
With any distribution utilized pacman, any of these can be instaled as a repository. 
So you can have BlackArch, or BlackManjaro or ManjaroAssault or BlackAntergos.

Pacman has real advantages compared with APT.
You can't install Kali on top of Debian nor ubuntu, 
without messing the host OS.

-- -- --

## Using pacman

{% highlight bash %}
># pacman -S blackarch 
{% endhighlight %}

Now, I'm thinking about, scrapping my Kali Partition
and switch to Manjaro instead.


Official Site

* <https://blackarch.org/>


![BlackArch Keyring][image-ss-blackarch-keyring]
<br/><br/>

-- -- --

## Using blackman (from source)

I'm still having a hard time while doing blackman.

{% highlight bash %}
># blackman -g blackarch-unpacker
...
[-] CRITICAL: Package not found in repository 'README' 
{% endhighlight %}

![BlackMan issue][image-ss-blackman-issue]

This one has not been solved yet.


[image-ss-blackarch-keyring]: {{ site.url }}/assets/posts/opensource/2014/04/blackarch-keyring.png
[image-ss-blackman-issue]: {{ site.url }}/assets/posts/opensource/2014/04/blackman-issue.png

