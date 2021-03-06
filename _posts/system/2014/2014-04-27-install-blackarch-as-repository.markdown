---
layout: post
title:  "How to Install BlackArch as Repository"
date      : 2014-04-27 14:50:15 +0700
# published: false
categories: system
tags      : [security, arch, package manager]
keywords  : [blackarch, install, pacman]
author: epsi

opengraph:
  image: /assets/site/images/topics/arch.png

excerpt:
  There is no need to install BlackArch or ArchAssault as a full distribution.
  With any distribution utilized pacman, any of these can be instaled as a repository. 
  So you can have BlackArch, or BlackManjaro or ManjaroAssault or BlackAntergos.

related_link_ids: 
  - 14040246  # Arch Install
  - 16020803  # Update Arch no Bloated  
  - 14122608  # Unbundling AUR
  - 14122758  # Selectively BlackArch Tools"
  
---

There is no need to install BlackArch or ArchAssault as a full distribution.
With any distribution utilized pacman, any of these can be instaled as a repository. 
So you can have BlackArch, or BlackManjaro or ManjaroAssault or BlackAntergos.

Pacman has real advantages compared with APT.
You can't install Kali on top of Debian nor ubuntu, 
without messing the host OS.

-- -- --

### Using pacman

{% highlight bash %}
># pacman -S blackarch 
{% endhighlight %}

Now, I'm thinking about, scrapping my Kali Partition
and switch to Manjaro instead.


Official Site

* <https://blackarch.org/>


[![BlackArch Keyring][image-ss-blackarch-keyring]{: .img-responsive }][photo-ss-blackarch-keyring]
<br/><br/>

-- -- --

### Using blackman (from source)

I'm still having a hard time while doing <code class="code-command">blackman</code>.

{% highlight bash %}
># blackman -g blackarch-unpacker
...
[-] CRITICAL: Package not found in repository 'README' 
{% endhighlight %}

[![BlackMan Issue][image-ss-blackman-issue]{: .img-responsive }][photo-ss-blackman-issue]

This one has not been solved yet.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/assets-system' %}

[image-ss-blackarch-keyring]:   {{ system_path }}/2014/04/blackarch-keyring.png
[photo-ss-blackarch-keyring]:   https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipP6OfE4IPS3HkwH5T_g5MYxm4j1o1-99DujKeWp
[image-ss-blackman-issue]:      {{ system_path }}/2014/04/blackman-issue.png
[photo-ss-blackman-issue]:      https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipMJQL9JAeZpIao4RJYSLTJLxjvrjevTrDPe5Riw
