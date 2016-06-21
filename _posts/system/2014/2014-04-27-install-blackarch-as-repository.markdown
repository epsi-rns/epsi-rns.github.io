---
layout: post
title:  "How to Install BlackArch as Repository"
date:   2014-04-27 14:50:15 +0700
# published: false
categories: system
tags: [security, blackarch, package manager]
author: epsi
 
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

## Using pacman

{% highlight bash %}
># pacman -S blackarch 
{% endhighlight %}

Now, I'm thinking about, scrapping my Kali Partition
and switch to Manjaro instead.


Official Site

* <https://blackarch.org/>


[![BlackArch Keyring][image-ss-blackarch-keyring]{: .img-responsive }][picasa-ss-blackarch-keyring]
<br/><br/>

-- -- --

## Using blackman (from source)

I'm still having a hard time while doing blackman.

{% highlight bash %}
># blackman -g blackarch-unpacker
...
[-] CRITICAL: Package not found in repository 'README' 
{% endhighlight %}

[![BlackMan Issue][image-ss-blackman-issue]{: .img-responsive }][picasa-ss-blackman-issue]

This one has not been solved yet.

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-blackarch-keyring]: {{ site.url }}/assets/posts/system/2014/04/blackarch-keyring.png
[picasa-ss-blackarch-keyring]: https://lh3.googleusercontent.com/-vxylPl4aXfY/Vz2okEH1u_I/AAAAAAAAARs/oowH1cKidiIWOqz8_Fm1sL-J2Gk7PFQSwCCo/s0/blackarch-keyring-full.png
[image-ss-blackman-issue]: {{ site.url }}/assets/posts/system/2014/04/blackman-issue.png
[picasa-ss-blackman-issue]: https://lh3.googleusercontent.com/-mlwqCRJBFxQ/Vz2oluHQN6I/AAAAAAAAARs/LZnq4igNeX4V7E3LyY7e7miSs5yeWDV5ACCo/s0/blackman-issue.png
