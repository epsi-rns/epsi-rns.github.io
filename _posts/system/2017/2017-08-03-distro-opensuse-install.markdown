---
layout: post
title: "Distribution - openSUSE First Time Install"
date: 2017-08-03 09:45:15 +0700
categories: system
tags: [opensuse, distro, package manager]
author: epsi

excerpt:
  openSUSE Post Install Log.

---

### Preface

	Tumbleweed install, using USB Live Plasma, was succeed.

Still on July.
I finally get a chance to experience OpenSUSE. 
I choose the rolling release caled <code>Tumbleweed</code>.
And I plan to use it for few years in my Office.
This is my third Linux Partition after Debian and Fedora in my PC in my Office.

### First Impression

Surprisingly the installment was succeed. There are few things in my experience.

*	Each install, download a huge amount from the internet, about 4 Gigabytes.

*	My bad, I mistakenly install i586 version instead of x86_64 version.
	So I have to redownload the USB stick installer,
	and also download aout 4GB installation package.

*	My first install using i586, I skipped network configuration.
	And that confused me while I configure later.
	But it fixed using YaST while doing x86_64 install.

This openSUSE utilize <code>BTRFS</code> in <code>root /</code>,
and <code>XFS</code> at <code>/home</code>.
I'm very happy that I finally get my first non <code>ext4</code>,
but I haven't got any time to explore them yet.

[![Tumbleweed YaST Install][image-ss-yast-install]{: .img-responsive }][photo-ss-yast-install]

openSUSE installation using <code>YaST</code> as shown in figure above.
I haven't explore <code>YaST</code> yet.

One thing for sure while install.
No need to touch command line.

-- -- --

### Post Install

#### Repository Check

{% highlight bash %}
$ zypper lr -r
Repository priorities are without effect. All enabled repositories share the same priority.

# | Alias               | Name                        | Enabled | GPG Check | Refresh
--+---------------------+-----------------------------+---------+-----------+--------
1 | openSUSE-20170721-0 | openSUSE-20170721-0         | Yes     | (r ) Yes  | Yes    
2 | repo-debug          | openSUSE-Tumbleweed-Debug   | No      | ----      | ----   
3 | repo-non-oss        | openSUSE-Tumbleweed-Non-Oss | Yes     | (r ) Yes  | Yes    
4 | repo-source         | openSUSE-Tumbleweed-Source  | No      | ----      | ----   
5 | repo-update         | openSUSE-Tumbleweed-Update  | Yes     | (r ) Yes  | Yes   
{% endhighlight %}

#### Playing with Package

{% highlight bash %}
$ sudo zypper up
$ sudo zypper in clementine
$ sudo zypper in vlc
$ sudo zypper in mpv
$ sudo zypper in gstreamer-plugins-ugly
$ sudo zypper in ffmpeg

Codec not supported:
VLC could not decode the format "h264" (H264 - MPEG-4 AVC (part 10))
Codec not supported:
VLC could not decode the format "mp4a" (MPEG AAC Audio)
{% endhighlight %}

It seems to be that it need <code>packman</code> repository.

#### Enable Packman repository

{% highlight bash %}
$ sudo zypper ar -f -n packman http://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Tumbleweed/ packman
{% endhighlight %}

And finally get it installed

{% highlight bash %}
$ sudo zypper in vlc-codecs
$ sudo zypper in mplayer
$ sudo zypper in smplayer
$ sudo zypper install k3b-codecs ffmpeg lame phonon-backend-vlc phonon4qt5-backend-vlc vlc-codecs
{% endhighlight %}

#### More Package

{% highlight bash %}
$ sudo zypper in htop fish powerline git rfkill sddm inkscape
{% endhighlight %}

It all done successfully.

-- -- --

### Zypper

Zypper use abbreviation, to make the command shorter. 

*	repos (lr) 

*	install (in)

*	update (up)

*	dist-upgrade (dup)

For more information use the manual and type <code>/</code> to search.

{% highlight bash %}
$ man zypper
{% endhighlight %}

I'm doing <code>zypper dup</code> in tumbleweed as suggested from the community.
It seems interesting that zypper ask user about license.
I respect license, so I type yes.

{% highlight bash %}
$ sudo zypper dup
Warning: ...
...
Loading repository data...
Reading installed packages...
Computing distribution upgrade...

The following 26 NEW packages are going to be installed:
  ...

The following 75 applications are going to be REMOVED:
  ...
                                                                                                       
The following 8 packages are going to be REMOVED:                                                        
  ...
                                                                                                           
The following 653 packages are going to be upgraded:                                                       
  ...
{% endhighlight %}

This is a screenshot for anyone who curious about openSUSE. 
After about a week leaving tumbleweed installation in peace
and suddenly running <code>zypper dup</code>.

![Zypper Distribution Upgrade][image-ss-zypper-dup]{: .img-responsive }

-- -- -- 

### Conclusion

I finally happy using openSUSE.
Better late than never.

I must admit I know nothing about 
<code>YaST</code> nor <code>zypper</code> nor <code>BTRFS</code>.
I need time to experience these interesting stuff.

Thank you for Reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[image-ss-yast-install]:    {{ asset_path }}/opensuse-yast-install-i586.png
[photo-ss-yast-install]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNQ6K8rEv1WPht6zwJhInaGEifO_TC-eAJV3Fyk?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-zypper-dup]:      {{ asset_path }}/opensuse-zypper-dup.png
