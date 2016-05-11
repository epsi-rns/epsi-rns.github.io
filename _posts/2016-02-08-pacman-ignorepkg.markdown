---
layout: post
title:  "Update Arch Linux without Unnecessary Bloated Package"
date:   2016-02-08 04:03:15 +0700
categories: package manager
tags: [arch, pacman, package manager]
---

Update Arch Linux without unnecessary bloated package.

I'm not sure if this long weekend is really a holiday, because I have so much to do. So I don't have hours of time just to update my computer. 

I also have limited bandwith. Actually my broadband connection is quite fast, but shared with my family. I don't want to hog all the bandwith.
 
But I hey, I'm still a geek, I have no life, and I would like to sync (update and upgrade) my computer regularly. Let's have some fun with /etc/pacman.conf. Open your favorite editor, e.g. nano or vim, and let's have alook at the 'IgnorePkg' option.

Before you do, you might want to know what size of package that your pacman (package manager) need to download with 'VerbosePkgLists' option.

{% highlight conf %}
# Misc options
VerbosePkgLists
{% endhighlight %}

Pacman command will show you the size of package like in the first picture below. In this case I typed shorter version here. These packages are always be sync'ed frequently. To frequent that they become annoying.

	extra/chromium               44.41 MiB     
	extra/libreoffice-fresh      89.31 MiB
	extra/libreoffice-fresh-sdk  22.07 MiB
	core/linux                   57.38 MiB
	core/linux-firmware          34.08 MiB
	extra/mariadb                13.82 MiB

	Total Download Size:    268.16 MiB

There is no need to sync 'em all right! Since we want to ignore it, add these line to pacman.conf as shown in second picture below. Do not forget to save it.

{% highlight conf %}
IgnorePkg   = linux
IgnorePkg   = linux-firmware
IgnorePkg   = firefox
IgnorePkg   = chromium
IgnorePkg   = opera
IgnorePkg   = mariadb
IgnorePkg   = libreoffice-fresh
IgnorePkg   = libreoffice-fresh-sdk
{% endhighlight %}

The result of 'pacman -Syu' is shown in the third picture.

	community/catfish        0.13 MiB
	extra/gnucash            4.56 MiB
	extra/libinput           0.08 MiB
	extra/libwnck3           0.37 MiB
	extra/ntp                1.71 MiB
	extra/protobuf-c         0.09 MiB
	community/stunnel        0.16 MiB

	Total Download Size:     7.09 MiB

Now, if there still to many package to be synchronized, you can limit pacman with these option in pacman.conf.

{% highlight conf %}
XferCommand = /usr/bin/curl --limit-rate 33k -C - -f %u > %o
XferCommand = /usr/bin/wget --limit-rate 33K --passive-ftp -c -O %o %u
{% endhighlight %}

![Pacman IgnorePkg]({{ site.url }}/assets/2016/02/pacman-ignorepkg.png)

Happy Sync.

