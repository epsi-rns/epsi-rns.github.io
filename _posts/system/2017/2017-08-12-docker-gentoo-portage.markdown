---
layout: post
title: "Docker - Gentoo Portage"
date: 2017-08-12 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, gentoo]
author: epsi

excerpt:
  Docker flow for Gentoo Portage,
  from emerge-webrsync to manual rsync.
  (not so) first time using Gentoo experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Preface

I am so glad that finally I can learn Gentoo Portage using Docker.

You can read a common overview here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]

-- -- --

### Getting Started With Docker

Our first move is, of course attach docker process, as usual.

{% highlight bash %}
$ docker image list 
{% raw %}
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY              SIZE
gentoo/stage3-amd64     873MB
vbatts/slackware        86.7MB
voidlinux/voidlinux     202MB
kevinleptons/lfs-auto   753MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it gentoo/stage3-amd64
ca9efc06241d / # exit
{% endhighlight %}

{% highlight bash %}
$ docker ps -a 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
IMAGE                 NAMES               STATUS
gentoo/stage3-amd64   amazing_shirley     Exited (0) 24 seconds ago
vbatts/slackware      cranky_keller       Exited (0) 37 minutes ago
{% endraw %}
{% endhighlight %}

{% highlight bash %}
$ docker start amazing_shirley
amazing_shirley

$ docker attach amazing_shirley
ca9efc06241d / #
{% endhighlight %}

![Getting Started][image-ss-gentoo-docker]{: .img-responsive }

-- -- --

### Updating System with Webrsync

First thing to do in my mind is updating my system.

You can use <code>emerge -u</code> command.

{% highlight bash %}
ca9efc06241d / # emerge-webrsync
Fetching most recent snapshot ...
 * Latest snapshot date: 20170814
 * 
 * Approximate snapshot timestamp: 1502757900
 *        Current local timestamp: 1502757301
 * 
 * The current local timestamp is possibly identical to the
 * timestamp of the latest snapshot. In order to force sync, use
 * the --revert option or remove the timestamp file located at
 * '/usr/portage/metadata/timestamp.x'.
{% endhighlight %}

[![Docker emerge-webrsync][image-ss-emerge-webrsync]{: .img-responsive }][photo-ss-emerge-webrsync]

{% highlight bash %}
ca9efc06241d / # emerge -u system
Calculating dependencies... done!
>>> Auto-cleaning packages...

>>> No outdated packages were found on your system.
{% endhighlight %}

{% highlight bash %}
ca9efc06241d / # emerge -u world
Calculating dependencies... done!
>>> Auto-cleaning packages...

>>> No outdated packages were found on your system.
{% endhighlight %}

{% highlight bash %}
ca9efc06241d / # eselect news read
No news is good news.
{% endhighlight %}

![Docker emerge --update][image-ss-emerge-update]{: .img-responsive }

-- -- --

### Manually Updating System

As a beginner, I always curious how things works.

Consider a fresh clean gentoo container.

{% highlight bash %}
$ emerge
!!! Section 'gentoo' in repos.conf has location attribute set to nonexistent directory: '/usr/portage'
!!! Section 'x-portage' in repos.conf has location attribute set to nonexistent directory: '/usr/portage'
!!! Invalid Repository Location (not a dir): '/usr/portage'


!!! /etc/portage/make.profile is not a symlink and will probably prevent most merges.
!!! It should point into a profile within /usr/portage/profiles/
!!! (You can safely ignore this message when syncing. It's harmless.)


!!! Your current profile is invalid. If you have just changed your profile
!!! configuration, you should revert back to the previous configuration.
!!! Allowed actions are limited to --help, --info, --search, --sync, and
!!! --version.
{% endhighlight %}

You can manually populate portage tree.

{% highlight bash %}
$ cd /usr/
$ wget -c http://distfiles.gentoo.org/snapshots/portage-latest.tar.xz
$ tar -xvf portage-latest.tar.xz
$ rm portage-latest.tar.xz
{% endhighlight %}

-- -- --

### Updating More

There are other command such as <code>emerge --sync</code>
Read the fine manual instead.

{% highlight bash %}
$ man emerge
{% endhighlight %}

-- -- --

### Install Package

What good is it life without our favorites packages ?
Consider install it at the first place after system update.

{% highlight bash %}
$ emerge htop fish ncdu
{% endhighlight %}

With <code>emerge</code> command,
these packages will be compiled, built and installed.
<code>emerge</code> has a very nice looks, 
many colors while building package.

[![Docker emerge Install Package][image-ss-emerge-install]{: .img-responsive }][photo-ss-emerge-install]

#### Ambiguous Package

How about this ?

{% highlight bash %}
$ emerge mc
{% endhighlight %}

![ebuild name is ambiguous][image-ss-emerge-ambigous]{: .img-responsive }

Use fully qualified ebuild name instead.

{% highlight bash %}
$ emerge app-misc/mc 
{% endhighlight %}

-- -- --

### Conclusion

No comment yet.
Just like everything else I have to go deeper to comprehend.

	I am a n00bs !

I just feel that I'm not afraid to use emerge.
Portage is not scary at all.

-- -- --

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-gentoo-docker]:    {{ asset_path }}/docker-gentoo-0-start.png

[image-ss-emerge-webrsync]:  {{ asset_path }}/docker-gentoo-1-webrsync-half.png
[photo-ss-emerge-webrsync]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipN4az7eFD30LhWON67bxjIl07QM5l2DlmxjGSGX?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-emerge-update]:    {{ asset_path }}/docker-gentoo-1-emerge-update.png

[image-ss-emerge-install]:   {{ asset_path }}/docker-gentoo-2-emerge-htop-half.png
[photo-ss-emerge-install]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMZZ4_7ak9JzR1bzgSdjvMft3xCQ5hd8CAZlWjS?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-emerge-ambigous]:  {{ asset_path }}/docker-gentoo-2-emerge-mc.png
