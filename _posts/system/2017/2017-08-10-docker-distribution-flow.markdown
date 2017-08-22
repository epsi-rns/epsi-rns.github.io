---
layout: post
title: "Docker - Flow for Learning Linux Distribution"
date: 2017-08-10 09:45:15 +0700
categories: system
tags: [docker, distro, package manager]
author: epsi

excerpt:
  Docker flow for learning linux distribution,
  whether package management, or init.

related_link_ids: 
  - 16022800  # Learn LXC
  - 16030301  # Docker Manjaro
  - 16030900  # Docker Debian 

---

### Preface

> Goal: Learning Package Manager, Focus on Command Line Interface

	Heaven does exist !

I have been dreaming of having my own lab,
examining variety of package manager, using my own notebook.
Five years has past, yet still broken dreams I have. 
I cannot afford high end machine with virtual machine performance.
Like a **dream come true**, docker allow me to install,
other distribution without CPU burden.
I finally fulfill my destiny.

I can use Docker to learn package management such as 

*	Gentoo Portage

*	Slackware Slackbuild

*	Void Linux XBPS

*	LFS Compilation/ Toolchain

*	And five other, in this blog.

-- -- --

### Persistence Docker Process

With LXC, the state changed as we exit each image.
With Docker, each time we run image, docker spawn a new process.

	We need a persistence state that our distro
	keep the change we made after exiting the session.

This is why we should beware of the flow.
Docker is different with LXC.
Docker has image, and process.
Each image an be spawn into several container process.

-- -- --

### The Flow

Since we want a persistence distro,
we only need one process.
So here the step.

#### (1) Terminal 1: List all images.

If this is empty, you can pull for docker hub.

{% highlight bash %}
$ sudo watch docker image list
{% endhighlight %}

![Terminal 1][image-ss-term-1]{: .img-responsive }

#### (2) Terminal 2: List all processes.

This list could be emptyor has several processess.
You can delete all unnecessary processes,
and start with a clean process.

Note that you can show based on docker id, or docker names.

{% highlight bash %}
$ sudo watch docker  ps -as
{% endhighlight %}

or

{% highlight bash %}
{% raw %}
$ sudo docker ps -a --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
{% endhighlight %}

or

{% highlight bash %}
{% raw %}
$ sudo watch "docker ps -a --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}\t{{.Size}}'"
{% endraw %}
{% endhighlight %}

![Terminal 2][image-ss-term-2]{: .img-responsive }

You can add the size, so you can monitor how the process grow
as you work with your container.
But size take a longer calculation as a drawback.

#### (3) Terminal 3: Run an image to create process

Do use interactive.

{% highlight bash %}
$ sudo docker run -it vbatts/slackware
{% endhighlight %}

And exit from process. You can start later.

{% highlight bash %}
sh-4.3# exit
{% endhighlight %}

![Terminal 3][image-ss-term-3]{: .img-responsive }

A new process will be shown in process list
such as _inspiring_davinci_. 
Docker give random name automatically.


#### (4) Terminal 4: Attach process to your favorite terminal

{% highlight bash %}
$ sudo docker start inspiring_davinci
{% endhighlight %}

{% highlight bash %}
$ sudo docker attach inspiring_davinci
{% endhighlight %}

![Terminal 1][image-ss-term-4]{: .img-responsive }

Do not forget to press any character.
The docker wait for your response.

After you exit, you need to start the process again.

-- -- --

### Screenshot:

*	OS: Artix Linux

*	WM: Herbstluftwm

[![Docker All Screenshot][image-ss-term-all]{: .img-responsive }][photo-ss-term-all]: 

#### Related Screenshot

*	[Learn Different Package Management Using LXC][local-manjaro-lxc]

*	[Docker Demonstration (Manjaro), Running Multiple Guest OS][local-manjaro-docker]

*	[Running Guest OS via Docker on top of Debian for the first time][local-debian-lxc]

-- -- --

### Without sudo

To avoid typing sudo all the time,
add user in <code>/etc/group<code>.

{% highlight conf %}
docker:x:1000:epsi
{% endhighlight %}

Do not forget to logout, and relogin.

-- -- --

### Docker Collection

These are some docker containers, that I utilize in this blog.
For each I provide the command.
In addition the default <code>latest</code> tag,
you can use other tag such as <code>tumbleweed</code>, 
<code>rawhide</code>, <code>stretch</code>.

{% highlight conf %}
$ docker pull vbatts/slackware
$ docker run -it vbatts/slackware
{% endhighlight %}

{% highlight conf %}
$ docker pull gentoo/stage3-amd64
$ docker run -it gentoo/stage3-amd64
{% endhighlight %}

{% highlight conf %}
$ docker pull voidlinux/voidlinux
$ docker run -it voidlinux/voidlinux bash
{% endhighlight %}

{% highlight conf %}
$ docker pull kevinleptons/lfs-auto
$ docker run -it voidlinux/voidlinux bash
{% endhighlight %}

{% highlight conf %}
$ docker pull crux
$ docker run -it crux
{% endhighlight %}

![Docker Pull Crux Linux][image-ss-pull-crux]{: .img-responsive }

{% highlight conf %}
$ docker pull dock0/arch
$ docker run -it dock0/arch
{% endhighlight %}

![Docker Pull Arch Linux][image-ss-pull-arch]{: .img-responsive }

{% highlight conf %}
$ docker pull fedora:rawhide
$ docker run -it fedora:rawhide bash
{% endhighlight %}

![Docker Pull Fedora Rawhide][image-ss-pull-fedora]{: .img-responsive }

{% highlight conf %}
$ docker pull opensuse/amd64:tumbleweed
$ docker run -it opensuse/amd64:tumbleweed bash
{% endhighlight %}

![Docker Pull openSUSE Tumbleweed][image-ss-pull-opensuse]{: .img-responsive }

{% highlight conf %}
$ docker pull debian:stretch
$ docker run -it debian:stretch
{% endhighlight %}

![Docker Pull Debian Stretch][image-ss-pull-debian]{: .img-responsive }

{% highlight conf %}
$ docker pull busybox
$ docker run -it busybox
{% endhighlight %}

![Docker Pull BusyBox][image-ss-pull-busybox]{: .img-responsive }

-- -- --

#### Conclusion

	Dream come true

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_flow = site.url | append: '/assets/posts/system/2017/08/docker-flow' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[image-ss-term-1]:    {{ asset_flow }}/terminal-1.png
[image-ss-term-2]:    {{ asset_flow }}/terminal-2.png
[image-ss-term-3]:    {{ asset_flow }}/terminal-3.png
[image-ss-term-4]:    {{ asset_flow }}/terminal-4.png

[image-ss-term-all]: {{ asset_flow }}/all.png
[photo-ss-term-all]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOGBr5RBUPtwBBJw14l1otPZZYR0WIur16lolb-?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[local-manjaro-lxc]:    {{ site.url }}/system/2016/02/28/lxc-demonstration.html
[local-manjaro-docker]: {{ site.url }}/system/2016/03/03/docker-demonstration-manjaro.html
[local-debian-docker]:  {{ site.url }}/system/2016/03/09/docker-demonstration-debian.html

[image-ss-pull-arch]:     {{ asset_pull }}/arch.png
[image-ss-pull-crux]:     {{ asset_pull }}/crux.png
[image-ss-pull-debian]:   {{ asset_pull }}/debian-stretch.png
[image-ss-pull-fedora]:   {{ asset_pull }}/fedora-rawhide.png
[image-ss-pull-opensuse]: {{ asset_pull }}/opensuse-tumbleweed.png
[image-ss-pull-busybox]:  {{ asset_pull }}/busybox.png
