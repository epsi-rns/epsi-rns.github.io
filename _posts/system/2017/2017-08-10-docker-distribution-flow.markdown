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

---

### Preface

You can use Docker to learn package management such as 

*	Gentoo Portage

*	Slackware Slackbuild

*	Void Linux XBPS

*	LFS Compilation/ Toolchain

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

I add the size, so you can monitor how the process grow as you work with your container.

#### (3) Terminal 3: Run an image to create process

Do use interactive.
And do not forget to detach from terminal.

{% highlight bash %}
$ sudo docker run -it vbatts/slackware &
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

[![Docker All Screenshot][image-ss-term-full]{: .img-responsive }][photo-ss-term-full]: 

-- -- --

Thank you for Reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[image-ss-term-1]:    {{ asset_path }}/docker-flow-terminal-1.png
[image-ss-term-2]:    {{ asset_path }}/docker-flow-terminal-2.png
[image-ss-term-3]:    {{ asset_path }}/docker-flow-terminal-3.png
[image-ss-term-4]:    {{ asset_path }}/docker-flow-terminal-4.png

[image-ss-term-full]: {{ asset_path }}/docker-flow-full.png
[photo-ss-term-full]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOViBJ7lY6YgTauYIWkB0kBp7_dDpyhIqto5nw9?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
