---
layout: post
title: "Docker - Arch ALPM - Part One"
date: 2017-08-24 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
# - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-arch-alpm.html %}

-- -- --

### Preface

> Goal: Examine Package Manager, Focus on Command Line Interface

We have this ALPM, The **Arch Linux Package Management**,
as a base for <code>pacman</code> binary package frontend,
and source package tools such as the famous <code>AUR</code>,
and <code>ASP</code> (formerly <code>ABS</code>),
or even the lower level <code>makepkg</code> tool.
They are all under ALPM umbrella.

Using Arch Linux minimal install in Docker,
is a good way to learn ALPM.
ALPM is considered a basic knowledge
utilized by Arch's based distribution
such as Manjaro, Antergos, Artix,
and some other cool Arch's influenced distribution such as KaOS.
In short, any distribution using <code>pacman</code>

{% include post/2017/08/docker-test-bed.md %}

#### Must Read

You are encouraged to read the holy __archwiki__ first,
before even starting to read this article.

*	<https://wiki.archlinux.org/index.php/pacman>

There are already so many references for this pacman on the internet.
This blog article is just another reference.

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull dock0/arch
{% endhighlight %}

![Docker Pull Debian Strecth][image-ss-pull-debian]{: .img-responsive }

{% highlight bash %}
$ docker image list  
{% raw %}
  --filter "reference=*/arch:*"
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY          SIZE
dock0/arch          870MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it dock0/arch
root@f77ea94688d1:/# exit
exit
{% endhighlight %}

![Docker Arch: Running Docker][image-ss-running-arch]{: .img-responsive }

{% highlight bash %}
$ docker ps 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE               NAMES               STATUS
dock0/arch          musing_pike         Up About a minute
{% endhighlight %}

![Docker Arch: Docker ps][image-ss-docker-ps]{: .img-responsive }

{% highlight bash %}
$ docker start musing_pike
musing_pike
{% endhighlight %}

{% highlight bash %}
$ docker attach musing_pike
[root@37f48ad6a711 /]# 
{% endhighlight %}

![Docker Arch: Getting Started][image-ss-getting-started]{: .img-responsive }

-- -- --

### Package Management

To DO

-- -- --

### Updating System

To DO

-- -- --

### Package IRSI

To DO

-- -- --

### Not Finished Yet

To DO

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-debian' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/24/docker-arch-alpm.html

[image-ss-pull-arch]:		{{ asset_pull }}/arch.png
[image-ss-running-arch]:	{{ asset_post }}/00-running-image.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-getting-started]: {{ asset_post }}/00-getting-started.png
