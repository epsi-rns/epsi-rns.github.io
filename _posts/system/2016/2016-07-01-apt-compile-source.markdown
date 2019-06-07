---
layout: post
title:  "Compile .deb Source with apt-src"
date      : 2016-07-01 21:05:15 +0700
categories: system
tags      : [debian, package manager]
keywords  : [apt, apt-src]
author: epsi

excerpt:
  If you think Debian package management based 
  is not challenging enough for beginner,
  maybe you should explore the manual deeper.
  Debian is not boring at all.
  After a this, you will feels like compiling is not that difficult.

related_link_ids: 
  - 15031749  # APT Pinning

---

If you think Debian package management based 
is not challenging enough for beginner,
maybe you should explore the manual deeper.

Apt command is easy, because it works with
already bundled <code>.deb</code> packages.
Compared with gentoo's emerge or arch's yaourt,
there is no need for apt to compile for source.

But again, for curious newbie who wants to know
how compilation works in debian, 
you can use <code>apt-src</code>, or <code>apt-get source</code> command.

[![Screenshot Summary of Compiling .deb Source][image-ss-summary]{: .img-responsive }][photo-ss-summary]

-- -- --

## Building Scope

**Reading**

* <https://wiki.debian.org/apt-src>

* <https://wiki.debian.org/BuildingTutorial>

This post will cover the first article,
an automatic compilation and bundling of <code>.deb</code>,
intended for beginner.

It is actually as simple as issuing just this command.

{% highlight bash %}
$ sudo apt-src --build install <your-package>
{% endhighlight %}

For beginner, This post give screenshot for each steps.

The second one has lower level,
unbundling the first process,
the second article won't covered here.

-- -- --

## Command Used

Compared with Arch's pacman that is a unified command to do all package things,
Debian scattered the command into few different command.
Debian has <code>dpkg</code>, <code>apt-get</code>, 
<code>apt-cache</code>, <code>aptitude</code>, <code>apt-src</code>.
These day some command has been unified into apt command.

So what you need to read is 

{% highlight bash %}
$ man dpkg
$ man apt
$ man apt-src
{% endhighlight %}

-- -- --

## Repository Preparation

Make sure that source repository
is enabled in your <code class="code-file">/etc/apt/sources.list</code>
by uncomment the <code>deb-src</code> line.

{% highlight bash %}
$ cat /etc/apt/sources.list
{% endhighlight %}

![Change your /etc/cat/sources.list][image-sources-list]{: .img-responsive }

you can even uncomment deb in <code class="code-file">/etc/apt/sources.list</code> and leave you system with source only repository.
In this post, we still need deb binary repository to select upgradable package.

Do not forget to update the repository
when you are done editing <code class="code-file">/etc/apt/sources.list</code>
by issuing one of this command below

{% highlight bash %}
$ sudo apt update
$ apt-src update
{% endhighlight %}

-- -- --

## Compilation Directory Preparation

Make any directory as a working directory 
for your compilation process

{% highlight bash %}
$ mkdir ~/apt-src
$ cd ~/apt-src
{% endhighlight %}

-- -- --

## Choose Package to Examine

Let's choose our victim,
see what package is upgradable
by issuing this command

{% highlight bash %}
$ apt list --upgradable
{% endhighlight %}

You can see in figure below,
that apt package can be upgraded.

![Upgradable Package List using apt][image-list-upgradable]{: .img-responsive }

apt 1.2.13 upgradable to 1.2.14.

From now on we will use 'apt' package a our focus.

-- -- --

## Check Package Version

Simply issue one of this command below

{% highlight bash %}
$ dpkg --list apt 
$ apt show apt
$ apt search '^apt$'
{% endhighlight %}

![Check Package Version with dpkg][image-dpkg-list]{: .img-responsive }

-- -- --

## Build Package

Let's do it.
This is going to takes time.

{% highlight bash %}
$ sudo apt-src --build install apt
{% endhighlight %}

![Compile and Build using apt-src][image-build-install]{: .img-responsive }

This above is the most important part of the process.

You can also unbundling this process into two steps.

{% highlight bash %}
$ apt-src install apt
$ apt-src build apt
{% endhighlight %}

-- -- --

## Examine Directory

Check your directoy, 
your apt-1.2.14 .deb package should already be there.

![List Working Directory for Compiling .deb][image-ls-directory]{: .img-responsive }

-- -- --

## Install

Install package using <code class="code-command">dpkg</code> command

{% highlight bash %}
$ sudo dpkg --install apt-1.2.14-amd64.deb
{% endhighlight %}
 
[![Install .deb Using dpkg][image-dpkg-install]{: .img-responsive }][photo-build-install]

-- -- --

You can check your <code>apt</code> version now

{% highlight bash %}
$ apt --version
apt 1.2.14 (amd64)
{% endhighlight %}

-- -- --

After a few days, you will feels like compiling is not difficult.

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2016/07' %}

[image-ss-summary]:      {{ asset_path }}/apt-src-summary.png
[image-sources-list]:    {{ asset_path }}/apt-src-sources-list.png
[image-list-upgradable]: {{ asset_path }}/apt-src-list-upgradable.png
[image-dpkg-list]:       {{ asset_path }}/apt-src-dpkg-list.png
[image-build-install]:   {{ asset_path }}/apt-src-install-build-half.png
[image-ls-directory]:    {{ asset_path }}/apt-src-ls.png
[image-dpkg-install]:    {{ asset_path }}/apt-src-dpkg-install.png

[photo-ss-summary]:    https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNdo5-ufbNUoAQo9rfl7uY_tXFDh-NL1ZL_5p6X
[photo-build-install]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOf1CvlfS4Zq33u37UmRpAVaYjnNaJcVl5JiaIK
