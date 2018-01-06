---
layout: post-sidemenu-jekyll
title:  "Jekyll Installation on Arch/Manjaro"
categories: webdev
date:   2016-06-22 22:02:15 +0700
tags: [install, jekyll]
author: epsi

excerpt:
  Installing Jekyll on Arch based distribution is a little bit tricky.
  Jekyll built on top of Ruby Gems.
  Ruby gems in Arch Linux can be installed 
  per user or system wide.
  The official guidance,
  do not apply well in Arch Linux.
  So we need a little more detail,
  on how we suppose to do it in Arch Linux.

related_link_ids: 
  - 16081315  # Side Menu Without Javascript
  - 16081215  # Side Menu Simple Tree
  - 16081115  # Responsive Side Menu
  - 16062329  # JekyllInstall Debian
  - 16062127  # Install Yaourt
  - 16061409  # Webcoder Begin
  - 16060951  # Jekyll Archives
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will move this page to Tutorial section later.
</div>

Installing Jekyll on Arch/Manjaro is a little bit tricky.
Jekyll built on top of Ruby Gems.
Ruby gems in Arch/Manjaro Linux can be installed 
per user or system wide.

The official guidance,
do not apply well in Arch/Manjaro Linux.
So we need a little more detail,
on how we suppose todo it in Arch/Manjaro Linux.

This article choose the system wide gems.
Since Jekyll is not available in the official repository,
there are a few more steps to utilize AUR package. 

* <https://wiki.archlinux.org/index.php/ruby#Installing_gems_per-user_or_system-wide>



-- -- --

## Install Ruby

* <https://wiki.archlinux.org/index.php/ruby>

{% highlight bash %}
 $ sudo pacman -S ruby
 $ gem list
{% endhighlight %}

[![Installing Ruby using Pacman][image-ss-ruby-install]{: .img-responsive }][photo-ss-ruby-install]

-- -- --

## Install Jekyll

The official site

* <https://jekyllrb.com/docs/installation/>

-- -- --

### Gems per User

{% highlight bash %}
 $ gem install jekyll
{% endhighlight %}


[![Installing Jekyll per USer Using Gem][image-ss-gem-jekyll]{: .img-responsive }][photo-ss-gem-jekyll]

This will result

{% highlight bash %}
 $ jekyll
 bash: jekyll: command not found
{% endhighlight %}

I will explore this later.
So I decide to trash my <code class="code-file">~/.gem</code> directory

-- -- --

### System Wide Gems

We will install ruby-jekyll with all their dependecies using yaourt.
If you have trouble with yaourt, you can 1install them separately.

This ruby-jekyll package require ruby-sass.

**Arch Linux**

{% highlight bash %}
 $ yaourt ruby-sass
{% endhighlight %}


**Manjaro**

It is in community repository

{% highlight bash %}
 $ sudo pacman -S ruby-sass
{% endhighlight %}

[![Installing ruby-sass AUR using yaourt][image-ss-yaourt-ruby-sass]{: .img-responsive }][photo-ss-yaourt-ruby-sass]

{% highlight bash %}
 $ yaourt ruby-jekyll
{% endhighlight %}

[![Installing ruby-jekyll AUR using yaourt][image-ss-yaourt-ruby-jekyll]{: .img-responsive }][photo-ss-yaourt-ruby-jekyll]

-- -- --

## Running Jekyll

### Test Installation 

Let's see if we have done the installation properly.

{% highlight bash %}
 $ jekyll -v
{% endhighlight %}

There is no manual page in command line for jekyll.
So this is all we got:

{% highlight bash %}
 $ jekyll --help
{% endhighlight %}

### Jekyll Server

Prepare your directory.
And run Jekyll in your directory.

{% highlight bash %}
 $ cd /media/Works/Development/
 $ mkdir test-jekyll
 $ cd test-jekyll
{% endhighlight %}

{% highlight bash %}
 $ jekyll build
 $ jekyll serve
{% endhighlight %}

[![Jekyll Build on Empty Directory][image-ss-jekyll-build]{: .img-responsive }][photo-ss-jekyll-build]

Since your diretory is currently empty.
This will run empty site.

You should see the site in your favorite browser runnng on port 4000.

* http://localhost:4000/

### Generate blog

Clear all files,
and let's generate site skeleton.

{% highlight bash %}
 $ jekyll new .
 $ ls -l
{% endhighlight %}

[![Jekyll Scaffolding Generate Web Skeleton][image-ss-jekyll-new]{: .img-responsive }][photo-ss-jekyll-new]

You will see, some new directory and files
required to run a simple Jekyll Blog.

-- -- --

## Jekyll Plugin

Let's go further.
My site is using jekyll-paginate.

In <code class="code-file">_config.yml</code>

{% highlight yaml %}
gems:
   - jekyll-paginate
{% endhighlight %}

If you run Jekyll serve on it,
it will complain about dependency error.

[![Jekyll Plugin Dependency Problem][image-ss-jekyll-serve-error]{: .img-responsive }][photo-ss-jekyll-serve-error]

All we need is to install standard plugin using yaourt.

{% highlight bash %}
 $ yaourt jekyll-paginate
{% endhighlight %}

[![Installing ruby-jekyll-paginate plugin AUR using yaourt][image-ss-yaourt-jekyll-paginate]{: .img-responsive }][photo-ss-yaourt-jekyll-paginate]

Now you can have your Jekyll Site running smoothly.

[![Successfully Running Jekyll][image-ss-jekyll-serve-succeed]{: .img-responsive }][photo-ss-jekyll-serve-succeed]

You can check on port 4000.

* http://localhost:4000/

-- -- --

Thank you for reading.



[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2016/06' %}

[image-ss-ruby-install]: {{ asset_path }}/jekyll-install-ruby-install-half.png
[photo-ss-ruby-install]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMfcH_gOoxKl-TjwPbsrr9LlY5oSZUZ4hsj4CDF

[image-ss-gem-jekyll]: {{ asset_path }}/jekyll-unused-gem-install-half.png
[photo-ss-gem-jekyll]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNIPXL_ZCefy0dn6o-nc-gn25Wf22gxqceER8xe

[image-ss-yaourt-ruby-sass]: {{ asset_path }}/jekyll-yaourt-ruby-sass-half.png
[photo-ss-yaourt-ruby-sass]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNhBhtu2yyvcoCpf64uDlymuIbWQkLE59ppKYui

[image-ss-yaourt-ruby-jekyll]: {{ asset_path }}/jekyll-yaourt-ruby-jekyll-half.png
[photo-ss-yaourt-ruby-jekyll]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOHCEImph9yIk2aC8vvwac4j-_gQHTSU7psH4ba

[image-ss-jekyll-build]: {{ asset_path }}/jekyll-build-half.png
[photo-ss-jekyll-build]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOk_bb_fOtjBszffx9Q4h73ZDn2jKDjBzxdswvt

[image-ss-jekyll-new]: {{ asset_path }}/jekyll-new.png
[photo-ss-jekyll-new]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPUC8BH3DTlBiRGDy5GrQCOXmIieoDSayK3x7WU 

[image-ss-jekyll-serve-error]: {{ asset_path }}/jekyll-serve-error-paginate.png
[photo-ss-jekyll-serve-error]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipNk5T5AXwMBYaeMzJb_CGgTS4Zc5hejiQpFWEfD

[image-ss-yaourt-jekyll-paginate]: {{ asset_path }}/jekyll-yaourt-paginate-half.png
[photo-ss-yaourt-jekyll-paginate]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipM3tBOdSsfYS3JVgC0A60d9RqakS9MBtFlnjMWY

[image-ss-jekyll-serve-succeed]: {{ asset_path }}/jekyll-serve-succeed.png
[photo-ss-jekyll-serve-succeed]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMZ4hGuZ3VqWHEBwIRKRJOTSBhQGK6xAqlX9sCq
