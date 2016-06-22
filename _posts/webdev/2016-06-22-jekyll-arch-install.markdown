---
layout: post
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
  - 16062127  # Install Yaourt
  - 16061409  # Webcoder Begin
  - 16060951  # Jekyll Archives
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will put this page to Tutorial section later.
</div>

Installing Jekyll on Arch/Manjaro is a little bit tricky.
Jekyll built on top of Ruby Gems.
Ruby gems in Arch/Manjaro Linux can be installed 
per user or system wide.

The official guidance,
do not apply well in Arch/Manjaro Linux.
So we need a little more detail,
on how we suppose todo it in Arch/Manjaro Linux.

This article choose the system wide gems
utilized AUR package.

* <https://wiki.archlinux.org/index.php/ruby#Installing_gems_per-user_or_system-wide>

-- -- --

## Install Ruby

* <https://wiki.archlinux.org/index.php/ruby>

{% highlight bash %}
 $ sudo pacman -S ruby
 $ gem list
{% endhighlight %}

[![Installing Ruby using Pacman][image-ss-ruby-install]{: .img-responsive }][picasa-ss-ruby-install]

-- -- --

## Install Jekyll

The official site

* <https://jekyllrb.com/docs/installation/>

-- -- --

### Gems per User

{% highlight bash %}
 $ gem install jekyll
{% endhighlight %}


[![Installing Jekyll per USer Using Gem][image-ss-gem-jekyll]{: .img-responsive }][picasa-ss-gem-jekyll]

This will result

{% highlight bash %}
 $ jekyll
 bash: jekyll: command not found
{% endhighlight %}

I will explore this later.
So I decide to trash my ~/.gem directory

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

[![Installing ruby-sass AUR using yaourt][image-ss-yaourt-ruby-sass]{: .img-responsive }][picasa-ss-yaourt-ruby-sass]

{% highlight bash %}
 $ yaourt ruby-jekyll
{% endhighlight %}

[![Installing ruby-jekyll AUR using yaourt][image-ss-yaourt-ruby-jekyll]{: .img-responsive }][picasa-ss-yaourt-ruby-jekyll]

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

[![Jekyll Build on Empty Directory][image-ss-jekyll-build]{: .img-responsive }][picasa-ss-jekyll-build]

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

[![Jekyll Scaffolding Generate Web Skeleton][image-ss-jekyll-new]{: .img-responsive }][picasa-ss-jekyll-new]

You will see, some new directory and files
required to run a simple Jekyll Blog.

-- -- --

## Jekyll Plugin

Let's go further.
My site is using jekyll-paginate.

In _config.yml

{% highlight yaml %}
gems:
   - jekyll-paginate
{% endhighlight %}

If you run Jekyll serve on it,
it will complain about dependency error.

[![Jekyll Plugin Dependency Problem][image-ss-jekyll-serve-error]{: .img-responsive }][picasa-ss-jekyll-serve-error]

All we need is to install standard plugin using yaourt.

{% highlight bash %}
 $ yaourt jekyll-paginate
{% endhighlight %}

[![Installing ruby-jekyll-paginate plugin AUR using yaourt][image-ss-yaourt-jekyll-paginate]{: .img-responsive }][picasa-ss-yaourt-jekyll-paginate]

Now you can have your Jekyll Site running smoothly.

[![Successfully Running Jekyll][image-ss-jekyll-serve-succeed]{: .img-responsive }][picasa-ss-jekyll-serve-succeed]

You can check on port 4000.

* http://localhost:4000/

-- -- --

Thank you for reading.



[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2016/06' %}

[image-ss-ruby-install]: {{ asset_path }}/jekyll-install-ruby-install-half.png
[picasa-ss-ruby-install]: https://lh3.googleusercontent.com/-rUGms8MFJiE/V2rOfFnIOBI/AAAAAAAAAYI/kJMC2iRGGwEz8QJivS8OaITUQfzZsvFdQCCo/s0/jekyll-install-ruby-install-full.png

[image-ss-gem-jekyll]: {{ asset_path }}/jekyll-unused-gem-install-half.png
[picasa-ss-gem-jekyll]: https://picasaweb.google.com/114746802090569033213/6285227631791370225#6299074128896865410

[image-ss-yaourt-ruby-sass]: {{ asset_path }}/jekyll-yaourt-ruby-sass-half.png
[picasa-ss-yaourt-ruby-sass]: https://lh3.googleusercontent.com/-VxLOPmbBnqQ/V2rOmPA8XxI/AAAAAAAAAYM/hQJendh42Z4c3Dw4aD-kvd5NNlItePfogCCo/s0/jekyll-yaourt-ruby-sass-full.png

[image-ss-yaourt-ruby-jekyll]: {{ asset_path }}/jekyll-yaourt-ruby-jekyll-half.png
[picasa-ss-yaourt-ruby-jekyll]: https://lh3.googleusercontent.com/-z8F3xW5297s/V2rOkk0fZ0I/AAAAAAAAAYM/YVWP8AuocRoQsgbmHWx7hxGJ1pkI1aixwCCo/s0/jekyll-yaourt-ruby-jekyll-full.png

[image-ss-jekyll-build]: {{ asset_path }}/jekyll-build-half.png
[picasa-ss-jekyll-build]: https://lh3.googleusercontent.com/-_kAxaq48hxc/V2rOeibSOYI/AAAAAAAAAYI/rXr92MV9PGM30W0uJWBVbFL7uj8nB67-gCCo/s0/jekyll-build-full.png

[image-ss-jekyll-new]: {{ asset_path }}/jekyll-new.png
[picasa-ss-jekyll-new]: https://lh3.googleusercontent.com/-DbsA94EDAbA/V2rOfhHEb1I/AAAAAAAAAYI/eyvw8v4EsD8PW8wElZyqvJ6XKPe9g6L8gCCo/s0/jekyll-new.png

[image-ss-jekyll-serve-error]: {{ asset_path }}/jekyll-serve-error-paginate.png
[picasa-ss-jekyll-serve-error]: https://lh3.googleusercontent.com/-D57DEK-bOcw/V2rOgc1b9fI/AAAAAAAAAYI/sWcu_YFHQ0Eumkp_stGgaKB8e_Y8dIOlQCCo/s0/jekyll-serve-error-paginate.png

[image-ss-yaourt-jekyll-paginate]: {{ asset_path }}/jekyll-yaourt-paginate-half.png
[picasa-ss-yaourt-jekyll-paginate]: https://lh3.googleusercontent.com/-RceT5EcMYjI/V2rOkvRgNMI/AAAAAAAAAYM/r63Rx8Z-mH8EH52WUEb0lD6tkZcO6HwOACCo/s0/jekyll-yaourt-paginate-full.png

[image-ss-jekyll-serve-succeed]: {{ asset_path }}/jekyll-serve-succeed.png
[picasa-ss-jekyll-serve-succeed]: https://lh3.googleusercontent.com/-5cvRHrYnunc/V2rOg9E_l-I/AAAAAAAAAYI/Mlmsswb0Ji4ccwVI5lNPxR36lMZ4ak4BgCCo/s0/jekyll-serve-succeed.png
