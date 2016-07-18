---
layout: sidebar
title:  "Jekyll Installation on Debian"
categories: webdev
date:   2016-06-23 04:29:15 +0700
tags: [install, jekyll]
author: epsi

excerpt:
  Since Jekyll is available in Official Debian repository,
  Jekyll Installation on Debian is Easy.
  Just issue this command to install system wide Jekyll gem,
  and Debian will take care of Jekyll's dependencies.

related_link_ids: 
  - 16062202  # JekyllInstall Arch/Manjaro
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

Since Jekyll is available in Official Debian repository,
Jekyll Installation on Debian is Easy.
Just issue this command to install system wide Jekyll gem,
and Debian will take care of Jekyll's dependencies.

{% highlight bash %}
 $ apt-get install jekyll
{% endhighlight %}

-- -- --

## Install Ruby

Let's not make this guidance too short,
and play for a while.

To check whether Ruby installed or not,
you can issue dpkg command in your shell..

{% highlight bash %}
 $ dpkg -l 'ruby*'
{% endhighlight %}

If you are using <code>ruby*</code> instead of just <code>ruby</code> word,
there will be bunch of ruby packages coming in the list.

[![Using dpkg to check Ruby Installation][image-ss-debian-dpkg-ruby]{: .img-responsive }][picasa-ss-debian-dpkg-ruby]

-- -- --

## Install Jekyll

Installing Jekyll in Debian means installing Jekyll Official Plugin.
Although you might not need them, it has its advantage.
You don't need to worry about missing plugin while cloning a blog from github.

{% highlight bash %}
 $ apt install jekyll
{% endhighlight %}

[![Using apt to install Jekyll][image-ss-debian-apt-jekyll]{: .img-responsive }][picasa-ss-debian-apt-jekyll]

-- -- --

## Running Jekyll

Let's see if Jekyll works

{% highlight bash %}
 $ jekyll -v
 $ jekyll --help
{% endhighlight %}

Prepare your directory.
And run Jekyll in your directory.

{% highlight bash %}
 $ cd /media/Works/Development/
 $ mkdir test-jekyll
 $ cd test-jekyll
{% endhighlight %}

{% highlight bash %}
 $ jekyll new .
 $ ls -l
{% endhighlight %}

You will see, some new directory and files
required to run a simple Jekyll Blog.

{% highlight bash %}
 $ jekyll build
 $ jekyll serve
{% endhighlight %}


[![Running Jekyll Server][image-ss-debian-jekyll-nbs]{: .img-responsive }][picasa-ss-debian-jekyll-nbs]

You should see the site in your favorite browser running on port 4000.

* http://localhost:4000/

-- -- --

Thank you for reading.



[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2016/06' %}

[image-ss-debian-dpkg-ruby]: {{ asset_path }}/debian-dpkg-ruby-only.png
[picasa-ss-debian-dpkg-ruby]: https://lh3.googleusercontent.com/-7IK89IV-I44/V2sJgSX7B8I/AAAAAAAAAYc/bJt3rLuFgFQZE2cwa6jR9m0N_TDSjrrLwCCo/s0/debian-dpkg-ruby-all.png

[image-ss-debian-apt-jekyll]: {{ asset_path }}/debian-apt-jekyll-l.png
[picasa-ss-debian-apt-jekyll]: https://lh3.googleusercontent.com/-jthCLbGY3g0/V2sJgEoD0OI/AAAAAAAAAYU/B0lM6jfarh4liYXe4ORMEVmH-BXzSGiZQCCo/s0/debian-apt-jekyll-r.png

[image-ss-debian-jekyll-nbs]: {{ asset_path }}/debian-jekyll-serve.png
[picasa-ss-debian-jekyll-nbs]: https://lh3.googleusercontent.com/-zJWv82pwuU4/V2sJgntfjAI/AAAAAAAAAYk/SvIL3wPk1kom8Au91PRZLzwpq8YHsMU4QCCo/s0/debian-jekyll-new-build-serve.png

