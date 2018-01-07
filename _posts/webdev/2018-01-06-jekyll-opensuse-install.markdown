---
layout: post-sidemenu-jekyll
title:  "Jekyll Installation on openSUSE"
categories: webdev
date:   2018-01-06 16:02:15 +0700
tags: [install, jekyll]
author: epsi

excerpt:
  Installing Jekyll on openSUSE is easy and straightforward
  This article also cover RVM.

related_link_ids: 
  - 16062329  # Jekyll Install Debian
  - 16062202  # Jekyll Install Arch/Manjaro"
  - 16052948  # Jekyll Related Posts

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will move this page to Tutorial section later.
</div>

This article is short. It is incredibly easy to run Jekyll on openSUSE.

### Install Required Ruby Package

{% highlight bash %}
% sudo zypper in ruby-devel
{% endhighlight %}

-- -- --

### Install Jekyll via Gem

Do not forget to use <code>sudo</code>, and also install optional package.

{% highlight bash %}
% sudo gem install jekyll
% gem list
% sudo gem install jekyll-paginate
{% endhighlight %}

[![Tumbleweed: sudo gem install jekyll][image-ss-gem-jekyll]{: .img-responsive }][photo-ss-gem-jekyll]

-- -- --

### Run jekyll

Since I already has my own blog, I can change directory and just run this jekyll blog server.

{% highlight bash %}
% cd ~/epsi-rns.github.io
% /usr/lib64/ruby/gems/2.4.0/gems/jekyll-3.7.0/exe/jekyll -h 
{% endhighlight %}

[![Tumbleweed: Ruby Long Path][image-ss-ruby-long-path]{: .img-responsive }][photo-ss-ruby-long-path]

-- -- --

### Set Path

For convenience, In <code>.bashrc</code> or <code>.zshrc</code>, so we do not
need to remember jekyll path.

{% highlight bash %}
export PATH=${PATH}:/usr/lib64/ruby/gems/2.4.0/gems/jekyll-3.7.0/exe
{% endhighlight %}

[![Tumbleweed: Setting Path in .bashrc or .zshrc][image-ss-set-path]{: .img-responsive }][photo-ss-set-path]

For convenience, we can also make an alias.

{% highlight bash %}
alias jekyll-blog='jekyll serve --config _config.yml,_config_dev.yml'
{% endhighlight %}

And run with very simple command.

{% highlight bash %}
% jekyll-blog
{% endhighlight %}

-- -- --

If you desire some details, you may refer to my old post about 

*	[installing Jekyll in Debian][local-jekyll-debian], or 

*	[installing Jekyll in Arch/Manjaro][local-jekyll-arch].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2018/01' %}

[local-jekyll-debian]: {{ site.url }}/webdev/2016/06/23/jekyll-debian-install.html
[local-jekyll-arch]:   {{ site.url }}/webdev/2016/06/22/jekyll-arch-install.html

[image-ss-gem-jekyll]: {{ asset_path }}/opensuse-gem-install-jekyll.png
[photo-ss-gem-jekyll]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipMuNhbTvbu0fJzMNnTw-gUGXinAvHBhuOYDpVWx

[image-ss-ruby-long-path]: {{ asset_path }}/opensuse-jekyll-long-path.png
[photo-ss-ruby-long-path]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipP4PtBdXBHluuaDB5erlSuGccoSdIAW_twFwOzQ

[image-ss-set-path]: {{ asset_path }}/opensuse-jekyll-set-path.png
[photo-ss-set-path]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipN2mRN53kgUPkgQklLeV5FRBkNi43ZGQk5MVfW3
