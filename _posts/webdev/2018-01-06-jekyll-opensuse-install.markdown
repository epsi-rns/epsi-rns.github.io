---
# layout: post-sidemenu-jekyll
layout: post
title:  "Jekyll Installation on openSUSE"
categories: webdev
date:   2018-01-06 16:02:15 +0700
tags: [install, jekyll]
author: epsi

excerpt:
  Installing Jekyll on openSUSE is easy and straightforward
  This article also cover installing RVM on openSUSE.

related_link_ids: 
  - 18022117  # Jekyll Step by Step
  - 16062329  # Jekyll Install Debian
  - 16062202  # Jekyll Install Arch/Manjaro
  - 16052948  # Jekyll Related Posts

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will move this page to Tutorial section later.
</div>

### Preface

In 2016 I wrote two articles:
[installing Jekyll in Debian][local-jekyll-debian], and
[installing Jekyll in Arch/Manjaro][local-jekyll-arch]. 
Now that I use openSUSE, I also write the third article.

There are two approaches for openSUSE,

* System Wide: Install gem as root with Ruby provided by openSUSE

* Local RVM: Install gem in user space with RVM (Ruby Version Manager) 

You can click images for complete screenshot.

-- -- --

### System Wide

This article is short, compared with Debian or Arch.
It is incredibly easy to run Jekyll on openSUSE.

#### Install Required Ruby Package

{% highlight bash %}
% sudo zypper in ruby-devel
{% endhighlight %}

#### Install Jekyll via Gem

Do not forget to use <code>sudo</code>, and also install optional package.

{% highlight bash %}
% sudo gem install jekyll
% gem list
% sudo gem install jekyll-paginate
{% endhighlight %}

[![Tumbleweed: sudo gem install jekyll][image-ss-gem-jekyll]{: .img-responsive }][photo-ss-gem-jekyll]

#### Run jekyll

Since I already has my own blog, I can change directory and just run this jekyll blog server.

{% highlight bash %}
% cd ~/epsi-rns.github.io
% /usr/lib64/ruby/gems/2.4.0/gems/jekyll-3.7.0/exe/jekyll -h 
{% endhighlight %}

[![Tumbleweed: Ruby Long Path][image-ss-ruby-long-path]{: .img-responsive }][photo-ss-ruby-long-path]

#### Set Path

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

### Local RVM

For some reasons, it is good to run Ruby outside root.
Don't ask me why, I'm a n00b in Ruby.

#### Reading

This is a must read link before you start RVM.

* [rvm.io](https://rvm.io/)

#### Prerequisite

Development Tools

{% highlight bash %}
% sudo zypper in ruby-devel
{% endhighlight %}

And

{% highlight bash %}
% sudo zypper in patch automake bison libtool m4 patch gdbm-devel libffi-devel libopenssl-devel readline-devel sqlite3-devel libyaml-devel
{% endhighlight %}

And this rpm downloaded

{% highlight bash %}
% sudo zypper in ~/Downloads/libdb-4_5-4.5.20-134.14.x86_64.rpm
{% endhighlight %}

[![RVM: System Dependencies][image-ss-rvm-deps]{: .img-responsive }][photo-ss-rvm-deps]

#### Install RVM

{% highlight bash %}
% \curl -sSL https://get.rvm.io | bash -s stable --ruby 
{% endhighlight %}

[![RVM: Install: Start ][image-ss-rvm-install-1]{: .img-responsive }][photo-ss-rvm-install-1]

[![RVM: Install: Finish][image-ss-rvm-install-3]{: .img-responsive }][photo-ss-rvm-install-3]

#### Setup RVM

{% highlight bash %}
% source ~/.rvm/scripts/rvm

% rvm list known

% rvm install 2.4

% rvm --default use 2.4.1

% ruby -v
ruby 2.4.1p111 (2017-03-22 revision 58053) [x86_64-linux]

% which ruby
/home/epsi/.rvm/rubies/ruby-2.4.1/bin/ruby
{% endhighlight %}

#### Install Gem

{% highlight bash %}
% source ~/.rvm/scripts/rvm
% gem install jekyll
{% endhighlight %}

[![RVM: gem install jekyll ][image-ss-rvm-gem]{: .img-responsive }][photo-ss-rvm-gem]

Do not forget any optional dependencies.

{% highlight bash %}
% gem install jekyll-paginate
{% endhighlight %}

Now you have these path for Jekyll.

* ~/.rvm/gems/ruby-2.4.1/gems/jekyll-3.7.0/exe/jekyll

* ~/.rvm/gems/ruby-2.4.1/bin/jekyll 

#### Running Jekyll

For convenience, add these lines to <code>.bashrc</code> or <code>.zshrc</code>.

{% highlight bash %}
export PATH=${PATH}:~/.rvm/gems/ruby-2.4.1/bin/
source ~/.rvm/scripts/rvm
alias jekyll-blog='jekyll serve --config _config.yml,_config_dev.yml'
{% endhighlight %}

[![RVM: .bashrc or .zshrc][image-ss-rvm-zshrc]{: .img-responsive }][photo-ss-rvm-zshrc]

Now we can run jekyll with either of these three

{% highlight bash %}
% ~/.rvm/gems/ruby-2.4.1/gems/jekyll-3.7.0/exe/jekyll serve --config _config.yml,_config_dev.yml

% ~/.rvm/gems/ruby-2.4.1/bin/jekyll serve --config _config.yml,_config_dev.yml

% jekyll-blog
{% endhighlight %}

[![RVM: Running Jekyll ][image-ss-rvm-jekyll]{: .img-responsive }][photo-ss-rvm-jekyll]

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

[image-ss-rvm-deps]: {{ asset_path }}/opensuse-rvm-dependencies.png
[photo-ss-rvm-deps]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipMiH_IZ7pF4tOnGiCo3gyIkGJjrvTihnMwzQE4s

[image-ss-rvm-install-1]: {{ asset_path }}/opensuse-rvm-install-1.png
[photo-ss-rvm-install-1]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipNB4EGcI81z6rsTeLCt_qzOnkpRFjedkDimTMM4

[image-ss-rvm-install-3]: {{ asset_path }}/opensuse-rvm-install-3.png
[photo-ss-rvm-install-3]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipMfVposE81bjLrO_ShXsRrVyBefZ_BXBg-ASUm3

[image-ss-rvm-gem]: {{ asset_path }}/opensuse-rvm-gem-install-jekyll.png
[photo-ss-rvm-gem]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipO5vkmrfdInRSfXlX1dGhD_iy4K8Nhog5s96GE0

[image-ss-rvm-zshrc]: {{ asset_path }}/opensuse-rvm-jekyll-zshrc.png
[photo-ss-rvm-zshrc]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipNOf9VGgLX8H_1nxyYBIwFhP6yj0N2qJoNPx9CD

[image-ss-rvm-jekyll]: {{ asset_path }}/opensuse-rvm-jekyll-blog.png
[photo-ss-rvm-jekyll]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipOo3P08pW_MVz8-IlF8XVft-Bkf9nXdT7mZkrO2
