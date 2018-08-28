---
layout: post
title:  "Bootstrap - Install Jekyll on RVM"
date:   2018-08-10 09:35:15 +0700
categories: webdev
tags:   [ruby, jekyll, bootstrap]
author: epsi

excerpt:
  Step by step install bootstrap v4.1 on localhost,
  using Jekyll bundle on RVM.  

---

{% include post/2018/08/toc-bootstrap-cli.html %}

-- -- --

### Preface

> Time to embrace the CLI webtools

You can install the boostrap v4.1 source code site on your localhost.

*	[getbootstrap.com/docs/4.1/examples/](https://getbootstrap.com/docs/4.1/examples/)

The bootstrap v4.1 utilize jekyll bundle.
This can be achieved using RVM (Ruby Version Manager).
RVM run on user space locally, and it won't affect your system.
That means it is distro-agnostic.

*	[rvm.io](https://rvm.io)

The Jekyll here run inside bundle.
This bundle lock the jekyll version, 
along with all of its dependencies.
That means any bundle can run anywhere without version conflict.
Very useful, while deploying, or moving from one machine to another.

#### Why Another Tutorial ?

The step in this guidance is
almost the same with the officials site below.

*	[rvm.io/rvm/install](https://rvm.io/rvm/install)

I need to keep the screenshot so I can help people.
For each step, I can give screenshot.
So the people get the idea, of what to expect,
when issuing command on terminal shell.

#### Step

* {: .list-content} Install Ruby with RVM

* {: .list-content} Install Jekyll with Bundle

* {: .list-content} Run the site on localhost

-- -- --

### Install Ruby with RVM

These are the steps summary:

{% highlight bash %}
$ gpg --keyserver hkp://keys.gnupg.net --recv-keys \
  409B6B1796C275462A1703113804BB82D39DC0E3 \
  7D2BAF1CF37B13E2069D6956105BD0E739499BDB

$ \curl -sSL https://get.rvm.io | bash -s stable

$ source ~/.rvm/scripts/rvm

$ rvm list known

$ rvm install 2.4

$ rvm —default use 2.4.4

$ ruby -v

$ which ruby
{% endhighlight %}

#### GPG

{% highlight bash %}
$ gpg --keyserver hkp://keys.gnupg.net --recv-keys \
  409B6B1796C275462A1703113804BB82D39DC0E3 \
  7D2BAF1CF37B13E2069D6956105BD0E739499BDB
{% endhighlight %}

You may consider, clicking the image for wider figure.

[![RVM: GPG Keys][image-ss-rvm-gpg-keys]{: .img-responsive }][photo-ss-rvm-gpg-keys]

#### Get RVM

{% highlight bash %}
$ \curl -sSL https://get.rvm.io | bash -s stable
{% endhighlight %}

[![RVM: Get RVM][image-ss-rvm-get-rvm]{: .img-responsive }][photo-ss-rvm-get-rvm]

#### List Known Ruby

{% highlight bash %}
$ source ~/.rvm/scripts/rvm

$ rvm list known
{% endhighlight %}

![RVM: List Known][image-ss-rvm-list-known]{: .img-responsive }

We can see some Ruby version, such as <code>2.4</code>.

#### Install Ruby

Consider install Ruby <code>2.4</code>.

{% highlight bash %}
$ rvm install 2.4
{% endhighlight %}

You may consider, clicking the image for bigger resolution.

[![RVM: Install Ruby][image-ss-rvm-install-ruby]{: .img-responsive }][photo-ss-rvm-install-ruby]

#### Set Default Ruby

{% highlight bash %}
$ source ~/.rvm/scripts/rvm

$ rvm —default use 2.4.4

$ ruby -v

$ which ruby
{% endhighlight %}

![RVM: Default Ruby][image-ss-rvm-default-ruby]{: .img-responsive }

#### Set Environment

Set yor path environment
such as in <code>.bashrc</code> or <code>.zshrc</code>.
You may use any text editor, e.g. gedit, geany, nano, or ViM.
Add these two lines:

{% highlight bash %}
export PATH=${PATH}:~/.rvm/gems/ruby-2.4.4/bin/
source ~/.rvm/scripts/rvm
{% endhighlight %}

![RVM: bashrc zshrc][image-ss-rvm-zsh-path]{: .img-responsive }

-- -- --

### Install Jekyll with Bundle

These are the steps summary:

{% highlight bash %}
$ gem install bundler

$ cd bootstrap-4.1.3

$ bundle install

$ bundle exec jekyll serve
{% endhighlight %}

#### Gem Install Bundler

{% highlight bash %}
$ gem install bundler
{% endhighlight %}

![Gem: gem install bundler][image-ss-gem-install-bundler]{: .img-responsive }

#### Gemfile Bundle Install

There is a file named <code>Gemfile</code>
in extracted bootstrap directory.
It is an ruby bundle configuration.
Jekyll it requires a ruby bundle.

{% highlight ruby %}
source 'https://rubygems.org'

group :development, :test do
  gem 'jekyll', '~> 3.8.3'
  gem 'jekyll-redirect-from', '~> 0.14.0'
  gem 'jekyll-sitemap', '~> 1.2.0'
  gem 'jekyll-toc', '~> 0.6.0'
end
{% endhighlight %}


{% highlight bash %}
$ cd bootstrap-4.1.3

$ bundle install
{% endhighlight %}

You may consider, clicking the image for longer message.

[![Gem: Gemfile bundle install][image-ss-gem-bundle-install]{: .img-responsive }][photo-ss-gem-bundle-install]

-- -- --

###	Run The Site on Localhost

There is a file named <code>_config.yml</code>
in extracted bootstrap directory.
It is a Jekyll configuration.

{% highlight yaml %}
# Dependencies
markdown:       kramdown
highlighter:    rouge

kramdown:
  auto_ids:     true

...
{% endhighlight %}

#### Bundle Exec

Run the Jekyll, this will read the <code>_config.yml</code>.

Instead of 

{% highlight bash %}
$ jekyll serve
{% endhighlight %}

Run the Jekyll inside bundle.

{% highlight bash %}
$ bundle exec jekyll serve
{% endhighlight %}

![Gem: bundle exec jekyll serve][image-ss-gem-bundle-exec]{: .img-responsive }

#### Open in Your Browser

> localhost:9001

[![Bootstrap: index on localhost:9001][image-ss-twbs-localhost]{: .img-responsive }][photo-ss-twbs-localhost]

-- -- --

### What's Next

There are however, other material such as making custom themes,
but let us now limit the scope here,
and lt them in other article series.

For now, consider go back to [ [Part One][local-part-one] ].

### Conclusion

It just works.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/webdev/2018/08' %}

[local-part-one]:	/webdev/2018/08/09/bootstrap-cli.html

[image-ss-twbs-localhost]:      {{ asset_path }}/bootstrap-jekyll-localhost-9001.png

[image-ss-rvm-gpg-keys]:        {{ asset_path }}/rvm-gpg-keys.png
[image-ss-rvm-get-rvm]:         {{ asset_path }}/rvm-curl-get-rvm.png
[image-ss-rvm-list-known]:      {{ asset_path }}/rvm-list-known.png
[image-ss-rvm-install-ruby]:    {{ asset_path }}/rvm-install-2.4.png
[image-ss-rvm-default-ruby]:    {{ asset_path }}/rvm-use-default.png
[image-ss-rvm-zsh-path]:        {{ asset_path }}/rvm-zsh-path.png

[image-ss-gem-install-bundler]: {{ asset_path }}/rvm-gem-install-bundler.png
[image-ss-gem-bundle-install]:  {{ asset_path }}/rvm-gemfile-bundle-install.png
[image-ss-gem-bundle-exec]:     {{ asset_path }}/rvm-bundle-exec-jekyll-serve.png

[photo-ss-rvm-gpg-keys]:        https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOer6IOrEbYiMKJ6gazdDYaV5StiHveoCNvW3Ns?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
[photo-ss-twbs-localhost]:      https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMZT23PSSIcKGlXaQpCxhqR8ylPM2i8SRApvvTt?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
[photo-ss-gem-bundle-install]:  https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNvQbqt63E6XUs9KysKEiQZ4tXKjN93IsZoT2aL?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
[photo-ss-rvm-get-rvm]:         https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMj5ZJY6vREheNRVTfZug10GDarRAsg3N-DJemc?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
[photo-ss-rvm-install-ruby]:    https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMv1orEVwsjmcupS4-eAEnt-bcH2qsjiitRi3k1?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
