---
layout: post
title:  "Bootstrap on Jekyll - Install RVM"
date:   2018-08-09 09:35:15 +0700
categories: webdev
tags:   [ruby, jekyll, bootstrap]
author: epsi

excerpt:
  Step by step install bootstrap v4.1 on localhost,
  using Jekyll bundle on RVM.  

---


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

*	Get Bootstrap Source Code

*	Install Ruby with RVM

*	Install Jekyll with Bundle

*	Generate dist (js + css)

*	Run the site on localhost

-- -- --

### Get Bootstrap

{% highlight bash %}
$ wget -c https://github.com/twbs/bootstrap/archive/v4.1.3.zip

$ unzip v4.1.3.zip

$ cd bootstrap-4.1.3
{% endhighlight %}

You can extract anywhere to suit your needs.

#### Examples Directory

This is the pure html, if you want to start, using templates

{% highlight bash %}
$ cd site/docs/4.1/examples
{% endhighlight %}

You may click the image for bigger resolution.

![Bootstrap: examples directory][image-ss-twbs-examples]{: .img-responsive }

And this is the result that we want to achieve:

![Bootstrap: cover on localhost:9001][image-ss-twbs-cover]{: .img-responsive }

-- -- --

### Generate Dist

We are not done yet.
We need to generate dist (jss + css).

There are two methods: 

*	Using NPM

*	Manually copy and paste

#### Using NPM

{% highlight bash %}
$ cd bootstrap-4.1.3

$ npm run dist
{% endhighlight %}

![Bootstrap: npm run dist][image-ss-twbs-npm-run-dist]{: .img-responsive }

#### Copy Using Terminal

{% highlight bash %}
$ cd bootstrap-4.1.3

$ cp -a dist site/docs/4.1/
{% endhighlight %}

![Bootstrap: copy paste dist][image-ss-twbs-dist-copy]{: .img-responsive }

#### Copy Paste Using File Manager

Depend on your Desktop Environment,
your file manager might be:

*	gnome-shell: nautilus

*	Plasma5: dolphin

*	Cinnamon: nemo

*	Mate: caja

*	XFCE4: thunar

*	LXQT: pcmanfm

*	Pantheon: pantheon-files

I know it looks dumb explaining this,
But sometimes I met people who does not have a clue about this.

{% highlight bash %}
$ thunar &
{% endhighlight %}

#### No server

You do not need Jekyll to have a local copy of your bootstrap.
Just open it from file manager to browser, such as firefox.

-- -- --

### What's Next


Consider finish reading [ [Part One][local-part-one] ].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/webdev/2018/08' %}

[local-part-one]:	/webdev/2018/01/11/bootstrap-cli.html

[image-ss-twbs-examples]:       {{ asset_path }}/bootstrap-examples-directory.png
[image-ss-twbs-cover]:          {{ asset_path }}/bootstrap-jekyll-localhost-9001-cover.png
[image-ss-twbs-localhost]:      {{ asset_path }}/bootstrap-jekyll-localhost-9001.png

[image-ss-twbs-npm-run-dist]:   {{ asset_path }}/bootstrap-npm-run-dist.png
[image-ss-twbs-dist-copy]:      {{ asset_path }}/bootstrap-dist-copy.png

[image-ss-rvm-gpg-keys]:        {{ asset_path }}/rvm-gpg-keys.png
[image-ss-rvm-get-rvm]:         {{ asset_path }}/rvm-curl-get-rvm.png
[image-ss-rvm-list-known]:      {{ asset_path }}/rvm-list-known.png
[image-ss-rvm-install-ruby]:    {{ asset_path }}/rvm-install-2.4.png
[image-ss-rvm-default-ruby]:    {{ asset_path }}/rvm-use-default.png
[image-ss-rvm-zsh-path]:        {{ asset_path }}/rvm-zsh-path.png

[image-ss-gem-install-bundler]: {{ asset_path }}/rvm-gem-install-bundler.png
[image-ss-gem-bundle-install]:  {{ asset_path }}/rvm-gemfile-bundle-install.png
[image-ss-gem-bundle-exec]:     {{ asset_path }}/rvm-bundle-exec-jekyll-serve.png

