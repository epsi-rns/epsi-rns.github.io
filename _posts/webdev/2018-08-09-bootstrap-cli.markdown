---
layout: post
title:  "Bootstrap on Localhost - Install RVM"
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

-- -- --

### What's Next


Consider finish reading [ [Part One][local-part-one] ].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/webdev/2018/08' %}

[local-part-one]:	/webdev/2018/01/11/bootstrap-cli.html

[image-ss-twbs-examples]:     {{ asset_path }}/bootstrap-examples-directory.png
[image-ss-twbs-cover]:        {{ asset_path }}/bootstrap-jekyll-localhost-9001-cover.png
[image-ss-twbs-localhost]:    {{ asset_path }}/bootstrap-jekyll-localhost-9001.png

[image-ss-twbs-npm-run-dist]: {{ asset_path }}/bootstrap-npm-run-dist.png
[image-ss-twbs-dist-copy]:    {{ asset_path }}/bootstrap-dist-copy.png

[image-ss-rvm-gpg-keys]:     {{ asset_path }}/rvm-gpg-keys.png
[image-ss-rvm-get-rvm]:      {{ asset_path }}/rvm-curl-get-rvm.png
[image-ss-rvm-list-known]:   {{ asset_path }}/rvm-list-known.png
[image-ss-rvm-install-ruby]: {{ asset_path }}/rvm-install-2.4.png
[image-ss-rvm-default-ruby]: {{ asset_path }}/rvm-use-default.png
[image-ss-rvm-zsh-path]:     {{ asset_path }}/rvm-zsh-path.png

[image-ss-gem-install-bundler]: {{ asset_path }}/rvm-gem-install-bundler.png
[image-ss-gem-bundle-install]:  {{ asset_path }}/rvm-gemfile-bundle-install.png
[image-ss-gem-bundle-exec]:     {{ asset_path }}/rvm-bundle-exec-jekyll-serve.png

