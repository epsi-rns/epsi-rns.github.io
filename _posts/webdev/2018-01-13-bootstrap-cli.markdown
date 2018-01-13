---
layout: post
title:  "Bootstrap CLI Webtools - Part Three"
date:   2018-01-13 17:35:15 +0700
categories: webdev
tags:   [css, js]
author: epsi

excerpt:
  Embracing CSS Bootstrap Backend By CLI Webtools.
  Playing around with CSS Bootstrap Backend,
  using bower, grunt, composer, gemfile, sass,
  all in command line interface.

---

{% include post/2018/01/toc-bootstrap-cli.html %}

-- -- --

### Composer

Give me a moment.
It has been a long time since my last PHP project,
in 2011 using Symfony Framework 2.

#### Setup PHP environment

Now I'm using openSUSE. 
Other distribution should have similar method.
You should install <code>PHP7</code>,
<code>PHP7 OpenSSL</code>, and <code>PHP7 PHAR</code>.

{% highlight bash %}
% sudo zypper in php7 php7-openssl php7-phar
{% endhighlight %}

[![openSUSE Zypper: Install PHP7][image-ss-zypper-in-php7]{: .img-responsive }][photo-ss-zypper-in-php7]

#### Download

I decided to download the <code>composer.phar</code> from the official site

* [getcomposer.org/download/](https://getcomposer.org/download/)

### Composer Inside Bootstrap.

Consider go back to bootstrap-less directory.

You may notice this <code>composer.json</code> in root directory.

{% highlight json %}
{
  "name": "twbs/bootstrap",
  "description": "The most popular front-end framework for developing responsive, mobile first projects on the web.",
  ...
}
{% endhighlight %}

Now you can run composer.

{% highlight bash %}
% php composer.phar install
{% endhighlight %}

![Composer: install inside bootstrap][image-ss-composer-install]{: .img-responsive }

or

{% highlight bash %}
% php composer.phar update
{% endhighlight %}

![Composer: update inside bootstrap][image-ss-composer-update]{: .img-responsive }

Do not worry if this does nothing. 
We need this <code>composer.json</code> information,
if we want to install bootstrap from outside.

#### Composer Outside Bootstrap.

You can install bootstrap using Composer,
using a ready to use <code>twbs/bootstrap</code>.

{% highlight bash %}
% php composer.phar require twbs/bootstrap
{% endhighlight %}

![Composer: require twbs/bootstrap][image-ss-composer-require]{: .img-responsive }

-- -- --

### Conclusion

	That's all for now.
	I hope that, this material useful.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2018/01/bootstrap' %}

[image-ss-zypper-in-php7]: {{ asset_path }}/zypper-in-php7.png
[photo-ss-zypper-in-php7]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNABiohFsyJct01pqs4yNKaE1cHx5JB-Qv0xnn5?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-composer-install]: {{ asset_path }}/php-composer-install.png
[image-ss-composer-update]:  {{ asset_path }}/php-composer-update.png
[image-ss-composer-require]: {{ asset_path }}/php-composer-require-twbs-bootstrap.png
