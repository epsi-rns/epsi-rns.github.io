---
layout: post
title:  "Bootstrap - Offline Template Examples"
date:   2018-08-09 09:35:15 +0700
categories: webdev
tags:   [ruby, jekyll, bootstrap]
author: epsi

excerpt:
  Get a local copy of bootstrap template examples,
  using no server.

---

{% include post/2018/08/toc-bootstrap-cli.html %}

-- -- --

### Preface

> Get a local copy of bootstrap template examples.

You can install the boostrap v4.1 template examples
as a local offline copy that can be launched from open file manager.

*	[getbootstrap.com/docs/4.1/examples/](https://getbootstrap.com/docs/4.1/examples/)

#### Step

* {: .list-content} Get Bootstrap Source Code

* {: .list-content} Generate dist (js + css)

* {: .list-content} Open The Offline Examples

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

You may consider, clicking the image for bigger resolution.

[![Bootstrap: examples directory][image-ss-twbs-examples]{: .img-responsive }][photo-ss-twbs-examples]

-- -- --

### Generate Dist

We are not done yet.
We need to generate dist (jss + css).

There are two methods: 

*	Using NPM

*	Manually copy and paste

#### Using NPM

There is a file named <code>package.json</code>
in extracted bootstrap directory.
It is an npm package bundle configuration.

{% highlight bash %}
{
  "name": "bootstrap",
  "description": "The most popular front-end framework for developing responsive, mobile first projects on the web.",
  "version": "4.1.3",
  "keywords": [
    "css",
    "sass",
    "mobile-first",
    "responsive",
    "front-end",
    "framework",
    "web"
  ],
...
}
{% endhighlight %}

Now you need to run <code>npm install</code> in that folder,
so that the npm can install the right package.

{% highlight bash %}
$ cd bootstrap-4.1.3

$ npm install
{% endhighlight %}

![Bootstrap: npm install][image-ss-twbs-npm-install]{: .img-responsive }

And <code>npm run dist</code> in the very same folder.

{% highlight bash %}
$ cd bootstrap-4.1.3

$ npm run dist
{% endhighlight %}

[![Bootstrap: npm run dist][image-ss-twbs-npm-run-dist]{: .img-responsive }][photo-ss-twbs-npm-run-dist]

#### Copy Using Terminal

{% highlight bash %}
$ cd bootstrap-4.1.3

$ cp -a dist site/docs/4.1/
{% endhighlight %}

![Bootstrap: copy paste dist][image-ss-twbs-dist-copy]{: .img-responsive }

#### Copy Paste Using File Manager

If you have difficulty using terminal,
you may use File Manager.
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
but sometimes I met people who does not have a clue about this.
I respect all the reader, both beginner and expert.

{% highlight bash %}
$ thunar &
{% endhighlight %}

-- -- --

### Open The Offline Examples

#### No server

You do not need Jekyll to have a local copy of your bootstrap.
Just open it from file manager to browser, such as firefox, or chromium.

![Bootstrap: oen from file manager][image-ss-twbs-open-with]{: .img-responsive }

#### Open in Your Browser

And this is the result:

[![Bootstrap: cover using localcopy][image-ss-twbs-cover]{: .img-responsive }][photo-ss-twbs-cover]

#### Fully Working Site

In order to have a fully working site, you need Jekyll.

-- -- --

### What's Next

Consider finish reading [ [Part Two][local-part-two] ].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/webdev/2018/08' %}

[local-part-two]:	/webdev/2018/08/10/bootstrap-cli.html

[image-ss-twbs-examples]:       {{ asset_path }}/bootstrap-examples-directory.png
[image-ss-twbs-cover]:          {{ asset_path }}/bootstrap-localcopy-cover.png
[image-ss-twbs-open-with]:      {{ asset_path }}/bootstrap-open-with.png
[image-ss-twbs-npm-run-dist]:   {{ asset_path }}/bootstrap-npm-run-dist.png
[image-ss-twbs-npm-install]:    {{ asset_path }}/bootstrap-npm-install.png
[image-ss-twbs-dist-copy]:      {{ asset_path }}/bootstrap-dist-copy.png

[photo-ss-twbs-cover]:          https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNr-NHrZmWFQ38-cSwmnq9O8JQibSS8HGgcHUZ0?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
[photo-ss-twbs-examples]:       https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipN1pglfVjhERNLs0ETKnBXiDVFJeTBr7qZIMqpw?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
[photo-ss-twbs-npm-run-dist]:   https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMDoZWvnqtW83zacS01IEpmnrmIJSvfzloPIlrm?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
