---
layout: post
title:  "Bootstrap CLI Webtools - Summary"
date:   2018-01-10 17:35:15 +0700
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

### Preface

> Time to embrace the CLI webtools

What I know, only the ready to use bootstrap CSS.
But there more sophisticated stuff beneath this.
I started with v3 because I want all the fun, and later with v4.
While examining bootstrap v3 source code I realize,
that there are many stuff other than CSS itself,
in these directory

* v3.3 root (all): bower.json, composer.json, Gemfile, Gruntfile.js

* v3.3 docs (less): a working jekyll site, that you can run locally

* v3.3 deep nest (sass): some ruby

* v4 root: webpack (sw.js), sache

-- -- --

### CLI Webtools

I never know such webtools exist well in command line interface.

![Grunt: docs][image-ss-grunt-docs]{: .img-responsive }

	feeling stupid in the corner

It is also the moment, that I realize, that I know nothing.
But don't worry, they are all can be learnt over a night.
Most of the official documentation is easy to follow.
In fact, I have done it all, from dusk until dawn.
Not more that one day.

-- -- --

### Table of Content

By the language platform,
let me reorganize this article as this below:

* Summary

* Part One (NodeJS): bower, grunt, less, eyeglass

* Part Two (Ruby): Gemfile, SASS/SCSS, Jekyll, sache

* Part Three (PHP): Composer

Looks complete right ?
There is also other popular technology, not being used by bootstrap,
such as as gulp, and ParcelJS.

-- -- --

### Prepare

Download bootstrap from official site:

* [v3: Source LESS and SASS](http://getbootstrap.com/docs/3.3/getting-started/#download)

* [v4: Source SCSS](https://getbootstrap.com/docs/4.0/getting-started/download/)

Consider put it on <code>~/sites/bootstrap</code> directory, and extract.

-- -- --

### What's Next

Since we are going to use <code>NodeJS</code> a lot,
such as <code>Bower</code>, and <code>Grunt</code>.
we should begin with Node Module using NPM.
At least for me as beginner in NodeJS land.
Consider finish reading [ [Part One][local-part-one] ].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2018/01/bootstrap' %}

[local-part-one]:		{{ site.url }}/webdev/2018/01/11/bootstrap-cli.html

[image-ss-grunt-docs]: {{ asset_path }}/grunt-docs.png
