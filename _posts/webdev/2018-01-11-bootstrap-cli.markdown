---
layout: post
title:  "Bootstrap CLI Webtools - Part One"
date:   2018-01-11 17:35:15 +0700
categories: webdev
tags:   [css, js, bootstrap]
author: epsi

excerpt:
  Embracing CSS Bootstrap Backend By CLI Webtools.
  Playing around with CSS Bootstrap Backend,
  using bower, grunt, composer, gemfile, sass,
  all in command line interface.

---

{% include post/2018/01/toc-bootstrap-cli.html %}

-- -- --

### Node Module

You may skip this part, but If you are new to NodeJS,
you should read the offical documentation first for both
[Node JS](https://nodejs.org/en/) and [NPM JS](https://www.npmjs.com/).

You may notice this <code>package.json</code> in root directory.

{% highlight json %}
{
  "name": "bootstrap",
  "description": "The most popular front-end framework for developing responsive, mobile first projects on the web.",
  "version": "3.3.7"
}
{% endhighlight %}

![Node Module: package.json][image-ss-node-package-json]{: .img-responsive }

Now you can install node modules in respective directory.
NPM recognize information from <code>package.json</code>.

{% highlight bash %}
% cd ~/bootstrap/bootstrap-less-3.3.7/
% npm install
{% endhighlight %}

[![Node Module: npm install][image-ss-source-npm-install]{: .img-responsive }][photo-ss-source-npm-install]

A newly directory has been created

{% highlight bash %}
% tree -d node_modules | head -n 10
node_modules
├── abbrev
├── accepts
├── acorn
│   ├── bin
│   ├── dist
│   ├── rollup
│   └── src
│       ├── bin
│       ├── loose
{% endhighlight %}

-- -- --

### Bower

#### Bower as General

You may skip this part, but If you are new to <code>Bower</code>,
you should read the offical documentation first for [Bower](https://bower.io/).
The official site said that you should migrate
to <code>Yarn</code> and <code>WebPack</code>.

Consider using sites directory.
You can install bower globally.

{% highlight bash %}
% sudo npm install -g bower
{% endhighlight %}

![Bower: NPM Install Bower][image-ss-npm-install-bower]{: .img-responsive }

After this you can do something cool with bower such as install JQuery

{% highlight bash %}
% bower install jquery
{% endhighlight %}

Or something more complex, as the documentation said.

{% highlight bash %}
% bower install desandro/masonry
{% endhighlight %}

[![Bower: Install JQuery][image-ss-bower-install-jquery]{: .img-responsive }][photo-ss-bower-install-masonry]

#### Bower Inside Bootstrap.

Consider go back to bootstrap-less directory.

You may notice this <code>bower.json</code> in root directory.

{% highlight json %}
{
  "name": "bootstrap",
  "description": "The most popular front-end framework for developing responsive, mobile first projects on the web.",
  ...
  "dependencies": {
    "jquery": "1.9.1 - 3"
  }
}
{% endhighlight %}

Now you can also install dependencies (JQuery) directly using bower.

![Bower: install inside bootstrap][image-ss-bootstrap-bower-install]{: .img-responsive }

#### Bower Outside Bootstrap.

You can also install bootstrap using Bower.

{% highlight bash %}
% bower install --save bootstrap-sass
{% endhighlight %}

![Bower: Install Bootstrap Official][image-ss-bower-install-bootstrap]{: .img-responsive }

-- -- --

### Grunt

#### Grunt as General

You may also skip this part, but If you are new to <code>Grunt</code>,
you should read the offical documentation first for [GruntJS](https://gruntjs.com/).

Consider using sites directory.
You can install grunt globally.

{% highlight bash %}
% sudo npm install -g grunt-cli
{% endhighlight %}

![Grunt: NPM Install Grunt CLI][image-ss-npm-install-grunt]{: .img-responsive }

#### Grunt Inside Bootstrap.

Consider go back to bootstrap-less directory.

You may notice this <code>Gruntfile.js</code> in root directory.

{% highlight javascript %}
module.exports = function (grunt) {
  'use strict';

  // Force use of Unix newlines
  grunt.util.linefeed = '\n';
  ...
};
{% endhighlight %}

Now you can run command as in official documentation.
This will generate all the files needed in respective dist directory.

{% highlight bash %}
% grunt dist
{% endhighlight %}

[![Grunt: dist][image-ss-grunt-dist]{: .img-responsive }][photo-ss-grunt-dist]

Or documentation.

{% highlight bash %}
% grunt docs
{% endhighlight %}

[![Grunt: docs][image-ss-grunt-docs]{: .img-responsive }][photo-ss-grunt-docs]

Or even test

{% highlight bash %}
% grunt test
{% endhighlight %}

-- -- --

### Gulp

There is also alternative to <code>grunt</code>.
I won't cover much since bootstrap does not use it.

#### Reading

Some good read:

* The Offical Documentation: [GulpJS](https://gulpjs.com/).

* Tutorial by Baretto: [Working with Sass, Bootstrap and Gulp](http://david-barreto.com/working-with-sass-bootstrap-and-gulp/)

#### Preview

It can do something cool just like Grunt.

{% highlight bash %}
% gulp
{% endhighlight %}

[![Gulp: example][image-ss-gulp-example]{: .img-responsive }][photo-ss-gulp-example]

-- -- --

### eyeglass

This is a tool to do SASS under NPM umbrella.
I must admit, I do not know about it. Not yet.

-- -- --

### sw.js (webpack)

I haven't explore yet.
I really a n00b in Javascript land.

-- -- --

### What's Next

There are, some interesting topic for <code>Ruby</code>,
such as <code>Gemfile</code>, <code>SASS</code>, and <code>Jekyll</code> (SSG).
Consider finish reading [ [Part Two][local-part-two] ].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/webdev/2018/01/bootstrap' %}

[local-part-two]:		/webdev/2018/01/12/bootstrap-cli.html

[image-ss-source-npm-install]: {{ asset_path }}/source-npm-install.png
[photo-ss-source-npm-install]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipPWjgC3oq4ebrZ6kNMDk30cX9IiRVK8zcNtcUwc?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-node-package-json]: {{ asset_path }}/vim-package-json.png

[image-ss-npm-install-bower]: {{ asset_path }}/bower-install.png

[image-ss-bower-install-jquery]: {{ asset_path }}/bower-install-jquery.png
[photo-ss-bower-install-masonry]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMUyBWCK3ygPO_wuLWgZMIQSvJ21gV3SG_Brsdg?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-bootstrap-bower-install]: {{ asset_path }}/less-bower-install.png

[image-ss-bower-install-bootstrap]: {{ asset_path }}/bower-install-bootstrap-sass-official.png

[image-ss-npm-install-grunt]: {{ asset_path }}/grunt-install.png

[image-ss-grunt-dist]: {{ asset_path }}/grunt-dist.png
[photo-ss-grunt-dist]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNFkCFoyyTshvZ_CqTiahoLoiFTBB19Xv8ZKIp0?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-grunt-docs]: {{ asset_path }}/grunt-docs.png
[photo-ss-grunt-docs]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNiZYNzbSLUVseq3dnIHbSrkOl6x18xLZXjE84t?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-gulp-example]: {{ asset_path }}/gulp-final.png
[photo-ss-gulp-example]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipPUPCrq0LMwimNsExNg7loAhJCrweqZLoojEeSy?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
