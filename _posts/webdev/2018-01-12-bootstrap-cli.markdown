---
layout: post
title:  "Bootstrap CLI Webtools - Part Two"
date:   2018-01-12 17:35:15 +0700
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

### Gemfile

This is pretty easy if yu have ever use Jekyll before.

#### Install bundler

In order to use this <code>Gemfile</code>,
you need to install <code>Bundler</code>.

{% highlight bash %}
% gem install bundler
{% endhighlight %}

![Ruby Gem: Install Bundler][image-ss-gem-install-bundler]{: .img-responsive }

#### Gemfile Inside Bootstrap.

Consider go back to bootstrap-less directory.
You may notice this <code>Gemfile</code> in root directory.

{% highlight ruby %}
source 'https://rubygems.org'

group :development, :test do
  gem 'jekyll', '~> 3.1.2'
  gem 'jekyll-sitemap', '~> 0.11.0'
end
{% endhighlight %}

Now you can run Gemfile.
This will install all the files needed by the dependencies.
I' using RVM, so in my case this will <code>~/.rvm/gems/ruby-2.4.1/</code> directory.

{% highlight bash %}
% bundle install
{% endhighlight %}

[![Gemfile: bundle install][image-ss-gem-bundle-install]{: .img-responsive }][photo-ss-gem-bundle-install]

Do not worry if this does nothing. 
We need this <code> Gemfile</code> information,
if we want to install bootstrap from outside.

#### Gemfile Outside Bootstrap.

You can install bootstrap using Gem.

{% highlight bash %}
% gem install bootstrap-sass
{% endhighlight %}

![Gem: Install Bootstrap SASS][image-ss-gem-install-bootstrap]{: .img-responsive }

-- -- --

### SASS (Ruby)

I cannot find anywhere in bootstrap that I can use as an example of SASS command.
I decide to use my own Jekyll site as a standalone example.
Here it is, I copied my <code>_sass</code>,
and rename it to <code>input</code>.
Then I make an empty <code>output</code> directory.
Now we can run sass.

{% highlight bash %}
% sass --update input:output  --style compressed --sourcemap=none
{% endhighlight %}

[![SASS: update][image-ss-sass-standalone]{: .img-responsive }][photo-ss-sass-standalone]

-- -- --

### Jekyll (Static Site Generator)

Yes, the documentaion is in Jekyll. 
And you can run it offline using <code>localhost:4000</code>.

#### Additional Setup

You need to create this Jekyll's <code>_config.yml</code> as this example below:

{% highlight yaml %}
# Site settings
title: "bootstrap docs"

# the subpath of your site, e.g. /blog
baseurl: "" 

# the base hostname & protocol for your site
url: "localhost:4000" 
{% endhighlight %}

You also need to copy grunt config as well if needed.

#### Run Server

Now you can run the server

{% highlight bash %}
% jekyll server
{% endhighlight %}

![Jekyll: Bootstrap Offline Documentation][image-ss-docs-jekyll-server]{: .img-responsive }

-- -- --

### Sache

There is this [sache](http://www.sache.in/) official documentation.
But I consider this good article:

* [Share your Sass with Sache](http://thesassway.com/intermediate/share-your-sass-with-sache-a-quick-guide)

If you use Bootstrap v4, you might recognize,
this <code>sache.json</code> at root directory.

{% highlight json %}
{
  "name": "bootstrap",
  "description": "The most popular HTML, CSS, and JavaScript framework for developing responsive, mobile first projects on the web.",
  "tags": ["bootstrap", "grid", "typography", "buttons", "ui", "responsive-web-design"]
}
{% endhighlight %}



-- -- --

### What's Next

There are still, some interesting topic for <code>PHP</code>,
the <code>composer</code>.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/webdev/2018/01/bootstrap' %}

[local-part-three]:		/webdev/2018/01/13/bootstrap-cli.html

[image-ss-gem-install-bundler]: {{ asset_path }}/gem-install-bundler.png

[image-ss-gem-bundle-install]: {{ asset_path }}/gemfile-bundle-install.png
[photo-ss-gem-bundle-install]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNGlXWCDssLsHMadN7a29wrBDZ5RPf7Xd0a08HL?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-gem-install-bootstrap]: {{ asset_path }}/gem-install-bootstrap-sass.png

[image-ss-sass-standalone]: {{ asset_path }}/sass-standalone.png
[photo-ss-sass-standalone]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOeaXtY6fjuRidnRJQscJrXHXbbwOE1maHoptDG?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-docs-jekyll-server]: {{ asset_path }}/docs-jekyll-server.png
