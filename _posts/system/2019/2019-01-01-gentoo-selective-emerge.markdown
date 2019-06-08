---
layout: post
title:  "Gentoo - Selective Emerge with Equery"
date      : 2019-01-01 09:45:15 +0700
categories: system
tags      : [gentoo, package manager]
keywords  : [emerge, equery]
author: epsi

opengraph:
  image: /assets/site/images/topics/gentoo.png

excerpt:
  With Equery, you can upgrade package by category.
  One category today. And other category the day after.
  You can install only what you need, instead of compile all at once.
  And save your precious time.

---

Hello again everybody.
It has been a while since my last article.

### Preface

> Goal: Upgrade Gentoo by Category using **equery**

Gentoo is Fun.
But sometimes it takes too long to compile.

With Equery, you can upgrade package by category.
One category today. And other category the day after.
You can install only what you need, instead of compile all at once.
And save your precious time.

I think this classic issue is, already somewhere in gentoo manual.
I just love to write a blog about this.

#### Issue

After a few month of not upgrading Gentoo,
I always have a bunch of package that needed to be compiled.
I do not have all the time in the world,
I have life to take care of, so compile all upgraded packages is not an option.

{% highlight bash %}
 % sudo emerge --ask -uvDN @world
{% endhighlight %}

#### Example

Last month I have got a bunch of packages started with:

* dev-ruby

* dev-python

* xfce-base

* xfce-extra

* x11-misc

* dev-qt

* kde-frameworks

![Portage: emerge -uvDN @world][image-ss-emerge-uvdn-cut]{: .img-responsive }

What a long list of packages!
Sure that, I can't give you the whole screenshot here.
Too long that, the list does not even fit my wide screen.

#### Prerequisite

Before you do this tutorial, make sure that,
you have already upgrade your base system.

{% highlight bash %}
 % sudo emerge --ask -uvDN @system
{% endhighlight %}

-- -- --

### Using equery with emerge

#### Summary

It is as this below:

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "dev-ruby/*")
{% endhighlight %}

Well, although it looks complicated, it is actually simple.

> How does it works

#### Listing Using equery

Start from this simple command:

{% highlight bash %}
 % sudo equery list "dev-ruby/*"
 * Searching for * in dev-ruby ...
[IP-] [  ] dev-ruby/did_you_mean-1.0.2:1
[IP-] [  ] dev-ruby/did_you_mean-1.1.2:2.4
[IP-] [  ] dev-ruby/fast_xs-0.8.0-r2:0
[IP-] [  ] dev-ruby/ffi-1.9.18:0
[IP-] [  ] dev-ruby/hpricot-0.8.6-r5:0
[IP-] [  ] dev-ruby/json-1.8.6-r1:0
[IP-] [  ] dev-ruby/json-2.1.0:2
[IP-] [  ] dev-ruby/kpeg-1.1.0-r1:1
[IP-] [  ] dev-ruby/listen-1.3.1-r4:1
[IP-] [  ] dev-ruby/listen-3.1.5-r1:3
[IP-] [  ] dev-ruby/minitest-5.10.3:5
[IP-] [  ] dev-ruby/mustache-1.0.5:0
[IP-] [  ] dev-ruby/net-telnet-0.1.1-r1:1
[IP-] [  ] dev-ruby/power_assert-1.1.1:0
[IP-] [  ] dev-ruby/racc-1.4.14:0
[IP-] [  ] dev-ruby/rake-12.3.1:0
[IP-] [  ] dev-ruby/rake-compiler-1.0.2:0
[IP-] [  ] dev-ruby/rb-inotify-0.9.10:0
[IP-] [  ] dev-ruby/rdiscount-2.2.0.1:0
[IP-] [  ] dev-ruby/rdoc-5.1.0:0
[IP-] [  ] dev-ruby/ruby_dep-1.5.0:1
[IP-] [  ] dev-ruby/rubygems-2.7.6-r1:0
[IP-] [  ] dev-ruby/sass-3.4.24:3.4
[IP-] [  ] dev-ruby/test-unit-3.2.7:2
[IP-] [  ] dev-ruby/xmlrpc-0.3.0:0
{% endhighlight %}

![Portage: equery list "dev-ruby/*"][image-ss-equery-ruby-list]{: .img-responsive }

-- -- --

#### equery with Formatting

Now we can get the name of package using this command below:

{% highlight bash %}
 % sudo equery list -F '$name' "dev-ruby/*"
 * Searching for * in dev-ruby ...
did_you_mean
did_you_mean
fast_xs
ffi
hpricot
json
json
kpeg
listen
listen
minitest
mustache
net-telnet
power_assert
racc
rake
rake-compiler
rb-inotify
rdiscount
rdoc
ruby_dep
rubygems
sass
test-unit
xmlrpc
{% endhighlight %}

![Portage: equery format "dev-ruby/*"][image-ss-equery-ruby-format]{: .img-responsive }

-- -- --

### Base System Issue

#### Prerequiste

Remember our prerequiste ?
We are not finished yet.

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "dev-ruby/*")
{% endhighlight %}

This command will still throw a bunch of packages,
if you haven't upgrade base system.

#### Emerge System

{% highlight bash %}
 % sudo emerge --ask -uvDN @system
{% endhighlight %}

[![Portage: emerge -uvDN @world][image-ss-emerge-uvdn-system]{: .img-responsive }][photo-ss-emerge-uvdn-system]

#### Emerge One Category

Now you can emerge your categories.

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "dev-ruby/*")
{% endhighlight %}

[![Portage: emerge -uvDN category][image-ss-emerge-dev-ruby]{: .img-responsive }][photo-ss-emerge-dev-ruby]

-- -- --

### Finishing

#### Check

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "dev-ruby/*")
{% endhighlight %}

![Portage: emerge -uvDN category empty][image-ss-emerge-empty]{: .img-responsive }

This should show empty, no more packages to be compiled.

#### Emerge Other Category.

Now you can emerge any other category as well:

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "xfce-base/*")
{% endhighlight %}

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "x11-misc/*")
{% endhighlight %}

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "dev-qt/*")
{% endhighlight %}

{% highlight bash %}
 % sudo emerge --ask -uvDN $(equery list -F '$name' "kde-frameworks/*")
{% endhighlight %}

#### At Last

After all packages upgraded, there might be some packages left.
Now we can go back to our simple command.

{% highlight bash %}
 % sudo emerge --ask -uvDN @world
{% endhighlight %}

-- -- --

### Conclusion

Solved.
Compiling in Gentoo is fun again.

Happy New Year 2019.
And thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/system/2019/01' %}

[image-ss-emerge-uvdn-cut]:     {{ asset_path }}/gentoo-01-emerge-uvdn-cut.png
[image-ss-equery-ruby-list]:    {{ asset_path }}/gentoo-01-equery-dev-ruby-list.png
[image-ss-equery-ruby-format]:  {{ asset_path }}/gentoo-01-equery-dev-ruby-format.png

[image-ss-emerge-uvdn-system]:  {{ asset_path }}/gentoo-01-emerge-uvdn-system-small.png
[photo-ss-emerge-uvdn-system]:  https://photos.google.com/album/AF1QipMhBXeFkoeJXHJaQ2A9agbh3-iEMEt0BFFipsex/photo/AF1QipP2hdG6-ODC7L23u7-IsnBXt2uHgOJ7_xwPpC6n

[image-ss-emerge-dev-ruby]:     {{ asset_path }}/gentoo-01-emerge-ruby-cut.png
[photo-ss-emerge-dev-ruby]:     https://photos.google.com/album/AF1QipMhBXeFkoeJXHJaQ2A9agbh3-iEMEt0BFFipsex/photo/AF1QipP4pOQdYzwZV0cvrNo4QnsYYgaYtky8YQHz3VYQ

[image-ss-emerge-empty]:        {{ asset_path }}/gentoo-01-emerge-uvdn-dev-ruby-empty.png
