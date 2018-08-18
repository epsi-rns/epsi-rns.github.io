---
layout: post
title:  "Openbox Install - obmenu-generator"
categories: desktop
date:   2018-07-18 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  Specific tools for openbox.
  Not really for idiots.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Specific tools for openbox.

In obmenu-generator this case, I will give an example in Fedora.
You can do it in other distribution as well,
such as Debian or Gentoo or openSUSE

This obmenu-generator is a Perl script,
that can be installed using CPAN.
This is the summary.

*	Install CPAN for local user

*	CPAN Gtk2 (in some case)

*	CPAN Data::Dump (in some case)

*	CPAN Test::More

*	CPAN Linux::DesktopFiles

*	Clone obmenu-generator

*	Run ./obmenu-generator

-- -- --

### Example CPAN in Fedora

Setup CPAN for local user first.

{% highlight bash %}
$ cpan
{% endhighlight %}

![fedora28 obmenu-generator: CPAN local][image-ss-f28-cpan-local]{: .img-responsive }

{% highlight bash %}
$ cpan Test::More
{% endhighlight %}

![fedora28 obmenu-generator: CPAN Test::More][image-ss-f28-cpan-test-more]{: .img-responsive }

{% highlight bash %}
$ cpan Linux::DesktopFiles
{% endhighlight %}

![fedora28 obmenu-generator: CPAN Linux::DesktopFiles][image-ss-f28-cpan-desktopfiles]{: .img-responsive }

-- -- --

### Example CPAN in Gentoo

Just like in Fedora, but with <code>Data::Dump</code>.
I do not know why.

{% highlight bash %}
$ cpan Data::Dump
{% endhighlight %}

![gentoo obmenu-generator: CPAN local][image-ss-gentoo-cpan-data-dump]{: .img-responsive }

{% highlight bash %}
$ cpan Test::More
{% endhighlight %}

![gentooobmenu-generator: CPAN Test::More][image-ss-gentoo-cpan-test-more]{: .img-responsive }

{% highlight bash %}
$ cpan Linux::DesktopFiles
{% endhighlight %}

![gentoo obmenu-generator: CPAN Linux::DesktopFiles][image-ss-gentoo-cpan-desktopfiles]{: .img-responsive }

-- -- --

### Example obmenu install

<code>git clone</code> first.
Just like in this Fedora Figure below:

{% highlight bash %}
$ git clone https://github.com/trizen/obmenu-generator
{% endhighlight %}

![fedora28 obmenu-generator: git clone][image-ss-f28-git-clone]{: .img-responsive }

Then run it.
Just like this Debian figure below:

{% highlight bash %}
$ ./obmenu-generator
{% endhighlight %}

![debian obmenu-generator: run][image-ss-debian-run]{: .img-responsive }

Or run it.
Just like this Gentoo figure below:

{% highlight bash %}
$ ./obmenu-generator
{% endhighlight %}

![gentoo obmenu-generator: run][image-ss-gentoo-run]{: .img-responsive }

-- -- --

### What's Next

We are finished with installation procedure.
Consider going back reading [ [Config: Overview][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/07' %}

[local-part-config]:  /desktop/2018/05/01/openbox-config.html

[image-ss-debian-run]:               {{ asset_path }}/04-debian-obmenu-generator.png
[image-ss-f28-cpan-desktopfiles]:    {{ asset_path }}/04-fedora28-cpan-linux-desktopfiles.png
[image-ss-f28-cpan-local]:           {{ asset_path }}/04-fedora28-cpan-local.png
[image-ss-f28-cpan-test-more]:       {{ asset_path }}/04-fedora28-cpan-test-more.png
[image-ss-f28-git-clone]:            {{ asset_path }}/04-fedora28-git-clone-obmenu-generator.png
[image-ss-gentoo-cpan-data-dump]:    {{ asset_path }}/04-gentoo-cpan-data-dump.png
[image-ss-gentoo-cpan-desktopfiles]: {{ asset_path }}/04-gentoo-cpan-linux-desktopfiles.png
[image-ss-gentoo-cpan-test-more]:    {{ asset_path }}/04-gentoo-cpan-test-more.png
[image-ss-gentoo-run]:               {{ asset_path }}/04-gentoo-obmenu-generator.png
