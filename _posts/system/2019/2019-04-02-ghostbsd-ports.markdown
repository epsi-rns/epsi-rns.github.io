---
layout: post
title:  "GhostBSD - Ports"
date:   2019-04-02 09:45:15 +0700
categories: system
tags: [bsd, package manager]
author: epsi

excerpt:
  GhostBSD provide pkg as binary package manager.
  Ports also provided as package manager to compile source code.
  This article give an example of Ports
  so that beginner can examine this case
  to help understanding the the process
  of installing package using ports.

---

### Preface

> Goal: One Example Case of Ports

#### Issue

I need to leverage BSD knowledge from using binary <code>pkg</code>
to source based <code>ports</code> using already installed GhostBSD,
without installing FreeBSD.

Although I'm using GhostBSD.
I'm sure this also works in TrueOS and Trident.

I hope that this example could help other BSD beginner like me,
understanding the process of installing package using ports.

#### Example Candidate

I examined a few packages to be a candidate for an example case,
from irssi, ncurse, ncdu, etc.
After a while, I decided that ncmpcpp is suitable for this example.

* ncmpcpp: It has many dependency, so I can do recursive config.

* ncurses: I do not want to risk th system by delete a base package.

* ncdu: No config dialog. to simple.

* irssi: Already in other tutorial.

> I decide to use ncmpcpp

#### Prerequisite

Almost all commands below require root privileges.
You can either login as root or run each command using sudo.

#### Reading

Official documentation of FreeBSD:

* [Using the Ports Collection](https://www.freebsd.org/doc/handbook/ports-using.html)

-- -- --

### Setting up Ports

Fresh install GhostBSD provide empty <code>/usr/ports</code>.
We need to setup the directory using <code>portsnap</code>.

#### Portsnap

You only need to know two commands.

First thing first. Fill the directory.

{% highlight bash %}
$ portsnap fetch extract
{% endhighlight %}

![Portsnap: fetch extract][image-ss-portsnap-extract]{: .img-responsive }

And later

{% highlight bash %}
$ portsnap fetch update
{% endhighlight %}

![Portsnap: fetch update][image-ss-portsnap-update]{: .img-responsive }

Now the ports directory is ready to be served.

-- -- --

### Ncmpcpp Ports (Your Example Case)

#### Common Tutorial

Ncmpcpp ports lies on <code>/usr/ports/audio/ncmpcpp</code> directory.
Most tutorial is usually ask to just go there, and run this command:

{% highlight bash %}
$ make install clean
{% endhighlight %}

As a beginner, I won't argue the official documentation.
The fact is, this is just an ordinary <code>Makefile</code>.
The <code>make</code> command might sufficient to compile.
However, we either need to <code>make install</code>
or <code>make package</code> to build package.

#### Dependencies

But I rather have different approach for this example case.
We need to know, how many config that we need to setup.
It means, we need to know the dependency of the main package.

{% highlight bash %}
$ make all-depends-list
{% endhighlight %}

![make: all-depends-list][image-ss-make-all-depends]{: .img-responsive }

Now we can see a bunch of package, that also required to be build.

-- -- --

### Prepare

#### Config

<code>make install</code> will shown up config dialog if necessary.

If you want, you can reconfigure by showing up config dialog,
before doing any compilation.

{% highlight bash %}
$ make config
{% endhighlight %}

![make: config][image-ss-make-config]{: .img-responsive }

#### Recursive Config

Config dialog that shown up after one compilation, to other compilation.
The issue with this is, you have to wait for the compilation to be done.

The soultion is to cofigure all, before any compilation.
You can achieve this with <code>config-recursive</code>.

{% highlight bash %}
$ make config-recursive
{% endhighlight %}

This will show you each dependency package that need to be configured.
A lot of dialogs actually.

![make: recursive: config: fftw3][image-ss-recursive-fftw3]{: .img-responsive }

![make: recursive: config: gettext][image-ss-recursive-gettext]{: .img-responsive }

![make: recursive: config: libconv][image-ss-recursive-libconv]{: .img-responsive }

![make: recursive: config: ncurses][image-ss-recursive-ncurses]{: .img-responsive }

![make: recursive: config: perl5][image-ss-recursive-perl5]{: .img-responsive }

![make: recursive: config: pkg][image-ss-recursive-pkg]{: .img-responsive }

After this you can install all package,
leave your computer with compilation jobs,
while you are doing your other important task,
such as chatting to friend or having a nap time.

-- -- --

### Install Process

Installation process can be very long or very short depend of the package.

{% highlight bash %}
$ make install
{% endhighlight %}

This ncmpppc is a common case that typically show below process in sequence:

![make: install: begin][image-ss-install]{: .img-responsive }

![make: install: check][image-ss-install-check]{: .img-responsive }

![make: install: compile][image-ss-install-compilation]{: .img-responsive }

![make: install: finished][image-ss-install-finished]{: .img-responsive }

You can also use only <code>make package</code> without <code>install</code>,
if you want to build package without installing.

{% highlight bash %}
$ make package
{% endhighlight %}

-- -- --

### Post Install

#### Uninstall

You can <code>uninstall</code> the package:

{% highlight bash %}
$ make deinstall
{% endhighlight %}

![make: deinstall][image-ss-make-deinstall]{: .img-responsive }

Or you can <code>install</code> the package again.

{% highlight bash %}
$ make install
{% endhighlight %}

But this time, there is no need for any compilation.

![make: install][image-ss-make-reinstall]{: .img-responsive }

#### Cleanup

As in most tutorial we need to clean up.

{% highlight bash %}
$ make clean
{% endhighlight %}

![make: clean][image-ss-make-clean]{: .img-responsive }

#### Distfiles

Actually, there are more to be cleaned up.
Consider have a look at this <code>/usr/ports/distfiles/</code> directory:

![/usr/ports/distfiles/][image-ss-disfiles]{: .img-responsive }

We can also clean this up with:

{% highlight bash %}
$ make distclean
{% endhighlight %}

![make: distclean][image-ss-make-distclean]{: .img-responsive }

After having a lot ports build.
There usually many packages in this directory.

-- -- --

### Run

At last time to run the application.
This time using user privilege.
Or quit from root login if you are using <code>su</code>.

{% highlight bash %}
$ ncmpcpp
{% endhighlight %}

![ncmppcpp: running][image-ss-ncmpcpp-running]{: .img-responsive }

Sounds in ncmpcpp works as charmed.

-- -- --

### Conclusion

There are three things that amaze me with BSD. The ZFS, Jail, Ports.
We have already discussed about ports here.

#### A little note about my motive.

The fact that installing and running GhostBSD,
is as easy Ubuntu and Manjaro, is a little annoying.
I always thought that BSD is as hard Gentoo Portage.
All I think about is ports.

Since, I do not have time to switch from pkg to ports due to load of works,
or even installing FreeBSD from the start.
All I can do is try ports just for one case. One example.
And I hope that this example could help other BSD beginner like me,
understanding the process of installing package using ports.

Although I'm using GhostBSD.
I'm sure this also works in TrueOS and Trident.

#### More Reading

Using ports in GhostBSD is fun, actually.

I just think I need to read this
[porters-handbook/](https://www.freebsd.org/doc/en/books/porters-handbook/) later.
And this [Less Known pkg(8) Features](https://vermaden.wordpress.com/2019/01/17/less-known-pkg8-features/).

#### Finally

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/system/2019/04/ncmpcpp' %}


[image-ss-portsnap-extract]:    {{ asset_path }}/01-portsnap-fetch-extract.png
[image-ss-portsnap-update]:     {{ asset_path }}/01-portsnap-fetch-update.png

[image-ss-make-all-depends]:    {{ asset_path }}/02-ncmpcpp-make-all-depends-list.png
[image-ss-make-config]:         {{ asset_path }}/02-ncmpcpp-config-recursive.png

[image-ss-recursive-fftw3]:     {{ asset_path }}/03-ncmpcpp-config-recursive-fftw3.png
[image-ss-recursive-gettext]:   {{ asset_path }}/03-ncmpcpp-config-recursive-gettext.png
[image-ss-recursive-libconv]:   {{ asset_path }}/03-ncmpcpp-config-recursive-libconv.png
[image-ss-recursive-ncurses]:   {{ asset_path }}/03-ncmpcpp-config-recursive-ncurses.png
[image-ss-recursive-perl5]:     {{ asset_path }}/03-ncmpcpp-config-recursive-perl5.png
[image-ss-recursive-pkg]:       {{ asset_path }}/03-ncmpcpp-config-recursive-pkg.png

[image-ss-install]:             {{ asset_path }}/04-ncmpcpp-make-install.png
[image-ss-install-check]:       {{ asset_path }}/04-ncmpcpp-make-install-check.png
[image-ss-install-compilation]: {{ asset_path }}/04-ncmpcpp-make-install-compilation.png
[image-ss-install-finished]:    {{ asset_path }}/04-ncmpcpp-make-install-finished.png

[image-ss-make-deinstall]:      {{ asset_path }}/05-ncmpcpp-make-deinstall.png
[image-ss-make-reinstall]:      {{ asset_path }}/05-ncmpcpp-make-install-again.png
[image-ss-make-clean]:          {{ asset_path }}/05-ncmpcpp-make-clean.png
[image-ss-make-distclean]:      {{ asset_path }}/05-ncmpcpp-make-distclean.png
[image-ss-disfiles]:            {{ asset_path }}/05-ncmpcpp-usr-ports-distfiles.png

[image-ss-ncmpcpp-running]:     {{ asset_path }}/06-ncmpcpp-run.png
