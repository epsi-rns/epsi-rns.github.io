---
layout: post
title:  "Install Awesome WM in Debian"
categories: desktop
date:   2016-06-30 02:52:15 +0700
tags: [awesome]
author: epsi

excerpt:
  My first Tiling Window Manager is Awesome.
  Installing Awesome WM in Debian is also easy,
  the only difference is you have to git-clone Lain module manually.

related_link_ids: 
  - 16071350  # Preparing Modularized
  - 16070650  # Modularized Structure
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This <a href="#" class="alert-link">page is using Temporary URL</a>,
  I will move this page to Tutorial section later.
</div>


My first Tiling Window Manager is Awesome.

Installing Awesome WM in Debian is also easy,
the only difference is you have to <code class="code-command">$ git clone</code> Lain module manually.
But don't worry, this additional module is optional.

**Reading**

* New: <http://awesomeWM.github.io>, or: <http://new.awesomewm.org/>

* Current: <https://awesomewm.org/>, or: <https://awesome.naquadah.org/>

* Wiki: <https://awesomewm.org/wiki/Main_Page>

[![Awesome Install Summary Screenshot][image-ss-awesome]{: .img-responsive }][picasa-ss-awesome]

To customize Awesome configuration,
You needs Lua Programming Knowledge.

-- -- --

## Install Awesome WM

Installing Awesome is straightforward.
Just issue <code class="code-command">apt</code> command, 
or <code class="code-command">apt-get</code> 
or <code class="code-command">aptitude</code>.
This will also install required packages
[liblua5.1-0, libxcb-xtest0, lua-lgi, rlwrap]

{% highlight bash %}
$ sudo apt install awesome
{% endhighlight %}
 
[![Install Awesome and Lua Dependency][image-awesome-lua]{: .img-responsive }][picasa-awesome-lua]

You can see how lightweight Awesome WM from figure above.
The installation footprint of both Awesome and Lua are less than four megabytes.
A perfect combination between Window Manager and Scripting Language.

-- -- --

## Running Awesome WM

To switch to Awesome WM after installaion completed,
you can logout your current DE/WM (Desktop Environment or Window Manager).
In your DM (Display Manager), login with Awesome WM Session.

If you need to make sure, that Awesome is in you DM list.
You can check xsession directory.

{% highlight bash %}
$ ls /usr/share/xsessions/
awesome.desktop                 i3.desktop                plasma.desktop
gnome-classic.desktop           i3-with-shmlog.desktop    twm.desktop
gnome.desktop                   lightdm-xsession.desktop  xfce.desktop
gnome-flashback-xmonad.desktop  openbox.desktop           xmonad.desktop
{% endhighlight %}

-- -- --

## Additional Packages

There are useful additional packages, 
to make your awesome become more awesome. 

* Lain: <https://awesomewm.org/wiki/Lain>

* Vicious: <https://awesomewm.org/wiki/Vicious>

-- -- --

## Install Vicious

Additional packages can be achieved by 
installing <code>awesome-extra</code> package from official repository.

{% highlight bash %}
$ sudo apt install awesome-extra
{% endhighlight %}

[![Install Awesome Additional Package][image-awesome-vicious]{: .img-responsive }][picasa-awesome-vicious]

-- -- --

## Install Lain Module

Lain is a fork of Vain Module.

In order to install Lain, you must clone the lain manually.

{% highlight bash %}
$ cd ~/.config/awesome/
$ git clone https://github.com/copycat-killer/lain.git
{% endhighlight %}


![Clone Awesome Lain Module Directly from Github][image-awesome-lain-git]{: .img-responsive }

-- -- --

## Sample Configuration

Awesome provide sample rc in their installation package.
Just copy them to <code class="code-file">~/.config/awesome/</code>

{% highlight bash %}
$ dpkg -L awesome | grep rc.lua
/etc/xdg/awesome/rc.lua
{% endhighlight %}

{% highlight bash %}
$ mkdir ~/.config/awesome/
$ cp /etc/xdg/awesome/rc.lua ~/.config/awesome/rc.lua
{% endhighlight %}
 
Now you can login to your Awesome Window Manager

-- -- --

## Modularized Configuration

You can modify your <code class="code-file">rc.lua</code>,
and make your awesome WM more awesome

My configuration dotfiles is here.
You can copy for your own needs.

* <https://github.com/epsi-rns/dotfiles/tree/master/awesome>

Since this configuration is already modularized,
it is easier to learn part by part.

I realized that it needs further refactoring,
but now I don't have much time to do it.

-- -- --

## Source Code

Surprisingly, Awesome WM is still in an active development

* <https://github.com/awesomeWM/awesome>

Another website migrate to github, (but not jekyll)

* <https://github.com/awesomeWM/awesomeWM.github.io>

Even the documentation:

* <http://new.awesomewm.org/doc/api/>

* <http://new.awesomewm.org/apidoc/>

It is a good start to learn Lua programming language.

-- -- --

## What's next ?

There are nice customizations beyond scope of this article.
This site might worth to look at.

* <https://github.com/copycat-killer/awesome-copycats>

-- -- --

Thank you for reading

Good night.


[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-awesome]: {{ site.url }}/assets/posts/desktop/2016/06/debian-awesome-screenshot-summary.png
[picasa-ss-awesome]: https://lh3.googleusercontent.com/-378BeBMqzeo/V3RJasTzbqI/AAAAAAAAAZk/1K_3AaGRFHEHr1Lf9CmJuG3XoqgwVTCDgCCo/s0/debian-awesome-screenshot-summary.png

[image-awesome-lua]: {{ site.url }}/assets/posts/desktop/2016/06/debian-awesome-lua-half.png
[picasa-awesome-lua]: https://lh3.googleusercontent.com/-JAouz6UbATs/V3RJZ2cLzDI/AAAAAAAAAZs/oXJ19GTbX6wIGIzx41joKgILHROoSickQCCo/s0/debian-awesome-lua-full.png

[image-awesome-vicious]: {{ site.url }}/assets/posts/desktop/2016/06/debian-awesome-extra-half.png
[picasa-awesome-vicious]: https://lh3.googleusercontent.com/-HgElJifNMBE/V3RJX140i0I/AAAAAAAAAZs/l6rFuXCnGD08zz5wOxM0l5_s4nKd3zZRQCCo/s0/debian-awesome-extra-full.png

[image-awesome-lain-git]: {{ site.url }}/assets/posts/desktop/2016/06/debian-awesome-extra-half.png