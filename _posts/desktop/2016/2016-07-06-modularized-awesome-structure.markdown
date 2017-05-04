---
layout: post-sidemenu-wm
title:  "Modularized Awesome WM Configuration Structure"
categories: desktop
date:   2016-07-06 12:50:15 +0700
tags: [awesome]
author: epsi

excerpt:
  This article explain directory structure of
  modularized Awesome WM configuration (rc.lua)

related_link_ids:
  - 16071350  # Preparing Modularized
  - 16063052  # Install Awesome Debian
  - 16062934  # Install Awesome Arch
  - 14113019  # Awesome TWM Beginner
  - 16031941  # Refactoring Awesome

---

I finally locked myself in my room for hours, doing Awesome WM refactoring.

After a while, the single rc.lua, has been modularized into chunk parts.
Nomore copycat kiddies, I have to learn lua,
and I have to understand every line of code, to configure this WM properly.

The looks is very similar to my previous configuration,
but the lua code inside has been altered heavily.
To make a modular code, I have to make change parts
from bunch of function and variables, into object and its properties.
In order to make a modularized theme, I also have to clean up the old theme.

[![Awesome WM Idul Fitri Satu Syawal][image-ss-awesome-white]{: .img-responsive }][photo-ss-awesome-white]

-- -- --

## Which Parts ?

The original default config only takes one file

{% highlight bash %}
$ cat /etc/xdg/awesome/rc.lua
{% endhighlight %}

Your custom awesome is in <code class="code-file">~/.config/awesome/rc.lua</code>.
You can check yours by issuing tree command.

{% highlight bash %}
$ tree -C -L 2 ~/.config/awesome | less -R
{% endhighlight %}

Common config is usually have two main parts

* <code class="code-file">rc.lua</code> (text only)

* Theme directory (text and image resources)
  There can be as many theme as needed.

{% highlight bash %}
awesome
├── rc.lua
└── theme
    └── themename
        └── theme.lua
{% endhighlight %}  

I start this project by split some code in main rc.lua to main folder.

{% highlight bash %}
awesome
├── rc.lua
└── main
    ├── error-handling.lua
    └── and stuff (*.lua)
{% endhighlight %}


After a while, I begin to realize,
there are also as many statusbar (wibox) setting as needed.
And I can also make it switchable.
Now we can split the statusbar/titlebar directory from the main <code class="code-file">rc.lua</code>.

{% highlight bash %}
awesome
├── rc.lua
├── themes
│   └── default
│   │   └── theme.lua
│   └── clone
│       └── theme.lua
└── anybox
    └── simple
    │   └── statusbar.lua
    ├─ arrow
    │   └── statusbar.lua
    └─ titlebar.lua
{% endhighlight %}

The main <code class="code-file">rc.lua</code> itself contain one very long parts
maintaining mouse binding and keys binding.
Let's put them in binding directory.

{% highlight bash %}
├── binding
│   ├── bindtotags.lua
│   ├── clientbuttons.lua
│   ├── clientkeys.lua
│   ├── globalbuttons.lua
│   ├── globalkeys.lua
│   ├── taglist.lua
│   └── tasklist.lua
{% endhighlight %}

The rest splitted into few files.
Let's put them in main directory.

{% highlight bash %}
├── main
│   ├── error-handling.lua
│   ├── layouts.lua
│   ├── menu-blackarch.lua
│   ├── menu.lua
│   ├── rules.lua
│   ├── signals.lua
│   ├── tags.lua
│   ├── theme.lua
│   └── user-variables.lua
{% endhighlight %}

Finally, there should be a place for third party code
other than result of this refactoring process.
Modules directories hold this stuff.
You can find the source in copycat-killer repository.

{% highlight bash %}
awesome
├── rc.lua
│
├── main
├── binding
├── anybox
├── themes
│
└── modules
{% endhighlight %}

[![Modularized Awesome WM Dark Background][image-ss-awesome-black]{: .img-responsive }][photo-ss-awesome-black]

--

## Real World Result

You can see the sample details below.
Off course you can make entirely different customization.

### Main Awesome Directory

{% highlight bash %}
~/.config/awesome
├── rc.lua
├── main
│   ├── error-handling.lua
│   ├── layouts.lua
│   ├── menu-blackarch.lua
│   ├── menu.lua
│   ├── rules.lua
│   ├── signals.lua
│   ├── tags.lua
│   ├── theme.lua
│   └── user-variables.lua
├── binding
│   ├── bindtotags.lua
│   ├── clientbuttons.lua
│   ├── clientkeys.lua
│   ├── globalbuttons.lua
│   ├── globalkeys.lua
│   ├── taglist.lua
│   └── tasklist.lua
├── modules
│   ├── eminent
│   │   └── init.lua
│   └── menugen
│       └── init.lua
├── anybox
└── themes
{% endhighlight %}

### The theme Directory

I named the theme <code>clone</code>, because ...
I shamefully grabbed them from many resources.

{% highlight bash %}
~/.config/awesome/themes/clone
├── theme.lua
├── elements.lua    
├── titlebar-copycat.lua
├── icons-copycat.lua
│
├── background.jpg
│
├── bar
│   ├── copycat-copland
│   └── copycat-rainbow
├── icons     (each contain *.png)
│   ├── clone
│   ├── copycat-copland
│   └── copycat-multicolor
├── launcher
│   ├── awesome-icon.png
│   └── some *.png logo ...
├── layout-default.lua
├── layouts     (each contain *.png)
│   ├── default-black
│   ├── default-white
│   ├── default-lain-black
│   ├── default-lain-white
│   ├── copycat-multicolor
│   └── lain-zenburn
├── misc        (each contain *.png)
│   ├── copycat-dremora
│   ├── copycat-holo
│   ├── copycat-steamburn
│   └── default
├── taglist     (each contain *.png)
│   ├── copycat-blackburn
│   ├── copycat-dremora
│   ├── copycat-steamburn
│   ├── copycat-zenburn
│   └── default
├── titlebar    (each contain *.png)
│   ├── copycat-copland
│   └── copycat-zenburn
│
└── if necessary, more resources to come ...
{% endhighlight %}

### The Wibox Statusbar Directory

Put your creativity here to make eye candy statusbar.

{% highlight bash %}
~/.config/awesome/anybox/
├── simple
│   └── statusbar.lua
├── vicious
│   ├── helper.lua
│   ├── statusbar.lua
│   └── vicious.lua
├── lain
│   ├── helper.lua
│   ├── statusbar.lua
│   ├── lain-battery.lua
│   ├── lain-diskfree.lua
│   ├── lain.lua
│   └── lain-sound.lua
└── arrow (clone of lain dir, with customization)
    ├── helper.lua
    ├── statusbar.lua 
    ├── lain-battery.lua
    ├── lain-diskfree.lua
    ├── lain.lua
    ├── lain-sound.lua
    └── custom.lua 
{% endhighlight %}

-- -- --

## Configuration Source

* <https://github.com/epsi-rns/dotfiles/tree/master/awesome>

With this complexity, now you know why
I can't just put all the code in this post.

[![Awesome WM Stacked Statusbar][image-ss-awesome-stacked]{: .img-responsive }][photo-ss-awesome-stacked]

-- -- --

## What's next ?

The code of course, what it takes to split,
from plain rc.lua to modular table objects.

-- -- --

Thank you for reading

Good night.


[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-awesome-white]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-refactoring-syawal.png
[photo-ss-awesome-white]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPB2dhgJov_v0PA8bBGtZbiDV4F2lea67M6BCU4

[image-ss-awesome-black]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-refactoring-manjaro.png
[photo-ss-awesome-black]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOyDYD12IuC4fOwV-7B8R8kh0g2_el2PeXAHh62

[image-ss-awesome-stacked]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-modularized-stacked.png
[photo-ss-awesome-stacked]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOoDYrrGAtGpM4vcmdRtlQRQyuILy8lq7M6lMR_
