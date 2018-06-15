---
layout: post
title:  "Openbox Config - Rules"
categories: desktop
date:   2018-05-05 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  A brief explanation about Openbox rc.xml Configuration

---

{% include post/2018/05/toc-openbox-config.html %}

### Rules

> Goal: Explaining openbox rules in rc.xml configuration 

again, have a look at the <code class="code-file">rc.xml</code>.

#### Case

I came from Tiling Window Manager,
and I always put my startup like this below:

* Desktop 1: Two terminals (left and right)

* Desktop 2: Browser (fullscreen)

* Desktop 3: Text Editor (fullscreen)

* Desktop 4: File Manager (fullscreen)

No matter what tiling I use.
eg. hlwm, dwm, bspwm, i3, awesome
I always use this almost similar configuration,
for my convenience.
So I do not need to change my workflow,
everytime I change window manager.

Of course, openbox is a stacking window manager.
Hence, I don't want to treat openbox as Tiling Window Manager.
But hey, there is some cool stuff that we can do with rules.

#### Reading

There is also a whole article for this.

*	[http://openbox.org/wiki/Help:Applications](http://openbox.org/wiki/Help:Applications)

#### Source

*	[gitlab.com/.../dotfiles/.../rc.xml][dotfiles-rc-xml]

#### Format

The format is:

{% highlight xml %}
  <applications>
    <application name="urxvt*">
      <desktop>1</desktop>
      ...
    </application>
    ...
    </application>
{% endhighlight %}

-- -- --

### Example

#### Virtual Desktop Placement

We can choose where, the application shown-up.
For eaxmple, we want browser to shown up,
maximized on second virtual desktop.

{% highlight xml %}
    <application name="firefox*">
      <desktop>2</desktop>
      <maximized>true</maximized>
    </application>
    <application name="chromium*">
      <desktop>2</desktop>
      <maximized>true</maximized>
    </application>
{% endhighlight %}

#### Size and Positioning

Supposed that I want my urxvt to always shown-up,
on the **center** of the first virtual desktop.

{% highlight xml %}
    <application name="urxvt*">
      <desktop>1</desktop>
      <size>
        <width>50%</width>
        <height>50%</height>
      </size>
      <position force="yes">
        <x>center</x>
        <y>center</y>
      </position>
    </application>
{% endhighlight %}

#### Splitting Window

Here a trick to make a simpe tiling with rules:
left and right, side by side.

{% highlight xml %}
    <application name="thunar" type="normal">
      <desktop>3</desktop>
      <size>
        <width>49%</width>
      </size>
      <position force="yes">
        <x>0</x>
        <y>0</y>
      </position>
      <maximized>vertical</maximized>
    </application>
    <application name="geany" type="normal">
      <desktop>3</desktop>
      <size>
        <width>49%</width>
      </size>
      <position force="yes">
        <x>50%</x>
        <y>0</y>
      </position>
      <maximized>vertical</maximized>
    </application>
{% endhighlight %}

![openbox Config: tiling rules][image-ss-config-rules]{: .img-responsive }

This above is just an example,
I actually always use maximized state for text editor.

We need to set the <code>type="normal"</code> to avoid dialog to be resized.

-- -- --

### What's Next

Consider continue reading [ [Menu: Static][local-part-config] ].


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]: {{ dotfiles }}/rc.xml

[local-part-config]:  /desktop/2018/05/07/openbox-config.html

[image-ss-config-rules]:   {{ asset_path }}/openbox-config-rules-tiling.png
