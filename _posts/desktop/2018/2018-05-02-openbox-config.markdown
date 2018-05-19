---
layout: post
title:  "Openbox Config - General rc.xml"
categories: desktop
date:   2018-05-02 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  A brief explanation about Openbox rc.xml configuration in general.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Explaining Openbox rc.xml configuration in general

#### Reading

<code>rc.xml</code> has been comprehensively covered in this article.

*	[http://openbox.org/wiki/Help:Configuration](http://openbox.org/wiki/Help:Configuration)

Therefore, this article only discuss about changes in rc.xml and stuff.

That article above clearly defined 13 sections of xml:
Resistance, Focus, Placement, Theme, Desktops, Resize (and move),
Applications, Keyboard, Mouse, Margins, Menu, Dock, and Coordinates.

#### OBConf

There is a GUI tools to edit the rc.xml, called <code>obconf</code>.

![openbox Config: theme][image-ss-obconf]{: .img-responsive }

#### Source

*	[github.com/.../dotfiles/.../rc.xml][dotfiles-rc-xml]

-- -- --

### Theme

The representative xml of theme configuration,
would looks similar as example below.

{% highlight xml %}
  <theme>
    <name>flatypuss</name>
    <titleLayout>NLSDIMC</titleLayout>
    ...
    <keepBorder>yes</keepBorder>
    <animateIconify>yes</animateIconify>
    <font place="ActiveWindow">
      ...
    </font>
  </theme>
{% endhighlight %}

Where the character means:

*	N: window icon

*	L: window label (AKA title).

*	I: iconify

*	M: maximize

*	C: close

*	S: shade (roll up/down)

*	D: omnipresent (on all desktops).

-- -- --

### Desktop

I like to use, geeky greek character to identify my desktop.

{% highlight xml %}
  <desktops>
    <number>4</number>
    <firstdesk>1</firstdesk>
    <names>
      <name>α</name>
      <name>β</name>
      <name>γ</name>
      <name>δ</name>
    </names>
    <popupTime>875</popupTime>
  </desktops>
{% endhighlight %}

Now you can see the result in <code>tint2</code> as below.

![openbox Config: desktops tint2][image-ss-config-desktops]{: .img-responsive }

-- -- --

### Gaps

You can set desktop margins.
Note that I put 40 on top,
because I have my tint2 panel.
You might want different setting.

{% highlight xml %}
  <margins>
    <top>40</top>
    <bottom>10</bottom>
    <left>10</left>
    <right>10</right>
  </margins>
{% endhighlight %}

![openbox Config: desktops margin][image-ss-config-margin]{: .img-responsive }

-- -- --

### Logging Out

To be done: OB Logout

-- -- --

### What's Next

Actually there is not many to say here,
because I splitted the article to: key/mouse bindng, rules, and menu.

This is only an overview.
Consider continue reading [ [Config: Key and Mouse Binding][local-part-config] ].



[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]: {{ dotfiles }}/rc.xml

[local-part-config]:  /desktop/2018/05/03/openbox-config.html

[image-ss-obconf]:          {{ asset_path }}/openbox-obconf.png
[image-ss-config-desktops]: {{ asset_path }}/openbox-config-desktops-tint2.png
[image-ss-config-margin]:   {{ asset_path }}/openbox-config-margin.png
