---
layout: post
title:  "Openbox Menu - Generator"
categories: desktop
date:   2018-05-07 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  Using OB Menu Generator as Static Openbox menu.

---

{% include post/2018/05/toc-openbox-config.html %}

### OB Menu Generator

> Goal: Using OB Menu Generator as Static Openbox menu.

Nowadays, people are using automation, rather than a crafting menu manually.
There is this <code>openbox-obmenu-generator</code> tools,.

*	[https://github.com/trizen/obmenu-generator](https://github.com/trizen/obmenu-generator)

It is cleverly using perl array preprocessor,
before converting to xml.
Since it has dfferent format than the original xml,
I put this on different menu article.

#### Install

First you need to clone.
or install using AUR.
Depend on your distribution.

{% highlight conf %}
$ git clone https://github.com/trizen/obmenu-generator
{% endhighlight %}

I'm using openSUSE, so I'm stick with zypper.
You can see more in manual installation page on github.

{% highlight conf %}
$ sudo zypper in perl-Linux-DesktopFiles
$ sudo zypper in perl-Gtk2
{% endhighlight %}

#### Config

Now you need to copy <code class="code-file">schema.pl</code>
to <code>~/.config/obmenu-generator</code>.
It contain predefined array in perl.

*	[github.com/.../dotfiles/.../schema.pl][dotfiles-schema-pl]

{% highlight perl %}
#!/usr/bin/perl

# ...

our $SCHEMA = [

    #          COMMAND                 LABEL              ICON
    {item => ['xdg-open .',       'File Manager', 'system-file-manager']},
    {item => ['xterm',            'Terminal',     'utilities-terminal']},
    {item => ['xdg-open http://', 'Web Browser',  'web-browser']},
    {item => ['gmrun',            'Run command',  'system-run']},

    # ...
]
{% endhighlight %}

#### Default Menu

Just run the <code>obmenu-generator</code>.

{% highlight bash %}
$ ./obmenu-generator -s > ~/.config/openbox/menu.xml 
{% endhighlight %}

#### Icon

Adding icon is as simply as adding <code>-i</code> argument.

{% highlight bash %}
$ ./obmenu-generator -s -i > ~/.config/openbox/menu.xml 
{% endhighlight %}

There will be temporary icon directory in 
<code>~/.config/obmenu-generator/icons/</code>.

And in the generated <code>menu.xml</code>, it has this hardcoded icon path:

{% highlight bash %}
...
    <item label="File Manager" icon="/home/epsi/.config/obmenu-generator/icons/bd6f34dfc816e8ba8054aeb1419095a4.png">
        <action name="Execute"><command><![CDATA[xdg-open .]]></command></action>
    </item>
...
{% endhighlight %}

![openbox Config: obmenu-generator with icon][image-ss-obmenu-icon]{: .img-responsive }

#### Icon Theme Case

I'm using **Breeze**, but I want **Numix Circle**, in my Menu.

All I need is to change <code>~/.gtkrc-2.0</code>
to **Numix Circle** temporarily,
run <code>obmenu-generator</code>,
and get it back to **Breeze**.

You can also use <code>lxapperance</code> to do this temporarily.
 
{% highlight conf %}
...
gtk-theme-name="Breeze"
gtk-icon-theme-name="Numix-Circle"
...
{% endhighlight %}

-- -- --

### What's Next

We are finished with openbox configuration.
Consider going back reading [ [Config: Overview][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]:    {{ dotfiles }}/rc.xml
[dotfiles-schema-pl]: {{ dotfiles }}/obmenu-generator/schema.pl

[local-part-config]:  /desktop/2018/05/01/openbox-config.html

[image-ss-obmenu-icon]:    {{ asset_path }}/openbox-obmenu-icon.png
