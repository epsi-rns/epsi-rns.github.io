---
layout: post
title:  "Openbox Menu - OB Menu Generator"
categories: desktop
date      : 2018-05-10 09:25:15 +0700
tags      : [openbox]
keywords  : [tutorial, configuration, menu]
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

You can see more in manual installation page on github.

Consider also reading installation detail on [ [OBMenu Generator: Install][local-part-install] ].


#### Config

Now you need to copy <code class="code-file">schema.pl</code>
to <code>~/.config/obmenu-generator</code>.
It contain predefined array in perl.

*	[gitlab.com/.../dotfiles/.../schema.pl][dotfiles-schema-pl]

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

And do not forget to <code>Reconfigure</code> openbox.

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

### Dynamic Menu

We can also achieve dynamic menu,
by editing the <code class="code-file">menu.xml</code>,
into this:

{% highlight xml %}
<?xml version="1.0" encoding="utf-8"?>
<openbox_menu xmlns="http://openbox.org/"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://openbox.org/">
    <menu id="root-menu" label="obmenu-generator" execute="/usr/bin/obmenu-generator" />
</openbox_menu>
{% endhighlight %}

*	[gitlab.com/.../dotfiles/.../menu.xml][dotfiles-dynamic]

But I'd prefer static menu, that is faster.

-- -- --

### Custom Schema

It is time to edit <code class="code-file">schema.pl</code>.
Most of the ideas comes form ArcoLinux.

*	[github.com/arcolinux/.../schema.pl](https://github.com/arcolinux/arcolinux-obmenu-generator/blob/master/schema.pl)

#### Favorites

Put this on top most:

{% highlight perl %}
    # Favorites
    {begin => ['Favorites', 'utilities-desktop-extra']},
        {sep => 'Terminal'},
        {item => ['urxvt',              'URxvt',            'terminal']},
        {item => ['xfce4-terminal',     'XFCE4 Terminal',   'terminal']},
        {sep => 'Office'},
        {item => ['libreoffice',        'LibreOffice',      'libreoffice-main']},
        {sep => 'File Manager'},        
        {item => ['pcmanfm-qt',         'PC Man FM Qt',     'file-manager']},
        {item => ['thunar',             'Thunar',           'thunar']},
        {sep => 'Internet'},
        {item => ['firefox',            'Firefox',          'firefox']},
        {item => ['chromium',           'Chromium',         'chromium']},
        {item => ['midori',             'Midori',           'midori']},
        {item => ['transmission',       'Transmission',     'transmission']},
        {item => ['telegram',           'Telegram',         'telegram']},
        {sep => 'Media'},
        {item => ['clementine',         'Clementine',       'clementine']},
        {item => ['vlc',                'VLC',              'vlc']},
        {item => ['xfce4-screenshoter', 'Screenshot',       'camera']},
        {sep => 'Editor'},
        {item => ['geany',              'Geany',            'geany']},
        {sep => 'System'},
        {item => ["xfce4-taskmanager",  'Taskmanager',      'gnome-system-monitor']},
        {item => ["hardinfo",           'Hardinfo',         'hardinfo']},
    {end => undef},
{% endhighlight %}

#### Replace Default Application

Comment unused line, and add these lines:

{% highlight perl %}
    #          COMMAND                 LABEL              ICON
    # {item => ['xdg-open .',       'File Manager', 'system-file-manager']},
    # {item => ['xterm',            'Terminal',     'utilities-terminal']},
    # {item => ['xdg-open http://', 'Web Browser',  'web-browser']},
    {item => ['gmrun',            'Run command',  'system-run']},
    {sep => undef},

    {item => ['exo-open --launch TerminalEmulator',                                 'Terminal',          'terminal']},
    {item => ['exo-open --launch FileManager',                                      'File Manager',      'file-manager']},
    {item => ['exo-open --launch WebBrowser ',                                      'Web Browser',       'webbrowser-app']},
{% endhighlight %}

#### Categories Remain Intact

{% highlight perl %}
    {sep => 'Categories'},

    #          NAME            LABEL                ICON
    {cat => ['utility',     'Accessories', 'applications-utilities']},
    {cat => ['development', 'Development', 'applications-development']},
    {cat => ['education',   'Education',   'applications-science']},
    {cat => ['game',        'Games',       'applications-games']},
    {cat => ['graphics',    'Graphics',    'applications-graphics']},
    {cat => ['audiovideo',  'Multimedia',  'applications-multimedia']},
    {cat => ['network',     'Network',     'applications-internet']},
    {cat => ['office',      'Office',      'applications-office']},
    {cat => ['other',       'Other',       'applications-other']},
    {cat => ['settings',    'Settings',    'applications-accessories']},
    {cat => ['system',      'System',      'applications-system']},
{% endhighlight %}

#### Add Places

{% highlight perl %}
    {sep => undef},
    {pipe => ['/home/epsi/.config/openbox/bin/bl-places-pipemenu',         'Places',       'folder']},
{% endhighlight %}

#### Bottom

{% highlight perl %}
    {pipe => ['/home/epsi/.config/openbox/bin/bl-help-pipemenu',              'Help &amp; Resources',              'info']},
    {sep  => undef},

    ## The xscreensaver lock command
    {item => ['xscreensaver-command -lock', 'Lock', 'system-lock-screen']},

    ## This option uses the default Openbox's "Exit" action
    # {exit => ['Exit', 'application-exit']},

    ## This uses the 'oblogout' menu
    {item => ['oblogout', 'Exit', 'application-exit']},
{% endhighlight %}

#### Reconfigure

{% highlight bash %}
$ ./obmenu-generator -s -i > ~/.config/openbox/menu.xml 
{% endhighlight %}

And do not forget to <code>Reconfigure</code> openbox.

![openbox Config: obmenu-generator custom schema.pl][image-ss-obmenu-custom]{: .img-responsive }

-- -- --

### What's Next

We are almost finished with openbox configuration.

Consider continue reading [ [Openbox: Exit][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]:    {{ dotfiles }}/rc.xml
[dotfiles-dynamic]:   {{ dotfiles }}/menu.dynamic.xml
[dotfiles-schema-pl]: {{ dotfiles }}/obmenu-generator/schema.pl

[local-part-config]:  /desktop/2018/05/11/openbox-config.html
[local-part-install]: /desktop/2018/07/18/openbox-install.html

[image-ss-obmenu-icon]:    {{ asset_path }}/openbox-obmenu-icon.png
[image-ss-obmenu-custom]:  {{ asset_path }}/openbox-obmenu-custom.png
