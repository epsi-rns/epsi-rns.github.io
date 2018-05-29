---
layout: post
title:  "Openbox Menu - Static"
categories: desktop
date:   2018-05-07 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  A brief explanation about Openbox rc.xml Configuration

---

{% include post/2018/05/toc-openbox-config.html %}

### Menu

> Goal: Explaining openbox menu.xml configuration

#### Reading

There is also a whole article for this.

*	[http://openbox.org/wiki/Help:Menus](http://openbox.org/wiki/Help:Menus)

-- -- --

### Default rc.xml.

The default <code class="code-file">~/.config/openbox/rc.xml</code> looks similar to this below:

{% highlight xml %}
  <menu>
    <file>menu.xml</file>
    <hideDelay>200</hideDelay>
    <middle>no</middle>
    <submenuShowDelay>100</submenuShowDelay>
    <submenuHideDelay>400</submenuHideDelay>
    <showIcons>yes</showIcons>
    <manageDesktops>yes</manageDesktops>
  </menu>
{% endhighlight %}

It simply show something like this:

![openbox Menu: SUSE default][image-ss-menu-default]{: .img-responsive }

My openSUSE's openbox come **without** <code class="code-file">~/.config/openbox/menu.xml</code>.
We need to create it manually.

#### Source

*	[github.com/.../dotfiles/.../rc.xml][dotfiles-rc-xml]

-- -- --

### Main Menu: OpenSUSE

You can find the default at <code>/etc/xdg/openbox/menu.xml</code>.
However, this is a manually created 
<code class="code-file">~/.config/openbox/menu.xml</code>.

{% highlight xml %}
<?xml version="1.0" encoding="utf-8"?>
<openbox_menu xmlns="http://openbox.org/3.4/menu">
    <menu id="system-menu" label="System">
        <item label="Openbox Configuration Manager">
            <action name="Execute">
                <command>obconf</command>
                <startupnotify>
                    <enabled>yes</enabled>
                </startupnotify>
            </action>
        </item>
        <item label="Reconfigure Openbox">
            <action name="Reconfigure"/>
        </item>
    </menu>
    <menu id="root-menu" label="Openbox 3">
        ...
        <separator label="System"/>        
        <menu id="system-menu"/>
        <separator/>
        <item label="Log Out">
            <action name="Exit">
                <prompt>yes</prompt>
            </action>
        </item>
    </menu>
</openbox_menu>
{% endhighlight %}

![openbox Menu: system][image-ss-menu-system]{: .img-responsive }

Note that the system menu is recreated manually.

{% highlight xml %}
<?xml version="1.0" encoding="utf-8"?>
<openbox_menu xmlns="http://openbox.org/3.4/menu">
    <menu id="system-menu" label="System">
        ...
    </menu>
    <menu id="root-menu" label="Openbox 3">
        ...       
        <menu id="system-menu"/>
        <separator/>
        <item label="Log Out">
            ...
        </item>
    </menu>
</openbox_menu>
{% endhighlight %}

#### Source

*	[github.com/.../dotfiles/.../menu.xml][dotfiles-menu-xml]

-- -- --

### Main Menu: Fedora

With Fedora, it slighlty different:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>

<openbox_menu xmlns="http://openbox.org/3.4/menu">

<menu id="applications-menu" label="Applications" execute="/usr/libexec/openbox-xdg-menu applications"/>
<menu id="preferences-menu" label="Preferences" execute="/usr/libexec/openbox-xdg-menu preferences"/>
<menu id="administration-menu" label="Administration" execute="/usr/libexec/openbox-xdg-menu system-settings"/>
<menu id="terminals-menu" label="Terminals" execute="/usr/libexec/openbox-xdg-menu /etc/xdg/openbox/terminals"/>

<menu id="root-menu" label="Openbox 3">
  <separator label="Openbox"/>
  <menu id="applications-menu"/>
  <menu id="preferences-menu"/>
  <menu id="administration-menu"/>
  <separator/>
  <menu id="terminals-menu"/>
  <separator/>
  <item label="Reconfigure">
    <action name="Reconfigure" />
  </item>
  <item label="Exit">
    <action name="Exit">
      <prompt>yes</prompt>
    </action>
  </item>
  <separator/>
  <item label="Log Out">
    <action name="SessionLogout">
      <prompt>yes</prompt>
    </action>
  </item>
</menu>

</openbox_menu>
{% endhighlight %}

![openbox Menu: Fedora default][image-ss-menu-fedora]{: .img-responsive }

-- -- --

### Main Menu: Debian

With Debian, it is also different:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>

<openbox_menu xmlns="http://openbox.org/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://openbox.org/
                file:///usr/share/openbox/menu.xsd">

<menu id="root-menu" label="Openbox 3">
  <item label="Terminal emulator">
    <action name="Execute"><execute>x-terminal-emulator</execute></action>
  </item>
  <item label="Web browser">
    <action name="Execute"><execute>x-www-browser</execute></action>
  </item>
  <!-- This requires the presence of the 'openbox-menu' package to work -->
  <menu id="/Debian" />
  <separator />
  <menu id="applications-menu" label="Applications" execute="/usr/bin/obamenu"/>
  <separator />
  <item label="ObConf">
    <action name="Execute"><execute>obconf</execute></action>
  </item>
  <item label="Reconfigure">
    <action name="Reconfigure" />
  </item>
  <item label="Restart">
    <action name="Restart" />
  </item>
  <separator />
  <item label="Exit">
    <action name="Exit" />
  </item>
</menu>

</openbox_menu>
{% endhighlight %}

-- -- --


### Pipe Menu

We can add sub menu in separate xml file.

{% highlight xml %}
    <menu id="root-menu" label="Openbox 3">
        <separator label="Static/Manual"/>
        <menu execute="cat /home/epsi/.config/openbox/menu.favorites.xml" 
            id="fav-menu" label="Favorites" icon="/usr/share/icons/gnome/16x16/categories/gnome-util.png"/>
        ...
    </menu>
{% endhighlight %}

I would rather create custom menu <code class="code-file">~/.config/openbox/menu.favorites.xml</code>,
based on my favorites application.

It needs a <code>openbox_pipe_menu</code> tag to deliver a pipe menu

{% highlight conf %}
<openbox_pipe_menu>
    <item label="URxvt" icon="...">
        <action name="Execute"><execute>urxvt</execute></action>
    </item>
    ...
</openbox_pipe_menu>
{% endhighlight %}

The complete version is here:

{% highlight conf %}
<openbox_pipe_menu>
    <separator label="Terminal"/>
    <item label="URxvt" icon="/usr/share/icons/gnome/16x16/apps/utilities-terminal.png">
        <action name="Execute"><execute>urxvt</execute></action>
    </item>
    <item label="XFCE Terminal">
        <action name="Execute"><execute>xfce4-terminal</execute></action>
    </item>
    <separator label="Office"/>
    <item label="Libre Office">
        <action name="Execute"><execute>libreoffice</execute></action>
    </item>    
    <separator label="File Manager"/>
    <item label="PC Man FM Qt">
        <action name="Execute"><execute>pcmanfm-qt </execute></action>
    </item> 
    <item label="Thunar">
        <action name="Execute"><execute>thunar </execute></action>
    </item> 
    <separator label="Network"/>        
    <item label="Firefox">
        <action name="Execute"><execute>firefox</execute></action>
    </item>
    <item label="Chromium">
        <action name="Execute"><execute>chromium</execute></action>
    </item>
    <item label="Midori">
        <action name="Execute"><execute>midori</execute></action>
    </item>
    <item label="Thunderbird">
        <action name="Execute"><execute>thunderbird</execute></action>
    </item>
    <item label="Transmission">
        <action name="Execute"><execute>transmission-qt</execute></action>
    </item>
    <separator label="Media"/>
    <item label="Clementine">
        <action name="Execute"><execute>clementine</execute></action>
    </item>
    <item label="VLC">
        <action name="Execute"><execute>vlc</execute></action>
    </item>    
    <item label="Screenshot">
        <action name="Execute"><execute>lximage-qt --screenshot</execute></action>
    </item>    
    <separator label="Miscellanous"/>
    <item label="Geany">
        <action name="Execute"><execute>geany</execute></action>
    </item>
</openbox_pipe_menu>
{% endhighlight %}

![openbox Menu: favorites][image-ss-menu-favorites]{: .img-responsive }

#### Source

*	[github.com/.../dotfiles/.../menu.favorites.xml][dotfiles-fav-xml]

#### Icons

You may also consider to use the nice <code>Numix Circle</code> icon set.

{% highlight conf %}
    <item label="Thunar"
        icon="/home/epsi/.local/share/icons/Numix-Circle/48/apps/thunar.svg">
        <action name="Execute"><execute>thunar </execute></action>
    </item> 
{% endhighlight %}

But I personally, like the plain menu, depend on my mood.

>	Less is more

-- -- --

### XDG Menu

We can create xdg menu for openbox using this command:

{% highlight bash %}
$ xdg_menu --format openbox3 --root-menu /etc/xdg/menus/applications.menu > ~/.config/openbox/menu.xdg.xml
{% endhighlight %}

*	[wiki.archlinux.org/index.php/Xdg-menu](https://wiki.archlinux.org/index.php/Xdg-menu#OpenBox)

First prepare your <code class="code-file">~/.config/openbox/menu.xml</code>

{% highlight xml %}
    <menu id="root-menu" label="Openbox 3">
        <separator label="Static/Manual"/>
        ...
        <menu execute="cat /home/epsi/.config/openbox/menu.xdg.xml" 
            id="xdg-menu" label="XDG Menu"/>
        ...
    </menu>
{% endhighlight %}

Depend on your setup,
the result of <code>~/.config/openbox/menu.xdg.xml</code> would looks similar like this one:

{% highlight conf %}
<menu id="SUSE Menu" label="SUSE Menu">
 <menu id="Development" label="Development">
  ...
  <menu id="Integrated Environment" label="Integrated Environment">
       <item label="Geany">
       <action name="Execute"><execute>geany </execute></action>
     </item>
  </menu> <!-- Integrated Environment -->
  ...
 </menu> <!-- Development -->
 ...
</menu> <!-- SUSE Menu -->
{% endhighlight %}

We need to change a bit before we can use it as pipe menu.

{% highlight conf %}
<openbox_pipe_menu>
 <separator label="SUSE Menu"/>
 <menu id="Development" label="Development">
  ...
  <menu id="Integrated Environment" label="Integrated Environment">
       <item label="Geany">
       <action name="Execute"><execute>geany </execute></action>
     </item>
  </menu> <!-- Integrated Environment -->
  ...
 </menu> <!-- Development -->
 ...
</openbox_pipe_menu>
{% endhighlight %}

Now you can see the openSUSE's XDG menu.

![openbox Menu: XDG SUSE][image-ss-menu-xdg-suse]{: .img-responsive }

#### Source

*	[github.com/.../dotfiles/.../menu.xdg.xml][dotfiles-xdg-xml]

-- -- --

### Merge All

You can merge the XDG menu to <code class="code-file">menu.xml</code> as a static menu,
to have a nice looking menu.
You need to copy the contetn of XDG menu under <code>id="root-menu"</code>.

{% highlight conf %}
<?xml version="1.0" encoding="utf-8"?>
<openbox_menu xmlns="http://openbox.org/3.4/menu">
    <menu id="system-menu" label="System">
        ...
    </menu>
    <menu id="root-menu" label="Openbox 3">
        <!-- XDG Menu -->
         <separator label="SUSE Menu"/>
         <menu id="Development" label="Development">
          ...
          <menu id="Integrated Environment" label="Integrated Environment">
               <item label="Geany">
               <action name="Execute"><execute>geany </execute></action>
             </item>
          </menu> <!-- Integrated Environment -->
          ...
         </menu> <!-- Development -->
         ...
        <!-- XDG Menu -->
        <separator label="Static/Manual"/>
        <menu execute="cat /home/epsi/.config/openbox/menu.favorites.xml" 
            id="fav-menu" label="Favorites" icon="/usr/share/icons/gnome/16x16/categories/gnome-util.png"/>
        <separator label="System"/>        
        <menu id="system-menu"/>
        <separator/>
        <item label="Log Out">
            <action name="Exit">
                <prompt>yes</prompt>
            </action>
        </item>
    </menu>
</openbox_menu>
{% endhighlight %}

And the final result is.

![openbox Menu: Merge][image-ss-menu-merge]{: .img-responsive }

-- -- --

### Additional Menu

#### OB Log Out/ BL Exit

You can add <code>oblogout</code> or item right away.

{% highlight conf %}
<?xml version="1.0" encoding="utf-8"?>
<openbox_menu xmlns="http://openbox.org/3.4/menu">
    <menu id="system-menu" label="System">
        ...
        <item label="Run Command">
            <action name="gmrun"/>
        </item>
        <item label="OB Log Out">
            <action name="Execute"><execute>oblogout</execute></action>
        </item>
    </menu>
    <menu id="root-menu" label="Openbox 3">
        ..
    </menu>
</openbox_menu>
{% endhighlight %}

-- -- --

### What's Next

Consider continue reading [ [Menu: Dynamic][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]:   {{ dotfiles }}/rc.xml
[dotfiles-menu-xml]: {{ dotfiles }}/menu.xml
[dotfiles-fav-xml]:  {{ dotfiles }}/menu.favorites.xml
[dotfiles-xdg-xml]:  {{ dotfiles }}/menu.xdg.xml

[local-part-config]:  /desktop/2018/05/08/openbox-config.html

[image-ss-menu-default]:   {{ asset_path }}/openbox-menu-default.png
[image-ss-menu-fedora]:    {{ asset_path }}/openbox-menu-fedora-default.png
[image-ss-menu-system]:    {{ asset_path }}/openbox-menu-system.png
[image-ss-menu-favorites]: {{ asset_path }}/openbox-menu-favorites.png
[image-ss-menu-xdg-suse]:  {{ asset_path }}/openbox-menu-xdg-suse.png
[image-ss-menu-merge]:     {{ asset_path }}/openbox-menu-merge.png


