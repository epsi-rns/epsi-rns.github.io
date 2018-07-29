---
layout: post
title:  "Openbox Utilty - Exit"
categories: desktop
date:   2018-05-12 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  Openbox logging out for dummies.

---

{% include post/2018/05/toc-openbox-config.html %}

### Overview

> Goal: Explaining how to log out from openbox

There are few options to log out from openbox:

*	Default Exit Action

*	OB Logout

*	Bunsen Exit

*	Clearine

*	Terminal: shutdown -h now. (just kidding)

This is not a part of config, but rather an additional feature,
utilize third party application.

-- -- --

### Default

This is the default logout dialog from openbox.

![openbox Logout: default exit][image-ss-exit]{: .img-responsive }

-- -- --

### OB Logout

However, there is a good application called <code>oblogout</code>,
that you can set them as keybinding, or just put it in menu.

{% highlight bash %}
$ oblogout
{% endhighlight %}

[![openbox Logout: oblogout elementary theme][image-ss-oblogout-eleme]{: .img-responsive }][photo-ss-oblogout-eleme]

#### Reading

*	[wiki.archlinux.org/index.php/Oblogout](https://wiki.archlinux.org/index.php/Oblogout)

#### Config

The default <code class="code-file">~/.config/oblogout/config</code> looks similar to this below:

{% highlight conf %}
[general]
theme=elementary

[button]
size=96

[label]
font=Monospace Bold 10
color=#B0C4DE
{% endhighlight %}

#### Theme

However you can add more theme from

*	[github.com/arcolinux/arcolinux-oblogout-themes](https://github.com/arcolinux/arcolinux-oblogout-themes)

*	[github.com/Antergos/oblogout-numix-theme](https://github.com/Antergos/oblogout-numix-theme)

*	[github.com/Cloudef/.../themes](https://github.com/Cloudef/oblogout-fork/tree/master/data/themes)

Just copy all the file to <code>/usr/share/oblogout</code>.

![openbox Logout: oblogout /usr/share][image-ss-oblogout-usr]{: .img-responsive }

And change the config.

{% highlight conf %}
#theme=elementary
#theme=green

theme = foom
#theme = oxygen

#theme = adeos-branco-mono
#theme = adeos-branco
#theme = adeos-cores
{% endhighlight %}

[![openbox Logout: oblogout adeos branco mono theme][image-ss-oblogout-adeos]{: .img-responsive }][photo-ss-oblogout-adeos]

oblogout theme contain png images, and sometimes also the svg source.
It seems like it also support svg images directly.
Unfortunately, I still don't know how to activate svg icons in oblogout.

#### Config Source

*	[gitlab.com/.../dotfiles/.../oblogout/config][dotfiles-oblogout]

-- -- --

### Bunsen Exit

#### Clone

*	[https://github.com/BunsenLabs/bunsen-exit](https://github.com/BunsenLabs/bunsen-exit)

{% highlight bash %}
$ git clone https://github.com/BunsenLabs/bunsen-exit
$ cd bunsen-exit
{% endhighlight %}

And run the script.

{% highlight bash %}
$ ./bin/bl-exit &
{% endhighlight %}

And Voila! The ugly dialog pops up.

![openbox Logout: bunsen-exit dialog][image-ss-blexit-dialog]{: .img-responsive }

#### Config

The default <code class="code-file">~/.config/bl-exit/bl-exitrc</code> is long.
I rarely change, except the theme

{% highlight conf %}
[theme]
# set the theme
# theme = helium
theme = bunsen-small
# theme = dark
# theme = light
# theme = minimal
{% endhighlight %}

And the theme itself looks similar to this:

{% highlight conf %}
[bunsen-small]
name = Default small Bunsen theme
author = BunsenLabs

#Overall height of the dialog.
dialogHeight=130

#Delay for the fade in counter
sleepDelay=0.001
#OverallOpacity of the dialog (0-100)
overallOpacity=80

#Space between Buttons
buttonSpacing=0

# path to icon files
iconpath=/home/epsi/.config/bl-exit/dark

#Button textures (i.e. the images on them)
buttonImageCancel=cancel.png
buttonImagePowerOff=poweroff.png
buttonImageReboot=reboot.png
buttonImageSuspend=sleep.png
buttonImageLogout=logout.png
buttonImageHybridSleep=hibernate.png
buttonImageHibernate=hibernate.png

# windowWidthAdjustment - 
# scale factor for window width 
#( 0 = default, 800px; 1 = full screen width)
windowWidthAdjustment=0.5
{% endhighlight %}

Now we have the beautiful exit dialog.

[![openbox Logout: bunsen-exit small theme][image-ss-blexit-small]{: .img-responsive }][photo-ss-blexit-small]

#### Config Source

*	[gitlab.com/.../dotfiles/.../bl-exit/bl-exitrc][dotfiles-bl-exit]

-- -- --

### Clearine

Surprisingly, my friend make this good python script.

*	[https://github.com/yuune/clearine](https://github.com/yuune/clearine)

The documentation is enough that I do not have to write more in my blog.

[![openbox Logout: clearine][image-ss-clearine]{: .img-responsive }][photo-ss-clearine]

-- -- --

### What's Next

We are finished with openbox configuration.

Consider continue reading [ [Tint2: Simple Taskbar][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-oblogout]: {{ dotfiles }}/oblogout/config
[dotfiles-bl-exit]:  {{ dotfiles }}/bl-exit/bl-exitrc

[local-part-config]:  /desktop/2018/05/15/tint2-config.html

[image-ss-exit]:            {{ asset_path }}/openbox-exit.png
[image-ss-oblogout-usr]:    {{ asset_path }}/oblogout-theme-directory.png
[image-ss-blexit-dialog]:   {{ asset_path }}/bunsen-exit-default.png

[image-ss-oblogout-eleme]:  {{ asset_path }}/openbox-oblogout-elementary.png
[photo-ss-oblogout-eleme]:  https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOoqY-Il9QBchlur9xIAhxgJsDpkQatGIcOvz2H?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-oblogout-adeos]:  {{ asset_path }}/openbox-oblogout-adeos-mono.png
[photo-ss-oblogout-adeos]:  https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMG0n3oreWCdMiCd8PmquI1JTkqXbi53S0vS4Pk?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-blexit-small]:    {{ asset_path }}/bunsen-exit-small.png
[photo-ss-blexit-small]:    https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipO1QAWaHxnJ6_kBfx111RnCQBy1lEb7pyDyuWs9?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-clearine]:        {{ asset_path }}/clearine.png
[photo-ss-clearine]:        https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipO7B33K9a9ej1V1dsDocTYrdtyqUuP20-pA7rkd?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
