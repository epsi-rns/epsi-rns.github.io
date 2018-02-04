---
layout: post-sidemenu-wm
title:  "Linux - Installing Font"
categories: desktop
date:   2018-02-04 09:25:15 +0700
tags: [install]
author: epsi

excerpt:
  Manually installing Font in Linux

related_link_ids:
  - 16080325  # Install i3 Arch
  - 16080224  # Install i3 Debian

---

### Preface

> Goal: Install Font Manually	

I have been in a few years in my comfort zone with Arch Linux.
Every font, can be installed directly using AUR.
Now, that I move on to Slackware,
I have to rethink about the way I install fonts.
From provided by packages, into manual install.

	This guidance applied for most distribution.

-- -- --

### From Empty Folder

My new working path is,
of course <code class="code-file">~/.local/share/fonts</code>,
instead of deprecated <code class="code-file">~/.fonts</code>.
This time I arrange to a few subdirectory:

{% highlight bash %}
% cd ~/.local/share/fonts

% tree -d
.
├── google
├── i18n
├── icons
└── inkscape
{% endhighlight %}

-- -- --

### Downloading from git

For most fonts, I prefer to download using browser.
But for system font, that I will be using in desktop,
I prefer git clone to make sure the font downloaded right.

{% highlight bash %}
% mkdir build
% cd build

% git clone https://github.com/FortAwesome/Font-Awesome

% git clone https://github.com/stark/siji

% git clone https://github.com/SumiTomohiko/UnnamedFukidashi
{% endhighlight %}

And copy-paste later on.

### Copy and Paste

Copy paste manually.

{% highlight bash %}
% cd ~/.local/share/fonts/icons
% cp ~/build/Font-Awesome/fonts/fontawesome-webfont.ttf .
{% endhighlight %}

The same method for other directory, as well.

{% highlight bash %}
% cd ~/.local/share/fonts/i18n
% cp ~/build/UnnamedFukidashi/takao-fonts-ttf/*.ttf .
{% endhighlight %}

Now you can check using <code>fc-list</code>

{% highlight bash %}
% fc-cache
% fc-list | grep -i some
/home/epsi/.local/share/fonts/icons/fontawesome-webfont.ttf: FontAwesome:style=Regular
{% endhighlight %}

![Font: fc-cache fc-list][image-ss-fc-list]{: .img-responsive }

-- -- --

### True Type Font

Installing TTF is easy, as example above.

Now you should see the font in some application:

#### Libreoffice

![Truetype Font: Libreoffice][image-ss-libreoffice]{: .img-responsive }

#### Inkscape

![Truetype Font: Inkscape][image-ss-inkscape]{: .img-responsive }

#### Lemonbar

{% highlight bash %}
% echo -e ' \uf015' | lemonbar -g "640x24+0+0" -u 2 \
-B '#aaffffff' -F '#ff000000' -p -f "FontAwesome"
{% endhighlight %}

![Truetype Font: Lemonbar][image-ss-lemonbar]{: .img-responsive }

#### XFCE4 Terminal

I have to logout, before I can show this in my <code>zsh</code>.

{% highlight bash %}
% echo -e '\uf015' 
{% endhighlight %}

![Truetype Font: XFCE4-Terminal][image-ss-terminal]{: .img-responsive }

-- -- --

### Bitmap Font

Consider have a look at <code>siji</code> as our example.

There are a few more step required for bitmap fonts.

*	<code>mkfontdir</code>, and <code>mkfontscale</code>

*	<code>xset fp+</code>

#### Notation

As you might already notice.
XLFD using exotic notation.

{% highlight bash %}
font_siji='-wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1'
{% endhighlight %}

#### Copy and Paste

Copy paste manually.

{% highlight bash %}
% mkdir ~/.local/share/fonts/misc
% cd ~/.local/share/fonts/icons
% cp ~/build/siji/pcf/siji.pcf .
{% endhighlight %}

![XLFD: Siji Copy][image-ss-siji-copy]{: .img-responsive }

#### Make Font

You need other tools,
it is <code>mkfontdir</code>
and <code>mkfontscale</code>.

{% highlight bash %}
% mkfontdir

% mkfontscale

% ls
fonts.dir  fonts.scale  siji.pcf

% cat fonts.dir
1
siji.pcf -wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1

% cat fonts.scale
0
{% endhighlight %}

![XLFD: mkfontdir mkfontscale][image-ss-siji-mkfont]{: .img-responsive }

#### Display Font

Now you can check using <code>xfd</code>

{% highlight bash %}
% xfd -fa siji
{% endhighlight %}

![XLFD: Siji XFD][image-ss-siji-xfd]{: .img-responsive }

#### Font Cache

{% highlight bash %}
% fc-cache -f -v
...
/home/epsi/.local/share/fonts/misc: skipping, existing cache is valid: 1 fonts, 0 dirs
...

% fc-list | grep siji
/home/epsi/.local/share/fonts/misc/siji.pcf: Siji:style=Regular
{% endhighlight %}

#### Add Font Path

This step, you should do, or the font won't shown up in Lemonbar.

{% highlight bash %}
% xset fp+ ~/.local/share/fonts/misc
{% endhighlight %}

![XLFD: Siji Set X Font Path][image-ss-siji-xset-fp]{: .img-responsive }

#### Select Font

Using XLFD

{% highlight bash %}
% xfontsel
{% endhighlight %}

![XLFD: Getting XLFD with xfontsel][image-ss-siji-xfontsel]{: .img-responsive }

#### Lemonbar

{% highlight bash %}
% echo -e ' \ue018' | lemonbar \
  -g "640x24+0+0" -u 2 -B '#aaffffff' -F '#ff000000' -p \
  -f "-wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1"
{% endhighlight %}

![XLFD: Siji at Lemonbar][image-ss-siji-lemonbar]{: .img-responsive }

-- -- --

### System Wide

Sometimes we need to use the font in system wide,
using <code>/usr/share/fonts</code>.

#### Copy and Paste

{% highlight bash %}
% cd /usr/share/fonts/misc

% sudo cp ~/.local/share/fonts/misc/siji.pcf .
Password: 

% sudo mkfontdir
% sudo mkfontscale

% fc-cache
% fc-list | grep siji
/home/epsi/.local/share/fonts/misc/siji.pcf: Siji:style=Regular
/usr/share/fonts/misc/siji.pcf: Siji:style=Regular
{% endhighlight %}

You need to logout and relogin in order to use this bitmap font.

-- -- --

### Font Tools

You can also browse a character in a font with either of these program, 

* <code>xffd</code>, 

* <code>unibrow</code>, or 

* <code>gucharmap</code>.

-- -- --

### Font to Use

I also ask some friends,
in [@dotfiles_id](https://t.me/dotfiles_id) telegram group,
about their favorites font. I would like to summarize here:

	Fonts @ dotfiles_id

#### Icon Font/ Glyph Bar

* [FontAwesome](https://fontawesome.com/)

* [Siji](https://github.com/stark/siji)

* [ionicons](http://ionicons.com/)

* [icomoon](https://icomoon.io/)

* [Powerline Symbol](http://powerline.readthedocs.io/en/master/installation/linux.html)

* [Powerline Extra Symbol](https://github.com/ryanoasis/powerline-extra-symbols)

#### Bar (Text)

* Terminus

* Roboto

#### Libreoffice

* Carlito (Calibri substitution)

* Caladea (Cambria substitution)

#### Design (Inkscape)

* Oswald (Google Font)

* Raleway (Google Font)

#### Web

* Default: arial, helvetica, sans 

#### Terminal

* Iosevka, Monofur, Tewi, ShureTech Mono, mPlus

* [IBM Plex](https://github.com/IBM/type)

* [Hermit](https://pcaro.es/p/hermit/)

#### Text Editor (Coding)

* Hack, Ubuntu Mono, Fira Code

* Monaco, Meslo

### Links:

* [Font to use in Terminal Emulator](https://www.slant.co/topics/7014/~fonts-to-use-in-a-terminal-emulator)

* [Free Font for 2017](https://www.creativebloq.com/features/10-new-free-fonts-for-2017)


-- -- --

### Conclusion

It works.

Thank you for reading and visiting.


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/02' %}

[image-ss-fc-list]:       {{ asset_path }}/font-fc-list.png
[image-ss-inkscape]:      {{ asset_path }}/font-inkscape.png
[image-ss-libreoffice]:   {{ asset_path }}/font-libreoffice.png
[image-ss-lemonbar]:      {{ asset_path }}/font-lemonbar.png
[image-ss-terminal]:      {{ asset_path }}/font-xfce4-terminal.png
[image-ss-siji-copy]:     {{ asset_path }}/font-siji-copy.png
[image-ss-siji-mkfont]:   {{ asset_path }}/font-siji-mkfont.png
[image-ss-siji-xset-fp]:  {{ asset_path }}/font-siji-xset-fp.png
[image-ss-siji-xfd]:      {{ asset_path }}/font-siji-xfd.png
[image-ss-siji-xfontsel]: {{ asset_path }}/font-siji-xfontsel.png
[image-ss-siji-lemonbar]: {{ asset_path }}/font-siji-lemonbar.png
