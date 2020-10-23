---
layout: post-sidemenu-wm
title:  "openSUSE Tumbleweed Tiling Experience"
categories: desktop
date      : 2018-01-02 09:25:15 +0700
tags      : [install]
keywords  : [opensuse, tumbleweed, tiling]
author: epsi

excerpt:
  Tiling window manager experience in openSUSE.

related_link_ids:
  - 16080325  # Install i3 Arch
  - 16080224  # Install i3 Debian

---

This 2017, I accidentaly moved from Arch to openSUSE. It just happen without
planning. I must admit, my plan is Gentoo. I know this article is a little bit too average, and the screenshot is also
boring. Boring is good for business. I'm a little bit of running out of time,
so I'm a kind of in need of stable OS to get my job done.

Two times in 2017, my 2009 old notebook has some hardware issue. The first
time is in August, this event pushed me to use other computer other than my notebook. So I installed
tumbleweed at home multiboot, along with my previously installed Mageia, and I
installed [tumbleweed, rawhide, and KaOS] in my office multiboot along with previously installed Debian. After my notebook get better form the service shop, I switch from Manjaro to Artix and from Debian to Devuan.

My second hardware issue is in December, pushed me to use my openSUSE
again. So I decide to get the environment right for me. In my office, I set up
administrative thing such as samba. And live happily with KDE. At home, I set
up window manager. I'm not really a fans of Desktop Ricing, but I love working
Tiling Window Manager. And surprisingly openSUSE repository is good.

[![tumbleweed: i3][image-ss-i3]{: .img-responsive }][photo-ss-i3]

Well, it is my opinion as a Debian user and also and AUR user. So here is what I just did

Download from repository with zypper

* xmonad herbstluftwm awesome bspwm i3-gaps 

* mpd cava ncmpcpp mpclient cmus

* scrot feh nitrogen scrot compton xcompmgr

* rofi dunst dzen2

* lxappearance conky xfontsel gucharmap  

* oh-mybash, oh-my-zsh, fish, powerline

* vim, emacs, vim-plugin-NERDtree


Download rpm manually then zypper

* lemonbar

* termite


Installer for specific language

* TERM::Extended-Color (via cpan)

* ls++ (git clone)

* gtop (via npm)

* qtile (via pip)

* jekyll (via gem) to blog


Copy paste manually form Arch

* from .fonts in Arch Linux to .fonts in openSUSE

Not solved yet (I'm busy) (maybe compile manually later)

* polybar

I had setup a few, such as i3, it works flawlessly. And surprisingly it does
not take a long time to setup. Awesome works well, but I still have trouble
login to bspwm.

-- -- --

### Feliz Año Nuevo

This new year, I also give up my geany for ViM and emacs.
It is just me that need to learn.

[![tumbleweed: Vim and emacs][image-ss-editor]{: .img-responsive }][photo-ss-editor]

And I also learn to communicate in some spanish telegram linux group, with help of
google translate. Larga Vida Resistencia.

I guess I'm going to continue using openSUSE.

Welcome 2018

-- -- --

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Update:</strong>I also replicate the case to my Leap in my Office,
  on February 2018.
</div>

### Tumbleweed to Leap

In openSUSE context, I grown up quickly.
Now I need stability in my Office, so switch my Tumbleweed to Leap 42.3.
You can say that I have migrated successfully,
with almost no issue at all.

#### From Scratch

I would like to make a guidance,
so uninstall what I have installed before.

{% highlight bash %}
% sudo zypper rm chromium ffmpeg inkscape blender Mesa-demo-x xfce4-mixer nethogs iftop dstat atop iotop nmon lua ghc nodejs go i3status i3lock i3-gaps herbstluftwm sxhkd bspwm xmonad openbox  xmobar dmenu dzen2 lemonbar tint2 rxvt-unicode lxappearance termite conky mc  vim tmux powerline dunst rofi w3m parcellite screenfetch feh nitrogen compton xcompmgr scrot transset cmus mpd cava
{% endhighlight %}

Now 'm installing using KDE.
Using no tiling yet.

#### Change Repository From Tumbleweed to Leap

{% highlight bash %}
% sudo mv repos.d/* repos.d.old/
% sudo zypper ar http://download.opensuse.org/distribution/leap/42.3/repo/oss/ repo-leap
% sudo zypper ar http://download.opensuse.org/update/leap/42.3/oss/ repo-update
% sudo mv repos.d.old/packman.repo repos.d/packman.repo

% sudo zypper ref
% sudo zypper dup
{% endhighlight %}

[![Leap: Switch from Tumbleweed][image-ss-leap]{: .img-responsive }][photo-ss-leap]

You can see these packages are already installed: lua sddm lightdm openbox vim w3m xfce4-mixer .

#### Official Repository

I ned suitable working environment,
so I also install stuff that I need.

{% highlight bash %}
% sudo zypper in chromium ffmpeg vlc xfce4-mixer
% sudo zypper in inkscape blender Mesa-demo-x
% sudo zypper in nethogs iftop dstat atop iotop nmon
% sudo zypper in ghc nodejs go python3-pip ruby2.1-rubygem-bundler
% sudo zypper in i3status i3lock i3
% sudo zypper in sxhkd bspwm xmonad xmobar awesome
% sudo zypper in dmenu dzen2 lemonbar tint2 
% sudo zypper in fish zsh rxvt-unicode lxappearance conky mc
% sudo zypper in dunst rofi parcellite screenfetch tmux powerline
% sudo zypper in feh nitrogen compton xcompmgr scrot transset
% sudo zypper in cmus mpd cava
{% endhighlight %}

#### X11 Repository

{% highlight bash %}
% sudo zypper ar http://download.opensuse.org/repositories/X11:/windowmanagers/openSUSE_Leap_42.3/X11:windowmanagers.repo
% sudo zypper ref
% sudo zypper in i3-gaps obmenu herbstluftwm 
{% endhighlight %}

#### Using YaST2 using Web Browser

Found in https://software.opensuse.org: One Click Install using YaST2

* glances byobu termite

-- -- --

### No Repository

However I still have to install manually using some tools.

#### Using Git 

Manual Compilation.
Successfully installed:

* neofetch

* dwm

Some dependencies.

{% highlight bash %}
% sudo zypper in libX11-devel libXinerama-devel
{% endhighlight %}

#### Via Gem

* teamocil

#### Some Riddles

A few more Riddles.
Manual git (fail): 
 
*	ncmpcpp 

{% highlight bash %}
$ sudo zypper in autoconf automake libtool
$ sudo zypper in gcc6 gcc6-c++

$ ./configure
configure: error: Your compiler doesn't seem to support C++14, please upgrade (GCC >= 5)
{% endhighlight %}

*	ncmpcpp (using Leap 42.1 rpm)

{% highlight bash %}
	ncmpcpp: symbol lookup error: ncmpcpp: undefined symbol: _ZN6TagLib6String14WCharByteOrderE
{% endhighlight %}

*	polybar

{% highlight bash %}
% sudo zypper install clang cmake

% cmake ..
Compiler not supported (Requires clang-3.4+ or gcc-5.1+)
{% endhighlight %}

*	bwmon 

I haven't got time to solve.

-- -- --

### Conclusion

Most of all works.

Thank you for reading and visiting.


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets-desktop/2018/01' %}

[image-ss-i3]: {{ asset_path }}/opensuse-i3.png
[photo-ss-i3]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipM0UianivdO-I2orCzgEb1XoJNcmSxpRAWZamq9

[image-ss-editor]: {{ asset_path }}/opensuse-i3-vim-emacs.png
[photo-ss-editor]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipOBeN0tqWvbr1N-V2NxtCOJnup8ly6DCIKt1tZe

[image-ss-leap]: {{ asset_path }}/opensuse-kde-to-leap.png
[photo-ss-leap]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNo8WF2idScXj5RgSxD2SZogAWgth5gatPrQudf?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
