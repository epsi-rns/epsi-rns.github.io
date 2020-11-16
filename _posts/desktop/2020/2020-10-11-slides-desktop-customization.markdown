---
layout    : post
title     : "Slides - Desktop Customization"
categories: desktop
date      : 2020-10-11 11:25:15 +0700
tags      : [presentation]
keywords  : [tiling, window manager, theme, terminal]
author: epsi

opengraph:
  image: /assets-desktop/2020/customization-terminal.png
excerpt:
  Introduction to Desktop Customization in Linux/BSD.

---

### The Document

This article contain all article slides.
Slide by slide, so anyone can copy desired slide,
without the need to open the Impress slide, or the PDF version.

![Teaser Preview: Desktop Customization][teaser-preview]

I believe, a ready to use slide,
is useful to help people in the group discussion.

#### Original Presentation

I usually draft a presentation in a simple text based,
that I can write down in any text editor.

Then I show in web based for a few moment (actually months).
This give time to append any additional material freely,
create necessary figure, and also correct any typo, or even dead links.

You can watch the presentation here:

* [Presentation - Desktop Customization][local-presentation]

This text based presentation is the most authentic version,
with all the links required provided.

#### Impress

This presentation is made in [LibreOffice Impress][libreoffice-impress],
using [candyclone template][candyclone-template].

You can download the Impress document from a page in here:

* [presentation-desktop-customization-diagram.odp][document-impress]

With this downloadable document,
you can use the style of this presentation,
for use with your own presentation works,
either for school material, office works, homeworks, or anything else.

#### PDF

You can also download the exported pdf from a page in here:

* [presentation-desktop-customization-diagram.pdf][document-pdf]

The pdf version cannot contain animation.
So the content might be little different,
compared with the original Impress document.

#### Inkscape

The Impress pages also utilize diagram illustration made in Inkscape.
I also provide the source image in SVG format,
that you can download from a page in here:

* [presentation-desktop-customization-content.svg][document-inkscape]

Here is the preview.

![Inkscape Illustration: Desktop Customization][inkscape-thumbs]

I intentionally share the source SVG image,
so you can alter the content for your own personal use.
I know there are already so many stock images for presentation,
I just need to share, what I have, with tutorial.
This way, anyone can make pretty presentation easier.
Of course you still have to learn Inkscape,
to suit the illustration for your own use.

#### Template

> What is this candyclone rubbish?

Candyclone is an Impress template that I have made for LibreOffice contest.

* [LibreOffice Impress Template Contest by the Indonesian Community][template-contest]

There are also other free templates as well in `lumbung` repository.

* [Lumbung LibreOffice Indonesia Repository][template-lumbung]
 
This candyclone is just an example template that you can use freely.
With this candyclone example you can also learn how to make your own template.
Your very own template to suit your ecosystem.

#### Disagreement

> What if I do not agree?

The source is available, so you can freely make your own slide.
Feel free to express your thoughts, either with text, or illustration.

-- -- --

### The Slides

Here I represent all the slides.

#### Slide 01: Cover

![Slide - Cover][slide-01]

Introduction to Desktop Customization in Linux/BSD.

#### Slide 02: About The Author

![Slide - About Author][slide-02]

> I have my own dotfiles repository.

#### Slide 03: About This Material

![Slide - About Material][slide-03]

After watching this, you will understand:

* The difference between GUI in linux or BSD.
* What customization can be done in linux or BSD.
* Choices to leverage your desktop appealing.

So that you can pick what customization suitable
for you for daily basis usage.

This material contain a bunch of terminal screenshots.
It does not mean that your daily desktop is all about terminal.

#### Slide 04: Chapter Break

![Slide - Modular Linux][slide-04]

#### Slide 05: How Modular is Linux

![Slide - Modular Linux: How Modular is Linux?][slide-05]

How Modular is Linux?

1. Package Manager:
   * APT, ALPM, DNF, XBPS, Zypper, Portage.

2. Init:
   * SysV, systemd, openRC, runit, S6.

3. Filesystem:
   * ext4, XFS, Reiserfs, BTRFS, ZFS.

4. Standard C library in OS:
   * glibc or musl.

5. DE (Desktop Environment):
   * GTK+ based, QT based, enlightenment.

6. WM (Window Manager):
   * Stacking, Tiling, Dynamic, Compositor.

#### Slide 06: Presentation Reference: Linux Diversity

![Slide - Modular Linux: Linux Diversity Presentation][slide-06]

Presentation:

> Dive into Linux Subsystem for Personal Educational Purpose.

* [Learning Linux Diversity](/system/2020/10/11/slides-linux-diversity.html)

#### Slide 07: Chapter Break

![Slide - Customization][slide-07]

#### Slide 08: Part of Desktop Art

![Slide - Customization: Part of Desktop Art][slide-08]

What Customization?

* DE/WM Customization:
  * window manager, panel, notification and the most ingredient called wallpaper.

* Terminal Customization:
  * shell, prompt, pixel-art, and multiplexer.

Terminal ricing along with CLI application,
are part of desktop [ricing](https://en.wikipedia.org/wiki/Rice_burner).

#### Slide 09: The Difference Between DE and WM

![Slide - Customization: The Difference Between DE and WM][slide-09]

The Difference: DE or WM?

* Desktop Environment:
  * Full Experience: Panel, File Manager, Application.

* Stacking Window Manager:
  * Stripped Down Version of Desktop Environment.

* Tiling Window Manager:
  * Non-overlapping screen real estate.

When you customize a Window Manager,
you are basically making your own Desktop Environment.

#### Slide 10: Desktop Environment Example: Cinnamon

![Slide - Customization: DE Example: Cinnamon][slide-10]

The good Cinnamon (GTK+3) based with Cinnamon's 3D Switcher.

#### Slide 11: DE/WM Categorization

![Slide - Customization: DE Customization][slide-11]

Link:

* [The Difference Between DE, Shell, WM and Compositor](/desktop/2016/06/13/de-wm-shell-difference.html)

DE/WM Categorization

* GTK+ based Desktop Environment:
  * XFCE4, Gnome shell, Cinnamon, Mate, Budgie.

* QT based Desktop Environment:
  * KDE Plasma, LXQT, Deepin.

* DE with Other Library:
  * Enlightenment, Lumina, or such.

* Stacking Window Manager:
  * Openbox, Fluxbox.

* Tiling Window Manager:
  * AwesomeWM, XMonad, i3wm (gaps), BSPWM, HerbsluftWM, DWM, or many others.

* Compositor:
  *  KWin, Compiz.

> The separation is not very clear, sometimes overlapped.

#### Slide 12: Desktop Customization Step by Step?

![Slide - Preface: Desktop Customization Step by Step][slide-12]

Desktop Customization Step by Step?

1. Install Linux Distribution
   * Pick your favorite distribution.
   * Debian Based, Arch Family, Void, or others.

2. Desktop Environment
   * Plasma, Gnome, XFCE4, and such.
   * Explore Theming.

3. Terminal Customization
   * Terminal, shell, prompt, multiplexer (tmux), pixel art,
     compositor (picom), CLI app (neofetch), editor (ViM).

4. Stacking Window Manager
   * Configuration: Openbox, Fluxbox.
   * Explore Panel (task bar), and Tools.

5. Tiling Window Manager
   * i3wm, AwesomeWM, BSPWM,
   * DWM, XMonad, HerbstluftWM.

> Learn how to google, make a screenshot (scrot), read documentation, and english.

Tips:

* Choose good wallpaper, fonts, and colorscheme.

* Enjoy the journey. Share your .dotfiles.

#### Slide 13: Desktop Environment Customization

![Slide - DE Customization][slide-13]

#### Slide 14: DE: LXQT Example

![Slide - DE: LXQT Example][slide-14]

Link:

* [LXQT with Windowish Looks, No Kidding](/desktop/2014/10/19/lxqt-windowish-looks.html)

You can switch to any looks with parts from internet.

#### Slide 15: DE: What can I do with my DE?

![Slide - DE: What can I do with my DE][slide-15]

What can I do with my DE?

* Theming: Taskbar and Border.
* Color Scheme.
* Window Animation.
* Cursor and Icon Collection.
* Context Menu in File Manager.
* Wallpaper: _The most ingredient for success_.
* Other Stuff: Font, Conky, or such.

#### Slide 16: DE Customization Example

Example: Custom XFCE4 Theme: PNG Gradient over XPM

![Slide - DE: XFCE4 Theme: PNG Gradient Over XPM][slide-16]

Link:

* [XFWM4 Theme - Overview](/desktop/2018/03/20/xfwm4-theme.html)

#### Slide 17: DE Customization Example

Example: Custom XFCE4 Theme: SVG Inkscape Source

![Slide- DE: XFCE4 Theme: SVG Inkscape Source][slide-17]

> UI/UGM (User Interface/ Unified Graphic Material).

The previous window icon was designed easily using inskcape.

#### Slide 18: Chapter Break

![Slide - Window Layout][slide-18]

#### Slide 19: Window Layout

![Slide - Layout: What Layout][slide-19]

What Layout is Available with Your DE/WM?

* Stacking: Suitable for ricing.
* Tiling: For masochist desktoper. Avoid n00b.
* Floating: Enable stacking in tiling.

#### Slide 20: Layout: Stacking

Stacking Layout Example: LXQT

![Slide - Layout: Stacking Layout Example: LXQT][slide-20]

Link:

* [LXQT with Windowish Looks, No Kidding](/desktop/2014/10/19/lxqt-windowish-looks.html)

#### Slide 21: Layout: Tiling

Tiling Layout Example: Herbstluftwm

![Slide - Layout: Tiling Layout Example: Herbstluftwm][slide-21]

#### Slide 22: Layout: Floating

Floating Example: AwesomeWM

![Slide - Layout: Floating Example: AwesomeWM][slide-22]

#### Slide 23: Chapter Break

![Slide - Ricing][slide-23]

#### Slide 24: Ricing Guidance

![Slide - Ricing: Ricing Guidance][slide-24]

Ricing Guidance

* Aesthetic is First class:
  * It is a work of art.

* Technical Difficulty is Appreciated:
  * Basic knowledge is a must.

* Everyone has their own personal character:
  * Yes, there is culture.
  * Many factions blended in,
    such as dark mode, or weebs, or flat color vs transparency.

* Ricing people prefer WM over DE:
  * More customization to avoid hardcoded looks.

* Be creative, be different:
  * Beautiful.

> Tips: Find your own style!

#### Slide 25: Is It Worth It?

![Slide - Ricing: Is It Worth It?][slide-25]

Is it worth it?

* Socially:
  * You need to show of, and get some votes.

* Technically:
  * You need a good screenshot
    while _communicating your issue_ on social media,
    or _publishing your work_ in a blog or other media.

* Learning:
  * A fun case while studying OS, and also coding.

* Community: Give a fun direction for n00b
  * that showing of ricing screenshot can be a better life,
    instead of just showing of illegal hacking.

#### Slide 26: Chapter Break

![Slide - WM: Customization][slide-26]

#### Slide 27: Window Manager Parts

![Slide - WM: Window Manager Parts][slide-27]

Window Manager Customization?

* Window Manager itself:
  * Choose your WM.

* Panel:
  * Polybar, dzen2, Lemonbar, or others.

* Notification:
  * Dunst, or others.

* Launcher:
  * dmenu, rofi, or others.

* Original Wallpaper:
  * Ricing Culture: Mostly Anime.

* Make your own authentic stuff.

> Tips: There is no wrong or right. It is all about choices.

#### Slide 28: Example: Custom Standalone Panel

![Slide - WM: Custom Standalone Panel][slide-28]

Link:

* [Standalone Lemonbar Using Conky](/desktop/2017/04/14/standalone-lemonbar-conky.html)

#### Slide 29: Optional Tools

![Slide - WM: Optional Tools][slide-29]

Links:

* Wallpaper Manager: 

* Good Font Installation: 

Optional tools?

* Screenshooter:
  * scrot or others.

* Compositor Manager:
  * picom or compton or xcompmgr.

* Wallpaper Manager:
  * Nitrogen or feh:
    [Openbox Install - Common Utility](/desktop/2018/07/16/openbox-install.html)

* Good Font Installation:
  * FontAwesome, siji, terminus:
    [Linux - Installing Font](/desktop/2018/02/04/installing-font.html)

* Logging Out:
  * OB Menu or others.

* LXAppearance

#### Slide 30: Custom Material Wallpaper

![Slide - WM: Example Genuine Wallpaper][slide-30]

Link:

* [More Inkscape Wallpapers](https://www.deviantart.com/nurwijayadi/gallery/61081890/inkscape-wallpaper)

* [Wallpaper Repository](https://github.com/epsi-rns/isometric-wallpaper)

#### Slide 31: Chapter Break

![Slide - Stacking: Window Manager][slide-31]

#### Slide 32: Why Stacking Window Manager?

![Slide - Stacking: Why Stacking WM][slide-32]

Why Stacking Window Manager?

* More on Customization:
  * Configuration in Text Editor (suitable for coder),
  * Shareable with dotfiles

* Lightweight:
  * Small Memory Footprint.
  * Install only what I need.

* The Looks:
  * Avoid hardcoded looks or just be simple.

#### Slide 33: Example Oldschool Openbox

![Slide - Stacking: Example Oldschool Openbox][slide-33]

#### Slide 34: More Articles about Stacking WM

![Slide - Stacking: More Articles][slide-34]

Links:

* [Openbox Configuration](/desktop/2018/05/01/openbox-config.html)

* [Openbox Tint2 Panel](/desktop/2018/05/15/tint2-config.html)

* [Openbox Theme](/desktop/2018/04/15/openbox-theme.html)

* [Fluxbox Overview](/desktop/2018/04/01/fluxbox-overview.html)

#### Slide 35: Example Openbox Theme: Multi Color

![Slide - Stacking: Example Openbox Theme][slide-35]

Link:

* [Openbox Theme - Special Trick](/desktop/2018/04/18/openbox-theme.html)

#### Slide 36: Example Openbox Theming Using Inkscape

![Slide - Stacking: Openbox Inkscape Theming][slide-36]

Link:

* Example: [openbox theming with Inkscape](/desktop/2018/04/16/openbox-theme.html)

#### Slide 37: Example Oldschool Fluxbox

![Slide - Stacking: Example Oldschool Fluxbox][slide-37]

Link:

* Example: [Fluxbox](/desktop/2014/11/08/fluxbox-with-glowing-wallpaper.html)

#### Slide 38: Chapter Break

![Slide - Tiling: Window Manager][slide-38]

#### Slide 39: Why Tiling Window Manager?

![Slide - Tiling: Why Tiling WM][slide-39]

Why Tiling Window Manager?

* Efficient:
  * Non-overlapping screen real estate.

* Scaling:
  * With Workspace or Tags.

* Customizable:
  * Most parts can be automated.

* Keyboard-driven workflow:
  * For masochist-desktoper.

* Lightweight:
  * Compared to DE.

#### Slide 40: Good Watch

> Aline Abler Presentation on Youtube

![Slide - Tiling: Aline Abler Video][slide-40]

Good Watch: [Aline Abler Presentation on Youtube](https://www.youtube.com/watch?v=Api6dFMlxAA)

Explained in systematic fashioned,
easy to follow,
and she is pretty too.

#### Slide 41: What Tiling Window Manager?

![Slide - Tiling Choices][slide-41]

What Tiling Window Manager?

* i3wm:
  * Simple configuration, suitable for beginner.

* HerbstluftWM:
  * Challenging technical difficulties.
  * The extra frame almost make me cry for happiness.

* XMonad:
  * I can config, but weird, I still don't understand Haskell.

* BSPWM

#### Slide 42: Dynamic Window Manager?

![Slide - Dynamic: Window Manager][slide-42]

> It is a kind of Tiling WM, with presets layouts.

Dynamic Window Manager?

* Awesome WM:
  * Easy to use for beginner, but very long config.
  * Built in menu, systray, notification.

* DWM:
  * First install for any minimalist distro.
  * You have to compile to change the behaviour.

#### Slide 43: Presets Layout (in animation)

Example of presets layout: AwesomeWM.

![Slide - Dynamic: Presets Layout][slide-43]

#### Slide 44: Example Bright i3-gaps WM with i3status

Example: i3wm (gaps, bright)

![Slide - Tiling: Example i3wm with i3status][slide-44]

Link:

* [Modularized Conky Configuration for i3status](/desktop/2016/08/01/modularized-i3status-conky-lua-json.html)

#### Slide 45: Example AwesomeWM

Example Tiling WM: AwesomeWM in GIF animation

![Slide - Dynamic: Example AwesomeWM Fix Width][slide-45]

Link:

* Example: [AwesomeWM](/desktop/2019/06/18/awesome-modularized-binding.html)

#### Slide 46: Dynamic: More About AwesomeWM

![Slide - Dynamic: More About AwesomeWM][slide-46]

Links:

* [Article: Awesome WM - Overview](/desktop/2019/06/15/awesome-overview.html)

* [Presentation: Awesome WM - Modularized](/desktop/2019/12/01/awesome-presentation-modularized.html)

* [Presentation: Awesome WM - Theming](/desktop/2019/12/02/awesome-presentation-theme.html)

* [Presentation: Awesome WM - Statusbar](/desktop/2019/12/03/awesome-presentation-statusbar.html)

#### Slide 47: Tiling Example: Herbstluftwm

![Slide - Tiling: Herbstluftwm][slide-47]

Link:

* Example: [HerbstluftWM](/desktop/2017/06/30/herbstlustwm-summary.html)

#### Slide 48: Solving HerbstluftWM Technical Difficulties

![Slide - Tiling: herbstluftwm: Technical Difficulties][slide-48]

Links:

* [Modularized HerbstluftWM Overview](/desktop/2017/05/01/herbstlustwm-modularized-overview.html)

* [HerbstluftWM Tag Status Overview](/desktop/2017/06/01/herbstlustwm-tag-status-overview.html)

* [HerbstluftWM Idle Event Overview](/desktop/2017/06/11/herbstlustwm-event-idle-overview.html)

* [Summary: HerbstluftWM in Seven Languages](/desktop/2017/06/30/herbstlustwm-summary.html)

#### Slide 49: Combining with Specific Panel Bar

![Slide - Tiling: Specific Panel Bar][slide-49]

Link:

* Example: [Dzen2 in XMonad](/desktop/2016/05/11/xmonad-with-conkyless-dzen.html)

> WM customization can be combined with Specific Panel Bar.

#### Slide 50: Chapter Break: Xorg and Wayland

![Slide - Other DE/WM Issue][slide-50]

> Xorg and Wayland

#### Slide 51: Chapter Break

![Slide - Terminal Customization][slide-51]

#### Slide 52: Example of Terminal Customization

![Slide - Terminal: Example Customization][slide-52]

Link:

* [Linux - Terminal Customization](/desktop/2018/08/07/terminal-ricing.html)

> Terminal Ricing: tmux + vim + gcc + C + Assembler

#### Slide 53: Parts of Terminal Ricing

![Slide - Terminal: Ricing Parts][slide-53]

Link:

* [Terminal Ricing](/desktop/2018/08/07/terminal-ricing.html)

Main Parts.

1. Terminal:
  * urxvt, xfce4-terminal, termite.

2. Shell:
  * bash, zsh, fish.
  * Prompt: powerline, oh-my-bash, oh-my-zsh, oh-my-fish.

3. Multiplexer:
  * tmux, gnu screen.
  * wrapper: teamocil, byobu.

4. Background Decoration:
  * Wallpaper.

Additional Parts.

1. Compositor Decoration:
  * Picom or Compton.

2. Padding Decoration:
  * gtk.css.

3. Example CLI application:
  * neofetch, htop, cava.

4. Special CLI application:
  * ViM Text Editor.

5. Pixel Art

#### Slide 54: Example Pixel Art

Example Terminal Ricing: Pixel Art: ANSI Color

![Slide - Terminal: Pixel Art][slide-54]

Link:

* Example: Pixel Art Repository: [ANSI color](https://bitbucket.org/epsi/ansi-color)

#### Slide 55: Example Command Line Interface

![Slide - Terminal: Standalone CLI][slide-55]

Link:

* [Standalone CLI System Monitoring Using Conky](/desktop/2017/04/12/standalone-cli-conky.html)

#### Slide 56: Example Shell Prompt

![Slide - Terminal Shell Prompt][slide-56]

Link:

* [Customizing Shell Prompt with Powerline](/desktop/2016/03/21/powerline-customizing.html)

#### Slide 57: Chapter Break

![Slide - Dotfiles][slide-57]

#### Slide 58: Where to Find Resources?

![Slide - Dotfiles: Resources][slide-58]

Links:

* Blog
  * Example: [epsi-rns.github.io](https://epsi-rns.github.io/),
    [bandithijo.github.io](https://bandithijo.github.io/)
  * Local Guidance: [Loonix-Newfag](https://gegenokitaro.github.io/talk/2014/01/30/Loonix-Newfag/)

* [dotshare.it](http://dotshare.it/) (this is not a dead site).

* Example Dotfiles Repository on github or gitlab:
  * [gitlab.com/epsi-rns/dotfiles](https://gitlab.com/epsi-rns/dotfiles)
  * [github.com/addy-dclxvi](https://github.com/addy-dclxvi)
  * [github.com/okitavera](https://github.com/okitavera)
  * [github.com/myugan](https://github.com/myugan/awesome-linux-customization)

* Community: Example Facebook Group:
  * [Linuxer Desktop Art](https://www.facebook.com/groups/linuxart)

* Community: Example Telegram Group:
  * [@dotfiles_id](https://t.me/dotfiles_id),
    [@dotfiles_id_channel](https://t.me/dotfiles_id_channel)
  * [@window_managers](https://t.me/window_managers)

#### Slide 59: Origin of the Dotfiles Word

![Slide - Dotfiles: Word Origin][slide-59]

Configuration files on linux can be shared between hobbyist.
Configuration mostly lies under _.config_ directory.
Or single files with name started with dot.

#### Slide 60: Managing Dotfiles

![Slide - Dotfiles: Managing Files][slide-60]

> Sharing Resources require good folder management.

#### Slide 61: Please Share!

![Slide - Dotfiles: Please Share][slide-61]

I share mine: [gitlab.com/epsi-rns/dotfiles](https://gitlab.com/epsi-rns/dotfiles).

_Would you do the same with each other?_

#### Slide 62: What's Next?

![Slide - What's Next?][slide-62]

What is Next?

* Make your own custom Statusbar Panel!
* Make your own custom Window Manager!
* Port old tools to a better programming language!

> Leverage Coding Skills!

#### Slide 63: Questions

![Slide - Questions?][slide-63]

> Don't be shy!

#### Slide 64: The End

![Slide - Thank You][slide-64]

> Thank you for your time.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = 'https://epsi-rns.github.io/assets-desktop/2020/customization' %}
{% assign dotfiles = 'https://github.com/epsi-rns/berkas2/blob/master/impress-presentation/02-desktop-customization' %}

[teaser-preview]:           https://epsi-rns.github.io/assets-desktop/2020/customization-terminal.png
[local-presentation]:       /desktop/2020/01/20/presentation-desktop-customization.html
[candyclone-template]:      https://epsi-rns.gitlab.io/design/2020/09/21/inkscape-impress-slides-01/
[libreoffice-impress]:      https://www.libreoffice.org/discover/impress/
[document-impress]:         {{ dotfiles }}/presentation-desktop-customization-diagram.odp
[document-pdf]:             {{ dotfiles }}/presentation-desktop-customization-diagram.pdf
[document-inkscape]:        {{ dotfiles }}/presentation-desktop-customization-content.svg
[inkscape-thumbs]:          {{ asset_path }}/thumbs.png
[template-contest]:         https://blog.documentfoundation.org/blog/2020/10/12/libreoffice-impress-template-contest-by-the-indonesian-community/
[template-lumbung]:         https://lumbung.libreoffice.id/

[slide-01]: {{ asset_path }}/slide-01-cover.png
[slide-02]: {{ asset_path }}/slide-02-about-author.png
[slide-03]: {{ asset_path }}/slide-03-about-material.png
[slide-04]: {{ asset_path }}/slide-04-modular-linux.png
[slide-05]: {{ asset_path }}/slide-05-modular-linux-how-modular.png
[slide-06]: {{ asset_path }}/slide-06-modular-linux-presentation-diversity.png
[slide-07]: {{ asset_path }}/slide-07-customization.png
[slide-08]: {{ asset_path }}/slide-08-customization-part-of-desktop-art.png
[slide-09]: {{ asset_path }}/slide-09-customization-the-difference-de-wm.png
[slide-10]: {{ asset_path }}/slide-10-customization-de-example-cinnamon.png
[slide-11]: {{ asset_path }}/slide-11-customization-de-customization.png
[slide-12]: {{ asset_path }}/slide-12-de-learning-step-by-step.png
[slide-13]: {{ asset_path }}/slide-13-de-customization.png
[slide-14]: {{ asset_path }}/slide-14-de-lxqt-example.png
[slide-15]: {{ asset_path }}/slide-15-de-what-can-i-do-with-my-de.png
[slide-16]: {{ asset_path }}/slide-16-de-xfce4-theme-png-gradient-over-xpm.png
[slide-17]: {{ asset_path }}/slide-17-de-xfce4-theme-svg-inkscape-source.png
[slide-18]: {{ asset_path }}/slide-18-window-layout.png
[slide-19]: {{ asset_path }}/slide-19-layout-what-layout.png
[slide-20]: {{ asset_path }}/slide-20-layout-stacking-layout-example.png
[slide-21]: {{ asset_path }}/slide-21-layout-tiling-layout-example.png
[slide-22]: {{ asset_path }}/slide-22-layout-floating-example.png
[slide-23]: {{ asset_path }}/slide-23-ricing.png
[slide-24]: {{ asset_path }}/slide-24-ricing-guidance.png
[slide-25]: {{ asset_path }}/slide-25-ricing-is-it-worth-it.png
[slide-26]: {{ asset_path }}/slide-26-wm-customization.png
[slide-27]: {{ asset_path }}/slide-27-wm-window-manager-parts.png
[slide-28]: {{ asset_path }}/slide-28-wm-custom-standalone-panel.png
[slide-29]: {{ asset_path }}/slide-29-wm-optional-tools.png
[slide-30]: {{ asset_path }}/slide-30-example-genuine-wallpaper.png
[slide-31]: {{ asset_path }}/slide-31-stacking-window-manager.png
[slide-32]: {{ asset_path }}/slide-32-stacking-why-stacking-wm.png
[slide-33]: {{ asset_path }}/slide-33-stacking-example-oldschool-openbox.png
[slide-34]: {{ asset_path }}/slide-34-stacking-more-articles.png
[slide-35]: {{ asset_path }}/slide-35-stacking-example-openbox-theme.png
[slide-36]: {{ asset_path }}/slide-36-stacking-openbox-inkscape-theming.png
[slide-37]: {{ asset_path }}/slide-37-stacking-example-oldschool-fluxbox.png
[slide-38]: {{ asset_path }}/slide-38-tiling-window-manager.png
[slide-39]: {{ asset_path }}/slide-39-tiling-why-tiling-wm.png
[slide-40]: {{ asset_path }}/slide-40-tiling-aline-abler-video.png
[slide-41]: {{ asset_path }}/slide-41-tiling-choices.png
[slide-42]: {{ asset_path }}/slide-42-dynamic-window-manager.png
[slide-43]: {{ asset_path }}/slide-43-dynamic-presets-layout.png
[slide-44]: {{ asset_path }}/slide-44-tiling-example-i3wm-with-i3status.png
[slide-45]: {{ asset_path }}/slide-45-dynamic-example-awesomewm-fix-width.png
[slide-46]: {{ asset_path }}/slide-46-dynamic-more-about-awesomewm.png
[slide-47]: {{ asset_path }}/slide-47-tiling-herbstluftwm.png
[slide-48]: {{ asset_path }}/slide-48-tiling-herbstluftwm-technical-difficulties.png
[slide-49]: {{ asset_path }}/slide-49-tiling-specific-panel-bar.png
[slide-50]: {{ asset_path }}/slide-50-other-de-wm-issue.png
[slide-51]: {{ asset_path }}/slide-51-terminal-customization.png
[slide-52]: {{ asset_path }}/slide-52-terminal-example-customization.png
[slide-53]: {{ asset_path }}/slide-53-terminal-ricing-parts.png
[slide-54]: {{ asset_path }}/slide-54-terminal-pixel-art.png
[slide-55]: {{ asset_path }}/slide-55-terminal-standalone-cli.png
[slide-56]: {{ asset_path }}/slide-56-terminal-shell-prompt.png
[slide-57]: {{ asset_path }}/slide-57-dotfiles.png
[slide-58]: {{ asset_path }}/slide-58-dotfiles-resources.png
[slide-59]: {{ asset_path }}/slide-59-dotfiles-word-origin.png
[slide-60]: {{ asset_path }}/slide-60-dotfiles-managing-files.png
[slide-61]: {{ asset_path }}/slide-61-dotfiles-please-share.png
[slide-62]: {{ asset_path }}/slide-62-whats-next.png
[slide-63]: {{ asset_path }}/slide-63-questions.png
[slide-64]: {{ asset_path }}/slide-64-thank-you.png
