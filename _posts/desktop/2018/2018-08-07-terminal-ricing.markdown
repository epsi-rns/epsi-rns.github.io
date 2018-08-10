---
layout: post
title:  "Linux - Terminal Ricing"
categories: desktop
date:   2018-08-07 09:25:15 +0700
tags: [terminal, shell]
author: epsi

excerpt:
  Terminal ricing is different with desktop ricing.

---

### Preface

> Goal: Share common configuration for terminal ricing.

Terminal ricing is different in with desktop ricing.
While in desktop ricing we deal with window manager, panel, notification
and the most ingredient called wallpaper.
Terminal ricing deal with shell, prompt, pixel-art, and multiplexer.
Terminal ricing along with CLI application, are part of desktop ricing.

![Terminal Ricing: Gentoo XFCE4 Terminal][image-ss-gentoo-xfce4-terminal]{: .img-responsive }

	This guidance applied for most distribution.

#### Table of Content

There are few parts in terminal ricing:

*	Terminal: urxvt, xfce4-terminal, termite

*	Shell: bash, zsh, fish

*	Shell Prompt: powerline, oh-my-bash, oh-my-zsh, oh-my-fish

*	Multiplexer: tmux, gnu screen

*	Multiplexer Wrapper: teamocil, byobu

*	Compositor Decoration: Compton

*	Padding Decoration: gtk.css

*	Background Decoration: Wallpaper

*	Example CLI application: neofetch

*	Special CLI application: ViM Text Editor

*	Pixel Art

#### Dofiles Source

The dotfiles are available at:

*	[gitlab.com/epsi-rns/dotfiles/tree/master/terminal](https://gitlab.com/epsi-rns/dotfiles/tree/master/terminal)
	
#### Path

Where to put the config, for each part ?

| Category| Part | Path |
| :--- | :--- | :--- |
| terminal | urxvt  | ~/.Xresources, <br/> ~/.Xdefaults |
| terminal | termite  | ~/.config/termite/config |
| shell | oh-my-bash  | ~/.bashrc |
| shell | oh-my-zsh  | ~/.zshrc |
| shell | powerline  | ~/.config/powerline/*, <br/> ~/.bashrc, <br/> ~/.config/fish/config.fish |
| tiling | tmux | ~/.tmux.conf |
| tiling | teamocil | ~/.teamocil/jekyll.yml |
| decoration | compton | ~/.config/compton.conf |
| decoration | gtk-3.0 | ~/.config/gtk-3.0/gtk.css |
| application | vim | ~/.vim/*, <br/> ~/.vimrc |
| application | neofetch | ~/.config/neofetch/config.conf |

-- -- --

### Terminal

> Not all terminal emulator is suitable for ricing

There are as many choice as listed below:

*	[gentoo x11-terms](https://packages.gentoo.org/categories/x11-terms)

The issue is, not all terinal emulator is suitable for ricing.
In ricing world, less is more, no need any fancy border, scrollbar and menu.
But at the same time, the terminal needs flexbility when it comes to colorscheme.

Most choices comes to these three terminals:

*	[urxvt](http://software.schmorp.de/pkg/rxvt-unicode.html):
	My favorite is <code>rxvt unicode</code>,
	but sometimes it has trouble with font setting,
	such as arrow in ViM when using bold fonts.

*	[xfce4-terminal](https://docs.xfce.org/apps/terminal/start):
	It just works, and stable.
	It has GUI based configuration.
	But not many customizable colorscheme.

*	[termite](https://github.com/thestinger/termite):
	I rarely use it.
	This emulator is now widely used in most distro.
	And it also have compilation time dependencies with mono.

#### urxvt configuration

You can use two configs, or just one merged config:

| Config | Path |
| :--- | :--- |
| urxvt | ~/.Xresources, <br/> ~/.Xdefaults |

You can load using <code>xrdb</code>.

{% highlight bash %}
$ xrdb ~/.Xresources
{% endhighlight %}

Source:

*	[gitlab.com/.../dotfiles/.../xdefaults][dotfiles-xdefaults]

*	[gitlab.com/.../dotfiles/.../xresources][dotfiles-xresources]

My config are originally grabbed from these my two friends:

*	[M. Yuga Nugraha](https://github.com/myugan/dotfiles/blob/master/home/.Xresources)

*	[Nanda Vera](https://github.com/yuune/dotfiles/blob/master/.Xresources)

You can grab more color from [dotshare](http://dotshare.it/).

*	[dotshare.it/category/terms/colors/](http://dotshare.it/category/terms/colors/)

And more <code>.Xresources</code> example, also from [dotshare](http://dotshare.it/).

*	[dotshare.it/category/misc/misc/](http://dotshare.it/category/misc/misc/)

#### urxvt extension

I never try this, but this is seems cool.

* [awesome-urxvt](https://github.com/bookercodes/awesome-urxvt/blob/master/readme.md)


#### termite configuration

| Config | Path |
| :--- | :--- |
| termite | ~/.config/termite/config |

Source:

*	[gitlab.com/.../dotfiles/.../termite][dotfiles-termite]

It was originally frtom my friend **morgareth**,
but I cannot find the dotfiles any longer.


-- -- --

### Shell and Shell Prompt

Shell are part of operating system.
There are many shells, most commonly used are:

*	[BASH](https://www.gnu.org/software/bash/)

*	[ZSH](http://www.zsh.org/)

*	[FISH](https://fishshell.com/)

you can switch the default shell by this command:

{% highlight bash %}
$ chsh -s /bin/zsh
{% endhighlight %}

For each shell, it has ready to use customizable prompt configuration:

*	BASH: [oh-my-bash][github-oh-my-bash] ([ohmybash.github.io][site-oh-my-bash])

*	ZSH: [oh-my-zsh][github-oh-my-zsh] ([ohmyz.sh][site-oh-my-zsh])

*	FISH: [oh-my-fish][github-oh-my-fish]

*	BASH [bash-it][github-bash-it]

But there are also a unique arrow prompt called <code>powerline</code>.
It needs special config. An it is also highly customizable.

#### .bashrc prompt

You can grab more <code>.bashrc</code> from [dotshare](http://dotshare.it/).

*	[dotshare.it/category/shells/bash/](http://dotshare.it/category/shells/bash/)

#### .zshrc prompt

You can grab more <code>.zshrc</code> from [dotshare](http://dotshare.it/).

*	[dotshare.it/category/shells/zsh/](http://dotshare.it/category/shells/zsh/)

#### oh-my-bash configuration

| Config | Path |
| :--- | :--- |
| bash | ~/.bashrc |

Source:

*	[gitlab.com/.../dotfiles/.../oh-my-bash][dotfiles-oh-my-bash]

#### oh-my-zsh configuration

| Config | Path |
| :--- | :--- |
| zsh | ~/.zshrc |


Source:

*	[gitlab.com/.../dotfiles/.../oh-my-zsh][dotfiles-oh-my-zsh]

#### powerline configuration

There are many files that can be configured in this powerline directory:

*	[gitlab.com/.../dotfiles/.../powerline][dotfiles-powerline]

To use powerline, add it in your <code>.bashrc</code>,
or <code>.zrc</code> or <code>.config/fish/config.fish</code>.

| Config | Path |
| :--- | :--- |
| bash | ~/.bashrc |
| fish | ~/.config/fish/config.fish |

*	[gitlab.com/.../dotfiles/.../pl-bash][dotfiles-pl-bash]

*	[gitlab.com/.../dotfiles/.../pl-fish][dotfiles-pl-fish]

-- -- --

### Multiplexer

There are two widely used terminal multiplexer.
In fact I only know this two.

*	[tmux](https://github.com/tmux/tmux/wiki)

*	[gnu screen](https://www.gnu.org/software/screen/)

#### tmux configuration

tmux can have a very nice panelbar,
that can be set on top or bottom of your terminal.

| Config | Path |
| :--- | :--- |
| tmux | ~/.tmux.conf |

Source:

*	[gitlab.com/.../dotfiles/.../tmux][dotfiles-tmux]

I like to copy the tmux config from [dotshare](http://dotshare.it/).

*	[dotshare.it/category/terms/tmux/](http://dotshare.it/category/terms/tmux/)

#### gnu screen configuration

I do not use gnu screen.
However you can grab the config from [dotshare](http://dotshare.it/).

*	[dotshare.it/category/terms/screen/](http://dotshare.it/category/terms/screen/)

-- -- --

### Multiplexer Wrapper

Beyond the multiplexer,
there are also ready to use application that manage previous tools

*	[teamocil](http://www.teamocil.com/): using tmux as backend, managing layout.

*	[byobu](http://byobu.co/): using either tmux or gnu screen backend, not managing layout.

![Terminal Ricing: Debian URXVT][image-ss-debian-urxvt]{: .img-responsive }

#### teamocil configuration

Teamocil is a simple tool used to automatically
create windows and panes in tmux with YAML files.

I'm using Jekyll for daily basis blogging.
Instead of typing the same command over and over again,
using teamocil can be helpful.

| Config | Path |
| :--- | :--- |
| teamocil | ~/.teamocil/jekyll.yml |

This is the configuration:

*	[gitlab.com/.../dotfiles/.../teamocil][dotfiles-teamocil]

-- -- --

### Compositor

> compositor for transparency

You need compositor to enable transparency, shadow and such effects.
There are two known compositor for ricing:

*	[xcompmgr](http://cgit.freedesktop.org/xorg/app/xcompmgr/)

*	[compton](https://github.com/chjj/compton): successor of xcompmgr

Simply run compton to enable it.

{% highlight bash %}
$ compton &!
{% endhighlight %}

#### compton configuration

| Config | Path |
| :--- | :--- |
| compton | ~/.config/compton.conf |

Source

*	[gitlab.com/.../dotfiles/.../compton][dotfiles-compton]

-- -- --

### gtk.css

Terminal can have padding using [gtk.css](https://developer.gnome.org/gtk3/stable/chap-css-overview.html)
Setting this padding would make your terminal way cooler.

| Config | Path |
| :--- | :--- |
| gtk.css | ~/.config/gtk-3.0/gtk.css |

Source

*	[gitlab.com/.../dotfiles/.../gtk.css][dotfiles-gtk-css]

A more complete config can be seen here:

*	[M. Yuga Nugraha](https://github.com/myugan/dotfiles/blob/master/home/.config/gtk-3.0/gtk.css)


-- -- --

### Background

Well .. well.. well... 
Wallpaper is the most ingredient in ricing.
You are free to use any wallpaper

But there are other trick as well.

#### custom wallpaper

I also make custom wallpaper at deviantart.

*	[deviantart.com/nurwijayadi](http://nurwijayadi.deviantart.com/)

#### hackish style

I like to use other terminal as background.
The background terminal contain compilation.

This would make a very hackish style effect,
as shown in most top picture above.

-- -- --

### Example CLI application

There are many CLI application such as

*	Music visualizer: [CAVA](https://github.com/karlstav/cava)

*	System information: [neofetch](https://github.com/dylanaraps/neofetch), screenfetch

*	System Monitoring: htop, [gtop](https://github.com/aksakalli/gtop)

*	[exa](https://the.exa.website/): colorful rust based ls 

This is not the place to explain detail for this CLI applications.
However, this is most commonly use <code>neofetch</code> custom config is a must:

| Config | Path |
| :--- | :--- |
| neofetch | ~/.config/compton.conf |

*	[gitlab.com/.../dotfiles/.../neofetch][dotfiles-neofetch]

-- -- --

### ViM as Special CLI application

[Vim](https://www.vim.org/) is an advanced text editor

This is also a must have config.

![Terminal Ricing: git log vim][image-ss-git-log-vim]{: .img-responsive }

#### vimrc configuration

| Config | Path |
| :--- | :--- |
| vimrc | ~/.vimrc |

Source:

*	[gitlab.com/.../dotfiles/.../vimrc][dotfiles-vimrc]

I copy the config from my friend **bandithijo**,
and strip down to suit my needs.

*	[bandithijo/.../.vimrc](https://github.com/bandithijo/dotfiles/blob/master/.vimrc)

There are more ViM rc at [dotshare](http://dotshare.it/).

*	[dotshare.it/category/vim/rc/](http://dotshare.it/category/vim/rc/)

#### vim path

There are two directories in this ViM directory:

| Config | Path |
| :--- | :--- |
| vim | ~/.vim/* |

*	[gitlab.com/.../dotfiles/.../vim][dotfiles-vim]

There are more ViM colors at [dotshare](http://dotshare.it/).

*	[http://dotshare.it/category/vim/colors/](http://dotshare.it/category/vim/colors/)

#### Learning ViM

Beside the basic <code>vimtutor</code>, you might want to 
[learn ViM script the hard way](http://learnvimscriptthehardway.stevelosh.com/).

-- -- --

### Pixel Art

You can see my pixelart repository on bitbucket here:

*	[bitbucket.org/epsi/ansi-color](https://bitbucket.org/epsi/ansi-color)

The repository is a compilation from this good thread:

*	[Crunchbang » ANSI colorschemes scripts](https://crunchbang.org/forums/viewtopic.php?id=13645)

#### Example of Pixelart Greeting

![Terminal Ricing: Greeting Syawal Pixel Art][image-ss-greeting-syawal]{: .img-responsive }

*	[gitlab.com/.../dotfiles/.../syawal-1439H.sh][dotfiles-syawal]

-- -- --

### Conclusion

That is all.

Thank you for reading and visiting.


[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/08' %}
{% assign dotfiles = 'https://gitlab.com/epsi-rns/dotfiles/tree/master/terminal' %}

[image-ss-gentoo-xfce4-terminal]:  {{ asset_path }}/tmux-vim-gcc-c-asm.png
[image-ss-debian-urxvt]:           {{ asset_path }}/byobu-objdump-and-source.png
[image-ss-greeting-syawal]:        {{ asset_path }}/syawal-1439h.png
[image-ss-git-log-vim]:            {{ asset_path }}/git-log-vim-r.png

[dotfiles-xdefaults]:     {{ dotfiles }}/urxvt/Xdefaults
[dotfiles-xresources]:    {{ dotfiles }}/urxvt/Xresources
[dotfiles-termite]:       {{ dotfiles }}/termite/config
[dotfiles-oh-my-bash]:    {{ dotfiles }}/oh-my-bash/bashrc
[dotfiles-oh-my-zsh]:     {{ dotfiles }}/oh-my-zsh/zshrc
[dotfiles-powerline]:     {{ dotfiles }}/powerline
[dotfiles-pl-bash]:       {{ dotfiles }}/powerline/bash/bashrc.powerline
[dotfiles-pl-fish]:       {{ dotfiles }}/powerline/fish/config.fish
[dotfiles-tmux]:          {{ dotfiles }}/tmux/tmux.conf
[dotfiles-teamocil]:      {{ dotfiles }}/teamocil/jekyll.yml
[dotfiles-compton]:       {{ dotfiles }}/compton/compton.default.conf
[dotfiles-gtk-css]:       {{ dotfiles }}/gtk-3.0/gtk.css
[dotfiles-neofetch]:      {{ dotfiles }}/neofetch/config.current.conf
[dotfiles-vim]:           {{ dotfiles }}/vim
[dotfiles-vimrc]:         {{ dotfiles }}/vimrc/vimrc.current
[dotfiles-syawal]:        https://gitlab.com/epsi-rns/dotfiles/blob/master/notes/greeting/syawal-1439H.sh

[github-oh-my-bash]: https://github.com/ohmybash/oh-my-bash
[github-oh-my-zsh]:  https://github.com/robbyrussell/oh-my-zsh
[github-oh-my-fish]: https://github.com/oh-my-fish/oh-my-fish
[github-bash-it]:    https://github.com/Bash-it/bash-it

[site-oh-my-bash]: https://ohmybash.github.io/
[site-oh-my-zsh]: http://ohmyz.sh/


