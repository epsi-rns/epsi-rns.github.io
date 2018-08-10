---
layout: post-sidemenu-wm
title:  "Customizing Shell Prompt with Powerline"
date:   2016-03-21 19:17:15 +0700
categories: desktop
tags: [ricing, dotfiles]
author: epsi

excerpt: 
  For those who wants to looks cool with their console screenshot.
  Or people who does ricing a lot. Well, Powerline is for you.
  
---

Dear command line fans, here is my 2 cents, powerline configuration.<br/>

* [github.com/epsi-rns/dotfiles/.../powerline][dotfiles-powerline]

All you need is to add these two configuration. They are theme (multiline) and colorscheme. 
And alter <code class="code-file">config.json</code>.<br/>

I haven't done anything yet for tmux, and other powerline capabilities.<br/>

-- -- --

{% capture ss_content %}
<strong>OS</strong>: Arch<br/>
  + <strong>WM</strong>: Awesome WM<br/>
  + <strong>Shell</strong>: Fish<br/>
{% endcapture %}

{% include part/screenshot.html ss_content = ss_content %}

![Powerline Light: arch+awesome][image-ss-powerline-light]{: .img-responsive }
<br/><br/>
![Powerline Dark: arch+awesome][image-ss-powerline-dark]{: .img-responsive }

-- -- --

## Reading

Always look at the official documentation first

* <http://powerline.readthedocs.io/en/master/installation/linux.html>

* <https://wiki.archlinux.org/index.php/Powerline>

-- -- --

## Installing

Using package management is straightforward.

Arch

{% highlight bash %}
$ sudo pacman -S powerline
{% endhighlight %}

Manjaro

{% highlight bash %}
$ sudo pacman -S powerline
{% endhighlight %}

[![Powerline: Install Arch][image-ss-install-arch]{: .img-responsive }][photo-ss-install-arch]

Debian

{% highlight bash %}
$ sudo apt install powerline
{% endhighlight %}

[![Powerline: Install Debian][image-ss-install-debian]{: .img-responsive }][photo-ss-install-debian]

-- -- --

## Font

You can download manually based on documentation,
or use package management above.

* <http://powerline.readthedocs.io/en/master/installation.html#fonts-installation>

Arch/Manjaro

Install Font package

{% highlight bash %}
$ sudo pacman -S powerline-fonts
{% endhighlight %}

Let's check.

{% highlight bash %}
$ pacman -Ql powerline-fonts
...
... /usr/share/fonts/OTF/PowerlineSymbols.otf
{% endhighlight %}

Debian

Font Package automaticaly installed as dependency.

{% highlight bash %}
$ dpkg -L fonts-powerline
...
/usr/share/fonts/opentype/PowerlineSymbols.otf
{% endhighlight %}


Note: Terminus Font also works.

-- -- --

## Shell Prompt


"Where and how can I set the <code>PS1</code> variable for Powerline bash?"<br/>
A good simple question.

The answer is in powerline manual documentation

* <http://powerline.readthedocs.io/en/master/usage/shell-prompts.html>

There is no need to explicitly export PS1 in bash.

### Bash

Arch/Manjaro (root/user)

{% highlight bash %}
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh
{% endhighlight %}

Debian (root/user)

{% highlight bash %}
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/share/powerline/bindings/bash/powerline.sh
{% endhighlight %}

### Fish

{% highlight bash %}
$ cat ~/.config/fish/config.fish 
powerline-daemon -q
set POWERLINE_BASH_CONTINUATION 1
set POWERLINE_BASH_SELECT 1
set fish_function_path $fish_function_path "/usr/lib/python3.5/site-packages/powerline/bindings/fish"
powerline-setup
{% endhighlight %}


Here is a summary, with nice powerline-in-vim colour-scheme.

* Terminal Above: .bashrc only (with ansi color art)

* Terminal Below: .bashrc with powerline

[![Powerline: .bashrc ViM][image-ss-bashrc]{: .img-responsive }][photo-ss-bashrc]

-- -- --

## Vim

Arch/Manjaro

No need to alter <code class="code-file">.vimrc</code> configuration in Arch/Manjaro

{% highlight bash %}
$ sudo pacman -S powerline-vim
{% endhighlight %}

Debian

Since no official plugin for ViM, whe have to do it with PIP.

{% highlight bash %}
$ sudo pip3 install git+git://github.com/Lokaltog/powerline
{% endhighlight %}

And insert this line to .viwmrc

{% highlight conf %}
set rtp+=/usr/local/lib/python3.5/dist-packages/powerline/bindings/vim
set laststatus=2
set t_Co=256
{% endhighlight %}

Reading

* <https://levlaz.org/installing-powerline-in-debian/>

--

## Customization

### Main Config

* <https://gitlab.com/epsi-rns/dotfiles/blob/master/config/powerline/config.json>

Here is the place, you configure which colorscheme and theme you are using.

{% highlight json %}
	"shell": {
		"colorscheme": "epsi",
		"theme": "epsi",
		"local_themes": {
			"continuation": "continuation",
			"select": "select"
		}
{% endhighlight %}

### Colorschemes

Self Explanatory

* https://gitlab.com/epsi-rns/dotfiles/blob/master/config/powerline/colorschemes/epsi.json

{% highlight json %}
		"user":      { "fg": "gray2", "bg": "yellow", "attr": ["bold"] },
		"superuser": { "fg": "yellow", "bg": "orange", "attr": ["bold"] },
{% endhighlight %}

### Theme

It define what widget, ad their position.
It is going to be your choice,
of what segment you need to show 
in your shell.

* https://gitlab.com/epsi-rns/dotfiles/blob/master/config/powerline/themes/shell/epsi.json

{% highlight json %}
	{
		"function": "powerline.segments.common.time.date",
		"name": "time",
		"args": {
			"format": "%H:%M:%S %A",
			"istime": true
		}
	},
	{
		"function": "powerline.segments.common.time.fuzzy_time"
	}
{% endhighlight %}

Note that BASH do not support right segment.
Right segment is very useful when working with git.
I usually switch to FISH, everytime I'm doing git.

You can see the sample in top of this this article.

-- -- --

Happy terminal customizing




**Reading**:<br/>
* [http://powerline.readthedocs.org/.../common.html][docs-powerline]

[//]: <> ( -- -- -- links below -- -- -- )

[docs-powerline]:     http://powerline.readthedocs.org/en/master/configuration/segments/common.html
[dotfiles-powerline]: https://gitlab.com/epsi-rns/dotfiles/tree/master/terminal/powerline

[image-ss-powerline-light]: {{ site.url }}/assets/posts/desktop/2016/03/powerline-light.png
[image-ss-powerline-dark]:  {{ site.url }}/assets/posts/desktop/2016/03/powerline-dark.png

[image-ss-install-arch]:    {{ site.url }}/assets/posts/desktop/2016/03/arch-powerline-install-half.png
[image-ss-install-debian]:  {{ site.url }}/assets/posts/desktop/2016/03/debian-powerline-install-half.png
[image-ss-bashrc]:          {{ site.url }}/assets/posts/desktop/2016/03/powerline-vim-nice-color.png

[photo-ss-install-arch]:    https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMdqYD48qXWiJKw-ljQJBXxAtpkMaVIx3RdTUnw
[photo-ss-install-debian]:  https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPSIJxwaHZg4lbzkhMF6sSRnDbqjbEUkVLd2giw
[photo-ss-bashrc]:          https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipMnzf6HS-eW6J0NRDcYNattXydVIOR4inswGPUX
