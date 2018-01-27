---
layout: post
title:  "Telegram Bot - BASH Makefile"
categories: code
date:   2018-01-25 09:17:35 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.
  Using loop with BASH script.
  No webhook in BASH series.

---

{% include post/2018/01/toc-telegram-bot.html %}

### Bashful Bot

> Goal: Deploy BASH Project to System Wide

#### Previous Guidance

We're already see an almost finished bash project in previous article.
We should finish what we have done.

#### Issue

	The need of Example

Installing part could be deep mistery to beginner.
Although this looks tricky, actually it is easy.
All we need is a <code>Makefile</code> sample.

-- -- --

### Source Files

What we want to achieve, is copy our script from any location.

Here are our source looks like

{% highlight bash %}
% ls
Makefile      functions.bash     main-noloop.bash
config.bash   main-loop.bash     messages.bash
cupubot-bash  main-modular.bash  options.bash
{% endhighlight %}

![BASH: Telegram Bot: Makefile Source][image-makefile-source]{: .img-responsive }

<code>cupubot-bash</code> is our main launcher

{% highlight bash %}
#!/usr/bin/env bash
# redirect script to library

DIR=$(dirname "$0")
DIR=${DIR}/../lib/cupubot
. ${DIR}/main-modular.bash
{% endhighlight %}

To enable this, we need to alter <code>main-modular.bash</code> a bit.

{% highlight bash %}
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

. ${DIR}/config.bash
. ${DIR}/functions.bash
. ${DIR}/messages.bash
. ${DIR}/options.bash
{% endhighlight %}

We are no longer can use use <code>DIR=$(dirname "$0")</code>.
Because our main path has been changed,
from the same path directory with <code>main-modular.bash</code>,
to different directory.
It will be called from <code>/usr/local/bin</code>.

-- -- --

### Destination Files

	From Source to Destination

What we want to achieve, is copy our script from any location,
to a particular directory.

*	<code>/usr/local/bin</code>

*	<code>/usr/local/lib</code>

To do this, let's create a <code>Makefile</code>

{% highlight bash %}
% touch Makefile
{% endhighlight %}

And edit with your favorite editor [geany, kate, nano, vim].
The choice is actually up to you, as long as you can add this line

{% highlight make %}
PREFIX ?= /usr/local
bindir        = $(PREFIX)/bin
libdir        = $(PREFIX)/lib
{% endhighlight %}

-- -- --

### Install

	More Detail Please

What we want to achieve, is copy our script from any location,
to a particular directory.

*	<code>/usr/local/bin</code>: cupubot-bash

*	<code>/usr/local/lib</code>: *.bash

Talk is cheap, here is the <code>install</code> target line code:

{% highlight make %}
install:
	@echo "Installing..."
	
	# Scripts
	@install -D -m755 ./cupubot-bash $(DESTDIR)$(bindir)/cupubot-bash
	
	# Libs
	@mkdir -p $(DESTDIR)$(libdir)/cupubot
	@install -D -m644 ./*.bash $(DESTDIR)$(libdir)/cupubot
{% endhighlight %}

It is pretty clear and self explanatory.

#### Action

Consider, run the command:

{% highlight bash %}
% sudo make install
Password: 
Installing...
# Scripts
# Libs
{% endhighlight %}

![BASH: Telegram Bot: Makefile Install][image-makefile-install]{: .img-responsive }

#### Result

Now here it is the result.

{% highlight bash %}
% ls /usr/local/bin/cupu*  /usr/local/lib/cupubot/*
/usr/local/bin/cupubot-bash
/usr/local/lib/cupubot/config.bash
/usr/local/lib/cupubot/functions.bash
/usr/local/lib/cupubot/main-loop.bash
/usr/local/lib/cupubot/main-modular.bash
/usr/local/lib/cupubot/main-noloop.bash
/usr/local/lib/cupubot/messages.bash
/usr/local/lib/cupubot/options.bash
{% endhighlight %}

![BASH: Telegram Bot: Makefile Destination][image-makefile-destination]{: .img-responsive }

Note that you can see the color changes.
Only one file is executable.

-- -- --

### Uninstall

	Reverse, Undo

Respectively, there is also <code>uninstall</code> target line code.

{% highlight make %}
uninstall:
	@echo "Uninstalling..."
	
	# Scripts
	@rm $(DESTDIR)$(bindir)/cupubot-bash
	
	# Libs
	@rm -r $(DESTDIR)$(libdir)/cupubot
{% endhighlight %}

That above is also self explanatory.

#### Action

Consider, run the command:

{% highlight bash %}
% sudo make uninstall
Uninstalling...
# Scripts
# Libs
{% endhighlight %}

![BASH: Telegram Bot: Makefile Uninstall][image-makefile-uninstall]{: .img-responsive }

-- -- --

### System Wide Project

Now you can test to see if the install works.

{% highlight bash %}
% cupubot-bash --help
usage:  cupubot [options]
operations:
 general
   -v, --version    display version information
   -h, --help       display help information
{% endhighlight %}

![BASH: Telegram Bot: Makefile System Wide][image-makefile-systemwide]{: .img-responsive }

I think that's all.

### What is Next ?

We will add some chat group tools such as

*	Greet New Member.

*	Text Logger.

*	HTML Logger.

Just have a look at the next article.

*	[Telegram Bot - BASH Group Tools][local-bash-group]

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/cupubot/tree/master/loop/bash' %}

[local-bash-group]:    /code/2018/01/26/telegram-bot-loop-bash.html

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[image-makefile-source]:      {{ asset_path }}/cupubot-makefile-ls-source.png
[image-makefile-destination]: {{ asset_path }}/cupubot-makefile-ls-destination.png
[image-makefile-install]:     {{ asset_path }}/cupubot-makefile-install.png
[image-makefile-uninstall]:   {{ asset_path }}/cupubot-makefile-uninstall.png
[image-makefile-systemwide]:  {{ asset_path }}/cupubot-systemwide.png
