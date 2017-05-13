---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM Overview"
date:   2017-05-01 17:35:15 +0700
categories: desktop
tags: [coding, herbstluftwm]
author: epsi

excerpt:
  Preface of Doing Hersbtluft WM Config
  
---

### Preface

Every Window Manager as its challenge.
That is where the fun come from.
Herbstluft WM is unique in way that 
the configuration set from command line interface.
So basically the config is just a bunch of 
separated <code>herbstclient</code> command.
It means, the configurtion can be bundled using any script
that has access to system command.

{% highlight bash %}
$ herbstclient attr theme.border_width 3
{% endhighlight %}

One liner is not so bad after all.

![HerbstluftWM: Command Line Interface][image-hlwm-01-cli]{: .img-responsive }

-- -- --

### Meanwhile

	Last couple of months in the real world,
	I had a few weeks of badassery.
	Now I decide to take time for myself.
	Relax, lock myself in my room, doing HersbtluftWM configuration, 
	get it online,and pour the result in a blog.
	I still have to be a little badass outside.
	I just need to share this HLWM config, before I go back.

-- -- --

### What is not in this Guidance

We don't do any statusbar in this tutorial.

	Separate Window Manager Tutorial, and Statusbar Tutorial.

One of the most complex part of Window Manager is Statusbar.
It is considered third party. It deserve its own article. 
And I did wrote a specific article exploring statusbar.
After this, we still need two more article.

*	Statusbar, designed specifically for Herbstluft Tag.

*	Putting it all together, Window Manager, StatusBar, Terminals.

Some people don't even bother with aestethic aspect of Window Manager.
No need any statusbar, not even gap between windows.
The lesser gap, the more information.

This will only discuss about the HLWM configuration.

-- -- --

### Redirect Config

The original Herbsluft WM equipped with one example of
<code class="code-file">autostart</code> configuration in BASH .
It is a single long file and plain.  We are going to make it modular.
I have seen a sophisticated HLWM config in Perl in a single file.
So why not go further, exercise scripting in few other language.
From BASH, Perl, Python, Ruby, PHP, Lua,
and finally the compiled Haskell.
It is not just possible, we already did write the config.
Every language has their own challenge.
And that is exactly where the fun comes from.

First, we need to redirect to appropriate language script in
<code class="code-file">~/.config/herbstluftwm/autostart</code>.
If we want to use BASH, just uncomment the line
containing <code class="code-file">bash/autostart.sh</code>.
It is applied to every other language.

{% highlight bash %}
#!/usr/bin/env bash

~/.config/herbstluftwm/bash/autostart.sh
#~/.config/herbstluftwm/perl/autostart.pl
#~/.config/herbstluftwm/python/autostart.py
#~/.config/herbstluftwm/ruby/autostart.rb
#~/.config/herbstluftwm/php/autostart.php
#~/.config/herbstluftwm/lua/autostart.lua
#~/.config/herbstluftwm/haskell/autostart
{% endhighlight %}

Each must have exectubale permission.

### Directory Structure

	The idea is Focus

Any WM user would be tempted to alter their WM behaviour.
So give them place in main <code class="code-file">autostart</code>.
script, for special customization.
Then a config file for common customization.
A personal startup file. A place for custom code.
Anything in place, anything has their own place.

With these ideas. For each language, script separated into

*	**autostart**: Main script. The logic flow through here.

*	**config**: It contain data, most setting here.

*	**helper**: It contain action procedure. All the code horse work.

*	**startup**: Everyone has their own startup application,
	So yeah, separate it, so anyone can freely modifytheir startup.

*	**gmc**: Google Material Color. Just a long Color Schemes.
	Separate it, so it won't ruin Main script.
	Since it is a reusable library that can be used for statusbar,
	i.e dzen2, or lemonbar. It is placed in special directory called assets.

{% highlight conf %}
.
├── autostart
├── config
├── helper
├── startup
└── assets
    └── gmc
{% endhighlight %}

-- -- --

### Approach on Tag Names

Instead of common practices,
I'm using different approach when it comes to tag status.
I'm using number as tag names, and using the number as a key,
for use in statusbar, and let the statusbar show display name.
It is better this way for me,
because the original tag names does not support unicode, e.g Kanji.
Meanwhile, I can manage unicode from script based on key number.

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

Three different things in here

*	Tag names refer to, the name of HLWM's tags shown in tag_status.
	The original use only number as tag name.
	Some smart people in dotshare use string i.e mail, term, 
	to define their tag name. This name usually shown in statusbar.

*	Tag Keys refer to keyboard shortcut.
	It mostly number 1 to 9. Although you can add 0 (I did).
	or any keyboard character (I never did).

*	My unique solution, the display name.
	I prefer to use number as tag names.
	And let the statusbar manage display name
	using the tag name number as the key.

-- -- --

### Blog Post

The rest is in their respective article.

	Let's get it started.

*	[Modularized HerbstluftWM in BASH][local-bash]


### Dotfiles (Source Code)

*	[lang/bash][dotfiles-BASH]

*	[lang/perl][dotfiles-Perl]

*	[lang/python][dotfiles-python]

*	[lang/ruby][dotfiles-Ruby]

*	[lang/php][dotfiles-PHP]

*	[lang/lua][dotfiles-Lua]

*	[lang/haskell][dotfiles-Haskell]

-- -- --

### Disclaimer

	Herbstluft Window Manager.

I do not claim myself as an HLWM expert.
In fact, I'm still trying to figure out how
to say the word right with german accent.

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm' %}
{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}

[image-hlwm-01-cli]:  {{ asset_path }}/hlwm-01-cli.png
[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell