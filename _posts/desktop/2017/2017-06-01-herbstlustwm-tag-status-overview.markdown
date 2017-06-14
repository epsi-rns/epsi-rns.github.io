---
layout: post-sidemenu-wm
title:  "HerbstluftWM Tag Status Overview"
date:   2017-06-01 17:35:15 +0700
categories: desktop
tags: [coding, herbstluftwm, dotfiles]
author: epsi

excerpt:
  Preface of Doing HersbtluftWM Tag Status
  implementation for both Lemonbar and Dzen2. 
  
---

### Preface

	Why not Conky ?

HerbstluftWM is event based.
Herbstclient sent its notification based on Window Event.
Conky is interval based, and because of that,
it cannot read HLWM event properly.
For that reason, we cannot use conky for this situation.

HerbstluftWM event produced by <code>--idle</code> command.

{% highlight bash %}
$ herbstclient --idle
{% endhighlight %}

![HerbstluftWM: Event Idle][image-hlwm-01-event-idle]{: .img-responsive }

For each tag event, tag status in HerbstluftWM should be updated.
HerbstluftWM tag status produced by <code>tag_status</code> command.

{% highlight bash %}
$ herbstclient tag_status
	#1	:2	:3	:4	:5	.6	.7	.8	.9	
{% endhighlight %}

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

Dzen2 and Lemonbar is very similar,
in which they render anything feed to them.
No matter the input is, event based or interval based.

This tutorial only giving guidance,
on how to transform the plain tag status text,
into nice statusbar, whether it is Dzen2 or Lemonbar.
From getting the statusbar geometry right, to aestethic aspect.

-- -- --

### Multi Language Implementation

	Why Port ?

The original Herbsluft WM equipped with one example of
<code class="code-file">panel</code> script in BASH .
It is a single long file and plain. We are going to make it modular.
My script is a heavy customization of the original.

I also have seen a sophisticated HLWM config in Perl in a single file.
Since there is no tutorial for human for that script,
I decided to write my own script, and make my own tutorial.

So why not go further, exercise scripting in few other language.
From BASH, Perl, Python, Ruby, PHP, Lua,
and finally the compiled Haskell.
As a beginner, I found that the Haskell part is full of tricks.
And that is exactly where the fun comes from.

-- -- --

### What is not in this Guidance

No Window Manager in this tutorial.

	Separate Window Manager Tutorial, and Statusbar Tutorial.

One of the most complex part of Window Manager is Statusbar.
It is considered third party. It deserve its own article. 
And I did wrote a specific article exploring statusbar.

The code given is complete, but I breakdown the tutorial into two parts.

1	HerbstluftWM Tag Status: Statusbar (this article)
	Focusing on tags: <code>herbstclient tag_status</code>

2	HerbstluftWM Event Idle: Advance Pipe and Fork (not in this article)
	Focusing on event: <code>herbstclient --idle</code>

This will only discuss about the Statusbar.

-- -- --

### Dzen2 or Lemonbar.

	What Statusbar ?

The code in github already have a complete working example
of both Dzen2 and Lemonbar. 
Since there are two option statusbar, Dzen2 and Lemonbar,
and most of the code are similar,
this tutorial should choose only one of them,
and let the reader use the source code in github,
in order to use other statusbar.

This blog already have Dzen2 tutorial in *Pipe and Fork* section.
Hence, lemonbar deserve to be in this tutorial.
And since lemonbar clickable areas needs more pipe,
lemonbar also have more challenge compared to dzen2.

-- -- --

### Directory Structure

	The idea is Focus

Any statusbar user would be tempted to alter their statusbar looks.
So give them place in main <code class="code-file">output</code>.
script, for special customization.
The rest, is static, no need to alter.

With this idea. For each language, script separated into

*	**panel**: Main script. The logic flow through here.

*	**pipehandler**: All the code horse work of piping and forking.

*	**helper**: Setting the geometry of statusbar.

*	**output**: The statusbar looks is here.
	So yeah, separate it, so anyone can freely modify their startup.

*	**gmc**: Google Material Color. Just a long Color Schemes.

*	**01-testparams** and **02-testoutput**: Blogging purpose.
	Step by step guidance.

{% highlight conf %}
.
├── panel
├── pipehandler
├── helper
├── output
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

In HLWM config in BASH, this would be like this

{% highlight conf %}
tag_names=( {1..9} )
tag_keys=( {1..9} 0 )
{% endhighlight %}

And in Dzen2/Lemonbar panel in BASH, this would be like this

{% highlight conf %}
tag_shows=( "一 ichi" "二 ni" "三 san" "四 shi" 
  "五 go" "六 roku" "七 shichi" "八 hachi" "九 kyū" "十 jū")
{% endhighlight %}

Before you start, you might consider to use proper HLWM config.

	Proper Tag Names is a requirement.

#### View Source File:  HerbstluftWM Configuration:

*	[github.com/.../dotfiles/.../herbstluftwm][dotfiles-hlwm-config]

#### View Blog Article: HerbstluftWM Configuration:

*	[Modularized HerbstluftWM Overview ][dotfiles-hlwm-config]

-- -- --

### Blog Post

The rest is in their respective article.

	Let's get it started.

*	[HerbstluftWM Tag Status in BASH][local-bash]

*	[HerbstluftWM Tag Status in Perl][local-perl]

*	[HerbstluftWM Tag Status in Python][local-python]

*	[HerbstluftWM Tag Status in Ruby][local-ruby]

*	[HerbstluftWM Tag Status in PHP][local-php]

*	[HerbstluftWM Tag Status in Lua][local-lua]

*	[HerbstluftWM Tag Status in Haskell][local-haskell]


### Dotfiles (Source Code)

*	[lang/bash][dotfiles-BASH]

*	[lang/perl][dotfiles-Perl]

*	[lang/python][dotfiles-python]

*	[lang/ruby][dotfiles-Ruby]

*	[lang/php][dotfiles-PHP]

*	[lang/lua][dotfiles-Lua]

*	[lang/haskell][dotfiles-Haskell]

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}

[image-hlwm-01-event-idle]:  {{ asset_path }}/herbstclient-01-event-idle.png
[image-hlwm-02-tag-status]:  {{ asset_path }}/herbstclient-02-tag-status.png

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/02/herbstlustwm-tag-status-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/03/herbstlustwm-tag-status-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/04/herbstlustwm-tag-status-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/06/herbstlustwm-tag-status-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell
