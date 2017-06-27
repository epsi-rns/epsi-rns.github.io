---
layout: post-sidemenu-wm
title:  "HerbstluftWM Idle Event Overview"
date:   2017-06-11 17:35:15 +0700
categories: desktop
tags: [coding, herbstluftwm, dotfiles]
author: epsi

excerpt:
  Preface of Handling HersbtluftWM Event Idle
  for both Lemonbar and Dzen2.
  An advance case of Pipe and Fork.
  
---

### Preface

	The challenge is, to process Hersbtclient Idle Event

HerbstluftWM event produced by <code>--idle</code> command.
Event based statusbar require us to create our own tools,
to manage the flow from one process,
and pipe the output to other process.

Interval based itself could be considered as timer event.
To combine both herbstluftWM idle event and interval event,
we need to create our own event producer.

{% highlight bash %}
$ herbstclient --idle
{% endhighlight %}

![HerbstluftWM: Event Idle][image-hlwm-01-event-idle]{: .img-responsive }

-- -- --

### Pipe

	How many Pipes ?

In shell it is basically just a pipe:

#### Dzen2

This is a two tier pipes,
since content processing happened between pipe. 
The content processing itself is a complex part. 
And there are some pipe and fork combinations as well.

{% highlight bash %}
herbstclient --idle | content_processing | dzen2
{% endhighlight %}

**Lemonbar**

This is a three tier pipes,
just like Dzen2, except Lemonbar pipe clickable areas to a shell.

{% highlight bash %}
herbstclient --idle | content_processing | lemonbar | sh
{% endhighlight %}


#### Unidirectional

The requirement of Dzen2 is unidirectional,
it means one way communication. Simple. 

The idle event as an input,
feed the content process,
the content process produce an output to Dzen2.
While Dzen2 process act as a writer,
idle event process act as a reader.

#### Bidirectional

The requirement of Lemonbar is bidirectional,
it means tow ways communication.
Lemonbar read input and write output at the same time.
Lemonbar process act as a reader and writer in the same time.

For each language we must carefully chose Pipe methods,
that support bidirectional,
since most pipe only support unidirectional.

#### Content Process

Content process live in an infinite loop
which write to statusbar whether it is Dzen2 or Lemonbar. 
This has no issue with unidirectional loops
But this create an issue in a application logic
for bidirectional Lemonbar, 
which has another loop to write to shell.

Both loops live at the same time.
There is no loop notation sufficient,
to implement this complex logic, without blocking each other.
The solution is to fork the infinite loop as a new process,
outside the shell loop.

#### Combined Event 

What if we want to put other stuff in our lovely panel, such as date?
Or any system monitoring information such as CPU, Memory, HDD,
temperature, uname or even MPD?

![HerbstluftWM: Custom Event][image-hlwm-06-event-custom]{: .img-responsive }

These system monitoring is usually called in interval based.
Date can be based on one second interval.
And other stuff can be groupped in longer interval.
And uname called only once, or one hour interval if you wish.

Luckily we can treat these interval as event.
Consider this combined event,
consist of idle event (asynchronous) and interval event (synchronous).

{% highlight bash %}
combined_event | content_processing | statusbar
{% endhighlight %}

Since we need to alter the original event,
to create new combined event, 
sometimes we need an intermediate process.
As an intermediate process in Perl, PHP, Ruby, and Haskell, 
we need to pipe both loop, into <code>cat</code>.

This is an overview of what we want to achieve.

![HerbstluftWM: Custom Event][image-hlwm-06-event-custom]{: .img-responsive }
 
Since both event also loops that write in the same time,
as the intermediate process being read.
We need to fork both loop.

-- -- --

### To Be Done

**TBD**

This article is, still unfinished.
Here are what I need to do.

*	The source code is still missing one main feature
	about combining these two:

	* herbsclient idle event based, and

	* custom interval based.

*	Putting it all together with other statusbar,
	and main HerbstluftWM configuration.

-- -- --

### Multi Language Implementation

The original script has beenported
from BASH, Perl, Python, Ruby, PHP, Lua,
and finally the compiled Haskell.

	Why Port ?

Because Piping and Forking is a very interesting topic.
Challenging, and no sufficient tutorial in internet,
especially for real life example.
That is why I'm eager to pour them down in a blog.

**Reading**

You might desire to read previous post,
that contain the basic of this tutorial,
with simple example.

*	[Piping and Forking Overview][local-pipe-overview]

-- -- --

### What is not in this Guidance

No Window Manager in this tutorial.

	Separate Window Manager Tutorial, and Statusbar Tutorial.

One of the most complex part of Window Manager is Statusbar.
It is considered third party. It deserve its own article. 
And I did wrote a specific article exploring statusbar.

The code given is complete, but I breakdown the tutorial into two parts.

1.	HerbstluftWM Tag Status: Statusbar (not this article)
	Focusing on tags: <code>herbstclient tag_status</code>

2.	HerbstluftWM Event Idle: Advance Pipe and Fork (this article)
	Focusing on event: <code>herbstclient --idle</code>

This will only discuss about Piping and Forking.

-- -- --

### Dzen2 or Lemonbar.

	What Statusbar ?

Lemonbar, as it is a continuation of previous part using Lemonbar.

	Source code is available for both Dzen2 and Lemonbar.

-- -- --

### Directory Structure

	The idea is Focus

We only use **output** script.
But the tutorial will show the output script step by step,
from simple code to the final form.

The project itself separated into **panel**,
**pipehandler**, **helper**, **output**, **gmc**.

{% highlight conf %}
.
├── panel
|
├── 01-testparams
├── 02-testoutput
|
├── pipehandler
├── helper
├── output
└── gmc
{% endhighlight %}

-- -- --

### Blog Post

The rest is in their respective article.

	Let's get it started.

*	[HerbstluftWM Event Idle Overview][local-overview]

*	[HerbstluftWM Event Idle in BASH][local-bash]

*	[HerbstluftWM Event Idle in Perl][local-perl]

*	[HerbstluftWM Event Idle in Python][local-python]

*	[HerbstluftWM Event Idle in Ruby][local-ruby]

*	[HerbstluftWM Event Idle in PHP][local-php]

*	[HerbstluftWM Event Idle in Lua][local-lua]

*	[HerbstluftWM Event Idle in Haskell][local-haskell]


### Dotfiles (Dzen2 Source Code)

*	[dzen2/bash][dotfiles-dzen2-bash]

*	[dzen2/perl][dotfiles-dzen2-perl]

*	[dzen2/python][dotfiles-dzen2-python]

*	[dzen2/ruby][dotfiles-dzen2-ruby]

*	[dzen2/php][dotfiles-dzen2-php]

*	[dzen2/lua][dotfiles-dzen2-lua]

*	[dzen2/haskell][dotfiles-dzen2-haskell]

### Dotfiles (Lemonbar Source Code)

*	[lemonbar/bash][dotfiles-lemon-bash]

*	[lemonbar/perl][dotfiles-lemon-perl]

*	[lemonbar/python][dotfiles-lemon-python]

*	[lemonbar/ruby][dotfiles-lemon-ruby]

*	[lemonbar/php][dotfiles-lemon-php]

*	[lemonbar/lua][dotfiles-lemon-lua]

*	[lemonbar/haskell][dotfiles-lemon-haskell]

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign dotfiles_dzen2 = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}
{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}

[local-pipe-overview]: http://epsi-rns.github.io/code/2017/04/23/overview-pipe-and-fork.html


[image-hlwm-01-event-idle]:  {{ asset_path }}/herbstclient-01-event-idle.png
[image-hlwm-02-tag-status]:  {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-06-event-custom]: {{ asset_path }}/herbstclient-06-event-custom.png

[image-hlwm-ss-event]: {{ asset_path }}/hlwm-event-ss.png

[local-overview]: {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/06/12/herbstlustwm-event-idle-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/06/13/herbstlustwm-event-idle-perl.html
[local-python]:   {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/06/16/herbstlustwm-event-idle-php.html
[local-lua]:      {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html

[dotfiles-dzen2-bash]:    {{ dotfiles_dzen2 }}/bash
[dotfiles-dzen2-perl]:    {{ dotfiles_dzen2 }}/perl
[dotfiles-dzen2-python]:  {{ dotfiles_dzen2 }}/python
[dotfiles-dzen2-ruby]:    {{ dotfiles_dzen2 }}/ruby
[dotfiles-dzen2-php]:     {{ dotfiles_dzen2 }}/php
[dotfiles-dzen2-lua]:     {{ dotfiles_dzen2 }}/lua
[dotfiles-dzen2-haskell]: {{ dotfiles_dzen2 }}/haskell

[dotfiles-lemon-bash]:    {{ dotfiles_lemon }}/bash
[dotfiles-lemon-perl]:    {{ dotfiles_lemon }}/perl
[dotfiles-lemon-python]:  {{ dotfiles_lemon }}/python
[dotfiles-lemon-ruby]:    {{ dotfiles_lemon }}/ruby
[dotfiles-lemon-php]:     {{ dotfiles_lemon }}/php
[dotfiles-lemon-lua]:     {{ dotfiles_lemon }}/lua
[dotfiles-lemon-haskell]: {{ dotfiles_lemon }}/haskell
