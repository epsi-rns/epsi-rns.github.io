---
layout: post-sidemenu-wm
title:  "Standalone Statusbar Overview"
categories: desktop
date      : 2017-04-10 13:35:15 +0700
tags      : [ricing, statusbar, bash]
keywords  : [standalone, dzen2, lemonbar]
author: epsi

excerpt:
  Dzen2, Lemonbar, and Conky are easy when you have guidance.
  Gather some tutorial together.

---

### Preface

It was first April 2017 when I configure my dzen setup,
and I'll speak the truth about dzen2.
Dzen is actually simple, the issue is, there was nobody to help me.
I could not find any good tutorial, sufficient to ease my troubled mind.
There was nothing I could do, but read as many dotshare sample as I could.

I can give you an illustration, a story you can imagine

	"It was a frozen winter in cold war era.
	We are two men, a boy, two women, a husky, and two shotguns.
	After three weeks, we finally configure dzen2.
	But we lost our beloved husky before we finally made it.
	Now, every january, we remember our husky,
	that helped all of us to survive."

Now that the hard day was over,
this tutorial is for you.
Dzen2 is easy when you have guidance.

-- -- --

### Statusbar Tutorial

This tutorial/ guidance/ article is one of some parts.

**Statusbar**

*	[Standalone Statusbar Overview][local-overview]

*	[Standalone Dzen2 Statusbar Using BASH][local-dzen2-bash]

*	[Standalone Dzen2 Statusbar Using Conky][local-dzen2-conky]

*	[Standalone Lemonbar Using Conky][local-lemon-conky]

**Conky**

*	[Standalone CLI System Monitoring Using Conky][local-cli-conky]

*	[Debugging Conky][local-debug-conky]

**Complementary**

*	[Create XBM for your Dzen2][local-xbm-dzen2]

*	[Getting XLFD Font][local-xlfd-font]

-- -- --

### Why Learn Statusbar?

As a complement of Tiling Window Manager. 
People use standalone panel (or statusbar),
e.g. Dzen2, Lemonbar, and stuff.

Let us see this preview image to see what can be achieved by Dzen2.

![Dzen2 Preview][image-01-preview]{: .img-responsive }

The thing is, Tiling Window Manager itself is also not complicated.
Once you get the idea of one Tiling Window Manager,
it is going to be easy to switch beetwen any of them.
Just learn some keystrokes, and everything is functional.
And later you can go deep into configuration if you want.

Furthermore, especially for ricing,
the hardest part is to make a beautiful panel/ statusbar setup.
Most of the time, the panel part, is the longest configuration part.

With understanding of statusbar,
learning Tiling Window Manager, become easier.
You can focus on the Window Manager instead of statusbar stuff.

-- -- --

### Lemonbar and Dzen2

Lemonbar has a more different compared to Dzen2.

1.	The ability to align: left, center, and right, in the same panel.
	With Dzen2, we have to create three panels,
	or just one panel with dzen-textwidth calculation.

2.	Font support. The original Lemonbar only support XDFL.
	But you can install lemonbar-xft-git, that support XFD.
	
3.	Unlike Dzen2, Lemonbar does not support Image. Nor .xbm or other.
	It is also designed to be lightweight in source code.

4.	Underline Feature: It is an important feature,
	to show tagged desktop very nicely, in Tiling Window Manager.

5.	Native transparency support.

6.	Lemonbar has manual page.

-- -- -- 

### Why Conky Guidance

Because I can. 

Just Kidding. I like to learn. And I also like to share what I learn. 
In Short. I don't even know, why I wrote this guidance, in the first place.
Or maybe I'm just to tied to write any preface as an eyecatching entry point.
I don't know what to write. I'm more in push people to read code than
explaining stuff. So All I've got is this paragraph.

Allright. Let me try.

Lemonbar and Dzen2 are great, and the creation
of this cool statusbar can be made easier with Conky.
There is a lot of dotfile out there showing amazing Lemonbar case.
But very few tutorial, so hard for beginner to step in 
to this ricing n00berland.
So yeah. We need a guidance, rather than scattered case.

How about conky? Conky is getting more interesting after
Lua binding. It makes the configuration very flexible, 
because it can be programmed.
With this article, I can say that Conky is proven
to be used as multipurpose system monitor tools,
as long as it takes text feed.
Name it, from conky itself, i3bar, dzen2, lemonbar,
and even command line interface.

-- -- --

### Conky Issue with Tiling Window Manager

	The only caveat is conky is interval based,
	and it does not response to desktop event.

From the perspective of Tiling Window Manager,
we still need a separate dzen panel
for use with desktop event.

No matter how cool Conky Lua is,
I just realize the issue,
that we can not do it all, only with conky lua.

-- -- --

### More thoughts

**First**

Yes I read the Source Code in github.
And it TODO in that repository tells me that
there is a plan to rewrite in C++ in Conky 2.0.
Which is great.

**Reading**:

*	<https://github.com/brndnmtthws/conky>

*	<https://github.com/robm/dzen>

I wonder if anyone would do rewrite dzen2 
in C++ or even Rust just because it can be done.
It is going to be an interesting project.

**Second**

Or maybe a decouple system monitor in conky
to make the output text based only.

1.	A Lua based system monitor framework.

2.	A general desktop output that can get any feed,
	from any scripting language.
	Just like dzen2, but for desktop.

**Finally**

But hey... Get a life.

-- -- --

I think that's all.
Thank you for reading.
Sorry for my english.
I know how terrible it is.

Thank you for Reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}

[local-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html
[local-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html

[image-01-preview]:    {{ asset_path }}/dzen2-01-preview.png
