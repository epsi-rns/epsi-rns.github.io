---
layout: post
title: "The LAMP Stack Evolution"
date: 2017-07-05 09:25:15 +0700
categories: system
tags: [lamp]
author: epsi

excerpt:
  This is a not so serious LAMP guidance for blogger.

---

### Preface

	Seriously a Blog Joke.

Dear All Webcoder,

I'm thinking about most LAMP guidance. And realized that my own is too specific. I won't write about it anytime soon. I can't even finish my tiling window manager guidance for the next couple of months.

*	<http://epsi-rns.github.io/system/2015/10/16/lamp-stack-manjaro-openrc.html>

[![LAMP OpenRC][image-ss-openrc]{: .img-responsive }][photo-ss-openrc]

It got me thinking of a bigger picture. There more about this stack than the acronym.

-- -- --

### History

	PHP-MySQL always Win for shared hosting. I know I'm cheap.

Now is 2017. We already live with LAMP stack for almost two decades. And one decades ago we have WAMP ported from Linux to Windows.

During this period, there are struggles.

*	Alternate script: the letter, from (P)HP to (P)erl and (P)ython.

*	Alternate database: from MySQL, to PostgreSQL for enterprise. SQLite is okay for static sites, but Firebird never make it to web standard.

*	We have CMS-based shift from just native PHP. 

But PHP-MySQL always win for most people. I mean shared hosting. Cheap, provide quick and dirty solution, large community.

The first five years of the last decades.

1.	PHP-MySQL itself has evolved from CMS-based to Framework. We have Symfony2-Doctrine2 tier, Laravel, and the old CodeIgniter. We also have Ruby on Rails, Django.

2.	The Apache2 webserver also has alternatives, such as Nginx and Lighttpd.

The last five years of the last decades. The struggle is still going on. Now we have Jekyll and Pelican, as static site generator.

Now is 2017. Most distribution has changed the default database, from MySQL to MariaDB. Unfortunate for Drizzle.

	Nomore MySQL

-- -- --

### The Tutorial Issue

	Most adful guidance looks the same, lack of details.

There are already hundreds of LAMP installation tutorial out there in the internet. But they all looks the same.

Almost all tutorial only cover a particular distribution, especially Debian Based, or more specifically Ubuntu. It means almost installation tutorial is using apt-get. Which is some people has switch over to pacman. And other scattered to Fedora DNF, or emerge.

There are also other consideration such as Ubuntu Snappy and Flatpak. And more advance cloud technology such as Docker.

The user is more scattered now. But I can say that 8apt* is still a major package manager, beside *pacman*.

	Do not be mistaken, between awful site and adful site.

-- -- --

### Hidden Tier

Now about the L word from the acronym, the Linux world. 

	No, I'm not talking about the Love Word. Nor Scott Pilgrim.

1.	Linux is not the only OS using this stack, there are already BSD for decades. Something I do not know.

2.	There are different distributions, package management affect installation, and policy affect /etc preconfigured stuff.

3.	There are different init systems, affect maintenance. Such as conservative SysV, systemd, openRC, Ubuntu upstart. And there is also runit that I never heard of.


The stack is actually more like this

> Package Manager <+> Init <+> Web Server <+> Database Server <+> Script 


You can also add your own tier such as

*	Distribution >+> Package Manager

*	Distribution >+> Preconfigured /etc Policy

*	Scripting Language >+> Framework


I appreciate other idea as well. I also open my mind that I could be wrong.

	Do not stop learning.

-- -- --

### Do me a favor

Next time you guyz make a guidance/ tutorial, please consider these.

*	Not everyone is using apt-get anymore. Please also cover Pacman.

*	Not everyone is using systemd. If you want niche tutorial, you can use this differentiation to make your blog looks more than just an ordinary.

*	The combination of the stack. You can have a bunch of articles if you want. Let's do the math.

(2) Package Manager <+> (3) Init <+> (3) Web Server <+> (2) Database Server <+> (3) Scripting Language 

No need to use permutation. Combination is sufficient.

> 2 x 3 x 3 x 2 x 3 = 108 articles

Phew, that a lot of knowledge. It is going to be a busy research for your tutorial.

	Do not worry. 108 is just a number.

To reduce the complexity, you can merge to reduce all of it, into fewer article or even one big article. The issue is **testing**, there are combinations that cannot be merge in testing context.

	42 is the answer of life.

-- -- --

### Testing The Combination

After that cheap talk above, here comes the implementation part.
Running all the combination above in enviroment suitable for testing.

	No multiboot required

How do I, a blogger can do such combination ? Do I need to install them all ?

> Distribution <+> Package Manager <+> Init

I do not desire to ruin my good notebook setup with hundred of OS and partition.
I do not recommend multiboot for this situation.

It takes long time to analyze problem.
Therefore I do not recommend Live OS either.

I bet you all thinking about virtualization, such as VMWare.
Yes it is possible.

But I'm thinking of this hint. Use docker instead. 
This is my choice as a cheap person.
Docker is light, all it need is host terminal.

*	<http://epsi-rns.github.io/system/2016/03/03/docker-demonstration-manjaro.html>

I do not recommend LXC, but I love this figure below. 
Docker is basically has the same result as this LXC figure.

[![LXC Package: pacman, emerge, yum, apt][image-ss-lxc-screenfetch]{: .img-responsive }][photo-ss-lxc-screenfetch]

That's all, a solution in my mind.

	Do not be afraid of Docker.

-- -- --

### Server Side Only

Of course for you beginner, this is just a server side.

In order to learn client side in the browser, you still have to learn 

HTML5 <+> (some) CSS3 Framework <+> (some) JS Framework

Bootstrap and AngularJS is just a framework. I mean there are other framework as well.

And hey, do not forget about design. From font, to application such as GIMP and Inkscape.

	Do not be panic. You just have to go through it.

-- -- --

### Conclusion

Have a nice overtime overnight with LAMP stack evolution.

	Do not cry !



[//]: <> ( -- -- -- links below -- -- -- )

[image-ss-openrc]: {{ site.url }}/assets/posts/system/2015/10/lamp-manjaro-openrc-terminal.png
[photo-ss-openrc]: https://photos.google.com/album/AF1QipOYi1tE3AwxfL0DQCn7eAhQHekVu1xxo2-lHjta/photo/AF1QipMNA4kSsPrBTD28YziByRo-SxTnvtjY1S5eYUkU

[image-ss-lxc-screenfetch]: {{ site.url }}/assets/posts/system/2016/02/lxc-package.png
[photo-ss-lxc-screenfetch]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOytPeGreF98b0YutShuQ7YVKkxeSdUkAcVJUkS
