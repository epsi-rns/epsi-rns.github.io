---
layout     : post
title      : "Piping and Forking Overview"
date       : 2017-04-23 17:35:15 +0700
categories : code
tags       : [coding, conky, dotfiles]
keywords   : [pipe and fork, dzen2, lemonbar, overview, summary]
author     : epsi
toc        : toc/2017/04/pipe-and-fork-language.html

excerpt:
  Overview on how to be a Plumber.
  
related_link_ids: 
  - 17042335  # Pipe and Fork Overview
  - 17041535  # Pipe and Fork BASH
  - 17041635  # Pipe and Fork Perl
  - 17041735  # Pipe and Fork Python
  - 17041835  # Pipe and Fork Ruby
  - 17041935  # Pipe and Fork PHP
  - 17042035  # Pipe and Fork Lua
  - 17042135  # Pipe and Fork Haskell

---

This is just a complementary topic for Dzen, Lemonbar and Conky. 
Where they are also just tools in Desktop Ricing, 
especially Tiling Windww Manager. 
Dive deeper, from decoration, to fundamental code.

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky.

### How not to be a Bashful Plumber.

Piping is a powerful concept in Linux and Unix. 
Concept is deeper than just syntax.
As meaning is more important than grammar.

Piping is very common, and very easy to do it in BASH.
We utilize a lot of Pipe to communicate with Dzen2 and Lemonbar.
But we might need a little googling
when it comes to other language.
This is why I brought this topic.

Beside <code>|</code> character in BASH,
most common method are using <code>popen</code>,
or <code>subProcess</code>.
but there are other mechanism as well,
depend on the language you deal with.

> Becoming plumber automagically.

I have made few example of Pipe, Port BASH to other language,
step by step for your convenience.
So now we have BASH, Perl, Python, Ruby, PHP, Lua, and Haskell.
These will give you overview on how
to flow your stream through pipe conduit.

> Make the script simple, less problem.

In short, this Pipe example has Source and Target

*	Pipe Source: using conky, or internal function

*	Pipe Target: using less command, or dzen2.

Since we want to go for walk step by step,
I use <code>less</code> for Pipe target,
and later <code>dzen2</code> for Pipe Target.
And for feed, we are using <code>conky</code>,
and also function as a pipe source.

> Dark art of daemonizing a process.

For each article I also add Fork example,
so your process can run in the background,
detached from console.

We only fork the previous pipe process,
we use less and dzen2. We do not fork cat,
or any other cute animal.

-- -- --

### Pipe and Fork in Desktop Ricing

I have seen a lot of configuration in dotfiles.
Most utilize external tool written in BASH, Perl of Python.
Between taste and color of your language, it is just about preferences.
If you want to combine your preferred language to configure
Dzen2 or Lemonbar,or any tools in need of piping and fork,
then this tutorial is a good starter kit for you.

For historical reason.
BASH evolve to Perl, Perl Evolve to PHP and Python.
And here we are with Ruby. This is why I put these scripts.

I also add Lua.
Lua only used as embedded script. But I can see Lua's potential.
I have never seen anybody utilize Lua as main scripting tools in dotfiles,
except Conky, and AwesomeWM that use Lua as main scripting configuration.
I myself utilize a lot of Conky a lot. So why not make a start ?

How about Haskell? 
This is not a scripting language but compiled.
The reason is XMonad. Most people is still combined
the Haskell Configuration, with BASH, conky, even Perl.
Makes it looks like a Frankestein.
We need a more unified approach.

And of course you have total Control of the script,
e.g. color across the configuration,
when you call from just one script, instead of many language.

But again, it is a matter of preferences.

### Dzen2 or Lemonbar ?

Both ! I'm using Dzen2 during tutorial.
And also give Lemonbar in source code.

### Blog Post

*	[Piping and Forking in BASH][local-bash]

*	[Piping and Forking in Perl][local-perl]

*	[Piping and Forking in Python][local-python]

*	[Piping and Forking in Ruby][local-ruby]

*	[Piping and Forking in PHP][local-php]

*	[Piping and Forking in Lua][local-lua]

*	[Piping and Forking in Haskell][local-haskell]

### Dotfiles (Source Code)

*	[pipe/bash][dotfiles-BASH]

*	[pipe/perl][dotfiles-Perl]

*	[pipe/python][dotfiles-python]

*	[pipe/ruby][dotfiles-Ruby]

*	[pipe/php][dotfiles-PHP]

*	[pipe/lua][dotfiles-Lua]

*	[pipe/haskell][dotfiles-Haskell]

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[local-bash]:    {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-perl]:    {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-python]:  {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-ruby]:    {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-php]:     {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html
[local-lua]:     {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-haskell]: {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell
