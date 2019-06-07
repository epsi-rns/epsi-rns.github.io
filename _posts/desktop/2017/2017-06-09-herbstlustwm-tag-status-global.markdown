---
layout: post
title:  "HerbstluftWM Tag Status Global Notes"
date      : 2017-06-09 17:35:15 +0700
categories: desktop
tags      : [coding, herbstluftwm, dotfiles]
keywords  : [tag status, lemonbar, dzen2]
author: epsi

excerpt:
  Global Variables Notes
  on Doing HersbtluftWM Tag Status
  implementation for both Lemonbar and Dzen2. 
  
related_link_ids: 
  - 17060135  # Tag Status Overview
  - 17060235  # Tag Status BASH
  - 17060335  # Tag Status Perl
  - 17060435  # Tag Status Python
  - 17060535  # Tag Status Ruby
  - 17060635  # Tag Status PHP
  - 17060735  # Tag Status Lua
  - 17060835  # Tag Status Haskell
  - 17060935  # Tag Status Global Notes

---

### Global Variable and Constant

	Why discuss about Coding here ?

Because I also include Haskell.
And Haskell distinct clearly between global constant and mutable state.

	Global variable is requirement in this project

Some say that global variable are bad.
But there are quick and dirty condition,
where global variable looks more suitable.
The other way is change the entire logic design.

This <code>output module</code> require a variable
that can be called from function,
without passing them as a parameter argument.
So we have to make scope of these variable global.
But since these variables live only in output module,
these are not so global after all.

Before getting deep, we need to distinct
between global variable and global constant.

*	global variable: mutable state, must be initialized.

*	global constant: it is usually defined as a global variable,
	except that, the value won't be altered during script execution.
	The value is defined at the beginning of program.

#### Global Variable

Accessing a global variable from within function
need different trick for each language.

*	BASH: Variable is global by default.

*	Perl: Variable defined using <code>my</code>
	can be accessed within function in the same module.

*	Python: Access within function using <code>global</code>

*	Ruby: In Module scope, variable defined using <code>@</code>
	can be accesed within function.

*	PHP: Access within function using <code>global</code>

*	Lua: Variable scope using table module such as <code>_M.</code>

*	Haskell: Officialy there is a no way to define global variable Haskell.
	But Haskell provide a few workaround to simulate mutable state.
	The easiest one is using <code>unsafePerformIO</code>.

#### Global Constant

Each language maintain global constant differently

*	BASH: There are ways to define constant in BASH.
	Constant in PHP may begin with the word <code>readonly</code>.

*	Perl: There are a several ways to define constant in Perl.
	Constant in Perl may begin with the word <code>use const</code>.

*	Python: Officialy there is a no way to define constant in Python.
	Python does not differ between these two.

*	Ruby: Constant in Ruby start with capital case.

*	PHP: Constant in PHP begin with the word <code>const</code>.

*	Lua: Officialy there is a no way to define constant in Lua.
	Lua does not differ between these two.

*	Haskell: Every variables in Haskell are immutable.

#### Global Conclusion

There is no special need to worry about global variable,
except with Haskell.

-- -- --

### Blog Post

The rest is in their respective article.

	Let's get it started.

*	[HerbstluftWM Tag Status Overview][local-overview]

*	[HerbstluftWM Tag Status in BASH][local-bash]

*	[HerbstluftWM Tag Status in Perl][local-perl]

*	[HerbstluftWM Tag Status in Python][local-python]

*	[HerbstluftWM Tag Status in Ruby][local-ruby]

*	[HerbstluftWM Tag Status in PHP][local-php]

*	[HerbstluftWM Tag Status in Lua][local-lua]

*	[HerbstluftWM Tag Status in Haskell][local-haskell]


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
{% assign dotfiles_dzen2 = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}
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
