---
layout: post
title:  "Examining Bind in Haskell"
date:   2017-05-22 01:10:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Uncover the mysterious bind >>= operator in Haskell
  without dark magic.

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

I'm not pretending that I know Monad.
I'm staying away from dark arts.
Instead I'm examining behaviour of this almost magic 
<code>bind >>=</code> operator.
Gentle, step by step.
Reference about <code>then >></code> operator
and <code>bind >>=</code> operator can be found here.

*	<https://en.wikibooks.org/wiki/Haskell/do_notation>


-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/bind' %}

[dotfiles-func]: {{ dotfiles_path }}/MyFunc.hs
[dotfiles-01]:   {{ dotfiles_path }}/01-functor.hs
[dotfiles-02]:   {{ dotfiles_path }}/02-bind.hs
[dotfiles-03]:   {{ dotfiles_path }}/03-do.hs
