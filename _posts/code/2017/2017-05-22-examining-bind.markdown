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

### Dotfiles

I'm using GHCI to show each step of this tutorial.
I put all the function in one module.
This module should be loaded in GHCI.

*	[MyFunc.hs][dotfiles-func]

<code>In module:</code>

{% highlight haskell %}
module MyFunc 
( unboxStr
, putUnbox
, say1, say2, say3, say4
, list1, list3, list4, list5
) where

...
...
{% endhighlight %}

<code>In GHCI:</code>

{% highlight haskell %}
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :l MyFunc
[1 of 1] Compiling MyFunc           ( MyFunc.hs, interpreted )
Ok, modules loaded: MyFunc.
*MyFunc> 
{% endhighlight %}

-- -- --

### Wrapping and unwrapping

Functor, Applicative and Monad, can be seen as
a concept of wrapping and wrapping data.
The box that wrap the data could be 
IO, list or the simple Maybe context.

	These three are standard spells from the book of white wood witch.

This tutorial use String in Maybe context.
In order to print, we need to unwrap the context first.

<code>In module:</code>

{% highlight haskell %}
unboxStr :: Maybe String -> String
unboxStr (Just text) = text
unboxStr Nothing = ""
{% endhighlight %}

<code>In GHCI:</code>

{% highlight haskell %}
> putStrLn $ unboxStr (Just "Hello World")
Hello World
{% endhighlight %}

Consider a more useful function

<code>In module:</code>

{% highlight haskell %}
putUnbox :: Maybe String -> IO ()
putUnbox (Just text) = putStrLn text
putUnbox Nothing = return ()
{% endhighlight %}

<code>In GHCI:</code>

{% highlight haskell %}
> putUnbox (Just "Hello World")
Hello World
{% endhighlight %}

You don't need this function in GHCI.
But you are going to need this when you do compile in command line.

-- -- --


-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/bind' %}

[dotfiles-func]: {{ dotfiles_path }}/MyFunc.hs
[dotfiles-01]:   {{ dotfiles_path }}/01-functor.hs
[dotfiles-02]:   {{ dotfiles_path }}/02-bind.hs
[dotfiles-03]:   {{ dotfiles_path }}/03-do.hs
