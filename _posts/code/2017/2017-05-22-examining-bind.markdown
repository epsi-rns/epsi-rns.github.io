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

### Simple Data Type: One Argument

Consider a <code>String -> String</code> function.
Start with simple.

<code>In module:</code>

{% highlight haskell %}
say1 :: String -> String
say1 str = "Hello " ++ str
{% endhighlight %}

<code>In GHCI:</code>

{% highlight haskell %}
> say1 "World"
"Hello World"
{% endhighlight %}

<code>In GHCI: with Functor <$></code>

Functor takes any function that can be mapped.
Whether Maybe context or a list can be mapped.

Here the data <code>(Just "World")</code> unwrapped
into <code>"World"</code>. And the result wrapped back.

{% highlight haskell %}
> say1 <$> (Just "World")
Just "Hello World"
{% endhighlight %}

We can see that from another perspective.
Functor operate inside the list.

{% highlight haskell %}
> say1 <$> ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

<code>In GHCI: with Applicative <*></code>

ApplicativeFunctor also wrapped the function.
So both function and data are wrapped.
And the result also wrapped.
Here we have two wrapper example,
Maybe context as wrapper, and list as wrapper.

{% highlight haskell %}
> (Just say1) <*> (Just "World")
Just "Hello World"
> [say1] <*> ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

	I love girl. Treat a girl like a lady.

-- -- --

### Simple Data Type with Two Argument

We need this two argument to show
how to combine Functor and ApplicativeFunctor.

<code>In module:</code>

{% highlight haskell %}
say2 :: String -> String -> String
say2 str1 str2 = str1 ++ " " ++ str2
{% endhighlight %}

<code>In GHCI:</code>

We can greet plainly.

{% highlight haskell %}
> say2 "Hello" "World"
"Hello World"
{% endhighlight %}

Or Curry the function.

{% highlight haskell %}
> (say2 "Hello") "World"
"Hello World"
{% endhighlight %}

Or even using function application

{% highlight haskell %}
> (say2 "Hello") $ "World"
"Hello World"
{% endhighlight %}

<code>In GHCI: using functor <$></code>

And get the wrapped result.

{% highlight haskell %}
> (say2 "Hello") <$> (Just "World")
Just "Hello World"
{% endhighlight %}

{% highlight haskell %}
> (say2 "Hello") <$> ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

<code>fmap</code> is just another name
for infix <code><*></code> operator.

{% highlight haskell %}
> fmap (say2 "Hello") (Just "World")
Just "Hello World"
{% endhighlight %}

{% highlight haskell %}
> fmap (say2 "Hello") ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

<code>In GHCI: with Applicative <*></code>

Get wrapped, both function and input argument.

{% highlight haskell %}
> Just (say2 "Hello") <*> (Just "World")
Just "Hello World"
{% endhighlight %}

{% highlight haskell %}
> [say2 "Hello"] <*> ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

<code>In GHCI: with Functor <$> for both arguments</code>

Now the function have a way to operate inside each wrapped argument.

{% highlight haskell %}
> say2 <$> (Just "Hello") <*> (Just "World")
Just "Hello World"
{% endhighlight %}

{% highlight haskell %}
> say2 <$> ["Hello"] <*> ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

-- -- --

### Summary of Functor and Applicative

I hope that you see these patterns clearly.
However, these above are just an example of wrapping and unwrapping.
There is a more complex pattern, that is <code>bind >>=</code> operator.

Source Code can be found here.

*	[01-functor.hs][dotfiles-01]

-- -- --

### Bind Introduction

Consider our last simple function.

<code>In module:</code>

{% highlight haskell %}
say1 :: String -> String
say1 str = "Hello " ++ str
{% endhighlight %}

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/bind' %}

[dotfiles-func]: {{ dotfiles_path }}/MyFunc.hs
[dotfiles-01]:   {{ dotfiles_path }}/01-functor.hs
[dotfiles-02]:   {{ dotfiles_path }}/02-bind.hs
[dotfiles-03]:   {{ dotfiles_path }}/03-do.hs
