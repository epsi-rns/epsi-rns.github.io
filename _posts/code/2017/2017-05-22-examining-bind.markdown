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

### Bind >>= Introduction

Consider our last simple function.

<code>In module:</code>

{% highlight haskell %}
say1 :: String -> String
say1 str = "Hello " ++ str
{% endhighlight %}

We can add a failsafe feature using Maybe context as an output.

{% highlight haskell %}
say3 :: String -> Maybe String
say3 str = 
    if (length str) /= 0
        then Just ("Hello " ++ str)
        else Nothing
{% endhighlight %}

<code>In GHCI:</code>

Now the output already wrapped.
Even with plain call.

{% highlight haskell %}
> say3 "World"
Just "Hello World"
{% endhighlight %}

Before thinking about chaining,
we can rewrite as below.

{% highlight haskell %}
> (Just "World") >>= say3
Just "Hello World"
{% endhighlight %}

You can imagine how the bind operator works.
Just like ApplicativeFunctor,
bind does unwrapping and wrapping operation.

-- -- --

### Chaining

Consider rewrite the function with two arguments,
and with Maybe context feature as an output.

<code>In module:</code>

{% highlight haskell %}
say4 :: String -> String -> Maybe String
say4 text greet = 
    if ((length text) /= 0) && ((length greet) /= 0) 
        then Just (greet ++ text)
        else Nothing
{% endhighlight %}

<code>In GHCI:</code>

Here we can chain each function using <code>bind >>=</code> operator.

{% highlight haskell %}
> Just "Hello " >>= say4 "world," >>= say4 " How are you ?"
Just "Hello world, How are you ?"
{% endhighlight %}

Or using the flip version, <code>reverse bind =<<</code> operator
for convenience.

{% highlight haskell %}
say4 " How are you ?" =<< say4 "world," =<< Just "Hello "
{% endhighlight %}

-- -- --

### Binding List

<code>In module:</code>

Now consider the Functor version that we already have.

{% highlight haskell %}
say1 :: String -> String
say1 str = "Hello " ++ str
{% endhighlight %}

Please pay attention to the function declaration.

{% highlight haskell %}
say1 :: String -> String
{% endhighlight %}

<code>In GHCI:</code>

{% highlight haskell %}
> ["World"] >>= say1
"Hello World"
{% endhighlight %}

{% highlight haskell %}
> say1 =<< ["Lady"]
"Hello Lady"
{% endhighlight %}

{% highlight haskell %}
> ["World, ", "Lady"] >>= say1
"Hello World, Hello Lady"
{% endhighlight %}

{% highlight haskell %}
> ["World"] >>= say2 "Hello" 
"Hello World"
{% endhighlight %}

-- -- --

### Bind Example Using Simple List

Now you can have the idea on
how the list binded with the Monad operator.
By examining the data type in function declaration.

Consider this simple function example.

<code>In module:</code>

{% highlight haskell %}
list1 :: [String] -> [String]
list1 str = map ("Hello " ++) str
{% endhighlight %}

<code>In GHCI:</code>

We can map each element in this list easily.

{% highlight haskell %}
> list1 ["World", "Lady"]
["Hello World","Hello Lady"]
{% endhighlight %}

Please pay attention to the function declaration.

{% highlight haskell %}
list1 :: [String] -> [String]
{% endhighlight %}

Now consider a list function made for bind.
We can rewrite it without <code>map</code> function.

<code>In module:</code>

{% highlight haskell %}
list3 :: String -> [String]
list3 str = 
    if (length str) /= 0
        then ["Hello " ++ str]
        else []
{% endhighlight %}

Please pay attention to the function declaration.

{% highlight haskell %}
list3 :: String -> [String]
{% endhighlight %}

<code>In GHCI:</code>

We can map each element in this list easily.

{% highlight haskell %}
> ["World", "Lady"] >>= list3
["Hello World","Hello Lady"]
{% endhighlight %}

How about empty list ?

{% highlight haskell %}
> [] >>= list3
[]
{% endhighlight %}

The <code>if then else</code> for empty list looks pretty useless in this function.
We are going to have the need for it in the next function.
Or you can just **skip** it.

-- -- --

### List with Two Argument

Consider these two functions with list output.

<code>In module:</code>

{% highlight haskell %}
list4 :: String -> String -> [String]
list4 greet text = 
    if ((length text) /= 0) && ((length greet) /= 0) 
        then [greet ++ " " ++ text]
        else []
{% endhighlight %}

{% highlight haskell %}
list5 :: String -> String -> [String]
list5 greet text = 
    if ((length text) /= 0) && ((length greet) /= 0) 
        then [text ++ " " ++ greet]
        else []
{% endhighlight %}

Please pay attention to the function declaration.

{% highlight haskell %}
list4 :: String -> String -> [String]
list5 :: String -> String -> [String]
{% endhighlight %}

<code>In GHCI:</code>

We can bind different function together.

{% highlight haskell %}
> ["World", "Lady"] >>= list4 "Hello" >>= list5 "Good Morning"
["Hello World Good Morning","Hello Lady Good Morning"]
{% endhighlight %}

Or using flip version operator,
as needed, for conveninence,
with about the same result.

{% highlight haskell %}
> (list4 "Hello" =<< ["World", "Lady"]) >>= list5 "Good Morning"
["Hello World Good Morning","Hello Lady Good Morning"]
{% endhighlight %}

-- -- --

### Summary for Bind

We can examine how <code>bind >>=</code> can
chain different functions together in such order.
The real world may have more complex case,
not just different functions,
but also functions with different input and output.

Source Code can be found here.

*	[02-bind.hs][dotfiles-02]

-- -- --

### Sequencing

By chaining different function together in such order,
we have already make a sequence.
One step closer to have sequence of command.

Consider our last chain, using vanilla monadic code.

{% highlight haskell %}
> ["World", "Lady"] >>= list4 "Hello" >>= list5 "Good Morning"
{% endhighlight %}

We can rewrite in a function, still with vanilla monadic code as below

{% highlight haskell %}
sequ1 :: [String]
sequ1 = 
    ["World", "Lady"] 
    >>= list4 "Hello" 
    >>= list5 "Good Morning"
{% endhighlight %}

Run this in command line, we will have the same result.

{% highlight haskell %}
main = print $ sequ2
{% endhighlight %}

Now we can convert it, for use with <code>do</code> notation.

{% highlight haskell %}
sequ2 :: [String]
sequ2 = do
    x <- ["World", "Lady"]
    y <- list4 "Hello" x
    list5 "Good Morning" y
{% endhighlight %}

In every command inside <code>do</code> notation,
each must have the same data type with the function output.
It is the list, <code>[String]</code>.

-- -- --

### Summary for Do notation

<code>do</code> notation, make sequence of command possible.
Well, I'm still not sure that I have understand <code>Monad</code>.
But I'm sure that many times I greet the World and Lady.

	I like the World. I like a Girl.

Source Code can be found here.

*	[03-do.hs][dotfiles-03]

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/bind' %}

[dotfiles-func]: {{ dotfiles_path }}/MyFunc.hs
[dotfiles-01]:   {{ dotfiles_path }}/01-functor.hs
[dotfiles-02]:   {{ dotfiles_path }}/02-bind.hs
[dotfiles-03]:   {{ dotfiles_path }}/03-do.hs
