---
layout: post
title:  "Examining Bind in Haskell: Example using Number"
date:   2017-05-23 07:10:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Bind >>= operator and friends.
  More example if you wish.

related_link_ids: 
  - 17052035  # Explaining Monad: Overview
  - 17052135  # Explaining Monad: References
  - 17052235  # Examining Bind: Hello World
  - 17052335  # Examining Bind: Using Number
  - 17052435  # Examining Bind: Do Notation
  - 17051235  # Haskell Loop Overview
  - 16051403  # How Haskell Syntax

---

	More example if you wish.
	With some more operators.

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This article is my personal note.  
</div>

-- -- --

### Explaining Monad

This tutorial/ guidance/ article is one of some parts.

*	[Overview][local-overview]: Summary.

*	[References][local-part-01]:
	About Monad.

*	[Examining Bind][local-part-02]: 
	Bind <code>>>=</code> operator.
	Hello World Example.

*	[Examining Bind][local-part-03]: 
	<code><*></code> and <code><$></code> operators.
	Personal Notes. Example using Number.

*	[Monadic Operator][local-part-04]:
	Fish <code>>=></code> operator.

The first one is overview, then some references.
The last three parts is all about Example Code.

-- -- --

### How does Monad works

I found good explanation about these three magic words
(functor, applicative, and monad), using boxing analogy.
I really respect the reader on how he made everything clear
by putting some funny image on his article. 

*	[Functors, Applicatives, And Monads In Pictures][ref-adit]

There's a caveat in boxing analogy.

*	<http://learnyouahaskell.com/functors-applicative-functors-and-monoids>

I did this note based on <code>adit</code> works.
But since, it has some differences, with some additional notes also.
I decide to make my own article.

-- -- --

### Context: Maybe, Just and Nothing

Haskell has context that can be wrapped around data type.
<code>Maybe</code>, <code>Just</code> and <code>Nothing</code>.
This context approach solved common null value issue.
Now a data Maybe contain Nothing or Just a value.

{% highlight haskell %}
data Maybe a = Nothing | Just a
    deriving (Eq, Ord)
{% endhighlight %}

Consider open <code>GHCI</code> and do some Math.

{% highlight haskell %}
>3 * 5
15

>:t 3
3 :: Num t => t
{% endhighlight %}

Compare with plain data above,
now we can wrap data in a context.

{% highlight haskell %}
>Just 3
Just 3

>Nothing
Nothing

>:t Just 3
Just 3 :: Num a => Maybe a

>:t Nothing
Nothing :: Maybe a

>3 * Just 5
<interactive>:20:1: error:

>Just 3 * 5
<interactive>:21:1: error:

{% endhighlight %}

<code>Just</code> is just a wrapper. Do not forget the bracket.

-- -- --

### Bare unwrapped

Consider this Prelude using application operator and its reverse.

{% highlight haskell %}
>(*3) 5
15

>(const 2) $ (*3) 5
2

>((const 2) . (*3)) 5
2

>5 (*3)
<interactive>:4:1: error:

>import Data.Function
Prelude Data.Function> (*3) 5 & (const 2)
2
{% endhighlight %}

Function is left associative.
Apply left bare function to the bare value right.

-- -- --

### Functor

How do we suppose to do simple thing with data context?
Here comes the <code><$></code> operator in rescue.

{% highlight haskell %}
>(*3) <$> (Just 5)
Just 15

>(const 2) <$> (*3) <$> (Just 5)
Just 2

>(const 2) <$> (*3) <$> [1..5]
[2,2,2,2,2]

>(id) <$> (*3) <$> [1..5]
[3,6,9,12,15]

>(Just 5) <$> (*3)
<interactive>:2:2: error:

>2 <$ [1..5]
[2,2,2,2,2]

Prelude Data.Functor> [1..5] $> 2
[2,2,2,2,2]
{% endhighlight %}

<code><$></code> is left associative.
Apply left bare function to the wrapped context right.

Lists are functors.
We can rewrite it using <code>fmap</code> alias,
or special <code>map</code> for list.
<code>fmap</code> is a generalisation of <code>map</code>.
<code>map</code> is a specialization of <code>fmap</code>.

{% highlight haskell %}
>(*3) <$> [1..5]
[3,6,9,12,15]

>fmap (*3) [1..5]
[3,6,9,12,15]

>map (*3) [1..5]
[3,6,9,12,15]
{% endhighlight %}

Function are Functor too.
It looks like magic, but it is not.

{% highlight haskell %}
>(const 2) <$> (*3) <$> Just 5
Just 2

>twofunc = (const 2) <$> (*3)

>twofunc 5
2

>twofunc Just 5
<interactive>:11:1: error:
{% endhighlight %}

-- -- --

### Applicative

How about also wrapping the left function ?
Here comes the <code><*></code> operator.

{% highlight haskell %}
>Just (*3) <*> Just 5
Just 15

>pure (*3) <*> Just 5
Just 15

>Just 5 <*> Just (*3)
<interactive>:3:1: error:
{% endhighlight %}

<code><*></code> is also left associative.
Apply left wrapped function to the context right.

{% highlight haskell %}
>Just (*3) *> Just 5
Just 5

>Just (const 2) <*> Just (*3) <*> (Just 5)
<interactive>:20:1: error:
{% endhighlight %}

Using List

{% highlight haskell %}
>[(*3), (const 2)] <*> [1..5]
[3,6,9,12,15,2,2,2,2,2]
{% endhighlight %}

-- -- --

### Wrapping, in Functor and Applicative

Using boxing analogy.
While <code><$></code> wrapped the value.
The <code><*></code> also wrapped the function.

{% highlight haskell %}
>(*3) <$> Just 5
Just 15

>fmap (*3) Just 5
Just 15

>Just (*3) <*> Just 5
Just 15
{% endhighlight %}

In context

{% highlight haskell %}
>(*3) <$> Just 5
Just 15

>(*3) <$> Nothing
Nothing

>Just (*3) <*> Nothing
Nothing

>Nothing <*> Just 5
Nothing

>Nothing <$> Just 5
<interactive>:22:1: error:
{% endhighlight %}

Go further with both.

{% highlight haskell %}
>(*) <$> (Just 3) <*> (Just 5)
Just 15

>min <$> (Just 3) <*> (Just 5)
Just 3
{% endhighlight %}

-- -- --

### Bind

Apply it to bind.

{% highlight haskell %}
> add3 int = [int + 3]

> [1, 3] >>= add3
[4,6]
{% endhighlight %}

And chain 'em.

{% highlight haskell %}
> addme int2 int1 = [int1 + int2]

> subme int2 int1 = [int1 - int2]

> [1, 3] >>= addme 3 >>= subme 2
[2,4]

> [1, 3] >>= subme 2 >>= addme 3
[2,4]
{% endhighlight %}

Rewrite the last sentence in multiline GHCI.

{% highlight haskell %}
> :{
| addme int2 int1 = [int1 + int2]
| subme int2 int1 = [int1 - int2]
| 
| sequ3 :: [Int]
| sequ3 = 
|     [1, 3] 
|     >>= subme 2 
|     >>= addme 3
| :}

> print sequ3
[2,4]
{% endhighlight %}

And finally in <code>do</code> block.

{% highlight haskell %}
> :{
| sequ4 :: [Int]
| sequ4 = do
|     x <- [1, 3] 
|     y <- subme 2 x
|     addme 3 y
| :}

> sequ4 
[2,4]
{% endhighlight %}

-- -- --

### Operator Cheat Sheet

Bear in mind

*	<code>$</code> function application

*	<code>.</code> function composition

*	<code><$></code> <code>fmap</code> as an infix operator.
	<code>`fmap`</code>.

*	<code><*></code> Applicative

*	(<$>) :: Functor f => (a -> b) -> f a -> f b

*	(<$>) = fmap

*	fmap :: Functor f => (a -> b) -> f a -> f b

*	map :: (a -> b) -> [a] -> [b]

*	(<*>) :: f (a > b) -> f a -> f b 

Bear in mind.

{% highlight haskell %}
class Monad m where
(>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
Nothing >>= func = Nothing
Just val >>= func = func val
{% endhighlight %}

More in this blog.

*	<http://www.holger-peters.de/haskell-by-types.html>

-- -- --

### Further Explanation.

Meanwhile. Consider go back to <code>adit</code>

*	[Functors, Applicatives, And Monads In Pictures][ref-adit]


-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

[local-overview]: {{ site.url }}/code/2017/05/20/explaining-monad.html
[local-part-01]:  {{ site.url }}/code/2017/05/21/explaining-monad.html
[local-part-02]:  {{ site.url }}/code/2017/05/22/examining-bind.html
[local-part-03]:  {{ site.url }}/code/2017/05/23/examining-bind.html
[local-part-04]:  {{ site.url }}/code/2017/05/24/examining-bind.html

[ref-adit]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
