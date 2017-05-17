---
layout: post
title:  "Loop in Haskell With Map, Overview"
date:   2017-05-12 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  There is no loop in Haskell. Haskell designed that way.  
  This is an example for beginner
  on how to iterate over array (list).

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

This day is exactly 363 days, since my first Haskell article.
I'm so excited, that I could finished this loop article, a year after.

*	[How Haskell Syntax can Make Your Code Cleaner][local-haskell-dollar]

-- -- --

### Preface

I always thought that learning Haskell
could hurt my brain, and cause brain damage.
Such as the scary myth of no loop in Haskell.
It is partially true, iteration is enough to mimic loop,
I just haven't dig deep enough to find good example, in that time.

As a pure functional language, Haskell has been designed,
in a way that, it does not have any loop clause.
Moving perspective from **how** the code do, to **what** the code does.
This obscure Haskell language has different approach,
compared with imperative programming.

	Not scary at all. In fact, it is interesting.

One day, I dip my toe scripting Haskell, and read some tutorial.
Then I realize that, what I need is,
real world example, for my specific needs.
This examples are available for free,
but scattered in <code>stackoverflow</code>.
This tutorial give gathered those information.
Therefore reader has enough content, before googling.

This article is just complementary tutorial.
Therefore, you still need to read other good tutorial.
After following this tutorial, you will find that,
Haskell is not that hard after all.
Not only, this tutorial show that making loop is easy,
This tutorial also give good tricks as well.

	This tutorial, also contain some common tricks

This guidance won't discuss mind blowing abstraction
such as: Functor, Applicative, Monoid or Monad.
But rather give example of something common,
that you can apply directly,
i.e. array, hash, map, function, and action.
As additional, it also, give example of curry function,
lambda function, eta reduction, and side effect, 
which is common in daily Haskell scripting.

This is an example, from beginner, to another beginner.
Using <code>map</code>, <code>mapM_</code>, <code>mapM</code>
and <code>forM_</code> to mimic loop.

-- -- --

### Preparation

	In Haskell we have to be ready to deal with operators.

A least, we are using these operators.
Not to mention function declaration.

*	dollar <code>$</code> (function application),

*	dot <code>.</code> (function composition),

*	index <code>||</code>,

*	IO  <code><-</code>, and  

*	lambda <code>\</code>.

Do not let it intimidate you,
we are going to use it step by step.

-- -- --

### Example of Doing Loop in Haskell With Map

This tutorial/ guidance/ article is one of three parts.
These three combined is going to be a long article.
So I won't speak too much. More on codes, than just words.

*	[Overview][local-overview]: Preface

*	[Part One][local-part-01]: List

*	[Part Two][local-part-02]: Tuple and Dictionary

*	[Part Three][local-part-03]: Mapping with Function

The first two parts discuss the most common loop,
array in part one and hash in part two.
Considering that combining map with function is tricky,
This deserve this an article of its own in part three.
Part three also contains comparation 
with other Languages, using Real World Function.

-- -- --

And from me as a Haskell beginner.

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

[local-haskell-dollar]: {{ site.url }}/code/2016/05/14/haskell-dollar-syntax.html

[local-overview]: {{ site.url }}/code/2017/05/12/haskell-loop-with-map.html
[local-part-01]:  {{ site.url }}/code/2017/05/13/haskell-loop-with-map.html
[local-part-02]:  {{ site.url }}/code/2017/05/14/haskell-loop-with-map.html
[local-part-03]:  {{ site.url }}/code/2017/05/15/haskell-loop-with-map.html
