---
layout: post
title:  "Loop in Haskell With Map, Overview"
date:   2017-05-12 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Overview of doing loop in Haskell.   

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

This day is exactly 363 days, since my first Haskell article.
I'm so excited, that I could finished this loop article, a year after.

*	[How Haskell Syntax can Make Your Code Cleaner][local-haskell-dollar]

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

### Preface

I used to think that learning Haskell
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
lambda anonymous function, eta reduction, and side effect, 
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

### Real World Application

Before you begin.

	A snippet on how it looks in Haskell

Allright, I must admit,
that I'm doing this tutorial guidance step by step,
because I have difficulties in doing HerbstluftWM config.
Not just Haskell, every language has their own challenge.

This long explanation in this artcle is
a supporting article for what I wrote here.
Let me pick this code here.

*	[Modularized HerbstluftWM in Haskell][local-hlwm-haskell]

{% highlight haskell %}
do_config :: String -> [Pair] -> IO ()
do_config command pairs = do
    mapM_ (\(key, value) -> do 
            hc(command ++ " " ++ key ++ " " ++ value)
        ) pairs   
{% endhighlight %}

This is what I got in Haskell,
very similar to one of our final example in the end of this article.
It takes hash arguments from a config module,
and run a <code>herbstclient</code> for each key-value pair in config.

-- -- --

### Comparation with Other Languages

Really ? Does above function scary ?
We should compare with other languages.
In fact, we will find out,
that Haskell Syntax is clear enough.

This is how it looks in bash.
Of course it cryptic, we need a hack to pass hash as argument in bash.

{% highlight bash %}
function do_config()
{
    local command="${1}"
    shift
    eval "declare -A hash="${1#*=}
   
    for key in "${!hash[@]}"; do
        local value=${hash[$key]}        
        hc $command $key $value
    done
}
{% endhighlight %}

And this is how I do it in Perl.
Another cryptic notation.

{% highlight perl %}
sub do_config($\%) {
    my ($command, $ref2hash) = @_;
    my %hash = %$ref2hash;

    while(my ($key, $value) = each %hash) { 
        hc("$command $key $value");
    }
}
{% endhighlight %}

How about python ?
Clear !
Self explanatory.

{% highlight python %}
def do_config(command, dictionary):
    for key, value in dictionary.items():
        hc(command+' '+key+' '+value)
{% endhighlight %}

And so is Ruby.

{% highlight ruby %}
def do_config(command, hash)  
    hash.each do |key, value|
        hc(command+' '+key+' '+value)
    end
end
{% endhighlight %}

PHP is also for human being.

{% highlight php %}
function do_config($command, $hash) {
    foreach ($hash as $key => $value) {
        hc($command.' '.$key.' '.$value);
    }
}
{% endhighlight %}

And later Lua. Also simple.

{% highlight lua %}
function _M.do_config(command, hash)
    for key, value in pairs(hash) do 
        _M.hc(command .. ' ' .. key .. ' ' .. value)
    end
end
{% endhighlight %}

-- -- --

Let's begin the tutorial.
In "[Part One][local-part-01]" we will discuss about List (array).

And from me as a Haskell beginner.
Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

[local-haskell-dollar]: {{ site.url }}/code/2016/05/14/haskell-dollar-syntax.html
[local-hlwm-haskell]:  {{ site.url }}/desktop/2017/05/08/herbstlustwm-modularized-haskell.html

[local-overview]: {{ site.url }}/code/2017/05/12/haskell-loop-with-map.html
[local-part-01]:  {{ site.url }}/code/2017/05/13/haskell-loop-with-map.html
[local-part-02]:  {{ site.url }}/code/2017/05/14/haskell-loop-with-map.html
[local-part-03]:  {{ site.url }}/code/2017/05/15/haskell-loop-with-map.html
