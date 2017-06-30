---
layout: post
title:  "Loop in Haskell With Map, Part One"
date:   2017-05-13 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  There is no loop in Haskell. Haskell designed that way.  
  This is an example for beginner
  on how to iterate over array (list).

related_link_ids: 
  - 17051235  # Haskell Loop Overview
  - 17051335  # Haskell Loop Part One
  - 17051435  # Haskell Loop Part Two
  - 17051535  # Haskell Loop Part Three
  - 17052035  # Explaining Monad: Overview
  - 16051403  # How Haskell Syntax

---

### Goal of Part One

> Process Array Loop by Iterating on List

This is the basic of loop using iteration,
with simple array like data structure, the list.

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

### Defining List

Haskell has a few Array implementation.
One that is very common is List.
It is actually a linked list that behaves like an Array.

Let's consider a list construct, contain sequence number from 1 to 9.

{% highlight haskell %}
list :: [Int]
list = [1..9]
{% endhighlight %}

It is not mandatory to write function declaration.
Sometime I put function declaration, just for clarity reason,
Also for my personal exercise. It won't harm anyone.
Now we can omit any function declaration for this list.

{% highlight haskell %}
list = [1..9]
{% endhighlight %}

-- -- --

### Printing List

{% highlight haskell %}
list = [1..9]

main = do
    print list
    putStrLn ""
{% endhighlight %}

This will show:

{% highlight conf %}
[1,2,3,4,5,6,7,8,9]
{% endhighlight %}

-- -- --

### More about Printing

We can ignore <code>do</code> clause.
<code>do</code> is just a special syntax for sequencing monad.
The type of <code>main</code>main is <code>IO ()</code>.
IO is a monad, hence omitting do.

Relax, forget about Monad, and just go on.
Omitting <code>do</code> notatin is useful
when creating oneliner example.
Consider this example of the same code rewritten without do clause.
But using <code>then >></code> operator.

<code>print</code> is just <code>putStrLn $ show</code>.
And <code>$</code> is just an operator to avoid closing bracket.
Now we can decompose it as below.

{% highlight haskell %}
main = putStrLn (show [1..9]) >> putStrLn ""
{% endhighlight %}

Or using reverse application operator <code>&</code>.

{% highlight haskell %}
import Data.Function
main = show [1..9] & putStrLn >> putStrLn ""
{% endhighlight %}

-- -- --

### List Variation

Let's add other element to this list.

{% highlight haskell %}
list :: [Int]
list = [1..9] ++ [0]

main = do
    putStr "list    : "
    putStrLn $ show list
{% endhighlight %}

Run this code, and this list will be interpreted as output below.

{% highlight conf %}
list    : [1,2,3,4,5,6,7,8,9,0]
{% endhighlight %}

-- -- --

### Accessing Index

Operator <code>||</code> can be used to accessing element by index.

{% highlight haskell %}
main = do
    print (list !! 3)
{% endhighlight %}

This will show 4, because it is a zero based index:

{% highlight conf %}
4
{% endhighlight %}

Since list is actually a linked list, it does not rely on indices.
For convenience for coder come from other language,
zero based indices can be done this way below.

{% highlight haskell %}
list :: [Int]
list = [1..9] ++ [0]

indices' :: [Int] -> [Int]
indices' l = [0 .. (length l) - 1]

main = do
    putStr "list    : "
    print list
    putStr "indices : "
    putStrLn $ show $ indices' list    
    putStrLn ""
{% endhighlight %}

Do not worry about the dollar <code>$</code> operator.
It is just a Haskell operator to avoid paranthese.
It could be written as <code>putStrLn(show(indices'(list)))</code>

Note that I intentionally using,
apostrophe <code>indices'</code> punctuation mark,
to differ from <code>indices</code>
in <code>Control.Lens</code> library.

This will display:

{% highlight conf %}
list    : [1,2,3,4,5,6,7,8,9,0]
indices : [0,1,2,3,4,5,6,7,8,9]
{% endhighlight %}

-- -- --

### Iterate with mapM_

Looping over array in Haskell is this simple. 
<code>mapM_</code> discard newly produced list,
and use side effect only, in this case print using IO.

{% highlight haskell %}
main = do
    mapM_ print list
{% endhighlight %}

Wait....!!!
This simple !!.... Yes! 
I also thought it was harder, but it turn out to be very short.
This will map each element, and applied it to print function.
Let's see the output

{% highlight conf %}
1
2
3
4
5
6
7
8
9
0
{% endhighlight %}

-- -- --

### Iterate with Map

If you care about tor produce new list,
<code>map</code> is for you.

{% highlight haskell %}
main = do
    print $ map show list
{% endhighlight %}

This show will convert each element to string,
and boxed it into new list of string.

{% highlight conf %}
["1","2","3","4","5","6","7","8","9","0"]
{% endhighlight %}

If you come from other language,
and too confused about Haskell notation,
you may consider this perspective:
<code>map(callback_function, array)</code>.

<code>map</code> is very useful
for transforming one list to another list.
However we still need <code>mapM_</code> or <code>forM_</code>
whenever we need side effect such as display each element using IO.

{% highlight haskell %}
main = do
    mapM_ putStrLn (map show list)
    putStrLn ""
{% endhighlight %}

<code>forM_</code> is just like a <code>mapM_</code>,
with reverse aguments. It is available in <code>Data.Foldable</code>.

{% highlight haskell %}
main = do
    forM_ (map show list) putStrLn
    putStrLn ""
{% endhighlight %}

Both codes have output as shown below.
They are using side effect of
newly produced <code>(map show list)</code>.

{% highlight conf %}
1
2
3
4
5
6
7
8
9
0
{% endhighlight %}

While doing <code>mapM_</code> or <code>forM_</code>
after <code>map</code>seems redundant.
It is just an example required in this tutorial,
not everything have to be printed out.

-- -- --

### Chaining Function

If you wish for another challenge you can make a complex one liner.

{% highlight haskell %}
main = mapM_ (putStr . (": " ++) . show) ([1..9] ++ [0]) >> putStrLn ""
{% endhighlight %}

Or using reverse argument <code>forM_</code>
to make it looks like regular loop.
With the same result.

{% highlight haskell %}
main = forM_ ([1..9] ++ [0]) (putStr . (": " ++) . show) >> putStrLn ""
{% endhighlight %}

This will show.

{% highlight conf %}
: 1: 2: 3: 4: 5: 6: 7: 8: 9: 0
{% endhighlight %}

How does it works ?
What is this <code>.</code> 
in <code>(putStr . (": " ++) . show)</code> anyway.

As our need grow, you might desire to use more than one function.
The issue is <code>mapM_</code> only accept one function.
The solution is to chain functions
with the dot <code>.</code> infix operator.
This will accept the sequence of function
as a whole compound operation.

-- -- --

### Alternative Loop Form

We can rewrite above loop, even better using <code>map</code>.
Also with the same result.

{% highlight haskell %}
main = putStrLn $ concat  $ map ((": " ++) . show) ([1..9] ++ [0])
{% endhighlight %}

Or <code>concatMap</code>.
Still with the same result.

{% highlight haskell %}
main = putStrLn $ concatMap ((": " ++) . show) ([1..9] ++ [0])
{% endhighlight %}

-- -- --

### Building Block

If you do not like oneliner, you can decompose this complex line
using let clause in do block, or where clause in function.

{% highlight haskell %}
main = do
    let
        myFunctionComposition = (putStr . (": " ++) . show)
        myList = ([1..9] ++ [0])
        in mapM_ myFunctionComposition myList
    
    putStrLn "" 
{% endhighlight %}

{% highlight haskell %}
putList :: [Int] -> IO ()
putList myList =
    mapM_ myFunctionComposition myList
    where
        myFunctionComposition = (putStr . (": " ++) . show)

main = do
    putList ([1..9] ++ [0])    
    putStrLn ""  
{% endhighlight %}

This will produce exactly the same result.

{% highlight conf %}
: 1: 2: 3: 4: 5: 6: 7: 8: 9: 0
{% endhighlight %}

-- -- --

### View Source File:

*	[github.com/.../dotfiles/.../01-list.hs][dotfiles-01-list]

-- -- --

In "[Part Two][local-part-02]" we will discuss about Hash (dictionary).

Happy Coding.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/map' %}

[local-overview]: {{ site.url }}/code/2017/05/12/haskell-loop-with-map.html
[local-part-01]:  {{ site.url }}/code/2017/05/13/haskell-loop-with-map.html
[local-part-02]:  {{ site.url }}/code/2017/05/14/haskell-loop-with-map.html
[local-part-03]:  {{ site.url }}/code/2017/05/15/haskell-loop-with-map.html

[dotfiles-01-list]:             {{ dotfiles_path }}/01-list.hs
[dotfiles-02-tuples]:           {{ dotfiles_path }}/02-tuples.hs
[dotfiles-03-dictionary]:       {{ dotfiles_path }}/03-dictionary.hs
[dotfiles-04-data-type]:        {{ dotfiles_path }}/04-data-type.hs
[dotfiles-05-passing-argument]: {{ dotfiles_path }}/05-passing-argument.hs
[dotfiles-06-passing-argument]: {{ dotfiles_path }}/06-passing-argument.hs
