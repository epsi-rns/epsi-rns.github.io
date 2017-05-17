---
layout: post
title:  "Loop in Haskell With Map, Part Two"
date:   2017-05-14 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  There is no loop in Haskell. Haskell designed that way.  
  This is an example for beginner
  on how to iterate over hash (dictionary).

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

### Goal of Part Two

	Process Hash (Key-Value Pair) Loop by Iterating on List

This is the topic after previous section about Array (list).

-- -- --

### Using Tuplets as a Pair of Key and Value

Tuplets can contain many elements.
Let's consider tuples contain two element below.
We are going to use it as a base for our hash later.

{% highlight haskell %}
pair :: (String, String)
pair = ("key", "value")
{% endhighlight %}

We can use standar method <code>fst</code> to access first element.
And <code>snd</code> to access second element.

{% highlight haskell %}
main = do
    print $ fst pair
    print $ snd pair
{% endhighlight %}

The use <code>$</code> infix operator is used
to avoid parantheses (round bracket).
It is actually just <code>print(fst(pair))</code>.
I just feel that Haskell syntax is sophisticatedly clearer.

And the result is:

{% highlight haskell %}
"key"
"value"
{% endhighlight %}

-- -- --

#### Alternative Data Structure

There are other tricks to build hash dictionary,
rather than use standard tuples.
I suggest you to take a look at 
the code below to examine the possibility.

*	[github.com/.../dotfiles/.../04-data-type.hs][dotfiles-04-data-type]

-- -- --

### Accessing Using Custom Function

We can recreate our very own special function
that behave like those two standard method above.
And also get rid of the double tick quotation mark in output
by using <code>putStrLn</code>.

{% highlight haskell %}
key   :: (String, String) -> String
key   (k, _) = k

value :: (String, String) -> String
value (_, v) = v

main = do
    putStrLn $ key pair
    putStrLn $ value pair
    putStrLn ""
{% endhighlight %}

And the result is slightly different:

{% highlight haskell %}
key
value
{% endhighlight %}

If you do not like the complexity, 
you can wrap these two function <code>putStrLn $ key</code>,
and leave the argument outside.

{% highlight haskell %}
import System.Process

pair :: (String, String)
pair = ("key", "value")

putKeyLn :: (String, String) -> IO ()
putKeyLn (k, _) = do
    putStrLn k
    
main = do
    putKeyLn pair
{% endhighlight %}

We require to import <code>System.Process</code>,
because we declare <code>IO ()</code> in function.

This will produce:

{% highlight haskell %}
key
{% endhighlight %}

-- -- --

### View Source File:

*	[github.com/.../dotfiles/.../02-tuples.hs][dotfiles-02-tuples]

-- -- --

### Defining Hash

Let us turn our pair of associative key-value,
into a more useful row of pairs.
There many terminology for this, you can call it 
associative array, or hash, or dictionary. 
Consider this color scheme,
that I borrow from google material color.

{% highlight haskell %}
colorSchemes :: [(String, String)]
colorSchemes =
    [("blue50",     "#e3f2fd")
    ,("blue100",    "#bbdefb")
    ,("blue200",    "#90caf9")
    ,("blue300",    "#64b5f6")
    ,("blue400",    "#42a5f5")
    ,("blue500",    "#2196f3")
    ,("blue600",    "#1e88e5")
    ,("blue700",    "#1976d2")
    ,("blue800",    "#1565c0")
    ,("blue900",    "#0d47a1")
    ]
{% endhighlight %}

	I do not put all the google material colors.
	That is just a too long list to be included here.

-- -- --

### Accessing Element

Accessing element of hash using index,
has the same syntax with previous lesson.
After all it is just list of pairs.

{% highlight haskell %}
main = do
    print (colorSchemes !! 2)
{% endhighlight %}

This will produce:

{% highlight haskell %}
("blue200","#90caf9")
{% endhighlight %}

-- -- --

### Iterate with mapM_

So is using side effect with <code>mapM_</code>.
It is as simple as the previous example.

{% highlight haskell %}
main = mapM_ print colorSchemes
{% endhighlight %}

This will produce:

{% highlight haskell %}
("blue50","#e3f2fd")
("blue100","#bbdefb")
("blue200","#90caf9")
("blue300","#64b5f6")
("blue400","#42a5f5")
("blue500","#2196f3")
("blue600","#1e88e5")
("blue700","#1976d2")
("blue800","#1565c0")
("blue900","#0d47a1")
{% endhighlight %}

-- -- --

### Iterate with mapM

How about producing new list with <code>map</code>?
    
{% highlight haskell %}
main = do
    print $ map fst colorSchemes
    putStrLn ""
{% endhighlight %}

Thsi will print raw list value.
Each newly produced element is first tuples element.
The key of pair.

{% highlight conf %}
["blue50","blue100","blue200","blue300","blue400","blue500","blue600","blue700","blue800","blue900"]
{% endhighlight %}

-- -- --

### Chaining Function

As our need grow, we might desire to use chain functions
This will accept the sequence of function
as a whole compound operation.

{% highlight haskell %}
main = do
    mapM_ (print . fst) colorSchemes
    putStrLn ""

    mapM_ (putStrLn . snd) colorSchemes
    putStrLn ""
{% endhighlight %}

These both map will show,
raw of keys (first element), 
and later unquoted values (second element):

{% highlight haskell %}
"blue50"
"blue100"
"blue200"
"blue300"
"blue400"
"blue500"
"blue600"
"blue700"
"blue800"
"blue900"

#e3f2fd
#bbdefb
#90caf9
#64b5f6
#42a5f5
#2196f3
#1e88e5
#1976d2
#1565c0
#0d47a1
{% endhighlight %}

-- -- --

### Iterating mapM_ with Custom Function

Furthermore as the code growing in need of more action,
it is more clear to create new function.
Here we have an example of an IO action procedure.

{% highlight haskell %}
putPairLn :: (String, String) -> IO ()
putPairLn (key, value) = do
    putStrLn(key ++ " | " ++ value)

main = do    
    mapM_ putPairLn colorSchemes
{% endhighlight %}

This will display:

{% highlight conf %}
blue50 | #e3f2fd
blue100 | #bbdefb
blue200 | #90caf9
blue300 | #64b5f6
blue400 | #42a5f5
blue500 | #2196f3
blue600 | #1e88e5
blue700 | #1976d2
blue800 | #1565c0
blue900 | #0d47a1
{% endhighlight %}

-- -- --

### Iterating mapM with Custom Function

And here it is, mapping function counterpart.
Producing new list first as a feed to output.

{% highlight haskell %}
pairStr :: (String, String) -> String
pairStr (key, value) = key ++ " | " ++ value

main = do
    mapM_ putStrLn (map pairStr colorSchemes)
    putStrLn ""   
{% endhighlight %}

This will show the same result as above.

While doing <code>mapM_</code>
after <code>map</code> seems redundant.
It is just an example required in this tutorial,
not everything have to be printed out.

### View Source File:

*	[github.com/.../dotfiles/.../03-dictionary.hs][dotfiles-03-dictionary]

-- -- --

I hope it is clear, on how simple <code>map</code> is,
compare to <code>for loop</code> counterpart.

However, combining map with function is tricky.
This topic deserve an article of its own.


In the next section we will discuss on writing function beyond loop.

Happy Coding.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/map' %}

[local-haskell-dollar]: {{ site.url }}/code/2016/05/14/haskell-dollar-syntax.html
[local-hlwm-haskell]:  {{ site.url }}/desktop/2017/05/08/herbstlustwm-modularized-haskell.html

[dotfiles-01-list]:             {{ dotfiles_path }}/01-list.hs
[dotfiles-02-tuples]:           {{ dotfiles_path }}/02-tuples.hs
[dotfiles-03-dictionary]:       {{ dotfiles_path }}/03-dictionary.hs
[dotfiles-04-data-type]:        {{ dotfiles_path }}/04-data-type.hs
[dotfiles-05-passing-argument]: {{ dotfiles_path }}/05-passing-argument.hs
[dotfiles-06-passing-argument]: {{ dotfiles_path }}/06-passing-argument.hs
