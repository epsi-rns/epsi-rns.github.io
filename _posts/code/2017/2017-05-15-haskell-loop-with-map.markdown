---
layout: post
title:  "Loop in Haskell With Map, Part Three"
date:   2017-05-15 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  There is no loop in Haskell. Haskell designed that way.  
  This is an example for beginner
  on how to compose function beyon loop iteration.

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

### Goal of Part Three

	Process Hash (Key-Value Pair) with Haskell Function

We have seen an introduction of 
handling map with function in the previous lesson.
Since combining map with function is tricky,
and contain many forms of syntatic sugar.
This deserve this long explanation article.

-- -- --

### Data Type Naming

Considering we might use a lot of 
<code>(String, String)</code> in our code.
It is a good idea to use synonim, to avoid repetitive typing.
This <code>Pair</code> type define tuples with two elements.

{% highlight haskell %}
type Pair = (String, String)

pair :: Pair
pair = ("key", "value")
{% endhighlight %}

*	[github.com/.../dotfiles/.../04-data-type.hs][dotfiles-04-data-type]

-- -- --

### Passing Arguments to Action Procedure

Now we can apply our <code>Pair</code> to new function.
This function has two arguments.

*	First, a text argument, with String type.

*	Second, a tuples, with Pair type.

Since it is an IO action procedure,
it must return <code>IO ()</code>.
This action looks like a void function,
but it is actually not.

{% highlight haskell %}
dumpPair :: String -> Pair -> IO ()
dumpPair text (key, value) = do
    putStrLn(text ++ ": " ++ key ++ " | " ++ value)

main = do
    dumpPair "Test" ("Key", "Value")
{% endhighlight %}

This will do display:

{% highlight haskell %}
Test: Key | Value
{% endhighlight %}

-- -- --

### Reintroduce Data Structure

Considering our material color again,
just in case we forget, or too lazy to scroll.

{% highlight haskell %}
colorSchemes :: [Pair]
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

-- -- --

### Iterate with mapM_ using Curry Function

Now we can do iterate our latest function.
Doing <code>mapM_</code> inside a function.

This function has two arguments.

*	First, a text argument, with String type.

*	Second, a dictionary, with Pair type.

{% highlight haskell %}
dumpHash1 :: String -> [Pair] -> IO ()
dumpHash1 text dictionary = do
    mapM_ (dumpPair text) dictionary
    
main = do
    dumpHash1 "Name" colorSchemes
{% endhighlight %}

Wait ... !??*@...??
Doesn't it defined earlier,
that <code>dumpPair</code> has two arguments ?

The trick in passing argument rely in the closing bracket.
<code>(dumpPair text)</code>. It is called Curry Function.
Since I'm a just another beginner, I suggest you to read about 
Haskell Curry Function somewhere else.

However, the result will echo as below:

{% highlight haskell %}
Name: blue50 | #e3f2fd
Name: blue100 | #bbdefb
Name: blue200 | #90caf9
Name: blue300 | #64b5f6
Name: blue400 | #42a5f5
Name: blue500 | #2196f3
Name: blue600 | #1e88e5
Name: blue700 | #1976d2
Name: blue800 | #1565c0
Name: blue900 | #0d47a1
{% endhighlight %}

It works. And plain simple.

### Using Lambda with mapM_

This is the trickiest part for beginner.
But I must go on, because we will likely to see,
a bunch of lambda everywhere, randomly marching,
in any Haskell source code we meet.
It is because lambda oftenly used as a wrapper of building block.

We can move above function <code>dumpPair</code> 
inside <code>dumpHash2</code> function,
using where clause.

{% highlight haskell %}
dumpHash2 :: String -> [Pair] -> IO ()
dumpHash2 text dictionary = do
    mapM_ (dumpPair' text) dictionary
    where
        dumpPair' text (key, value) = do
            putStrLn(text ++ ": " ++ key ++ " | " ++ value)
{% endhighlight %}

And convert it to lambda later on.
Merge both above function 
<code>dumpPair</code> and <code>dumpHash1</code>
into one <code>dumpHash3</code>.

{% highlight haskell %}
dumpHash3 :: String -> [Pair] -> IO ()
dumpHash3 text dictionary = do
    mapM_ (\(key, value) -> do 
            putStrLn(text ++ ": " ++ key ++ " | " ++ value)
        ) dictionary   
{% endhighlight %}

It looks exactly like <code>foreach</code> loop,
with different syntax. Once we get it, it is more flexible.

Does it look literally cryptic, with operator marching,
scattered all over the place ?
Not really, the most cryptic part is the function declaration.
This function declaration part is not mandatory.
You can safely remove in this situation.
Or just comment out to disable it.

### Debugging

I actually use this function as a based model,
to read key-value pairs from config.
Sometimes strange thing happen in my application,
and I need too see what happened in the process of applying config.

So how if I want some kind IO operation inside, like debug for example.
Well, here it is.

{% highlight haskell %}
dumpHash4 :: String -> [Pair] -> IO ()
dumpHash4 text dictionary = do
    -- loop over a hash dictionary of tuples
    mapM_ (\(key, value) -> do 
            let message = text ++ ": " ++ key ++ " | " ++ value
            
            putStrLn message

            -- uncomment to debug in terminal
            -- putStrLn ("Debug [" ++ message ++ "]")
        ) dictionary  
{% endhighlight %}

Note that in real application,
I replace the line <code>putStrLn message</code>
with my own IO action.

-- -- --

### View Source File:

*	[github.com/.../dotfiles/.../05-passing-argument.hs][dotfiles-05-passing-argument]

-- -- --

### Passing Arguments to Function

Now we can apply our <code>Pair</code> to new function.
This function has two arguments, and one returning value

*	First, a text argument, with String type.

*	Second, a tuples, with Pair type.

*	Return String Type.

{% highlight haskell %}
pairMessage :: String -> Pair -> String
pairMessage text (key, value) = 
    text ++ ": " ++ key ++ " | " ++ value

main = do
    putStrLn $ pairMessage "Test" ("Key", "Value")
    putStrLn ""
{% endhighlight %}

This will show display:

{% highlight haskell %}
Test: Key | Value
{% endhighlight %}

The difference with the function <code>dumpPair</code>,
is in we place IO operation such <code>putStr</code>
outside the function.

-- -- --

### Iterate with mapM using Curry Function

Now we can do iterate our latest function.
Doing <code>map</code> inside a function.

This function has two arguments, and one returning value.

*	First, a text argument, with String type.

*	Second, a dictionary, with List of Pair.

*	Return List of String.

{% highlight haskell %}
hashMessage1 :: String -> [Pair] -> [String]
hashMessage1 text dictionary = 
    map (pairMessage text) dictionary 
    
main = do
    mapM_ putStrLn (hashMessage3 "Name" colorSchemes)
    putStrLn ""
{% endhighlight %}

you should not be surprised with
the curry function <code>pairMessage text</code>.
This wil produce as below:

{% highlight conf %}
Name: blue50 | #e3f2fd
Name: blue100 | #bbdefb
Name: blue200 | #90caf9
Name: blue300 | #64b5f6
Name: blue400 | #42a5f5
Name: blue500 | #2196f3
Name: blue600 | #1e88e5
Name: blue700 | #1976d2
Name: blue800 | #1565c0
Name: blue900 | #0d47a1
{% endhighlight %}

It also works. And plain simple.

-- -- --

### Using Lambda with map

This article is getting more and more repetitious,
But I must go on for an extra mile.

We can move above function <code>pairMessage</code> 
inside <code>hashMessage2</code> function,
using where clause.

{% highlight haskell %}
hashMessage2 :: String -> [Pair] -> [String]
hashMessage2 text dictionary = 
    map (pairMessage' text) dictionary 
    where
        pairMessage' text (key, value) = 
            text ++ ": " ++ key ++ " | " ++ value
{% endhighlight %}

And convert it to lambda later on.
Merge both above function 
<code>pairMessage</code> and <code>hashMessage1</code>
into one <code>hashMessage3</code>.

{% highlight haskell %}
hashMessage3 :: String -> [Pair] -> [String]
hashMessage3 text dictionary = 
    map ( \(key, value) ->
          text ++ ": " ++ key ++ " | " ++ value    
        ) dictionary   
{% endhighlight %}

It looks exactly like <code>foreach</code> loop,
with different syntax. Once we get it, it is more flexible.
**my-repetitious-paragraph**.

-- -- --

### Simplify Main

Considering mking the main clause simpler.
How about moving <code>mapM_ putStrLn</code> inside an action?
Going back to IO action procedure,
we can keep the value of list by using <code>let</code> inside do clause.

{% highlight haskell %}
-- function
dumpHash5 :: String -> [Pair] -> IO ()
dumpHash5 text dictionary = do
    -- loop over a hash dictionary of tuples
    let messages = map ( \(key, value) ->
            text ++ ": " ++ key ++ " | " ++ value
            ) dictionary      
    mapM_ putStrLn messages

main = do
    dumpHash5 "Name" colorSchemes
    putStrLn ""
{% endhighlight %}

How about the output ?
The same as previous sir.

-- -- --

### Where Clause

Well, using <code>where</code> first as usual.
No need to covert into lambda.
We can have another form of this function.

{% highlight haskell %}
dumpHash6 :: String -> [Pair] -> IO ()
dumpHash6 text dictionary = do
    -- loop over a hash dictionary of tuples    
    forM_ messages putStrLn
    where messages = map ( \(key, value) ->
            text ++ ": " ++ key ++ " | " ++ value
            ) dictionary 
{% endhighlight %}

-- -- --

### Debugging

Really ? This tutorial is getting repetitious.
We almost finished.

Again, how if I want **multiple** IO operation inside ?
Such as debugging capability, after applying config.

{% highlight haskell %}
dumpHash7 :: String -> [Pair] -> IO ()
dumpHash7 text dictionary = do
    -- loop over a hash dictionary of tuples    
    forM_ messages debugStrLn
    where 
        debugStrLn message = do 
            putStrLn message

            -- uncomment to debug in terminal
            -- putStrLn ("Debug [" ++ message ++ "]")

        messages = map ( \(key, value) ->
            text ++ ": " ++ key ++ " | " ++ value
            ) dictionary 
{% endhighlight %}

You can try it yourself in your terminal.

-- -- --

### View Source File:

*	[github.com/.../dotfiles/.../06-passing-argument.hs][dotfiles-06-passing-argument]

-- -- --

### Real World Application

Allright, I must admit,
that I'm doing this tutorial guidance step by step,
because I have difficulties in doing HerbstluftWM config.
Not just Haskell, every language has their own challenge.
This long explanation above is a supporting article for what I wrote here.

*	[Modularized HerbstluftWM in Haskell][local-hlwm-haskell]

-- -- --

### Comparation with Other Languages

Really ? Does above function scary ?
We should compare with other languages.
In fact, we will find out,
that Haskell Syntax is clear enough.

This is what I got in Haskell,
very similar to one of our final example above.
It takes hash arguments from a config module,
and run a <code>herbstclient</code> for each key-value pair in config.

{% highlight haskell %}
do_config :: String -> [Pair] -> IO ()
do_config command pairs = do
    mapM_ (\(key, value) -> do 
            hc(command ++ " " ++ key ++ " " ++ value)
        ) pairs   
{% endhighlight %}

And this is how it looks in bash.
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

### Conclusion

Coding in Haskell is fun.
I love it.

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

