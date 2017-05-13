---
layout: post
title:  "Example of Doing Loop in Haskell With Map"
date:   2017-05-13 05:35:15 +0700
categories: code
tags: [coding, haskell, language]
author: epsi

excerpt:
  There is no loop in Haskell. Haskell designed that way.  
  This is an example for beginner on how to iterate
  over hash or array using mapM_.

related_link_ids: 
  - 16051403  # How Haskell Syntax
  - 16051102  # XMonad with Conkyless Dzen
  - 16032658  # Modularized XMonad Config

---

This day is exactly 364 days, since my first Haskell article.
I'm so excited, that I could finished this loop article, a year after.

*	[How Haskell Syntax can Make Your Code Cleaner][local-haskell-dollar]

As a pure functional language, Haskell has been designed,
in a way that, it does not have any loop clause.
Moving perspective from **how** the code do, to **what** the code does.

This is an example, from beginner, to another beginner.
Using <code>mapM_</code> to mimic <code>foreach</code>,
applied for array, and hash with key and value.
There are other method as well, but this is enough for beginner.

This obscure Haskell language has different approach,
compared with imperative programming,
and this <code>mapM_</code> does affect codes beyond the loop.
So this example must go further. 

Let's walk step by step

*	List

*	Tuple

*	Dictionary

*	Passing Argument

*	Real World and Comparation

It is going to be a long article.
So I won't speak too much.
More on codes, than just words.

-- -- --

### Preparation

	In Haskell we have to be ready to deal with operators.

A least, we are using these operators.
Not to mention function declaration.

*	dollar <code>$</code>,

*	dot <code>.</code>,

*	index <code>||</code>,

*	IO  <code><-</code>, and  

*	lambda <code>\</code>.

Do not let it intimidate you,
we are going to use it step by step.

-- -- --

### Loop Over List

Haskell has a few Array implementation.
One that is very common is List.
It is actually a linked list that behaves like an Array.

Let's consider a list construct, contain sequence number from 1 to 9.

{% highlight haskell %}
list = [1..9]
{% endhighlight %}

Not everything in Haskell is function.
And it is not mandatory to write function declaration.
For clarity reason, I put function declaration.
Also for my personal exercise. It won't harm anyone.

Let's add other element to this list.

{% highlight haskell %}
list :: [Int]
list = [1..9] ++ [0]

main = do
    putStr "list    : "
    print list
{% endhighlight %}

Run this code, and this will have output as below.

{% highlight conf %}
list    : [1,2,3,4,5,6,7,8,9,0]
{% endhighlight %}

#### Accessing Index

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
    print $ indices' list
    putStrLn ""
{% endhighlight %}

Note that I intentionally using,
apostrophe <code>indices'</code> punctuation mark,
to differ from <code>indices</code>
in <code>Control.Lens</code> library.

This will show:

{% highlight conf %}
list    : [1,2,3,4,5,6,7,8,9,0]
indices : [0,1,2,3,4,5,6,7,8,9]
{% endhighlight %}

#### Iterate with mapM_

Looping over array in Haskell is this simple.

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

If you come from other language,
and too confused about Haskell notation,
you may consider this perspective:
<code>mapM_(callback_function, array)</code>.

#### View Source File:

*	[github.com/.../dotfiles/.../01-list.hs][dotfiles-01-list]

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

#### Accessing Using Custom Function

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

#### View Source File:

*	[github.com/.../dotfiles/.../02-tuples.hs][dotfiles-02-tuples]

-- -- --

### Loop Over Dictionary

Let's turn our pair of associative key-value,
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

#### Accessing Element

Accessing element of hash using index,
has the same syntax.
After all it is just list of pairs.

{% highlight haskell %}
main = do
    print (colorSchemes !! 2)
{% endhighlight %}

This will produce:

{% highlight haskell %}
("blue200","#90caf9")
{% endhighlight %}

#### Iterate with mapM_

So is using <code>mapM_M</code>,
it is as simple as the previous example.

{% highlight haskell %}
    mapM_ print colorSchemes
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

### Chain Function

As our need grow, we might desire to use more than one function.
The issue is <code>mapM_M</code> only accept one function.
The solution is to chain functions
with the dot <code>.</code> infix operator.
This will accept the chained function as one compound operation.

{% highlight haskell %}
main = do
    mapM_ (print . fst) colorSchemes
    putStrLn ""
    
    mapM_ (putStrLn . snd) colorSchemes
    putStrLn ""
{% endhighlight %}

These both map will show,
row keys, and later unquoted values:

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

#### Custom Function

Furthermore as the code growing in need of more action,
it is more clear to create new function.
Here we have an example of an IO procedure.

{% highlight haskell %}
putPairLn :: (String, String) -> IO ()
putPairLn (key, value) = do
    putStrLn(key ++ " | " ++ value)

main = do    
    mapM_ putPairLn colorSchemes
{% endhighlight %}

This will display:

{% highlight haskell %}
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

I hope it is clear, on how simple <code>mapM_</code> is,
compare to <code>for loop</code> counterpart.

#### View Source File:

*	[github.com/.../dotfiles/.../03-dictionary.hs][dotfiles-03-dictionary]

-- -- --

### Data Type Naming

Since we might use a lot <code>(String, String)</code> in our code.
It is a good idea to use synonim, to avoid repetitive typing.
This <code>Pair</code> type define tuples with two elements.

{% highlight haskell %}
type Pair = (String, String)

pair :: Pair
pair = ("key", "value")
{% endhighlight %}

#### Alternative Data Structure

There are other tricks to build hash dictionary,
rather than use standard tuples.
I suggest you to take a look at 
the code below to examine the possibility.

*	[github.com/.../dotfiles/.../04-data-type.hs][dotfiles-04-data-type]

-- -- --

### Passing Arguments

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

#### Iterate with mapM_ using Curry Function

Let's consider our material color again,
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
Since I'm a just snother beginner, I suggest you to read about 
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

#### Using Lambda with mapM_

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

#### View Source File:

*	[github.com/.../dotfiles/.../04-passing-argument.hs][dotfiles-04-passing-argument]

-- -- --

### Real World Application

Really ? Does above function scary ?
We should compare with other languages.
In fact, we will find out,
that Haskell Syntax is clear enough.

Allright, I must admit,
that I'm doing this tutorial guidance step by step,
because I have difficulties in doing HerbstluftWM config.
Not just Haskell, every language has their own challenge.

This is what I got in Haskell, very similar to our final example above.
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

-- -- --

Happy Coding.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/notes/haskell/map' %}

[dotfiles-01-list]:             {{ dotfiles_path }}/01-list.hs
[dotfiles-02-tuples]:           {{ dotfiles_path }}/02-tuples.hs
[dotfiles-03-dictionary]:       {{ dotfiles_path }}/03-dictionary.hs
[dotfiles-04-data-type]:        {{ dotfiles_path }}/04-data-type.hs
[dotfiles-05-passing-argument]: {{ dotfiles_path }}/05-passing-argument.hs

[dotfiles-layout]: https://github.com/epsi-rns/dotfiles/blob/master/xmonad/xmonad-dzen-2/lib/MyLayoutHook.hs
[image-ss-xmonad-dollar]: {{ site.url }}/assets/posts/code/2016/05/haskell-dollar.png
[photo-ss-xmonad-dollar]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOsazbKB0CKjB1fbBIs1pD9bLhbZoDbfIOc8Uo4

[image-ss-xmonad-layout]: {{ site.url }}/assets/posts/code/2016/05/haskell-layout.png
[photo-ss-xmonad-layout]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPkC85qB-0XNSzPm683dbSdzwgAEq9Y6N0-0vxu

[local-Haskell-dollar]: {{ site.url }}/code/2016/05/14/haskell-dollar-syntax.html
