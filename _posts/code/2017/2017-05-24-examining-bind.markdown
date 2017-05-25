---
layout: post
title:  "Examining Bind in Haskell: Do Notation"
date:   2017-05-24 09:10:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Monadic Operator.
  Walking the Monad code by code.  
  Yet another Monad Tutorial.

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

While in examining binding article,
we start from bind operator, then convert it to do notation.
This article start from do, and revert it back to monadic code
using a few operator.

I also add Kleiski Fish Operator, 
that is very useful as a shortcut in a do notation.

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

#### Processing Symbol

I you need to know more about operator,
you can just ask <code>hoogle</code>.

*	<https://www.haskell.org/hoogle/>

And if you are curious for another operator,
you can read this article and have fun.

*	[Is your keyboard jammed, or are you just writing Haskell?][ref-virtuous]

-- -- --

### Monadic Action

Reference about <code>then >></code> operator
and <code>bind >>=</code> operator can be found here.

*	<https://en.wikibooks.org/wiki/Haskell/do_notation>

#### Function

	Think Function in Haskell as Math Equation

Suppose we have this very short function.

{% highlight haskell %}
greetingFunction :: String
greetingFunction = "Hello " ++ "World"

main = do
    putStrLn greetingFunction
{% endhighlight %}

#### Action

	Think Action in Haskell as Procedure, Sequence of Command

We can rewrite this as an action

{% highlight haskell %}
greetingAction :: IO ()
greetingAction = do
   putStr "Hello "
   putStr "World"
   putStrLn ""

main = do
    greetingAction
{% endhighlight %}

#### Then Operator

Removing the <code>do</code> special notation,
we can desugar the <code>greetingAction</code>
using <code>then >></code> operator as below.

{% highlight haskell %}
greetingAction :: IO ()
greetingAction = 
    putStr "Hello " >>
    putStr "World" >>
    putStrLn ""
{% endhighlight %}

Writing it oneliner, would make this action looks exactly
like a function. And in fact, it is just a function.

{% highlight haskell %}
greetingAction = putStr "Hello " >> putStr "World" >> putStrLn ""
{% endhighlight %}

<code>then >></code> operator is 
just like <code>bind >>=</code> operator,
except it ignore input. 
We need other example containing input.

<code>do</code> notation does avoid coding horror,
but sooner or later we need to know what is inside.

#### Bind Operator

Consider this action.
Each command has a <code>Maybe String</code> result type.

{% highlight haskell %}
import Data.Maybe

greetingStr :: Maybe String
greetingStr = do
   str1 <- Just "Hello "
   str2 <- Just "World"
   Just (str1 ++ str2)

main = putStrLn $ fromMaybe "Nothing" greetingStr
{% endhighlight %}

We can desugar the action above
into vanilla monadic code without <code>do</code>:

{% highlight haskell %}
greetingStr :: Maybe String
greetingStr =
   Just "Hello " >>= \str1 ->
   Just "World" >>= \str2 ->
   Just (str1 ++ str2)
{% endhighlight %}

Or you can make it oneliner vanilla monadic code.

{% highlight haskell %}
greetingStr = Just "Hello " >>= \str1 ->  Just "World" >>= \str2 ->  Just (str1 ++ str2)
{% endhighlight %}

Here is another IO example, showing you home directory.

{% highlight haskell %}
import System.Directory
main = getHomeDirectory >>= putStrLn
{% endhighlight %}

There is also good example here 

*	<http://www.idryman.org/blog/2014/01/23/yet-another-monad-tutorial/>

-- -- --

### Operation inside Bind

Monads is way to unwrap stuff,
do something about it, and wrap the result.
Or better, Monad give access for method,
to work inside wrapped stuff, without unwrapping it.

This list, will show Monad is overloaded for different types.
Every monad has its own implementation.

{% highlight haskell %}
intList :: [Int]
intList = do 
   x <- [3..5]
   [x*2]

main = putStrLn $ show intList
{% endhighlight %}

This will display

{% highlight conf %}
[6,8,10]
{% endhighlight %}

It can be desugared into oneliner vanilla monadic code.

{% highlight haskell %}
intList = [3..5] >>= \x -> [x*2]
{% endhighlight %}

#### The Monad Class

It is clear that sequence command in do notation is
just functions chained together 
by <code>bind >>=</code> operator
or <code>then >></code> operator.

Now the next question.
What is these two operators has to do with monad ?
So here it is the definition of monad in Prelude 4.9.1.0

{% highlight haskell %}
class Applicative m => Monad m where
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    (>>)        :: forall a b. m a -> m b -> m b

    m >> k = m >>= \_ -> k 

    return      :: a -> m a
    return      = pure

    fail        :: String -> m a
    fail s      = errorWithoutStackTrace s
{% endhighlight %}

Those two are Monad Operators.

-- -- --

### Monadic Composition

	What other practical used, Monads are good for?

There are many I guess. One of them is Kleiski Arrow.
Kleiski Arrow does function composition, just like <code>.</code>,
except it perform monadic effects.
Reference about Kleiski Arrow <code>>=></code>
and <code><=<</code> operator can be found here.
I looks like fish, so we can call this <code>fish</code> operator.

*	<https://elvishjerricco.github.io/2016/10/12/kleisli-functors.html>

#### Function

Consider this function

{% highlight haskell %}
greetStr :: String -> String
greetStr text = "Hello " ++ text
{% endhighlight %}

We can avoid closing bracket
using <code>function application $</code>.

{% highlight haskell %}
main = putStrLn $ greetStr "World"
{% endhighlight %}

Or we can join two function 
using <code>function composition .</code>.

{% highlight haskell %}
main = (putStrLn . greetStr) "World"
{% endhighlight %}

#### Action

Consider this action

{% highlight haskell %}
greetStrIO :: String -> IO String
greetStrIO text = return ("Hello " ++ text)
{% endhighlight %}

Since the output is <code>IO String</code>, while expected input type 
from <code>puStrLn</code> is <code>String</code>.
The composition have to be Monadic. Kleiski Arrow perform this well.

{% highlight haskell %}
import Control.Monad
main = (putStrLn <=< greetStrIO) "World"
{% endhighlight %}

Or Kleisli Composition, using reversed arrow.

{% highlight haskell %}
import Control.Monad
main = (greetStrIO >=> putStrLn) "World"
{% endhighlight %}

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

[local-overview]: {{ site.url }}/code/2017/05/20/explaining-monad.html
[local-part-01]:  {{ site.url }}/code/2017/05/21/explaining-monad.html
[local-part-02]:  {{ site.url }}/code/2017/05/22/examining-bind.html
[local-part-03]:  {{ site.url }}/code/2017/05/23/examining-bind.html
[local-part-04]:  {{ site.url }}/code/2017/05/24/examining-bind.html

[ref-virtuous]: http://www.virtuouscode.com/2009/01/30/haskell-operators/
