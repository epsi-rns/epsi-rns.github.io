---
layout: post
title:  "Explaining Monad"
date:   2017-05-20 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Breaking the curse, of the unexplainable Monad. 
  Walking my Monad code by code.  
  Yet another Monad Tutorial.

related_link_ids: 
  - 16051403  # How Haskell Syntax

---

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> This article need a lot of inkscape diagram.
  I'm busy on something else right now.
</div>

### The Riddle

Common questions.
What monads are ?
What monads can do ?
How a monad behaves ?

	What Monads are good for?

Imagine, there is a programming language
that everything is function (as mathematical equation),
and every function called only by dependency,
not by order as it is written.
People come from imperative would haunt by this question.

	How do we make a sequence command,
	called line by line, in such order ?

Here comes <code>do</code> notation as the rescue.
<code>do</code> itself is just a notation,
while Monads take all the horseworks.
Of course this conclusion is oversimplistic.
Monad is an abstraction, more than just for sequencing purpose.
But for now this understanding is enough.
So beginner do not need to worry too much about Monad.

	Do notation allows you to write monadic computations
	using a pseudo-imperative.

Here is how Monad works in binding pattern:
Monad solve sequencing issue in functional programming,
by treat each sequence command as function,
and wrapped each input and output returned from the function,
into box of special monadic type, in a consistent manner,
so that the function can be chained together.
Thus making sequence function possible,
running as the order written.

	Monad make, different functions composable,
	by boxing both input and output.

If you ever need a bigger picture of Monad,
you can watch this Douglas Crockford in YUIConf Evening Keynote
about **Monads and Gonads ()**. He gave his notes on Javascript.
This will make us realize that we might already use Monad,
all this time in our daily coding.
  
*	<https://www.youtube.com/watch?v=9QveBbn7t_c>

It become a must ritual for Haskell beginner,
to write in a blog, explaining about Monad.

	Why don't they tell me about Monad ?
	Not in College, nor in Kindergarten.

-- -- --

### The Monad Cursed Myth

It was stated by **Douglas Crockford** that **Monad are Cursed**.

	The moment that it clicks and you understand what it means,
	You are suddenly incapable of explaining it anyone else.

This curse is made worse by the fact that
after you understand it, you’ll begin to see it everywhere

We already have some people, trying to break the curse,
by quote Monad in simple sentence.

#### Mark Dominus

	Monads are like burritos.

*	<http://blog.plover.com/prog/burritos.html>

I'm sure this guy is a genuinely pure genius.
No need to speak spanish in order to understand Burrito.
Anyone who ever write Monad would understand.
But how do exactly a beginner implement a code is still unclear.

#### Mike Vanier

	A monad is a purely abstract concept,
	with no fundamental relationship 
	to anything you've probably ever heard of before.

*	<http://reg.dorogaved.ru/old/smallfish/haskell/haskell2/YAMT/YAMT0.html>

This is a good read.
A key feature of programming languages is 
their capability for designing abstractions.
Monads are just too general a concept
to be described by a single descriptive analogy.

#### Jeff Newbern

	Monad is a type constructor, a function called return, 
	and a combinator function called bind or >>=. 
	These three elements work together to encapsulate a strategy 
	for combining computations to produce more complex computations.

*	<https://wiki.haskell.org/All_About_Monads>


#### Martin Heuschober

Two sentence from the Intro.

	Monads were brought to solve the problem of IO in Haskell.

*	<https://metalab.at/wiki/images/1/11/MonadTalk.pdf>

Good explanation for Monad, the most dreaded Topic. Very nice slide.

#### Saunders Mac Lane 

	A monad is just a monoid in the category of endofunctors.

*	[A monad is just ...][ref-slide]

You can find a hieroglyphic math explanation,
for this quote, in wiki, easily.
Mediocre Mortal like me should not be burdened by abstraction.
We are not lazy, we just need to write a few line codes.
Maybe the author did not write for beginner audience.

#### Stefan Klinger

	Don’t Panic

*	<http://stefan-klinger.de/files/monadGuide.pdf>

Should I get dual degree in Computer Science and Math
just to get along with these People ?

-- -- --

### Monadic Action

Reference about <code>then >></code> operator
and <code>bind >>=</code> operator can be found here.

*	<https://en.wikibooks.org/wiki/Haskell/do_notation>

#### Function

	Think Function in Haskell as Math Equation

Consider this function

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
we can rewrite <code>greetingAction</code>
using <code>then >></code> operator as below.

{% highlight haskell %}
greetingAction :: IO ()
greetingAction = 
    putStr "Hello " >>
    putStr "World" >>
    putStrLn """
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

#### Bind Operator

Consider this action

{% highlight haskell %}
import Data.Maybe

greetingStr :: Maybe String
greetingStr = do
   str1 <- Just "Hello "
   str2 <- Just "World"
   Just (str1 ++ str2)

main = putStrLn $ fromMaybe "Nothing" greetingStr
{% endhighlight %}

We can rewrite in action above
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

Consider run in GHCI

{% highlight haskell %}
Prelude> import System.Directory
Prelude System.Directory> getHomeDirectory >>= putStrLn
/home/epsi
{% endhighlight %}

There is also good example here 

*	<http://www.idryman.org/blog/2014/01/23/yet-another-monad-tutorial/>

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

#### Processing Symbol

I you need to know more about operator,
you can just ask <code>hoogle</code>.

*	<https://www.haskell.org/hoogle/>

And if you are curious for another operator,
you can read this article and have fun.

*	<http://www.virtuouscode.com/2009/01/30/haskell-operators/>

-- -- --

### How does it works

After searching for about, what behind Monad, I found a diagram here.

*	<https://wiki.haskell.org/Typeclassopedia>

Monad is a special kind of Applicative.
Where Apllicative is special kind of Functor.
Applicative itself is abbreviation of ApplicativeFunctor.
So here some basic class we need to comprehend.

I also found good explanation about these three, using boxing analogy.
I really respect hte reader on how he made everything clear
by putting some funny image on his article. 

*	<http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html>

There's a caveat in boxing analogy.

*	http://learnyouahaskell.com/functors-applicative-functors-and-monoids

-- -- --

### Context: Maybe, Just and Nothing

Let's open <code>GHCI</code> and do some Math.

{% highlight haskell %}
Prelude> 3 * 5
15

Prelude> :t 3
3 :: Num t => t
{% endhighlight %}

Before we begin we need add context to data type.
<code>Maybe</code>, <code>Just</code> and <code>Nothing</code>.
This context approach solved common null value issue.
Now a data Maybe contain Nothing or Just a value.

{% highlight haskell %}
data Maybe a = Nothing | Just a
    deriving (Eq, Ord)
{% endhighlight %}

Back to Prelude in GHCI,
now we can wrap data in a context.

{% highlight haskell %}
Prelude> Just 3
Just 3

Prelude> Nothing
Nothing

Prelude> :t Just 3
Just 3 :: Num a => Maybe a

Prelude> :t Nothing
Nothing :: Maybe a

Prelude> 3 * Just 5
<interactive>:20:1: error:

Prelude> Just 3 * 5
<interactive>:21:1: error:

{% endhighlight %}

<code>Just</code> is just a wrapper.

-- -- --

### Operator Declaration

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

-- -- --

### Bare unwrapped

Consider this Prelude using application operator and its reverse.

{% highlight haskell %}
Prelude> (*3) 5
15

Prelude> (const 2) $ (*3) 5
2

Prelude> ((const 2) . (*3)) 5
2

Prelude> 5 (*3)
<interactive>:4:1: error:

Prelude> import Data.Function
Prelude Data.Function> (*3) 5 & (const 2)
2
{% endhighlight %}

Function is left associative.
Apply left bare function to the bare value right.


-- -- --

### Functor

How do we suppose to do simple thing with data context ?
Here comes the <code><$></code> operator. in rescue.

{% highlight haskell %}
Prelude> (*3) <$> (Just 5)
Just 15

Prelude> (const 2) <$> (*3) <$> (Just 5)
Just 2

Prelude> (const 2) <$> (*3) <$> [1..5]
[2,2,2,2,2]

Prelude> (id) <$> (*3) <$> [1..5]
[3,6,9,12,15]

Prelude> (Just 5) <$> (*3)
<interactive>:2:2: error:

Prelude> 2 <$ [1..5]
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
Prelude> (*3) <$> [1..5]
[3,6,9,12,15]

Prelude> fmap (*3) [1..5]
[3,6,9,12,15]

Prelude> map (*3) [1..5]
[3,6,9,12,15]
{% endhighlight %}

Function are Functor too.
It looks like magic, but it is not.

{% highlight haskell %}
Prelude> (const 2) <$> (*3) <$> Just 5
Just 2

Prelude> twofunc = (const 2) <$> (*3)

Prelude> twofunc 5
2

Prelude> twofunc Just 5
<interactive>:11:1: error:
{% endhighlight %}

-- -- --

### Applicative

How about also wrapping the left function ?
Here comes the <code><*></code> operator.

{% highlight haskell %}
Prelude> Just (*3) <*> Just 5
Just 15

Prelude> pure (*3) <*> Just 5
Just 15

Prelude> Just 5 <*> Just (*3)
<interactive>:3:1: error:
{% endhighlight %}

<code><*></code> is also left associative.
Apply left wrapped function to the context right.

{% highlight haskell %}
Prelude> Just (*3) *> Just 5
Just 5

Prelude> Just (const 2) <*> Just (*3) <*> (Just 5)
<interactive>:20:1: error:
{% endhighlight %}

Using List

{% highlight haskell %}
Prelude> [(*3), (const 2)] <*> [1..5]
[3,6,9,12,15,2,2,2,2,2]
{% endhighlight %}

-- -- --

### Wrapping in Functor, and Applicative

Using boxing analogy.
While <code><$></code> wrapped the value.
The <code><*></code> also wrapped the function.

{% highlight haskell %}
Prelude> (*3) <$> Just 5
Just 15

Prelude> fmap (*3) Just 5
Just 15

Prelude> Just (*3) <*> Just 5
Just 15
{% endhighlight %}

In context

{% highlight haskell %}
Prelude> (*3) <$> Just 5
Just 15

Prelude> (*3) <$> Nothing
Nothing

Prelude> Just (*3) <*> Nothing
Nothing

Prelude> Nothing <*> Just 5
Nothing

Prelude> Nothing <$> Just 5
<interactive>:22:1: error:
{% endhighlight %}

Go further with both.

{% highlight haskell %}
Prelude> (*) <$> Just (3) <*> (Just 5)
Just 15

Prelude> min <$> Just (3) <*> (Just 5)
Just 3
{% endhighlight %}

--

### Monad

Here comes the most interesting part.

Bear in mind.

{% highlight haskell %}
class Monad m where
(>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
Nothing >>= func = Nothing
Just val >>= func = func val
{% endhighlight %}

Consider this function in a module.

{% highlight haskell %}
module MyGreet () where

say :: String -> String -> Maybe String
say text greet = 
    if ((length text) /= 0) && ((length greet) /= 0) 
        then Just (greet ++ text)
        else Nothing
{% endhighlight %}

Load module in GHCI.

{% highlight haskell %}
Prelude> :l MyGreet
[1 of 1] Compiling MyGreet          ( MyGreet.hs, interpreted )
Ok, modules loaded: MyGreet.

*MyGreet> Just "Hello " >>= say "world," >>= say " How are you ?"
Just "Hello world, How are you ?"

*MyGreet> Nothing >>= say "world," >>= say " How are you ?"
Nothing
{% endhighlight %}

-- -- --

### How does it works ?

The <code>Just "Hello"</code> unwrapped 
into plain String <code>"Hello"</code>.
And the rest follow.
We have already make the output wrapped in box.

Confused ?

Me too. Maybe I Just need some more exercises.
And some books to read.

<div class="alert alert-dismissible alert-info">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <strong>Information:</strong> It needs a lot o effort to finish this article.
  Especially experience that also takes some time.
</div>

Meanwhile. Consider go back to <code>adit</code>

*	<http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html>

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )


[ref-slide]: http://slides.com/julientournay/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-what-s-the-problem/fullscreen#/
