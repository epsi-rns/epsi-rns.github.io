---
layout: post
title:  "Explaining Monad: References"
date:   2017-05-21 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Some Book of Witch written by Wizards.

related_link_ids: 
  - 17052035  # Explaining Monad: Overview
  - 17052135  # Explaining Monad: References
  - 17052235  # Examining Bind: Hello World
  - 17052335  # Examining Bind: Using Number
  - 17052435  # Examining Bind: Do Notation
  - 17051235  # Haskell Loop Overview
  - 16051403  # How Haskell Syntax

---

	Confused? Me too.

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

### The Riddle

Common questions.
What monads are ?
What monads can do ?
How a monad behaves ?

	What Monads are good for?

Imagine, you have a paper, solving some math equations.
You can write down those equations anywhere in that paper freely,
and substitute each equation until it solved.
Consider mimic these situation for programming language.
Equation subtitution can be done by function call, until it solved.
Every function called by dependency.

Now imagine, there is a programming language
that everything is function (as mathematical equation),
and every function called only by dependency,
not by order as it is written.
People come from imperative would haunt by this question below.

	How do we make a sequence command,
	called line by line, in such order ?

Here comes Monad in rescue, force a structure to a flow.
And here is how Monad works in binding pattern:
Monad solve sequencing issue in functional programming,
by treat each sequence command as function,
and wrapped each input and output returned from the function,
into box of special monadic type, in a consistent manner,
so that the sequence function can be chained together.
Thus making evaluation control possible,
running as the order written.

	Monad make, different functions composable,
	by boxing both input and output.

In Haskell, <code>do</code> notation make Monad more simple.
<code>do</code> itself is just a notation,
while Monads take all the horseworks.
Of course this conclusion is oversimplistic.
Monad is an abstraction, more than just for sequencing purpose.
But for now this understanding is enough.
So beginner do not need to worry too much about Monad.

	Do notation allows you to write monadic computations
	using a pseudo-imperative.

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

	The moment that it clicks and you understand what it means, you are suddenly incapable of explaining it anyone else.

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

	A monad is a purely abstract concept, with no fundamental relationship to anything you've probably ever heard of before.

*	<http://reg.dorogaved.ru/old/smallfish/haskell/haskell2/YAMT/YAMT0.html>

This is a good read.
A key feature of programming languages is 
their capability for designing abstractions.
Monads are just too general a concept
to be described by a single descriptive analogy.

#### Jeff Newbern

	Monad is a type constructor, a function called return, and a combinator function called bind or >>=. These three elements work together to encapsulate a strategy for combining computations to produce more complex computations.

*	<https://wiki.haskell.org/All_About_Monads>


#### Martin Heuschober

Two sentence from the Intro.

	Monads were brought to solve the problem of IO in Haskell.

*	<https://metalab.at/wiki/images/1/11/MonadTalk.pdf>

Good explanation for Monad, the most dreaded Topic. Very nice slide.

#### Some Helpful People

*	<http://stackoverflow.com/questions/44965/what-is-a-monad>

Long discussion here. 
Get an answer from stackoverflow is 
instantly faster than reading a book.
Just don't forget the get the understanding
by keep reading the concept from holy book.
If you can afford to buy printed one.

#### Saunders Mac Lane 

	A monad is just a monoid in the category of endofunctors.

*	[... monoid in the category of endofunctors][ref-slide]

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

Allright. I respect the writer. I just need a little patient.
After locking myself all alone in my office in saturday night,
reading his explanation. It make sense.
In fact his guide explaining in a gentle way.
I found many important properties, not covered by other blog.

-- -- --

### How does Monad works

After searching for about, what behind Monad, I found a diagram here.

*	<https://wiki.haskell.org/Typeclassopedia>

Monad is a special kind of Applicative.
Although it is not derived directly.
Where Apllicative is special kind of Functor.
Applicative itself is abbreviation of ApplicativeFunctor.
So here some basic class we need to comprehend.

-- -- --

### Specific Application

If you insist, and love operator marching.

*	<https://github.com/xmonad/xmonad/blob/master/src/XMonad/ManageHook.hs>

-- -- --

### Practical Application

We have been doing it in <code>do block</code>.
Everytime.

	Don't be Panic

-- -- --

Thank you for Reading.

[//]: <> ( -- -- -- links below -- -- -- )

[local-overview]: {{ site.url }}/code/2017/05/20/explaining-monad.html
[local-part-01]:  {{ site.url }}/code/2017/05/21/explaining-monad.html
[local-part-02]:  {{ site.url }}/code/2017/05/22/examining-bind.html
[local-part-03]:  {{ site.url }}/code/2017/05/23/examining-bind.html
[local-part-04]:  {{ site.url }}/code/2017/05/24/examining-bind.html

[ref-slide]: http://slides.com/julientournay/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-what-s-the-problem/fullscreen#/
