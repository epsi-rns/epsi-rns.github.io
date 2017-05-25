---
layout: post
title:  "Explaining Monad: Overview"
date:   2017-05-20 05:35:15 +0700
categories: code
tags: [coding, haskell]
author: epsi

excerpt:
  Breaking the curse, of the unexplainable Monad. 


related_link_ids: 
  - 16051403  # How Haskell Syntax

---

### The Riddle

Common questions.
What monads are ?
What monads can do ?
How a monad behaves ?
What Monads are good for?

	Monad make, different functions composable, by boxing both input and output.

This is the simplified answer, using intuitive analogy.

-- -- --

### The Monad Cursed Myth

It was stated that **Monad are Cursed**.
The moment that it clicks and you understand what it means,
you are suddenly incapable of explaining it anyone else.
Monad is an abstraction, that is why it is so hard to be explained.
Many smart people has done explaining Monad by definition in Stackoverflow.
All we need is real code example.

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
