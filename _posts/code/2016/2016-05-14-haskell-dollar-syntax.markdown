---
layout: post
title:  "How Haskell Syntax can Make Your Code Cleaner"
date:   2016-05-14 10:03:15 +0700
categories: code
tags: [xmonad, dotfiles, haskell]
author: epsi

excerpt:
  Haskell syntatic sugar is somehow weird.
  But once you get the idea, you are going to love it.
  Dollar $ infix operator can be your favorite one.

related_link_ids: 
  - 14121259  # XMonad Tiling
  - 14121558  # XMonad Menu Bar
  - 16051102  # XMonad with Conkyless Dzen
  - 16032658  # Modularized XMonad Config

---

Before you read. You can examine a neat <code>$</code> coding style in my old screenshot. It is somehow weird, and it is probably, not something avaliable in other programming language.

**Source**:<br/>
* [github.com/epsi-rns/dotfiles/.../MyLayoutHook.hs][dotfiles-layout]

[![Haskell Dollar Syntax with Minion in XMonad][image-ss-xmonad-dollar]{: .img-responsive }][photo-ss-xmonad-dollar]

* * *

If you love math. You are going to love Haskell.<br/>
I still don't understand Math. Nor Haskell.

Let's consider simple Mathematical function.

> y = f(x)

Parantheses is not mandatory in Haskell. Haskell is a Functional Programming. It has different paradigm.

You can write the representation either

{% highlight haskell %}
y = f(x)
{% endhighlight %}

or 

{% highlight haskell %}
y = f x
{% endhighlight %}

Space in haskell is left associative function application

* * *

Let's consider this chained Mathematical function.

> y = i( h( g( f(x) ) ) ) 

You can write this function in your haskell code with common notation. The result is the same representation as above.

{% highlight haskell %}
y = i( h( g( f(x) ) ) ) 
{% endhighlight %}

This is error prone and you'll get lost in parentheses easily.

Let's see how space as left associative function application works by consider this representation.

{% highlight haskell %}
z = i h g f x
{% endhighlight %}

After translating it back, with parentheses.  It is actually, not something you are expecting.

{% highlight haskell %}
z = ((((i (h) ) g) f) x)
{% endhighlight %}

Now we are facing problem of how to represent chained functions without parentheses.

So how do we solve the problem ?
Off course with another function application.

* * *

Dollar to avoid parentheses.

> <http://learnyouahaskell.com/higher-order-functions#function-application>

$ is right associative function application

Let's again, consider this chained Mathematical function.

> y = i( h( g( f(x) ) ) ) 

Yes, you can write this down again using $ infix notation for avoiding parantheses.

{% highlight haskell %}
y = i $ h $ g $ f x
{% endhighlight %}

* * *

Function Composition.

Dot to chain function.

> <https://wiki.haskell.org/Function_composition>

In math you can write this

> (i . h . g . f )(x) = i( h( g( f(x) ) ) ) 

With haskell yo can simplified into this

{% highlight haskell %}
j = i . h . g . f
y = j x
{% endhighlight %}

* * *

Although

{% highlight haskell %}
y = i $ h $ g $ f x
{% endhighlight %}

and

{% highlight haskell %}
y = i . h . g . f x
{% endhighlight %}

looks similar.

They are not always interchangeable.

* * *

Enough with math. How about real life coding ?

Yes, you can simplify doing those cumbersome parantheses by replacing it with <code>$</code> infix operator as shown in my old screenshot below.

[![Haskell Dollar Syntax in XMonad Layout][image-ss-xmonad-layout]{: .img-responsive }][photo-ss-xmonad-layout]

That's my XMonad Configuration in Haskell that I've been using since 2014.

* * *

Remember. Dollar can make your life easier.
After all, it is never to late to learn Math.

Happy Coding.

[//]: <> ( -- -- -- links below -- -- -- )

[dotfiles-layout]: https://gitlab.com/epsi-rns/dotfiles/blob/master/xmonad/xmonad-dzen-2/lib/MyLayoutHook.hs
[image-ss-xmonad-dollar]: {{ site.url }}/assets/posts/code/2016/05/haskell-dollar.png
[photo-ss-xmonad-dollar]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipOsazbKB0CKjB1fbBIs1pD9bLhbZoDbfIOc8Uo4


[image-ss-xmonad-layout]: {{ site.url }}/assets/posts/code/2016/05/haskell-layout.png
[photo-ss-xmonad-layout]: https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPkC85qB-0XNSzPm683dbSdzwgAEq9Y6N0-0vxu
