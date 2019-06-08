---
layout: post-sidemenu-wm
title:  "Parsing BASH command in ZSH"
date      : 2017-12-30 09:35:15 +0700
categories: code
tags      : [coding, bash, zsh]
keywords  : [clobbering]
author: epsi

opengraph:
  image: /assets/site/images/topics/bash.png

excerpt:
  Parsing BASH config for use with other program
  such as zsh, fish, or maybe Perl and Python.
  
related_link_ids: 
  - 17050135  # HerbstluftWM Overview
  - 17060135  # Tag Status Overview
  - 17061135  # Event Idle Overview
  - 17042335  # Pipe and Fork Overview

---

Good day dear shell coder, hello again.
Almost new year over here.
How about you ?

### Preface

One of my issue I found when I try to port pacaur is that it use a BASH configuration as a part of source.

{% highlight bash %}
. source /etc/makepkg.conf
{% endhighlight %}

And the fact about this config that, BASH is not compatible with ZSH in context of clobbering
<code>opt/*/{doc,gtk-doc}</code>.

{% highlight bash %}
DOC_DIRS=(usr/{,local/}{,share/}{doc,gtk-doc} opt/*/{doc,gtk-doc})
{% endhighlight %}

You can have a look at the difference between BASH and ZSH in figure below.

![BASH and ZSH compatibility][image-ss-difference]{: .img-responsive }

Here is step by step.

*	First, the issue itself: parsing BASH, still in BASH

*	Second, make it an array in BASH

*	Third, make it an array in Z Shell

-- -- --

### Subshell

I asked in stackoverflow,
and somebody is pointing me to the right direction,
using <code>bash -c</code>.

{% highlight bash %} 
#!/usr/bin/env bash

TEXT_DIRS='usr/{,local/}{,share/}{doc,gtk-doc} opt/*/{doc,gtk-doc}'
bash -c echo\ $TEXT_DIRS
{% endhighlight %}

And the output is:

{% highlight bash %} 
usr/doc usr/gtk-doc usr/share/doc usr/share/gtk-doc usr/local/doc usr/local/gtk-doc usr/local/share/doc usr/local/share/gtk-doc
{% endhighlight %}

Now you can put it in variable using command substitution.

{% highlight bash %} 
#!/usr/bin/env bash

TEXT_DIRS='usr/{,local/}{,share/}{doc,gtk-doc} opt/*/{doc,gtk-doc}'
DIRS=$(bash -c echo\ $TEXT_DIRS)
{% endhighlight %}

-- -- --

### In BASH

{% highlight bash %} 
#!/usr/bin/env bash

TEXT_DIRS='usr/{,local/}{,share/}{doc,gtk-doc} opt/*/{doc,gtk-doc}'

DIRS=$(bash -c echo\ $TEXT_DIRS)
echo -e "${DIRS[@]} \n"

DIRS_ARRAY=($DIRS)
for DIR in "${DIRS_ARRAY[@]}"; do 
  echo "$DIR"
done
{% endhighlight %}

And the output is:

![Array from String BASH][image-ss-brace-bash]{: .img-responsive }

{% highlight bash %} 
usr/doc usr/gtk-doc usr/share/doc usr/share/gtk-doc usr/local/doc usr/local/gtk-doc usr/local/share/doc usr/local/share/gtk-doc 

usr/doc
usr/gtk-doc
usr/share/doc
usr/share/gtk-doc
usr/local/doc
usr/local/gtk-doc
usr/local/share/doc
usr/local/share/gtk-doc
{% endhighlight %}

-- -- --

### In ZSH

{% highlight bash %} 
#!/usr/bin/env zsh

TEXT_DIRS='usr/{,local/}{,share/}{doc,gtk-doc} opt/*/{doc,gtk-doc}'

DIRS=$(bash -c echo\ $TEXT_DIRS)
DIRS_ARRAY=("${(@s/ /)DIRS}") 
for DIR in "${DIRS_ARRAY[@]}"; do 
  echo "$DIR"
done
{% endhighlight %}

And the output is:

![Array from String ZSH][image-ss-brace-zsh]{: .img-responsive }

{% highlight bash %} 
usr/doc
usr/gtk-doc
usr/share/doc
usr/share/gtk-doc
usr/local/doc
usr/local/gtk-doc
usr/local/share/doc
usr/local/share/gtk-doc
opt/*/doc
opt/*/gtk-doc
{% endhighlight %}

-- -- --

I think this is enough for today.
Good luck my friend.
Have some fun with your shell.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = site.url | append: '/assets/posts/code/2017/12' %}

[image-ss-difference]: {{ asset_path }}/difference-bash-zsh.png
[image-ss-brace-bash]: {{ asset_path }}/brace-expansion-bash.png
[image-ss-brace-zsh]:  {{ asset_path }}/brace-expansion-zsh.png
