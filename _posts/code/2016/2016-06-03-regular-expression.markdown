---
layout: post
title:  "Meet Our Friend, Regular Expression"
date:   2016-06-03 16:53:15 +0700
categories: code
tags: [thought, shell]
author: epsi

excerpt:
  It is easy to replace text with text editor.
  But this is not the case when you need
  to automate repetitive task with coding.
  It takes simpler tools than text editor.
  Luckily there is Regular Expressions,
  that has been around for a few decades.
  Regular Expression is a language
  for string (text) processing.
  Search and replace utilize pattern match.

---

## Regular Expression in command line with sed and awk

Let's remember your Personal Love Song for a while.
And Replace all "<code>Love</code>" word with "<code>Math</code>" word.

Then, put your Favorite sentence
as your comment here.

[![Regular Expression with sed][image-regular-expression]{: .img-responsive }][picasa-regular-expression]

-- -- --

It is easy to replace text with text editor.
But this is not the case when you need
to automate repetitive task with coding.
It takes simpler tools than text editor.

Luckily there is Regular Expressions,
that has been around for a few decades.
Regular Expression is a language
for string (text) processing.
Search and replace utilize pattern match.

-- -- --

Dear Geeks,

Outside of programming language scope,
there are two linux commands helper
utilizing regular expression.
It is the <code>sed</code>.
And the more advance one, the <code>awk</code>.

{% highlight bash %}
 $ man sed
 $ man awk
 $ man gawk
{% endhighlight %}

Now you can do this with this simple command

{% highlight bash %}
 $ sed -E s/love\|Love/Math/ 'Love is.txt'
 $ perl -pe s/love\|Love/Math/ 'Love is.txt'
 $ awk '{gsub(/love|Love/,"Math"); print}' 'Love is.txt'
{% endhighlight %}

Now, that you know regular expressions.
You can reduce your search-and-replace task
in your script to just a very few lines.

Regular Expression It is very helpful.
I've been regular expression in my script
for at least one decade.

**Reading**

* <https://www.gnu.org/software/sed/manual/sed.html>

* <https://www.gnu.org/software/gawk/>

Have Fun 

-- -- --

	How Romantic is Love ?
	How Numeric is Math ?
	Why Roman Numeral ?


[//]: <> ( -- -- -- links below -- -- -- )

[image-regular-expression]: {{ site.url }}/assets/posts/opensource/2016/06/regular-expression.png
[picasa-regular-expression]: https://lh3.googleusercontent.com/-hDo6dt4SQYA/V1_FS4yDhCI/AAAAAAAAAVc/ye6lnmW2G5wZ7eLcqn77UVwvhF0oEpfbwCCo/s0/regular-expression.png
