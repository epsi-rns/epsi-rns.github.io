---
layout: post-sidemenu-jekyll
title:  "Elapsed Time, Yet Another Jekyll Liquid Port"
categories: webdev
date:   2016-06-12 06:25:15 +0700
tags: [thought, jekyll, liquid]
author: epsi

excerpt:
  It is common in a blog, 
  not to write the calendar date of the post,
  but rather elapsed time (e.g some time ago).
  I'm writing new liquid script for use in Jekyll.

related_link_ids: 
  - 16081315  # Side Menu Without Javascript
  - 16081215  # Side Menu Simple Tree
  - 16081115  # Responsive Side Menu
  - 16061409  # Webcoder Begin
  - 16060951  # Jekyll Archives
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---


It is common in a blog, not to write the calendar date of the post, but rather elapsed time (e.g some time ago). I found a good PHP script years ago, cryptic using array and loop. Looks smart, and  efficient. 

![Elapsed Time in Jekyll Liquid][image-jekyll-time-elapsed]{: .img-responsive }

Now I'm using liquid. Liquid is not a scripting language, but rather a templating Engine. A templating engine is designed to have a very limited capability. Liquid in Jekyll does not support array constructor. So I have a porting problem here. 

Again, let's rewrite. KISS. Keep the code simple and stupid.

From PHP to Jekyll Liquid.

Original PHP Script:

* [www.zachstronaut.com/.../php-relative-date-time-string.html][link-zach]

Jekyll Liquid Port:

* [github.com/epsi-rns/.../time-elapsed.html][dotfiles-time-elapsed]

You can examine both script, and see how a different approach can be achieved from just one idea. The code looks entirely altered now.

{% highlight liquid %}
{% raw %}

{% capture spaceless %}
  {% assign now = 'now' | date: '%s' %}
  {% assign postdate = page.date | date: '%s' %}
  {% assign epoch_diff = now | minus: postdate %}
  
  {% assign a_year = 12 * 30 * 24 * 60 * 60 %}
  {% assign a_second = 1 %}
  {% assign a_minute = a_second | times:  60 %}
  {% assign an_hour  = a_minute | times:  60 %}
  {% assign a_day    = an_hour  | times:  24 %}
  {% assign a_week   = a_day    | times:   7 %}
  {% assign a_month  = a_day    | times:  30 %}
  {% assign a_year   = a_day    | times: 365 %}

  {% if (epoch_diff > a_year) %}
     {% assign elapsed_time = epoch_diff | divided_by: a_year %}
     {% assign elapsed_text = 'year' %}
  {% elsif (epoch_diff > a_month) %}
     {% assign elapsed_time = epoch_diff | divided_by: a_month %}
     {% assign elapsed_text = 'month' %}
  {% elsif (epoch_diff > a_week) %}
     {% assign elapsed_time = epoch_diff | divided_by: a_week %}
     {% assign elapsed_text = 'week' %}
  {% elsif (epoch_diff > a_day) %}
     {% assign elapsed_time = epoch_diff | divided_by: a_day %}
     {% assign elapsed_text = 'day' %}
  {% elsif (epoch_diff > an_hour) %}
     {% assign elapsed_time = epoch_diff | divided_by: an_hour %}
     {% assign elapsed_text = 'hour' %}
  {% elsif (epoch_diff > a_minute) %}
     {% assign elapsed_time = epoch_diff | divided_by: a_minute %}
     {% assign elapsed_text = 'minute' %}     
  {% elsif (epoch_diff > a_second) %}
     {% assign elapsed_time = epoch_diff | divided_by: a_second %}
     {% assign elapsed_text = 'second' %}     
  {% else %}
     {% assign elapsed_time = 0 %}
     {% assign elapsed_text = 'second' %}
  {% endif %}  

  {% if elapsed_time > 1 %}
     {% assign elapsed_text = elapsed_text | append: 's' %}  
  {% endif %}       

  {% if elapsed_time > 0 %}
     {% assign elapsed_text = elapsed_text | append: ' ago' %}  
  {% endif %}       

{% endcapture %} 
 
          <span title="{{ page.date | date: "%b %-d, %Y" }}">
            {{ elapsed_time }} {{ elapsed_text }}
          </span>
 
{% endraw %}
{% endhighlight %}


Done.


[//]: <> ( -- -- -- links below -- -- -- )

[link-zach]: http://www.zachstronaut.com/posts/2009/01/20/php-relative-date-time-string.html
[dotfiles-time-elapsed]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/post/time-elapsed.html
[image-jekyll-time-elapsed]: {{ site.url }}/assets/posts/webdev/2016/06/jekyll-time-elapsed.png

