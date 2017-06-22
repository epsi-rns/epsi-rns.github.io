---
layout: post-sidemenu-jekyll
title:  "Jekyll Post Archives Without Plugin"
categories: webdev
date:   2016-06-09 03:51:15 +0700
tags: [jekyll, liquid]
author: epsi

excerpt:
  Blog Archives is very common in Wordpress.
  It is also available in Jekyll in many ways.
  Blog Archives can be achieved without plugin.
  You can make your own custom archives,
  that is more suitable for your project.
  Here is my custom liquid archives script
  that you can modify for your own.

related_link_ids: 
  - 16081315  # Side Menu Without Javascript
  - 16081215  # Side Menu Simple Tree
  - 16081115  # Responsive Side Menu
  - 16061225  # Elapsed Time
  - 16061409  # Webcoder Begin
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---

Blog Archives is very common in Wordpress.
It is also available in Jekyll in many ways.

Blog Archives can be achieved without plugin.
You can make your own custom archives,
that is more suitable for your project.

This archive column come from
my custom liquid archives script,
not just another auto widget
as usual at another platform.

* Archives by Year

* Archives by Month

* Sidebar Archive Column

* Navigation Menu: Using YAML Data.

You can modify it for your own.

-- -- --

Before you start.
It is worth to have a peek in this official
jekyll-archives-plugin written in ruby that works with github.

> <https://github.com/jekyll/jekyll-archives/>

I started searching for a pluginless jekyll archives 
and found these amazing script.

> <http://reyhan.org/2013/03/jekyll-archive-without-plugins.html>

> <http://mikerowecode.com/2010/08/jekyll_archives_grouped_by_year.html>

Above script taught me how to use post.next inside the for iteration loop.
I'm  using his method, <code>post.next</code> to mark opening html tag of list it, 
and <code>post.previous</code> to mark the closing tag.

{% highlight liquid %}
{% raw %}
  {% for post in site.posts %}
  ...
    {% unless post.next %}
    ...
      {% assign year = post.date | date: '%Y' %}
      {% assign n_year = post.next.date | date: '%Y' %}
    ...
    {% endunless %}
  ...
  {% endfor %}
{% endraw %}
{% endhighlight %}
    
Unless is conditional statement.
'unless' is a shorter way to say 'if not'.
The idea comes from Ruby.
Liquid template mimics this behaviour.

I can see that these smart coder have
a ruby influence in writing their script style.
You can see how efficient their code,
in just a few lines, and its done.

The issue is when I try to extend for my purpose,
it becomes more and more complicated.
So I decide to rewrite, with different logic.

-- -- --

Instead of relying of post.next, 
I created the html DOM block
utilizing comparison of year number within the for loop.
I know it is primitive compared with previous script,
but the template logic is clearer.

{% highlight liquid %}
{% raw %}
    {% if year != n_year %}
    <ul class="archive-item">
    {% endif %}
{% endraw %}
{% endhighlight %}

I also move all the variable into capture section,
separate the code variable, and DOM block.
Using capture , your liquid template
won't be bloated by huge of unnecessary new lines.

{% highlight liquid %}
{% raw %}
    {% capture cache %}
    ...
      {% assign n_year = post.next.date | date: '%Y' %}
    ...
    {% endcapture %} 
{% endraw %}
{% endhighlight %}
     
You can see the complete result in this archive page displayed by year.
It is just an archive of posts sorted by date.

> <https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/archives-m.html>

-- -- --

Let's go further, archive page displayed by month.
With the same logic, It won't go that complicated.

*	<https://github.com/epsi-rns/epsi-rns.github.io/blob/master/pages/archives-m.html>

I put the logic in _includes section,
because I want to reuse the code.

*	<https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/archives-m.html>

And finally, let's make it a sidebar as below figure.

*	<https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/panel/archives.html>

It is a little bit longer,
because I only show archives with the same month with the current post.
So I have to make a comparison with current post.

{% highlight liquid %}
{% raw %}
    {% capture cache %}
    ...
      {% assign pg_year  = page.date | date: '%Y' %}
      {% assign pg_month = page.date | date: '%m' %}
    ...
    {% endcapture %}
{% endraw %}
{% endhighlight %}

-- -- --

Last, the header navigation bar drop down menu.

The YAML part

> <https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_data/navigation.yml>

And the header part

> <https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/header.html>

{% highlight liquid %}
{% raw %}
    {% for link in site.data.navigation %}
    ...
      {% unless link.dropdown %}
      ...
      {% else %}
        {% for sublink in link.dropdown %}
        ...
        {% endfor %}
      {% endunless %}
    ...
    {% endfor %}
{% endraw %}
{% endhighlight %}
    
-- -- --

Sample screenshot here.


![Jekyll Liquid Archives Without Plugin][image-jekyll-archives]{: .img-responsive }

Sample complete code here.

{% highlight liquid %}
{% raw %}
<div class="panel panel-primary">
  <div class="panel-heading">
    <h3 class="panel-title pull-left">Archives</h3>
    <span class="fa fa-archive pull-right"></span>
    <div class="clearfix"></div>
  </div>
  <div class="panel-body">

  {% for post in site.posts %}  
    {% capture cache %}

      {% assign pg_year  = page.date | date: '%Y' %}
      {% assign pg_month = page.date | date: '%m' %}
    
      {% assign year   = post.date | date: '%Y' %}
      {% assign month  = post.date | date: '%m' %}    
      
      {% assign n_year  = 2000 %}
      {% assign p_year  = 2025 %}
      {% assign n_month = 0 %}
      {% assign p_month = 13 %}    
    
      {% if post.next %}
        {% assign n_year  = post.next.date | date: '%Y' %}
        {% assign n_month = post.next.date | date: '%m' %}
      {% endif %}

      {% if post.previous %}
        {% assign p_year  = post.previous.date | date: '%Y' %}
        {% assign p_month = post.previous.date | date: '%m' %}
      {% endif %}  
      
    {% endcapture %}
    
    {% if year != n_year %}       
      <div class ="archive-year">
        <a href="{{ site.baseurl }}/pages/archives-y#{{ year }}">
        {{ year }}</a>
      </div>
    {% endif %}

    {% if year == pg_year %}    
    {% if year != n_year %}
    <ul class="archive-month">
    {% endif %}

      {% if month != n_month %}
      <li class="list-month">
        <span class ="archive-month">
          <a href="{{ site.baseurl }}/pages/archives-m#{{ year }}-{{ month }}">
            {{ post.date | date: '%B %Y' }}
          </a></span>
      {% endif %}
      
      {% if month == pg_month %}
        {% if month != n_month %}
        <ul class="archive-item">
        {% endif %}
        {% if post.title != page.title %}
          <li class="list-content">    
            <a href="{{ post.url }}">{{ post.title }}</a>        
          </li>
        {% endif %}
        {% if month != p_month %}          
        </ul>   
        {% endif %}
      {% endif %}
      
      {% if month != p_month %}
      </li>
      {% endif %}
    
    {% if year != p_year %}
    </ul>
    {% endif %}
    {% endif %}
    
  {% endfor %}
  </div>
</div> 

{% endraw %}
{% endhighlight %}

It is not very sophisticated.
I hope that, this script is useful.


Thank you for reading.

---

[//]: <> ( -- -- -- links below -- -- -- )

[image-jekyll-archives]: {{ site.url }}/assets/posts/webdev/2016/06/jekyll-archives.png

