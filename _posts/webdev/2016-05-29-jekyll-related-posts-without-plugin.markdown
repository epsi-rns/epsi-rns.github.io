---
layout: post
title:  "Jekyll Related Posts Without Plugin"
categories: webdev
date:   2016-05-29 23:48:15 +0700
tags: [thought, jekyll, liquid]
author: epsi

excerpt:
  Related Posts in a Blog is very common in Wordpress.
  CMS require a database access to create 'Related Posts' feature. 
  Database table, can be simulated using _data yml/json/csv format.


related_link_ids: 
  - 16061225  # Elapsed Time
  - 16061409  # Webcoder Begin
  - 16061051  # Inkscape Stripes
  - 16060951  # Jekyll Archives   

---

Jekyll is a static site generator,
you cannot make any dynamic site request,
but with a litlle imagination, 
you can do your own clever trick.

-- -- --

**Question**:

"I have searched in google, and I can't find any relevan link 
to create 'Related Posts' automatically in Jekyll. 
'Related Posts' is a standard feature in any CMS. 
I can do this manually by typing in post documents,
but it is error prone."

**Problems**:

CMS require a database access to create 'Related Posts' feature. 
It needs table consist of [id, title, url]. 
Id reference is a must have requirement in relational database,
one post to many related-links.

-- -- --

## Tricks

1. Table, can be simulated by simple array.

2. Data in Jekyll can be stored in _data folder in yml/json/csv format.

3. Unique ID, can be generated from post date. Or you can use other method that you like.

.:. Thus, 'read only database access' can be simulated using simple YAML file.

-- -- --

## Step by step

### pages/all_posts.yml

Create a yaml generator using liquid template.
It is easier using generator, than type each post's property manually.

* [github.com/epsi-rns/.../pages/all_posts.yml][source-pages-all-posts]

{% highlight yaml %}
{% raw %}
layout: null
sitemap:
  exclude: 'yes'
---

{% for post in site.posts %}{% unless post.published == false %}{% if post.date %}
- id: {{ post.date | date: "%y%m%d%M" }}
  title: "{{ post.title }}"
  url: {{ post.url }}
{% endif %}{% endunless %}{% endfor %}
{% endraw %}
{% endhighlight %}


### _site/pages/all_posts.yml

This file will be generated automatically.
Copy file above manually to _data/all_posts.yml

What ? Manual ?
Yes. It is basically a hack. Breaking the limit of Jekyll just for fun.

Off course there is a workaround, using ruby, cron or javascript, but it is beyond this scope.

### _data/all_posts.yml

You can see the result here. Especialy the unique ID.
I'm using date as using id, because it is very common to have date variable in each post, beside title. Date/Time, Title, and URL is standard. No need to create new variable name. 

{% highlight liquid %}
- id: 16061409
  title: "Step by Step Guidance to be a Webcoder"
  url: /opensource/2016/06/14/how-a-webcoder-begin.html

- id: 16061316
  title: "The Difference Between DE, Shell, WM and Compositor"
  url: /desktop/2016/06/13/de-wm-shell-difference.html

- id: 16061225
  title: "Elapsed Time, Yet Another Jekyll Liquid Port"
  url: /opensource/2016/06/12/jekyll-liquid-time-elapsed.html
{% endhighlight %}

People may change the title anytime, change the URL if necessary, but rarely change the creation date/time. Title is not Unique, but URL is unique. Although date/time is not unique,it is rare to have a post with a very smae date/time. That's why date can be used as a unique ID.

* [github.com/epsi-rns/.../_data/all_posts.yml][source-data-all-posts]

### Put your 'Related-Posts' in your posts

Using frontmatter in yml format.

{% highlight yaml %}
related_link_ids: 
  - 14010246  # Debian Install
  - 14050934  # My Mageia Experiment
  - 14031331  # Linux Multiboot
  - 14031332  # Wireless in Command Line
  - 16020803  # Update Arch no Bloated
  - 14122608  # Unbundling AUR
  - 14042750  # BlackArch as Repository
  - 14122758  # Selectively BlackArch Tools"
{% endhighlight %}

I know this looks primitive, but this is exactly what CMS do without their fancy GUI.

* [github.com/epsi-rns/.../2014-04-02-arch-install-log.markdown][source-sample-post]

### Transformed into page template

Again, using Liquid.

* [github.com/epsi-rns/.../panel/related-posts.html][source-liquid-panel]

{% highlight liquid %}
{% raw %}
{% unless page.related_link_ids == nil or page.related_link_ids == empty %}
<div class="panel panel-primary hidden-sm">
  <div class="panel-heading">
    <h3 class="panel-title pull-left">Related Posts</h3>
    <span class="fa fa-link pull-right"></span>
    <div class="clearfix"></div>
  </div>
  <div class="panel-body">
  <ul class="recent-post">
    {% for link_id in page.related_link_ids %}
    <li>
      {% for dpost in site.data.all_posts %}
      {% if dpost.id == link_id %}
        <a href="{{ site.baseurl}}{{ dpost.url }}">{{ dpost.title }}</a>
      {% endif %}
      {% endfor %}
    </li>
    {% endfor %}
  </ul>
  </div>
</div>
{% endunless %}
{% endraw %}
{% endhighlight %}

-- -- --

## Sample page in action:

![Jekyll Related Posts][image-jekyll-related-posts]

I put my 'Related-Posts' in my sidebar layout as shown in this figure.

Now your Jekyll behave better than what you can find in stackoverflow.

Smile :-) and Happy Jekyll Brother !


---




[//]: <> ( -- -- -- links below -- -- -- )

[source-pages-all-posts]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/pages/all_posts.yml
[source-data-all-posts]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_data/all_posts.yml
[source-sample-post]: https://raw.githubusercontent.com/epsi-rns/epsi-rns.github.io/master/_posts/system/2014/2014-04-02-arch-install-log.markdown
[source-liquid-panel]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/panel/related-posts.html
[image-jekyll-related-posts]: {{ site.url }}/assets/posts/webdev/2016/06/jekyll-related-posts.png

