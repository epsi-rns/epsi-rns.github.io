---
# layout: post-sidemenu-jekyll
layout: post
title:  "Jekyll Sidemenu Simple Tree"
categories: webdev
date:   2016-08-12 01:15:15 +0700
tags: [jekyll, liquid]
author: epsi

excerpt:
  My tutorial template require a sidebar menu with tree capability.
  Using Liquid template we will have all tree node collapsed,
  except for the current url, th tree should be opened.

related_link_ids: 
  - 16081315  # Side Menu Without Javascript
  - 16081115  # Responsive Side Menu
  - 16061225  # Elapsed Time
  - 16061409  # Webcoder Begin
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---

My tutorial template require a sidebar menu with tree capability.
There are many types of sidebar menus with tree capability

*	Show all tree node opened. 

*	Show all tree node collapsed, except current url opened. Using javascript.

*	Show all tree node collapsed, except current url opened. Using serverside, or static page.

Using Liquid template we will have all tree node opened.
We will discuss a more complex example in next post.

## Stylesheet

There's nothing special,
and you can see required css for sidemenu navigation here.

*	[github.com/../_navsidemenu.scss][github-css-sidemenu]

## Navigation Data

It has been said a thousand times that Jekyll doesn't support database connection.
But we have YAML data as a workaround. 
And actually I find that reading yaml is easier when it comes to tree,
compared with tweaking RDBMS.

We need three fields for each branch [title, url, and submenu tree node]
Let's see our sample <code class="code-file">_data/navigation_wm.yml</code>
to hold my Window Manager (WM) menu.

{% highlight yaml %}
# Site navigation links

- title: Preface
  url: /pages/desktop/index
  submenu:
  - title: Difference WM/DE
    url: /desktop/2016/06/13/de-wm-shell-difference.html
  - title: Dotfiles
    url: https://github.com/epsi-rns/dotfiles/  

- title: Tiling WM
  url: /pages/desktop/twm
  submenu:
  - title: Awesome
    url: /pages/desktop/awesome
    submenu:
    - title: Modular
      url: /desktop/2016/07/13/modularized-awesome-preparing.html
    - title: Directory Structure
      url: /desktop/2016/07/06/modularized-awesome-structure.html
  - title: XMonad
    url: /pages/desktop/xmonad
  - title: i3
    url: /pages/desktop/i3
{% endhighlight %}

See a complete tree in github

*	[github.com/../navigation_wm.yaml][github-data-navigation]

To make the page flexible,
let's decouple the data source name from the processing file.

In the layout file <code class="code-file">_layouts/page-sidemenu.html</code>
we will pass the variable name:

{% highlight liquid %}
{% raw %}
  {% include layout/sidemenu.html 
     menusource = site.data.navigation_wm
  %}
{% endraw %}
{% endhighlight %}

In the include file <code class="code-file">_includes/layout/sidemenu.html</code>
we will use the variable as below:

{% highlight liquid %}
{% raw %}
  <ul class="nav" id="menu">
  {% for link in include.menusource %}
  ...
  {% endfor %}
  </ul>
{% endraw %}
{% endhighlight %}

## A Very Simple Loop 

We will use the data field wisely to create a very simple menu.
This <code class="code-file">_sidemenu.html</code> will produce side bar menu with no tree.

![A very simple menu without tree][image-jekyll-notree]

Don't worry if it is looks complex, it is just html formatting.

{% highlight html %}
{% raw %}
  <ul class="nav" id="menu">
  {% for link in include.menusource %}
  
    {% if link.url contains 'http' %}
      {% assign domain = '' %}
    {% else %}
      {% assign domain = site.url %}
    {% endif %}
    
    {% if link.url == page.url%}
    <li class="active">{% else %}<li>
    {% endif %}
      <span class="fa fa-list"></span>
      <a class="page-link" href="{{ domain }}{{ link.url }}" 
        {% if link.url contains 'http' %}target="_blank"{% endif %}>
        
        <span class="collapse in">{{ link.title }}</span>
        
        {% if link.url == page.url%}
        <span class="sr-only">(current)</span>
        {% endif %}
      </a>
    </li>
  {% endfor %}
  </ul>
{% endraw %}
{% endhighlight %}

## Adding Tree Level

Since we have three levels, we need to add two more loops.
Each inside another.

{% highlight liquid %}
{% raw %}
  <ul class="nav" id="menu">
  {% for link in include.menusource %}
  ...
    {% if link.submenu %}
    <ul>
       {% for sublink in link.submenu %}
       ...
         {% if sublink.submenu %}
           <ul>
             {% for subsublink in sublink.submenu %}
              ...
             {% endfor %}
           </ul>
         {% endif %}
       ...
       {% endfor %}
    </ul>  
    {% endif %}
  ...
  {% endfor %}
  </ul>
{% endraw %}
{% endhighlight %} 

This will produce a simple tree menu with all node opened.

![A simple tree menu with all node opened][image-jekyll-simpletree]

## Complete Code

This original code is going to be long.

{% highlight html %}
{% raw %}
  <!--ul class="nav nav-sidebar"-->
  <ul class="nav" id="menu">
  {% for link in include.menusource %}
  
    {% if link.url contains 'http' %}
      {% assign domain = '' %}
    {% else %}
      {% assign domain = site.url %}
    {% endif %}
    
    {% if link.url == page.url%}
    <li class="active">{% else %}<li>
    {% endif %}
      <span class="fa fa-list"></span>
      <a class="page-link" href="{{ domain }}{{ link.url }}" 
        {% if link.url contains 'http' %}target="_blank"{% endif %}>
        <span class="collapse in">{{ link.title }}</span>
        {% if link.url == page.url%}
        <span class="sr-only">(current)</span>
        {% endif %}
      </a>
      {% if link.submenu %}
      <ul>
         {% for sublink in link.submenu %}
         
           {% if sublink.url contains 'http' %}
             {% assign domain = '' %}
           {% else %}
             {% assign domain = site.url %}
           {% endif %}
           
           {% if sublink.url == page.url%}
           <li class="active">{% else %}<li>
           {% endif %}
              <span class="fa fa-chevron-right"></span>
              <a class="page-link" href="{{ domain }}{{ sublink.url }}" 
                {% if sublink.url contains 'http' %}target="_blank"{% endif %}>
                <span class="collapse in">{{ sublink.title }}</span>
                {% if sublink.url == page.url%}
                <span class="sr-only">(current)</span>
                {% endif %}
              </a>
              {% if sublink.submenu %}
              <ul>
                 {% for subsublink in sublink.submenu %}
         
                   {% if subsublink.url contains 'http' %}
                     {% assign domain = '' %}
                   {% else %}
                     {% assign domain = site.url %}
                   {% endif %}
           
                   {% if subsublink.url == page.url%}
                   <li class="active">{% else %}<li>
                   {% endif %}
                      <span class="fa fa-asterisk"></span>
                      <a class="page-link" href="{{ domain }}{{ subsublink.url }}" 
                        {% if subsublink.url contains 'http' %}target="_blank"{% endif %}>
                        <span class="collapse in">{{ subsublink.title }}</span>
                        {% if subsublink.url == page.url%}
                        <span class="sr-only">(current)</span>
                        {% endif %}
                      </a>
                   </li>
         
                 {% endfor %}
              </ul>
              {% endif %}
           </li>
         
         {% endfor %}
      </ul>
      {% endif %}
    </li>
  {% endfor %}
  </ul>
{% endraw %}
{% endhighlight %} 

You can see the complexity.
It gets complicated when you mixed logical code (liquid)
and presentation code (html formatting).
We will solve this issue in the next post.

[//]: <> ( -- -- -- links below -- -- -- )

[image-jekyll-notree]: {{ site.url }}/assets/posts/webdev/2016/08/jekyll-sidemenu-notree.png
[image-jekyll-simpletree]: {{ site.url }}/assets/posts/webdev/2016/08/jekyll-sidemenu-simpletree.png
[github-data-navigation]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_data/navigation_wm.yml
[github-css-sidemenu]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_sass/themes/oriclone/_navsidemenu.scss
