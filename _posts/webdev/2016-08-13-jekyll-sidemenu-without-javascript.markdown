---
# layout: post-sidemenu-jekyll
layout: post
title:  "Jekyll Collapsed Sidemenu Without Javascript"
categories: webdev
date:   2016-08-13 01:15:15 +0700
tags: [jekyll, liquid]
author: epsi

excerpt:
  My tutorial template require a sidebar menu with tree capability.
  Using Liquid template we will have all tree node collapsed,
  except for the current url, th tree should be opened.

related_link_ids: 
  - 16081215  # Side Menu Simple Tree
  - 16081115  # Responsive Side Menu
  - 16061225  # Elapsed Time
  - 16061409  # Webcoder Begin
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---

My tutorial template require a sidebar menu with tree capability.
There are many types of sidebar menus with tree capability

*	Show all tree node opened. As explained in previous post.

*	Show all tree node collapsed, except current url opened. Using javascript.

*	Show all tree node collapsed, except current url opened. Using serverside, or static page.

Using Liquid template we will have all tree node collapsed,
except for the current url, the tree node should be opened.


**Sigh**. I don't know how to say it in english. So let me explain with this figure.

![Jekyll Side Menu Tree Collapsed][image-jekyll-collapsed-sidemenu]{: .img-responsive }


## Caveat

One big issue with Liquid template in Jekyll static site is,
it become slower as things become more complex.
Of course liquid is just a template engine.
It is not a real language. It has limitation.
So basically you cannot create complex things
without sacrificing performance.

Writing server side code (e.g. PHP) for this is easy,
but writing in Liquid require a hack.
Especially when it comes to array.

I'm going to use a complex liquid code to achieve this,
so I warn you, this going to slow down your static page generation
for about 1 or 3 seconds, depends on your machine.

## Navigation Data

It has been said a thousand times that Jekyll doesn't support database connection.
But we have YAML data as a workaround. 
And actually I find that reading yaml is easier when it comes to tree,
compared with tweaking RDBMS.

Let's see our sample <code class="code-file">_data/navigation_wm.yml</code>
to hold my Window Manager (WM) menu.

{% highlight yaml %}
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

## Transform Data

We need to transform each data by comparing to current <code>page.url</code>.

### Original

*	0: title

*	1: url

*	2: submenu


### Transformed

*	0: title

*	1: url: add <code>site.url</code> if local

*	2: blank: set true if target url is not local, otherwise false.

*	3: current: set true if target url is the <code>page.url</code>, otherwise false.

*	4: active: set true if active tree, based on current, also set parent node, the default is false.

*	5: submenu

## Challenge

Officialy there is no array constructor in liquid.
So we have to use this <code>split</code> workaround.

{% highlight liquid %}
{% raw %}
  {% assign menu = "" | split: ""  %}
{% endraw %}
{% endhighlight %}

Changing array value is also not allowed in Liquid.
Array value can only set once using <code>push</code> filter.

{% highlight liquid %}
{% raw %}
  {% assign menu = "" | split: ""  %}

  {% for link1 in include.menusource %}
    {% assign item1 = "" | split: ""  %}
     % assign item1 = item1 | push: link1.title  %}    
    {% assign menu = menu | push: item1 %}
  {% endfor %}  
{% endraw %}
{% endhighlight %}

There is no <code>not</code> operator in liquid
so you have to do comparison operator twice.

{% highlight liquid %}
{% raw %}
  {% for link1 in include.menusource %}
    {% unless link1.url contains 'http://' %}
      {% unless link1.url contains 'https://' %}
        ...
      {% endunless %}
    {% endunless %}
  {% endfor %}  
{% endraw %}
{% endhighlight %}

## Simple Loop

With this limitation, unfortunately,
the only option is to rebuild the array.

{% highlight liquid %}
{% raw %}
  {% assign menu = "" | split: ""  %}

  {% for link1 in include.menusource %}
    {% assign item1 = "" | split: ""  %}
     
    {% assign item1 = item1 | push: link1.title  %}
  
    {% assign url1 = link1.url %}
    {% assign blank1 = true %}
    {% unless link1.url contains 'http://' %}
      {% unless link1.url contains 'https://' %}
        {% assign url1 = site.url  | append: link1.url %}
        {% assign blank1 = false %}
      {% endunless %}
    {% endunless %}
    {% assign item1 = item1 | push: url1  %}
    {% assign item1 = item1 | push: blank1  %}
    
    {% assign menu = menu | push: item1 %}
  {% endfor %}  
{% endraw %}
{% endhighlight %}

## Adding Level

After a few trial and error, the complete logic had been achieved as below.

{% highlight liquid %}
{% raw %}
{% capture cache %}

  {% comment %}
  Rebuilt Array
    0: title
    1: url
    2: blank
    3: current
    4: active
    5: submenu
  {% endcomment %}

  {% assign menu = "" | split: ""  %}

  {% for link1 in include.menusource %}
    {% assign i1 = forloop.index0 %}
    {% assign item1 = "" | split: ""  %}
     
    {% assign item1 = item1 | push: link1.title  %}
  
    {% assign url1 = link1.url %}
    {% assign blank1 = true %}
    {% unless link1.url contains 'http://' %}
      {% unless link1.url contains 'https://' %}
        {% assign url1 = site.url  | append: link1.url %}
        {% assign blank1 = false %}
      {% endunless %}
    {% endunless %}
    {% assign item1 = item1 | push: url1  %}
    {% assign item1 = item1 | push: blank1  %}
  
    {% assign found_active1 = false %}
    
    {% if(link1.url == page.url) %}
      {% assign current1 = true %}
      {% assign found_active1 = true %}
    {% else %}
      {% assign current1 = false %}
    {% endif %}
  
    {% assign item1 = item1 | push: current1 %}

    {% assign sub1 = "" | split: ""  %}    
    {% if link1.submenu %} 
    
      {% for link2 in link1.submenu %}
        {% assign i2 = forloop.index0 %}
        {% assign item2 = "" | split: ""  %}    
      
        {% assign item2 = item2 | push: link2.title  %}
  
        {% assign url2 = link2.url %}
        {% assign blank2 = true %}
        {% unless link2.url contains 'http://' %}
          {% unless link2.url contains 'https://' %}
            {% assign url2 = site.url  | append: link2.url %}
            {% assign blank2 = false %}
          {% endunless %}
        {% endunless %}
        {% assign item2 = item2 | push: url2  %}
        {% assign item2 = item2 | push: blank2  %}
      
        {% assign found_active2 = false %}     
  
        {% if(link2.url == page.url) %}
          {% assign current2 = true %}
          {% assign found_active2 = true %} 
          {% assign found_active1 = true %}
        {% else %}
          {% assign current2 = false %}
        {% endif %}
  
        {% assign item2 = item2 | push: current2 %}

        {% assign sub2 = "" | split: ""  %}    
        {% if link2.submenu %}
    
          {% for link3 in link2.submenu %}
            {% assign i3 = forloop.index0 %}
            {% assign item3 = "" | split: ""  %}    
          
            {% assign item3 = item3 | push: link3.title  %}
  
            {% assign url3 = link3.url %}
            {% assign blank3 = true %}
            {% unless link3.url contains 'http://' %}
              {% unless link3.url contains 'https://' %}
                {% assign url3 = site.url  | append: link3.url %}
                {% assign blank3 = false %}
              {% endunless %}
            {% endunless %}
            {% assign item3 = item3 | push: url3  %}
            {% assign item3 = item3 | push: blank3  %}
  
            {% if(link3.url == page.url) %}
              {% assign current3 = true %}
              {% assign found_active2 = true %}
              {% assign found_active1 = true %}
            {% else %}
              {% assign current3 = false %}
            {% endif %}
            {% assign item3 = item3 | push: current3 %}
          
            {% assign found_active3 = current3 %}
            {% assign item3 = item3 | push: found_active3 %}
          
            {% assign sub2 = sub2 | push: item3 %}
          {% endfor %}  
        {% endif %}
          
        {% assign item2 = item2 | push: found_active2 %} 
        {% assign found_active2 = false %}   
    
        {% assign item2 = item2 | push: sub2 %}
        
        {% assign sub1 = sub1 | push: item2 %}

      {% endfor %}  
    {% endif %}
      
    {% assign item1 = item1 | push: found_active1 %} 
    {% assign found_active1 = false %}       
    
    {% assign item1 = item1 | push: sub1 %}

    {% assign menu = menu | push: item1 %}
  {% endfor %}
{% endcapture %}
{% endraw %}
{% endhighlight %}

whoaaaa... It is complicated.

## Presentation using HTML

Now we can use the array above as feed to these loops below.

{% highlight html %}
{% raw %}
  <!--ul class="nav nav-sidebar"-->
  <ul class="nav" id="menu">
  {% for item1 in menu %}      
           
    {% if item1[3] %}
    <li class="active">{% else %}<li>
    {% endif %}
      <span class="fa fa-list"></span>
      <a class="page-link" href="{{ item1[1] }}" 
        {% if item1[2] %}target="_blank"{% endif %}>
        <span class="collapse in">{{ item1[0] }}</span>
        {% if item1[3] %}
        <span class="sr-only">(current)</span>
        {% endif %}
      </a>
      {% if item1[5] %}
      {% if item1[4] %}
      <ul>
         {% for item2 in item1[5] %}
                         
           {% if item2[3] %}
           <li class="active">{% else %}<li>
           {% endif %}
              <span class="fa fa-chevron-right"></span>
              <a class="page-link" href="{{ item2[1] }}" 
                {% if item2[2] %}target="_blank"{% endif %}>
                <span class="collapse in">{{ item2[0] }}</span>
                {% if item2[3] %}
                <span class="sr-only">(current)</span>
                {% endif %}
              </a>
              {% if item2[5] %}
              {% if item2[4] %}
              <ul>
                 {% for item3 in item2[5] %}
           
                   {% if item3[3] %}
                   <li class="active">{% else %}<li>
                   {% endif %}
                      <span class="fa fa-asterisk"></span>
                      <a class="page-link" href="{{ item3[1] }}" 
                        {% if item3[2] %}target="_blank"{% endif %}>
                        <span class="collapse in">{{ item3[0] }}</span>
                        {% if item3[3] %}
                        <span class="sr-only">(current)</span>
                        {% endif %}
                      </a>
                   </li>
         
                 {% endfor %}
              </ul>
              {% endif %}
              {% endif %}
           </li>
         
         {% endfor %}
      </ul>
      {% endif %}
      {% endif %}
    </li>
  {% endfor %}
  </ul>
{% endraw %}
{% endhighlight %}

The number inside the item[number] representing the field.
It is [0: title, 2: url, 3: blank, 4: current, 5: active, 6: subnode].

## The Source Code

See a complete source in github

*	[github.com/../sidemenu.html][github-layout-sidemenu]


I know. I know. 
There's nothing fancy with this code.
But it is just works.

That's all.


[//]: <> ( -- -- -- links below -- -- -- )

[image-jekyll-collapsed-sidemenu]: {{ site.url }}/assets/posts/webdev/2016/08/jekyll-collapsed-sidemenu.png

[github-data-navigation]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_data/navigation_wm.yml
[github-layout-sidemenu]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_includes/layout/sidemenu.html
