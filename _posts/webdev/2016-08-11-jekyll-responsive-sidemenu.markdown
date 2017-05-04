---
layout: post-sidemenu-jekyll
title:  "Jekyll Responsive Side Menu"
categories: webdev
date:   2016-08-11 03:15:15 +0700
tags: [responsive, css]
author: epsi

excerpt:
  One of the challenge in maintaining blogsite
  is creating design template for each use case.
  for my tutorial template,
  I need a left sidebar menu using bootstrap framework
  with a button toggle to display or hide this sidebar menu.

related_link_ids: 
  - 16081315  # Side Menu Without Javascript
  - 16081215  # Side Menu Simple Tree  
  - 16061225  # Elapsed Time
  - 16061409  # Webcoder Begin
  - 16061051  # Inkscape Stripes
  - 16052948  # Jekyll Related Posts

---

One of the challenge in maintaining blogsite
is creating design template for each use case.
I'm planning to have a blog template, tutorial template,
maybe screenshot template, and perhaps design gallery.
All of them should be responsive using a chosen framework.

-- -- --

## Goal

> A Jekyll left sidebar menu using bootstrap framework

In this case, In short, I need: a button toggle to display or hide sidemenu.

![Jekyll Bootstrap Opened Side Menu Button][image-sidemenu-button-open]{: .img-responsive }

-- -- --

## Requirement

Let's see the show case in figure below:

*	Side menu always visible in medium screen. No button visible.

*	Side menu is not visible in small screen.

*	Side menu in small screen can be activated using toggle button.

[![Jekyll Bootstrap Opened Side Menu Button][image-sidemenu-show-cases]{: .img-responsive }][photo-sidemenu-show-cases]

### Button

There are many ways to display button.
I like using FontAwesome character as button.

*	<http://fontawesome.io/icons/>

{% highlight html %}
{% raw %}
<a href="#" id="menu-toggle"><i class="fa fa-navicon fa-2x"></i></a>
{% endraw %}
{% endhighlight %}

I'm using <code>visible-xs-*</code>.
It means the button only appear in extra small screen.
This button always appear in small screen..

I also use <code>col-xs-12</code>.
It will take all the grid space.

{% highlight html %}
{% raw %}
  <div class="col-xs-12 visible-xs-block">
      &nbsp;&nbsp;
      <a href="#" id="menu-toggle">
        <i class="fa fa-navicon fa-2x"></i>
      </a>
  </div>
{% endraw %}  
{% endhighlight %}

### Menu

This is the menu part with <code>id="sidebar"</code>
that can be displayed or hidden using javascript.

This menu part is visible in small, medium and larger screen,
but hidden in extra small screen using <code>hidden-xs-*</code>.

{% highlight html %}
{% raw %}
  <div class="col-xs-11 col-sm-4 col-md-3 hidden-xs" id="sidebar">
  {% include layout/sidemenu.html 
    menusource = site.data.navigation_wm
  %}
  </div>
{% endraw %}
{% endhighlight %}

### JQuery Javascript

{% highlight javascript %}
  $('#menu-toggle').click(function() {
      $('#sidebar').toggleClass('hidden-xs');  
  });
{% endhighlight %}

When <code>#menu-toggle</code> clicked,
<code>#sidebar</code> will lose its <code>hidden-xs</code> class,
and become visible.

### Content

Bootstrap is using 12 based grid.
So I arrange the width for each screen device as below

*	Medium: 3 Left + 9 Right = 12 Total

*	Small: 4 Left + 8 Right = 12 Total

*	Extra Small: 11 Left + 12 Right = Become Top and Bottom

{% highlight html %}
{% raw %}
  <!-- sidebar left col -->
  <div class="col-xs-11 col-sm-4 col-md-3 hidden-xs" id="sidebar">
  {% include layout/sidemenu.html 
    menusource = site.data.navigation_wm
  %}
  </div>
  <!-- /sidebar -->
  
  <!-- main right col -->
  <div class="col-xs-12 col-sm-8 col-md-9" id="main"> 
    {{ content }}
  </div>
  <!-- /main -->

{% endraw %}
{% endhighlight %}

### CSS

Boostrap is more than enough.
No custom css trick required.


### Complete Code

See in github

*	[github.com/../page-sidemenu.html][github-layout-sidemenu]


That's all. 

[//]: <> ( -- -- -- links below -- -- -- )

[image-sidemenu-button-open]: {{ site.url }}/assets/posts/webdev/2016/08/jekyll-sidemenu-button-open.png
[image-sidemenu-show-cases]:  {{ site.url }}/assets/posts/webdev/2016/08/jekyll-sidemenu-show-cases.png
[photo-sidemenu-show-cases]:  https://photos.google.com/album/AF1QipOI-OvBHZtRX5saQhwM3h7JWm32xboQ5aCs5fLr/photo/AF1QipPk6C8yKIpHBQhhAruW6ngiQA2_8386wk7B7iZb


[github-layout-sidemenu]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/_layouts/page-sidemenu.html
