---
layout: post
title:  "Jekyll Responsive Side Menu"
categories: webdev
date:   2016-08-12 03:15:15 +0700
tags: [responsive, bootstrap]
author: epsi

excerpt:
  One of the challenge in maintaining blogsite
  is creating design template for each use case.
  for my tutorial template,
  I need aleft sidebar menu using bootstrap framework
  with a button toggle to display or hide this sidebar menu.

related_link_ids: 
  - 16081315  # Side Menu Without Javascript
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

[![Jekyll Bootstrap Opened Side Menu Button][image-sidemenu-show-cases]{: .img-responsive }][picasa-sidemenu-show-cases]

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


That's all. 

[//]: <> ( -- -- -- links below -- -- -- )

[image-sidemenu-button-open]: {{ site.url }}/assets/posts/webdev/2016/08/jekyll-sidemenu-button-open.png
[image-sidemenu-show-cases]: {{ site.url }}/assets/posts/webdev/2016/08/jekyll-sidemenu-show-cases.png
[picasa-sidemenu-show-cases]: https://lh3.googleusercontent.com/V9aa1svROLbwI2CMlGHxu8Tot7U0yfjDO1LKyzdKAmoBWY-v4rAIzRCR5SRxkofNzH5gjBcgLFLPJBlZNtRGF7OxCDZMDhX7G3P3MgAE5vAIE8SZ3tU5C0Hh8WAmk6HZgnzArUKfYQlU2PdjIiTmT8-BQN0dlPKQ8qsOM-S1SK7unVPxg86EMUCtvxJuFw4XEoFdAYlE9R6qWAL0BNnYkUK4WUf2XxNxRDo1jJGcRkHr3s7PMyA6LV6S-ZYxGi9Y3XWxP_sQnkgAzGJ9jXriU4JVx-dUtGkt7hP4jEjRN_f_x7fAXbAy5tyWQpa-iAPFJwAIjv9jyUPj9OJGc90ry9r7F5lA_gjs8ABKqnKzgrMsFHZ2auirovpPkSUL7tQVR_n2qPK8677QcCcyhgyoJFcWooN9yQH7wsIvtKvVf0bzlBpoPfeXUsDtYDdI87ffaAz7Rz-Vewnzgoaci7cqmG5jG3IYeG7166Jy0nAmFqiOqUkNPWomcJl4pJWGodzsQcH8CHPlH5iBf6voVTdyorAP5b9k8Za1g7X_T_B7n4BcOM0Rj3QI_PnmRBEmxLIPsRNSXAwcolZsaRJ3ldXa7dWW0Uhs_3w=s0
