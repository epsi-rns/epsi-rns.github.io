---
layout: post
title:  "How Do a Webcoder Begin ?"
date:   2016-06-14 01:09:15 +0700
categories: opensource
tags: [thought]
author: epsi

excerpt:
  Between HTML, Javascript, PHP, CSS?
  Always start with Plain HTML, then HTML+CSS,
  You can learn Client Side or Server Side later.
---

Between HTML, Javascript, PHP, CSS?
Where do I start?

This is more like a guidance.
Coder with different experience might have different answer.

-- -- --

## Short Answer:

1. Document: HTML + CSS. HTML First, then CSS.
2. Client Side: Javascript
3. Server Side: PHP, or Ruby or else.
4. HTTP request for serious developer.

Always start with Plain HTML, then HTML+CSS,
exactly in this sequence.

You can learn Client Side or Server Side later.
Beginner should not use server-side or client-side
before they learn HTML.

-- -- --

## HTML

Unless you are a web-designer,
you should learn html by writing your HTML using Text Editor.

My favorite is Geany Text Editor.
But you may have ypur own preferences.

There ar basic things to learn

* Page formatting

* Image handling

* DOM: Document Object Model

* and stuff

* Don't forget the HTML5 standard

-- -- --

## HTML + CSS

HTML and CSS is tightly related.
In my experience, learning CSS is different
than learning programming language.
While you can guess code in script by the logic,
with CSS, you have to try it in browser.
You have to walk through it.

There a some part that you have to learn

* CSS for formatting document, style and typography

* CSS for page layout, and media query for responsive design

* CSS3 effect (shadow, animation, pattern)

* A popular CSS Framework called Bootstrap

* CSS generator, using LESS or SASS/SCSS.

-- -- --

## Client Side: Javascript

Nowadays every coder is using unobtrusive javascript. 
There was a dark age when coder put the javascript into html tag,
and make everything looks complicated.
Now those days are gone, thanks to javascript framework.

Talking about framework, 
you should start with the most popular one. 
It is the JQuery Javascript Toolkit.
It is not really a framework but rather a toolkit,
compared with Mootools.
But it is so popular, compared with mootools.

Nowadays people has moved on to a better framework called AngularJS.
I haven't tried Angular yet. So I don't have any opinion here.

-- -- --

## Server Side: PHP

Let's begin from Native PHP.
Then experiment with CMS (Joomla, Wordpress, Drupal).
And later try PHP Framework, laravel/symfony2 or the light one e.g. code igniter.

When you stepped in server side, you must prepare to learn about database connection.
Native PHP is using mysql/mysqli driver or pdo_mysql.
Framework sometimes utilized ORM, e.g Symfony2 tightly related with Doctrine2 ORM.

-- -- --

## Static Site Generator: Jekyll

Build your own site, You dont always need Server Side.
Now It can be achieved by using static page generator.

Now there is Jekyll static generator,
and Github is officialy using Jekyll.

### Markdown

Instead of using HTML Markup, Jekyll recognize Markdown.
Markdown is a very easy to use document formatting.
Jekyll will translate your markdown to HTML markup.

### Liquid

To enhance the HTML document,
it utilized a template engine called Liquid.

### SASS/SCSS

And generate CSS using SASS/SCSS

-- -- --

## Sample Blog

I built my blog without PHP,
and using JQuery only for Navigation Bar.

> http://epsi-rns.github.io

Source code-nya :

> https://github.com/epsi-rns/epsi-rns.github.io

You can clone with

{% highlight bash %}
$ git clone git://github.com/epsi-rns/epsi-rns.github.io.git
{% endhighlight %}

Currently, I don't put any Ruby script 
in Jekyll while generating my Blog.
I keep my Blog with minimal overhead,
Simple and Stupid.

-- -- --

Have Fun with Learning.


[//]: <> ( -- -- -- links below -- -- -- )

