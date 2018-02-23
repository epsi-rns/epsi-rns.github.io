---
# layout: post-sidemenu-jekyll
layout: post
title:  "Jekyll - Step by Step"
categories: webdev
date:   2018-02-21 09:17:35 +0700
tags: [jekyll]
author: epsi

excerpt:
  Building jekyll Step by step.
  When you are done with install, this is what to do.

related_link_ids: 
  - 16062329  # Jekyll Install Debian
  - 16062202  # Jekyll Install Arch/Manjaro
  - 18010602  # Jekyll Install openSUSE
  - 16052948  # Jekyll Related Posts

---

### Preface

This article is intended for beginner.

> Explain Jekyll Directory Step by Step

Jekyll is not the only **SSG** (Static Site Generator).
In fact I have migration plan to Hugo.
But I still find that Jekyll is easy for beginner,
in context of its relationship with **github**.
You can just uploaded it to github,
and you can view your **github.io** site,
right away, just like that.

Here, I present a Jekyll Tutorial, step by step, for beginners.

* Preface: Table of Content

* SSG: Static Site Generator

* 1: Jekyll Build/ Serve

* 2: Config: <code>_config.yml</code>

* 3: Layout: <code>index.html</code> dan <code>_layouts/default.html</code>

* 4: Example Page: <code>salam.html</code>

* 5: Includes: <code>_includes/head.html</code>

* 6: Custom directories: untuk <code>pages</code> dan <code>assets</code>

* 7: Post: <code>_posts</code>, <code>_layouts/post.html</code>

* 8: Directory Structure: *Real Life Site*

* Penutup

	Salam mean **greet**, in Indonesia Language

-- -- --

### Static site Generator

In case, that you need a broader view about SSG.
Here is a list:

* [Hugo](https://gohugo.io/) (go)

* [Hexo](https://hexo.io/) (nodejs/npm)

* [Cobalt](https://cobalt-org.github.io/) (rust)

* [Pelican](https://blog.getpelican.com/) (python)

* [Jekyll](https://jekyllrb.com/) (ruby)

* [Sculpin](https://sculpin.io/) (php)

* [Gatsby](https://www.gatsbyjs.org/) (nodejs/npm)

If this list above is not enough, you might consider to have a look at
[staticgen.com]([https://www.staticgen.com/)

-- -- --

### 1: Jekyll Build/ Serve

It only takes a few steps.

#### 1. First

{% highlight bash %}
% jekyll -v
jekyll 3.7.0
{% endhighlight %}

#### 2. Second

{% highlight bash %}
% mkdir ~/githublab/jekyll
{% endhighlight %}

#### 3. Other Step

{% highlight bash %}
% cd ~/githublab/jekyll
{% endhighlight %}

#### 4. An Another

{% highlight bash %}
% jekyll build
Configuration file: none
            Source: /home/epsi/githublab/jekyll
       Destination: /home/epsi/githublab/jekyll/_site
 Incremental build: disabled. Enable with --incremental
      Generating... 
                    done in 0.046 seconds.
 Auto-regeneration: disabled. Use --watch to enable.
{% endhighlight %}

Or you can right away use this command below.

#### 5. And The Last One

{% highlight bash %}
% jekyll serve          
Configuration file: none
            Source: /home/epsi/githublab/jekyll
       Destination: /home/epsi/githublab/jekyll/_site
 Incremental build: disabled. Enable with --incremental
      Generating... 
                    done in 0.017 seconds.
 Auto-regeneration: enabled for '/home/epsi/githublab/jekyll'
    Server address: http://127.0.0.1:4000
{% endhighlight %}

![Jekyll: Start][image-ss-01-start]{: .img-responsive }

#### Notes

The directory <code>_site</code> has been made automatically.

#### Project Tree

{% highlight bash %}
% tree
.
└── _site

1 directory, 0 files
{% endhighlight %}

![Jekyll: Tree][image-ss-01-tree]{: .img-responsive }

#### Preview

It is time to refresh your browser
to <code>http://localhost:4000/</code>

![Jekyll: Browser][image-ss-01-browser]{: .img-responsive }

#### Jekyll New

Some tutorial use an easier way to create a Jekyll Skeleton,
but I do not use it in this tutorial, because I need to explain,
step by step.

{% highlight bash %}
$ jekyll new .
{% endhighlight %}

I simply ignore this command.

-- -- --

### 2: Configuration

Consider make our configuration: <code>_config.yml</code>

Don't let the config intimidate you.
This is mostly just a description <code>YAML</code> format.

#### _config.yml

{% highlight yaml %}
# Welcome to Jekyll!

# Site settings
title: Yet Another Open Source Blog
email: epsi.nurwijayadi@gmail.com
description: > # description below
  Learn and Discover Open Source with Daily Genuine Experience.
  From Coding, Design, Package Management Tweak to Desktop Customization.
baseurl: "" # the subpath of your site, e.g. /blog

# the base hostname & protocol for your site
# url: "http://epsi-rns.github.io" 
url: "http://localhost:4000" 

# Missing timezone in metadata date can lead to wrong date in url
timezone: Asia/Jakarta
{% endhighlight %}

#### Notes

No <code>_site\index.html</code> has been made.
This will be ayutomatically made later.

#### Project Tree

{% highlight bash %}
$ tree
.
├── _config.yml
└── _site

1 directory, 1 file
{% endhighlight %}

![Jekyll: YAML Configuration][image-ss-02-config]{: .img-responsive }

-- -- --

### 3: Layouts

Consider make these three artefacts.

1. Directory: <code>_layouts</code>

2. File: <code>index.html</code>

3. File: <code>_layouts/default.html</code>

#### Project Tree

{% highlight bash %}
$ tree
.
├── _config.yml
├── _layouts
│   └── default.html
├── _site
│   └── index.html
└── index.html

2 directories, 4 files
{% endhighlight %}

#### Content

With content as below:

<code>index.html</code>

{% highlight html %}
---
layout: default
---
    <p>Hola Amigos, Saludos a todos.<br/>
    Buenos Dias/Tardes/Noches.</p>

    <p>Quiero aprender Jekyll.<br/>
    Larga Vida Jekyll!<br/>
    </p>

    <h4>Felicitaciones.</h4>
    <p>Y Perdon por mi espanol.<br/>
    Soy de Indonesia.<br/>
    Estoy usando Jekyll.</p>
{% endhighlight %}

<code>_layouts/default.html</code>

{% highlight html %}
{% raw %}
<!DOCTYPE html>
<html lang="en">

<head>
    <title>{{ site.title | escape }}</title>
</head>

<body>

    <div class="page-content">
      <div class="wrapper">
        {{ content }}
      </div>
    </div>

</body>
</html>
{% endraw %}
{% endhighlight %}

#### Notes

We need to pay attention to these artefacts:

Frontmatter in: <code>index.html</code>

{% highlight html %}
---
layout: default
---
{% endhighlight %}

Liquid snippet in: <code>_layouts/default.html</code>

{% highlight html %}
{% raw %}
      <div class="wrapper">
        {{ content }}
      </div>
{% endraw %}
{% endhighlight %}

#### Preview

It is time to refresh your browser
to <code>http://localhost:4000/</code>

![Jekyll: Plain][image-ss-03-plain]{: .img-responsive }

-- -- --

### 4: Example Page

Instead of using <code>index.html</code>, we need another example.
Consider make <code>salam.html</code>. It means **greet** in Indonesia.
This file will be using <code>default</code> layout.


#### salam.html

{% highlight html %}
---
layout: default
---
    <p>Apa kabar kawan-kawan, Salam semuanya.<br/>
    Selamat Pagi/Siang/Sore.</p>

    <p>Saya sedang belajar Jekyll.<br/>
    Hidup Jekyll!<br/>
    </p>

    <h4>Selamat.</h4>
    <p>Maafkan bahasa Indonesia saya.<br/>
    Saya berasal dari Indonesia.<br/>
    Saya menggunakan Jekyll.</p>
{% endhighlight %}


#### Preview

Now we can happily see the result in <code>http://localhost:4000/salam</code>.
The browser <code>title</code> set in <code>_layouts/default.html</code>.
We can set it per pages later, using frontmatter.

#### Notes

If you want, you can use other format, instead of <code>HTML</code>,
such as <code>markdown</code>.

![Jekyll: Example Page][image-ss-04-greet]{: .img-responsive }

-- -- --

### 5: Includes

Now consider preparing these artefacts:

1. Directory: <code>_includes</code>

2. File: <code>_includes/head.html</code>

3. Icon: <code>favicon.ico</code>

And change these artefacts

4. Liquid File: <code>_layouts/default.html</code>

5. Frontmatter in File: <code>salam.html</code>

#### Project Tree

{% highlight bash %}
$ tree
.
├── _config.yml
├── _includes
│   └── head.html
├── _layouts
│   └── default.html
├── _site
│   ├── favicon.ico
│   ├── index.html
│   └── salam.html
├── favicon.ico
├── index.html
└── salam.html

3 directories, 9 files
{% endhighlight %}

#### _includes/head.html

Again, do not let these code below intimidate you.
This is just an example of <code>liquid</code> template.

{% highlight html %}
{% raw %}
<head>
  <title>{% if page.title %}{{ page.title | escape }}{% else %}{{ site.title | escape }}{% endif %}</title>
  <link href="{{ site.url }}/favicon.ico" rel="shortcut icon" type="image/vnd.microsoft.icon" />
</head>
{% endraw %}
{% endhighlight %}

You should see that any directory or any file,
except with prefix <code>_</code> is rendered in <code>_site</code> directory.

#### Liquid: _layouts/default.html

{% highlight html %}
{% raw %}
<!DOCTYPE html>
<html lang="en">
	
  {% include head.html %}

<body>

    <div class="page-content">
      <div class="wrapper">
        {{ content }}
      </div>
    </div>

</body>
</html>
{% endraw %}
{% endhighlight %}

#### Frontmatter: salam.html

{% highlight html %}
---
layout: default
title:  Salam Pramuka
---
{% endhighlight %}

#### Preview 

Check your browser for these two

* <code>http://localhost:4000/</code>

* <code>http://localhost:4000/salam</code>

Pay attention that both have different <code>title </code>:

* <code>salam.html</code> as set locally in <code>frontmatter</code>

* <code>index.html</code> as set globally in <code>_config.yml</code>

![Jekyll: _includes][image-ss-05-includes]{: .img-responsive }

You should also see the <code>favicon.ico</code>.

-- -- --

### 6: Custom Directories


This part is where we put <code>html pages</code> dan <code>images</code>.
Like said before, any directory or any file,
is rendered in <code>_site</code> directory,
except with prefix <code>_</code>.

Consider make these two directories.

1. <code>pages</code> (for any html pages other than _post)

2. <code>assets</code> (for anything else: images, css, javascript)

#### Project Tree

{% highlight bash %}
$ tree -I _site
.
├── _config.yml
├── _includes
│   └── head.html
├── _layouts
│   └── default.html
├── assets
│   └── logo-orb-spiral.png
├── favicon.ico
├── index.html
└── pages
    └── salam.html

4 directories, 7 files
{% endhighlight %}

#### Assets

Put any picture into <code>assets</code> directory
For example: <code>assets/logo-orb-spiral.png</code>

#### Pages

Move <code>salam.html</code> into <code>pages/salam.html</code>

Dan change a little bit, add picture image in this document.

{% highlight html %}
{% raw %}
---
layout: default
title:  Salam Pramuka
---
    <p>Apa kabar kawan-kawan, Salam semuanya.<br/>
    Selamat Pagi/Siang/Sore.</p>
    
    <a href="{{ site.url }}">
        <img src="/assets/logo-orb-spiral.png" 
             alt="{{ site.title }}" />
    </a>

    <p>Saya sedang belajar Jekyll.<br/>
    Hidup Jekyll!<br/>
    </p>

    <h4>Selamat.</h4>
    <p>Maafkan bahasa Indonesia saya.<br/>
    Saya berasal dari Indonesia.<br/>
    Saya menggunakan Jekyll.</p>
{% endraw %}
{% endhighlight %}

![Jekyll: Custom Directory][image-ss-06-custom]{: .img-responsive }

-- -- --

### 7: Posts

We are almost finished.
Now it is time to make a post.
Consider preparing these artefacts:

1. Directory: <code>_post</code>

2. Example Post: <code>_posts/2018-02-20-using-jekyll.markdown</code>

3. Layout Post: <code>_layouts/post.html</code>

4. Add Post List Code in: <code>index.html</code>

#### Project Tree

{% highlight bash %}
$ tree -I _site
.
├── _config.yml
├── _includes
│   └── head.html
├── _layouts
│   ├── default.html
│   └── post.html
├── _posts
│   └── 2018-02-20-using-jekyll.markdown
├── assets
│   └── logo-orb-spiral.png
├── favicon.ico
├── index.html
└── pages
    └── salam.html

5 directories, 9 files
{% endhighlight %}

#### Frontmatter: _posts/2018-02-20-using-jekyll.markdown

{% highlight html %}
---
layout: post
title:  "Using Jekyll"
date:   2018-02-20 09:17:35 +0700
author: epsi
---

### Preface

My first article.
{% endhighlight %}

#### _layouts/post.html

As a <code>content</code> of <code>default</code> layout.

{% highlight html %}
{% raw %}
---
layout: default
---
  <div class="post-content">
    {{ content }}
  </div>
{% endraw %}  
{% endhighlight %}

#### index.html

{% highlight html %}
{% raw %}
---
layout: default
---
  ...

  {%for post in site.posts %}
    <ul>
      <li>
        <a href="{{ post.url }}">
          {{ post.title }}
        </a>        
      </li>      
    </ul>
  {% endfor %}
{% endraw %}
{% endhighlight %}

#### Note

Post can be either <code>HTML</code> format or <code>markdown</code> format.

#### Preview

Again, check browser for both URL:

* <code>http://localhost:4000/</code> And clik link to open below URL:

* <code>http://localhost:4000/2018/02/20/using-jekyll.html</code> as set in <code>date</code> in frontmatter.

![Jekyll: _post][image-ss-07-post]{: .img-responsive }

-- -- --

### 8: Directory Structure: Real Life Site

In _real life site_, we can expand directory structure.
This is an eaxample from my current blog:

#### 1: pages

{% highlight bash %}
$ tree -d pages
pages
├── _layout_test
├── desktop
└── webdev
{% endhighlight %}

#### 2: assets

{% highlight bash %}
$ tree -d assets -L 2
assets
├── posts
│   ├── code
│   ├── design
│   ├── desktop
│   ├── opensource
│   ├── system
│   └── webdev
├── site
│   ├── images
│   └── js
├── themes
│   ├── default
│   └── oriclone
└── vendors
    ├── bootstrap
    ├── font-awesome
    ├── jquery
    └── lunr
{% endhighlight %}

Details

{% highlight bash %}
$ tree -d assets/posts/code 
assets/posts/code
├── 2016
│   └── 05
├── 2017
│   ├── 04
│   └── 12
└── 2018
    └── 01
{% endhighlight %}

#### 3: _includes

{% highlight bash %}
$ tree -d _includes
_includes
├── layout
├── page
├── panel
├── part
├── post
│   ├── 2017
│   │   ├── 04
│   │   ├── 05
│   │   ├── 06
│   │   └── 08
│   └── 2018
│       └── 01
└── service

13 directories
{% endhighlight %}

![Jekyll: Tree _includes][image-ss-08-includes]{: .img-responsive }

#### 4: _posts

{% highlight bash %}
$ tree -d _posts   
_posts
├── code
│   ├── 2016
│   ├── 2017
│   └── 2018
├── design
│   ├── 2015
│   └── 2016
├── desktop
│   ├── 2014
│   ├── 2015
│   ├── 2016
│   ├── 2017
│   └── 2018
├── opensource
│   ├── 2014
│   ├── 2016
│   └── 2018
├── system
│   ├── 2014
│   ├── 2015
│   ├── 2016
│   ├── 2017
│   └── 2018
└── webdev

24 directories
{% endhighlight %}

![Jekyll: Tree _post][image-ss-08-posts]{: .img-responsive }


Jekyll is very flexible in context of directory management.

-- -- --

### Conclusion

I think this is all for now.

There are other topic though, such as bootstrap (with SaSS) in Jekyll in other article. And a bunch of enhancement. This is already a very long article, so I have to close the guidance. See you with the next tutorial.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2018/02' %}

[image-ss-01-browser]:  {{ asset_path }}/jekyll-01-browser.png
[image-ss-01-tree]:     {{ asset_path }}/jekyll-01-directory.png
[image-ss-01-start]:    {{ asset_path }}/jekyll-01-start.png
[image-ss-02-config]:   {{ asset_path }}/jekyll-02-config.png
[image-ss-03-plain]:    {{ asset_path }}/jekyll-03-plain-html.png
[image-ss-04-greet]:    {{ asset_path }}/jekyll-04-salam.png
[image-ss-05-includes]: {{ asset_path }}/jekyll-05-includes-favicon.png
[image-ss-06-custom]:   {{ asset_path }}/jekyll-06-assets-and-pages.png
[image-ss-07-post]:     {{ asset_path }}/jekyll-07-posts.png
[image-ss-08-includes]: {{ asset_path }}/jekyll-08-tree-includes.png
[image-ss-08-posts]:    {{ asset_path }}/jekyll-08-tree-posts.png
