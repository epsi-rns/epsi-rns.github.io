---
layout: post-sidemenu-jekyll
title:  "Jekyll Directory Structure"
categories: webdev
date:   2016-12-04 11:52:15 +0700
tags: [jekyll, liquid]
author: epsi

excerpt:
  This section shows how flexible Jekyll Directory is.
  If you came from framework. You are going to love it.
  Basically it is just folders and files management, inside Jekyll.
 
# related_link_ids: 
 
---

## Preface

Let's get organized, there's a place for everything,
and everything should be in their respectable place.

This article cover, folders and files management, inside Jekyll.
I created this pattern based on my own experience.
Different than basic example provide by Jekyll.

So you can have a tidy post archives,
for your growing blog, without too much thinking.
At least you won't get lost
with your own data, article, image and code.

### Nomore Dead Links Sir !

Archiving posts could be boring
because its nature of repetitive task.
This long article is also boring.
You have been warned.

But, it is important step
that you should consider
from very beginning of your Blog.

Mostly when you share your blog.
Once it has been shared,
you can't just change your URL,
or it would be a dead link.

### Reference

Jekyll Directory is very flexible.
If you came from framework, then you are going to love it.
Before we dive into deeper directory,
You might consider to read this official guidance.

* <https://jekyllrb.com/docs/structure/>

And see all real world example for this Blog.

* <https://github.com/epsi-rns/epsi-rns.github.io/>

-- -- --

## Main Directory

Here we will see transformation from root directory to 
static page directory <code class="code-file">_site</code>.

### Jekyll Perspective

From the Jekyll perspective.
It has these standard directories:
<code class="code-file">_data</code>,
<code class="code-file">_includes</code>,
<code class="code-file">_layouts</code>,
<code class="code-file">_posts</code>,
<code class="code-file">_sass</code>,
<code class="code-file">_site</code>.

{% highlight bash %}
├── index.html
│
├── _config.yml
├── _data
├── _includes
├── _layouts
├── _posts
├── _sass
│
└── _site
{% endhighlight %}  

Every text files in main directory,
except <code class="code-file">_site</code> 
will be pre-processed into static site 
into <code class="code-file">_site</code> directory.

For example a page <code class="code-file">index.html</code>.
Will be transformed into a static page <code class="code-file">_site/index.html</code>.
That can be called in web browser as <code class="code-file">http://localhost:4000/index.html</code>
without further overhead processing.

Anything started with <code class="code-file">_</code>
are considered Jekyll's internal.
So it won't be shown in <code class="code-file">_site</code>.

For example a page <code class="code-file">_config.yml</code>,
or <code class="code-file">_config_dev.yml</code>,
won't be in your live site.


### Developer Perspective

From the Developer Perspective,
I need to place my own stuff following my own flavour.
Let's get organized, there's a place for everything,
and everything should be in their respectable place.

*	Be tidy.
	Minimize files in root directory.
	Don't let your directory be bloated.

*	Refactor.
	Keep your directory clean.
	No directory contain too many files.

{% highlight bash %}
├── index.html
│
├── assets
└── pages
{% endhighlight %} 

The <code class="code-file">assets</code> directory
contains images, stylesheet, font and javascript, all at once.
Meanwhle the <code class="code-file">pages</code> directory
contains any static links. 

For example a page <code class="code-file">pages/about.md</code>.
Will produce a static page <code class="code-file">_site/pages/about/index.html</code>.
That can be called in web browser as <code class="code-file">http://localhost:4000/pages/about/</code>.

Since I place my Jekyll files in my Github repository, 
I put <code class="code-file">README.md</code>
beside <code class="code-file">index.html</code>.

I also put common web files in root directory.
So this is all.

{% highlight bash %}
.
├── favicon.ico
├── feed.xml
├── index.html
├── README.md
└── sitemap.xml
{% endhighlight %} 

![Jekyll Directory: Root][image-ss-root-root]{: .img-responsive }

### Permalinks

The official guidance is here
* <https://jekyllrb.com/docs/permalinks/>

The <code class="code-file">_posts</code>
directory contain all your posts.
For each post, you can set a category, in frontmatter.

For example a post 
<code class="code-file">2014-03-26-wireless-command-line.markdown</code>
that contain below formatter.

{% highlight yaml %}
date:   2014-03-13 18:32:15 +0700
categories: system
{% endhighlight %}

Will produce a static page
<code class="code-file">_site/system/2014/03/13/wireless-command-line.html</code>.
That can be called in web browser as 
<code class="code-file">http://localhost:4000/system/2014/03/13/wireless-command-line.html</code>.

Each blogger has different categories, mine are
<code class="code-file">design</code>,
<code class="code-file">desktop</code>,
<code class="code-file">opensource</code>,
<code class="code-file">system</code>,
<code class="code-file">webdev</code>.

Now let's see what's happened in <code class="code-file">_site</code>.

![Jekyll Directory: _site][image-ss-site-root]{: .img-responsive }

-- -- --

## pages Directory

What's in here? 
Static page of course.
About, Searching, Blog Archive, Index.
Just use your imagination.

![Jekyll Directory: pages][image-ss-root-pages]{: .img-responsive }

The transformation on the <code class="code-file">_site</code>
should be clear one to one, exactly the same as the original one.
Except that I'm using <code>jekyll-paginate</code> that transform
links in <code class="code-file">pages/index.html</code>
into some <code class="code-file">_site/pages/blog-n.html</code>

![Jekyll Directory: _site/pages][image-ss-site-pages]{: .img-responsive }

-- -- --

## _posts Directory

You can put all your posts in one single
<code class="code-file">_posts</code> directory.
But as soon as your blog growing to 200 posts,
it is going to be hard to find a particular post,
when they all mixed in only one place.

You need to make folder to group some posts together.
Jekyll will loop over all <code class="code-file">_posts</code>
directories to find all your post.
So make yourself at home,
rearrange your furniture as you wish.

I choose to have those folders
by categories followed by year.

{% highlight bash %}
. _posts/
├── alumni
├── design
│   ├── 2015
│   └── 2016
├── desktop
│   ├── 2014
│   ├── 2015
│   └── 2016
│       ├── 2016-03-26-xmonad-config.markdown
│       ├── 2016-07-06-awesome-structure.markdown
│       ├── 2016-07-13-awesome-preparing.markdown
│       └── 2016-08-01-i3status-conky-lua-json.markdown
├── journal
├── opensource
│   ├── 2014
│   ├── 2015
│   └── 2016
├── system
│   ├── 2014
│   ├── 2015
│   └── 2016
└── webdev
    ├── 2016-05-14-jekyll-blog-overview.markdown
    ├── 2016-06-22-jekyll-arch-install.markdown
    └── 2016-06-23-jekyll-debian-install.markdown
{% endhighlight %}

And it would be transformed just like this.

{% highlight bash %}
. _site/desktop/2016
├── 03
│   └── 26
│       └── xmonad-config.html
├── 07
│   ├── 06
│   │   └── awesome-structure.html
│   └── 13
│       └── awesome-preparing.html
└── 08
    └── 01
        └── i3status-conky-lua-json.html
{% endhighlight %}

And instantly, those can be called in your favorite web browser.

* <code class="code-file">http://localhost:4000/desktop/2016/03/26/xmonad-config.html</code>.

* <code class="code-file">http://localhost:4000/desktop/2016/07/06/awesome-structure.html</code>.

* <code class="code-file">http://localhost:4000/desktop/2016/07/13/awesome-preparing.html</code>.

* <code class="code-file">http://localhost:4000/desktop/2016/08/01/i3status-conky-lua-json.html</code>.

## assets Directory

### Overview.

This is completely yours to arrange.

{% highlight bash %}
. assets
├── posts
│   ├── alumni
│   ├── design
│   ├── desktop
│   ├── journal
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

### assets/posts

Respectively I also create an posts directory inside assets directory.
This directory contain static images related to each post.

{% highlight bash %}
. assets/posts
├── alumni
├── design
├── desktop
│   ├── 2014
│   │   ├── 04
│   │   ├── 05
│   │   ├── 10
│   │   ├── 11
│   │   └── 12
│   ├── 2015
│   │   └── 02
│   └── 2016
│       ├── 03
│       │   ├── modularized-xmonad-1.png
│       │   └── modularized-xmonad-2.png
│       ├── 07
│       │   ├── awesome-modularized-code-binding.png
│       │   ├── awesome-modularized-code-main.png
│       │   ├── awesome-modularized-stacked.png
│       │   ├── awesome-refactoring-manjaro.png
│       │   └── awesome-refactoring-syawal.png
│       └── 08
│           ├── i3-conky-1-old.png
│           ├── i3-conky-2-lua.png
│           ├── i3-conky-3-json.png
│           ├── i3-conky-lua-json-bottom.png
│           └── i3-conky-lua-json-top.png
├── journal
├── opensource
├── system
└── webdev
{% endhighlight %}

And later, you attach image from any of your markdown post.

{% highlight liquid %}
{% raw %}
![Modularized Awesome WM Code Main][image-ss-awesome-main]{: .img-responsive }
![Modularized Awesome WM Code Module][image-ss-awesome-module]{: .img-responsive }

[image-ss-awesome-main]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-modularized-code-main.png
[image-ss-awesome-module]: {{ site.url }}/assets/posts/desktop/2016/07/awesome-modularized-code-binding.png
{% endraw %}
{% endhighlight %}

## _layout Directory

There is not much to talk about layout unless you are a hardcore site designer.
This directory is containing your most html5 code.

{% highlight bash %}
. _layout
├── default.html
├── gen-sidemenu.html
├── page.html
├── page-sidemenu-jekyll.html
├── page-sidemenu-wm.html
├── post.html
├── post-sidemenu-jekyll.html
└── post-sidemenu-wm.html
{% endhighlight %}

## _include Directory

This one is not too boring as _layout directory, and it deserve its own article.
But for now, I give you brief explanation.
This directory is containing your most liquid code as part of your layout.

The idea is basically putting your own code,
just to be not too crowded in one single directory.

![Jekyll Directory: _include][image-ss-root-include]{: .img-responsive }

And here is the details.

{% highlight bash %}
. _include
├── layout
│   ├── footer.html
│   ├── header.html
│   ├── head.html
│   └── sidemenu.html
├── page
│   └── sidebar.html
├── panel
│   ├── archives.html
│   ├── category.html
│   ├── recent-posts.html
│   ├── related-posts.html
│   └── tags.html
├── part
│   └── screenshot.html
├── post
│   ├── footer.html
│   ├── header.html
│   ├── pagination.html
│   ├── sidebar.html
│   └── time-elapsed.html
├── service
│   ├── disqus-comments.html
│   ├── google-adsense.html
│   ├── google-analytics.html
│   ├── muut-comments.html
│   └── yandex-metrica.html
├── opengraph.html
├── pagination-better.html
└── pagination.html

{% endhighlight %}

## _sass Directory

This one is interesting, and it also deserve its own article.
But for now, all I can do is giving you brief explanation.

This SASS directory contain your raw stylesheet
before processed into CSS, called in 
<code class="code-file">assets/themes</code> directory.
This CSS Technology has become so complex,
that it requires its own folders management.

Hypotethically, you can create your own switchable theme, with Jekyll.
Although I have never seen anyone reach that far.

![Jekyll Directory: _sass][image-ss-root-sass]{: .img-responsive }

-- -- --

Happy Jekylling !

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/webdev/2016/12' %}

[image-ss-root-root]:    {{ asset_path }}/jekyll-dir-01-root-root.png
[image-ss-site-root]:    {{ asset_path }}/jekyll-dir-01-site-root.png

[image-ss-root-pages]:   {{ asset_path }}/jekyll-dir-02-root-pages.png
[image-ss-site-pages]:   {{ asset_path }}/jekyll-dir-02-site-pages.png

[image-ss-root-include]: {{ asset_path }}/jekyll-dir-03-root-include.png
[image-ss-root-sass]:    {{ asset_path }}/jekyll-dir-04-root-sass.png
