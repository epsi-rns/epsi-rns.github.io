epsi-rns.github.io
=====================

A ready to use, Jekyll site customization.

Installing Jekyll is something cool.
Having an example of working site is something else.

![epsi-vexel][image-epsi-vexel]

-- -- --

Github allow a site hosted directly from a GitHub repository 
as announced in [pages.github.com][link-pages-github].
Further than just using github.io,
it allow user to use any domain using CNAME and stuff.

Jekyll transform your plain text into static websites and blogs.
Using [Jekyll][link-jekyll], 
you can blog using beautiful [Markdown][link-markdown] syntax,
and without having to deal with any databases.
Static database can be emulated with simple yaml data/collections. 

Instead of server-side scripting, 
Jekyll utilized [Liquid][link-liquid], a template engine.
A [Front Matter][link-frontmatter] in [YAML][link-yaml] format.
And a built-in support for [Sass/Scss][link-sass]. 
With Scss, CSS can be organized easily.
All you need is a text editor. An ideal environment for beginner.
You can test your markdown with [live preview here][link-markdown-test]

There's a good reading here from [Ramona Harrison][link-ramona]
on how to build a blog using Jekyll.
And there is another review from [Smashing Magazine][link-smash].
Even [Official Manjaro Site][link-manjaro] is using Jekyll.

-- -- --

As a site growing up, the more feature it needs.
Don't worry. Many things can be achieved, 
with only Jekyll built in feature.
Most of them can be found ini [Jekyll Tips][link-jekyll-tips] site.

If you are looking for a ready to use one.
Just look at examples in this repository.
This showcase is more like a list to do for site owner,
But it it is already included this repository.
So there is no need for beginner to reinvent the wheel.

## SASS Vendor

* [Bootstrap][link-bootstrap], 
  CSS Framework. I only need, the responsive layout.

* [Awesome Font][link-fontawesome], 
  The iconic font and CSS toolkit.
  Using font instead of images to reduce http headers.

## Assets

* [jQuery][link-jquery] Javascript Library (temporary).

## Themes

There's a bunch of [Jekyll theme here][link-jekyll-theme].
Starting with Jekyll Theme is inconvenient.

Bootstrap user should start from [Bootstrap Example][link-bootstrap-example].
There are some goodies in [startbootstrap.com][link-startbootstrap].


I started from simple html and build my own
using [Bootstrap Tutorial][link-bootstrap-tutorial].
I found this layout [Code Snippet][link-code-snippet],
and create my first layout based on this.







## Example: Site Wide

All code, utilized Liquid.

* _data (yaml): navigation. 
  For flexible navigation menu.

* _data (yaml): author. 
  For multi author blog.

* _data (yaml): site owner. 
  Usually in footer, containing site owner's social media account.

* Social share. Share your post to social media.
  Based on [So Simple Code here][link-social-share].

* [Open Graph][link-opengraph] Protocol to enhance SEO.
  Based on [So Simple Code here][link-open-graph].

## Example: Pages and Layout

All code, utilized Liquid.

* Tags, my custom liquid.

* Pagination, using [Timble][link-timble-story]'s
  [Repository Here][link-timble-pagination]

* Search, using [lunr.js][link-lunrjs] client-side search. 
  Based on [Jekyll Tips Search][link-jekyll-tips-search].

* Sitemap, [David Ensinger][link-sitemap-david]'s sitemap.xml.
  I found the link from [Jekyll Tips SEO][link-jekyll-tips-seo].
  
## Example: External Service

* [Yandex Metrica][link-yandex], 
  A free tool for evaluating site traffic and analyzing user behavior.

* [Google Analytic][link-google-analytic], 
  a freemium web analytics service offered by Google that tracks and reports website traffic.

* <del>[Google AdSense][link-google-adsense],</del>
  media advertisements, that are targeted to site content and audience.

* [Disqus Comment][link-disqus], 
  a blog comment hosting service for web sites and online communities.

* [Muut Comment][link-muut], 
  The complete discussion system for your site.

  

## Source

* GitHub page with [README.md][link-readme-md]

-- -- --

## Not Finished yet

There are more to come.
just give me time to do it.


![kitten][image-kitten]




[image-epsi-vexel]: http://epsi-rns.github.io/assets/site/images/authors/epsi-vexel.png
[image-kitten]: http://epsi-rns.github.io/assets/site/images/kitten.jpg


[link-jekyll-tips]: http://jekyll.tips/
[link-manjaro]: https://github.com/manjaro/manjaro.github.io?files=1
[link-smash]: https://www.smashingmagazine.com/2014/08/build-blog-jekyll-github-pages/
[link-bootstrap-tutorial]: http://www.tutorialspoint.com/bootstrap/
[link-bootstrap-example]: http://getbootstrap.com/getting-started/#examples
[link-startbootstrap]: http://startbootstrap.com/template-categories/all/ 

[link-pages-github]: https://pages.github.com/
[link-jekyll]: https://jekyllrb.com/
[link-markdown]: https://daringfireball.net/projects/markdown/
[link-markdown-test]: http://markdownlivepreview.com/
[link-liquid]: https://github.com/Shopify/liquid/wiki
[link-frontmatter]: https://jekyllrb.com/docs/frontmatter/
[link-yaml]: http://yaml.org/
[link-sass]: http://sass-lang.com/

[link-jekyll-theme]: https://github.com/jekyll/jekyll/wiki/Themes
[link-code-snippet]: http://bootsnipp.com/snippets/featured/complete-blog-layout

[link-bootstrap]: http://getbootstrap.com/
[link-fontawesome]: http://fontawesome.io/
[link-jquery]: https://jquery.com/

[link-yandex]: https://metrica.yandex.com/
[link-google-analytic]: https://analytics.google.com
[link-google-adsense]: https://analytics.google.com

[link-disqus]: https://disqus.com/
[link-muut]: https://muut.com/

[link-opengraph]: http://ogp.me/
[link-social-share]: https://github.com/mmistakes/so-simple-theme/blob/master/_includes/social-share.html
[link-open-graph]: https://github.com/mmistakes/so-simple-theme/blob/master/_includes/open-graph.html
[link-timble-story]: https://www.timble.net/blog/2015/05/better-pagination-for-jekyll/
[link-timble-pagination]: https://github.com/timble/jekyll-pagination/blob/master/template/pagination.html

[link-lunrjs]: http://lunrjs.com/
[link-jekyll-tips-search]: http://jekyll.tips/tutorials/search/
[link-jekyll-tips-seo]: http://jekyll.tips/tutorials/seo/
[link-sitemap-david]: http://davidensinger.com/2013/11/building-a-better-sitemap-xml-with-jekyll/

[link-ramona]: http://ramonaharrison.github.io/jekyll/pixyll/technical/git/github/2015/03/09/how-i-built-my-blog/


[link-readme-md]: https://github.com/epsi-rns/epsi-rns.github.io/blob/master/README.md
