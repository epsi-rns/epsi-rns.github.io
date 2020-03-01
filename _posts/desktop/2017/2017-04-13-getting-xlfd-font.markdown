---
layout     : post
title      : "Getting XLFD Font"
categories : desktop
date       : 2017-04-13 23:20:15 +0700
tags       : [ricing]
keywords   : [font, xlfd]
author     : epsi
toc        : toc/2017/04/toc-standalone.html

excerpt:
  Font, another knowledge we almost missed in school.

---

### Preface

Font is one of the very foundation of GUI.
And give good impact in ricing.
But when it comes to GUI, I was got lost without a clue.
Just like, there is no sociology class while I was in college,
there's no such things as font class either.

So I decide to make this short guidance,
for unlucky people who never attend in font class
while in college.

-- -- --

### XLFD

I don't know what is XLFD (X Logical Font Description).
I guess it is another standard beside XFT (X Font Server).
Why don't you search in google yourself ?

**Reading**:<br/>

*	<https://en.wikipedia.org/wiki/X_logical_font_description>

*	<https://wiki.archlinux.org/index.php/X_Logical_Font_Description>


Let quote from this wiki.

	"XLFD was originally designed for bitmap fonts
	and support for scalable fonts was added later."

	"XLFDs are still supported in current X window implementations
	for compatibility with legacy software."


Now it is clear, if you want retro looks for your ricing,
this Bitmap Font is a must have knowledge.
Something you need to handle well.

-- -- --

### Where to get Bitmap Font?

As an inkscape user, I use scalable Fonts.
I know that there are many site offered free font.
And mostly I used Google Fonts. All I have to do is to put it 
in <code class="code-file">~/.fonts</code>.
But this doesn't apply in Ricing, as this is not XLFD Font.

Most of popular Bitmap Font, available in github nowadays.
And in my Archbox, I can even search, query , and install it,
via AUR. Your linux distribution might have
different package management than mine in image below.

![XLFD: Install Siji][image-font-01]{: .img-responsive }

-- -- --

### Logical Description in Script

You might found some XLFD notations marching in a script,
especially in dotfiles. And you might also wondering,
how the original author, supposed to get this notation.

{% highlight bash %}
font_siji='-wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1'
font_awesome='-*-fontawesome-medium-*-*-*-17-*-*-*-*-*-*-*'
{% endhighlight %}

You can compose this notation using <code>xfontsel</code>.

{% highlight bash %}
$ xfontsel
{% endhighlight %}

![XLFD: Logical Description][image-font-02]{: .img-responsive }

-- -- --

### Finding Unicode in Script

The next issue, is how to write each character in script.
Some character can just can be copy-paste to script.
And others can only be interpreted as unicode notation.
Bu even if the character shown well in your editor,
you should first get the unicode number extracted from your font.

You can browse a character in a font with either of these program,
<code>xffd</code>, <code>unibrow</code>, or <code>gucharmap</code>.

{% highlight bash %}
$ xfd -fa FontAwesome
$ xfd -fa PowerlineSymbols
{% endhighlight %}

![XLFD: Finding Unicode][image-font-03]{: .img-responsive }

As you can see fro the image above,
the home icon from FontAwesome has unicode code
of <code>\uf015</code>.

{% highlight bash %}
$ echo -e '\uf015 '
{% endhighlight %}

![XLFD: Echoing Unicode][image-font-04]{: .img-responsive }

Now you can copy-paste the result to your script,
or remain to use the unicode number.

{% highlight bash %}
homeicon_awesome='\uf015' 
timeicon_siji='\ue018' 
{% endhighlight %}

By the time of this writing. 
I still do not know, how to find Siji Unicode.
This <code>xfd</code> and terminal show different bitmap shape.

-- -- --

### The Looks of, XLFD and XFT

I found that, the easiest comparation by the looks,
between XLFD (bitmap) and XFT (scalable),
can be made by echoing text into Lemonbar.
I'm using lemonbar-xft-git that support both XLFD and XFT.

The first figure is using XLFD

{% highlight bash %}
font_awesome='-*-fontawesome-medium-*-*-*-17-*-*-*-*-*-*-*'
{% endhighlight %}

![FontAwesome Character XLFD][image-font-05-xlfd]{: .img-responsive }

And the second figure is using XFT

{% highlight bash %}
font_awesome="FontAwesome-17"
{% endhighlight %}

![FontAwesome Character XFT][image-font-05-xft]{: .img-responsive }

-- -- --

I wish I knew earlier.
But well, better late than never.

That is all for now.
Thank you for reading.

Have a nice long weekend.
Have Fun with Font.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/04' %}

[image-font-01]: {{ asset_path }}/font-01-siji-pacaur-install.png
[image-font-02]: {{ asset_path }}/font-02-siji-xfontsel.png
[image-font-03]: {{ asset_path }}/font-03-awesome-xfontsel.png
[image-font-04]: {{ asset_path }}/font-04-awesome-home-unicode.png
[image-font-05-xlfd]: {{ asset_path }}/font-05-awesome-home-xlfd.png
[image-font-05-xft]:  {{ asset_path }}/font-05-awesome-home-xlft.png

[local-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html
[local-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html
