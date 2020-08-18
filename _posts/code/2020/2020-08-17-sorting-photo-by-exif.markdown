---
layout    : post
title     : "Sorting Photo by EXIF"
date      : 2020-08-17 09:35:15 +0700
categories: code
tags      : [bash]
keywords  : [EXIF, imagemagick]
author: epsi

opengraph:
  image: /assets/site/images/topics/bash.png

excerpt:
  How to sort photo in file manager by EXIF?
  This can be automated with imagemagick and bash.

---

It has been seven months since my last post.

### Preface

> Goal: Automate file renaming using BASH, to sort file by EXIF.

#### Sort by Renaming

Rare but happened, designer require to sort artwork,
such as photograph by the date the picture taken.
The idea is using embedded EXIF data.
If we can extract EXIF data,
we can rename files with almost any EXIF field as suffix.
For example using date and time we can rename from this file:

```
191320.jpg
```

to this

```
2020:08:04 19:13:20 191320.jpg
```

you can automate the task with `bash` script.
And then, it is the file manager job to display in a sorted fashioned.

#### Check EXIF

Most file manager can obtain EXIF information.
Either from file properties, or preview or else.

![EXIF in Thunar: File Manager Properties][image-thunar-properties]{: .img-responsive }

-- -- --

### Extract EXIF Using Imagemagick

There are many ways to extract EXIF,
one of them is using imagemagick.

#### Identify

Extracting EXIF, can be done using `imagemagick`.
You must install `imagemagick` first.
Then run this `identify` command with `verbose` option.

{% highlight bash %}
$ identify -verbose 121350.jpg
Image:
  Filename: 121350.jpg
  Format: JPEG (Joint Photographic Experts Group JFIF format)
  Mime type: image/jpeg
  Class: DirectClass
  Geometry: 2560x1536+0+0
  Resolution: 72x72
  Print size: 35.5556x21.3333
  Units: PixelsPerInch
  Colorspace: sRGB
...
{% endhighlight %}

![Imagemagick: identity -verbose][image-identity-verbose]{: .img-responsive }

#### Filtering Information

Yes, we can filter information using pipe in bash.
For eaxmple using `grep` to get any line containg `Date`.

{% highlight bash %}
$ identify -verbose 121350.jpg | grep Date
    exif:DateTime: 2020:08:15 12:13:50
    exif:DateTimeDigitized: 2020:08:15 12:13:50
    exif:DateTimeOriginal: 2020:08:15 12:13:50
{% endhighlight %}

![Imagemagick: identity, grep Date][image-identity-grep-date]{: .img-responsive }

Beware with the capital letter in `grep`,
or you may end up with the wrong information.

{% highlight bash %}
$ identify -verbose 121350.jpg | grep date
    date:create: 2020-08-18T12:47:25+00:00
    date:modify: 2020-08-15T05:19:38+00:00
{% endhighlight %}

![Imagemagick: identity, grep date][image-identity-grep-case]{: .img-responsive }

Now we can go further with just getting the first line.

{% highlight bash %}
$ identify -verbose 121350.jpg | grep "exif:DateTime" | head -1
    exif:DateTime: 2020:08:15 12:13:50
{% endhighlight %}

![Imagemagick: identity, grep, head first line][image-identity-head-first]{: .img-responsive }

-- -- --

### Automate Renaming Using BASH

#### Test with one file.

Consider to start from simple script.

{% highlight bash %}
#!/usr/bin/env bash

# initialize contstant
prefix="exif:DateTime"
jpg="121350.jpg"

# some unknown magic here
exif=$(identify -verbose ${jpg} | grep ${prefix} | head -1)
echo -e $exif

# whoaa... read the bash manual
datetime=${exif:19:20}
echo -e "[$datetime]"
{% endhighlight %}

Name the file as you want such as `exif-extract-test.sh`.
And do not forget to set the executable with `chmod +x`.

{% highlight bash %}
$ ./exif-extract-test.sh
exif:DateTime: 2020:08:15 12:13:50
[2020:08:15 12:13:50]
{% endhighlight %}

I'm using bracket box, to check,
if there are any spaces that need to be trimmed.

![BASH Source in ViM: Extract EXIF from one image file][image-source-extract-test]{: .img-responsive }

With the result as below figure:

![BASH: Extract EXIF from one image file][image-bash-extract-test]{: .img-responsive }

#### Prepare The Case

Supposed we have these folder with a few `jpg` images.

{% highlight bash %}
$ ls -1
121350.jpg
121403.jpg
121405.jpg
121407.jpg
191320.jpg
191322.jpg
191334.jpg
191347.jpg
191355.jpg
191402.jpg
exif-rename-by-datetime.sh
{% endhighlight %}

![BASH: image before renamed][image-bash-case-input]{: .img-responsive }

#### The Final Script

Name the file as you want such as `exif-rename-by-datetime.sh`.

{% highlight bash %}
#!/usr/bin/env bash

# initialize contstant
prefix="exif:DateTime"

# color
esc=""; purplef="${esc}[35m"; reset="${esc}[0m"

# loop
for jpg in *.jpg; do
  # if not exist
  [[ -e $jpg ]] || continue 

  # do the deed, with imagemagick's identify
  exif=$(identify -verbose ${jpg} | grep ${prefix} | head -1)
  datetime=${exif:19:20}
  
  # carry on, print a fancy notification
  newname="$datetime $jpg"
  echo -e "${purplef}Renaming to${reset}: $newname"
  
  # beware of the space, use double quotes.
  mv "$jpg" "$newname"
done

# cheerio, see you next time
{% endhighlight %}

![BASH Source in ViM: Running The Script to Rename Image Files][image-source-rename-all]{: .img-responsive }

I assume you can get any bash reference yourself.

#### Running The Script

Consider run the script

{% highlight bash %}
$ ./exif-rename-by-datetime.sh
Renaming to: 2020:08:15 12:13:50 121350.jpg
Renaming to: 2020:08:15 12:14:03 121403.jpg
Renaming to: 2020:08:15 12:14:05 121405.jpg
Renaming to: 2020:08:15 12:14:07 121407.jpg
Renaming to: 2020:08:04 19:13:20 191320.jpg
Renaming to: 2020:08:04 19:13:22 191322.jpg
Renaming to: 2020:08:04 19:13:34 191334.jpg
Renaming to: 2020:08:04 19:13:47 191347.jpg
Renaming to: 2020:08:04 19:13:55 191355.jpg
Renaming to: 2020:08:04 19:14:02 191402.jpg
{% endhighlight %}

![BASH: Running The Script to Rename Image Files][image-bash-rename-all]{: .img-responsive }

And have a look at the result.

{% highlight bash %}
$ ls -1
'2020:08:04 19:13:20 191320.jpg'
'2020:08:04 19:13:22 191322.jpg'
'2020:08:04 19:13:34 191334.jpg'
'2020:08:04 19:13:47 191347.jpg'
'2020:08:04 19:13:55 191355.jpg'
'2020:08:04 19:14:02 191402.jpg'
'2020:08:15 12:13:50 121350.jpg'
'2020:08:15 12:14:03 121403.jpg'
'2020:08:15 12:14:05 121405.jpg'
'2020:08:15 12:14:07 121407.jpg'
exif-rename-by-datetime.sh
{% endhighlight %}

![BASH: image after renamed][image-bash-case-output]{: .img-responsive }

We are done.
Have fun with your photographs.
Good luck with your designs.

-- -- --

I think this is enough for today.
Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = site.url | append: '/assets/posts/code/2020/08' %}

[image-thunar-properties]:  {{ asset_path }}/exif-thunar-properties.png
[image-identity-verbose]:   {{ asset_path }}/exif-identity-verbose.png
[image-identity-grep-date]: {{ asset_path }}/exif-identity-grep-date-capital.png
[image-identity-grep-case]: {{ asset_path }}/exif-identity-grep-date-lowercase.png
[image-identity-head-first]:{{ asset_path }}/exif-identity-head-first.png

[image-bash-extract-test]:  {{ asset_path }}/bash-extract-test.png
[image-bash-rename-all]:    {{ asset_path }}/bash-rename-all.png
[image-bash-case-input]:    {{ asset_path }}/case-input.png
[image-bash-case-output]:   {{ asset_path }}/case-output.png

[image-source-extract-test]:{{ asset_path }}/bash-vim-extract-test.png
[image-source-rename-all]:  {{ asset_path }}/bash-vim-rename-by-datetime.png
