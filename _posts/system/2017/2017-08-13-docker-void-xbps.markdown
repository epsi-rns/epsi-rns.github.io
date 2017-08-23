---
layout: post
title: "Docker - Void XBPS"
date: 2017-08-13 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, void]
author: epsi

excerpt:
  Docker flow for Void Linux XBPS,
  first time using Void experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Preface

> Goal: Learning Package Manager, Focus on Command Line Interface

Void Linux is so interesting

*	Void has their own package manager, named XBPS.

*	Void is using runit, as alternative init to systemd, sysvinit, or openrc.

Since we are going to use docker again,
you can read a common overview here.

*	[Docker - Flow for Learning Linux Distribution][local-docker-flow]

-- -- --

### Getting Started With Docker

As usual, first, we do attach docker process.

{% highlight bash %}
$ docker pull voidlinux/voidlinux
{% endhighlight %}

{% highlight bash %}
$ docker image list 
{% raw %}
  --format 'table {{.Repository}}\t{{.Size}}'
{% endraw %}
REPOSITORY              SIZE
gentoo/stage3-amd64     873MB
vbatts/slackware        86.7MB
voidlinux/voidlinux     202MB
kevinleptons/lfs-auto   753MB
{% endhighlight %}

{% highlight bash %}
$ docker run -it voidlinux/voidlinux bash
bash-4.4# exit
{% endhighlight %}

{% highlight bash %}
$ docker ps -a 
{% raw %}
  --format 'table {{.Image}}\t{{.Names}}\t{{.Status}}'
{% endraw %}
IMAGE                 NAMES               STATUS
voidlinux/voidlinux   awesome_davinci     Up About an hour
gentoo/stage3-amd64   amazing_shirley     Exited (0) 23 hours ago
vbatts/slackware      cranky_keller       Exited (0) 26 hours ago

{% endhighlight %}

{% highlight bash %}
$ docker start awesome_davinci
awesome_davinci
{% endhighlight %}

{% highlight bash %}
$ docker attach awesome_davinci
bash-4.4#
{% endhighlight %}

![Docker Void: Getting Started][image-ss-void-docker]{: .img-responsive }

-- -- --

### Package Management

Void Linux use The X Binary Package System (XBPS) as package management.

#### Reading

*	<https://www.voidlinux.eu/usage/xbps/>

*	<https://wiki.voidlinux.eu/Rosetta_stone>

#### Source Code

*	<https://github.com/voidlinux/xbps>

-- -- --

### Update

	First Thing First

{% highlight bash %}
$ xbps-install -Su

[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...
{% endhighlight %}

[![Docker Void: XBPS System Upgrade: Start][image-ss-xbps-su-top]{: .img-responsive }][photo-ss-xbps-su-top]

[![Docker Void: XBPS System Upgrade: End][image-ss-xbps-su-bottom]{: .img-responsive }][photo-ss-xbps-su-bottom]

-- -- --

### Package IRSI

	Install, Remove, Search, Info

Read the fine manual.

#### Package Install

{% highlight bash %}
$ xbps-install -S nano htop mc ncdu curl
[*] Updating 'https://repo.voidlinux.eu/current/x86_64-repodata' ...

Name    Action    Version           New version            Download size
nano    install   -                 2.8.6_1                - 
htop    install   -                 2.0.2_2                - 
libgpm  install   -                 1.20.7_8               - 
libelf  install   -                 0.170_1                - 
libffi  install   -                 3.2.1_2                - 
glib    install   -                 2.52.3_1               - 
mc      install   -                 4.8.19_1               - 
ncdu    install   -                 1.12_1                 - 
jansson install   -                 2.10_1                 - 
nghttp2 install   -                 1.24.0_1               - 
libcurl install   -                 7.55.1_1               - 
curl    install   -                 7.55.1_1               - 

Size required on disk:          22MB
Free space on disk:             27GB

Do you want to continue? [Y/n] 
{% endhighlight %}

[![Docker Void:  XBPS Package Install: Start][image-ss-xbps-fav-top]{: .img-responsive }][photo-ss-xbps-fav-top]

[![Docker Void: XBPS Package Install: End][image-ss-xbps-fav-bottom]{: .img-responsive }][photo-ss-xbps-fav-bottom]

#### Package Remove

{% highlight bash %}
$ xbps-remove -R nano htop mc ncdu curl

Name    Action    Version           New version            Download size
nano    remove    2.8.6_1           -                      - 
htop    remove    2.0.2_2           -                      - 
mc      remove    4.8.19_1          -                      - 
glib    remove    2.52.3_1          -                      - 
libgpm  remove    1.20.7_8          -                      - 
libffi  remove    3.2.1_2           -                      - 
libelf  remove    0.170_1           -                      - 
ncdu    remove    1.12_1            -                      - 
curl    remove    7.55.1_1          -                      - 
libcurl remove    7.55.1_1          -                      - 
nghttp2 remove    1.24.0_1          -                      - 
jansson remove    2.10_1            -                      - 

Size freed on disk:             22MB
Free space on disk:             27GB

Do you want to continue? [Y/n]
{% endhighlight %}

[![Docker Void: XBPS Package Removal][image-ss-xbps-remove]{: .img-responsive }][photo-ss-xbps-remove]

#### Package Query Search

{% highlight bash %}
$ xbps-install fish
Unable to locate 'fish' in repository pool.
{% endhighlight %}

{% highlight bash %}
$ xbps-query -Rs fish
[-] bluefish-2.2.10_1               A powerful HTML editor for e...
[-] fish-shell-2.6.0_1              User friendly shell intended...
[-] perl-Crypt-Blowfish-2.14_5      Crypt::Blowfish - Blowfish c...
[-] perl-Crypt-Blowfish_PP-1.12_2   Blowfish encryption algorith...
[-] python-jellyfish-0.5.6_2        Python2 library for approxim...
[-] python3-jellyfish-0.5.6_2       Python3 library for approxim...
[-] python3.4-jellyfish-0.5.6_2     Python3.4 library for approx...
[-] stockfish-8_1                   A free UCI chess engine deri...
[-] wifish-1.1.3_2                  Simple wifi tool
[-] zsh-syntax-highlighting-0.5.0_1 Fish shell like syntax highl...
{% endhighlight %}

{% highlight bash %}
$ xbps-install fish-shell
3 packages will be downloaded:
  bc-1.07.1_1 groff-1.22.3_3 fish-shell-2.6.0_1 
3 packages will be installed:
  bc-1.07.1_1 groff-1.22.3_3 fish-shell-2.6.0_1 

Size to download:             3070KB
Size required on disk:          19MB
Free space on disk:             27GB

Do you want to continue? [Y/n] y
{% endhighlight %}

[![Docker Void: XBPS Search Query][image-ss-xbps-query]{: .img-responsive }][photo-ss-xbps-query]

#### Package Show Info

{% highlight bash %}
xbps-query -RS htop
{% endhighlight %}

![Docker Void: XBPS Show Info][image-ss-xbps-info]{: .img-responsive }

-- -- --

### Conclusion

There are still things that I do not understand,
such as <code>xbps-src</code> and
install from git source.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-void' %}

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-void-docker]:     {{ asset_post }}/00-getting-started.png

[image-ss-xbps-su-top]:     {{ asset_post }}/01-xbps-install-su-1-half.png
[photo-ss-xbps-su-top]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipN7yNgtq3tLkiTAAwArqzRtIjtdFEdHjEoHYp-_?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-xbps-su-bottom]:  {{ asset_post }}/01-xbps-install-su-2-half.png
[photo-ss-xbps-su-bottom]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOAAatxsZJD9QdtiJeYv4gZdjrY4-Z1gzOcUkdk?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-xbps-fav-top]:    {{ asset_post }}/13-xbps-install-1-half.png
[photo-ss-xbps-fav-top]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMhv4nG5EBx7QOcBVxLhNxwinyMV3SnAwjKifN2?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-xbps-fav-bottom]: {{ asset_post }}/13-xbps-install-2-half.png
[photo-ss-xbps-fav-bottom]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNYFTqIMX7qwej3iG01pnf0NoApP_ILFFpwsuZX?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-xbps-query]:      {{ asset_post }}/13-xbps-query-half.png
[photo-ss-xbps-query]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNqZ1HtZh9xjxFEIV5S7AH7F6x26oogQA8m1XEg?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-xbps-remove]:     {{ asset_post }}/13-xbps-remove-half.png
[photo-ss-xbps-remove]:     https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP_ea-U8XItnqBn0FV8XfB7JbDGUpjcUZMAd3LO?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-xbps-info]:       {{ asset_post }}/13-xbps-info.png
