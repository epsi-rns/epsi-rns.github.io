---
layout: post
title: "Docker - Gentoo Portage - Part Two"
date: 2017-08-12 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, gentoo]
author: epsi

excerpt:
  Docker flow for Gentoo Portage,
  from emerge-webrsync to manual rsync.
  (not so) first time using Gentoo experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
# - 17081145  # Gentoo Portage

---

{% include post/2017/08/toc-docker-gentoo-portage.html %}

-- -- --

### Interesting Tools

I do think that these tools are what I need.

*	<code>Equery</code>

*	<code>Layman</code>

*	<code>Eix</code>

#### Equery

There are a lot of interesting stuff in package site.

*	<https://wiki.gentoo.org/wiki/Equery>

{% highlight bash %}
$ emerge --ask app-portage/gentoolkit
{% endhighlight %}

{% highlight bash %}
$ equery depgraph ncdu
{% endhighlight %}

![Docker Equery: Dependency in Graph][image-ss-equery-depgraph]{: .img-responsive }

#### Layman

Managing Repository

*	<https://wiki.gentoo.org/wiki/Layman>

{% highlight bash %}
$ layman-updater -R
{% endhighlight %}

{% highlight bash %}
$ layman -L
{% endhighlight %}

![Docker Layman: List Available][image-ss-layman-available]{: .img-responsive }

#### Eix

Diffing local ebuild.

*	<https://wiki.gentoo.org/wiki/Eix>

{% highlight bash %}
$ emerge --ask app-portage/eix
{% endhighlight %}

First Thing First, as usual.

{% highlight bash %}
$ eix-update
{% endhighlight %}

![Docker Eix: Update][image-ss-eix-update]{: .img-responsive }

{% highlight bash %}
$ eix-sync
{% endhighlight %}

![Docker Eix: Sync][image-ss-eix-sync]{: .img-responsive }

-- -- --

### Repository

Gentoo utilize <code>portage tree</code>
as main <code>ebuild repository</code>.

#### Configuration

*	<https://wiki.gentoo.org/wiki/etc/portage/repos.conf>

{% highlight bash %}
$ cat /usr/share/portage/config/repos.conf
[DEFAULT]
main-repo = gentoo

[gentoo]
location = /usr/portage
sync-type = rsync
sync-uri = rsync://rsync.gentoo.org/gentoo-portage
auto-sync = yes

# for daily squashfs snapshots
#sync-type = squashdelta
#sync-uri = mirror://gentoo/../snapshots/squashfs
{% endhighlight %}

![Docker Portage: Configuration][image-ss-r-repos-conf]{: .img-responsive }

In Funtoo this wouldlook a little bit different.

{% highlight bash %}
[DEFAULT]
main-repo = gentoo

[gentoo]
location = /usr/portage
sync-type = git
sync-uri = git://github.com/funtoo/ports-2012.git
auto-sync = yes
{% endhighlight %}

I do not know how it looks in Zentoo.

#### Overlay

Gentoo has repository concept called <code>ebuild repository</code>,
or in short called <code>overlay</code>.
Overlay is additional ebuild outside the main package tree.

*	<https://wiki.gentoo.org/wiki/Ebuild_repository>

This is a list of overlay.

*	<https://overlays.gentoo.org/>


There is an ebuild repository frontend called <code>layman</code>.

*	<https://wiki.gentoo.org/wiki/Layman>


#### Adding Overlay

Adding overlay is pretty straightforward.

{% highlight bash %}
$ layman -a xwing

 * Adding overlay...
 * Overlay "xwing" is not official. Continue installing? [y/n]: y
 * Running Rsync... # /usr/bin/rsync -rlptDvz --progress --delete --delete-after --timeout=180 --exclude=distfiles/* --exclude=local/* --exclude=packages/* rsync://gentoo.xwing.info/xwing-overlay/ /var/lib/layman/xwing
This is xwing.info / alderaan.xwing.info

Server Address : 91.134.139.206
Contact Name   : webmaster@xwing.info
Hardware       : VPS 2016 SSD 3
{% endhighlight %}

**More Info**

*	<https://gpo.zugaina.org/Overlays/xwing>

[![Docker Layman: Add Overlay][image-ss-r-layman-add]{: .img-responsive }][photo-ss-r-layman-add]

#### Configuration

Now xwing has been added to the <code>layman.conf</code>.

{% highlight bash %}
$ cat /etc/portage/repos.conf/layman.conf 
[xwing]
priority = 50
location = /var/lib/layman/xwing
layman-type = rsync
auto-sync = No
{% endhighlight %}

![Docker Layman: Configuration][image-ss-r-layman-conf]{: .img-responsive }

Consider have a look at the provided path.

{% highlight bash %}
$ ls /var/lib/layman/xwing/
app-office   media-gfx      net-misc        skel.metadata.xml
eclass       media-plugins  profiles        x11-misc
licenses     media-video    skel.ChangeLog  x11-plugins
mail-filter  metadata       skel.ebuild     x11-themes
{% endhighlight %}

![Docker Layman: /var/lib/layman][image-ss-r-var-lib-layman]{: .img-responsive }

#### List Overlays 

List available overlays. This will produce long list.

{% highlight bash %}
$ layman -L
{% endhighlight %}

List installed overlays.

{% highlight bash %}
$ layman -l

 * xwing                     [Rsync     ] (rsync://gentoo.xwing....)
{% endhighlight %}

![Docker Layman: List Installed][image-ss-r-layman-installed]{: .img-responsive }

#### Overlay 

Overlay removal is also straightforward.

{% highlight bash %}
$ layman -d xwing

 * Deleting selected overlay(s)...
 * Deleting directory "/var/lib/layman/xwing"
 * Successfully deleted overlay(s) xwing.
{% endhighlight %}

![Docker Layman: Remove Overlay][image-ss-r-layman-del]{: .img-responsive }

#### Synchronize Overlays

{% highlight bash %}
$ layman -S

 * Fetching remote list...
 * Fetch Ok

 * Syncing selected overlay(s)...
 * Running Rsync... # /usr/bin/rsync -rlptDvz --progress --delete --delete-after --timeout=180 --exclude=distfiles/* --exclude=local/* --exclude=packages/* rsync://gentoo.xwing.info/xwing-overlay/ /var/lib/layman/xwing
This is xwing.info / alderaan.xwing.info
...
{% endhighlight %}

[![Docker Layman: Synchronize][image-ss-r-layman-sync]{: .img-responsive }][photo-ss-r-layman-sync]

#### List Packages

We can use <code>eix</code> to list packages.
No need to check out <code>/var/lib/layman/xwing/</code> manually.
First we have to <code>eix-sync</code>.

{% highlight bash %}
$ eix-sync
{% endhighlight %}

{% highlight bash %}
$ eix --in-overlay xwing --only-names
app-office/grisbi
mail-filter/sqlgrey
media-gfx/AfterShotPro
...
{% endhighlight %}

![Docker Eix: --in-overlay][image-ss-r-eix-overlay]{: .img-responsive }

This is basic operation for ebuild repository.
It is enough for preview.
We are done.

-- -- --

### Mirror

Mirror can be configured in <code>/etc/portage/make.conf</code>.

*	<https://wiki.gentoo.org/wiki/Mirrorselect>

#### Install

{% highlight bash %}
$ emerge --ask app-portage/mirrorselect

These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild  N     ] dev-util/dialog-1.3.20170131  USE="nls unicode -examples -minimal -static-libs" 
[ebuild  N     ] app-portage/mirrorselect-2.2.2-r2  PYTHON_TARGETS="python2_7 python3_4" 

Would you like to merge these packages? [Yes/No] 
{% endhighlight %}

#### Select

{% highlight bash %}
$ mirrorselect -i -c Japan
* Using url: https://api.gentoo.org/mirrors/distfiles.xml
* Limiting test to "country=Japan" hosts. 
* Downloading a list of mirrors...
...
* Modifying /etc/portage/make.conf with new mirrors...
	Reading make.conf
	Moving to /etc/portage/make.conf.backup
	Writing new /etc/portage/make.conf
* Done.
{% endhighlight %}

![Docker Portage: mirrorlist dialog][image-ss-m-mirrorlist-dialog]{: .img-responsive }

#### Configuration

{% highlight bash %}
$ less /etc/portage/make.conf
...
GENTOO_MIRRORS="rsync://ftp.jaist.ac.jp/pub/Linux/Gentoo/ http://ftp.jaist.ac.jp/pub/Linux/Gentoo/"
{% endhighlight %}

![Docker Portage: mirror make.conf][image-ss-m-mirror-make-conf]{: .img-responsive }

#### Test

Now we have new Mirror

{% highlight bash %}
$ emerge-webrsync 
Fetching most recent snapshot ...
Trying to retrieve 20170908 snapshot from rsync://ftp.jaist.ac.jp/pub/Linux/Gentoo ...
Fetching file portage-20170908.tar.xz.md5sum ...
Fetching file portage-20170908.tar.bz2.md5sum ...
Fetching file portage-20170908.tar.gz.md5sum ...
Trying to retrieve 20170908 snapshot from http://ftp.jaist.ac.jp/pub/Linux/Gentoo ...
Fetching file portage-20170908.tar.xz.md5sum ...
Fetching file portage-20170908.tar.xz.gpgsig ...
Fetching file portage-20170908.tar.xz ...
Checking digest ...
Getting snapshot timestamp ...
...
{% endhighlight %}

![Docker Portage: jaist webrsync][image-ss-m-jaist-webrsync]{: .img-responsive }

-- -- -- 

### Hold Package

Portage hold package using package.mask configuration.

*	<https://wiki.gentoo.org/wiki//etc/portage/package.mask>

#### Example

{% highlight bash %}
$ eix --installed --upgrade
[U] app-text/docbook-xml-dtd
     Available versions:  
     (4.1.2) 4.1.2-r6
     (4.2)  4.2-r2
     (4.3)  4.3-r1
     (4.4)  4.4-r2
     (4.5)  4.5-r1
     Installed versions:  4.1.2-r6(4.1.2)(05:57:54 08/03/17)
     Homepage:            http://www.docbook.org/
     Description:         Docbook DTD for XML
{% endhighlight %}

![Docker Portage: upgradable][image-ss-h-eix-iu]{: .img-responsive }

Consider app-text/docbook-xml-dtd as our guinea pig example.

#### Before Mask

This is the result before mask.

*	Latest version available: 4.5-r1

{% highlight bash %}
$ emerge -a docbook-xml-dtd

These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild  NS    ] app-text/docbook-xml-dtd-4.5-r1 [4.1.2-r6]

Would you like to merge these packages? [Yes/No] n

Quitting.
{% endhighlight %}

![Docker Portage: example docbook][image-ss-h-example]{: .img-responsive }

{% highlight bash %}
$ emerge -s docbook-xml-dtd
  
[ Results for search key : docbook-xml-dtd ]
Searching...

...

*  app-text/docbook-xml-dtd
      Latest version available: 4.5-r1
      Latest version installed: 4.1.2-r6
      Size of files: 97 KiB
      Homepage:      http://www.docbook.org/
      Description:   Docbook DTD for XML
      License:       docbook
{% endhighlight %}

#### Configuration

Limit version to: 4.1.2-r6

{% highlight bash %}
$ echo ">app-text/docbook-xml-dtd-4.1.2-r6" > /etc/portage/package.mask/docbook
{% endhighlight %}

{% highlight bash %}
$ cat /etc/portage/package.mask/docbook
>app-text/docbook-xml-dtd-4.1.2-r6
{% endhighlight %}

Consider have a look at comparation below.

![Docker Portage: comparation][image-ss-h-package-mask]{: .img-responsive }

#### After Mask

This is the result after mask.

*	Latest version available: 4.1.2-r6

{% highlight bash %}
$ emerge -a docbook-xml-dtd
These are the packages that would be merged, in order:

Calculating dependencies... done!
[ebuild   R    ] app-text/docbook-xml-dtd-4.1.2-r6 

Would you like to merge these packages? [Yes/No] 
{% endhighlight %}

{% highlight bash %}
$ emerge -s docbook-xml-dtd
  
[ Results for search key : docbook-xml-dtd ]
Searching...

...

*  app-text/docbook-xml-dtd
      Latest version available: 4.1.2-r6
      Latest version installed: 4.1.2-r6
      Size of files: 74 KiB
      Homepage:      http://www.docbook.org/
      Description:   Docbook DTD for XML
      License:       docbook
{% endhighlight %}

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.

{% highlight bash %}
$ less /var/log/portage/elog/summary.log
>>> Messages generated by process 282 on 2017-08-22 16:59:40 -00 for
 package sys-process/htop-2.0.2:

WARN: setup
To use lsof features in htop(what processes are accessing
what files), you must have sys-process/lsof installed.
Unable to find kernel sources at /usr/src/linux
Unable to calculate Linux Kernel version for build, attempting to use running version
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/zypp/history][image-ss-less-log]{: .img-responsive }

-- -- --

### Clean Up

Time after time, your portage source directory,
may growing bigger and bigger in size.
You can clean these directory.

Package Source
	
*	/usr/portage/distfiles

{% highlight bash %}
$ ls -lR /usr/portage/distfiles
{% endhighlight %}

![Docker portage: Source][image-ss-portage-source]{: .img-responsive }

This will only remove package if there are newer version.
This command takes long time.
Do not do that when you are very tired.

{% highlight bash %}
$ eclean distfiles
 * Building file list for distfiles cleaning...
 * Your distfiles directory was already clean.
{% endhighlight %}

![Docker portage: Clean Dist][image-ss-eclean-distfiles]{: .img-responsive }

This will will keep only installed package.

{% highlight bash %}
$ eclean-dist -d
 * Building file list for distfiles cleaning...
 * Cleaning distfiles...
 [    3.3 M ] fish-2.4.0.tar.gz
 [  465.2 K ] htop-2.0.2.tar.gz
 ===========
 [    3.8 M ] Total space from 2 files were freed in the distfiles directory

{% endhighlight %}

![Docker portage: Clean Deep][image-ss-eclean-dist-deep]{: .img-responsive }

-- -- --

### Conclusion

	I am a n00bs !

I just feel that I'm not afraid to use emerge.
Portage is not scary at all.

-- -- --

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-gentoo' %}

[image-ss-portage-source]:     {{ asset_post }}/17-dir-source.png
[image-ss-eclean-distfiles]:   {{ asset_post }}/17-eclean-distfiles.png
[image-ss-eclean-dist-deep]:   {{ asset_post }}/17-eclean-dist-deep.png

[image-ss-less-log]:           {{ asset_post }}/19-log.png

[image-ss-equery-depgraph]:    {{ asset_post }}/21-equery-depgraph.png

[image-ss-layman-available]:   {{ asset_post }}/21-layman-list.png

[image-ss-eix-sync]:      {{ asset_post }}/21-eix-sync-half.png
[photo-ss-eix-sync]:      https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM0XeXwWZOJK2dUO-hl8DvUWfUVb3Sn5WfwebdJ?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-eix-update]:    {{ asset_post }}/21-eix-update.png

[image-ss-r-layman-add]:  {{ asset_post }}/25-layman-add-half.png
[photo-ss-r-layman-add]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOMu9nSOgb10i0Omosy3lV-yymYS4YEuQNu7Grp?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-r-layman-sync]: {{ asset_post }}/25-layman-sync-half.png
[photo-ss-r-layman-sync]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNUNmp7-O6k7TW-p_WddOXnsLnpYhAmmPI-1Th6?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-r-repos-conf]:  {{ asset_post }}/25-repos-conf.png
[image-ss-r-eix-overlay]: {{ asset_post }}/25-eix-in-overlay.png
[image-ss-r-layman-conf]: {{ asset_post }}/25-layman-conf.png
[image-ss-r-layman-del]:  {{ asset_post }}/25-layman-delete.png
[image-ss-r-layman-installed]: {{ asset_post }}/25-layman-list-installed.png
[image-ss-r-var-lib-layman]:   {{ asset_post }}/25-var-lib-layman-xwing.png

[image-ss-m-jaist-webrsync]:    {{ asset_post }}/26-mirror-jaist-webrsync.png
[image-ss-m-mirrorlist-dialog]: {{ asset_post }}/26-mirrorlist-dialog.png
[image-ss-m-mirror-make-conf]:  {{ asset_post }}/26-mirror-make-conf.png

[image-ss-h-eix-iu]:       {{ asset_post }}/27-eix-installed-upgrade.png
[image-ss-h-example]:      {{ asset_post }}/27-emerge-s-docbook-xml-dtd.png
[image-ss-h-package-mask]: {{ asset_post }}/27-package-mask.png
