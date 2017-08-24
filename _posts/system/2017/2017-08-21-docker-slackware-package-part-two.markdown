---
layout: post
title: "Docker - Slackware Package - Part Two"
date: 2017-08-21 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, slackware]
author: epsi

excerpt:
  Docker flow for Slackware Package Management,
  using slapt-get, slapt-src, and slpkg.
  First time using Slackware experience.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Topics

This is a two-parts article.
There are few topics here.

*	[ [Part One][local-part-one] ] Preface

*	[ [Part One][local-part-one] ] Preparing Docker

*	[ [Part One][local-part-one] ] IRSI: slackpkg install, removepkg, search, info

*	[ [Part One][local-part-one] ] Using slackbuild, manual compilation, and installpkg

*	[ [Part One][local-part-one] ] Using sbopkg, automatic compilation

*	[ [Part Two][local-part-two] ] Using slapt-get and slapt-src

*	[ [Part Two][local-part-two] ] Using slpkg

*	[ [Part Two][local-part-two] ] Conclusion

There are still other topic uncovered here

*	slackpkgplus

*	adding non official repo such as alien and slacky

-- -- --

### Using slapt-get

There also unofficial package manager for slackware that emulate Debian apt-get.
<code>slapt-get</code> for binary,
and <code>slapt-src</code> for source such as slackbuild.

#### Reading

*	<https://software.jaos.org/>

*	<https://www.slackwiki.com/Slapt-get>

#### Installing slapt-get

{% highlight bash %}
$ wget -c  --no-check-certificate https://software.jaos.org/slackpacks/14.2-x86_64/slapt-get/slapt-get-0.10.2t-x86_64-1.tgz

$ tar -xvf slapt-get-0.10.2t-x86_64-1.tgz
{% endhighlight %}

{% highlight bash %}
$ installpkg slapt-get-0.10.2t-x86_64-1.tgz 
Verifying package slapt-get-0.10.2t-x86_64-1.tgz.
Installing package slapt-get-0.10.2t-x86_64-1.tgz:
...
Executing install script for slapt-get-0.10.2t-x86_64-1.tgz.
Package slapt-get-0.10.2t-x86_64-1.tgz installed.
{% endhighlight %}

![Docker slapt-get: Installing][image-ss-install-slaptget]{: .img-responsive }

#### Dependencies of slapt-get

These packages are required to run <code>slapt-get</code>

{% highlight bash %}
$ slackpkg install gpgme

$ slackpkg install libassuan

$ slackpkg install libgpg

$ slackpkg install cyrus-sasl
{% endhighlight %}

#### Update

	First Thing First

{% highlight bash %}
$ slapt-get --update
Retrieving package data [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Retrieving patch list [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Retrieving checksum list [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Retrieving checksum signature [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Verifying checksum signature [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Verified
Retrieving ChangeLog.txt [ftp://ftp.slackware.com/pub/slackware/slackware64-14.2/]...Cached
Reading Package Lists...Done
Retrieving package data [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Retrieving patch list [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Retrieving checksum list [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Retrieving checksum signature [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Verifying checksum signature [http://software.jaos.org/slackpacks/14.2-x86_64/]...No key for verification
Retrieving ChangeLog.txt [http://software.jaos.org/slackpacks/14.2-x86_64/]...Cached
Reading Package Lists...Done
{% endhighlight %}

[![Docker slapt-get: Update][image-ss-slaptget-update]{: .img-responsive }][photo-ss-slaptget-update]

#### Upgrade

{% highlight bash %}
$ slapt-get --upgrade
Reading Package Lists...Done
The following packages have been EXCLUDED:
  glibc-solibs 
0 upgraded, 0 reinstalled, 0 newly installed, 0 to remove, 1 not upgraded.

Done
{% endhighlight %}

[![Docker slapt-get: Upgrade][image-ss-slaptget-upgrade-half]{: .img-responsive }][photo-ss-slaptget-upgrade-full]

#### Install

This <code>htop</code> install succeed.

{% highlight bash %}
$ slapt-get --install htop
Reading Package Lists...Done
htop is up to date.
0 upgraded, 0 reinstalled, 0 newly installed, 0 to remove, 0 not upgraded.

Done
{% endhighlight %}

![Docker slapt-get: Install htop][image-ss-slaptget-htop]{: .img-responsive }

But this <code>ncdu</code> install failed,
because <code>ncdu</code> comes from slackbuild.

{% highlight bash %}
slapt-get --install ncdu
Reading Package Lists...Done
No such package: ncdu
{% endhighlight %}

![Docker slapt-get: Install ncdu][image-ss-slaptget-ncdu]{: .img-responsive }

#### Show Info

{% highlight bash %}
slapt-get --show ncdu
Package Name: ncdu
Package Mirror: 
Package Priority: Default
Package Location: 
Package Version: 1.8-x86_64-1_SBo
Package Size: 28 K
Package Installed Size: 80 K
Package Required: 
Package Conflicts: 
Package Suggests: 
Package MD5 Sum:  
Package Description:
 ncdu (NCurses Disk Usage)

 As the name already suggests, ncdu is an NCurses version of the famous
 old 'du' unix command. It provides a fast and easy interface to your
 harddrive. Where is your disk space going? Why is your home directory
 that large? ncdu can answer those questions for you in just a matter
 of seconds!

 http://dev.yorhel.nl/ncdu/


Package Installed: yes
{% endhighlight %}

![Docker slapt-get: Show Info ncdu][image-ss-slaptget-show-ncdu]{: .img-responsive }

Since ncdu come from slackbuild repository that needed to be compiled first.
We need another tool to manage.

-- -- --

### slapt-src

In order to use slackbuild form source,
we can utilize <code>slapt-src</code>.
This time no need to wget,
since we already have <code>slapt-get</code>.

{% highlight bash %}
$ slapt-get --install slapt-src
Reading Package Lists...Done
The following NEW packages will be installed:
  slapt-src 
0 upgraded, 0 reinstalled, 1 newly installed, 0 to remove, 0 not upgraded.
Need to get 48.0kB of archives.
After unpacking 220.0kB of additional disk space will be used.
1/1 Get http://software.jaos.org/slackpacks/14.2-x86_64/ slapt-src 0.3.2i-x86_64-1 [48.0kB]...Done

Preparing to install slapt-src-0.3.2i-x86_64-1
Verifying package slapt-src-0.3.2i-x86_64-1.tgz.
Installing package slapt-src-0.3.2i-x86_64-1.tgz:
PACKAGE DESCRIPTION:
# slapt-src (slapt slackbuild utility)
# slapt-src is a utility to make querying, retrieving, and building
# slackbuilds as easy as working with binary packages with slapt-get.
# 
#
Executing install script for slapt-src-0.3.2i-x86_64-1.tgz.
Package slapt-src-0.3.2i-x86_64-1.tgz installed.

Done
{% endhighlight %}

![Docker slapt-src: Installing][image-ss-slaptget-slaptsrc]{: .img-responsive }

#### Update

	First Thing First

Do update first before doing nay task with any command.

{% highlight bash %}
$ slapt-src -u
Fetching slackbuild list from http://www.slackware.org.uk/slackbuilds.org/14.0/...Cached
{% endhighlight %}

![Docker slapt-src: Update][image-ss-slaptsrc-update]{: .img-responsive }

#### Install

Now we can install <code>ncdu</code> from slackbuild.

{% highlight bash %}
sh-4.3# slapt-src --install ncdu
The following packages will be installed:
 ncdu 
Do you want to continue? [y/N] y
Fetching README...Done
Fetching ncdu.SlackBuild...Done
Fetching ncdu.info...Done
Fetching slack-desc...Done
ncdu-1.8/
ncdu-1.8/doc/
ncdu-1.8/doc/Makefile.am
ncdu-1.8/doc/Makefile.in
ncdu-1.8/doc/ncdu.1
ncdu-1.8/src/
{% endhighlight %}

[![Docker slapt-src: Install ncdu][image-ss-slaptsrc-ncdu-half]{: .img-responsive }][photo-ss-slaptsrc-ncdu-full]

-- -- --

### Installing slpkg

I have been wondering how to add other repository,
such as <code>alien</code> and <code>slacky</code>.
And I found this <code>slpkg</code>. 

#### Dependency

{% highlight bash %}
$ slackpkg install python
{% endhighlight %}

![Docker slackpkg: Install Python][image-ss-slackpkg-python]{: .img-responsive }

#### Install

Now you can install <code>slpkg</code> using any method you like.

{% highlight bash %}
$ sbopkg -i slpkg
{% endhighlight %}

#### No certificate

My first attempt, is always failure as usual.

{% highlight bash %}
$ slpkg update 

To connect to mirrors.slackware.com insecurely, use --no-check-certificate.
{% endhighlight %}

![Docker slpkg: Update Certificate Error][image-ss-slpkg-update-err]{: .img-responsive }

If you encounter this error, you need to install <code>ca-certificates</code>.

{% highlight bash %}
$ slackpkg install ca-certificates
{% endhighlight %}

![Docker slackpkg: CA Certificates][image-ss-slackpkg-ca-cert]{: .img-responsive }

Now you should be ready.

-- -- --

### Using slpkg

#### Update

	First Thing First

{% highlight bash %}
$ slpkg update 

Check and update repositories:

Check repository [slack] ... Done
Check repository [sbo] ... Done
{% endhighlight %}

![Docker slpkg: Update Succeed][image-ss-slpkg-update]{: .img-responsive }

#### Repository 

{% highlight bash %}
$ slpkg repo-list

+==============================================================================
| Repo id  Repo URL                                            Default   Status
+==============================================================================
  alien    http://bear.alienbase.nl/mirrors/people/alien/sb~   yes     disabled
  connos   https://connochaetos.org/slack-n-free/              yes     disabled
  conrad   http://slack.conraid.net/repository/slackware64-~   yes     disabled
  csb      http://slackware.uk/csb/                            yes     disabled
  ktown    http://alien.slackbook.org/ktown/                   yes     disabled
  mles     http://slackware.uk/microlinux/                     yes     disabled
  msb      http://slackware.org.uk/msb/                        yes     disabled
  multi    http://bear.alienbase.nl/mirrors/people/alien/mu~   yes     disabled
  rested   http://bear.alienbase.nl/mirrors/people/alien/re~   yes     disabled
  rlw      http://slackware.uk/people/rlworkman/               yes     disabled
  salix    http://download.salixos.org/                        yes     disabled
  sbo      http://slackbuilds.org/slackbuilds/                 yes      enabled
  slack    http://mirrors.slackware.com/slackware/             yes      enabled
  slacke   http://ngc891.blogdns.net/pub/                      yes     disabled
  slackl   http://www.slackel.gr/repo/                         yes     disabled
  slacky   http://repository.slacky.eu/                        yes     disabled
  slonly   https://slackonly.com/pub/packages/                 yes     disabled

Repositories summary
===============================================================================
2/17 enabled default repositories and 0 custom.
For enable or disable default repositories edit '/etc/slpkg/repositories.conf'
file or run 'slpkg repo-enable' command.
{% endhighlight %}

![Docker slpkg: Repo List][image-ss-slpkg-repo-list]{: .img-responsive }

You can edit directly.

{% highlight bash %}
$ cat /etc/slpkg/repositories.conf
[REPOSITORIES]
slack
sbo
# alien
# rlw
...
{% endhighlight %}

![Docker slpkg: repositories.conf][image-ss-slpkg-etc-repo]{: .img-responsive }

#### Install:

{% highlight bash %}
$ slpkg -s sbo fish
Reading package lists... Done
Resolving dependencies... Done

The following packages will be automatically installed or upgraded 
with new version:

+==============================================================================
| Package                 New version        Arch    Build  Repos          Size
+==============================================================================
Installing:
  fish                    2.6.0              x86_64         SBo
Installing for dependencies:
  man-db                  2.7.6.1            x86_64         SBo

Installing summary
===============================================================================
Total 2 packages.
2 packages will be installed, 0 already installed and 0 package
will be upgraded.

Would you like to continue [y/N]? 
{% endhighlight %}

Unfortunately installation failed, we need to solve some dependency first.

{% highlight bash %}
$ slackpkg install gdbm
{% endhighlight %}

And install again. Do the installation command over again.

{% highlight bash %}
$ slpkg -s sbo fish
{% endhighlight %}

[![Docker slpkg: Install sbo fish][image-ss-slpkg-sbo-fish]{: .img-responsive }][photo-ss-slpkg-sbo-fish]

Mission accomplished, Now I can <code>fish</code>.

{% highlight bash %}
$ fish
Welcome to fish, the friendly interactive shell
root@662f86a36b6d ~# 
{% endhighlight %}

You are freely to try other package other than <code>fish</code>.

{% highlight bash %}
$ slpkg -s sbo ncdu
{% endhighlight %}

-- -- --

### Conclusion

It is said that "_Slackware does not resolve dependency_".
I haven't got enough experience about this situation.
I simply do not understand what it means.

	I guess I have to learn slackware more.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-slackware' %}

[local-part-one]: {{ site.url }}/system/2017/08/20/docker-slackware-part-one.html
[local-part-two]: {{ site.url }}/system/2017/08/21/docker-slackware-part-two.html

[image-ss-install-slaptget]:      {{ asset_post }}/24-install-slaptget.png
[image-ss-slaptget-update]:       {{ asset_post }}/24-slaptget-update.png
[photo-ss-slaptget-update]:       https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNqu4dS9aQFxUQhNUZYBaynXouzDmhv2vjmO1ms?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ss-slaptget-upgrade-half]: {{ asset_post }}/24-slaptget-upgrade-half.png
[photo-ss-slaptget-upgrade-full]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMC8TaVP2h4_9ptQHL0pGZ1axhn9GtdQ_9BuYP7?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-ss-slaptget-htop]:         {{ asset_post }}/24-slaptget-install-htop.png
[image-ss-slaptget-show-ncdu]:    {{ asset_post }}/24-slaptget-show-ncdu.png
[image-ss-slaptget-ncdu]:         {{ asset_post }}/24-slaptget-install-ncdu.png
[image-ss-slaptget-slaptsrc]:     {{ asset_post }}/25-slaptget-install-slaptsrc.png
[image-ss-slaptsrc-update]:       {{ asset_post }}/25-slaptsrc-update.png
[image-ss-slaptsrc-ncdu-half]:    {{ asset_post }}/25-slaptsrc-install-ncdu-half.png
[photo-ss-slaptsrc-ncdu-full]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMQeESVsylBl5qU3t9C-l6fy0BsdAwezYvjhTjq?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-slackpkg-python]:  {{ asset_post }}/27-slackpkg-install-python.png
[image-ss-slpkg-update-err]: {{ asset_post }}/27-slpkg-update-certificate-error.png
[image-ss-slackpkg-ca-cert]: {{ asset_post }}/27-slackpkg-install-ca-certificates.png
[image-ss-slpkg-update]:     {{ asset_post }}/27-slpkg-update.png

[image-ss-slpkg-repo-list]:  {{ asset_post }}/28-slpkg-repo-list.png
[image-ss-slpkg-etc-repo]:   {{ asset_post }}/28-slpkg-etc-repositories.png
[image-ss-slpkg-sbo-fish]:   {{ asset_post }}/28-slpkg-sbo-fish-half.png
[photo-ss-slpkg-sbo-fish]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP8DnPxKw_sjsW2jlR_LAjIBJiP2BcMjsDmHSyv?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

