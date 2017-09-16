---
layout: post
title: "Docker - Slackware Package - Part Three"
date: 2017-08-22 09:35:15 +0700
categories: system
tags: [docker, distro, package manager, slackware]
author: epsi

excerpt:
  Docker flow for Slackware Package Management,
  using slapt-get, slapt-src, and slpkg.
  First time using Slackware experience.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082415  # Arch ALPM
  - 17082215  # Debian Portage
# - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17082715  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-slackware-package.html %}

-- -- --

### Install slpkg

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

The name <code>slpkg</code> in Slackware,
remind me of <code>dpkg</code> in Debian.

We can manage repository very nicely using <code>slpkg</code>

#### Official Documentation

*	<https://github.com/dslackw/slpkg>

#### Update

	First Thing First

{% highlight bash %}
$ slpkg update 

Check and update repositories:

Check repository [slack] ... Done
Check repository [sbo] ... Done
{% endhighlight %}

![Docker slpkg: Update Succeed][image-ss-slpkg-update]{: .img-responsive }

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

#### The slpkg Log File

{% highlight bash %}
$ tail -n 15 /var/log/slpkg/sbo/build_logs/build_fish_log 
etc/fish/conf.d/
install/
install/doinst.sh
install/slack-desc

Slackware package /tmp/fish-2.6.0-x86_64-1_SBo.tgz created.
{% endhighlight %}

![Docker Slackware: slpkg build log][image-ss-slpkg-build-log]{: .img-responsive }

-- -- --

### Repository 

We can utilize <code>slpkg</code> to manage repository in Slackware

#### List

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

#### Configuration

You can edit directly to enable repository.

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

#### Enable Repository

Alternatively you can use interactive dialog to enable repository. 

{% highlight bash %}
$ slpkg -s sbo python2-pythondialog

$ slpkg repo-enable
+==============================================================================
| Enabled repositories:
+==============================================================================
| slacky, slack, sbo, alien
+==============================================================================
Total 4/17 repositories enabled.
{% endhighlight %}

![Docker slpkg: Repository Dialog][image-ss-slpkg-repo-enable]{: .img-responsive }

Do not forget to update as usual.

{% highlight bash %}
$ slpkg update
{% endhighlight %}

#### List Package

{% highlight bash %}
$ slpkg -l alien

BeautifulSoup-4.1.1-x86_64-1alien
NetworkManager-openconnect-1.2.4-x86_64-1alien
OpenAL-1.18.0-x86_64-1alien
SDL_sound-1.0.3-x86_64-2alien
...
openbox-3.6.1-x86_64-1alien
...
xvidcore-1.3.2-x86_64-1alien
zope.interface-4.1.0-x86_64-1alien
{% endhighlight %}

![Docker slpkg: Repository's Packages][image-ss-slpkg-list-package]{: .img-responsive }

#### Install Package

Now we can install openbox package from alien repository
but without openbox dependency.

{% highlight bash %}
$ slpkg -s alien openbox 
Reading package lists... Done
Resolving dependencies... Done

The following packages will be automatically installed or upgraded 
with new version:

+==============================================================================
| Package                 New Version        Arch    Build  Repos          Size
+==============================================================================
Installing:
  openbox                 3.6.1              x86_64  1      alien         432 K

Installing summary
===============================================================================
Total 1 package.
1 package will be installed, 0 will be upgraded and 0 will be reinstalled.
Need to get 432 Kb of archives.
After this process, 1.48 Mb of additional disk space will be used.

Would you like to continue [y/N]? y

[1/1][ Download ] --> openbox-3.6.1-x86_64-1alien.tgz
{% endhighlight %}

![Docker slpkg: repositories.conf][image-ss-slpkg-alien-example]{: .img-responsive }

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

[image-ss-slackpkg-python]:  {{ asset_post }}/31-slackpkg-install-python.png
[image-ss-slpkg-update-err]: {{ asset_post }}/31-slpkg-update-certificate-error.png
[image-ss-slackpkg-ca-cert]: {{ asset_post }}/31-slackpkg-install-ca-certificates.png
[image-ss-slpkg-update]:     {{ asset_post }}/31-slpkg-update.png

[image-ss-slpkg-sbo-fish]:   {{ asset_post }}/33-slpkg-sbo-fish-half.png
[image-ss-slpkg-build-log]:  {{ asset_post }}/34-slpkg-log.png

[photo-ss-slpkg-sbo-fish]:   https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipP8DnPxKw_sjsW2jlR_LAjIBJiP2BcMjsDmHSyv?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-slpkg-repo-list]:     {{ asset_post }}/35-slpkg-repo-list.png
[image-ss-slpkg-etc-repo]:      {{ asset_post }}/35-slpkg-etc-repositories.png
[image-ss-slpkg-repo-enable]:   {{ asset_post }}/35-slpkg-repo-enable.png
[image-ss-slpkg-list-package]:  {{ asset_post }}/35-slpkg-list-packages.png
[image-ss-slpkg-alien-example]: {{ asset_post }}/35-slpkg-alien-openbox.png
