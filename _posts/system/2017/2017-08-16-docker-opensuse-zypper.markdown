---
layout: post
title: "Docker - openSUSE Zypper - Part Two"
date: 2017-08-16 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, opensuse]
author: epsi

excerpt:
  Examine zypper step by step,
  using openSUSE container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
  - 17081815  # Fedora DNF
# - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-opensuse-zypper.html %}

-- -- --

### Dependency

There are two main topics in package dependency,
the _dependency_ itself, and _reverse dependency_.
Beside these two, there are other topic as well,
such as _managing conflict_ that we do not cover here.

#### Help

Zypper has a very nice help that show all dependency related options.

{% highlight bash %}
$ zypper help search
search (se) [options] [querystring]
...
    --provides             Search for packages which provide the search strings.
    --recommends           Search for packages which recommend the search strings.
    --requires             Search for packages which require the search strings.
    --suggests             Search for packages which suggest the search strings.
    --conflicts            Search packages conflicting with search strings.
    --obsoletes            Search for packages which obsolete the search strings.
...
{% endhighlight %}

{% highlight bash %}
$ zypper help info
info (if) [options] <name> ...
...
    --provides            Show provides.
    --requires            Show requires and prerequires.
    --conflicts           Show conflicts.
    --obsoletes           Show obsoletes.
    --recommends          Show recommends.
    --suggests            Show suggests.
{% endhighlight %}

![Docker Zypper: Help Info][image-ss-zypper-help-info]{: .img-responsive }

#### Dependency

	Package that required by: such as man need less and other.

This _dependency_ information can be achieved by <code>info</code> command.
This will show required parts of the package.

{% highlight bash %}
$ zypper info --requires man
...
Description    :                                   
    A program for displaying man pages on the screen or sending them to a
    printer (using groff).
Requires       : [32]                              
...
    cron
    glibc-locale
    libgdbm.so.4()(64bit)
    less
{% endhighlight %}

![Docker Zypper: Info Require][image-ss-zypper-if-require]{: .img-responsive }

#### Reverse Dependency

	Package that require: such as less needed by man or other.

This _reverse dependency_ require <code>search</code> command.

{% highlight bash %}
$ zypper search --requires --match-exact less
Loading repository data...
Reading installed packages...

S  | Name                 | Summary                                | Type   
---+----------------------+----------------------------------------+--------
   | calc                 | C-style arbitrary precision calculator | package
   | git-core             | Core git tools                         | package
   | lftp                 | Command Line File Transfer Program     | package
i+ | man                  | A Program for Displaying man Pages     | package
   | nmh                  | Unix Mail Handler                      | package
   | patterns-caasp-Stack | openSUSE Kubic Stack                   | package
   | quilt                | A Tool for Working with Many Patches   | package
{% endhighlight %}

![Docker Zypper: Search Require][image-ss-zypper-se-require]{: .img-responsive }

#### Test

Removing <code>less</code> would remove <code>man</code>.
And also remove <code>fish</code>,
because <code>fish</code> depend on <code>man</code>.

{% highlight bash %}
$ zypper rm less
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following 3 packages are going to be REMOVED:
  fish less man

3 packages to remove.
After the operation, 9.4 MiB will be freed.
Continue? [y/n/...? shows all options] (y):
{% endhighlight %}

![Docker Zypper: Test Dependency][image-ss-zypper-test-remove]{: .img-responsive }

#### Verify

Zypper has a tool, to install or repair, missing dependencies.

{% highlight bash %}
$ zypper verify
{% endhighlight %}

Equal to:

{% highlight bash %}
$ zypper ve
Loading repository data...
Reading installed packages...

The following 3 NEW packages are going to be installed:
  dracut pinentry systemd-sysvinit

3 new packages to install.
Overall download size: 792.0 KiB. Already cached: 0 B. After the operation, additional 1.3 MiB will be
used.
Some of the dependencies of installed packages are broken. In order to fix these dependencies, the following actions need to be taken:
Continue? [y/n/...? shows all options] (y): y
{% endhighlight %}

![Docker Zypper: Verify Dependency][image-ss-zypper-verify]{: .img-responsive }

-- -- --

### Group

I cannot find any reference about group in Zypper.
Although there is group concept in YaST.

#### Metapackage

Neither metapackage exist in Zypper.

#### Pattern

The closest concept about group in zypper is,
by using <code>pattern</code> package.

{% highlight bash %}
$ zypper pt
Loading repository data...
Reading installed packages...
S | Name                 | Version       | Repository | Dependency
--+----------------------+---------------+------------+-----------
  | apparmor             | 20170319-10.2 | OSS        |           
  | apparmor             | 20170319-10.2 | OSS        |           
  | base                 | 20170319-10.2 | OSS        |           
  | base                 | 20170319-10.2 | OSS        |           
  | basesystem           | 20170319-10.2 | OSS        |           
  | basesystem           | 20170319-10.2 | OSS        |           
  | books                | 20170319-4.1  | OSS        |           
{% endhighlight %}

![Docker Zypper: Pattern][image-ss-zypper-pattern]{: .img-responsive }

-- -- --

### System Wide

System Wide Information

#### List Packages

There is this <code>zypper packages</code> command

{% highlight bash %}
$ zypper help packages
packages (pa) [options] [repository] ...

List all packages available in specified repositories.

  Command options:

-r, --repo <alias|#|URI>  Just another means to specify repository.
-i, --installed-only      Show only installed packages.
-u, --not-installed-only  Show only packages which are not installed.
    --orphaned            Show packages which are orphaned (without repository).
    --suggested           Show packages which are suggested.
    --recommended         Show packages which are recommended.
    --unneeded            Show packages which are unneeded.
-N, --sort-by-name        Sort the list by package name.
-R, --sort-by-repo        Sort the list by repository.
 {% endhighlight %}
 
 {% highlight bash %}
 $ zypper pa --unneeded
Loading repository data...
Reading installed packages...
S | Repository | Name               | Version    | Arch  
--+------------+--------------------+------------+-------
i | oss        | asciidoc           | 8.6.9-3.1  | noarch
i | oss        | glib2-devel        | 2.54.0-1.1 | x86_64
i | oss        | libsgutils2-1_43-2 | 1.43-3.1   | x86_64
i | oss        | libxslt-devel      | 1.1.29-6.1 | x86_64
i | oss        | openssl            | 1.0.2l-2.1 | noarch
i | oss        | p11-kit            | 0.23.2-2.5 | x86_64
i | oss        | p11-kit-tools      | 0.23.2-2.5 | x86_64
i | oss        | xorg-x11-devel     | 7.6-47.3   | noarch
 {% endhighlight %}
 
 {% highlight bash %}
$ zypper pa --orphaned
Loading repository data...
Reading installed packages...
S  | Repository | Name                  | Version          | Arch  
---+------------+-----------------------+------------------+-------
i+ | @System    | herbstluftwm          | 1335135043-3.280 | x86_64
i  | @System    | openSUSE-release-mini | 20170816-1.2     | x86_64
{% endhighlight %}

![Docker Zypper: Package Unneeded][image-ss-packages-unneeded]{: .img-responsive }

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.

{% highlight bash %}
$ less /var/log/zypp/history
# 2017-08-23 08:57:51 man-2.7.6-3.3.x86_64.rpm installed ok
# Additional rpm output:
# Updating /etc/sysconfig/cron ...
# 
2017-08-23 08:57:51|install|man|2.7.6-3.3|x86_64|root@d2e88a46e111|oss|21490bfff69e98449f8ae00bb9e91b15038566ca|
2017-08-23 10:01:09|command|root@d2e88a46e111|'zypper' 'install' '--force' 'man' 'nano' 'htop' 'ncdu' 'fish'|
2017-08-23 10:01:12|install|htop|2.0.2-3.4|x86_64|root@d2e88a46e111|oss|2807afd80fa606228799ab76baddfc2e688d60b8|
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/zypp/history][image-ss-less-log]{: .img-responsive }

-- -- --

### Clean Up

Opensuse as default does not keep downloaded package,
unless <code>keeppackages=1</code>
sets in <code class="code-file">/etc/zypp/repos.d/</code>.
But sometimes cache files left for some reason.

Package Cache
	
*	/var/cache/zypp/packages/ * /x86_64/ * .x86_64.rpm
	
*	/var/cache/zypp/packages/ * /suse/noarch/ * .noarch.rpm

{% highlight bash %}
$ ls -lR /var/cache/zypp/packages/
{% endhighlight %}

![Docker Zypper: Cache][image-ss-zypper-cache]{: .img-responsive }

You can clean these directory.

{% highlight bash %}
$ zypper clean
{% endhighlight %}

![Docker Zypper: Clean][image-ss-zypper-clean]{: .img-responsive }

-- -- --

### What's Next

Repository deserve their own article.
Consider finish reading [ [Part Three][local-part-three] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-opensuse' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-three]: {{ site.url }}/system/2017/08/17/docker-opensuse-zypper.html

[image-ss-zypper-help-info]:   {{ asset_post }}/14-help-info.png
[image-ss-zypper-if-require]:  {{ asset_post }}/14-info-requires.png
[image-ss-zypper-se-require]:  {{ asset_post }}/14-search-requires.png
[image-ss-zypper-test-remove]: {{ asset_post }}/14-rm-less.png
[image-ss-zypper-verify]:      {{ asset_post }}/14-verify.png

[image-ss-zypper-pattern]: {{ asset_post }}/15-pattern.png

[image-ss-zypper-cache]: {{ asset_post }}/17-cache.png
[image-ss-zypper-clean]: {{ asset_post }}/17-clean.png

[image-ss-less-log]:     {{ asset_post }}/19-log.png
[image-ss-packages-unneeded]:   {{ asset_post }}/19-packages-unneeded.png
