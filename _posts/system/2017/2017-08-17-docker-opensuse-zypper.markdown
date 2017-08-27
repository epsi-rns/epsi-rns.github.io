---
layout: post
title: "Docker - openSUSE Zypper - Part Two"
date: 2017-08-17 09:45:15 +0700
categories: system
tags: [docker, distro, package manager, opensuse]
author: epsi

excerpt:
  Examine zypper step by step,
  using openSUSE container in Docker
  One of Two Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution

---

### Topics

This is a two-parts article.
There are few topics here.

[ [Part One][local-part-one] ]

*	Preface: Test Bed

*	Getting Started With Docker

*	Issues on Minimal Install: No Reset

*	Package Management: ZYpp Frontend, Get Help, Zypper Shell

*	Updating System: OS Release, List Updates, Update, Upgrades, Process Being Used

*	Package IRSI: Install, Removal, Query Search, Show Info

*	History: The Log File

*	Dependency: Help, Dependency, Reverse Dependency, Test, Verify

*	Clean Up

*	Group: Pattern

*	What's Next

[ [Part Two][local-part-two] ]

*	Repositories: List, Add/Remove/, Modify

*	Interesting Issue: systemd Dependencies

*	Unsolved Issues on Minimal Install: No Manual

*	Conclusion

-- -- --

### Repository

Zypper has amazing <code>repository</code> commands.

#### List Repository

Like usual, zypper manual is more than enough.
I mean, always check the fine manual first.

{% highlight bash %}
$ zypper help repos
repos (lr) [options] [repo] ...

List all defined repositories.

  Command options:
-e, --export <FILE.repo>  Export all defined repositories as a single local .repo file.
-a, --alias               Show also repository alias.
-n, --name                Show also repository name.
-u, --uri                 Show also base URI of repositories.
-p, --priority            Show also repository priority.
-r, --refresh             Show also the autorefresh flag.
-d, --details             Show more information like URI, priority, type.
-s, --service             Show also alias of parent service.
-E, --show-enabled-only   Show enabled repos only.
-U, --sort-by-uri         Sort the list by URI.
-P, --sort-by-priority    Sort the list by repository priority.
-A, --sort-by-alias       Sort the list by alias.
-N, --sort-by-name        Sort the list by name.
{% endhighlight %}

![Docker Zypper: Help Repository][image-ss-zypper-help-repos]{: .img-responsive }

Consider getting started with this simple command.

{% highlight bash %}
$ zypper lr
{% endhighlight %}

or a more complex one.

{% highlight bash %}
$ zypper repos -p -E -N
# | Alias   | Name    | Enabled | GPG Check | Refresh | Priority
--+---------+---------+---------+-----------+---------+---------
1 | non-oss | NON-OSS | Yes     | (r ) Yes  | Yes     |   99    
2 | oss     | OSS     | Yes     | (r ) Yes  | Yes     |   99    
{% endhighlight %}

![Docker Zypper: Repository -p -E -N][image-ss-zypper-repos-pen]{: .img-responsive }

openSUSE put it in <code>/etc/zypp/repos.d/</code>.
Like most configuration in unix world, you can edit manualy.
 
{% highlight bash %}
$ ls /etc/zypp/repos.d/
non-oss.repo  oss.repo
{% endhighlight %}

![Docker Zypp: /etc/zypp/repos.d/][image-ss-etc-zypp-repos]{: .img-responsive }

And here is what each repository configuration.

{% highlight bash %}
$ cat /etc/zypp/repos.d/oss.repo 
[oss]
name=OSS
enabled=1
autorefresh=1
baseurl=http://download.opensuse.org/tumbleweed/repo/oss/
path=/
type=yast2
keeppackages=0
{% endhighlight %}

![Docker Zypp: /etc/zypp/repos.d/oss.repo][image-ss-etc-zypp-repos-oss]{: .img-responsive }

There is also <code>zypper refresh</code> command.

{% highlight bash %}
$ zypper ref    
Repository 'NON-OSS' is up to date.                                 
Repository 'OSS' is up to date.                                     
All repositories have been refreshed.
{% endhighlight %}

![Docker Zypper: Refresh][image-ss-zypper-refresh]{: .img-responsive }

<code>zypper refresh</code> support process for specific repository.

{% highlight bash %}
$ zypper ref -f oss
Forcing raw metadata refresh
Retrieving repository 'OSS' metadata .........................[done]
Forcing building of repository cache
Building repository 'OSS' cache ..............................[done]
Specified repositories have been refreshed.
{% endhighlight %}

![Docker Zypper: Refresh Specific][image-ss-zypper-refresh-oss]{: .img-responsive }

#### Add/ Remove

It has been a riddle for a SUSE's beginner like me,
thinking about how openSUSE handle my weird favorites package,
such as <code>herbstluftwm</code>.
In openSUSE we have to add <code>X11:windowmanagers</code> first,
using <code>zypper addrepo</code> command.

{% highlight bash %}
$ zypper ar http://download.opensuse.org/repositories/X11:windowmanagers/openSUSE_Tumbleweed/X11:windowmanagers.repo
Adding repository 'Various window managers (openSUSE_Tumbleweed)' ................................[done]
Repository 'Various window managers (openSUSE_Tumbleweed)' successfully added

URI         : http://download.opensuse.org/repositories/X11:/windowmanagers/openSUSE_Tumbleweed/
Enabled     : Yes                                                                               
GPG Check   : Yes                                                                               
Autorefresh : No                                                                                
Priority    : 99 (default priority)                                                             

Repository priorities are without effect. All enabled repositories share the same priority.
{% endhighlight %}

![Docker Zypper: Add Repository][image-ss-zypper-addrepo]{: .img-responsive }

And refresh.

{% highlight bash %}
$ zypper ref
Retrieving repository 'Various window managers (openSUSE_Tumbleweed)' metadata ...................[done]
Building repository 'Various window managers (openSUSE_Tumbleweed)' cache ........................[done]
Repository 'NON-OSS' is up to date.                                                                     
Repository 'OSS' is up to date.                                                                         
All repositories have been refreshed.
{% endhighlight %}

![Docker Zypper: Refresh Newly Added][image-ss-zypper-refresh-wm]{: .img-responsive }

So that we can install <code>herbstluftwm</code>.

{% highlight bash %}
$ zypper install herbstluftwm
Loading repository data...
Reading installed packages...
Resolving package dependencies...

The following 5 NEW packages are going to be installed:
  herbstluftwm libX11-6 libX11-data libXau6 libxcb1

5 new packages to install.
Overall download size: 866.3 KiB. Already cached: 0 B. After the
operation, additional 2.9 MiB will be used.
Continue? [y/n/...? shows all options] (y): y
{% endhighlight %}

![Docker Zypper: Install Herbstluftwm][image-ss-zypper-install-hl]{: .img-responsive }

If you want, you can remove the newly add repository.

{% highlight bash %}
$ zypper rr X11_windowmanagers
Removing repository 'Various window managers (openSUSE_Tumbleweed)' ..............................[done]
Repository 'Various window managers (openSUSE_Tumbleweed)' has been removed.
{% endhighlight %}

![Docker Zypper: Remove Repository][image-ss-zypper-removerepo]{: .img-responsive }


#### Modify


-- -- --

### Unsolved Issues on Minimal Install

This is my bad. Not openSUSE's fault.

#### No Manual

No manual in openSUSE Docker.
I have two others openSUSE full installation in PC,
and all manual works well.

{% highlight bash %}
$ man man
No manual entry for man

$ echo $MANPATH

$ cat /etc/manpath.config
{% endhighlight %}

I have tried to reinstall, but it doesn't work.

{% highlight bash %}
$ zypper in -f man man-pages man-pages-posix 
{% endhighlight %}

-- -- --

### Interesting Issue

#### systemd Dependencies

While solving the _no manual_ problem,
I encountered another issue.

{% highlight bash %}
$ zypper in man
{% endhighlight %}

This is somehow interesting,
manual pages in openSUSE depend on systemd.

![systemd dependency issue on openSUSE][image-ss-zypper-systemd]{: .img-responsive }

Why would a manual <code>man</code> need to depend to an init ?

{% highlight bash %}
$ zypper info --requires man
Requires : [32]
cron

$ zypper info --requires cron
Requires : [5]
cronie = 1.5.1-66.3

$ zypper info --requires cronie
Requires : [26]
systemd
{% endhighlight %}

-- -- --

### Conclusion

	These are just preliminary knowledge about Zypper.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-opensuse' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-one]: {{ site.url }}/system/2017/08/16/docker-opensuse-zypper.html
[local-part-two]: {{ site.url }}/system/2017/08/17/docker-opensuse-zypper.html

[local-docker-flow]: {{ site.url }}/system/2017/08/10/docker-distribution-flow.html

[image-ss-pull-opensuse]:   {{ asset_pull }}/opensuse-tumbleweed.png
[image-ss-opensuse-docker]: {{ asset_post }}/00-getting-started.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-zypper-shell]:    {{ asset_post }}/01-zypper-shell.png

[image-ss-zypper-lu]:  {{ asset_post }}/01-list-updates.png
[image-ss-zypper-up]:  {{ asset_post }}/01-update.png
[image-ss-zypper-dup]: {{ asset_post }}/01-distribution-upgrade.png

[image-ss-zypper-if]:  {{ asset_post }}/13-info-fish.png
[image-ss-zypper-in]:  {{ asset_post }}/13-install.png
[image-ss-zypper-rm]:  {{ asset_post }}/13-remove-systemd.png
[image-ss-zypper-se]:  {{ asset_post }}/13-search-fish.png

[image-ss-zypper-systemd]: {{ asset_post }}/13-install-man-systemd-issue.png

[image-ss-zypper-help-info]:   {{ asset_post }}/14-help-info.png
[image-ss-zypper-if-require]:  {{ asset_post }}/14-info-requires.png
[image-ss-zypper-se-require]:  {{ asset_post }}/14-search-requires.png
[image-ss-zypper-test-remove]: {{ asset_post }}/14-rm-less.png
[image-ss-zypper-verify]:      {{ asset_post }}/14-verify.png

[image-ss-zypper-pattern]:     {{ asset_post }}/15-pattern.png

[image-ss-zypper-addrepo]:     {{ asset_post }}/16-addrepo-windowmanagers.png
[image-ss-etc-zypp-repos]:     {{ asset_post }}/16-etc-zypp-repos-d.png
[image-ss-etc-zypp-repos-oss]: {{ asset_post }}/16-etc-zypp-repos-d-oss-repo.png
[image-ss-zypper-help-repos]:  {{ asset_post }}/16-help-repos.png
[image-ss-zypper-install-hl]:  {{ asset_post }}/16-install-herbstluftwm.png
[image-ss-zypper-refresh]:     {{ asset_post }}/16-refresh.png
[image-ss-zypper-refresh-oss]: {{ asset_post }}/16-refresh-f-oss.png
[image-ss-zypper-refresh-wm]:  {{ asset_post }}/16-refresh-windowmanager.png
[image-ss-zypper-removerepo]:  {{ asset_post }}/16-removerepo-windowmanagers.png
[image-ss-zypper-repos-pen]:   {{ asset_post }}/16-repos-lr-pen.png

[image-ss-zypper-cache]: {{ asset_post }}/17-cache.png
[image-ss-zypper-clean]: {{ asset_post }}/17-clean.png

[image-ss-less-log]:     {{ asset_post }}/19-log.png
