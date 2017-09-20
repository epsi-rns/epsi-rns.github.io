---
layout: post
title: "Docker - openSUSE Zypper - Part Three"
date: 2017-08-17 13:15:35 +0700
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

openSUSE put it in <code class="code-file">/etc/zypp/repos.d/</code>.
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

Always check the fine manual, like usual.

{% highlight bash %}
$ zypper help modifyrepo
...
  Command options:
-d, --disable             Disable the repository (but don't remove it).
-e, --enable              Enable a disabled repository.
-r, --refresh             Enable auto-refresh of the repository.
-R, --no-refresh          Disable auto-refresh of the repository.
-n, --name <name>         Set a descriptive name for the repository.
-p, --priority <integer>  Set priority of the repository.
-k, --keep-packages       Enable RPM files caching.
-K, --no-keep-packages    Disable RPM files caching.
...
-a, --all                 Apply changes to all repositories.
-l, --local               Apply changes to all local repositories.
-t, --remote              Apply changes to all remote repositories.
-m, --medium-type <type>  Apply changes to repositories of specified type.
{% endhighlight %}

I like to change to change the repo directly, especially the long name,
so I do not need to have long column in my terminal.
And also I disable this newly added repo.

{% highlight bash %}
$ cat /etc/zypp/repos.d/X11_windowmanagers.repo 
[X11_windowmanagers]
name=Window Managers
enabled=0
autorefresh=0
{% endhighlight %}

![Docker Nano: Window Managers][image-ss-nano-repo]{: .img-responsive }

Consider zypper repos again.

{% highlight bash %}
$ zypper repos
Repository priorities are without effect. All enabled repositories share the same priority.

# | Alias              | Name            | Enabled | GPG Check | Refresh
--+--------------------+-----------------+---------+-----------+--------
1 | X11_windowmanagers | Window Managers | No      | ----      | ----   
2 | non-oss            | NON-OSS         | Yes     | (r ) Yes  | Yes    
3 | oss                | OSS             | Yes     | (r ) Yes  | Yes  
{% endhighlight %}

![Docker Zypper: Repos Newly Added][image-ss-zypper-repos-newly]{: .img-responsive }

Note that <code>X11_windowmanagers</code> has <code>1</code> index.
Now we can enable it again using this command

{% highlight bash %}
$ zypper mr -e 1
Repository 'X11_windowmanagers' has been successfully enabled.
{% endhighlight %}

Or using name, to make a clearer mandate.

{% highlight bash %}
$ zypper mr -e X11_windowmanagers
Nothing to change for repository 'X11_windowmanagers'.
{% endhighlight %}

![Docker Zypper: mr enable][image-ss-zypper-mr-enable]{: .img-responsive }

And **multiple** command at once. Raise priority.

{% highlight bash %}
$ zypper mr -r -k -p 70 X11_windowmanagers      
Autorefresh has been enabled for repository 'X11_windowmanagers'.
RPM files caching has been enabled for repository 'X11_windowmanagers'.
Repository 'X11_windowmanagers' priority has been set to 70.
{% endhighlight %}

![Docker Zypper: mr multiple][image-ss-zypper-mr-multiple]{: .img-responsive }

Or **all remote** repository at once.

{% highlight bash %}
$ zypper mr -Kt
RPM files caching has been disabled for repository 'X11_windowmanagers'.
Nothing to change for repository 'non-oss'.
Nothing to change for repository 'oss'.
{% endhighlight %}

![Docker Zypper: mr remotes][image-ss-zypper-mr-remotes]{: .img-responsive }

#### Service List

There are other related command as well.
I found this, about a month after this article written.

{% highlight bash %}
$ zypper service-list
# | Alias              | Name            | Enabled | GPG Check | Refresh | Type  
--+--------------------+-----------------+---------+-----------+---------+-------
1 | X11_windowmanagers | Window Managers | No      | ----      | ----    | rpm-md
2 | non-oss            | non-oss         | Yes     | (r ) Yes  | No      | yast2 
3 | oss                | oss             | Yes     | (r ) Yes  | No      | yast2 
4 | oss-jp             | oss-jp          | No      | ----      | ----    | yast2 
{% endhighlight %}

![Docker Zypper: service list][image-ss-service-list]{: .img-responsive }

#### Additional

There are also additional repository contain specific package,
i.e driver and multimedia codec provided by packman.

*	<https://en.opensuse.org/Additional_package_repositories>

Consider enable packman repository.

{% highlight bash %}
$ sudo zypper ar -f -n packman http://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Tumbleweed/ packman
Adding repository 'packman' ..................................[done]
Repository 'packman' successfully added

URI         : http://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Tumbleweed/
Enabled     : Yes                                                                
GPG Check   : Yes                                                                
Autorefresh : Yes                                                                
Priority    : 99 (default priority)                                              

Repository priorities in effect:    (See 'zypper lr -P' for details)
      70 (raised priority)  :  1 repository  
      99 (default priority) :  3 repositories
{% endhighlight %}

![Docker Zypper: Additional Packman][image-ss-zypper-additional]{: .img-responsive }

Sometimes packman cannot be reach.
For this docker, I would rather remove it.

#### Mirror

Sometimes you need to switch mirror,
whether just to find nearest mirror such as your campus,
or because the main repository is has been down for a few hours.
Unfortunately I can not find good reference on how to switch mirror in openSUSE.

All you need to do is go check the site,
and make sure you check the column.

*	<https://mirrors.opensuse.org/>

{% highlight bash %}
$ zypper ar http://ftp.riken.jp/Linux/opensuse/tumbleweed/repo/oss/ oss-jp
Adding repository 'oss-jp' ...................................[done]
Repository 'oss-jp' successfully added

URI         : http://ftp.riken.jp/Linux/opensuse/tumbleweed/repo/oss/
Enabled     : Yes                                                    
GPG Check   : Yes                                                    
Autorefresh : No                                                     
Priority    : 99 (default priority)                                  

Repository priorities are without effect. All enabled repositories share the same priority.
{% endhighlight %}

{% highlight bash %}
$ zypper ref oss-jp
Retrieving repository 'oss-jp' metadata ......................[done]
Building repository 'oss-jp' cache ...........................[done]
Specified repositories have been refreshed.
{% endhighlight %}

![Docker openSUSE: Mirror][image-ss-opensuse-mirror]{: .img-responsive }

After this you may safely disable main repository.

{% highlight bash %}
$ zypper mr -d oss   
Repository 'oss' has been successfully disabled.
{% endhighlight %}

Switching mirror has never been easier.

{% highlight bash %}
$ zypper mr -d oss-jp
Repository 'oss-jp' has been successfully disabled.

$ zypper mr -e oss   
Repository 'oss' has been successfully enabled.
{% endhighlight %}

-- -- --

### What's Next

We have not finished yet. There is still Build from Source.
Consider finish reading [ [Part Four][local-part-four] ].

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-opensuse' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-four]: {{ site.url }}/system/2017/08/18/docker-opensuse-zypper.html

[image-ss-zypper-addrepo]:		{{ asset_post }}/16-addrepo-windowmanagers.png
[image-ss-etc-zypp-repos]:		{{ asset_post }}/16-etc-zypp-repos-d.png
[image-ss-etc-zypp-repos-oss]:	{{ asset_post }}/16-etc-zypp-repos-d-oss-repo.png
[image-ss-zypper-help-repos]:	{{ asset_post }}/16-help-repos.png
[image-ss-zypper-install-hl]:	{{ asset_post }}/16-install-herbstluftwm.png
[image-ss-zypper-refresh]:		{{ asset_post }}/16-refresh.png
[image-ss-zypper-refresh-oss]:	{{ asset_post }}/16-refresh-f-oss.png
[image-ss-zypper-refresh-wm]:	{{ asset_post }}/16-refresh-windowmanager.png
[image-ss-zypper-removerepo]:	{{ asset_post }}/16-removerepo-windowmanagers.png
[image-ss-zypper-repos-pen]:	{{ asset_post }}/16-repos-lr-pen.png
[image-ss-zypper-mr-enable]:	{{ asset_post }}/16-mr-enable.png
[image-ss-zypper-mr-multiple]:	{{ asset_post }}/16-mr-multiple-commands.png
[image-ss-zypper-mr-remotes]:	{{ asset_post }}/16-mr-all-remotes.png
[image-ss-nano-repo]:			{{ asset_post }}/16-nano-windowmanagers-repo.png
[image-ss-zypper-repos-newly]:	{{ asset_post }}/16-repos-modified.png
[image-ss-zypper-additional]:	{{ asset_post }}/16-repo-additional.png
[image-ss-opensuse-mirror]:		{{ asset_post }}/16-mirror-jp.png
[image-ss-service-list]:		{{ asset_post }}/16-service-list.png
