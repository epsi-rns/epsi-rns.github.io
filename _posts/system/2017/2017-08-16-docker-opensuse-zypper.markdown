---
layout: post
title: "Docker - openSUSE Zypper - Part Two"
date: 2017-08-16 13:35:15 +0700
categories: system
tags: [docker, distro, package manager, opensuse]
author: epsi

excerpt:
  Examine zypper step by step,
  using openSUSE container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17082035  # Slackware Package
  - 17081845  # Fedora DNF
# - 17081535  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081435  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

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

And **multiple** command at once.

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

-- -- --

### What's Next

We have not finished yet.
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
[image-ss-zypper-mr-enable]:   {{ asset_post }}/16-mr-enable.png
[image-ss-zypper-mr-multiple]: {{ asset_post }}/16-mr-multiple-commands.png
[image-ss-zypper-mr-remotes]:  {{ asset_post }}/16-mr-all-remotes.png
[image-ss-nano-repo]:          {{ asset_post }}/16-nano-windowmanagers-repo.png
[image-ss-zypper-repos-newly]: {{ asset_post }}/16-repos-modified.png



