---
layout: post
title: "Docker - Fedora DNF - Part Three"
date      : 2017-08-20 09:45:15 +0700
categories: system
tags      : [docker, distro, package manager, fedora]
keywords  : [dnf]
author: epsi

opengraph:
  image: /assets/site/images/topics/docker.png

excerpt:
  Examine DNF step by step,
  using Fedora container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
  - 17082715  # Arch ALPM
  - 17082415  # Debian APT
  - 17082115  # Slackware Package
# - 17081815  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081315  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-fedora-dnf.html %}

-- -- --

### Plugin

One of the DNF feature that differ DNF from other package is Plugin.

#### List

Unfortunately I cannot any command to list DNF plugins.
DNF search also won't work either.
Luckily there are good documentation.

*	<http://dnf-plugins-core.readthedocs.io/en/latest/>

Or google if the plugin is not mentioned in above link.

*	<https://fedoraproject.org/wiki/DNF_system_upgrade>

#### Install 

Consider this example: <code>config-manager</code>

{% highlight bash %}
$ dnf install dnf-plugin-config-manager
Last metadata expiration check: 0:33:51 ago on Sat Sep  2 18:26:16 2017.
Dependencies resolved.
=============================================================================
 Package                      Arch       Version           Repository   Size
=============================================================================
Installing:
 dnf-plugins-core             noarch     2.1.3-2.fc27      rawhide      50 k
Installing dependencies:
 python3-dnf-plugins-core     noarch     2.1.3-2.fc27      rawhide     138 k

Transaction Summary
=============================================================================
Install  2 Packages

Total download size: 188 k
Installed size: 451 k
Is this ok [y/N]: 
{% endhighlight %}

![Docker DNF: Plugin: config-manager][image-ss-dnf-cm-install]{: .img-responsive }

#### Help

{% highlight bash %}
$ dnf help config-manager
{% endhighlight %}

![Docker DNF: Plugin: config-manager][image-ss-dnf-cm-help]{: .img-responsive }

#### Config Manager Example

Now we can enable a repository without the need to edit configuration manually.

{% highlight bash %}
$ dnf config-manager --set-enabled rawhide-source
{% endhighlight %}

![Docker DNF: Plugin: config-manager][image-ss-dnf-cm-set-enabled]{: .img-responsive }

-- -- --

### System Wide

System Wide Information

#### List Packages

Listing packages handled by package manager,
available in repository.

{% highlight bash %}
$ dnf list --available | less
Last metadata expiration check: 0:29:01 ago on Wed Sep 20 14:19:59 2017.
Available Packages
0ad.x86_64                               0.0.22-2.fc27                   rawhide
0ad-data.noarch                          0.0.22-1.fc27                   rawhide
0ad-debugsource.x86_64                   0.0.22-2.fc27                   rawhide
0install.x86_64                          2.12.1-1.fc27                   rawhide
0xFFFF.x86_64                            0.3.9-15.fc26                   rawhide
2048-cli.x86_64                          0.9.1-4.fc27                    rawhide
2048-cli-debugsource.x86_64              0.9.1-4.fc27                    rawhide
{% endhighlight %}

![Docker DNF: List Available][image-ss-list-available]{: .img-responsive }

Extra package. The documentation define extras as
_that is packages installed on the system that are not available in any known repository_ .

{% highlight bash %}
$ dnf list --extras
Last metadata expiration check: 0:44:25 ago on Wed Sep 20 14:19:59 2017.
Extra Packages
9wm.x86_64                           1.2-7.fc20             @System 
audit-libs.x86_64                    2.7.7-5.fc27           @System 
python3-asn1crypto.noarch            0.22.0-4.fc27          @rawhide
python3-rpm.x86_64                   4.14.0-0.rc1.2.fc28    @System 
rpm.x86_64                           4.14.0-0.rc1.2.fc28    @System 
rpm-build.x86_64                     4.14.0-0.rc1.2.fc28    @System 
rpm-build-libs.x86_64                4.14.0-0.rc1.2.fc28    @System 
rpm-libs.x86_64                      4.14.0-0.rc1.2.fc28    @System 
rpm-plugin-selinux.x86_64            4.14.0-0.rc1.2.fc28    @System 
rpm-plugin-systemd-inhibit.x86_64    4.14.0-0.rc1.2.fc28    @System 
shared-mime-info.x86_64              1.8-6.fc28             @System 
systemd.x86_64                       234-5.fc27             @System 
systemd-container.x86_64             234-5.fc27             @rawhide
systemd-libs.x86_64                  234-5.fc27             @System 
systemd-pam.x86_64                   234-5.fc27             @System 
vim-minimal.x86_64                   2:8.0.1097-1.fc28      @System 
{% endhighlight %}

![Docker DNF: List Extras][image-ss-list-extras]{: .img-responsive }

You can try other such as

{% highlight bash %}
$ dnf list recent 
{% endhighlight %}

For more information you can:

{% highlight bash %}
$ dnf help list | less
...
  --all                 show all packages (default)
  --available           show only available packages
  --installed           show only installed packages
  --extras              show only extras packages
  --updates             show only upgrades packages
  --upgrades            show only upgrades packages
  --autoremove          show only autoremove packages
  --recent              show only recently changed packages
...
{% endhighlight %}

-- -- --

### History

#### The Log File

This is most the forgotten part of package management,
although it is not uncommon to notice messages.
For that reason, I put the recorded event here, 
before discussing about any further feature.

{% highlight bash %}
$ less /var/log/dnf.log
2017-08-23T10:47:59Z INFO --- logging initialized ---
2017-08-23T10:47:59Z DDEBUG timer: config: 1381 ms
2017-08-23T10:47:59Z DEBUG DNF version: 2.5.1
2017-08-23T10:47:59Z DDEBUG Command: dnf repolist 
2017-08-23T10:47:59Z DDEBUG Installroot: /
2017-08-23T10:47:59Z DDEBUG Releasever: 27
2017-08-23T10:47:59Z DEBUG cachedir: /var/cache/dnf
2017-08-23T10:47:59Z DDEBUG Base command: repolist
2017-08-23T10:47:59Z DDEBUG Extra commands: ['repolist']
{% endhighlight %}

Most likely you want the tail, latest transaction,
at the bottom of the recorded event.

![Docker: /var/log/dnf.log][image-ss-var-log-dnf]{: .img-responsive }

#### DNF History

DNF has a very nice history feature.

{% highlight bash %}
$ dnf history
ID     | Command line             | Date and time    | Action(s)      | Altered
-------------------------------------------------------------------------------
    19 | update                   | 2017-09-18 13:10 | Update         |    1   
    18 | distro-sync              | 2017-09-17 19:15 | I, O, U        |   93 **
    17 | downgrade herbstluftwm   | 2017-09-17 18:49 | Downgrade      |    1   
    16 | -y install mock          | 2017-09-09 13:29 | Install        |   24   
    15 | install wget             | 2017-09-09 11:35 | Install        |    1  <
    14 | install gcc              | 2017-09-03 02:47 | Install        |    8 > 
    13 | install make             | 2017-09-03 02:43 | Install        |    1   
    12 | install glib2-devel libX | 2017-09-03 02:42 | Install        |   18   
    11 | install rpm-build        | 2017-09-03 02:30 | I, U           |   41   
    10 | install dnf-plugin-confi | 2017-09-02 19:02 | Install        |    2   
     9 | install rpmreaper        | 2017-08-26 16:07 | Install        |    1   
     8 | upgrade                  | 2017-08-24 14:48 | Update         |   15 **
     7 | reinstall man-db         | 2017-08-23 15:47 | Reinstall      |    1   
     6 | install man              | 2017-08-23 15:45 | Install        |    4   
     5 | remove less              | 2017-08-23 13:14 | Erase          |    4   
     4 | install man-db nano htop | 2017-08-23 12:56 | Install        |    5   
     3 | install man              | 2017-08-23 12:55 | Install        |    5   
     2 | upgrade --nogpgcheck     | 2017-08-23 11:31 | I, U           |  171 **
     1 |                          | 2017-07-11 11:29 | Install        |  170 EE
{% endhighlight %}

![Docker DNF: History][image-ss-dnf-history]{: .img-responsive }

**.**

Now we can <code>undo</code>.

{% highlight bash %}
$ dnf history undo 15
Last metadata expiration check: 0:06:49 ago on Wed Sep 20 14:19:59 2017.
Undoing transaction 15, from Sat Sep  9 11:35:43 2017
    Install wget-1.19.1-3.fc27.x86_64 @rawhide
====================================================================
 Package    Arch         Version               Repository      Size
====================================================================
Removing:
 wget       x86_64       1.19.1-3.fc27         @rawhide       2.8 M

Transaction Summary
====================================================================
Remove  1 Package

Freed space: 2.8 M
Is this ok [y/N]:
{% endhighlight %}

But not everything can be undone.

{% highlight bash %}
$ dnf history undo 19
Last metadata expiration check: 0:10:19 ago on Wed Sep 20 14:19:59 2017.
Undoing transaction 19, from Mon Sep 18 13:10:27 2017
    Upgraded ncdu-1.12-5.fc27.x86_64 @rawhide
    Upgrade       1.12-6.fc28.x86_64 @rawhide
No package ncdu-0:1.12-5.fc27.x86_64 available.
Error: An operation cannot be undone
{% endhighlight %}

Or <code>rollback</code>.

{% highlight bash %}
$ dnf history rollback 18
Last metadata expiration check: 0:10:31 ago on Wed Sep 20 14:19:59 2017.
Rollback to transaction 18, from Sun Sep 17 19:15:24 2017
  Undoing the following transactions: 19
    Upgraded ncdu-1.12-5.fc27.x86_64 @rawhide
    Upgrade       1.12-6.fc28.x86_64 @rawhide
No package ncdu-0:1.12-5.fc27.x86_64 available.
Error: A transaction cannot be undone
{% endhighlight %}

-- -- --

### Hold Package

DNF can hold package using versionlock plugin.
Or using <code>exclude</code> directive in configuration.

#### Example

Suppose you want to do system upgrade,
but you do not want to upgrade certain package.
There is a good reason for these,
such as keeping old driver,
because the latest has a issue or such reason.
Or maybe we want to keep our current beloved newly compiled package
that equipped with super duper specific configuration parameter optimization.

{% highlight bash %}
$  dnf upgrade
Last metadata expiration check: 0:22:55 ago on Sat Sep  9 11:49:43 2017.
Dependencies resolved.
====================================================================
 Package                  Arch   Version              Repository
                                                               Size
====================================================================
Upgrading:
 bash                     x86_64 4.4.12-11.fc28       rawhide 1.5 M
 coreutils                x86_64 8.28-1.fc28          rawhide 1.2 M
 coreutils-common         x86_64 8.28-1.fc28          rawhide 1.9 M
 cpp                      x86_64 7.2.1-1.fc28         rawhide 9.2 M
{% endhighlight %}

![Docker DNF: BASH Unlocked][image-ss-h-dnf-bash-unlocked]{: .img-responsive }

Consider BASH as our guinea pig locking example.

#### Add Lock

We can add lock easily using DNF.
By using <code>exclude</code> directive in configuration.

{% highlight bash %}
$ nano /etc/dnf/dnf.conf 
[main]
gpgcheck=1
installonly_limit=3
clean_requirements_on_remove=True
exclude=bash
{% endhighlight %}

![Docker DNF: Nano DNF Configuration][image-ss-h-nano-dnf-conf]{: .img-responsive }

Now bash will be ignored.

{% highlight bash %}
$ dnf upgrade
Last metadata expiration check: 0:40:43 ago on Sat Sep  9 11:49:43 2017.
Dependencies resolved.
====================================================================
 Package                  Arch   Version              Repository
                                                               Size
====================================================================
Upgrading:
 coreutils                x86_64 8.28-1.fc28          rawhide 1.2 M
 coreutils-common         x86_64 8.28-1.fc28          rawhide 1.9 M
 cpp                      x86_64 7.2.1-1.fc28         rawhide 9.2 M
 crypto-policies          noarch 20170823-1.git8d18c27.fc28
{% endhighlight %}

![Docker DNF: BASH Locked][image-ss-h-dnf-bash-locked]{: .img-responsive }
  
#### Remove Lock

You can unlocked package by commenting the exclude in dnf.conf

{% highlight bash %}
$ cat /etc/dnf/dnf.conf 
# exclude=bash
{% endhighlight %}

Example done successfully.
Guinea pig is alive.

-- -- --

### Clean Up

Time after time, your cache size may growing bigger and bigger.

Package Cache
	
*	/var/cache/dnf/ * /packages/ * .rpm

{% highlight bash %}
$ ls -lR /var/cache/dnf/
{% endhighlight %}

![Docker DNF: Cache][image-ss-dnf-cache]{: .img-responsive }

You can clean these directory.

{% highlight bash %}
$ dnf clean packages
18 files removed
{% endhighlight %}

{% highlight bash %}
$ dnf clean all
9 files removed
{% endhighlight %}

![Docker DNF: Clean][image-ss-dnf-clean]{: .img-responsive }

And also orphan package with no dependency.

{% highlight bash %}
$ dnf autoremove
{% endhighlight %}

-- -- --

### Miscellanous

#### Downgrade

We can downgrade easily with DNF.

{% highlight bash %}
$ dnf downgrade herbstluftwm
...
Running transaction
  Preparing        :                                            1/1 
  Downgrading      : herbstluftwm-0.6.2-8.fc27.x86_64           1/2 
  Erasing          : herbstluftwm-0.6.2-8.fc28.x86_64           2/2 
  Running scriptlet: herbstluftwm-0.6.2-8.fc28.x86_64           2/2 
  Verifying        : herbstluftwm-0.6.2-8.fc27.x86_64           1/2 
  Verifying        : herbstluftwm-0.6.2-8.fc28.x86_64           2/2 

Downgraded:
  herbstluftwm.x86_64 0.6.2-8.fc27                                  

Complete!
{% endhighlight %}

![Docker DNF: Downgrade][image-ss-m-downgrade]{: .img-responsive }

#### Distro Sync

There is also this command, that sync packages to your current version.
Downgrade if necessary. Useful in case of multiple repository.

{% highlight bash %}
$ dnf distro-sync
Last metadata expiration check: 0:13:34 ago on Sun Sep 17 18:38:02 2017.
Dependencies resolved.
====================================================================
 Package            Arch   Version                    Repository
                                                               Size
====================================================================
Upgrading:
 bash               x86_64 4.4.12-11.fc28             rawhide 1.5 M
 bc                 x86_64 1.07.1-4.fc28              rawhide 126 k
 binutils           x86_64 2.29-9.fc28                rawhide 5.9 M
...
Installing weak dependencies:
 btrfs-progs        x86_64 4.13-1.fc28                rawhide 762 k

Transaction Summary
====================================================================
Install   6 Packages
Upgrade  83 Packages

Total download size: 84 M
Is this ok [y/N]:
{% endhighlight %}

![Docker DNF: Downgrade][image-ss-m-dist-sync]{: .img-responsive }

-- -- --

### What's Next

These are this build from source topic.
Consider finish reading [ [Part Four][local-part-four] ].

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/system' %}
{% assign asset_post  = system_path | append: '/2017/08/docker-fedora' %}

[local-part-four]: {{ site.url }}/system/2017/08/21/docker-fedora-dnf.html

[image-ss-dnf-cm-set-enabled]:  {{ asset_post }}/18-cm-set-enabled.png
[image-ss-dnf-cm-help]:         {{ asset_post }}/18-help-cm.png
[image-ss-dnf-cm-install]:      {{ asset_post }}/18-plugin-install.png

[image-ss-dnf-cache]:     {{ asset_post }}/17-cache.png
[image-ss-dnf-clean]:     {{ asset_post }}/17-clean.png

[image-ss-var-log-dnf]:   {{ asset_post }}/19-log.png
[image-ss-dnf-history]:   {{ asset_post }}/19-history.png
[image-ss-list-available]:   {{ asset_post }}/19-list-available.png
[image-ss-list-extras]:   {{ asset_post }}/19-list-extras.png

[image-ss-h-dnf-bash-locked]:    {{ asset_post }}/27-hold-dnf-upgrade-bash-locked.png
[image-ss-h-dnf-bash-unlocked]:  {{ asset_post }}/27-hold-dnf-upgrade-bash-unlocked.png
[image-ss-h-nano-dnf-conf]:      {{ asset_post }}/27-hold-nano-dnf-conf.png

[image-ss-m-downgrade]:      {{ asset_post }}/29-downgrade.png
[image-ss-m-dist-sync]:      {{ asset_post }}/29-dist-sync.png
