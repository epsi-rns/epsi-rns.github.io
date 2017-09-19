---
layout: post
title: "Docker - Arch ALPM - Part Four"
date: 2017-08-30 09:35:15 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Four Parts Article.

related_link_ids: 
  - 17083145  # Docker Summary
  - 17083015  # LFS Build
# - 17082715  # Arch ALPM
  - 17082415  # Debian Portage
  - 17082115  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage
  - 17081015  # Crux Ports

---

{% include post/2017/08/topics-docker.html %}

{% include post/2017/08/toc-docker-arch-alpm.html %}

-- -- --

### AUR Helper

Consider make our live easier with AUR Helper.
Reading official documentation is a must.

*	https://wiki.archlinux.org/index.php/AUR_helpers

#### yaourt

I also wrote an article once.
With some configuration explained.

*	[Install Yaourt, the AUR Helper][local-install-yaourt]

Now consider cower again.

{% highlight bash %}
$ cd ~

$ cower -d -c yaourt
:: yaourt downloaded to /home/epsi

$ cd yaourt

$ makepkg -i
makepkg -i
==> Making package: yaourt 1.9-1 (Sat Sep 16 04:20:38 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading yaourt-1.9.tar.gz...
...
{% endhighlight %}

![Docker AUR: Yaourt: Install][image-ss-yaourt-install]{: .img-responsive }

Always RTFM

{% highlight bash %}
$ man yaourt
{% endhighlight %}

Consider use <code>yaourt</code> to query other AUR Helper.

{% highlight bash %}
$ yaourt pacaur
...
3 aur/pacaur 4.7.10-1 (1001) (52.44)
    An AUR helper that minimizes user interaction
...
==> Enter n° of packages to be installed (e.g., 1 2 3 or 1-3)
==> ----------------------------------------------------------
==> 3
{% endhighlight %}

![Docker AUR: Yaourt: Action][image-ss-yaourt-action]{: .img-responsive }

Pressing <code>3</code> and <code>Enter</code> will install <code>aur/pacaur</code>

Yaourt comes with config file.
Just copy them to your home directory

{% highlight bash %}
$ cat /etc/yaourtrc
$ cp /etc/yaourtrc ~/.yaourtrc
{% endhighlight %}

I left my <code class="code-file">~/.yauortrc</code> config as below to avoid
too many confirmation question from yaourt.

{% highlight conf %}
$ cat ~/.yauortrc
# SUDO
SUDONOVERIF=1      # Avoid multiple sudo checks when timestamp_timeout=0

# Prompt
BUILD_NOCONFIRM=1  # Only prompt for editing files
EDITFILES=0

# Command
MAKEPKG="makepkg --skippgpcheck"
{% endhighlight %}

![Docker AUR: Yaourtrc: ViM][image-ss-yaourtrc-vim]{: .img-responsive }

This configuration is very helpful
if you maintain your AUR upgrade regularly
with a lot of Syua option in yaourt.

This <code>yaourt -Syua</code> command will update all your AUR at once.
Make sure you know what you are doing when skipping PGP Verification.

{% highlight bash %}
$ yaourt -Syua
{% endhighlight %}

![Docker AUR: yaourt -Syua][image-ss-yaourt-syua]{: .img-responsive }

#### pacaur

Set editor as environment variable if necessary.

{% highlight bash %}
$ cat .profile
export EDITOR=vim

$ . .profile
{% endhighlight %}

Consider use <code>pacaur</code> to install <code>asp</code>.

{% highlight bash %}
$ pacaur -S asp   
[sudo] password for epsi: 
resolving dependencies...
looking for conflicting packages...

Packages (3) jq-1.5-5  oniguruma-6.6.1-1  asp-2-1

Total Download Size:   0.35 MiB
Total Installed Size:  1.50 MiB

:: Proceed with installation? [Y/n] y
:: Retrieving packages...
 asp-2-1-any               10.3 KiB   103K/s 00:00 [##########] 100%
 oniguruma-6.6.1-1-x...   154.3 KiB   359K/s 00:00 [##########] 100%
 jq-1.5-5-x86_64          191.8 KiB   291K/s 00:01 [##########] 100%
(3/3) checking keys in keyring                     [##########] 100%
(3/3) checking package integrity                   [##########] 100%
(3/3) loading package files                        [##########] 100%
(3/3) checking for file conflicts                  [##########] 100%
:: Processing package changes...
(1/3) installing oniguruma                         [##########] 100%
(2/3) installing jq                                [##########] 100%
(3/3) installing asp                               [##########] 100%
{% endhighlight %}

This behave like <code>pacman</code>.
On most cases <code>pacaur</code> goes well.

![Docker AUR: pacaur -S asp][image-ss-pacaur-asp]{: .img-responsive }

Consider try another.
Use <code>pacaur</code> to query AUR Helper.

{% highlight bash %}
$ pacaur -S packer
:: Package packer not found in repositories, trying AUR...
:: resolving dependencies...
:: looking for inter-conflicts...
:: packer-20150808-1 has been flagged out of date on Sun Apr  2 10:26:23 2017

AUR Packages  (1) packer-20150808-1  
Repo Packages (2) jansson-2.10-2  jshon-20131105-1  

Repo Download Size:   0.05 MiB
Repo Installed Size:  0.19 MiB

:: Proceed with installation? [Y/n] 
...
==> Cleaning up...
:: Installing packer package(s)...
:: packer package(s) failed to install.
:: ensure package version does not mismatch between .SRCINFO and PKGBUILD
:: ensure package name has a VCS suffix if this is a devel package
:: jshon is now an orphan package
{% endhighlight %}

![Docker AUR: pacaur -S packer][image-ss-pacaur-packer]{: .img-responsive }

But oouch, sometimes an issue happened such as _failed to install_.
This time we need a lower level method.

{% highlight bash %}
$ cower -d packer
:: packer downloaded to /home/epsi

$ cd packer/

$ makepkg
==> Making package: packer 20150808-1 (Sat Sep 16 05:21:21 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Cloning packer git repo...

$ sudo pacman -U packer-20160325-1-any.pkg.tar.xz 
...
:: Processing package changes...
(1/1) installing packer                            [##########] 100%
...
{% endhighlight %}

#### packer

Consider use <code>packer</code> to query other AUR Helper.

{% highlight bash %}
$ packer -S aura-bin
packer -S aura-bin

Aur Targets    (1): aura-bin

Proceed with installation? [Y/n] y
Edit aura-bin PKGBUILD with $EDITOR? [Y/n] n
==> Making package: aura-bin 1.3.9-1 (Sat Sep 16 05:28:01 UTC 2017)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading aura-1.3.9-x86_64.tar.gz...
...
==> Checking for packaging issue...
==> Creating package "aura-bin"...
  -> Generating .PKGINFO file...
  -> Generating .BUILDINFO file...
  -> Generating .MTREE file...
  -> Compressing package...
==> Leaving fakeroot environment.
==> Finished making: aura-bin 1.3.9-1 (Sat Sep 16 05:28:47 UTC 2017)
{% endhighlight %}

![Docker AUR: packer -S aura-bin][image-ss-packer-aura-bin]{: .img-responsive }

I like to have aliases in <code>.bashrc</code>
so I do not have to remember all the options.
This is an unoffocial tip for unofficial package.

{% highlight bash %}
alias pksyu="packer -Syu --noconfirm --noedit"
{% endhighlight %}

![Docker AUR: alias packer][image-ss-packer-alias]{: .img-responsive }

{% highlight bash %}
$ pksyu
[sudo] password for epsi: 
:: Synchronizing package databases...
 core is up to date
 extra is up to date
 community is up to date
:: Starting full system upgrade...
 there is nothing to do
:: Synchronizing aur database...
 aur                                          6  6 [##########]100%
:: Starting full aur upgrade...
 local database is up to date
 {% endhighlight %}

![Docker AUR: pksyu][image-ss-pkyu]{: .img-responsive }

There are still some other AUR helpers, but I never use any of them.
Yet this article is already long. I have decided not to make it any longer.

-- -- --

### Screenshot

#### Query AUR

A special case for AUR,
there are so many way to Query AUR in one system. 

I Didn't Know What To Do Last Summer
I still don't know what to do today.
So I play with AUR instead.

	#ihavenolife

{% highlight bash %}
$ package-query - As gst
$ cower - s gst
$ yaourt - s gst
$ aura - As gst
$ packer - Ss gst -- auronly
$ pacaur - Ssa gst
{% endhighlight %}


#### Screenshot: Query AUR

OS: Arch Linux

*	Window Manager: HerbstluftWM

[![Arch: Query AUR][image-ss-arch-query-aur]{: .img-responsive }][photo-ss-arch-query-aur]

-- -- --

### Conclusion

After years of being user,
I realize that I do not now anything about ALPM.

	These are just preliminary knowledge about ALPM.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}

[local-install-yaourt]:	{{ site.url }}/system/2016/06/21/install-yaourt.html

[image-ss-yaourt-install]:	{{ asset_post }}/26-install-yaourt.png
[image-ss-yaourt-action]:	{{ asset_post }}/26-yaourt-pacaur.png
[image-ss-yaourtrc-vim]:	{{ asset_post }}/26-vim-yaourtrc.png
[image-ss-yaourt-syua]:		{{ asset_post }}/26-yaourt-syua.png
[image-ss-pacaur-asp]:		{{ asset_post }}/26-packer-asp.png
[image-ss-pacaur-packer]:	{{ asset_post }}/26-pacaur-packer.png
[image-ss-packer-aura-bin]:	{{ asset_post }}/26-packer-aura-bin.png
[image-ss-packer-alias]:	{{ asset_post }}/26-packer-alias.png
[image-ss-pkyu]:			{{ asset_post }}/26-pksyu.png

[image-ss-arch-query-aur]:	{{ asset_path }}/package-manager/arch-hlwm-aur-no-rofi.png
[photo-ss-arch-query-aur]:	https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNE2B9cj-nGL6eUIr08vN3MLe1h78NVPglm6LKW?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
