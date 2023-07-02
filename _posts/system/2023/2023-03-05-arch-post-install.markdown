---
layout    : post
title     : "Arch: Post Install"
categories: system
date      : 2023-04-05 09:25:15 +0700
tags      : [install]
keywords  : [vanilla arch, lenovo, post install]
author: epsi
toc        : toc/2023/05/toc-install.html

excerpt:
  Minimal arch install configuration.

opengraph:
  image: /assets/posts/system/2023/04/014-lsinitcpio.png

---

### Preface

> Goal: Minimal arch install configuration.

The first thing to do after boot is of course login in TTY.
After first boot and first login,
our duty is not complete yet.
We still need to setup user administration.
Then we scan setup GUI as soon as possible,
so we can easily do more basic administration.

<a name="toc"></a>

#### Table of Content

* [Table of Content](#toc)
* [Networking](#networking)
* [User Administration](#user-admin)
* [GUI Display](#gui-display)
* [Basic Tools](#basic-tools)
* [What is Next?](#whats-next)

-- -- --

<a name="networking"></a>

### Networking

After login, my needs is to connect to internet,
so I can get required package to do basic administration.

Let's ping.
My expectation is failure.

{% highlight bash %}
❯# ping archlinux.org
Temporary failure in name resolution
❯# ping 8.8.8.8
Network is unreachable
{% endhighlight %}

#### Using iwd

If you are in a hurry, you can use `iwd`.
I do not use `iwd` for daily basis.

I provide, an article for `iwd` in this article series.

#### Network Manager

I use `NetworkManager` for daily basis.
What I do is simply this below:

{% highlight bash %}
❯# systemctl enable wpa_supplicant.service
❯# systemctl start wpa_supplicant.service
❯# systemctl enable NetworkManager.service
❯# systemctl start NetworkManager.service
❯# systemctl enable dhcpcd.service
❯# systemctl start dhcpcd.service
{% endhighlight %}

Networking deserve an article of its own.
So I won't give the detail here.
The detail of the command can be found in other article,
provided in this article series.

-- -- --

<a name="user-admin"></a>

### User Administration

#### Adding User

You can just make a user, and add this user in a `wheel` group.
For example this one will create user `rizqi` with group `rizqi`.

{% highlight bash %}
❯# useradd -m -G wheel rizqi
❯# passwd rizqi
{% endhighlight %}

Or you can make different name for the group.
For example this one will create user `epsi` in a group `users`.
This way you can omit to create a specific user group.

{% highlight bash %}
❯# useradd -m -g users -G wheel -s /bin/bash epsi
❯# passwd epsi
{% endhighlight %}

#### The Artefact

> passwd, group, shadow, gshadow

Now we can check the result

{% highlight bash %}
❯ cat /etc/passwd | grep "bash"
root:x:0:0::/root:/bin/bash
epsi:x:1000:984::/home/epsi:/bin/bash
rizqi:x:1001:1001::/home/rizqi:/bin/bash
{% endhighlight %}

![Arch Post Install: User Administration: passwd][021-usman-passwdb]

{% highlight bash %}
❯# cat /etc/group \
| grep "root\|epsi\|rizqi\|user"
root:x:0:brltty,root
wheel:x:998:epsi,rizqi
users:x:984:
rizqi:x:1001:
{% endhighlight %}

![Arch Post Install: User Administration: group][021-usman-group]

{% highlight bash %}
❯# cat /etc/shadow \
| grep "root\|epsi\|rizqi"
root:$ ... :19539::::::
epsi:$ ... :19511:0:99999:7:::
rizqi:$ ... :19517:0:99999:7:::
{% endhighlight %}

![Arch Post Install: User Administration: shadow][021-usman-shadow]

{% highlight bash %}
❯# cat /etc/gshadow \
| grep "root\|epsi\|rizqi\|user"
root:::brltty,root
wheel:!*::epsi,rizqi
users:!*::
rizqi:!::
{% endhighlight %}

![Arch Post Install: User Administration: gshadow][021-usman-gshadow]

#### Sudoers

We are not finished yet.
We still need to setup privilege for sudo.
This means who can do sudo,
and how the password asked.

{% highlight bash %}
❯# pacman -S sudo
❯# nano /etc/sudoers
{% endhighlight %}

Instead of setting per user,
you can set the `wheel` group.

{% highlight bash %}
❯# cat /etc/sudoers | grep -v "^#|^$"
root ALL=(ALL:ALL) ALL
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
@includedir /etc/sudoers.d
{% endhighlight %}

I grep all blank lines and commented lines.

![Arch Post Install: User Administration: sudoers][022-sudoers]

Die hard veteran would love `visudo`

{% highlight bash %}
❯# pacman -S vi visudo
❯# visudo
{% endhighlight %}

![Arch Post Install: User Administration: visudo][022-visudo]

Now try login in TTY for user that you made.

#### Change Shell

`bash` is not the only shell,
you can change to other shell.
First you have to install the shell.

{% highlight bash %}
❯# pacman -S zsh fish
{% endhighlight %}

And change the shell.

{% highlight bash %}
❯ chsh -s /bin/zsh
Changing shell for epsi.
Password: 
Shell changed.
{% endhighlight %}

![Arch Post Install: Change Shell: chsh][021-chsh-s]

And finally, check the result in user administration.

{% highlight bash %}
❯# cat /etc/passwd \
| grep "bash\|zsh"
root:x:0:0::/root:/bin/bash
epsi:x:1000:984::/home/epsi:/bin/zsh
rizqi:x:1001:1001::/home/rizqi:/bin/bash
{% endhighlight %}

![Arch Post Install: User Administration: passwd][021-usman-passwdz]

-- -- --

<a name="display-gui"></a>

### GUI Display

> Setting up GUI with xorg/wayland

It is a good time to setup GUI.
With GUI we can configure stuff easier.
I would like to set up xorg first.
Then wayland later.

#### Display Server

> xorg

First the xorg itself

{% highlight bash %}
❯# pacman -S xorg-server xorg-xhost xorg-xwayland xorg-xinit 
{% endhighlight %}

I don't know if I could install wayland wthout xorg at all.

#### Desktop Manager

I like `lightdm`,
but unfortunately it doesn't work well with wayland.
So use `sddm` instead.

{% highlight bash %}
❯# pacman -S sddm
{% endhighlight %}

You can enable.

{% highlight bash %}
❯ sudo systemctl enable sddm
Created symlink /etc/systemd/system/display-manager.service → /usr/lib/systemd/system/sddm.service.
{% endhighlight %}

![Arch Post Install: Display: SDDM: enable][023-sddm-enable]

Then start from TTY.

{% highlight bash %}
❯ sudo systemctl start sddm
{% endhighlight %}

you need to relogin if you are already in window manager.

![Arch Post Install: Display: SDDM: start][023-sddm-start]

Or you can stop and disable, 
so you can learn how to run window manager,
directly from TTY.

{% highlight bash %}
❯ sudo systemctl disable sddm
Removed "/etc/systemd/system/display-manager.service".
{% endhighlight %}

![Arch Post Install: Display: SDDM: disable][023-sddm-disable]

#### Window Manager

Let us check the window manager

{% highlight bash %}
❯# pacman -S i3-wm sway
{% endhighlight %}

*i3 in xorg* with picom compositor.

![Arch Post Install: Display: i3wm in Xorg][023-i3wm]

*sway in wayland*

![Arch Post Install: Display: sway in Wayland][023-sway]

#### Desktop Manager

Beginner would love xfce4.

{% highlight bash %}
❯# pacman -S xfce4 xfce4-goodies
{% endhighlight %}

![Arch Post Install: Display: XFCE4][023-xfce4]

Personally, I prefer to use AwesomeWM.
But I know Awesome is not for everbidy especially beginner.

![Arch Post Install: Display: AwesomeWm][023-awesome]

I've been using this AwesomeWM in about four years.
Just simply because I do not have time,
to explore other goodies.

#### Screenshot

For the screenshot, I use this tools.
Because `wayland` require different screenshooter.

{% highlight bash %}
❯# pacman -S scrot grim swappy
{% endhighlight %}

#### Second Account

For some weird reason, I need two accounts.
And sometimes I need to run GUI for other account, without switching desktop.
this can be done by this.

{% highlight bash %}
❯ xhost local:
non-network local connections being added to access control list
{% endhighlight %}

Now I can navigate my alternate account,
using any file manager.

{% highlight bash %}
❯ sudo -u rizqi caja &!
{% endhighlight %}

-- -- --

<a name="basic-tools"></a>

### Basic Tools

#### Additional Packages

I put my basic tools here,
I need the `rankmirror` available in `pacman-contrib`,
the `lsusb` available in `usbutils`,
the `git` and `base-devel` to build `yay`.
And at last `man` to read the manual.
just in case I have an issue and require troubleshooting.

{% highlight bash %}
❯# pacman -S pacman-contrib tree
❯# pacman -S usbutils
❯# pacman -S git base-devel
❯# pacman -S man-db
{% endhighlight %}

I should rmind myself to visit,
the manual of the `man` command regularly.

{% highlight bash %}
❯# man man
{% endhighlight %}

![Arch Post Install: man man][022-man-man]

That is all.

#### AUR

> Arch User Repository

I choose `yay` to manage AUR.
And just find out that there is already,
this easy to use `yay-bin`.
First we have to clone the git source.

{% highlight bash %}
❯ git clone https://aur.archlinux.org/yay-bin.git
Cloning into 'yay-bin'...
remote: Enumerating objects: 414, done.
remote: Counting objects: 100% (414/414), done.
remote: Compressing objects: 100% (274/274), done.
remote: Total 414 (delta 138), reused 408 (delta 138), pack-reused 0
Receiving objects: 100% (414/414), 90.93 KiB | 221.00 KiB/s, done.
Resolving deltas: 100% (138/138), done.
{% endhighlight %}

![Arch Post Install: AUR: yay-bin: git clone][026-git-yay-bin]

Then `makepkg`.
This is going to be a long message.

{% highlight bash %}
$ cd yay-bin
$ makepkg -si
{% endhighlight %}

![Arch Post Install: AUR: yay-bin: makepkg][026-makepkg-si-01]

![Arch Post Install: AUR: yay-bin: makepkg][026-makepkg-si-02]

Then the `yay` can just run.

{% highlight bash %}
❯ yay
:: Synchronizing package databases...
 core                  134.0 KiB  65.9 KiB/s 00:02
 extra                   8.3 MiB   761 KiB/s 00:11
:: Searching AUR for updates...
:: Searching databases for updates...
{% endhighlight %}

![Arch Post Install: AUR: yay][026-yay-sync]

Well, I love to see compilation in my terminal, so much.

-- -- --

<a name="whats-next"></a>

### What is Next 🤔?

After basic administration and GUI,
we can go back setting our arch linux,
to work coexist with pre existing partition,
in a multiboot situation.

Consider continue reading [ [Arch: Multiboot][local-whats-next] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2023/04' %}

[local-whats-next]: /system/2023/04/07/arch-multiboot.html

[021-usman-group]:  {{ asset_path }}/021-usman-group.png
[021-usman-gshadow]:{{ asset_path }}/021-usman-gshadow.png
[021-usman-passwdz]:{{ asset_path }}/021-usman-passwd-zsh.png
[021-usman-passwdb]:{{ asset_path }}/021-usman-passwd-bash.png
[021-usman-shadow]: {{ asset_path }}/021-usman-shadow.png
[021-chsh-s]:       {{ asset_path }}/021-chsh-s.png

[022-sudoers]:      {{ asset_path }}/022-sudoers.png
[022-visudo]:       {{ asset_path }}/022-visudo.png
[022-man-man]:      {{ asset_path }}/022-man-man.png

[023-awesome]:      {{ asset_path }}/023-awesome.png
[023-i3wm]:         {{ asset_path }}/023-i3wm.png
[023-sway]:         {{ asset_path }}/023-sway.png
[023-xfce4]:        {{ asset_path }}/023-xfce4.png

[023-sddm-disable]: {{ asset_path }}/023-sddm-disable.png
[023-sddm-enable]:  {{ asset_path }}/023-sddm-enable.png
[023-sddm-start]:   {{ asset_path }}/023-sddm-start.png

[026-git-yay-bin]:  {{ asset_path }}/026-git-yay-bin.png
[026-makepkg-si-01]:{{ asset_path }}/026-makepkg-si-01.png
[026-makepkg-si-02]:{{ asset_path }}/026-makepkg-si-02.png
[026-yay-sync]:     {{ asset_path }}/026-yay-sync.png
