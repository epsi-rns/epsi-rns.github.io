---
layout: post
title: "Distribution - Fedora First Time Install"
date: 2017-08-02 09:45:15 +0700
categories: system
tags: [fedora, distro, package manager]
author: epsi

excerpt:
  Fedora Install Log.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
  - 17080945  # Package Manager Diversity
  - 17080845  # Debian Devuan Migration
  - 17080745  # Manjaro Artix Migration
  - 17080645  # Mageia 6 Upgrade
  - 17080545  # GRUB2 BTRFS Support
  - 17080345  # openSUSE Tumbleweed Install
  - 17080145  # Manjaro OpenRC Issues

---

### Preface

	I was successfully login, after Fedora 23 install, using USB Live XFCE4.

I'm a linux hobbyist.
I have been using Debian since 2007,
and since 2013 I also use Arch Linux.
I also tried both derivatives such as:
Ubuntu, Kali, Manjaro, and BlackArch.
I also use Mageia at home.

This July, without a plan, I suddenly upgrade my knowledge.
I did these

*	installed Fedora, 

*	installed OpenSUSE (BTRFS+XFS),

*	installed KaOSX (XFS),

*	and later migrate my Debian (Systemd) to Devuan (OpenRC),

*	and migrate my Manjaro (OpenRC) to pure OpenRC distribution named Artix.

[![Fullscreen Fedora Core 6 Screenshot][image-ss-fedora-gnome]{: .img-responsive }][photo-ss-fedora-gnome]

I've been enjoying playing with Fedora Core installation.
The process begun, since friday night (TGF), until sunday morning.

And it looks good too.

-- -- --

### Overview

I realize I installed the previous FC23 instead of FC26.

	I never had any experience with Fedora.
	I don't even have any Hat. Or thinking of using Boot.

I did what everybody else does, updating my newly installed to latest version. 
I have done many issues, and by this experience, I desire to share the knowledge.

Direct clean installation to FC26 crossed my mind, but I decide to solve these issuse. 
As a n00b, I need some experience, to comprehend how rpm works. 
I better learn it now, than confused in busy hours.

Also unsolved issue haunting me at night. 
I desire not to be a failure person, but that is just me. 
Sorry for my english, I'm not native english speaker.

> Goal: System Upgrade from FC23 to FC26 using dnf

So this is the chapter, article skeleton.

1.	Using DNF for the first time

2.	From FC23 to FC24

3.	From FC24 to FC25

4.	From FC25 to FC26

5.	Post Install.

With this experience, I realize that I do not understand RPM philosophy yet. 
It is a different package managment compared with APT and ALPM. 
Takes time to comprehend.

-- -- --

### Detail Steps

I rebuilt steps from "Станислав Нижниченко",
and makes things more complete (or complicated).

For each FC upgrade, this is the step.

(1)	Check

{% highlight bash %}
$ cat /etc/fedora-release 
$ uname - a
$ dnf repolist
{% endhighlight %}

(2)	Pre Upgrade

{% highlight bash %}
$ sudo dnf install dnf-plugin-system-upgrade
{% endhighlight %}

(3)	Upgrade

{% highlight bash %}
$ sudo dnf system-upgrade download --releasever=24
$ sudo dnf system-upgrade reboot
{% endhighlight %}

(4)	Check Again (Step 1)

(5)	Post Upgrade

{% highlight bash %}
$ sudo dnf clean all
$ sudo dnf upgrade -- refresh
$ sudo reboot
{% endhighlight %}

(6)	Clean-up Legacy Packages

This steps require manual removing.

{% highlight bash %}
$ sudo dnf repoquery --unsatisfied
$ sudo dnf repoquery --duplicated
{% endhighlight %}

(7)	Clean-up More

Some packages might stay on your system while they have been removed from the repositories.

{% highlight bash %}
$ dnf list extras
{% endhighlight %}

And update

{% highlight bash %}
$ sudo dnf update
{% endhighlight %}

If needed

{% highlight bash %}
$ sudo dnf autoremove
{% endhighlight %}

(8)	Update grub if needed

{% highlight bash %}
$ rpm -q kernel
$ sudo dnf remove kernel-core- ...
{% endhighlight %}

{% highlight bash %}
$ ls /boot | grep linuz
$ sudo grub2-mkconfig -o /boot/grub2/grub.cfg
{% endhighlight %}

-- -- --

### First Time

As a Fedora n00b, First I read the manual.

{% highlight bash %}
$ man dnf
$ sudo dnf install firefox
{% endhighlight %}

It turn out that I need <code>RPMFusion</code>

{% highlight bash %}
$ sudo rpm -Uvh http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
$ sudo rpm -Uvh http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
$ sudo rpm -V rpmfusion-free-release
{% endhighlight %}

#### under rpm fusion

{% highlight bash %}
$ sudo dnf install ffmpeg-libs
$ sudo dnf install vlc
{% endhighlight %}

-- -- --

### FC23 to FC24

>	Issue 1: Kernel not upgraded to /boot.

It appear that I need a <code>system-uprade</code>  plugin

{% highlight bash %}
$ sudo dnf install dnf-plugin-system-upgrade

$ sudo dnf system-upgrade download --refresh --releasever=24
$ sudo dnf system-upgrade download --releasever=24
$ sudo dnf system-upgrade reboot

$ cat /etc/fedora-release 
Fedora release 24 (Twenty Four)
{% endhighlight %}

It looks easy.
But I was wrong.

#### Still FC23

Why is that, I still get FC23 instead of FC24 ?
It seems like I'm still using FC23 the whole time.

{% highlight bash %}
$ sudo dnf clean all
77 files removed
{% endhighlight %}

{% highlight bash %}
$ sudo dnf upgrade --refresh
Fedora 23 - x86_64 - Updates                      586 kB/s |  25 MB     00:44    
RPM Fusion for Fedora 23 - Free                   448 kB/s | 457 kB     00:01    
RPM Fusion for Fedora 23 - Free - Updates         347 kB/s | 371 kB     00:01    
RPM Fusion for Fedora 23 - Nonfree - Updates      110 kB/s |  74 kB     00:00    
RPM Fusion for Fedora 23 - Nonfree                 34 kB/s | 156 kB     00:04    
Fedora 23 - x86_64                                640 kB/s |  43 MB     01:08    
Last metadata expiration check: 0:01:09 ago on Sat Jul 22 19:01:15 2017.

Nothing to do.
Complete!
{% endhighlight %}

{% highlight bash %}
$ dnf repoquery --unsatisfied
Last metadata expiration check: 0:03:53 ago on Sat Jul 22 19:01:15 2017.
{% endhighlight %}

{% highlight bash %}
$ dnf repoquery --duplicated
Last metadata expiration check: 0:04:10 ago on Sat Jul 22 19:01:15 2017.
dnf-0:1.1.10-1.fc23.noarch
dnf-0:1.1.10-4.fc24.noarch
dnf-conf-0:1.1.10-1.fc23.noarch
dnf-conf-0:1.1.10-4.fc24.noarch
...
{% endhighlight %}

{% highlight bash %}
$ sudo dnf remove dnf-0:1.1.10-1.fc23.noarch dnf-conf-0:1.1.10-1.fc23.noarch dnf-langpacks-conf-0:0.15.1-1.fc23.noarch
No match for argument: dnf-langpacks-conf-0:0.15.1-1.fc23.noarch
Dependencies resolved.
Error: The operation would result in removing the following protected packages: dnf.
{% endhighlight %}

Bump !

#### Grub Still FC23

After upgrade attempt to 24 succeed,
I realize that the kernel in /boot is still FC23.
No FC24 in /boot.

grub2-mkconfig change the grub2 title to (Twenty Four),
but the kernel is still FC23.

[![DNF inconsistency with /boot][image-ss-dnf-kernel]{: .img-responsive }][photo-ss-dnf-kernel]

And this steps below won't works.

{% highlight bash %}
$ sudo rpm -q kernel
kernel-4.11.10-100.fc24.x86_64
kernel-4.2.3-300.fc23.x86_64
{% endhighlight %}

{% highlight bash %}
$ sudo dnf remove kernel-4.2.3
...
Remove  1 Package

...
Removed:
  kernel.x86_64 4.2.3-300.fc23                                                                                                                                          

Complete!
{% endhighlight %}

{% highlight bash %}
$ sudo rpm -q kernel
kernel-4.11.10-100.fc24.x86_64
{% endhighlight %}

{% highlight bash %}
$ sudo dnf install kernel
Last metadata expiration check: 1:37:10 ago on Sat Jul 22 12:37:46 2017.
Package kernel-4.11.10-100.fc24.x86_64 is already installed, skipping.
Package kernel-core-4.11.10-100.fc24.x86_64 is already installed, skipping.
Package kernel-core-4.2.3-300.fc23.x86_64 is already installed, skipping.
Dependencies resolved.
Nothing to do.
Complete!
{% endhighlight %}

{% highlight bash %}
$ ls /boot | grep linuz
vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
vmlinuz-4.2.3-300.fc23.x86_64
{% endhighlight %}

{% highlight bash %}
$ sudo grub2-mkconfig -o /boot/grub2/grub.cfg
Generating grub configuration file ...
Found linux image: /boot/vmlinuz-4.2.3-300.fc23.x86_64
Found initrd image: /boot/initramfs-4.2.3-300.fc23.x86_64.img
Found linux image: /boot/vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
Found initrd image: /boot/initramfs-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e.img
Found Windows 7 (loader) on /dev/sda1
Found Debian GNU/Linux (9.0) on /dev/sda7
done
{% endhighlight %}

Even worse. 
I can't upgrade from FC24 to FC25

{% highlight bash %}
$ sudo dnf system-upgrade reboot
{% endhighlight %}

System Upgrade won't do any upgrade at boot.
It is just reboot, but no upgrade.

#### Finally a Workaround

Manually installing FC24's kernel does the trick.
And I do not know why.

{% highlight bash %}
$ wget -c https: kojipkgs.fedoraproject.org//packages/kernel/4.11.10/100.fc24/x86_64/kernel-core-4.11.10-100.fc24.x86_64.rpm

$ sudo rpm --reinstall kernel core-4.11.10-100.fc24.x86_64.rpm

$ rpm -q kernel
kernel-4.11.10-100.fc24.x86_64

$ ls /boot | grep linuz
vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
vmlinuz-4.11.10-100.fc24.x86_64
vmlinuz-4.2.3-300.fc23.x86_64
{% endhighlight %}

	Now I can upgrade from FC24 to FC25

-- -- --

### FC24 to FC25

>	Issue 2: DNF system Upgrade Segmentation Fault.

This is what's going on after kernel switch from FC23 to FC24. 
Any DNF command, after <code>system-upgrade</code> command
will result freeing read locks.

{% highlight bash %}
$ sudo dnf system-upgrade download -- releasever=25
[SKIPPED] ...
[SKIPPED] ...
[SKIPPED] youtube-dl-2017.07.09-1.fc25.noarch.rpm: Already downloaded                                                                                                  
Running transaction check
Transaction check succeeded.
Running transaction test
Segmentation fault (core dumped)
{% endhighlight %}

{% highlight bash %}
$ sudo dnf update
BDB2053 Freeing read locks for locker 0x2828: 2152/140417528944384
...
BDB2053 Freeing read locks for locker 0x283b: 2152/140417528944384
Last metadata expiration check: 0:23:30 ago on Sat Jul 22 11:07:21 2017.
...
{% endhighlight %}

{% highlight bash %}
$ sudo dnf update
Last metadata expiration check: 0:36:40 ago on Sat Jul 22 11:07:21 2017.
Dependencies resolved.
...
{% endhighlight %}

[![DNF segmentation fault][image-ss-dnf-core-dump]{: .img-responsive }][photo-ss-dnf-core-dump]

#### Clean Won't Help

Using this, still the same error message "Segmentation fault"

{% highlight bash %}
$ sudo dnf clean all
$ sudo dnf system-upgrade download --releasever=25
...
Segmentation fault (core dumped)
{% endhighlight %}

#### Workaround

This tip from stackoverflow does the trick.

*	<https:  stackoverflow.com/questions/43709845/dnf-crashes-with-segmentation-fault>

{% highlight bash %}
$ sudo db_verify /var/lib/rpm/
$ sudo rm /var/lib/rpm/__db *
$ sudo rpm --rebuilddb
{% endhighlight %}

I finally succeed doing system upgrade from FC24 to FC25.
And here is my log, the result after boot.

#### After Boot Check

{% highlight bash %}
$ cat /etc/fedora-release 
Fedora release 25 (Twenty Five)
{% endhighlight %}

{% highlight bash %}
$ uname -a
Linux utama 4.11.10-200.fc25.x86_64 #1 SMP Wed Jul 12 19:04:52 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux
{% endhighlight %}

{% highlight bash %}
$ dnf repolist
Fedora 25 - x86_64                                814 kB/s |  50 MB     01:03    
RPM Fusion for Fedora 25 - Nonfree - Updates      143 kB/s |  40 kB     00:00    
RPM Fusion for Fedora 25 - Free                   464 kB/s | 515 kB     00:01    
Fedora 25 - x86_64 - Updates                      510 kB/s |  24 MB     00:48    
RPM Fusion for Fedora 25 - Free - Updates         266 kB/s | 290 kB     00:01    
RPM Fusion for Fedora 25 - Nonfree                233 kB/s | 144 kB     00:00    
Last metadata expiration check: 0:00:01 ago on Sat Jul 22 21:47:51 2017.
repo id                      repo name                                      status
*fedora                      Fedora 25 - x86_64                             51,669
*rpmfusion-free              RPM Fusion for Fedora 25 - Free                   541
*rpmfusion-free-updates      RPM Fusion for Fedora 25 - Free - Updates         224
*rpmfusion-nonfree           RPM Fusion for Fedora 25 - Nonfree                169
*rpmfusion-nonfree-updates   RPM Fusion for Fedora 25 - Nonfree - Updates       55
*updates                     Fedora 25 - x86_64 - Updates                   20,743
{% endhighlight %}

#### Package Clean Up

{% highlight bash %}
$ sudo dnf clean all
[sudo] password for epsi: 
45 files removed
{% endhighlight %}

{% highlight bash %}
$ sudo dnf upgrade --refresh
RPM Fusion for Fedora 25 - Free                   448 kB/s | 515 kB     00:01    
RPM Fusion for Fedora 25 - Nonfree                340 kB/s | 144 kB     00:00    
Fedora 25 - x86_64                                528 kB/s |  50 MB     01:37    
RPM Fusion for Fedora 25 - Free - Updates         371 kB/s | 290 kB     00:00    
Fedora 25 - x86_64 - Updates                      675 kB/s |  24 MB     00:36    
RPM Fusion for Fedora 25 - Nonfree - Updates      7.7 kB/s |  40 kB     00:05    
Dependencies resolved.
Nothing to do.
Complete!
{% endhighlight %}

{% highlight bash %}
$ dnf repoquery --unsatisfied
Last metadata expiration check: 0:13:36 ago on Sat Jul 22 21:47:51 2017.

$ dnf repoquery --duplicated
Last metadata expiration check: 0:14:32 ago on Sat Jul 22 21:47:51 2017.
ustr-0:1.0.4-18.fc22.x86_64
ustr-0:1.0.4-21.fc24.x86_64

$ sudo dnf remove ustr-0:1.0.4-18.fc22.x86_64
[sudo] password for epsi: 
...
Removed:
  ustr.x86_64 1.0.4-18.fc22                                                       
{% endhighlight %}

Complete!

### Kernel Clean Up

{% highlight bash %}
$ ls /boot | grep linuz
vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
vmlinuz-4.11.10-100.fc24.x86_64
vmlinuz-4.11.10-200.fc25.x86_64
vmlinuz-4.2.3-300.fc23.x86_64
{% endhighlight %}

{% highlight bash %}
$ sudo dnf remove kernel-core-4.2.3-300.fc23.x86_64
$ sudo dnf remove kernel-core-4.11.10-100.fc24.x86_64
{% endhighlight %}

{% highlight bash %}
$ dnf list extras
Last metadata expiration check: 0:26:08 ago on Sat Jul 22 21:47:51 2017.
Extra Packages
libsilc.x86_64                         1.1.10-14.fc23                      @System
pam_pkcs11.x86_64                      0.6.8-8.fc24                        @System
yumex.noarch                           3.0.17-2.fc23                       @System
{% endhighlight %}

{% highlight bash %}
$ ls /boot | grep linuz
vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
vmlinuz-4.11.10-200.fc25.x86_64
{% endhighlight %}

{% highlight bash %}
$ sudo grub2-mkconfig -o /boot/grub2/grub.cfg
Generating grub configuration file ...
Found linux image: /boot/vmlinuz-4.11.10-200.fc25.x86_64
Found initrd image: /boot/initramfs-4.11.10-200.fc25.x86_64.img
Found linux image: /boot/vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
Found initrd image: /boot/initramfs-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e.img
Found Windows 7 on /dev/sda1
Found Debian GNU/Linux 9 (stretch) on /dev/sda7
done
{% endhighlight %}

#### Update

{% highlight bash %}
$ sudo dnf update
Last metadata expiration check: 0:24:27 ago on Sat Jul 22 21:53:00 2017.
Dependencies resolved.
Nothing to do.
Complete!
{% endhighlight %}

I think we are good.

-- -- --

### FC25 to FC26

This step should be easier after first and second upgrade.
And the upgrade work flawlessly.

[![Fullscreen Fedora Core 6 DNF Update][image-ss-fedora-dnf]{: .img-responsive }][photo-ss-fedora-dnf]

{% highlight bash %}
$ sudo dnf system-upgrade download --releasever=26
$ sudo dnf system-upgrade reboot
{% endhighlight %}

{% highlight bash %}
$ cat /etc/fedora-release 
Fedora release 26 (Twenty Six)

$ uname -a
Linux utama 4.11.10-300.fc26.x86_64 #1 SMP Wed Jul 12 17:05:39 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux

$ dnf repolist
Fedora 26 - x86_64 - Updates                      269 kB/s | 6.6 MB     00:24    
Fedora 26 - x86_64                                208 kB/s |  53 MB     04:22    
RPM Fusion for Fedora 26 - Free - Updates          28 kB/s |  56 kB     00:01    
RPM Fusion for Fedora 26 - Free                   182 kB/s | 519 kB     00:02    
RPM Fusion for Fedora 26 - Nonfree - Updates       27 kB/s | 3.0 kB     00:00    
RPM Fusion for Fedora 26 - Nonfree                296 kB/s | 158 kB     00:00    
Last metadata expiration check: 0:00:00 ago on Sun 23 Jul 2017 02:17:47 AM WIB.
repo id                      repo name                                      status
*fedora                      Fedora 26 - x86_64                             53,912
*rpmfusion-free              RPM Fusion for Fedora 26 - Free                   536
*rpmfusion-free-updates      RPM Fusion for Fedora 26 - Free - Updates          33
*rpmfusion-nonfree           RPM Fusion for Fedora 26 - Nonfree                202
*rpmfusion-nonfree-updates   RPM Fusion for Fedora 26 - Nonfree - Updates        3
*updates                     Fedora 26 - x86_64 - Updates                    3,865
{% endhighlight %}

{% highlight bash %}
$ sudo dnf clean all
[sudo] password for epsi: 
79 files removed

$ sudo dnf upgrade --refresh
Fedora 26 - x86_64 - Updates                      336 kB/s | 6.6 MB     00:20    
Fedora 26 - x86_64                                182 kB/s |  53 MB     05:00    
RPM Fusion for Fedora 26 - Free - Updates          43 kB/s |  56 kB     00:01    
RPM Fusion for Fedora 26 - Free                   135 kB/s | 519 kB     00:03    
RPM Fusion for Fedora 26 - Nonfree - Updates       13 kB/s | 3.0 kB     00:00    
RPM Fusion for Fedora 26 - Nonfree                119 kB/s | 158 kB     00:01    
Last metadata expiration check: 0:00:00 ago on Sun 23 Jul 2017 02:27:12 AM WIB.
Error: No packages marked for upgrade.

$ sudo reboot
{% endhighlight %}

{% highlight bash %}
$ sudo dnf repoquery --unsatisfied
Last metadata expiration check: 0:03:53 ago on Sun 23 Jul 2017 02:27:12 AM WIB.

$ sudo dnf repoquery --duplicated
Last metadata expiration check: 0:07:41 ago on Sun 23 Jul 2017 02:27:12 AM WIB.
{% endhighlight %}

{% highlight bash %}
$ rpm -q kernel
kernel-4.11.10-200.fc25.x86_64
kernel-4.11.10-300.fc26.x86_64

$ sudo dnf remove kernel-core-4.11.10-200.fc25.x86_64
...
Removed:
  kernel-core.x86_64 4.11.10-200.fc25                                             
  kernel-modules.x86_64 4.11.10-200.fc25                                          
  kernel-modules-extra.x86_64 4.11.10-200.fc25  
{% endhighlight %}

{% highlight bash %}
$ dnf list extras
Last metadata expiration check: 0:22:20 ago on Sun 23 Jul 2017 02:17:47 AM WIB.
Extra Packages
clucene09-core.x86_64                  0.9.21b-16.fc24               @System      
compat-gnutls28.x86_64                 3.3.21-1.fc24                 @System      
hawkey.x86_64                          0.6.4-3.fc25                  @@commandline
libsilc.x86_64                         1.1.10-14.fc23                @System      
pam_pkcs11.x86_64                      0.6.8-8.fc24                  @System      
rodent-icon-theme.noarch               5.0-8.fc24                    @System      
yumex.noarch                           3.0.17-2.fc23                 @System      
{% endhighlight %}

{% highlight bash %}
$ sudo dnf update
Last metadata expiration check: 0:13:44 ago on Sun 23 Jul 2017 02:27:12 AM WIB.
Dependencies resolved.
Nothing to do.
Complete!
{% endhighlight %}

{% highlight bash %}
$ ls /boot | grep linuz
vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
vmlinuz-4.11.10-300.fc26.x86_64
 
$ sudo grub2-mkconfig -o /boot/grub2/grub.cfg
Generating grub configuration file ...
Found linux image: /boot/vmlinuz-4.11.10-300.fc26.x86_64
Found initrd image: /boot/initramfs-4.11.10-300.fc26.x86_64.img
Found linux image: /boot/vmlinuz-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e
Found initrd image: /boot/initramfs-0-rescue-d903a5c6e0a947ccb45d1f9d8d22516e.img
Found Windows 7 on /dev/sda1
Found Debian GNU/Linux 9 (stretch) on /dev/sda7
done
{% endhighlight %}

Works like a charm.

-- -- --

### Post Install

#### Specific App for Fedora

YUM Extender (Package Manager GUI)

{% highlight bash %}
$ sudo dnf install yumex-dnf
{% endhighlight %}

#### Some App I need to live with.

{% highlight bash %}
$ sudo dnf install htop
$ sudo dnf install fish
$ sudo dnf install powerline
$ sudo dnf install git
$ sudo dnf install rfkill
$ sudo dnf install sddm
$ sudo dnf install inkscape
{% endhighlight %}

#### Some troubleshooting

{% highlight bash %}
$ youtube-dl --extract-audio --audio-format mp3 ...
...
[download] 100% ...
ERROR: ffprobe or avprobe not found. Please install one.

$ dnf search ffmpeg
{% endhighlight %}

And the solution is using <code>rpmfusion-free </code>

{% highlight bash %}
$ sudo dnf install ffmpeg
$ sudo dnf install mplayer
{% endhighlight %}

#### Playgroup (Playing with Group)

	I know, this will bloat my system. I'm just curious.

{% highlight bash %}
$ dnf grouplist
{% endhighlight %}

[![Fedora DNF Group List][image-ss-dnf-grouplist]{: .img-responsive }][photo-ss-dnf-grouplist]

{% highlight bash %}
$ dnf group info "Fedora Workstation"
$ sudo dnf group install "Fedora Workstation"
$ sudo dnf group install "KDE Plasma Workspaces"
$ dnf group info "Window Managers"
{% endhighlight %}

[![Bloating my PC][image-ss-dnf-group-install]{: .img-responsive }][photo-ss-dnf-group-install]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

### DNF Update

	First Thing First

Since we focus to command line package manager,
I would like to show what DNF do, for anyone who never seen DNF.
This is is what happened after about two weeks without update.

{% highlight bash %}
$ sudo dnf update
{% endhighlight %}

[![DNF Update: Package ][image-ss-dnf-update-1]{: .img-responsive }][photo-ss-dnf-update-1]

Downloading

[![DNF Update: Download][image-ss-dnf-update-2]{: .img-responsive }][photo-ss-dnf-update-2]

This time, DNF is upgrading instead of installing.

[![DNF Update: Upgrade ][image-ss-dnf-update-3]{: .img-responsive }][photo-ss-dnf-update-3]

#### Shorter Example

And you can update again to get shorter example.

[![DNF Update: Shorter Example][image-ss-dnf-update-7]{: .img-responsive }][photo-ss-dnf-update-7]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

### Conclusion

I finally happy using Fedora.
Better late than never.

Thank you for Reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}

[image-ss-dnf-kernel]:    {{ asset_path }}/fedora-dnf-kernel.png
[photo-ss-dnf-kernel]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOnDoyhw0GtzrtPijGBIoAhIDFI9nkJaifhS_7a?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-core-dump]: {{ asset_path }}/fedora-dnf-core-dump.png
[photo-ss-dnf-core-dump]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNLisEUnOre57ZHVYTm0MYJX2u29PKfLEcO1ZTV?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-grouplist]: {{ asset_path }}/fedora-dnf-grouplist-half-gnome.png
[photo-ss-dnf-grouplist]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipO22ZLLQSZybd1Vipnzpi1XTJSiB5rYFy-I0kLY?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-group-install]: {{ asset_path }}/fedora-dnf-group-install.png
[photo-ss-dnf-group-install]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM8XVX0j0t3lwipCFP52jd-U9Gp0LDbaTEA5qu5?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-fedora-gnome]:  {{ asset_path }}/fedora-gnome.png
[photo-ss-fedora-gnome]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOvzXOlxlMKIYYNaoqBG704hrrHvG8eFjCby8uq?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-fedora-dnf]:    {{ asset_path }}/fedora-dnf-update-0-screen.png
[photo-ss-fedora-dnf]:    https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNjJm0AatbIGRj_bwW0Na8Cavf7CVRE1iQzo7ca?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-update-1]: {{ asset_path }}/fedora-dnf-update-1-half-gnome.png
[photo-ss-dnf-update-1]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPBVBFV9qi45AS5cnUqTy9ZOk7vC4kUFbjz9a3G?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-update-2]: {{ asset_path }}/fedora-dnf-update-2-half-gnome.png
[photo-ss-dnf-update-2]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPndh9-ayozMIzTDPjVFwv4FkZ_5r0KIMP9NkMl?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-update-3]: {{ asset_path }}/fedora-dnf-update-3-half-gnome.png
[photo-ss-dnf-update-3]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOe7bMNS54Pn4Y9JcocrX20eXLbh4dw6Kz3iKlm?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-ss-dnf-update-7]: {{ asset_path }}/fedora-dnf-update-7-gnome.png
[photo-ss-dnf-update-7]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMZHKmxD4JNJMDHRIf1bGqZsohv3XnJT8Z9SqzF?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
