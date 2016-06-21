---
layout: post
title:  "Debian Wheezy - Post Install Log"
date:   2014-01-02 12:46:15 +0700
categories: system
tags: [debian, package manager]
author: epsi

excerpt:
  Using terminal only. 
  What to do on first run and.... 
  What to do when dependencies happened.
  Using apt-get and using aptitude.

related_link_ids: 
  - 14031331  # Linux Multiboot
  - 14050934  # My Mageia Experiment
  - 14040246  # Arch Install

---

goal: upgrade with terminal only

What to do on first run and...<br/>
What to do when dependencies happened.

note: a n00b may consider synaptic that fix broken packages
but you may miss the fun of 'solving dependencies problem' :D

note: No need to install all. Pick only what you need.
<br/>
This article contain two parts.

* Using apt-get and,

* Using aptitude.

[![Debian Fresh Install][image-ss-debian-install]{: .img-responsive }][picasa-ss-debian-install]
<br/><br/>

-- -- -- 

## Using apt-get and aptitude

first step: using root, and setup admin's privileges

{% highlight bash %}
># su
Password: 

># visudo
{% endhighlight %}
-- -- --

this step: upgrade current system from custom repository

{% highlight bash %}
># cat /etc/apt/sources.list

deb http://kambing.ui.ac.id/debian jessie main contrib non-free
deb http://www.deb-multimedia.org jessie main non-free

># apt-get update
># apt-get install deb-multimedia-keyring
># apt-get upgrade
{% endhighlight %}

-- -- --

note: experiencing dependencies problem with gnome
relogin with gnome flashback, trying to fix with these command 

{% highlight bash %}
># apt-get -f install
Correcting dependencies... Done

># apt-get upgrade -f
># apt-get upgrade

># apt-get clean
># apt-get autoclean
># reboot
{% endhighlight %}

-- -- --

note: give up with apt-get, switch to aptitude to fix broken packages
meanwhile: go to regrigerator, try to find something to eat

{% highlight bash %}
># apt-get install task-gnome-desktop
The following packages have unmet dependencies:
 task-gnome-desktop : Depends: gnome-core but it is not going to be installed

> # aptitude install task-gnome-desktop
The following actions will resolve these dependencies:
...
...
...
Accept this solution? [Y/n/q/?] Y

> # aptitude upgrade
Resolving dependencies...                
The following NEW packages will be installed:

># aptitude install gnome
{% endhighlight %}

-- -- --

note: succeed login with gnome-shell
next step: install basic applications, 
next step: configure boot manager, as default to windows

{% highlight bash %}
># apt-get install fish wajig grub2 systemd htop mc
Reading package lists... Done
Building dependency tree       
Reading state information... Done

># pico /boot/grub/grub.cfg
># reboot
{% endhighlight %}

-- -- --

note: grub configuration succeed for both system
next step: install favorites applications

{% highlight bash %}
># apt-get install task-xfce-desktop nemo muffin cairo-dock
># apt-get install libreoffice blender inkscape gimp
># apt-get install geany chromium icedove clementine vlc flashplugin-nonfree

># aptitude update; aptitude full-upgrade
># apt-get clean
># halt
{% endhighlight %}

-- -- --

last step: try to sleep, should go back to work early in the morning

-- -- --

## Using aptitude only

The easier way, a workaround

-- -- --

{% highlight bash %}
user@compaq-cq40:~$ su
Password: 

># visudo

># man aptitude

># aptitude upgrade
Resolving dependencies...                
open: 78167; closed: 265627; defer: 244; conflict: 943 
{% endhighlight %}

-- -- --

note: as this take too long, i decided to upgrade partially

{% highlight bash %}
># aptitude install gnome
483)     network-manager-gnome recommends gnome-bluetooth  
484)     cups recommends printer-driver-gutenprint                                                                                          

Accept this solution? [Y/n/q/?] Y

49 packages upgraded, 24 newly installed, 361 to remove and 836 not upgraded.
Need to get 76.4 MB of archives. After unpacking 1,043 MB will be freed.
Do you want to continue? [Y/n/?] Y

># aptitude upgrade

384 packages upgraded, 45 newly installed, 7 to remove and 448 not upgraded.
Need to get 154 MB of archives. After unpacking 154 MB will be used.
Do you want to continue? [Y/n/?] 

># aptitude install task-gnome-desktop

># aptitude upgrade
{% endhighlight %}

-- -- --

{% highlight bash %}
># apt-get install fish wajig grub2 systemd htop mc
># apt-get install task-xfce-desktop nemo muffin cairo-dock
># apt-get install libreoffice blender inkscape gimp
># apt-get install geany chromium icedove clementine vlc flashplugin-nonfree

># aptitude update; aptitude full-upgrade
># aptitude clean; aptitude autoclean

># halt
{% endhighlight %}

-- -- --

last step: go to the refigerator find myself something to eat

[//]: <> ( -- -- -- links below -- -- -- )


[image-ss-debian-install]: {{ site.url }}/assets/posts/system/2014/01/debian-install.jpg
[picasa-ss-debian-install]: https://lh3.googleusercontent.com/-j65GwnT_z3g/Vz2o4lw6y5I/AAAAAAAAAR0/zJnk7wzO4-EXuqkyoyNwuLptfuKT7HpygCCo/s0/debian-install.jpg

