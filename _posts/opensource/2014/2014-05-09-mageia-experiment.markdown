---
layout: post
title:  "My Mageia Experiment"
date:   2014-05-09 19:34:15 +0700
categories: opensource
tags: [screenshot, mageia, kwin]
author: epsi
excerpt:
  I'm curious about Mageia. Because there is something that 
  I haven't learned in my early days of linux using Mandrake.  
---

Back to Mageia

{% highlight bash %}
># urpmi task-kde
{% endhighlight %}

[![Mageia Welcome Screen][image-ss-mageia-welcome]{: .img-responsive }][picasa-ss-mageia-welcome]

I spent my early days of linux using Red Hat in 1999, 
followed by Mandrake one year later.
Returned to Windows. Then Debian in 2004
which was my longest years of linux distro,
dual booting with Windows.
In the mean time I was also a distro hopper with
Mageia, Ubuntu, kFreeBSD, and Kali.
Until all of a sudden in 2014 I decided
to migrate completely to Arch Linux.
My journey is not complete yet. 
Learning is my never ending hobby.

Magiea is a successor of Mandriva,
while Mandriva itself is successor of Mandrake.
After changed its name from Mandrake to Mandriva to Mageia,
The latest of Mageia Development is unclear,
and it is not a linux distro that I recommend.

My only reason to use Mageia is for a sentimental nostalgic reason.
There is something that I haven't learned in the past.
I'm curious, and I don't want to live in regrets,
before this distro vanished for ever.
That's why I'm back to Mandrake aka Mandriva aka Mageia.

[![Mageia Control Center][image-ss-mageia-controlcenter]{: .img-responsive }][picasa-ss-mageia-controlcenter]
<br/><br/>

Well. It has a very nice Control Panel.

<hr/>

### Brief

Instead of Drake. I'm using command line as usual.

{% highlight bash %}
 $ su

># urpmi.removemedia -a
># urpmi.addmedia --distrib --mirrorlist http://mirrors.mageia.org/api/mageia.4.x86_64.list
># urpmi --replacefiles --auto-update --auto
># urpmi --replacefiles --auto-update --auto
># urpmi --replacefiles --auto-update --auto

># urpmi task-cinnamon

># urpmi htop gfxboot plymouth systemd fish 
># urpmi blender inkscape gimp chromium thunderbird
># urpmi geany clementine vlc cairo-dock docky

># urpmi task-lxde task-razorqt task-xfce task-e17
># urpmi icewm twm lxdm xdm pekwm
># urpmi afterstep awesome fluxbox matchbox-desktop 

># urpmi task-mate
># urpmi task-sugar

$ drakconf

># urpmi.update -a

># urpmi --auto-update 

?# rpm -e --nodeps
?# urpme

># rpm -q --whatrequires python3
># rpm -qR yum
># rpm -qR yum-3.4.3-8.mga4
># rpm -qa | grep yum | xsel -b

># urpmq -i geany
># urpmf --summary geany

{% endhighlight %}

[![Mageia Upgrade][image-ss-mageia-upgrade]{: .img-responsive }][picasa-ss-mageia-upgrade]

<hr/>

### Mageia with urpm*

{% highlight bash %}
[root@localhost epsi]# urpmq -i yum

Name        : yum
Version     : 3.4.3
Release     : 8.mga4
Group       : System/Packaging
Size        : 2027418                      Architecture: noarch
Source RPM  : yum-3.4.3-8.mga4.src.rpm
URL         : http://yum.baseurl.org/
Summary     : RPM installer/updater
Description :
Yum is a utility that can check for and automatically download and
install updated RPM packages. Dependencies are obtained and downloaded
automatically prompting the user as necessary.

Name        : yum
Version     : 3.4.3
Release     : 8.mga4
Group       : System/Packaging
Size        : 2027418                      Architecture: noarch
Source RPM  : yum-3.4.3-8.mga4.src.rpm
URL         : http://yum.baseurl.org/
Summary     : RPM installer/updater
Description :
Yum is a utility that can check for and automatically download and
install updated RPM packages. Dependencies are obtained and downloaded
automatically prompting the user as necessary.
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# urpmi yum
To satisfy dependencies, the following packages are going to be installed:
  Package                        Version      Release       Arch    
(medium "Core Release")
  lib64rpmsign3                  4.11.1       8.mga4        x86_64  
  python-celementtree            1.0.5        10.mga4       x86_64  
  python-gpgme                   0.3          3.mga4        x86_64  
  python-iniparse                0.4          4.mga4        noarch  
  python-rpm                     4.11.1       8.mga4        x86_64  
  python-urlgrabber              3.10         2.mga4        noarch  
  python-yum                     3.4.3        8.mga4        noarch  
  yum                            3.4.3        8.mga4        noarch  
  yum-metadata-parser            1.1.4        6.mga4        x86_64  
5.2MB of additional disk space will be used.
1MB of packages will be retrieved.
Proceed with the installation of the 9 packages? (Y/n) Y

    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/python-iniparse-0.4-4.mga4.noarch.rpm
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/python-celementtree-1.0.5-10.mga4.x86_64.rpm                                        
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/python-yum-3.4.3-8.mga4.noarch.rpm                                                  
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/lib64rpmsign3-4.11.1-8.mga4.x86_64.rpm                                              
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/python-gpgme-0.3-3.mga4.x86_64.rpm                                                  
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/python-urlgrabber-3.10-2.mga4.noarch.rpm                                            
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/python-rpm-4.11.1-8.mga4.x86_64.rpm                                                 
    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/yum-metadata-parser-1.1.4-6.mga4.x86_64.rpm                                         
installing python-iniparse-0.4-4.mga4.noarch.rpm python-celementtree-1.0.5-10.mga4.x86_64.rpm python-yum-3.4.3-8.mga4.noarch.rpm lib64rpmsign3-4.11.1-8.mga4.x86_64.rpm python-gpgme-0.3-3.mga4.x86_64.rpm python-rpm-4.11.1-8.mga4.x86_64.rpm python-urlgrabber-3.10-2.mga4.noarch.rpm yum-metadata-parser-1.1.4-6.mga4.x86_64.rpm from /var/cache/urpmi/rpms
Preparing...                     ####################################################################################################################################
      1/9: yum-metadata-parser   ####################################################################################################################################
      2/9: python-urlgrabber     ####################################################################################################################################
      3/9: python-gpgme          ####################################################################################################################################
      4/9: lib64rpmsign3         ####################################################################################################################################
      5/9: python-rpm            ####################################################################################################################################
      6/9: python-celementtree   ####################################################################################################################################
      7/9: python-iniparse       ####################################################################################################################################
      8/9: python-yum            ####################################################################################################################################

    rsync://ftp.akakom.ac.id/pub/linux/mageia/distrib/4/x86_64/media/core/release/yum-3.4.3-8.mga4.noarch.rpm
installing yum-3.4.3-8.mga4.noarch.rpm from /var/cache/urpmi/rpms                                                                                                     
Preparing...                     ####################################################################################################################################
      9/9: yum                   ####################################################################################################################################
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# urpmf --summary yum
yum-metadata-parser:A fast metadata parser for yum
yum:RPM installer/updater
python-yum:Reusable YUM code
yum:RPM installer/updater
python-yum:Reusable YUM code
yum-metadata-parser:A fast metadata parser for yum
{% endhighlight %}

<hr/>

### Mageia with rpm

{% highlight bash %}
[root@localhost epsi]# rpm -qR yum
python-yum = 3.4.3-8.mga4
rpmlib(PartialHardlinkSets) <= 4.0.4-1
rpmlib(PayloadFilesHavePrefix) <= 4.0-1
rpmlib(CompressedFileNames) <= 3.0.4-1
python(abi)
rpmlib(PayloadIsXz) <= 5.2-1
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# rpm -qa | grep yum 
yum-3.4.3-8.mga4
python-yum-3.4.3-8.mga4
yum-metadata-parser-1.1.4-6.mga4
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# rpm -q --whatrequires rpm
perl-URPM-4.29-1.mga4
locales-2.18-2.mga4
basesystem-minimal-2-17.mga4
python-rpm-4.11.1-8.mga4
{% endhighlight %}

<hr/>

Mageia with yum (not recommended)

{% highlight bash %}
[root@localhost epsi]# yum list installed | grep yum
python-yum.noarch                         3.4.3-8.mga4                 installed
yum.noarch                                3.4.3-8.mga4                 installed
yum-metadata-parser.x86_64                1.1.4-6.mga4                 installed
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# yum info yum
Installed Packages
Name        : yum
Arch        : noarch
Version     : 3.4.3
Release     : 8.mga4
Size        : 1.9 M
Repo        : installed
Summary     : RPM installer/updater
URL         : http://yum.baseurl.org/
License     : GPLv3+
Description : Yum is a utility that can check for and automatically download and
            : install updated RPM packages. Dependencies are obtained and downloaded
            : automatically prompting the user as necessary.
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# yum remove fish
Setting up Remove Process
Resolving Dependencies
--> Running transaction check
---> Package fish.x86_64 0:2.1.0-1.mga4 will be erased
--> Finished Dependency Resolution

Dependencies Resolved

===============================================================================
 Package       Arch            Version                Repository          Size
===============================================================================
Removing:
 fish          x86_64          2.1.0-1.mga4           installed          3.9 M

Transaction Summary
===============================================================================
Remove        1 Package

Installed size: 3.9 M
Is this ok [y/N]: Y
Downloading Packages:
Running Transaction Check
Running Transaction Test
Transaction Test Succeeded
Running Transaction
Warning: RPMDB altered outside of yum.
  Erasing    : fish-2.1.0-1.mga4.x86_64                                    1/1 

Removed:
  fish.x86_64 0:2.1.0-1.mga4                                                   

Complete!
{% endhighlight %}

{% highlight bash %}
[root@localhost epsi]# yum provides yum
yum-3.4.3-8.mga4.noarch : RPM installer/updater
Repo        : installed
{% endhighlight %}

<hr/>

I also test some Desktop Environment.
e.g. Cinnamon, XFCE4, RazorQT, and KDE.

Especially Kwin Effect in this PC,
because I can't afford it in my Notebook.

<div class="sectionbox">
  <div class="sectionbox-heading">
    Screenshot Information
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Mageia<br/>
<strong>DE</strong>: XFCE4<br/>
<strong>WM</strong>: KWin<br/>
+ Cube Effect
    </div>
  </div>
</div>

[![Mageia XFCE4 Kwin Cube Effect][image-ss-mageia-xfce4-kwin]{: .img-responsive }][picasa-ss-mageia-xfce4-kwin]
<br/>

And go further with RazorQT.

<div class="sectionbox">
  <div class="sectionbox-heading">
    Screenshot Information
  </div>
  <div class="sectionbox-body">
    <div>
<strong>OS</strong>: Mageia<br/>
<strong>DE</strong>: RazorQT<br/>
<strong>WM</strong>: KWin<br/>
+ Cube Effect
    </div>
  </div>
</div>



[![Mageia RazorQT Kwin Cube Effect][image-ss-mageia-razorqt-kwin]{: .img-responsive }][picasa-ss-mageia-razorqt-kwin]
<br/><br/>


<hr/>

At the end. I keep my Mageia in my Family's PC. Nobody use it but me. And I rarely use it either.


[image-ss-mageia-welcome]: {{ site.url }}/assets/posts/opensource/2014/05/mageia-welcome.png
[picasa-ss-mageia-welcome]: https://lh3.googleusercontent.com/-k6WRjfFM2lg/Vz2oaDzsFCI/AAAAAAAAARk/hlaqABuIODozZSSdRI22dyJatQJB5N5IACCo/s0/mageia-welcome.png
[image-ss-mageia-controlcenter]: {{ site.url }}/assets/posts/opensource/2014/05/mageia-controlcenter.png
[picasa-ss-mageia-controlcenter]: https://lh3.googleusercontent.com/-k13r-0cS-VE/Vz2oYnpJ-QI/AAAAAAAAARk/Wc1QakGlrKkYu9_Jugsba2O5_tuIatHYgCCo/s0/mageia-controlcenter.png
[image-ss-mageia-upgrade]: {{ site.url }}/assets/posts/opensource/2014/05/mageia-upgrade.png
[picasa-ss-mageia-upgrade]: https://lh3.googleusercontent.com/-WfroOgCn-PI/Vz2oXcsNIYI/AAAAAAAAARk/pJ7SlzzBbJ8U_76f55lMU3OqTkGAJVDbwCCo/s0/mageia-upgrade.png
[image-ss-mageia-xfce4-kwin]: {{ site.url }}/assets/posts/opensource/2014/05/mageia-xfce4-kwin.png
[picasa-ss-mageia-xfce4-kwin]: https://lh3.googleusercontent.com/-utAVSadUfw8/Vz2odL-wS0I/AAAAAAAAARk/lr90iKwBXMcfQqoNpKednUOSyG5vNDK-wCCo/s0/mageia-xfce4-kwin.png
[image-ss-mageia-razorqt-kwin]: {{ site.url }}/assets/posts/opensource/2014/05/mageia-razorqt-kwin.png
[picasa-ss-mageia-razorqt-kwin]: https://lh3.googleusercontent.com/-E9CIA0Lv0pc/Vz2oakhiJII/AAAAAAAAARk/g407bDQc_scCxk75HWAhkzCXzoyxcC6CgCCo/s0/mageia-razorqt-kwin.png
