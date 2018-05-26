---
layout: post
title:  "Linux Network - Samba"
date:   2018-05-25 09:25:15 +0700
categories: system
tags: [thought]
author: epsi

excerpt:
  Based on my experience.
  Thorough /etc/samba/smb.conf example

---

{% include post/2018/05/toc-multiboot.html %}

### Overview

> Goal: Thorough /etc/samba/smb.conf example

This is more like a network topic rather than a multiboot topic,
I gather this samba article here, because of my multiboot situation.


#### Samba Configuration

<code>smb.conf</code> for each distribution is available at:

* [github.com/epsi-rns/dotfiles/.../multiboot][dotfiles-multiboot]

- -- --

### Share Definitions

#### Common Share Definition: Debian, Fedora, openSUSE, KaOSx

The basic <code>smb.conf</code> is similar.
Because I copy paste from my Debian to other distribution.

	I made it that way

{% highlight conf %}
[Samba]
   path = /media/Works/Samba/
   available = yes
   valid users = epsi
   read only = no
   browseable = yes
   public = yes
   writeable = yes

# [homes]
#  read only = yes
#  create mask = 0700
#  directory mask = 0700
#  valid users = %S
{% endhighlight %}

#### Other Share Definitions

Off course you can add more share definitions,
but the default setting is difference between distribution.
For example, fedora and openSUSE use printer path,
as **/var/tmp** rather than **/var/spool/samba**.

{% highlight conf %}
# [homes]
#  read only = yes
#  create mask = 0700
#  directory mask = 0700
#  valid users = %S

[printers]
   comment = All Printers
   browseable = no
   path = /var/spool/samba
   printable = yes
   guest ok = no
   read only = yes
   create mask = 0700

[print$]
   comment = Printer Drivers
   path = /var/lib/samba/printers
   browseable = yes
   read only = yes
   guest ok = no
{% endhighlight %}

-- -- --

### Global Configuration

#### Fedora

I do not know why, but the default config is really short.

{% highlight conf %}
[global]
	workgroup = WORKGROUP
	security = user

	passdb backend = tdbsam
{% endhighlight %}

Have a look at the <code class="code-file">smb.conf</code> config for Fedora:

*	[github.com/.../dotfiles/.../smb.fedora.conf][dotfiles-smb-fedora]

I have an issue with SELinux in Fedora.
The easiest workaround is simply to disable SELinux.
However this quick-fix is considered bad practice.

#### Debian

Debian takes longer default cofig.

{% highlight conf %}
[global]

## Browsing/Identification ###
   workgroup = WORKGROUP
   dns proxy = no

#### Networking ####

#### Debugging/Accounting ####
   log file = /var/log/samba/log.%m
   max log size = 1000
   syslog = 0
   panic action = /usr/share/samba/panic-action %d

####### Authentication #######
   server role = standalone server
   passdb backend = tdbsam
   obey pam restrictions = yes
   unix password sync = yes

   passwd program = /usr/bin/passwd %u
   passwd chat = *Enter\snew\s*\spassword:* %n\n *Retype\snew\s*\spassword:* %n\n *password\supdated\ssuccessfully* .
   pam password change = yes

   map to guest = bad user

########## Domains ###########

############ Misc ############

   usershare allow guests = yes
{% endhighlight %}

Have a look at the <code class="code-file">smb.conf</code> config for Debian:

*	[github.com/.../dotfiles/.../smb.debian.conf][dotfiles-smb-debian]

#### KaOSx

Very similar with Debian.
Except that I have to put <code>netbios name</code>,
in order to work from local area network.

{% highlight conf %}
[global]

## Browsing/Identification ###
   workgroup = WORKGROUP
   dns proxy = no

   netbios name = andalan

#### Networking ####

#### Debugging/Accounting ####
   log file = /var/log/samba/log.%m
   max log size = 1000
   syslog = 0
   panic action = /usr/share/samba/panic-action %d

####### Authentication #######
   server role = standalone server
   passdb backend = tdbsam
   obey pam restrictions = yes
   unix password sync = yes
   passwd program = /usr/bin/passwd %u
   passwd chat = *Enter\snew\s*\spassword:* %n\n *Retype\snew\s*\spassword:* %n\n *password\supdated\ssuccessfully* .
   pam password change = yes
   map to guest = bad user

########## Domains ###########

############ Misc ############

   usershare allow guests = yes
{% endhighlight %}

Have a look at the <code class="code-file">smb.conf</code> config for KaOSx:

*	[github.com/.../dotfiles/.../smb.kaosx.conf][dotfiles-smb-kaosx]

#### openSUSE

openSUSE has different beast of configuration.
I also have to put <code>netbios name</code>.

{% highlight conf %}
[global]
	workgroup = WORKGROUP
	passdb backend = tdbsam
	printing = cups
	printcap name = cups
	printcap cache time = 750
	cups options = raw
	map to guest = Bad User
	include = /etc/samba/dhcp.conf
	logon path = \\%L\profiles\.msprofile
	logon home = \\%L\%U\.9xprofile
	logon drive = P:
	usershare allow guests = No
	add machine script = /usr/sbin/useradd  -c Machine -d /var/lib/nobody -s /bin/false %m$
	domain logons = No
	domain master = No
	ldap admin dn = 
	security = user
	wins server = 
	wins support = No
	
	netbios name = andalan
{% endhighlight %}

Have a look at the <code class="code-file">smb.conf</code> config for openSUSE:

*	[github.com/.../dotfiles/.../smb.opensuse.conf][dotfiles-smb-openSUSE]

-- -- --

### Real Life Access

#### Preparation

For each linux, add samba Password.

{% highlight conf %}
$ sudo smbpasswd -a epsi
New SMB password:
Retype new SMB password:
Added user epsi.
{% endhighlight %}

And check

{% highlight conf %}
$ sudo pdbedit -L
epsi:1000:Epsi Sayidina
{% endhighlight %}

#### Fedora

Check Service.

{% highlight conf %}
sudo systemctl status smb nmb
{% endhighlight %}

Check samba:

{% highlight conf %}
% smbtree
WORKGROUP
	\\ANDALAN        		Samba 4.8.1
		\\ANDALAN\IPC$           	IPC Service (Samba 4.8.1)
		\\ANDALAN\Samba 
{% endhighlight %}

![Network: Fedora: smbtree][image-ss-fedora-smbtree]{: .img-responsive }

{% highlight conf %}
% smbclient -L localhost
Enter WORKGROUP\epsi's password: 

	Sharename       Type      Comment
	---------       ----      -------
	Samba           Disk      
	IPC$            IPC       IPC Service (Samba 4.8.1)
Reconnecting with SMB1 for workgroup listing.

	Server               Comment
	---------            -------

	Workgroup            Master
	---------            -------
	WORKGROUP 
{% endhighlight %}

![Network: Fedora: smbclient][image-ss-fedora-smbclient]{: .img-responsive }

Access form Android:

![Network: Android: Fedora SMB][image-ss-andsmb-fedora]{: .img-responsive }

#### Debian

Check Service.

{% highlight conf %}
$ sudo systemctl status smbd nmbd
{% endhighlight %}

Check samba:

{% highlight conf %}
$ smbtree
WORKGROUP
        \\ANDALAN                       Samba 4.7.7
                \\ANDALAN\IPC$                  IPC Service (Samba 4.7.7)
                \\ANDALAN\Samba          
{% endhighlight %}

![Network: Debian: smbtree][image-ss-debian-smbtree]{: .img-responsive }

{% highlight conf %}
$ smbclient -L localhost
WARNING: The "syslog" option is deprecated
Enter WORKGROUP\epsi's password: 

	Sharename       Type      Comment
	---------       ----      -------
	print$          Disk      Printer Drivers
	Samba           Disk      
	IPC$            IPC       IPC Service (Samba 4.7.4-Debian)
	HP_LaserJet_Professional_P1102 Printer   HP_LaserJet_Professional_P1102
	Deskjet_1510    Printer   Deskjet_1510
Reconnecting with SMB1 for workgroup listing.

	Server               Comment
	---------            -------

	Workgroup            Master
	---------            -------
	WORKGROUP        
{% endhighlight %}

![Network: Debian: smbclient][image-ss-debian-smbclient]{: .img-responsive }

Access form Android:

![Network: Android: Debian SMB][image-ss-andsmb-debian]{: .img-responsive }

#### KaOSx

{% highlight conf %}
% sudo systemctl status smbd nmbd
{% endhighlight %}

Check samba:

{% highlight conf %}
% smbtree 
WORKGROUP
        \\ANDALAN                       Samba 4.7.7
                \\ANDALAN\IPC$                  IPC Service (Samba 4.7.7)
                \\ANDALAN\Samba  
{% endhighlight %}

![Network: KaOSx: smbtree][image-ss-kaosx-smbtree]{: .img-responsive }

{% highlight conf %}
% smbclient -L localhost
WARNING: The "syslog" option is deprecated
Enter WORKGROUP\epsi's password: 

        Sharename       Type      Comment
        ---------       ----      -------
        Samba           Disk      
        IPC$            IPC       IPC Service (Samba 4.7.7)
Reconnecting with SMB1 for workgroup listing.

        Server               Comment
        ---------            -------

        Workgroup            Master
        ---------            -------
        WORKGROUP            ANDALAN
{% endhighlight %}

![Network: KaOSx: smbclient][image-ss-kaosx-smbclient]{: .img-responsive }

#### openSUSE

{% highlight conf %}
sudo systemctl status smb nmb
{% endhighlight %}

Check samba:

{% highlight conf %}
% smbtree
WORKGROUP
	\\ANDALAN        		Samba 4.6.13-git.72.2a684235f4112.1-SUSE-SLE_12-
{% endhighlight %}

![Network: openSUSE: smbtree][image-ss-opensuse-smbtree]{: .img-responsive }

{% highlight conf %}
% smbclient -L localhost
Enter WORKGROUP\epsi's password: 
Domain=[ANDALAN] OS=[Windows 6.1] Server=[Samba 4.6.13-git.72.2a684235f4112.1-SUSE-SLE_12-x86_64]

	Sharename       Type      Comment
	---------       ----      -------
	profiles        Disk      Network Profiles Service
	users           Disk      All users
	groups          Disk      All groups
	print$          Disk      Printer Drivers
	Samba           Disk      
	IPC$            IPC       IPC Service (Samba 4.6.13-git.72.2a684235f4112.1-SUSE-SLE_12-x86_64)
	epsi            Disk      Home Directories
Domain=[ANDALAN] OS=[Windows 6.1] Server=[Samba 4.6.13-git.72.2a684235f4112.1-SUSE-SLE_12-x86_64]

	Server               Comment
	---------            -------

	Workgroup            Master
	---------            -------
	WORKGROUP            ANDALAN
{% endhighlight %}

![Network: openSUSE: smbclient][image-ss-opensuse-smbclient]{: .img-responsive }

Access form Android:

![Network: Android: openSUSE SMB][image-ss-andsmb-opensuse]{: .img-responsive }

-- -- --

### Conclusion

Finished. We are done with multiboot article series.
Consider going back reading old article [ [Multiboot: Setting up Partition][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/multiboot/pc-01' %}

[local-part-config]:        /system/2014/03/13/linux-multiboot.html

[dotfiles-multiboot]:      {{ dotfiles }}
[dotfiles-smb-debian]:     {{ dotfiles }}/smb.debian.conf
[dotfiles-smb-fedora]:     {{ dotfiles }}/smb.fedora.conf
[dotfiles-smb-opensuse]:   {{ dotfiles }}/smb.opensuse.conf
[dotfiles-smb-kaosx]:      {{ dotfiles }}/smb.kaosx.conf

[image-ss-debian-smbclient]:    {{ asset_path }}/debian-smbclient.png
[image-ss-debian-smbtree]:      {{ asset_path }}/debian-smbtree.png
[image-ss-fedora-smbclient]:    {{ asset_path }}/fedora-smbclient.png
[image-ss-fedora-smbtree]:      {{ asset_path }}/fedora-smbtree.png
[image-ss-kaosx-smbclient]:     {{ asset_path }}/kaosx-smbclient.png
[image-ss-kaosx-smbtree]:       {{ asset_path }}/kaosx-smbtree.png
[image-ss-opensuse-smbclient]:  {{ asset_path }}/opensuse-smbclient.png
[image-ss-opensuse-smbtree]:    {{ asset_path }}/opensuse-smbtree.png
[image-ss-andsmb-debian]:       {{ asset_path }}/andsmb-debian.png
[image-ss-andsmb-fedora]:       {{ asset_path }}/andsmb-fedora.png
[image-ss-andsmb-opensuse]:     {{ asset_path }}/andsmb-opensuse.png
