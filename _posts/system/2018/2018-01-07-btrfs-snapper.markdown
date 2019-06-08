---
layout: post
title: "File System - Trapped in Snapper Rollback"
date      : 2018-01-07 09:45:15 +0700
categories: system
tags      : [btrfs, grub2, filesystem]
keywords  : [opensuse, snapper, troubleshooting]
author: epsi

opengraph:
  image: /assets/site/images/topics/opensuse.png

excerpt:
  My openSUSE Experience, solving unintended snapper rollback.

related_link_ids: 
  - 17083145  # Docker Package Manager Summary

---

### Preface

Dear BTRFS folks, I'm using snapper in openSUSE.

For learning purpose I did <code>% snapper rollback</code>.
It was more like a curiosity reason about snapper, but become a blunder.

> BTRFS Mount Trapped in Snapper Rollback

After <code>% snapper rollback</code>, my <code>% snapper list</code> become empty.

{% highlight bash %}
% sudo snapper list
[sudo] password for root: 
Type   | # | Pre # | Date | User | Cleanup | Description | Userdata
-------+---+-------+------+------+---------+-------------+---------
single | 0 |       |      | root |         | current     |     
{% endhighlight %}

And this strange error message from snapper.

{% highlight bash %}
% sudo snapper rollback
IO error (.snapshots is not a btrfs subvolume).
{% endhighlight %}

It seems like root filesystem mounted to a snapshot volume as shown in command below.

{% highlight bash %}
% mount | grep '/dev/sda8 on / type'
/dev/sda8 on / type btrfs (rw,relatime,space_cache,subvolid=400,subvol=/@/.snapshots/75/snapshot)
{% endhighlight %}

[![Snapper: Rollback Issue][image-ss-snapper-issue]{: .img-responsive }][photo-ss-snapper-issue]

How do I solve the issue, undo the rollback to my normal filesystem ?

The answer is, add <code>.snapshots</code> entry in <code>/etc/fstab</code>.

-- -- --

### Before The Issue

I actually had wonderful life with snapper.

#### Reading

* [Snapper Tutorial](http://snapper.io/tutorial.html)

* [openSUSE Reference](https://doc.opensuse.org/documentation/leap/reference/html/book.opensuse.reference/cha.snapper.html)

* [Shujianyang Blog](http://shujianyang.github.io/2017/01/16/Fix-Space-Problem.html)

#### Setup Subvolume for Root

{% highlight bash %}
% sudo snapper -c root create-config /
{% endhighlight %}

{% highlight bash %}
% sudo snapper list-configs
Config | Subvolume
-------+----------
root   | /
{% endhighlight %}

#### Configuration

{% highlight bash %}
% sudo cat /etc/snapper/configs/root
...
# limit for number cleanup
NUMBER_MIN_AGE="1800"
NUMBER_LIMIT="20"
NUMBER_LIMIT_IMPORTANT="7"
...
{% endhighlight %}

#### Some Cool Commands

{% highlight bash %}
% sudo snapper status 3..7
...
c..... /var/lib/systemd/random-seed
c..... /var/lib/wicked/lease-wlp3s0-dhcp-ipv4.xml
c..... /var/lib/wicked/lease-wlp3s0-dhcp-ipv6.xml
c..... /var/lib/zypp/AutoInstalled
{% endhighlight %}

Or if you wish, summary using line count.

{% highlight bash %}
% sudo snapper status 3..7 | wc -l
1805
{% endhighlight %}

[![snapper: status][image-ss-snapper-status]{: .img-responsive }][photo-ss-snapper-status]

Undo Change

{% highlight bash %}
% sudo snapper undochange 11..39
{% endhighlight %}

Even delete

{% highlight bash %}
% for i in `seq 11 39`; do sudo snapper delete --sync $i; done
{% endhighlight %}

#### Make a snapshot

{% highlight bash %}
% snapper -c config create --description backup.epsi
{% endhighlight %}

-- -- --

### Solving The Issue

#### First I need to add this line to /etc/fstab

{% highlight bash %}
UUID=e901452a-b3ea-48d2-acdb-6687fad0be50 /.snapshots btrfs subvol=@/.snapshots   0 0
{% endhighlight %}

and mount it, no need to reboot.

{% highlight bash %}
% sudo mount /.snapshots
{% endhighlight %}

#### Get current snap

In my case, previously I have snapshot number 75.
This is below just an example.

{% highlight bash %}
% sudo btrfs subvolume show /
@/.snapshots/45/snapshot
	Name: 			snapshot
	UUID: 			29d4436b-72d1-f14d-a9e9-f04b70c0ebe0
	Parent UUID: 		efa9f21b-f3b7-3543-9649-3897e083b39d
	Received UUID: 		-
	Creation time: 		2018-01-03 00:15:01 +0700
	Subvolume ID: 		355
	Generation: 		5677
	Gen at creation: 	3968
	Parent ID: 		301
	Top level ID: 		301
	Flags: 			-
	Snapshot(s):
				@/.snapshots/50/snapshot
				@/.snapshots/51/snapshot
{% endhighlight %}

{% highlight bash %}
% sudo btrfs subvolume get-default /
ID 355 gen 5682 top level 301 path @/.snapshots/45/snapshot 
{% endhighlight %}

[![BTRFS: get default subvolume][image-ss-btrfs-get-default]{: .img-responsive }][photo-ss-btrfs-get-default]

#### list snapper

{% highlight bash %}
% sudo snapper list
Type   | #  | Pre # | Date                            | User | Cleanup  | Description  | Userdata     
-------+----+-------+---------------------------------+------+----------+--------------+--------------
single | 0  |       |                                 | root |          | current      |              
single | 3  |       | Mon 01 Jan 2018 02:45:03 PM WIB | root | timeline | timeline     |              
single | 7  |       | Mon 01 Jan 2018 04:30:01 PM WIB | root | timeline | timeline     |              
single | 40 |       | Tue 02 Jan 2018 10:15:01 PM WIB | root | timeline | timeline     |              
single | 41 |       | Tue 02 Jan 2018 10:52:35 PM WIB | root |          | backup.epsi  |              
pre    | 42 |       | Tue 02 Jan 2018 11:01:48 PM WIB | root | number   | zypp(zypper) | important=yes
post   | 43 | 42    | Tue 02 Jan 2018 11:07:23 PM WIB | root | number   |              | important=yes
single | 44 |       | Tue 02 Jan 2018 11:15:01 PM WIB | root | timeline | timeline     |              
single | 45 |       | Wed 03 Jan 2018 12:15:01 AM WIB | root | timeline | timeline     |              
pre    | 46 |       | Wed 03 Jan 2018 12:16:34 AM WIB | root | number   | zypp(zypper) | important=no 
post   | 47 | 46    | Wed 03 Jan 2018 12:19:38 AM WIB | root | number   |              | important=no 
pre    | 48 |       | Wed 03 Jan 2018 12:47:47 AM WIB | root | number   | zypp(zypper) | important=no 
post   | 49 | 48    | Wed 03 Jan 2018 12:48:10 AM WIB | root | number   |              | important=no 
...
{% endhighlight %}

I decide to go back to 45.

#### Get BTRFS ID

{% highlight bash %}
% sudo btrfs subvolume list -a /
ID 257 gen 4804 top level 5 path <FS_TREE>/@
ID 258 gen 5096 top level 257 path <FS_TREE>/@/opt
ID 259 gen 4157 top level 257 path <FS_TREE>/@/srv
ID 260 gen 5672 top level 257 path <FS_TREE>/@/tmp
ID 261 gen 5538 top level 257 path <FS_TREE>/@/usr/local
ID 262 gen 5688 top level 257 path <FS_TREE>/@/var/cache
ID 263 gen 4157 top level 257 path <FS_TREE>/@/var/crash
ID 264 gen 4157 top level 257 path <FS_TREE>/@/var/lib/libvirt/images
ID 265 gen 4157 top level 257 path <FS_TREE>/@/var/lib/machines
ID 266 gen 4157 top level 257 path <FS_TREE>/@/var/lib/mailman
ID 267 gen 4157 top level 257 path <FS_TREE>/@/var/lib/mariadb
ID 268 gen 4157 top level 257 path <FS_TREE>/@/var/lib/mysql
ID 269 gen 4157 top level 257 path <FS_TREE>/@/var/lib/named
ID 270 gen 4157 top level 257 path <FS_TREE>/@/var/lib/pgsql
ID 271 gen 5688 top level 257 path <FS_TREE>/@/var/log
ID 272 gen 4157 top level 257 path <FS_TREE>/@/var/opt
ID 273 gen 5688 top level 257 path <FS_TREE>/@/var/spool
ID 274 gen 5672 top level 257 path <FS_TREE>/@/var/tmp
ID 301 gen 5674 top level 257 path <FS_TREE>/@/.snapshots
ID 304 gen 3008 top level 301 path <FS_TREE>/@/.snapshots/3/snapshot
ID 311 gen 3160 top level 301 path <FS_TREE>/@/.snapshots/7/snapshot
ID 345 gen 3737 top level 301 path <FS_TREE>/@/.snapshots/40/snapshot
ID 350 gen 3849 top level 301 path <FS_TREE>/@/.snapshots/41/snapshot
ID 351 gen 3862 top level 301 path <FS_TREE>/@/.snapshots/42/snapshot
ID 353 gen 3874 top level 301 path <FS_TREE>/@/.snapshots/43/snapshot
ID 354 gen 3887 top level 301 path <FS_TREE>/@/.snapshots/44/snapshot
ID 355 gen 5688 top level 301 path <FS_TREE>/@/.snapshots/45/snapshot
ID 356 gen 3972 top level 301 path <FS_TREE>/@/.snapshots/46/snapshot
ID 357 gen 3980 top level 301 path <FS_TREE>/@/.snapshots/47/snapshot
ID 358 gen 4021 top level 301 path <FS_TREE>/@/.snapshots/48/snapshot
ID 359 gen 4022 top level 301 path <FS_TREE>/@/.snapshots/49/snapshot
{% endhighlight %}

I find the ID of 45, it is 355.

#### Change default

To make it writable

{% highlight bash %}
% sudo btrfs property set -ts /.snapshots/45/snapshot ro false
{% endhighlight %}

And make it default

{% highlight bash %}
% sudo btrfs subvolume set-default 355 /
{% endhighlight %}

Then reboot

#### Remove the annoying snapshot.

{% highlight bash %}
% sudo snapper delete 75
% sudo snapper delete 76
% sudo snapper delete 77
% sudo snapper list
{% endhighlight %}

That's all

-- -- --

### Conclusion

	I have to learn about the basic of BTRFS before going back to snapper !

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/system/2018/01' %}

[image-ss-snapper-issue]: {{ asset_path }}/snapper-issue.png
[photo-ss-snapper-issue]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipMZ_vBHEVIO0l6mSuBArZStbxGqLzcsURZSzGr1

[image-ss-snapper-status]: {{ asset_path }}/snapper-status.png
[photo-ss-snapper-status]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipP0lEwTVU3QAbMPyCT4xYvskrWmzZ-vJ0peAF5H

[image-ss-btrfs-get-default]: {{ asset_path }}/btrfs-get-default.png
[photo-ss-btrfs-get-default]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipPfJs-hf-Hx3BDOWwnmLS86go7iN8iZl-PcmO2r

