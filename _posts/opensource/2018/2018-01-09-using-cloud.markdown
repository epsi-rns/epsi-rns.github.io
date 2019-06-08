---
layout: post
title:  "Using Cloud for Daily Activity"
date      : 2018-01-09 12:56:15 +0700
categories: opensource
tags      : [console]
keywords  : [cloud, dropbox, google drive, mega]
author: epsi

opengraph:
  image: /assets/site/images/topics/git.png

excerpt:
  General cloud goodies for personal document transfer.
  Sync data using file sharing tool on the cloud,
  either Dropbox, Goggle Drive, Mega and Git,
  instead of USB Flash Disk, between Home and Office.

---

### Preface

I use to work with my Notebook, and back them every month to external HDD/
Now that my notebook getting old, and not in a reliable condition, I must buy new one.
Just like everybody else in the world, I have a grown up responsibility,
such as bill to pay, and I also have delayed payment, so forget about
budgeting a fancy notebook. Instead use my current PC at home and office.

In my temporary session, I moved my data using an also old usb flash disk,
from openSUSE at home, sync with Arch in Notebook, to Fedora at Office.
It is an error prone way to sync, rely on the memory on my brain.

After a month it has grown to 200MB,
from libreoffice document, image from scan to pdf.
The big part, I save directly into external hard drive, instead of sync.
This USB fash disk limit, keep my folder tidy.
I also have ISP limitation. Very good speed at download, but horribe at upload.
I cannot even send mail for something attachment more than 1,5 MB.
Unlike my code in github, most of all is not for public,
for either security and privacy issue.

My requirement for this sharing platform are simple.

* Free service, for relatively small data size.

* Speed, not too slow with my ISP.

* Command Line Interface for linux. I rarely use windows.

-- -- --

### Git Pull

Before we begin,
I have been an active git user for almost three years,
using single user and single notebook.

* [Using Git for Daily Repo Updating][local-using-git]

The only different is that now I have to switch using multiple computer.
Now my git, act like a cloud. I have to add this one command as my new habit.

{% highlight bash %}
% git pull
{% endhighlight %}

[![git pull][image-ss-git-pull]{: .img-responsive }][photo-ss-git-pull]

-- -- --

### Dropbox

This is my new toy.

I can install it well in both openSUSE and Arch Linux.
But there is this difference though.

*   Arch is using <code>% dropbox-cli</code> command

*   openSUSE is using <code>% dropbox</code> command

#### Help

This is my <code>% dropbox-cli help</code> command with arch

[![dropbox-cli help][image-ss-dropbox-help]{: .img-responsive }][photo-ss-dropbox-help]

#### Get started

First time using Dropbox CLI, you need to download the official application.

[![dropbox start][image-ss-dropbox-start]{: .img-responsive }][photo-ss-dropbox-start]

#### Status

I used to complain about lack of progress status in Dropbox in the past.
But now I can use <code>watch</code> to monitor my status at console.

{% highlight bash %}
% watch dropbox status
{% endhighlight %}

[![dropbox status][image-ss-dropbox-status]{: .img-responsive }][photo-ss-dropbox-status]

### File Status

Again I use <code>watch</code> to monitor file and direcory status at console.

{% highlight bash %}
% cd ~/Dropbox
% watch dropbox filestatus
{% endhighlight %}

This is my root directory.

[![dropbox filestatus root][image-ss-dropbox-filestatus]{: .img-responsive}][photo-ss-dropbox-filestatus]

### Test

I move back to my Arch and openSUSE, done some renaming, file deletion, and
add other file. It is all works well.

It shows nicely, what file is in sync progress.

[![dropbox filestatus progress][image-ss-dropbox-filesync]{: .img-responsive}][photo-ss-dropbox-filesync]

-- -- --

### Google Drive

I used to be a googledrive user using <code>grive</code>.
Now that Google Drive has been rebrand to Google Backup.
<code>grive2</code> doesn't seems to work in my Arch,
and I also cannot find in openSUSE.

There is a good alternative, called <code>gdrive</code>

* [github.com/prasmussen/gdrive](https://github.com/prasmussen/gdrive)

But I still do not know how to sync yet. My bad.

Time is precious for me.
I have business to do.
Job to get done.
I'm running out of time
I can't spend my life solving issue.

I guess, I have to tag googledrive, as <code>later</code>
You can google other blog to find about gdrive.

-- -- --

## Mega.co.nz

Some Amigos from telegram told me about this Free 50GB storage service.
It is very nice, and also fast, except with my ISP.
I have to do tethering from my smartphone while uploading.
It also works with both my Arch and openSUSE.

There is official command line tools.
But I prefer this one.

* [github.com/megous/megatools](https://github.com/megous/megatools)

#### Configuration

After making an account,
you have to set up <code>.megarc</code>.

{% highlight bash %}
% mkdir ~/megacloud
% cd ~/megacloud
% touch ./.megarc
{% endhighlight %}

{% highlight bash %}
% cat ~/megacloud/.megarc
[Login]
Username = my_email
Password = my_secret_password
{% endhighlight %}

Now you can test the command

{% highlight bash %}
% megadf -h
{% endhighlight %}

[![mega disk free human][image-ss-mega-df]{: .img-responsive}][photo-ss-mega-df]

#### Upload and Download

Just read the manual page.
Copy and Download is straightforward using <code>megacopy</code>.
This won't do reupoad if such file exist.

{% highlight bash %}
% megacopy -l . -R /Root
{% endhighlight %}

[![mega copy upload][image-ss-mega-upload]{:.img-responsive}][photo-ss-mega-upload]

I had successfully upload my data from my openSUSE, and download to my Arch.
Frankly speaking, I have no idea about sync.
My first guess is, Mega has different role with dropbox.
But, I'm too busy to examine that. My Bad.

I really like the upload progress that mega has.

#### List

After upload, you can check the file list nicely.

{% highlight bash %}
% megals
{% endhighlight %}

[![mega ls][image-ss-mega-ls]{: .img-responsive}][photo-ss-mega-ls]

-- -- --

That's all.
I can work normal again now.

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/opensource/2018/01' %}

[local-using-git]: /opensource/2016/06/19/using-git.html

[image-ss-git-pull]: {{ asset_path }}/arch-git-pull.png
[photo-ss-git-pull]: https://photos.google.com/album/AF1QipONAnoC9vhwhNUX26skiUTYcT1xwz71c10Aqi3M/photo/AF1QipNS1hY7GLVRnTn1G-46H1BbOPzWpKfKn1ymQz4v

[image-ss-dropbox-help]: {{ asset_path }}/arch-dropbox-cli-help.png
[photo-ss-dropbox-help]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMsKI2wmh_xxpmCLGle3iIW8QLyMibPhqnm3Kua?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-dropbox-start]: {{ asset_path }}/opensuse-dropbox-start.png
[photo-ss-dropbox-start]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipNSnRbrqmfx8WTiY5M3i28mNn5h_u1hnuTKwqD-?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-dropbox-status]: {{ asset_path }}/opensuse-watch-dropbox-status.png
[photo-ss-dropbox-status]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOYN6sST6XwZorMeKLFp2OCL0EOke2kIiK9emnq?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-dropbox-filestatus]: {{ asset_path }}/opensuse-watch-dropbox-filestatus.png
[photo-ss-dropbox-filestatus]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMvbmzdSH2FdKNPJvZ9V2Lq0sgpveoVNMJuHZPN?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-dropbox-filesync]: {{ asset_path }}/opensuse-watch-dropbox-filestatus-sync.png
[photo-ss-dropbox-filesync]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOfGJF6aHkF8Oze_rsau_EysfEJlNgP2JcyQOcI?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-mega-upload]: {{ asset_path }}/opensuse-megacopy-upload.png
[photo-ss-mega-upload]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipPxmYkPyYjZBpglrv5Jvqw9JvLsIyj9WgqwG06E?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-mega-df]: {{ asset_path }}/opensuse-megadf.png
[photo-ss-mega-df]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipMYjlL6F7Hs5pSWNMZqWwVZbeynhK6znDB5GJeI?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR

[image-ss-mega-ls]: {{ asset_path }}/opensuse-megals.png
[photo-ss-mega-ls]: https://photos.google.com/share/AF1QipMCFikwVY_d7DR9OMOmp-t4qwKDgluWO9lU6qK01_y9IUYA7eorvCdHkmRrRxnatA/photo/AF1QipOzsL1eENvtQbma5L2GRpSvqYv5bkjHPMXdUdPO?key=U2l0bFJCRFZuY00xOUlCeUhiRGVEOTJESVo5MmFR
