---
layout     : post
title      : "Windows - Setting up CLI Environment"
date       : 2019-01-21 09:45:15 +0700
categories : system
tags       : [windows, package manager]
keywords   : [chocolatey, cmder]
author     : epsi
toc        : toc/2019/01/windows-cli.html

opengraph:
  image: /assets/site/images/topics/bash.png

excerpt:
  Windows can have powerful command line environment using
  chocolatey package manager and cmder terminal.

---

<a name="preface"></a>

### Preface

> Goal: Prepare comfortable CLI environment in Windows.

#### Table of Content

* [Preface](#preface): Table of Content

* 1: [Issue](#issue)

* 2: [Choco Package Manager](#choco)

* 3: [Cmder Console](#cmder)

* 4: [Install with Choco](#install)

* [Conclusion](#conclusion)

-- -- --

<a name="issue"></a>

### 1: Issue

The issue I face, when explaining things
to my fellow discussion pals is, different platform.
Some folks on group desire windows as their environment.
What can I say ? All I can do is going back to basic stuff,
such as setting up environment in windows.

	Not everyone is using Gentoo, allright!!

#### Solution

Fortunately, now we have these tools:

*	cmder: pretty terminal, as replacement tools for windows's cmd.

*	chocolatey: package manager for windows

*	schoop: alternative installer other than chocolatey.

After a while, these stuff making Windows great again.
As a combination of <code>choco</code> and <code>cmder</code>,
we have cool package manager inside beautiful terminal.

	It feels like home to me, it feels like home to me
	It feels like I'm all the way back where I belong
	
	Chantal Kreviazuk

They are pretty easy to setup.
In a few minutes, you can also be a productive human with windows.

-- -- --

<a name="choco"></a>

### 2: Choco Package Manager

As I said, there are at least two breathtaking tools

*	[chocolatey.org](https://chocolatey.org/) Package Manager for Windows

*	[scoop.sh](https://scoop.sh/) Installer for Windows

#### Install Choco

As I stated, this one is glorious.
Go ahead with install page

*	[chocolatey install](https://chocolatey.org/install)

You should see the long command line that you need, on that website.

{% highlight bash %}
@"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" bla bla bla ....
{% endhighlight %}

Now open <code>cmd</code> with administrator privilege,
and copy paste the command.
Do not forget to press enter to run the long command.

![Windows cmd: Chocolatey Setup][image-ss-cmd-choco-setup]{: .img-responsive }

-- -- --

<a name="cmder"></a>

### 3: Cmder Console

Now comes, the magical parts: The terminal.

*	[cmder.net](http://cmder.net/)

As you can see, there are two options:

![Windows: Which cmder][image-ss-which-cmder]{: .img-responsive }

Just pick one of the two, download, unzip, and open it.
You can pin to taskbar to make <code>cmder</code> easy to use.
You can also run <code>cmder</code> as administrator, whenever you need.

Between the two.
I found that cmder mini is enough.
However, let me share my experience.

#### Cmder Full

Notice the errors.
Unfortunately, my old windows 7 is 32 bit.

![Windows: cmder full][image-ss-terminal-cmder-full]{: .img-responsive }

It comes with git for windows.
And it will show you this:

{% highlight bash %}
$ git clone https://gitlab.com/epsi-rns/demo-hugo

This version of D:\cmder\vendor\git-for-windows\cmd\git.exe is not compatible with the v
ersion of Windows you're running. Check your computer's system information to see whethe
r you need a x86 (32-bit) or x64 (64-bit) version of the program, and then contact the s
oftware publisher.
{% endhighlight %}

#### Cmder Mini

The mini version is pretty similar.
Except it has minimal error in 32 bit version.

![Windows: cmder mini][image-ss-terminal-cmder-mini]{: .img-responsive }

Mini is also very slim in size.

#### Git issue in 32 bit

Here comes the help from <code>chocolatey</code>.
We can install git easily using <code>chocolatey</code>.

{% highlight bash %}
$ choco install git
{% endhighlight %}

![Windows cmder: choco install git][image-ss-choco-git-install]{: .img-responsive }

As you can see in figure below, now git run well in windows.

![Windows cmder: git clone][image-ss-cmder-git-clone]{: .img-responsive }

-- -- --

<a name="install"></a>

### 4: Install with Choco

I'm having some fun with this new lovely toy.
Installing few packages.

{% highlight bash %}
$ choco install curl
{% endhighlight %}

![Windows cmder: choco install curl][image-ss-choco-install-curl]{: .img-responsive }

{% highlight bash %}
$ choco install youtube-dl
{% endhighlight %}

![Windows cmder: choco install youtube-dl][image-ss-choco-install-ydl]{: .img-responsive }

-- -- --

<a name="conclusion"></a>

### Conclusion

Solved.
Developing in Windows is fun again.

### What is Next 🤔?

There is other article, that you might need to read.
Consider continue reading [ [Windows - Chocolatey - Sass][local-whats-next] ].

Happy New Year 2019.
And thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/assets-system' %}

[local-whats-next]:         https://epsi-rns.gitlab.io/frontend/2019/01/23/windows-choco-sass/

[image-ss-choco-install-curl]:     {{ system_path }}/2019/01/windows-01-choco-install-curl.png
[image-ss-choco-install-ydl]:      {{ system_path }}/2019/01/windows-01-choco-install-youtube-dl.png
[image-ss-cmd-choco-setup]:        {{ system_path }}/2019/01/windows-01-cmd-choco-setup.png

[image-ss-which-cmder]:            {{ system_path }}/2019/01/windows-01-cmder-download.png
[image-ss-terminal-cmder-full]:    {{ system_path }}/2019/01/windows-01-terminal-cmder-full.png
[image-ss-terminal-cmder-mini]:    {{ system_path }}/2019/01/windows-01-terminal-cmder-mini.png

[image-ss-choco-git-install]:      {{ system_path }}/2019/01/windows-02-choco-git-install.png
[image-ss-cmder-git-clone]:        {{ system_path }}/2019/01/windows-02-cmder-git-clone-demo.png
