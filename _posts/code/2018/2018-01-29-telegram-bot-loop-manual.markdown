---
layout: post
title:  "Telegram Bot - Manual Page"
categories: code
date:   2018-01-29 09:17:35 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.
  Using loop with BASH script.
  No webhook in BASH series.

---

{% include post/2018/01/toc-telegram-bot.html %}

### Bashful Bot

> Goal: An example of Manual Page in Troff Format

Documentation would make you feel like a pro.

#### Previous Guidance

We're already see a finished bash project in previous article.
We should finish what we have done.

#### Issue

	The need of Example

Manual page is easy. All you need is troff format knowledge.

-- -- --

### Simple Manual Page

I grab my first manual page by using an example from this good site.

* [www.cyberciti.biz](https://www.cyberciti.biz/faq/linux-unix-creating-a-manpage/)

I name it <code class="code-file">cupubot-bash.1</code>.
The lasl character <code>1</code> means this manual in section <code>1</code> category.

{% highlight tex %}
.\" Manpage for cupubot-bash.
.\" Contact epsi.nurwijayadi@gmail.com to correct errors or typos.
.TH man 1 "29 Jan 2018" "v0.001" "cupubot.bash man page"
.SH NAME
cupubot-bash \- a telegram bot in bash
.SH SYNOPSIS
cupubot-bash [options]
.SH DESCRIPTION
Cupubot is a telegram bot for learning purpose, or lame people. Just another learning project.
.SH OPTIONS
The cupubot-bash takes exactly one option.
.SH SEE ALSO
jq(1), json_reformat(1)
.SH BUGS
No known bugs.
.SH AUTHOR
E. Rizqi N. Sayidina (epsi.nurwijayadi@gmail.com)
{% endhighlight %}

-- -- --

### Processing

Manual page stored as <code>.gz</code> file.
Hence we need to convert it using <code>gzip</code>.
We can read the compressed source using <code>zcat</code>.

{% highlight bash %}
% gzip -k -f cupubot-bash.1
% zcat cupubot-bash.1.gz
{% endhighlight %}

![BASH: Telegram Bot: gzip and zcat][image-zcat]{: .img-responsive }

We can also test the result.

{% highlight bash %}
% man ./cupubot-bash.1
{% endhighlight %}

![BASH: Telegram Bot: Manual Page][image-manual]{: .img-responsive }

-- -- --

### Complete Text

<code class="code-file">cupubot-bash.1</code>

{% highlight tex %}
.\" Manpage for cupubot-bash.
.\" Contact epsi.nurwijayadi@gmail.com to correct errors or typos.
.TH man 1 "29 Jan 2018" "v0.001" "cupubot.bash man page"
.SH NAME
cupubot-bash \- a telegram bot in bash
.SH SYNOPSIS
cupubot-bash [options]
.SH DESCRIPTION
Cupubot is a telegram bot for learning purpose, or lame people. Just another learning project.
.SH OPTIONS
The cupubot-bash takes exactly one option.
.TP
\fB\-h\fR, \fB\-\-help\fR
display help information
.TP
\fB\-v\fR, \fB\-\-version\fR, \fBversion\fR
display version information
.TP
\fB\-\-observe\fR
show JSON output of getUpdates
.TP
\fB\-\-reply\fR
reply all messages
.TP
\fB\-\-new-member\fR
greet new member
.TP
\fB\-\-logger-text\fR
log chat conversation
.TP
\fB\-\-logger-html\fR
log chat conversation
.SH SEE ALSO
jq(1), json_reformat(1)
.SH BUGS
No known bugs.
.SH AUTHOR
E. Rizqi N. Sayidina (epsi.nurwijayadi@gmail.com)
{% endhighlight %}

**Source**:
*	[github.com/.../cupubot/.../cupubot-bash.1][dotfiles-manual]

### Conclusions

I think that's all.

I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/cupubot/tree/master/loop/bash' %}

[dotfiles-manual]: {{ dotfiles_path }}/man/cupubot-bash.1

[image-zcat]:      {{ asset_path }}/cupubot-zcat.png
[image-manual]:    {{ asset_path }}/cupubot-manual.png
