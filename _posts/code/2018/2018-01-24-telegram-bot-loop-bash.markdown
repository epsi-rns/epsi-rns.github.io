---
layout: post
title:  "Telegram Bot - BASH Option Argument"
categories: code
date:   2018-01-24 09:17:35 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.
  Using loop with BASH script.
  No webhook in BASH series.

---

### Bashful Bot

> Goal: Utilize Command Line Argument Parser

#### Preface

Would it be nice if we can have help udage for our script ?

{% highlight bash %}
% cd ~/Documents/cupubot/bash

% ./main-modular.bash --version
cupubot v0.001

% ./main-modular.bash --help
usage:  cupubot [options]
operations:
 general
   -v, --version    display version information
   -h, --help       display help information
{% endhighlight %}

![BASH: Telegram Bot: Script with Usage][image-cli-s-usage]{: .img-responsive }

#### Preparation

Before You begin, you need to get a telegram bot token.
It is already discussed in previous article.

#### Previous Guidance

We have already see a modular bash script in previous article.
Now, let's make this script usefulfor anyone by showing usage and version.

#### Issue

Although this looks easy, actually it is tricky.
We need to parse argument from script,
which is somehow, can be so complicated.

Luckily bash has, this <code>OPTIN</code> feature, comes to the rescue.

-- -- --

### Additional Script

I'm going to make it as brief as possible.
We need a bit of change in our main script.
Two more scripts.

{% highlight bash %}
DIR=$(dirname "$0")

. ${DIR}/config.bash
. ${DIR}/functions.bash
. ${DIR}/messages.bash
. ${DIR}/options.bash
{% endhighlight %}

### Message Function

These are the functions, that will be called later.
Politically, every public bash project should have these two.
Do not let your user, getting confused in the dark without a clue.

{% highlight bash %}
function message_usage() {
    cat <<-EOF
usage:  cupubot [options]
operations:
 general
   -v, --version    display version information
   -h, --help       display help information

EOF
}

function message_version() {
    local version='v0.001'
    echo "cupubot $version"
}
{% endhighlight %}

### Message Function

And these is the script that process the argument.

{% highlight bash %}
function get_options_from_arguments() {

    # http://wiki.bash-hackers.org/howto/getopts_tutorial

    # get options
    count=0
    
    # ! : indirect expansion
    while [[ -n "${!OPTIND}" ]]; do
        case "${!OPTIND}" in
            version)   
                message_version; 
                exit;;
        esac

        while getopts "vh-:" OPT; do
            case "$OPT" in
                -)
                    case "$OPTARG" in
                        version) 
                            message_version; 
                            exit;;
                        help) 
                            message_usage; 
                            exit;;
                        *) 
                            continue;;
                    esac;;
                h)  
                    message_usage; 
                    exit;;
                v)  
                    message_version; 
                    exit;;
                *)  
                    continue;;
            esac
        done

        shift $OPTIND
        OPTIND=1
    done
}
{% endhighlight %}

-- -- --

### How does it works

Most built-in BASH internal variable are not easy to understand.
But once you get it, you will know the beauty.

Consider this chunck of code.

#### OPTIND

{% highlight bash %}
    while [[ -n "${!OPTIND}" ]]; do
        ...
        shift $OPTIND
        OPTIND=1
    done
{% endhighlight %}

The <code>OPTIND</code> is an index of argument,
the <code>!</code> is indirect expansion.
It means <code>!OPTIND</code> return the value of index.

The <code>while</code> along with <code>shift</code>,
will iterate argument from beginning to end.

In this case it will only use

*	<code>version</code>, or

*	use <code>getopt "vh-:"</code> instead

Hints:

{% highlight bash %}
% man optind
{% endhighlight %}

I must admit I cannot explain this concpet,
but there is a good explanation here in stackoverflow.

* [StackOverflow OPTIN](https://stackoverflow.com/questions/14249931/how-does-the-optind-variable-work-in-the-shell-builtin-getopts)

#### getopts

{% highlight bash %}
        while getopts "vh-:" OPT; do
            case "$OPT" in
            ...
            esac
        done
{% endhighlight %}

What is this <code>"vh-:"</code> meaning ?
It means getopt except these arguments

*	<code>-v</code>

*	<code>-h</code>

*	<code>--:</code>

The prefix <code>:</code> means, an argument expecting an options.
Noe here are our options

*	<code>-v</code>

*	<code>-h</code>

*	<code>--version</code>

*	<code>--help</code>

Hints:

{% highlight bash %}
% man getopt
{% endhighlight %}

This is also a good reading.

* [getopts tutorial](http://wiki.bash-hackers.org/howto/getopts_tutorial)

-- -- --

### Conclusion

![BASH: Telegram Bot: Script with Usage][image-cli-s-usage]{: .img-responsive }

So here is our acceptable argument.

*	<code>./main-modular.bash</code>

*	<code>./main-modular.bash version</code>

*	<code>./main-modular.bash --version</code>

*	<code>./main-modular.bash --help</code>

*	<code>./main-modular.bash -v</code>

*	<code>./main-modular.bash -h</code>


Yeah, that is all. Not so complicated after all.

	Can you read the pattern ?

-- -- --

There above are some simple codes, that I put together. 
I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[local-overview]: /code/2017/04/23/overview-pipe-and-fork.html

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[image-cli-s-usage]:  {{ asset_path }}/cupubot-cli-script-usage.png
