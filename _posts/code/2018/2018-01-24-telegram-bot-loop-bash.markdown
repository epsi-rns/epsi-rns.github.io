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

{% include post/2018/01/toc-telegram-bot.html %}

### Bashful Bot

> Goal: Utilize Command Line Argument Parser

#### Preface

Would it be nice if we can have help usage for our script ?

{% highlight bash %}
% cd ~/Documents/cupubot/bash

% ./main.bash --version
cupubot v0.001

% ./main.bash --help
usage:  cupubot [options]

operations:
 general
   -h, --help       display help information
   -v, --version    display version information
{% endhighlight %}

![BASH: Telegram Bot: Script with Usage][image-cli-s-usage]{: .img-responsive }

#### Previous Guidance

We have already see a modular bash script in previous article.
Now, let's make this script useful for anyone by showing usage and version.

#### Issue

Although this looks easy, actually it is tricky.
We need to parse argument from script,
which is somehow, can be so complicated.

Luckily bash has, this <code>OPTIN</code> feature, comes to the rescue.

-- -- --

### Additional Script

I'm going to make it as brief as possible.
We need a bit of change in our main script.
Two more scripts: <code>messages.bash</code>,
and <code>options.bash</code>.

{% highlight bash %}
#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# module: main
. ${DIR}/config.bash
. ${DIR}/messages.bash
. ${DIR}/options.bash
. ${DIR}/controller.bash

# module: task
. ${DIR}/tasks/observe.bash
. ${DIR}/tasks/reply.bash

### -- main --

get_options_from_arguments "$@"
{% endhighlight %}

**Source**:
*	[github.com/.../cupubot/.../main.bash][dotfiles-main]

### Message Script

These are the functions, that will be called later.
Politically, every public bash project should have these two.
Do not let your user, getting confused in the dark without a clue.

{% highlight bash %}
function message_usage() {
    cat <<-EOF
usage:  cupubot [options]

operations:
 general
   -h, --help       display help information
   -v, --version    display version information

EOF
}

function message_version() {
    local version='v0.001'
    echo "cupubot $version"
}
{% endhighlight %}

**Source**:
*	[github.com/.../cupubot/.../messages.bash][dotfiles-messages]

### Beginner Version

Consider this short script.

{% highlight bash %}
#!/usr/bin/env bash

function get_options_from_arguments() {   
    # ! : indirect expansion
    while [[ -n "${!OPTIND}" ]]; do
        case "${!OPTIND}" in
            version)   
                message_version
                exit;;
        esac

        shift $OPTIND
        OPTIND=1
    done
}
{% endhighlight %}

You can run with this command:

{% highlight bash %}
% ./main.bash version
cupubot v0.001
{% endhighlight %}

#### How does it works

The <code>OPTIND</code> is an index of argument,
the <code>!</code> is indirect expansion.
It means <code>!OPTIND</code> return the value of index.

When we pass one argument,
the <code>${OPTIND}</code> will have the value of index <code>1</code>.
And then the <code>${!OPTIND}</code> have the value <code>version</code>.
After shift the <code>${OPTIND}</code> will have the value of index <code>0</code>,
and the loop will be terminated.

### Option Script

Now the complete version of the script that process the argument.

{% highlight bash %}
# reading
# http://wiki.bash-hackers.org/howto/getopts_tutorial

# handling command
function handle_command_plain() {
	local command=$1

    case "$command" in
        version)   
            message_version
            exit;;
    esac
}

# handling -command
function handle_command_opt() {
	local command=$1
	
    case "$command" in
        -)
            handle_command_optarg "$OPTARG"
            ;;
         h)  
            message_usage
            exit;;
         v)  
            message_version
            exit;;
         *)  
            # Invalid Option
            message_usage
            exit;;
    esac
}

# handling --command
function handle_command_optarg() {
	local command=$1
	
    case "$command" in
        version) 
            message_version
            exit;;
        help) 
            message_usage
            exit;;
        observe)
            do_observe
            exit;;
        reply)
            loop_reply
            exit;;
        *) 
            # Invalid Option
            message_usage
            exit;;
    esac
}

function get_options_from_arguments() {  
	# get argument length	
	[[ $# -eq "0" ]] && message_usage && exit;
	 
    # ! : indirect expansion
    while [[ -n "${!OPTIND}" ]]; do
        handle_command_plain "${!OPTIND}"

        while getopts "vh-:" OPT; do
             handle_command_opt "$OPT"
        done

        shift $OPTIND
        OPTIND=1
    done
    
	# Invalid Option
    message_usage
    exit
}
{% endhighlight %}

**Source**:
*	[github.com/.../cupubot/.../options.bash][dotfiles-options]

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

### What is Next ?

We will add some chat group tools such as

*	Greet New Member.

*	Text Logger.

*	HTML Logger.

Just have a look at the next article.

*	[Telegram Bot - BASH Group Tools][local-bash-group]

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/cupubot/tree/master/loop/bash' %}

[local-bash-group]:    /code/2018/01/26/telegram-bot-loop-bash.html

[dotfiles-main]:     {{ dotfiles_path }}/main.bash
[dotfiles-messages]: {{ dotfiles_path }}/messages.bash
[dotfiles-options]:  {{ dotfiles_path }}/options.bash

[image-cli-s-usage]:  {{ asset_path }}/cupubot-cli-script-usage.png
