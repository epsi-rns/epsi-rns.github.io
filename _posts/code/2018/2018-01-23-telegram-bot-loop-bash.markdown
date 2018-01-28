---
layout: post
title:  "Telegram Bot - BASH Script"
categories: code
date:   2018-01-23 09:17:35 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.
  Using loop with BASH script.
  No webhook in BASH series.

---

{% include post/2018/01/toc-telegram-bot.html %}

### Bashful Bot

> Goal: Create a Modularized Script

When you are in the mood,
the growth of code become escalated quickly.
We know, how, a long script can looks complicated. 
Before you know it, it become unmaintainable.
Even with BASH, it is a good idea to start with,
an example of code skeleton.

Single script also good when it comes to other thing such as performance.
The thing is, I have a brain issue, especially when reading long script,
I suddenly feeling stupid in the corner.

	Feeling stupid, alone in the corner

So let's compartmentalize each part into chunk.
Make a separate script is good to understand how it works.

#### Preparation

Before You begin, you need to get a telegram bot token.
It is already discussed in previous article.

#### Previous Guidance

We have already see a few bash command tricks in previous article.
Now, let's gather these commands together into a script.
Continue the previous lesson.

-- -- --

### Our Very First Script

Every journey has a begining. 
This is a script, only to observe the telegram update.

<code>01-main-simple</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- config -- 

# $token variable here in config.sh
config_file=~/.config/cupubot/config.sh

if [ ! -f $config_file ];
then
    echo "Config not found!" && exit 0
else
    source $config_file
fi

tele_url="https://api.telegram.org/bot${token}"

### -- main -- 

updates=$(curl -s "${tele_url}/getUpdates")
count_update=$(echo $updates | jq -r ".result | length") 
    
for ((i=0; i<$count_update; i++)); do
    update=$(echo $updates | jq -r ".result[$i]")
    echo "$update\n"
done
{% endhighlight %}

#### How does it works ?

What matters here is the for loop (using C++ style).

{% highlight bash %}
for ((i=0; i<$count_update; i++)); do
    ...
done
{% endhighlight %}

#### Execute

Say something with your bot in your smartphone.
And run the script.

{% highlight bash %}
% ./01-main-simple.bash
{% endhighlight %}

![BASH: Telegram Bot: Observe Script][image-cli-s-observe]{: .img-responsive }

-- -- --

### Simple Script with Section

We can rewrite this script in a more elegant fashion.
Just like mowadays coding, using function,
and separate it with sections.

<code>02-main-single</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- config -- 

# $token variable here in config.sh
config_file=~/.config/cupubot/config.sh

if [ ! -f $config_file ];
then
    echo "Config not found!" && exit 0
else
    source $config_file
fi

tele_url="https://api.telegram.org/bot${token}"

### -- task -- 

function process_observe() {
	local i update
    local updates=$(curl -s "${tele_url}/getUpdates")
    local count_update=$(echo $updates | jq -r ".result | length") 
    
    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        echo "$update\n"
    done
}

### -- controller --

function do_observe() {
	# no loop
    process_observe
} 

### -- main --

do_observe
{% endhighlight %}

Do not worry about the controller,
It will be more clear later,
after some few codes.

#### Execute

Run the script.
No need to say something with your bot in your smartphone.

{% highlight bash %}
% ./02-main-single.bash
{% endhighlight %}

-- -- --

### Simple Modular Script

Now, consider compartmentalize each section,
refactor each chunk into script.

<code>03-main-modular</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- module --

DIR=$(dirname "$0")
. ${DIR}/03-config.bash
. ${DIR}/03-controller.bash
. ${DIR}/03-task-observe.bash

### -- main --

do_observe
{% endhighlight %}

<code>03-config</code>

{% highlight bash %}
#!/usr/bin/env bash
# no need sha-bang for the script to run,
# but needed, so file manager can detect its type.

### -- config -- 

# $token variable here in config.sh
config_file=~/.config/cupubot/config.sh

if [ ! -f $config_file ];
then
    ## exit success (0)
    echo "Config not found!" && exit 0 
else
    source $config_file
fi

tele_url="https://api.telegram.org/bot${token}"
{% endhighlight %}

<code>03-controller.</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- task controller --

function do_observe() {
    process_observe
} 
{% endhighlight %}

<code>03-task-observe</code>

{% highlight bash %}
#!/usr/bin/env bash

function process_observe() {
	local i update
    local updates=$(curl -s "${tele_url}/getUpdates")
    local count_update=$(echo $updates | jq -r ".result | length") 
    
    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        echo "$update\n"
    done
}
{% endhighlight %}

#### Execute

Run the script.
No need to say something with your bot in your smartphone.

{% highlight bash %}
% ./03-main-modular.bash
{% endhighlight %}

This our first modular skeleton.

-- -- --

### Modular Script without Loop

Now we are ready to a more useful Telegram Bot API,
to reply a message. Just one message.
We are going to reply many messages later in a loop.

Now, consider compartmentalize each section,
refactor each chunk into script.

<code>04-main-noloop</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- module --

DIR=$(dirname "$0")
. ${DIR}/03-config.bash         # no change
. ${DIR}/04-controller.bash
. ${DIR}/03-task-observe.bash   # no need
. ${DIR}/04-task-reply.bash

### -- main --

do_reply

{% endhighlight %}

<code>04-controller.</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- task controller --

function do_observe() {
    process_observe
} 

function do_reply() {
    process_reply
} 

{% endhighlight %}

<code>04-task-reply</code>

We can summarize all previous lessons, in this short script.

{% highlight bash %}
#!/usr/bin/env bash

function process_reply() {
	local i update message_id chat_id
    local updates=$(curl -s "${tele_url}/getUpdates")
    local count_update=$(echo $updates | jq -r ".result | length") 
    
    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
    
        message_id=$(echo $update | jq -r ".message.message_id")     
        chat_id=$(echo $update | jq -r ".message.chat.id") 
           
        result=$(curl -s "${tele_url}/sendMessage" \
                  --data-urlencode "chat_id=${chat_id}" \
                  --data-urlencode "reply_to_message_id=${message_id}" \
                  --data-urlencode "text=Thank you for your message."
            );
    done
}
{% endhighlight %}

#### Execute

Run the script.
No need to say something with your bot in your smartphone.

{% highlight bash %}
% ./04-no-loop.bash
{% endhighlight %}

![BASH: Telegram Bot: Simple Script No Loop][image-cli-s-noloop]{: .img-responsive }

I know it looks like it does nothing.
But hey, let's have a look here at the smartphone.

![BASH: Telegram Bot: Simple Script on Smartphone][image-phone-noloop]{: .img-responsive }

-- -- --

### Modular Script with Loop

We are still have to wrap all the messages in a loop,
so all messages can be replied.

	OMG long script !

<code>05-main-loop</code>

{% highlight bash %}
#!/usr/bin/env bash

### -- module --

DIR=$(dirname "$0")
. ${DIR}/05-config.bash
. ${DIR}/05-controller.bash
. ${DIR}/03-task-observe.bash   # no need
. ${DIR}/05-task-reply.bash

### -- main --

loop_reply
{% endhighlight %}

<code>05-config</code>

Add this line to config below <code>tele_url</code>.

{% highlight bash %}
...
tele_url="https://api.telegram.org/bot${token}"

### -- last update --
last_id_file=~/.config/cupubot/id.txt
last_id=0

if [ ! -f $last_id_file ];
then
    touch $last_id_file
    echo 0 > $last_id_file    
else
    last_id=$(cat $last_id_file)
    # echo "last id = $last_id"
fi
{% endhighlight %}

<code>05-controller.</code>

Remove the previous <code>do_reply</code> and change to <code>loop_reply</code>.

{% highlight bash %}
#!/usr/bin/env bash

### -- task controller --

function do_observe() {
    process_observe
} 

function loop_reply() {
    while true; do 
        process_reply   
        sleep 1
    done
}
{% endhighlight %}

<code>05-task-observe</code>

And finally this long script.
This script is self explanatory.
And I also add some hints about global and local variable.

{% highlight bash %}
#!/usr/bin/env bash

function process_reply() {
	# global-project : last_id
	# global-module  : _
	local i update message_id chat_id text

    local updates=$(curl -s "${tele_url}/getUpdates?offset=$last_id")
    local count_update=$(echo $updates | jq -r ".result | length") 
    
    [[ $count_update -eq 0 ]] && echo -n "."

    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")   
        last_id=$(echo $update | jq -r ".update_id")     
        message_id=$(echo $update | jq -r ".message.message_id")    
        chat_id=$(echo $update | jq -r ".message.chat.id") 
        
        get_feedback_reply "$update"
    
        result=$(curl -s "${tele_url}/sendMessage" \
                  --data-urlencode "chat_id=${chat_id}" \
                  --data-urlencode "reply_to_message_id=${message_id}" \
                  --data-urlencode "text=$return_feedback"
            );

        last_id=$(($last_id + 1))            
        echo $last_id > $last_id_file
        
        echo -e "\n: ${text}"
    done
}

function get_feedback_reply() {
	# global-module  : return_feedback
    local update=$1
    
    text=$(echo $update | jq -r ".message.text") 	
	local first_word=$(echo $text | head -n 1 | awk '{print $1;}')
	
	return_feedback='Good message !'
	case $first_word in
        '/id') 
            username=$(echo $update | jq -r ".message.chat.username")
            return_feedback="You are the mighty @${username}"
        ;;
        *)
            return_feedback='Thank you for your message.'            
        ;;
    esac
}
{% endhighlight %}

#### Execute

Run the script.
No need to say something with your bot in your smartphone.

{% highlight bash %}
% ./05-main-loop.bash
{% endhighlight %}

![BASH: Telegram Bot: Simple Script with Loop][image-cli-s-loop]{: .img-responsive }

I know it looks like it does nothing.
But hey, let's have a look here at the smartphone.

![BASH: Telegram Bot: Script Feedback on Smartphone][image-phone-loop]{: .img-responsive }

-- -- --

### How Does it works ?

What matters here are.

#### The while loop.

{% highlight bash %}
while true; do 
    ...
done
{% endhighlight %}

#### The Function, Wrapping Process

{% highlight bash %}
function parse_reply() {
    ...
    for ((i=0; i<$count_update; i++)); do
        ...
    done
}
{% endhighlight %}

#### The Function, Bot Feedback Text

This function, made to handle

* command: /id

* or text

{% highlight bash %}
function get_feedback_reply()
    ...
}
{% endhighlight %}


I think that is all.

-- -- --

### What is Next ?

We will make the script more usable for other people,
by adding <code>help usage </code>.

*	[Telegram Bot - BASH Option Argument][local-bash-argument]

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/cupubot/tree/master/loop/bash' %}

[local-bash-argument]: /code/2018/01/24/telegram-bot-loop-bash.html

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[image-cli-config]:     {{ asset_path }}/cupubot-cli-config.png
[image-cli-getme]:      {{ asset_path }}/cupubot-cli-getme.png
[image-cli-empty]:      {{ asset_path }}/cupubot-cli-getupdate-empty.png
[image-cli-result]:     {{ asset_path }}/cupubot-cli-getupdate-result.png
[image-cli-chat-id]:    {{ asset_path }}/cupubot-cli-chat-id.png
[image-cli-feedback]:   {{ asset_path }}/cupubot-cli-feedback.png
[image-cli-s-observe]:  {{ asset_path }}/cupubot-cli-script-observe.png
[image-cli-s-noloop]:   {{ asset_path }}/cupubot-cli-script-noloop.png
[image-cli-s-loop]:     {{ asset_path }}/cupubot-cli-script-loop.png
[image-phone-feedback]: {{ asset_path }}/cupubot-phone-feedback.png
[image-phone-noloop]:   {{ asset_path }}/cupubot-phone-script-noloop.png
[image-phone-loop]:     {{ asset_path }}/cupubot-phone-script-loop.png

