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

#### Preparation

Before You begin, you need to get a telegram bot token.
It is already discussed in previous article.

#### Previous Guidance

We have already see a few bash command tricks in previous article.
Now, let's gather these commands together into a script

	Continue

-- -- --

### Our Very First Script

Every journey has a begining. 
This is a script, only to observe the telegram update.

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

#### Execute

Say something with your bot in your smartphone.
And run the script.

{% highlight bash %}
% ~/Documents/cupubot/bash/main-observe.bash
{% endhighlight %}

![BASH: Telegram Bot: Observe Script][image-cli-s-observe]{: .img-responsive }

-- -- --

### Simple Script without Loop

We can summarize all previous lessons,
by changing the main parts of the previous script.


{% highlight bash %}
### -- main -- 

updates=$(curl -s "${tele_url}/getUpdates")
count_update=$(echo $updates | jq -r ".result | length") 
    
for ((i=0; i<$count_update; i++)); do
    update=$(echo $updates | jq -r ".result[$i]")
        
    last_id=$(echo $update | jq -r ".update_id")      
    message_id=$(echo $update | jq -r ".message.message_id")     
    chat_id=$(echo $update | jq -r ".message.chat.id") 
           
    result=$(curl -s "${tele_url}/sendMessage" \
                  --data-urlencode "chat_id=${chat_id}" \
                  --data-urlencode "reply_to_message_id=${message_id}" \
                  --data-urlencode "text=Thank you for your message."
        );
done
{% endhighlight %}

What matters here is the for loop (using C++ style).

{% highlight bash %}
for ((i=0; i<$count_update; i++)); do
    ...
done
{% endhighlight %}

#### Execute

Say something again with your bot in your smartphone.
And run the script.

{% highlight bash %}
% ~/Documents/cupubot/bash/main-noloop.bash
{% endhighlight %}

![BASH: Telegram Bot: Simple Script No Loop][image-cli-s-noloop]{: .img-responsive }

I know it looks like it does nothing.
But hey, let's have a look here at the smartphone.

![BASH: Telegram Bot: Simple Script on Smartphone][image-phone-noloop]{: .img-responsive }

-- -- --

### Single Script with Loop

Now we are ready to our loop.
This script is self explanatory.
And I also add some hints on how to debug with commented echo.

	OMG long script !

Do not worry about the length,
we will make it modular later on.

{% highlight bash %}
% #!/usr/bin/env bash

# This is a telegram bot in bash

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

### -- function -- 

function parse_update() {

    updates=$(curl -s "${tele_url}/getUpdates?offset=$last_id")
    # echo $updates | json_reformat

    count_update=$(echo $updates | jq -r ".result | length") 
    # echo $count_update
    
    [[ $count_update -eq 0 ]] && echo -n "."

    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        # echo "$update"
    
        last_id=$(echo $update | jq -r ".update_id") 
        # echo "$last_id"
     
        message_id=$(echo $update | jq -r ".message.message_id") 
        # echo "$message_id"
    
        chat_id=$(echo $update | jq -r ".message.chat.id") 
        # echo "$chat_id"
        
        text=$(echo $update | jq -r ".message.text") 
        # echo "$text"
        
        get_feedback "$text"
    
        result=$(curl -s "${tele_url}/sendMessage" \
                  --data-urlencode "chat_id=${chat_id}" \
                  --data-urlencode "reply_to_message_id=${message_id}" \
                  --data-urlencode "text=$feedback"
            );
        # echo $result | json_reformat

        last_id=$(($last_id + 1))            
        echo $last_id > $last_id_file
        
        echo -e "\n: ${text}"
    done
}

function get_feedback() {
	first_word=$(echo $text | head -n 1 | awk '{print $1;}')
	# echo $first_word
	
	feedback='Good message !'
	case $first_word in
        '/id') 
            username=$(echo $update | jq -r ".message.chat.username")
            feedback="You are the mighty @${username}"
        ;;
        *)
            feedback='Thank you for your message.'            
        ;;
    esac
}

### -- main -- 

while true; do 
    parse_update    
    sleep 1
done
{% endhighlight %}

#### Execute

Again, say something again with your bot in your smartphone.
And run the script.

{% highlight bash %}
% ~/Documents/cupubot/bash/main-loop.bash
{% endhighlight %}

![BASH: Telegram Bot: Simple Script with Loop][image-cli-s-loop]{: .img-responsive }

I know it looks like it does nothing.
But hey, let's have a look here at the smartphone.

![BASH: Telegram Bot: Script Feedback on Smartphone][image-phone-loop]{: .img-responsive }

We can how long script can looks complicated. 

What matters here are 

#### The while loop.

{% highlight bash %}
while true; do 
    ...
done
{% endhighlight %}

#### The Function, Wrapping Process

{% highlight bash %}
function parse_update() {
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
function get_feedback()
    ...
}
{% endhighlight %}
-- -- --

### Modular Script with Loop

I have a brain issue, especially when reading long script,
I suddenly feeling stupid in the corner.

	feeling stupid, alone in the corner

So let's compartmentalize each part into chunk.
Make a separate script is good to understand how it works.
But single script also good when it comes to other thing such as performance.

{% highlight bash %}
#!/usr/bin/env bash
# sha-bang required

# This is a telegram bot in bash

DIR=$(dirname "$0")
. ${DIR}/config.bash
. ${DIR}/functions.bash

### -- main -- 

while true; do 
    parse_update    
    sleep 1
done
{% endhighlight %}

Now it looks simple, right !
What matters here is this line.

{% highlight bash %}
DIR=$(dirname "$0")
{% endhighlight %}

The <code>config.bash</code>:

{% highlight bash %}
#!/usr/bin/env bash
# no need sha-bang for the script to run,
# but needed, so file manager can detect its type.

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

The <code>function.bash</code>:

{% highlight bash %}
#!/usr/bin/env bash
# no need sha-bang for the script to run,
# but needed, so file manager can detect its type.

### -- function -- 

function parse_update() {

    updates=$(curl -s "${tele_url}/getUpdates?offset=$last_id")
    # echo $updates | json_reformat

    count_update=$(echo $updates | jq -r ".result | length") 
    # echo $count_update
    
    [[ $count_update -eq 0 ]] && echo -n "."

    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        # echo "$update"
    
        last_id=$(echo $update | jq -r ".update_id") 
        # echo "$last_id"
     
        message_id=$(echo $update | jq -r ".message.message_id") 
        # echo "$message_id"
    
        chat_id=$(echo $update | jq -r ".message.chat.id") 
        # echo "$chat_id"
        
        text=$(echo $update | jq -r ".message.text") 
        # echo "$text"
        
        get_feedback "$text"
    
        result=$(curl -s "${tele_url}/sendMessage" \
                  --data-urlencode "chat_id=${chat_id}" \
                  --data-urlencode "reply_to_message_id=${message_id}" \
                  --data-urlencode "text=$feedback"
            );
        # echo $result | json_reformat

        last_id=$(($last_id + 1))            
        echo $last_id > $last_id_file
        
        echo -e "\n: ${text}"
    done
}

function get_feedback() {
	first_word=$(echo $text | head -n 1 | awk '{print $1;}')
	# echo $first_word
	
	feedback='Good message !'
	case $first_word in
        '/id') 
            username=$(echo $update | jq -r ".message.chat.username")
            feedback="You are the mighty @${username}"
        ;;
        *)
            feedback='Thank you for your message.'            
        ;;
    esac
}
{% endhighlight %}

#### Execute

{% highlight bash %}
% ~/Documents/cupubot/bash/main-modular.bash
{% endhighlight %}

![BASH: Telegram Bot: Modular Script with Loop][image-cli-s-modular]{: .img-responsive }

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
[image-cli-s-modular]:  {{ asset_path }}/cupubot-cli-script-modular.png
[image-phone-feedback]: {{ asset_path }}/cupubot-phone-feedback.png
[image-phone-noloop]:   {{ asset_path }}/cupubot-phone-script-noloop.png
[image-phone-loop]:     {{ asset_path }}/cupubot-phone-script-loop.png

