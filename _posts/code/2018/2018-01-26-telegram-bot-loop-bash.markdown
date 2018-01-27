---
layout: post
title:  "Telegram Bot - BASH Group Tools"
categories: code
date:   2018-01-26 09:17:35 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.
  Using loop with BASH script.
  No webhook in BASH series.

---

{% include post/2018/01/toc-telegram-bot.html %}

### Bashful Bot

> Goal: Basic Bot Tools: New Member, Text Logger, Web Logger

We're almost finished.
It is time to connect our lovely Bot to Group.
Like many other project, ideas always comes out.
This is the last part in BASH before we move on to other shell, such as FISH.

These are a few group feature

*	Chat New Member: Greet any new member in group

*	Text Logger: Text only, no images

*	HTML Logger: With user Avatar, no images, nor sticker

I haven't got any time to implement image and sticker.
It is pretty easy actually with Telegram API.
But I'm pretty busy right now.
It is your turn to practice.

For most real group log, I utilize web telegram,
and print to PDF feature from browser.

#### Prerequisite

To enable this group tools.

*	Disable privacy in your chat bot so the chat would listen to all conversations.

*	Add your lovely bot to your chat group.

-- -- --

### Chat New Member

To make it simple I utilize only three script.
<code>main</code>, <code>config</code>, <code>functions</code>.

{% highlight bash %}
#!/usr/bin/env bash

DIR=$(dirname "$0")
. ${DIR}/config-newmember.bash
. ${DIR}/functions-newmember.bash

### -- main -- 

while true; do 
    parse_update
    sleep 1
done
{% endhighlight %}

The config is exactly the same as previous guidance.
This is just a reminder so you do not need to open previous article.

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

But this functions script is a little bit different.

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

        get_feedback "$update"
        
        if [ -n "$update_with_new_member" ];
        then
            echo -e "\n: ${feedback}"

            result=$(curl -s "${tele_url}/sendMessage" \
                      --data-urlencode "chat_id=${chat_id}" \
                      --data-urlencode "text=$feedback"
                );
            # echo $result | json_reformat
        fi

        last_id=$(($last_id + 1))            
        echo $last_id > $last_id_file
    done
}

function get_feedback() {
    local update=$1

    update_with_new_member=$(echo $update | jq -r ".message | select(.new_chat_member != null)")
    if [ -n "$update_with_new_member" ];
    then
        # echo "${update_with_new_member}"
        
        new_chat_member=$(echo $update | jq -r ".message.new_chat_member")
        # echo "$new_chat_member"
        
        first_name=$(echo $new_chat_member | jq -r ".first_name")
        last_name=$(echo $new_chat_member | jq -r ".last_name")
        username=$(echo $new_chat_member | jq -r ".username")
        
        # "😊"
        feedback="Selamat datang di @dotfiles_id 😊, $first_name $last_name @${username}."
    else
        feedback=""
    fi
}
{% endhighlight %}

#### Execute

Let us see it in action.
Add your good friend to a telegram chat group.
No need to say something.
And run the script.

{% highlight bash %}
% ~/Documents/cupubot/bash/new_member/main-newmember.bash
{% endhighlight %}

![BASH: Telegram Bot: New Member Script][image-group-newmember]{: .img-responsive }


#### How does it works ?

This below is the JSON part.
All we need to know is the JSON path <code>message.new_chat_member</code>

{% highlight json %}
{
  "update_id": 140984590,
  "message": {
    "message_id": 1786,
    ...
    "new_chat_member": {
      "id": 216278915,
      "is_bot": false,
      "first_name": "Mas",
      "last_name": "Duwiika",
      "username": "MasDuwiika"
    },
    ...
  }
}
{% endhighlight %}

The bot only responsive, if this path popped out in telegram updates.
And the rest is parsing the path.

Consider, check the smartphone.

![BASH: Telegram Bot: New Member Feedback Script on Smartphone][image-phone-newmember]{: .img-responsive }


-- -- --

### Text Logger

{% highlight bash %}

{% endhighlight %}


{% highlight bash %}

{% endhighlight %}


{% highlight bash %}

{% endhighlight %}

-- -- --

### HTML Logger

{% highlight bash %}

{% endhighlight %}


{% highlight bash %}

{% endhighlight %}


{% highlight bash %}

{% endhighlight %}


-- -- --


I think that's all.

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


[image-group-newmember]: {{ asset_path }}/cupubot-group-newmember.png
[image-phone-newmember]: {{ asset_path }}/cupubot-phone-newmember.png
