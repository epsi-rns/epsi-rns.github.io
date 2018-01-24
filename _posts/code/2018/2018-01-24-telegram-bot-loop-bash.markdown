---
layout: post
title:  "Telegram Bot, Using Loop, with BASH"
categories: code
date:   2018-01-24 09:35:15 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.

---

### Bashful Bot

> Goal: Parsing JSON feed from Telegram API in BASH console

Telegram Bot is one of the interesting hype I met.
Its API is simple and easy to use.
Even a bashful coder can do it, in spare time.

At first I want to use telebot as a name of my project.
But there are already too many project using telebot as a repository name.
So I decide to use a unique name <code>cupubot</code>.

	Cupu is an Indonesian slank language for LAME

This guidance is using terminal.

-- -- --

### Preparation

Before You begin, you need to get a telegram bot token.
It is already discussed in previous article.



-- -- --

### Make a config

Since we are using terminal we can just write

{% highlight bash %}
% token='your_token_here'
{% endhighlight %}

But consider a more elegant way, use config.
We should separate secret stuff, such as token and password,
outside the console, and outside the script.

{% highlight bash %}
% mkdir ~/.config/cupubot
% touch ~/.config/cupubot/config.sh 
% echo "token='your_token_here'" > ~/.config/cupubot/config.sh 
% source ~/.config/cupubot/config.sh
% echo $token
{% endhighlight %}

![BASH: Telegram Bot: config][image-cli-config]{: .img-responsive }

-- -- --

### Telegram Bot API

	https://api.telegram.org/bot<token>/METHOD_NAME

You should read the official document first:

* [Telegram Bot API](https://core.telegram.org/bots/api)

Consider do this.

{% highlight bash %}
% tele_url="https://api.telegram.org/bot${token}"
{% endhighlight %}

We are going to use this <code>$tele_url</code> many times.

Now do our very first API call.
Telegram is using <code>JSON</code> as their format.

{% highlight bash %}
% curl -s "${tele_url}/getMe"
{"ok":true,"result":{"id":547870369,"is_bot":true,"first_name":"epsi_bot","username":"epsi_bot"}}
{% endhighlight %}

We need a little helper here to make the json output looks tidy.
The <code>json_reformat</code>.

{% highlight bash %}
% curl -s "${tele_url}/getMe" | json_reformat
{
    "ok": true,
    "result": {
        "id": 547870369,
        "is_bot": true,
        "first_name": "epsi_bot",
        "username": "epsi_bot"
    }
}
{% endhighlight %}

![BASH: Telegram Bot: getMe API with json_reformat][image-cli-getme]{: .img-responsive }

If the <code>json_reformat</code> does not exist in your system,
you should install it first.

-- -- --

### Get Updates

This is the heart of the API.

You can do it with <code>json_reformat</code>.
Note that the result could be different
if you are already open your bot.

{% highlight bash %}
% curl -s "${tele_url}/getUpdates" | json_reformat
{
    "ok": true,
    "result": [

    ]
}
{% endhighlight %}

Now you can get the value in JSON with <code>jq</code>.
<code>jq</code> is like walking in a DOM in HTML.
If the <code>jq</code> does not exist in your system,
you should install it first.

{% highlight bash %}
% % curl -s "${tele_url}/getUpdates" | jq -r ".ok"
true
{% endhighlight %}

![BASH: Telegram Bot: getupdates API with jq JSON][image-cli-empty]{: .img-responsive }

-- -- --

### Start The Bot

It is okay if you are already start the bot.
I just want to show you how the <code>jq</code>
walk through JSON.

Consider say something in your bot,
such as <code>Hello World</code>.
And pipe the output to <code>jq</code>.
You can see that we can isolate the output for certain JSON value.

{% highlight bash %}
% curl -s "${tele_url}/getUpdates" | jq -r ".result"
[
  {
    "update_id": 408173037,
    "message": {
      "message_id": 122,
      "from": {
        "id": 507894652,
        "is_bot": false,
        "first_name": "Epsi",
        "last_name": "Sayidina",
        "username": "RizqiSayidina",
        "language_code": "en-US"
      },
      "chat": {
        "id": 507894652,
        "first_name": "Epsi",
        "last_name": "Sayidina",
        "username": "RizqiSayidina",
        "type": "private"
      },
      "date": 1516742659,
      "text": "Hello World"
    }
  }
]
{% endhighlight %}

Consider see the nice color output.

![BASH: Telegram Bot: JSON Result][image-cli-result]{: .img-responsive }

Note that getUpdates method is plural.
It fetch array of messages.

-- -- --

### Chat ID

A feedback to telegram, need certain variable, such as <code>chat_id</code>.
We can easly get it using <code>jq</code>.

{% highlight bash %}
% % curl -s "${tele_url}/getUpdates" | jq -r ".result[].message.chat.id"
507894652
{% endhighlight %}

Showing is not enough, we need to store it in variable,
using special bash command subtitution form <code>$(...)</code>.

{% highlight bash %}
% chat_id=$(curl -s "${tele_url}/getUpdates" | jq -r ".result[0].message.chat.id")
{% endhighlight %}

Now we can show the variable.

{% highlight bash %}
% echo $chat_id
507894652
{% endhighlight %}

![BASH: Telegram Bot: JSON Result][image-cli-chat-id]{: .img-responsive }

-- -- --

### Sending Feedback

Don't be silence,
people would think that the bot is down.

{% highlight bash %}
% curl -s "${tele_url}/sendMessage?chat_id=${chat_id}" \
  --data-urlencode "text=Thank you for visiting" | json_reformat
{
    "ok": true,
    "result": {
        "message_id": 123,
        "from": {
            "id": 547870369,
            "is_bot": true,
            "first_name": "epsi_bot",
            "username": "epsi_bot"
        },
        "chat": {
            "id": 507894652,
            "first_name": "Epsi",
            "last_name": "Sayidina",
            "username": "RizqiSayidina",
            "type": "private"
        },
        "date": 1516743765,
        "text": "Thank you for visiting"
    }
}
{% endhighlight %}

![BASH: Telegram Bot: Send Feedback Message][image-cli-feedback]{: .img-responsive }

Note that sendMessage method is singular.
It send one feedback at a time.
We are going to do it in loop script later to reply all messages.

Now it is a good time to see the reply message,
in your smartphone or any telegram client.

![BASH: Telegram Bot: Feedback Message on Smartphone][image-phone-feedback]{: .img-responsive }

Before we are going to move from console to script,
we can empty the update by using <code>offset</code>,
just add one (or bigger number) to the <code>update_id</code> value.

{% highlight bash %}
% curl -s "${tele_url}/getUpdates?offset=408173039"
{"ok":true,"result":[]}
{% endhighlight %}

-- -- --

### Simple Script without Loop

We can summarize all lessons above to this short script.

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

Say something again with your bot in your smartphone.
And run the script.

{% highlight bash %}
% ~/Documents/cupubot/bash/cupubot-noloop.bash
{% endhighlight %}

![BASH: Telegram Bot: Simple Script No Loop][image-cli-s-noloop]{: .img-responsive }

I know it looks like it does nothing.
But hey, let's have a look here at the smartphone.

![BASH: Telegram Bot: Simple Script on Smartphone][image-phone-noloop]{: .img-responsive }

-- -- --

### Script with Loop

Now we are ready to our final script.
This script is self explanatory.
And I also add some hints on how to debug with commented echo.

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

Again, say something again with your bot in your smartphone.
And run the script.

{% highlight bash %}
% ~/Documents/cupubot/bash/cupubot.bash
{% endhighlight %}

![BASH: Telegram Bot: Simple Script with Loop][image-cli-s-loop]{: .img-responsive }

I know it looks like it does nothing.
But hey, let's have a look here at the smartphone.

![BASH: Telegram Bot: Script Feedback on Smartphone][image-phone-loop]{: .img-responsive }

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

[image-cli-config]:     {{ asset_path }}/cupubot-cli-config.png
[image-cli-getme]:      {{ asset_path }}/cupubot-cli-getme.png
[image-cli-empty]:      {{ asset_path }}/cupubot-cli-getupdate-empty.png
[image-cli-result]:     {{ asset_path }}/cupubot-cli-getupdate-result.png
[image-cli-chat-id]:    {{ asset_path }}/cupubot-cli-chat-id.png
[image-cli-feedback]:   {{ asset_path }}/cupubot-cli-feedback.png
[image-cli-s-noloop]:   {{ asset_path }}/cupubot-cli-script-noloop.png
[image-cli-s-loop]:     {{ asset_path }}/cupubot-cli-script-loop.png
[image-phone-feedback]: {{ asset_path }}/cupubot-phone-feedback.png
[image-phone-noloop]:   {{ asset_path }}/cupubot-phone-script-noloop.png
[image-phone-loop]:     {{ asset_path }}/cupubot-phone-script-loop.png
