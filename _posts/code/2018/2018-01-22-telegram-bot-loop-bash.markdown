---
layout: post
title:  "Telegram Bot - BASH CLI"
categories: code
date:   2018-01-22 09:17:35 +0700
tags: [coding, API, bash]
author: epsi

excerpt:
  How to be a Bashful Bot in Telegram.
  Using loop with BASH command.
  No webhook in BASH series.

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

#### Preparation

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
