---
layout: post
title:  "Telegram Bot, Getting Token"
categories: code
date:   2018-01-24 09:35:15 +0700
tags: [coding, API]
author: epsi

excerpt:
  Getting Telegram Token from @botfather.

---

### Preparation

> Goal: Getting Telegram Token

Before You begin, you need to get a telegram bot token

*	Subscribe to [@botfather](https://telegram.me/BotFather), in your telegram.

*	Make a /newbot, in @botfather.

*	Save your token, somewhere in your notebook.

I'aint looking down to my reader.
This part is a little bit too average, 
too easy to do, seemed (sort of) pretty regular.
The thing is, I have to put this step here,
because we can't code without this token.

-- -- --

### Start

Let's get it started. Open your telegram client,
and Subscribe to [@botfather](https://telegram.me/BotFather)

![@botfather: Telegram Bot: start][image-cli-start]{: .img-responsive }

Click <code>start</code> to begin.

-- -- --

### Getting Token

Here is my conversation log.

{% highlight conf %}
% /newbot

$ Alright, a new bot. How are we going to call it? Please choose a name for your bot.

% Cupu Bot

$ Good. Now let's choose a username for your bot. It must end in `bot`. Like this, for example: TetrisBot or tetris_bot.

% cupu_cupu_bot

$ Done! Congratulations on your new bot. 
$ Use this token to access the HTTP API:
your_token_here
{% endhighlight %}

![@botfather: Telegram Bot: newbot][image-cli-newbot]{: .img-responsive }

-- -- --

### Set Description

Here is my conversation log.

{% highlight conf %}
% /setdescription

$ Choose a bot to change description.

% @cupu_cupu_bot

$ OK. Send me the new description for the bot. People will see this description when they open a chat with your bot, in a block titled 'What can this bot do?'.

% Cupu the experimental bot.

$ Success! Description updated. /help
{% endhighlight %}

![@botfather: Telegram Bot: description][image-cli-description]{: .img-responsive }

-- -- --

### Set User Picture

	Avatar is important for me

Here is my conversation log.

{% highlight conf %}
% /setuserpic

$ Choose a bot to change profile photo.

% @cupu_cupu_bot

$ OK. Send me the new profile photo for the bot.

% Success! Profile photo updated. /help
{% endhighlight %}

![@botfather: Telegram Bot: setuserpic][image-cli-setuserpic]{: .img-responsive }

-- -- --

There above are some my logs,
that I put together, so I won't have
any problems finding it in the future.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[local-overview]: /code/2017/04/23/overview-pipe-and-fork.html

[image-cli-start]:       {{ asset_path }}/botfather_start.png
[image-cli-newbot]:      {{ asset_path }}/botfather_newbot.png
[image-cli-description]: {{ asset_path }}/botfather_description.png
[image-cli-setuserpic]:  {{ asset_path }}/botfather_setuserpic.png
