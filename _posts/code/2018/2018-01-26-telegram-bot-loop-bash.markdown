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

### Change in Main File

<code>main.bash</code>:

{% highlight bash %}
#!/usr/bin/env bash
# This is a telegram bot in bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# module: main
. ${DIR}/config.bash
. ${DIR}/messages.bash
. ${DIR}/options.bash
. ${DIR}/controller.bash

# module: task
. ${DIR}/tasks/observe.bash
. ${DIR}/tasks/reply.bash
. ${DIR}/tasks/new_member.bash
. ${DIR}/tasks/logger_text.bash
. ${DIR}/tasks/logger_html.bash

### -- main --

get_options_from_arguments "$@"
{% endhighlight %}

<code>config.bash</code>:

The config is exactly the same as previous guidance,
except these two lines, that required for logger.

{% highlight bash %}
...

### -- logfile --
log_file_text=~/Documents/logfile.txt
log_file_html=~/Documents/logfile.html
{% endhighlight %}


<code>options.bash</code>:

{% highlight bash %}
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
        new-member)
            loop_newmember
            exit;;
        logger-text)
            do_logger_text
            exit;;
        logger-html)
            do_logger_html
            exit;;
        *) 
            # Invalid Option
            message_usage
            exit;;
    esac
}
{% endhighlight %}

<code>controller.bash</code>:

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

function loop_newmember() {
    while true; do 
        process_newmember   
        sleep 1
    done
}

function do_logger_text() {
    # empty logger file
    [[ -f $log_file_text ]] && rm $log_file_text
	
    process_logger_text
} 

function do_logger_html() {
	
    # empty logger file
    [[ -f $log_file_html ]] && rm $log_file_html
	
cat << EOF >> $log_file_html
<html>
<head>
  <title>Log</title>
  <meta charset="utf-8">
</head>
<body>
  <table>
EOF

process_logger_html

cat << EOF >> $log_file_html
  </table>
</body>
</html>
EOF

} 
{% endhighlight %}

-- -- --

### Chat New Member

This functions script is a little bit different,
compared with the previous example.

<code>config.bash</code>:

{% highlight bash %}
#!/usr/bin/env bash

function process_newmember() {
	# global-project : last_id
	# global-module  : _
	local i update message_id chat_id

    local updates=$(curl -s "${tele_url}/getUpdates?offset=$last_id")
    local count_update=$(echo $updates | jq -r ".result | length") 
    
    [[ $count_update -eq 0 ]] && echo -n "."

    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        last_id=$(echo $update | jq -r ".update_id") 
        message_id=$(echo $update | jq -r ".message.message_id") 
        chat_id=$(echo $update | jq -r ".message.chat.id") 

        local update_with_new_member=$(echo $update | jq -r ".message | select(.new_chat_member != null)")

        if [ -n "$update_with_new_member" ];
        then
            get_feedback_newmember "$update"
            
            # display on standard output
            echo -e "\n: ${return_feedback}"

            result=$(curl -s "${tele_url}/sendMessage" \
                      --data-urlencode "chat_id=${chat_id}" \
                      --data-urlencode "text=$return_feedback"
                );
        fi

        last_id=$(($last_id + 1))            
        echo $last_id > $last_id_file
    done
}

function get_feedback_newmember() {
	# global-module  : return_feedback
    local update=$1

    local new_chat_member=$(echo $update | jq -r ".message.new_chat_member")
        
    local first_name=$(echo $new_chat_member | jq -r ".first_name")
    local last_name=$(echo $new_chat_member | jq -r ".last_name")
    local username=$(echo $new_chat_member | jq -r ".username")
        
    # "😊"
    return_feedback="Selamat datang di @dotfiles_id 😊, $first_name $last_name @${username}."
}
{% endhighlight %}


#### Execute

Let us see it in action.
Add your good friend to a telegram chat group.
No need to say something.
And run the script.

{% highlight bash %}
% ./main.bash --new-member
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

I enjoy making an online tutorial at telegram chat group.
Why not go further, by logging it.
And the script is even simple. It needs no loop.

The functions are entirely different,
compared with the previous one.

{% highlight bash %}
#!/usr/bin/env bash

function process_logger_text() {
	# global-project : last_id
	# global-module  : _
	local i update message
    
    local updates=$(curl -s "${tele_url}/getUpdates")
    local count_update=$(echo $updates | jq -r ".result | length") 

    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        message=$(echo $update | jq -r ".message") 
        last_id=$(echo $update | jq -r ".update_id") 
        
        get_log_text_line "$message"
        echo -e "${return_log_line_text}" | tee -a $log_file_text

        last_id=$(($last_id + 1))            
        # echo $last_id > $last_id_file
    done
}

function get_log_text_line() {
	# global-module  : return_log_line_text
    local message=$1

    local chat_id=$(echo $message | jq -r ".chat.id") 
    local from=$(echo $message | jq -r ".from") 

    local first_name=$(echo $from | jq -r ".first_name")
    local last_name=$(echo $from | jq -r ".last_name")
    local username=$(echo $from | jq -r ".username")

    local unixdate=$(echo $message | jq -r ".date") 
    local textdate=$(date -d @$unixdate +'%H:%M:%S')

    local text=$(echo $message | jq -r ".text") 
    
    local message_is_reply=$(echo $message | jq -r "select(.reply_to_message != null)")
    
    if [ -n "$message_is_reply" ];
    then
       local reply=$(echo $message | jq -r ".reply_to_message") 
       local reply_text=$(echo $reply | jq -r ".text")
       local reply_first_name=$(echo $reply | jq -r ".from.first_name")
       text=":: ~ ${reply_first_name} : ${reply_text}\n\n${text}"
    fi

    return_log_line_text="[ $textdate ] @${username} ~ $first_name $last_name:\n$text\n\n"
}
{% endhighlight %}

#### Execute

Consider see it in action.
Just run the script.

{% highlight bash %}
% ./main.bash --logger-text
{% endhighlight %}

Forgive me for my  old screenshot.
This is before utilizing <code>getopt</code>.

![BASH: Telegram Bot: Logger Text Result][image-group-logger-text]{: .img-responsive }

#### How does it works ?

Just dump all the text messages.
Nothing special here.

-- -- --

### HTML Logger

HTML logging has no big differences with Text logging.
HTML is actually just a text with tag.
It is just, has nicer preview.

The only different is, HTML logger require Avatar images.
Which is, this URL require a few steps of Telegram API.
As an implementation, there is this badass script.

{% highlight bash %}
#!/usr/bin/env bash

function process_logger_html() {
	# global-project : last_id
	# global-module  : _
	local i update message avatar_image
    
    local updates=$(curl -s "${tele_url}/getUpdates")
    local count_update=$(echo $updates | jq -r ".result | length") 
    
    [[ $count_update -eq 0 ]] && echo -n "."

    for ((i=0; i<$count_update; i++)); do
        update=$(echo $updates | jq -r ".result[$i]")
        message=$(echo $update | jq -r ".message") 
        last_id=$(echo $update | jq -r ".update_id") 

        get_avatar "$message"
        get_log_html_line "$message"
        
        # display on standard output
        echo -e "${return_log_line_text}"

        avatar_image="<img src='${return_avatar_url}' height='42' width='42'"
        avatar_image+=" style=' vertical-align: text-top;'>"
        
cat << EOF >> $log_file_html
    <tr>
      <td valign="top">
        ${avatar_image}
      </td>
      <td valign="top">
        ${return_log_line_html}
      </td>
    </tr>
EOF

        last_id=$(($last_id + 1))            
        # echo $last_id > $last_id_file
    done
}

function get_log_html_line() {
	# global-module  : return_log_line_text return_log_line_html
    local message=$1
    
    local from=$(echo $update | jq -r ".message.from") 
    
    local first_name=$(echo $from | jq -r ".first_name")
    local last_name=$(echo $from | jq -r ".last_name")
    local username=$(echo $from | jq -r ".username")

    local unixdate=$(echo $message | jq -r ".date") 
    local textdate=$(date -d @$unixdate +'%H:%M:%S')

    local text=$(echo $message | jq -r ".text") 
    
    local message_is_reply=$(echo $message | jq -r "select(.reply_to_message != null)")
    
    if [ -n "$message_is_reply" ];
    then
       local reply=$(echo $message | jq -r ".reply_to_message") 
       local reply_text=$(echo $reply | jq -r ".text")
       local reply_first_name=$(echo $reply | jq -r ".from.first_name")
       text=":: ~ ${reply_first_name} : ${reply_text}<br/>${text}"
    fi

    return_log_line_text="[ $textdate ] @${username} ~ $first_name $last_name:\n$text\n\n"

    text=$(echo "${text//$'\n'/<br/>}")    
    return_log_line_html="[ $textdate ] @${username} ~ $first_name $last_name:<br/>$text<br/>"
}

function get_avatar() {
	# global-module  : return_avatar_url
    local message=$1   

    local chat_id=$(echo $message | jq -r ".chat.id") 
    local from=$(echo $update | jq -r ".message.from") 

    local from_id=$(echo $from | jq -r ".id")
    local photos=$(curl -s "${tele_url}/getUserProfilePhotos?user_id=$from_id&limit=1")

    local photo=$(echo $photos | jq -r ".result.photos[0][0]")   
    local file_id_photo=$(echo $photo | jq -r ".file_id")
    
    local get_file=$(curl -s "${tele_url}/getFile?file_id=$file_id_photo")
    
    local file_path=$(echo $get_file | jq -r ".result.file_path")   
    local file_id=$(echo $get_file | jq -r ".result.file_id")   
    
    return_avatar_url="https://api.telegram.org/file/bot${token}/$file_path?file_id=$file_id"
    
    # https://api.telegram.org/file/bot<token>/
}
{% endhighlight %}

#### Execute

Consider see it in action.
Just run the script.

{% highlight bash %}
% ./main.bash --logger-html
{% endhighlight %}

And open the result in a browser.

![BASH: Telegram Bot: Logger HTML Result][image-group-logger-html]{: .img-responsive }

#### How does it works ?

Hot to get Avatar URL.

*	First, we need the ID of the user, that we can get from each JSON.

*	Call <code>getUserProfilePhotos</code> API using that ID, to retrieve the <code>file_id</code>.

*	Call <code>getFile</code> API using that <code>file_id</code>, to retrieve the <code>file_path</code>.

*	Compose the file URL, based on Telegram API using <code>token</code>, <code>file_id</code> and <code>file_path</code>.

-- -- --

### Conclusions

I think that's all.

I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = '/assets/posts/code/2018/01' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/cupubot/tree/master/loop/bash' %}

[local-overview]: /code/2017/04/23/overview-pipe-and-fork.html

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[image-group-newmember]:   {{ asset_path }}/cupubot-group-newmember.png
[image-phone-newmember]:   {{ asset_path }}/cupubot-phone-newmember.png
[image-group-logger-text]: {{ asset_path }}/cupubot-logger-text.png
[image-group-logger-html]: {{ asset_path }}/cupubot-logger-html.png
