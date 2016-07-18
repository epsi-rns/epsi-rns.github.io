---
layout: post
title:  "Create Your Own Monitoring Script"
date:   2016-06-15 22:53:15 +0700
categories: opensource
tags: [shell]
author: epsi

excerpt:
  Conky is good for desktop decoration. And watch is pretty in Console.
  Using only bash script only to make our monitoring script more transparent.
  
related_link_ids: 
  - 16051102  # XMonad with Conkyless Dzen

---

Somebody asked in Debian group about speed monitoring application.
There is a good application for this called conky or watch.

Conky is good for desktop decoration. And watch is pretty in Console.
For a lightweight portable script,
you need to consider to remove any conky/watch dependency.

Actually we can simplified the process for our own use.
Using bash script only, and get rid of conky,
to make our monitoring script more transparent.

-- -- --

## Monitor in loop

The basic of monitoring is pool
a data result for each monitoring interval.

{% highlight bash %}
 $ watch date
{% endhighlight %}

You can do this from bash with this simple while loop.

{% highlight bash %}
 $ while sleep 1; do date; done
{% endhighlight %}

I know it is weird to have a date while sleeping.

-- -- --

## Sample Script, Net Speed

I remember two months ago I have found bash script,
combined with answer from stackoverflow,
I have rewritten this script for my own purpose.

This script utilize statistic form <code class="code-file">/sys/class/net/</code> for dzen2 feed.

Original Source

* [github.com/ksevelyar/.../dzen_main.sh][link-ksevelyar]

* [stackoverflow.com/.../how-to-get-percentage-of-processor-use-with-bash][link-stackoverflow]

Rewrite

* [github.com/epsi-rns/dotfiles/.../chunk_net_speed.sh][dotfiles-net-speed]

With slight modification.
I remove the dzen stuff.
And change the echo to printf.
And prepend carriage return.

{% highlight bash %}
printf "\r[$RX_text] [$TX_text]"
{% endhighlight %}

-- -- --

## Complete Script

{% highlight bash %}
#!/bin/bash

interface=$(iw dev | grep Interface | awk '{print $2}')

if [ "$interface" ]; then 

  # Read first datapoint
  read TX_prev < /sys/class/net/$interface/statistics/tx_bytes
  read RX_prev < /sys/class/net/$interface/statistics/rx_bytes

  sleep 1

  # Read second datapoint

  read TX_curr < /sys/class/net/$interface/statistics/tx_bytes
  read RX_curr < /sys/class/net/$interface/statistics/rx_bytes

  # compute 
  TX_diff=$((TX_curr-TX_prev))
  RX_diff=$((RX_curr-RX_prev))

  # printout var
  TX_text=$(echo "scale=1; $TX_diff/1024" | bc | awk '{printf "%.1f", $0}')
  RX_text=$(echo "scale=1; $RX_diff/1024" | bc | awk '{printf "%.1f", $0}')

  printf "\r[$RX_text] [$TX_text]      "

fi; 

exit 0
{% endhighlight %}


You can see the result in figure below

[![BASH Monitoring Script][image-monitoring-script]{: .img-responsive }][picasa-monitoring-script]

-- -- --

Now you can use, modify, 
and extend this script for your own purpose.

Thank you for reading





[//]: <> ( -- -- -- links below -- -- -- )

[link-ksevelyar]: https://github.com/ksevelyar/dotfiles/blob/master/xmonad/dzen/status_bars/dzen_main.sh
[link-stackoverflow]: https://stackoverflow.com/questions/26791240/how-to-get-percentage-of-processor-use-with-bash
[dotfiles-net-speed]: https://github.com/epsi-rns/dotfiles/blob/master/xmonad/xmonad-dzen-2/assets/bin/chunk_net_speed.sh

[image-monitoring-script]: {{ site.url }}/assets/posts/opensource/2016/06/monitoring-script.png
[picasa-monitoring-script]: https://lh3.googleusercontent.com/-onz8jZY_IKM/V2pLU0tMP8I/AAAAAAAAAW0/ayjKc6CrqiItMQM-9YBVbAeod0cmrqxmQCCo/s0/monitoring-script.png
