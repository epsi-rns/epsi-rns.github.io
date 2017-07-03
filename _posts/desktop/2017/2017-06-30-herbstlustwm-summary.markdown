---
layout: post-sidemenu-wm
title:  "HerbstluftWM in Seven Languages Summary"
date:   2017-06-30 17:35:15 +0700
categories: desktop
tags: [coding, herbstluftwm, dotfiles]
author: epsi

excerpt:
  Summary of HersbtluftWM script using
  BASH, Perl, Python, Ruby, PHP, Lua, Haskell
  
related_link_ids: 
  - 17050135  # HerbstluftWM Overview
  - 17060135  # Tag Status Overview
  - 17061135  # Event Idle Overview
  - 17042335  # Pipe and Fork Overview

---

Good day dear coder, hello again.

HerbstluftWM has been stealing my attention for the past three months. [Primary] topic is **Modularized HerbstluftWM Configuration**. 

And there are also complementary topics. I have done, implementing herbstluftWM statusbar, 

*	[Part One] contain **tag status decoration** 
	Both Dzen2 and Lemonbar in [Part One], designed specifically for HerbstluftWM's Tag.

*	[Part Two] contain **idle event**. 
	This idle event in [Part Two] give me challenge, and lead me to solve some **Advance Pipe and Fork** issue. 

Now we have *example* code in Seven Languages: BASH, Perl, Python, Ruby, PHP, Lua, and Haskell. I have so much fun playing with the last language. These are the interesting multipart topics:

1.	Idle Event (Advance Pipe and Fork)

2.	Tag Status (Dzen2 and Lemonbar)

3.	Modularized Configuration

4.	Pipe and Fork (Introduction)


	"Once hard, now easy."


>	#standalone #modular #herbstluftwm #lemonbar #dzen2 #pipe #fork #bash #perl #python #ruby #php #lua #haskell

-- -- --

### Screenshot

[![HerbstluftWM: Screenshot Dual Panel][image-ss-hlwm-red-small]{: .img-responsive }][photos-ss-hlwm-red]

-- -- --

### Source Code

HerbstluftWM

*	<https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm>

Dzen2

*	<https://github.com/epsi-rns/dotfiles/tree/master/standalone/dzen2-hlwm>

Lemonbar

*	<https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm>

-- -- --

### Micellanous Links

Dotshare

*	<http://dotshare.it/dots/1498/>

Red Screenshot

*	[photos.google.com Red][photos-ss-hlwm-red1]

Blue Screenshot

*	[photos.google.com Blue][photos-ss-hlwm-blue1]

Original Material Design Wallpaper, with SVG Source (inkscape) Available.

*	<http://nurwijayadi.deviantart.com/art/Flat-Wallpaper-with-Isometric-Looks-646980074>

*	<http://nurwijayadi.deviantart.com/art/Material-Design-Wallpaper-654721928>

-- -- --

### Blog

Overview

*	[Modularized HerbstluftWM Overview][local-modularized-overview]

*	[HerbstluftWM Idle Event Overview][local-event-idle-overview]

*	[HerbstluftWM Tag Status Overview][local-tag-status-overview]

*	[Piping and Forking Overview][local-pipe-fork-overview]

BASH

*	[Modularized HerbstluftWM in BASH][local-modularized-bash]

*	[HerbstluftWM Idle Event in BASH][local-event-idle-bash]

*	[HerbstluftWM Tag Status in BASH][local-tag-status-bash]

*	[Piping and Forking in BASH][local-pipe-fork-bash]

Perl

*	[Modularized HerbstluftWM in Perl][local-modularized-perl]

*	[HerbstluftWM Idle Event in Perl][local-event-idle-perl]

*	[HerbstluftWM Tag Status in Perl][local-tag-status-perl]

*	[Piping and Forking in Perl][local-pipe-fork-perl]

Python

*	[Modularized HerbstluftWM in Python][local-modularized-python]

*	[HerbstluftWM Idle Event in Python][local-event-idle-python]

*	[HerbstluftWM Tag Status in Python][local-tag-status-python]

*	[Piping and Forking in Python][local-pipe-fork-python]

Ruby

*	[Modularized HerbstluftWM in Ruby][local-modularized-ruby]

*	[HerbstluftWM Idle Event in Ruby][local-event-idle-ruby]

*	[HerbstluftWM Tag Status in Ruby][local-tag-status-ruby]

*	[Piping and Forking in Ruby][local-pipe-fork-ruby]

PHP

*	[Modularized HerbstluftWM in PHP][local-modularized-php]

*	[HerbstluftWM Idle Event in PHP][local-event-idle-php]

*	[HerbstluftWM Tag Status in PHP][local-tag-status-php]

*	[Piping and Forking in PHP][local-pipe-fork-php]

Lua

*	[Modularized HerbstluftWM in Lua][local-modularized-lua]

*	[HerbstluftWM Idle Event in Lua][local-event-idle-lua]

*	[HerbstluftWM Tag Status in Lua][local-tag-status-lua]

*	[Piping and Forking in Lua][local-pipe-fork-lua]

Haskell

*	[Modularized HerbstluftWM in Haskell][local-modularized-haskell]

*	[HerbstluftWM Idle Event in Haskell][local-event-idle-haskell]

*	[HerbstluftWM Tag Status in OHaskell][local-tag-status-haskell]

*	[Piping and Forking in Haskell][local-pipe-fork-haskell]

-- -- --

### Complementary

Statusbar

*	[Standalone Lemonbar Using Conky][local-standalone-lemon-conky]

*	[Standalone Dzen2 Statusbar Using Conky][local-standalone-dzen2-conky]

*	[Standalone Dzen2 Statusbar Using BASH][local-standalone-dzen2-bash]

Haskell Coding

*	[Loop in Haskell With Map, Part Three][local-haskell-loop]

*	[Examining Bind in Haskell: Hello World][local-haskell-bind]


-- -- --

**Caveat**: There is still an issue in closing pipe in Ruby. The script is fine, but it happens after the process closed. I still have to kill pipe manually, or it will hogs the CPU process.

-- -- --

### Finally Finished

	"No Bridge is Too Far"

So what's next? I think I'm going to enjoy my HLWM setup for one or a couple of months, then I will moved on to BSPWM. There are still about 20 articles that I have to write. There are so much knowledge that I wish learn. But I'd rather decouple the topics from HerbstluftWM, and write them later on. 

Something in me cannot stop typing codes. But I have to stop anyway. I have grown up reponsibility. I have primary works to do. I have life to live on. And I desire to be back.

	I'll be back.

-- -- --

	I hope that, this material useful.
	Enjoy The Window Manager.
	Enjoy The Statusbar.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign dotfiles_dzen2 = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}
{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}

[local-modularized-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-modularized-bash]:     {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-modularized-perl]:     {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html
[local-modularized-python]:   {{ site.url }}/desktop/2017/05/04/herbstlustwm-modularized-python.html
[local-modularized-ruby]:     {{ site.url }}/desktop/2017/05/05/herbstlustwm-modularized-ruby.html
[local-modularized-php]:      {{ site.url }}/desktop/2017/05/06/herbstlustwm-modularized-php.html
[local-modularized-lua]:      {{ site.url }}/desktop/2017/05/07/herbstlustwm-modularized-lua.html
[local-modularized-haskell]:  {{ site.url }}/desktop/2017/05/08/herbstlustwm-modularized-haskell.html

[local-event-idle-overview]:  {{ site.url }}/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
[local-event-idle-bash]:      {{ site.url }}/desktop/2017/06/12/herbstlustwm-event-idle-bash.html
[local-event-idle-perl]:      {{ site.url }}/desktop/2017/06/13/herbstlustwm-event-idle-perl.html
[local-event-idle-python]:    {{ site.url }}/desktop/2017/06/14/herbstlustwm-event-idle-python.html
[local-event-idle-ruby]:      {{ site.url }}/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html
[local-event-idle-php]:       {{ site.url }}/desktop/2017/06/16/herbstlustwm-event-idle-php.html
[local-event-idle-lua]:       {{ site.url }}/desktop/2017/06/17/herbstlustwm-event-idle-lua.html
[local-event-idle-haskell]:   {{ site.url }}/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html

[local-tag-status-overview]:  {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html
[local-tag-status-bash]:      {{ site.url }}/desktop/2017/06/02/herbstlustwm-tag-status-bash.html
[local-tag-status-perl]:      {{ site.url }}/desktop/2017/06/03/herbstlustwm-tag-status-perl.html
[local-tag-status-python]:    {{ site.url }}/desktop/2017/06/04/herbstlustwm-tag-status-python.html
[local-tag-status-ruby]:      {{ site.url }}/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
[local-tag-status-php]:       {{ site.url }}/desktop/2017/06/06/herbstlustwm-tag-status-php.html
[local-tag-status-lua]:       {{ site.url }}/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
[local-tag-status-haskell]:   {{ site.url }}/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html

[local-pipe-fork-overview]:   {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
[local-pipe-fork-bash]:       {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-pipe-fork-perl]:       {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-pipe-fork-python]:     {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-pipe-fork-ruby]:       {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-pipe-fork-php]:        {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html
[local-pipe-fork-lua]:        {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-pipe-fork-haskell]:    {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html

[image-ss-hlwm-red-small]:  {{ asset_path }}/herbstluftwm-dualpanel.png

[photos-ss-hlwm-blue]: https://lh3.googleusercontent.com/ljvFheUuz-7eR58c3iqngwmVZ7l5HdIRIYZsJNBvyrOW5PSbF0PGPgCuEAuMezumvJkg2HXMWfS-3KJJrMVQO9Ft2etnCh4ur1CXT5PV7G7fbQQqK1X3aBLKfLLb7dWjvSdvR_jkdH6QjfQAhlVrauFxj1C86Zw-nxgITRDgcF12mrWHtzJtVx8s_jN89pTykF17gB8gQWg7rGPEnIlIuqlWZwlrtGeJ5lbWKuqxEcw1IsCfWusH2986IE4GBbRAPn3Wc9w0LxbIP9TPpputL01ETslZxziwAeWDq93HvT_37jyLh0cieQVTN7pzY6A4y-hP1yb3JNjwL1-1u8HNSbcJL3LRyB4hl3dPA4kY594RszuRXFpzEF1OePFm4nctQkCGu19bdqHXRu_W03hlL8DoCnHmLNnFXDORspPgSddbXd1JrjfBZBCLOQmhCcmtsupwGtYHyIUM43_QAnRMR3WqPoSVQxz8u-Ku5kBY0VnBY78iNJVkJoqJA2WBbm8cIAzBRmqF2CMMCa9x3BtEvYW9f2AZbXY-gwEMPJba8ffGF2L0yQQ9oEJp6TeW3cnFOcwR6hKfJAIrlDoGLE8nWO9CRUez_-7nKq5y3mbfGtAc2-EOjRrr6dM4nZ1JA1EWJFWUxa3VD57EemHW8xOeHWi69tDN3YAf9dsqVfT7Og=w1280-h800-no

[photos-ss-hlwm-red]:  https://lh3.googleusercontent.com/Q9Af_TFU9wtq-X6gPdGUMXJKoxfZL6pKXErFRMJwDljnuW45_nqH6LANauwUpfKg3SHwEKEs-Rvy5FnzGna2ZoEUUF8Sb2KYQmsffCG2967e-ZMSk2MUDHRVTO1yv_F70cfh5ONCbUu-vAiEXzw2MkXmYi9Bbj7sHN09YCWyUgzQiNNDPXI5CXnxzWJyv7fwdHrrslDyQEoKWcPrXEH3nOU5PkvuHzZ0Dzxa1uzYreSS0UO6pxD50ut6omurZbzDfkRfGsuX7umwNupYPdmGQNuC0EyDdVg6gm-A_DS8bkj9p8FVEQB7P3sWkiMa3uIoiS1dzAQJRpw_u5DmIt6hP-L-Z815rkTkmnZgyxpggoKGUTuy2k5uXg9RA3TXLMDNDlAsd6F9GYTtdknCVbervXtAY5W6pux-mejG8XIxEydE6m3YmPwBKnJpygGN1spr7895UxZz46tfoZCfHrhEY6_xWbFHOvCi3V2JiWkClwP1GOhnmFFPq2gRoOTGMqNnZZ8CwwCBKpPZf2ZOWcbLG-wBOLvaoY8MjrltFfMcVQctoE5AjP61CjsHEaF3ofdHT2yV5Yvi2i_I2YzUF4_Gv1w9IrPsBQfy0CBcYS4c_9rpOPJ_DD0ckSvdttQiTwFplOwu7mOgRSmZwAtSyNLyISEHBsjbqNmt1GqdzOCFbA=w1280-h800-no

[photos-ss-hlwm-blue1]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOT7-pmGwsLEivr7U4glNkWVUbUgJbWMZ-ytMs7?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[photos-ss-hlwm-red1]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPqMNt9e3_UypKHqASPs_njHBQPX7Kn8X_O9aTp?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn


[local-standalone-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html
[local-standalone-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-standalone-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-standalone-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-standalone-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-standalone-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-standalone-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-standalone-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html

[local-haskell-loop]: {{ site.url }}/code/2017/05/15/haskell-loop-with-map.html
[local-haskell-bind]: {{ site.url }}/code/2017/05/22/examining-bind.html
