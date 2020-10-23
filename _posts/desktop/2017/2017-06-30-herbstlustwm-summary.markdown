---
layout: post-sidemenu-wm
title:  "HerbstluftWM in Seven Languages Summary"
date      : 2017-06-30 17:35:15 +0700
categories: desktop
tags      : [coding, herbstluftwm, dotfiles]
keywords  : [overview, summary]
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

### Preface

Good day dear coder, hello again.

The hardest parts of HerbstluftWM is statusbar.
I have done, implementing HerbstluftWM statusbar, that I can share.
And there are also complementary topics.

*	[Part One] contain **tag status decoration** 
	Both Dzen2 and Lemonbar in [Part One], designed specifically for HerbstluftWM's Tag.

*	[Part Two] contain **idle event**. 
	This idle event in [Part Two] give me challenge, and lead me to solve some **Advance Pipe and Fork** issue.

<iframe width="420" height="315"
src="https://www.youtube.com/embed/erWOZpi8uWU">
</iframe>

>	#standalone #modular #herbstluftwm #lemonbar #dzen2 #pipe #fork

### The Seven Languages

HerbstluftWM has been stealing my attention for the past three months.
[Primary] topic is **Modularized HerbstluftWM Configuration**. 

Now we have *example* code in Seven Languages: BASH, Perl, Python, Ruby, PHP, Lua, and Haskell.
I have so much fun playing with the last language. These are the interesting multipart topics:

1.	Idle Event (Advance Pipe and Fork)

2.	Tag Status (Dzen2 and Lemonbar)

3.	Modularized Configuration

4.	Pipe and Fork (Introduction)

	"Once hard, now easy."

>	#bash #perl #python #ruby #php #lua #haskell

### Screenshot

[![HerbstluftWM: Screenshot Dual Panel][image-ss-hlwm-red-small]{: .img-responsive }][photos-ss-hlwm-red]

### Tiling Window Manager

> Update 2020

![Illustration: Why Tiling Window Manager?][illustration-custom-tiling]

### All Links

You should start with overview, especially pipe and fork, then the rest.

{% include toc/2017/06/herbstulftwm-source-code.html %}

{% include toc/2017/06/herbstulftwm-all-blog-links.html %}

**Caveat**: There is still an issue in closing pipe in Ruby.
The script is fine, but it happens after the process closed.
I still have to kill pipe manually, or it will hogs the CPU process.

{% include toc/2017/06/herbstulftwm-complementary-links.html %}

{% include toc/2017/06/herbstulftwm-miscellanous-links.html %}

### Finally Finished

	"No Bridge is Too Far"

![Giphy Screencast][giphy-img]

So what's next? I think I'm going to enjoy my HLWM setup for one or a couple of months, then I will moved on to BSPWM. There are still about 20 articles that I have to write. There are so much knowledge that I wish learn. But I'd rather decouple the topics from HerbstluftWM, and write them later on. 

Something in me cannot stop typing codes. But I have to stop anyway. I have grown up reponsibility. I have primary works to do. I have life to live on. And I desire to be back.

	I'll be back.

Thank you for reading.

	I hope that, this material useful.
	Enjoy The Window Manager.
	Enjoy The Statusbar.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign dotfiles_dzen2 = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}
{% assign asset_path = site.url | append: '/assets-desktop/2017/06' %}


[image-ss-hlwm-red-small]:  {{ asset_path }}/herbstluftwm-dualpanel.png

[photos-ss-hlwm-blue]: https://lh3.googleusercontent.com/ljvFheUuz-7eR58c3iqngwmVZ7l5HdIRIYZsJNBvyrOW5PSbF0PGPgCuEAuMezumvJkg2HXMWfS-3KJJrMVQO9Ft2etnCh4ur1CXT5PV7G7fbQQqK1X3aBLKfLLb7dWjvSdvR_jkdH6QjfQAhlVrauFxj1C86Zw-nxgITRDgcF12mrWHtzJtVx8s_jN89pTykF17gB8gQWg7rGPEnIlIuqlWZwlrtGeJ5lbWKuqxEcw1IsCfWusH2986IE4GBbRAPn3Wc9w0LxbIP9TPpputL01ETslZxziwAeWDq93HvT_37jyLh0cieQVTN7pzY6A4y-hP1yb3JNjwL1-1u8HNSbcJL3LRyB4hl3dPA4kY594RszuRXFpzEF1OePFm4nctQkCGu19bdqHXRu_W03hlL8DoCnHmLNnFXDORspPgSddbXd1JrjfBZBCLOQmhCcmtsupwGtYHyIUM43_QAnRMR3WqPoSVQxz8u-Ku5kBY0VnBY78iNJVkJoqJA2WBbm8cIAzBRmqF2CMMCa9x3BtEvYW9f2AZbXY-gwEMPJba8ffGF2L0yQQ9oEJp6TeW3cnFOcwR6hKfJAIrlDoGLE8nWO9CRUez_-7nKq5y3mbfGtAc2-EOjRrr6dM4nZ1JA1EWJFWUxa3VD57EemHW8xOeHWi69tDN3YAf9dsqVfT7Og=w1280-h800-no

[photos-ss-hlwm-red]:  https://lh3.googleusercontent.com/Q9Af_TFU9wtq-X6gPdGUMXJKoxfZL6pKXErFRMJwDljnuW45_nqH6LANauwUpfKg3SHwEKEs-Rvy5FnzGna2ZoEUUF8Sb2KYQmsffCG2967e-ZMSk2MUDHRVTO1yv_F70cfh5ONCbUu-vAiEXzw2MkXmYi9Bbj7sHN09YCWyUgzQiNNDPXI5CXnxzWJyv7fwdHrrslDyQEoKWcPrXEH3nOU5PkvuHzZ0Dzxa1uzYreSS0UO6pxD50ut6omurZbzDfkRfGsuX7umwNupYPdmGQNuC0EyDdVg6gm-A_DS8bkj9p8FVEQB7P3sWkiMa3uIoiS1dzAQJRpw_u5DmIt6hP-L-Z815rkTkmnZgyxpggoKGUTuy2k5uXg9RA3TXLMDNDlAsd6F9GYTtdknCVbervXtAY5W6pux-mejG8XIxEydE6m3YmPwBKnJpygGN1spr7895UxZz46tfoZCfHrhEY6_xWbFHOvCi3V2JiWkClwP1GOhnmFFPq2gRoOTGMqNnZZ8CwwCBKpPZf2ZOWcbLG-wBOLvaoY8MjrltFfMcVQctoE5AjP61CjsHEaF3ofdHT2yV5Yvi2i_I2YzUF4_Gv1w9IrPsBQfy0CBcYS4c_9rpOPJ_DD0ckSvdttQiTwFplOwu7mOgRSmZwAtSyNLyISEHBsjbqNmt1GqdzOCFbA=w1280-h800-no

[photos-ss-hlwm-blue1]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipOT7-pmGwsLEivr7U4glNkWVUbUgJbWMZ-ytMs7?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[photos-ss-hlwm-red1]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipPqMNt9e3_UypKHqASPs_njHBQPX7Kn8X_O9aTp?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[giphy-512]: https://giphy.com/gifs/modular-standalone-herbstluftwm-l4FGmHTcdl3R4jGxi
[giphy-img]: https://media.giphy.com/media/l4FGmHTcdl3R4jGxi/giphy.gif

[deviant-hlwm]: http://nurwijayadi.deviantart.com/art/HerbstluftWM-in-Seven-Languages-690831034

[local-standalone-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html

[local-standalone-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-standalone-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-standalone-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-standalone-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html

[illustration-custom-tiling]:   {{ site.url }}/assets/posts/desktop/2020/customization-tiling.png

