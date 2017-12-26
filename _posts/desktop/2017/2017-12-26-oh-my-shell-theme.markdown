---
layout: post-sidemenu-wm
title:  "Iterate Oh My BASH Theme"
date:   2017-12-26 09:35:15 +0700
categories: desktop
tags: [ricing]
author: epsi

excerpt:
  Preview each oh-my-bash theme live.
  Also apllicable to oh-my-zsh.
  
related_link_ids: 
  - 17050135  # HerbstluftWM Overview
  - 17060135  # Tag Status Overview
  - 17061135  # Event Idle Overview
  - 17042335  # Pipe and Fork Overview

---

Good day dear ricer, hello again.

### Preface

For a linux lover, I always late, I never use Z Shell, but always BASH or FISH.
And now I decide to use ZSH, just to find out what I never learnt in BASH.
Yes, these are tools that you can learn them all over a night.
It is all intended for end user.

*	[oh-my-bash][github-oh-my-bash] ([ohmybash.github.io][site-oh-my-bash])

*	[oh-my-zsh][github-oh-my-zsh] ([ohmyz.sh][site-oh-my-zsh])

*	[oh-my-fish][github-oh-my-fish]

*	[bash-it][github-bash-it]

-- -- --

### Issue

One of my issue with oh-my-bash is that I cannot find preview for each theme. 
But do not worry, there is a workaround. 
Even better, we can see each theme live, in action for each theme.

If you ever wonder how to get the looks of each oh-my-bash theme easily, I made a script for you to play with. 

Here is the step by step procedure.

*	First, Install [oh-my-bash][github-oh-my-bash]

*	Second, Comment the **#** _OSH_THEME=something_ in .bashrc

*	Third, Try this oneliner code in shell, and then type exit for newly created bash session.

{% highlight bash %}
$ export OSH_THEME='font'; bash
{% endhighlight %}

*	Fourth, Implement that oneliner code as a script as below. 

[![oh-my-bash: Iterate Theme][image-ss-oh-my-bash-source]{: .img-responsive }][photos-ss-oh-my-bash-theme]

*	Fifth, Run the script. And manually type exit for each bash sessions. You can see the output in figure below.

[![oh-my-bash: Iterate Theme][image-ss-oh-my-bash-theme]{: .img-responsive }][photos-ss-oh-my-bash-theme]

Now you are done.

-- -- --

### oh-my-bash Iteration Source

{% highlight bash %}
#/bin/env bash
themes=('90210' 'axin' 'bakke' 'binaryanomaly' 'bobby')

# show theme
for theme in "${themes[@]}"
do
    echo "[ theme = $theme ]"
    export OSH_THEME=$theme; bash
    echo
done
{% endhighlight %}

Of course you can grab all the themes at once.
I made above code simpler, so the reader won't get focus lost.

{% highlight bash %}
themes=('90210' 'axin' 'bakke' 'binaryanomaly' 'bobby' 'bobby-python' 'brainy' 'brunton' 'candy' 'clean' 'cooperkid' 'cupcake' 'demula' 'dos' 'doubletime' 'doubletime_multiline' 'doubletime_multiline_pyonly' 'dulcie' 'duru' 'emperor' 'envy' 'font' 'gallifrey' 'hawaii50' 'iterate' 'kitsune' 'luan' 'mairan' 'mbriggs' 'minimal' 'modern' 'modern-t' 'morris' 'n0qorg' 'nwinkler' 'nwinkler_random_colors' 'pete' 'powerline' 'powerline-multiline' 'powerline-naked' 'powerline-plain' 'primer' 'pro' 'pure' 'purity' 'rainbowbrite' 'rana' 'rjorgenson' 'roderik' 'sexy' 'simple' 'sirup' 'slick' 'standard' 'tonka' 'tonotdo' 'tylenol' 'wanelo' 'zitron' 'zork')
{% endhighlight %}

You can also modify the theme, such as to add character from **powerline extra** font.

[![oh-my-bash: Powerline Extra][image-ss-powerline-extra]{: .img-responsive }][photos-ss-powerline-extra]

-- -- --

### oh-my-zsh Iteration Source

Respectively, it is also applicable with [oh-my-zsh][github-oh-my-zsh].

{% highlight shell %}
#/bin/env zsh
themes=('adben' 'af-magic' 'afowler' 'agnoster' 'alanpeabody')

# show theme
for theme in "${themes[@]}"
do
    echo "[ theme = $theme ]"
    export ZSH_THEME=$theme; zsh
    echo
done
{% endhighlight %}

Of course you can grab all the themes at once.

{% highlight shell %}
themes=('adben' 'af-magic' 'afowler' 'agnoster' 'alanpeabody' 'amuse' 'apple' 'arrow' 'aussiegeek' 'avit' 'awesomepanda' 'bira' 'blinks' 'bureau' 'candy' 'candy-kingdom' 'clean' 'cloud' 'crcandy' 'crunch' 'cypher' 'dallas' 'darkblood' 'daveverwer' 'dieter' 'dogenpunk' 'dpoggi' 'dst' 'dstufft' 'duellj' 'eastwood' 'edvardm' 'emotty' 'essembeh' 'evan' 'example' 'fino' 'fino-time' 'fishy' 'flazz' 'fletcherm' 'fox' 'frisk' 'frontcube' 'funky' 'fwalch' 'gallifrey' 'gallois' 'garyblessington' 'gentoo' 'geoffgarside' 'gianu' 'gnzh' 'gozilla' 'half-life' 'humza' 'imajes' 'intheloop' 'itchy' 'jaischeema' 'jbergantine' 'jispwoso' 'jnrowe' 'jonathan' 'josh' 'jreese' 'jtriley' 'juanghurtado' 'junkfood' 'kafeitu' 'kardan' 'kennethreitz' 'kiwi' 'kolo' 'kphoen' 'lambda' 'linuxonly' 'lukerandall' 'macovsky' 'macovsky-ruby' 'maran' 'mgutz' 'mh' 'michelebologna' 'mikeh' 'miloshadzic' 'minimal' 'mira' 'mortalscumbag' 'mrtazz' 'murilasso' 'muse' 'nanotech' 'nebirhos' 'nicoulaj' 'norm' 'obraun' 'peepcode' 'philips' 'pmcgee' 'pure' 'pygmalion' 're5et' 'refined' 'rgm' 'risto' 'rixius' 'rkj' 'rkj-repos' 'robbyrussell' 'sammy' 'simonoff' 'simple' 'skaro' 'smt' 'Soliah' 'sonicradish' 'sorin' 'sporty_256' 'steeef' 'strug' 'sunaku' 'sunrise' 'superjarin' 'suvash' 'takashiyoshida' 'terminalparty' 'theunraveler' 'tjkirch' 'tjkirch_mod' 'tonotdo' 'trapd00r' 'wedisagree' 'wezm' 'wezm+' 'wuffers' 'xiong-chiamiov' 'xiong-chiamiov-plus' 'ys' 'zhann')
{% endhighlight %}

[![oh-my-zsh: Iterate Theme][image-ss-oh-my-zsh-theme]{: .img-responsive }][photos-ss-oh-my-zsh-theme]

Later I found out that oh-my-zsh has this cool official script that show all themes.

{% highlight shell %}
% ~/.oh-my-zsh/tools/theme_chooser.sh
{% endhighlight %}

-- -- --

Good luck my friend.
Have some fun with your shell.

Thank you for reading.

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/12' %}

[github-oh-my-bash]: https://github.com/ohmybash/oh-my-bash
[github-oh-my-zsh]:  https://github.com/robbyrussell/oh-my-zsh
[github-oh-my-fish]: https://github.com/oh-my-fish/oh-my-fish
[github-bash-it]:    https://github.com/Bash-it/bash-it

[site-oh-my-bash]: https://ohmybash.github.io/
[site-oh-my-zsh]: http://ohmyz.sh/

[image-ss-oh-my-bash-theme]:  {{ asset_path }}/oh-my-bash-themes-iterate.png
[image-ss-oh-my-bash-source]: {{ asset_path }}/oh-my-bash-themes-iterate-source.png
[image-ss-powerline-extra]:   {{ asset_path }}/oh-my-bash-themes-powerline-extra.png
[image-ss-oh-my-zsh-theme]:   {{ asset_path }}/oh-my-zsh-themes-iterate.png

[photos-ss-oh-my-bash-theme]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNM3CN5Z2aPUFC3PnhkMUMwK5PPiVIojB7Y3M2A?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[photos-ss-oh-my-zsh-theme]:  https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMqlaB0q_YuoxegAgXM_TGo-zSyqBmgM8gBDHGT?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[photos-ss-powerline-extra]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipNUgpuTy6IxP4j7yZ2q7oVrJ5a1X4dqmNbHA94_?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
