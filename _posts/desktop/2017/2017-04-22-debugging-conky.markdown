---
layout     : post
title      : "Debugging Conky"
categories : desktop
date       : 2017-04-22 19:15:15 +0700
tags       : [ricing, statusbar, conky]
keywords   : [dzen2, lua]
author     : epsi
toc        : toc/2017/04/toc-standalone.html

opengraph:
  image: /assets/site/images/topics/lua.png

excerpt:
  A simple trick of Debugging Conky
  
---

<a name="preface"></a>

### Preface

> Goal: Show A simple trick of Debugging Conky

Since we utilize a lot of Conky in Tiling WM Tutorial,
we need to find a way to debug,
just in case we encounter undesired result.

Have fun with this cheap trick.

#### Table of Content

* [Preface](#preface): Table of Content

* 1: [Simple Conky](#simple)

* 2: [Altered Conky](#altered)

* 3: [Real Life Example](#real-life)

* [Conclusion](#conclusion)

-- -- --

<a name="simple"></a>

### 1: Simple Conky

Suppose that you have an original code as below that need to be debugged.

**Source**:

*	[gitlab.com/.../dotfiles/.../conky.lua][dotfiles-conky-simple]

{% highlight lua %}
conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

conky.text = [[\
${time %a %b %d %H:%M:%S}\
]]
{% endhighlight %}

All you need to do is to inject <code>tput cup 0 0</code> in the terminal
to put terminal cursor in top left, for each time conky interval.
The issue is how to transform this tput executable into conky text.
All we need to do is to exec this tput in terminal environment. 

We need to alter this code a bit.

-- -- --

<a name="altered"></a>

### 2: Altered Conky

The debug code after injection of 
additional debugging utilities is as below

**Source**:

*	[gitlab.com/.../dotfiles/.../debug.lua][dotfiles-conky-debug]

{% highlight lua %}
conky.config = {
    out_to_x = false,
    out_to_console = true,
    short_units = true,
    update_interval = 1
}

-- Lua Function Demo 
-- https://github.com/brndnmtthws/conky/issues/62

function exec(command)
    local file = assert(io.popen(command, 'r'))
    local s = file:read('*all')
    file:close()

    s = string.gsub(s, '^%s+', '') 
    s = string.gsub(s, '%s+$', '') 
    s = string.gsub(s, '[\n\r]+', ' ')

    return s
end


function gototopleft()
  return exec('tput cup 0 0') 
end

conky.text = gototopleft() .. [[\
${time %a %b %d %H:%M:%S}\
]]
{% endhighlight %}

Now you can run on terminal.

{% highlight bash %}
$ clear && conky -c ~/Documents/standalone/lang/assets/debug.lua
{% endhighlight %}

And get each conky result, always running from top left terminal.

-- -- --

<a name="real-life"></a>

### 3: Real Life Example

Since the code above just dump too simple date.
We need to something more complex.
Something error prone in need of debugging.

Here is another example of Conky as a feed to Dzen2
<code class="code-file">conky-example/conky.sh</code> and
<code class="code-file">conky-example/conky.lua</code>.

![Dzen2 Decorated Conky Dzen2 Example][image-11-example-02]{: .img-responsive }

**Source**:<br/>

*	[gitlab.com/.../dotfiles/.../conky.sh][dotfiles-conky-deco-debug]

*	[gitlab.com/.../dotfiles/.../conky.lua][dotfiles-conky-deco-debug]


{% highlight bash %}
#!/usr/bin/env bash

clear
conky -c ~/Documents/standalone/dzen2/debug/conky.lua
{% endhighlight %}

Now you can see the raw text before it become feed to dzen2.

![Dzen2 Decorated Conky Dzen2 Example][image-conky-debug]{: .img-responsive }

-- -- --

<a name="conclusion"></a>

### Conclusion

Just another cheap tool to have fun with.
That is all for now.
Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets-desktop/2017/04' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone' %}

[dotfiles-conky-simple]:     {{ dotfiles_path }}/pipe/assets/conky.lua
[dotfiles-conky-debug]:      {{ dotfiles_path }}/pipe/assets/debug.lua
[dotfiles-conky-deco-debug]: {{ dotfiles_path }}/dzen2/debug/conky.lua
[dotfiles-bash-deco-debug]:  {{ dotfiles_path }}/dzen2/debug/conky.lua

[image-11-example-02]: {{ asset_path }}/dzen2-conky-11-example-02-deco.png
[image-conky-debug]:   {{ asset_path }}/conky-debug.png

[local-overview]:    {{ site.url }}/desktop/2017/04/10/standalone-overview.html
[local-dzen2-bash]:  {{ site.url }}/desktop/2017/04/01/standalone-dzen2-bash.html  
[local-dzen2-conky]: {{ site.url }}/desktop/2017/04/11/standalone-dzen2-conky.html
[local-lemon-conky]: {{ site.url }}/desktop/2017/04/14/standalone-lemonbar-conky.html
[local-cli-conky]:   {{ site.url }}/desktop/2017/04/12/standalone-cli-conky.html
[local-xbm-dzen2]:   {{ site.url }}/desktop/2017/04/08/create-xbm-for-dzen2.html
[local-xlfd-font]:   {{ site.url }}/desktop/2017/04/13/getting-xlfd-font.html
[local-debug-conky]: {{ site.url }}/desktop/2017/04/22/debugging-conky.html
