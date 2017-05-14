---
layout: post-sidemenu-wm
title:  "Modularized HerbstluftWM in Haskell"
date:   2017-05-08 17:35:15 +0700
categories: desktop
tags: [coding, haskell, herbstluftwm]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using Haskell.
  
---

### Preface

	Goal: Separate Main Flow, Code, and Data.

So anyone can focus to alter special customization in Main Script,
without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[Modularized HerbstluftWM Overview][local-overview]

And also this <code>MapM_</code> tutorial.

*	[Example of Doing Loop in Haskell With Map][local-haskell-loop]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[github.com/.../dotfiles/.../haskell/][dotfiles-haskell-directory]

-- -- --

### Modularized HerbstluftWM in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Modularized Overview ]][local-overview]
[[ BASH ]][local-BASH]
[[ Perl ]][local-Perl]
[[ Python ]][local-python]
[[ Ruby ]][local-Ruby]
[[ PHP ]][local-PHP]
[[ Lua ]][local-Lua]
[[ Haskell ]][local-Haskell]

Source Code Directory:
[[ BASH ]][dotfiles-BASH]
[[ Perl ]][dotfiles-Perl]
[[ Python ]][dotfiles-python]
[[ Ruby ]][dotfiles-Ruby]
[[ PHP ]][dotfiles-PHP]
[[ Lua ]][dotfiles-Lua]
[[ Haskell ]][dotfiles-Haskell]

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
This figure will explain how it looks 
in <code>Haskell script</code> directory.

![HerbstluftWM: Directory Structure][image-haskell-01-tree]{: .img-responsive }

-- -- --

### Modularizing in Haskell

Module in Haskell follow rules in identifier naming.
And the module filename should also follow these rules.

#### Declare a module

Haskell module have to explicitly define what to export.
Anything exported become public in caller script.
And the rest is private to module.

{% highlight lua %}
module MyHelper
( hc
, do_config
, set_tags_with_name
, do_panel
) where
{% endhighlight %}

#### Call a module

{% highlight lua %}
import MyHelper
{% endhighlight %}

-- -- --

### System Calls

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.hs</code>

{% highlight lua %}
hc :: String -> IO ExitCode
hc arguments = system $ "herbstclient " ++ arguments
{% endhighlight %}

<code class="code-file">autostart.hs</code>

{% highlight lua %}
main = do
    -- Read the manual in $ man herbstluftwm
    hc "emit_hook reload"

    -- gap counter
    system $ "echo 35 > /tmp/herbstluftwm-gap"
{% endhighlight %}

-- -- --

### Array: Tag Names and Keys

Is it just me? Or didn't I do googling hard enough ?
I cannot find a way to define array by range in just one line.

<code class="code-file">config.hs</code>

{% highlight lua %}
tag_names = [1..9]
tag_keys  = [1..9] ++ [0]
{% endhighlight %}

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

-- -- --

### Hash: Color Schemes

Using **key-value pairs**, a simple data structure.

<code class="code-file">assets/gmc.hs</code>

{% highlight lua %}
colorSchemes :: [(String, String)]
colorSchemes =
    [("white",     "#ffffff")
    ,("black",     "#000000")

    ,("grey50",     "#fafafa")
    ,("grey100",    "#f5f5f5")
    ]

myColor :: String -> String
myColor key = M.findWithDefault "#ffffff" key (fromList colorSchemes)
{% endhighlight %}

<code class="code-file">autostart.hs</code>

{% highlight lua %}
main = do
    -- background before wallpaper
    system $ "xsetroot -solid '" ++ myColor "blue500" ++ "'"
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../haskell/assets/gmc.hs][dotfiles-haskell-gmc]

Similar Code: 
[[ BASH Color ][dotfiles-bash-gmc]]
[[ Perl Color ][dotfiles-perl-gmc]]
[[ Python Color ][dotfiles-python-gmc]]
[[ Ruby Color ][dotfiles-ruby-gmc]]
[[ PHP Color ][dotfiles-php-gmc]]
[[ Lua Color ][dotfiles-lua-gmc]]
[[ Haskell Color ][dotfiles-haskell-gmc]]

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string concatenation all over the place.

<code class="code-file">config.hs</code>

{% highlight lua %}
-- Modifier variables
s = "Shift"
c = "Control"
m = "Mod4"
a = "Mod1"

keybinds ::  [Pair]
keybinds =
  -- session
    [(m ++ "-" ++ s ++ "-q", "quit")
    ,(m ++ "-" ++ s ++ "-r", "reload")
    ,(m ++ "-" ++ s ++ "-c", "close")
    ]
{% endhighlight %}

This config will be utilized in main script
as shown in the following code.

<code class="code-file">autostart.hs</code>

{% highlight lua %}
main = do
    do_config "keybind"   keybinds
    do_config "keybind"   tagskeybinds
    do_config "mousebind" mousebinds
    do_config "attr"      attributes
    do_config "set"       sets
    do_config "rule"      rules

{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../haskell/config.hs][dotfiles-haskell-config]

Similar Code: 
[[ BASH Config ][dotfiles-bash-config]]
[[ Perl Config ][dotfiles-perl-config]]
[[ Python Config ][dotfiles-python-config]]
[[ Ruby Config ][dotfiles-ruby-config]]
[[ PHP Config ][dotfiles-php-config]]
[[ Lua Config ][dotfiles-lua-config]]
[[ Haskell Config ][dotfiles-haskell-config]]

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.

<code class="code-file">helper.hs</code>

{% highlight lua %}
do_config :: String -> [Pair] -> IO ()
do_config command pairs = do
    -- loop over a hash dictionary of tuples
    mapM_ (\(key, value) -> do 
            hc(command ++ " " ++ key ++ " " ++ value)

            -- uncomment to debug in terminal
            -- putStrLn(command ++ " " ++ key ++ " " ++ value)
          ) pairs   
{% endhighlight %}

#### Reading

Anyone new in Haskell,
should not get intimidated by Haskell operator.
It is just foreach written in Haskell.

I also wrote a tutorial specifically for the function above.
I hope this helps.

*	[Example of Doing Loop in Haskell With Map][local-haskell-loop]

#### Debug Herbstclient Command

I do not remove line where I do debug when I made this script,
so anyone can use it later, avoid examining blindly.
Sometimes strange things happen.
Just uncomment the line to see what happened.

{% highlight lua %}
            putStrLn(command ++ " " ++ key ++ " " ++ value)
{% endhighlight %}

You can see the debugging result in figure below.

![HerbstluftWM: Tag Status][image-hlwm-03-debug-config]{: .img-responsive }

#### View Source File:

*	[github.com/.../dotfiles/.../haskell/helper.hs][dotfiles-haskell-helper]

Similar Code: 
[[ BASH Helper ][dotfiles-bash-helper]]
[[ Perl Helper ][dotfiles-perl-helper]]
[[ Python Helper ][dotfiles-python-helper]]
[[ Ruby Helper ][dotfiles-ruby-helper]]
[[ PHP Helper ][dotfiles-php-helper]]
[[ Lua Helper ][dotfiles-lua-helper]]
[[ Haskell Helper ][dotfiles-haskell-helper]]

-- -- --

### Setting the Tags

For clarity, I break down the process into some function.
This approach works. But you should read **coder issue** below.

<code class="code-file">helper.hs</code>

{% highlight lua %}
get_indices :: [Int] -> [Int]
get_indices l = [0 .. (length l) - 1]

-- Pattern Matching
keybind_keytag :: Int -> Maybe Int -> IO ()
keybind_keytag index Nothing = do return ()
keybind_keytag index (Just key) = do
        hc("keybind Mod4-" ++ show(key) 
                ++ " use_index '" ++ show(index) ++ "'")
        hc("keybind Mod4-Shift-" ++ show(key) 
                ++ " move_index '" ++ show(index) ++ "'")
        return ()

set_tag :: Int -> IO ()
set_tag index = do
    hc("add '" ++ show(tag_names !! index) ++ "'")

    let key = tag_keys !! index
    keybind_keytag index (Just key)
    
    return ()

set_tags_with_name :: IO ()
set_tags_with_name = do
    hc("rename default '" 
        ++ show (tag_names !! 0) ++ "' 2>/dev/null || true")

    mapM_ set_tag (get_indices tag_names)
    
    return ()
{% endhighlight %}

#### Specific Coder Issue

	Instead of wrote some function,
	I wrote everything in IO action.

It was poorly designed, thus took a so many lines.
I will give an update, as soon as I got this right.

Please consider it, as a coder issue, not a Haskell issue.
I'm a n00b in Haskell, and I still need time 
to understand how to write it right in Haskell.
I need to switch my imperative thinking to functional thinking.

	Please accept my apology

-- -- --

### Launch the Panel

Whoaaaa... This is long.
This approach works. But you should read **coder issue** below.

{% highlight lua %}
do_panel :: IO ()
do_panel = do
    -- Source directory is irrelevant in Haskell
    -- So hardcoded, for the sake of learning
    let path = "/.config/herbstluftwm/bash/dzen2/panel.sh"
    
    home <- getHomeDirectory
    let file = home ++ path

    isExist <- doesFileExist file
    permission <- getPermissions file
    let exec_ok = executable permission
    
    -- need to check for executable permission also
    let panel = if(isExist && exec_ok)
            then file    
            else "/etc/xdg/herbstluftwm/panel.sh"
    
    -- uncomment to debug in terminal
    -- putStrLn panel
     
    (_, Just pipeout, _, _) <- 
        createProcess (proc "herbstclient" ["list_monitors"])
        { std_out = CreatePipe } 

    (_, Just cutout, _, ph)  <- 
        createProcess (proc "cut" ["-d:", "-f1"]) 
        { std_in = UseHandle pipeout, std_out = CreatePipe }
        
    raw <- hGetContents cutout   
    _ <- waitForProcess ph

    -- uncomment to debug in terminal       
    -- putStrLn raw
    let monitors = lines raw
    
    mapM_ (\monitor -> do
            system(panel ++ " " ++ show(monitor) ++ " &")
          ) monitors

    return ()
{% endhighlight %}

#### Specific Coder Issue

	Instead of wrote some function,
	I wrote everything in IO action.

It was poorly designed, thus took a so many lines.
I will give an update, as soon as I got this right.

	This code also need some refactoring

Please consider it, as a coder issue, not a Haskell issue.
I'm just a beginner in Haskell, and I still need time 
to understand how to write it right in Haskell.
I need to switch my imperative thinking to functional thinking.

	Again, Please accept my apology

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.hs</code>

{% highlight lua %}
startup_run :: IO ()
startup_run = do
    -- no need to use silent, as the std_err redirected
    let args = ["new_attr", "bool", "my_not_first_autostart"]
    
    (_, _, Just pipeerr, ph) <- 
        createProcess (proc "herbstclient" args)
        { std_err = CreatePipe } 

    capture_err <- hGetContents pipeerr   
    exitcode <- waitForProcess ph

    -- The test may use against, either exitcode or capture_err    
    when (exitcode == ExitSuccess) $ do
     -- non windowed app
        system $ "compton &"
        system $ "dunst &"
        system $ "parcellite &"
        system $ "nitrogen --restore &"
        system $ "mpd &"

     -- windowed app
        system $ "xfce4-terminal &"
        system $ "sleep 1 && firefox &"
        system $ "sleep 2 && geany &"
        system $ "sleep 2 && thunar &"

        return ()
{% endhighlight %}

#### Specific Haskell Advantge

Since the standard eror is redirected,
We can use either exitcodo or standard error,
to determine whether the application should be launch or not.

#### View Source File:

*	[github.com/.../dotfiles/.../haskell/startup.hs][dotfiles-haskell-startup]

Similar Code: 
[[ BASH Startup ][dotfiles-bash-startup]]
[[ Perl Startup ][dotfiles-perl-startup]]
[[ Python Startup ][dotfiles-python-startup]]
[[ Ruby Startup ][dotfiles-ruby-startup]]
[[ PHP Startup ][dotfiles-php-startup]]
[[ Lua Startup ][dotfiles-lua-startup]]
[[ Haskell Startup ][dotfiles-haskell-startup]]

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.hs</code>

{% highlight lua %}
import System.Process

import Assets.MyGMC
import MyConfig
import MyHelper
import MyStartup
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.hs</code>

{% highlight lua %}
main = do
    -- background before wallpaper
    system $ "xsetroot -solid '" ++ myColor "blue500" ++ "'"

    -- Read the manual in $ man herbstluftwm
    hc "emit_hook reload"

    -- gap counter
    system $ "echo 35 > /tmp/herbstluftwm-gap"
    
    -- do not repaint until unlock
    hc "lock"

    -- standard
    hc "keyunbind --all"
    hc "mouseunbind --all"
    hc "unrule -F"

    set_tags_with_name

    -- do hash config
    do_config "keybind"   keybinds
    do_config "keybind"   tagskeybinds
    do_config "mousebind" mousebinds
    do_config "attr"      attributes
    do_config "set"       sets
    do_config "rule"      rules

    -- unlock, just to be sure
    hc "unlock"

    -- launch statusbar panel (e.g. dzen2 or lemonbar)
    do_panel

    -- load on startup
    startup_run
{% endhighlight %}

#### View Source File:

*	[github.com/.../dotfiles/.../haskell/autostart.hs][dotfiles-haskell-autostart]

Similar Code: 
[[ BASH autostart ][dotfiles-bash-autostart]]
[[ Perl autostart ][dotfiles-perl-autostart]]
[[ Python autostart ][dotfiles-python-autostart]]
[[ Ruby autostart ][dotfiles-ruby-autostart]]
[[ PHP autostart ][dotfiles-php-autostart]]
[[ Lua autostart ][dotfiles-lua-autostart]]
[[ Haskell autostart ][dotfiles-haskell-autostart]]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-haskell-loop]: {{ site.url }}/code/2017/05/13/haskell-loop-with-map.html

[image-hlwm-02-tag-status]:  {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]:  {{ asset_path }}/hlwm-03-debug-config.png

[image-haskell-01-tree]:  {{ asset_path }}/hlwm-haskell-01-tree.png

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-bash]:     {{ site.url }}/desktop/2017/05/02/herbstlustwm-modularized-bash.html
[local-perl]:     {{ site.url }}/desktop/2017/05/03/herbstlustwm-modularized-perl.html
[local-python]:   {{ site.url }}/desktop/2017/05/04/herbstlustwm-modularized-python.html
[local-ruby]:     {{ site.url }}/desktop/2017/05/05/herbstlustwm-modularized-ruby.html
[local-php]:      {{ site.url }}/desktop/2017/05/06/herbstlustwm-modularized-php.html
[local-lua]:      {{ site.url }}/desktop/2017/05/07/herbstlustwm-modularized-lua.html
[local-haskell]:  {{ site.url }}/desktop/2017/05/08/herbstlustwm-modularized-haskell.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell

[dotfiles-bash-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/bash
[dotfiles-bash-autostart]: {{ dotfiles_path }}/bash/autostart.sh
[dotfiles-bash-gmc]:       {{ dotfiles_path }}/bash/assets/gmc.sh
[dotfiles-bash-config]:    {{ dotfiles_path }}/bash/config.sh
[dotfiles-bash-helper]:    {{ dotfiles_path }}/bash/helper.sh
[dotfiles-bash-startup]:   {{ dotfiles_path }}/bash/startup.sh

[dotfiles-perl-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/perl
[dotfiles-perl-autostart]: {{ dotfiles_path }}/perl/autostart.pl
[dotfiles-perl-gmc]:       {{ dotfiles_path }}/perl/assets/gmc.pm
[dotfiles-perl-config]:    {{ dotfiles_path }}/perl/config.pm
[dotfiles-perl-helper]:    {{ dotfiles_path }}/perl/helper.pm
[dotfiles-perl-startup]:   {{ dotfiles_path }}/perl/startup.pm

[dotfiles-python-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/python
[dotfiles-python-autostart]: {{ dotfiles_path }}/python/autostart.py
[dotfiles-python-gmc]:       {{ dotfiles_path }}/python/assets/gmc.py
[dotfiles-python-config]:    {{ dotfiles_path }}/python/config.py
[dotfiles-python-helper]:    {{ dotfiles_path }}/python/helper.py
[dotfiles-python-startup]:   {{ dotfiles_path }}/python/startup.py

[dotfiles-ruby-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/ruby
[dotfiles-ruby-autostart]: {{ dotfiles_path }}/ruby/autostart.rb
[dotfiles-ruby-gmc]:       {{ dotfiles_path }}/ruby/assets/gmc.rb
[dotfiles-ruby-config]:    {{ dotfiles_path }}/ruby/config.rb
[dotfiles-ruby-helper]:    {{ dotfiles_path }}/ruby/helper.rb
[dotfiles-ruby-startup]:   {{ dotfiles_path }}/ruby/startup.rb

[dotfiles-php-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/php
[dotfiles-php-autostart]: {{ dotfiles_path }}/php/autostart.php
[dotfiles-php-gmc]:       {{ dotfiles_path }}/php/assets/gmc.php
[dotfiles-php-config]:    {{ dotfiles_path }}/php/config.php
[dotfiles-php-helper]:    {{ dotfiles_path }}/php/helper.php
[dotfiles-php-startup]:   {{ dotfiles_path }}/php/startup.php

[dotfiles-lua-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/lua
[dotfiles-lua-autostart]: {{ dotfiles_path }}/lua/autostart.lua
[dotfiles-lua-gmc]:       {{ dotfiles_path }}/lua/assets/gmc.lua
[dotfiles-lua-config]:    {{ dotfiles_path }}/lua/config.lua
[dotfiles-lua-helper]:    {{ dotfiles_path }}/lua/helper.lua
[dotfiles-lua-startup]:   {{ dotfiles_path }}/lua/startup.lua

[dotfiles-haskell-directory]: https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/haskell
[dotfiles-haskell-autostart]: {{ dotfiles_path }}/haskell/autostart.hs
[dotfiles-haskell-gmc]:       {{ dotfiles_path }}/haskell/Assets/MyGMC.hs
[dotfiles-haskell-config]:    {{ dotfiles_path }}/haskell/MyConfig.hs
[dotfiles-haskell-helper]:    {{ dotfiles_path }}/haskell/MyHelper.hs
[dotfiles-haskell-startup]:   {{ dotfiles_path }}/haskell/MyStartup.hs
