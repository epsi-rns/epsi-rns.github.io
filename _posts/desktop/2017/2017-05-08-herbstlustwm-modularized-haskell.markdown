---
layout: post
title:  "Modularized HerbstluftWM in Haskell"
date      : 2017-05-08 17:35:15 +0700
categories: desktop
tags      : [coding, haskell, herbstluftwm]
keywords  : [modularized]
author: epsi

excerpt:
  Doing Hersbtluft WM Config using Haskell.
  
related_link_ids: 
  - 17050135  # HerbstluftWM Overview
  - 17050235  # HerbstluftWM BASH
  - 17050335  # HerbstluftWM Perl
  - 17050435  # HerbstluftWM Python
  - 17050535  # HerbstluftWM Ruby
  - 17050635  # HerbstluftWM PHP
  - 17050735  # HerbstluftWM Lua
  - 17050835  # HerbstluftWM Haskell

---

### Preface

> Goal: Separate Main Flow, Code, and Data.

So anyone can focus to alter special customization in Main Script,
without changing the whole stuff.

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[Modularized HerbstluftWM Overview][local-overview]

And also this <code>MapM_</code> tutorial.

*	[Example of Doing Loop in Haskell With Map][local-haskell-loop]

It seems to be I'm not the only person who used Haskell with HerbsluftWM.

*	https://hackage.haskell.org/package/hlwm#readme

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	[gitlab.com/.../dotfiles/.../haskell/][dotfiles-haskell-directory]

-- -- --

{% include post/2017/05/herbstlustwm-modularized-language.md %}

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

{% highlight haskell %}
module MyHelper
( hc
, do_config
, set_tags_with_name
, do_panel
) where
{% endhighlight %}

#### Call a module

{% highlight haskell %}
import MyHelper
{% endhighlight %}

-- -- --

### System Calls

Here we wrap <code>herbstclient</code> system call
in a function named <code>hc</code>.

<code class="code-file">helper.hs</code>

{% highlight haskell %}
hc :: String -> IO ExitCode
hc arguments = system $ "herbstclient " ++ arguments
{% endhighlight %}

<code class="code-file">autostart.hs</code>

{% highlight haskell %}
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

{% highlight haskell %}
tag_names = [1..9]
tag_keys  = [1..9] ++ [0]
{% endhighlight %}

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

-- -- --

### Hash: Color Schemes

Using **key-value pairs**, a simple data structure.

<code class="code-file">gmc.hs</code>

{% highlight haskell %}
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

{% highlight haskell %}
main = do
    -- background before wallpaper
    system $ "xsetroot -solid '" ++ myColor "blue500" ++ "'"
{% endhighlight %}

#### View Source File:

*	[gitlab.com/.../dotfiles/.../haskell/gmc.hs][dotfiles-haskell-gmc]

{% include post/2017/05/herbstlustwm-modularized-gmc.md %}

-- -- --

### Hash: Config

The Hash in Config is very similar with the colors above.
Except that it has string concatenation all over the place.

<code class="code-file">config.hs</code>

{% highlight haskell %}
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

{% highlight haskell %}
main = do
    do_config "keybind"   keybinds
    do_config "keybind"   tagskeybinds
    do_config "mousebind" mousebinds
    do_config "attr"      attributes
    do_config "set"       sets
    do_config "rule"      rules

{% endhighlight %}

#### View Source File:

*	[gitlab.com/.../dotfiles/.../haskell/config.hs][dotfiles-haskell-config]

{% include post/2017/05/herbstlustwm-modularized-config.md %}

-- -- --

### Processing The Hash Config

**This is the heart of this script**.
 
This <code>do-config</code> function has two arguments,
the herbstclient command i.e "keybind", and hash from config.

<code class="code-file">helper.hs</code>

{% highlight haskell %}
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

{% highlight haskell %}
            putStrLn(command ++ " " ++ key ++ " " ++ value)
{% endhighlight %}

You can see the debugging result in figure below.

[![HerbstluftWM: Debug Command][image-hlwm-03-debug-config]{: .img-responsive }][photo-hlwm-03-debug-config]

#### View Source File:

*	[gitlab.com/.../dotfiles/.../haskell/helper.hs][dotfiles-haskell-helper]

{% include post/2017/05/herbstlustwm-modularized-helper.md %}

-- -- --

### Setting the Tags

For clarity, I break down the process into some function.

{% highlight haskell %}
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

Using lambda syntatic sugar and pattern matching.

<code class="code-file">helper.hs</code>

{% highlight haskell %}
set_tags_with_name :: IO ()
set_tags_with_name = do
    hc("rename default '" 
        ++ show (tag_names !! 0) ++ "' 2>/dev/null || true")

    mapM_ (\index -> do
            hc("add '" ++ show(tag_names !! index) ++ "'")
    
            -- uncomment to debug in terminal
            -- putStrLn $ show index

            let key = tag_keys !! index
            case (Just key) of
                Nothing   -> do return()
                Just akey -> do
                    hc("keybind Mod4-" ++ show(akey) 
                        ++ " use_index '" ++ show(index) ++ "'")
                    hc("keybind Mod4-Shift-" ++ show(akey) 
                        ++ " move_index '" ++ show(index) ++ "'")
                    return ()

        ) ([0 .. (length tag_names) - 1]) -- indices
{% endhighlight %}

-- -- --

### Launch the Panel

The panel code is short, but the detail is long.
We have to extract value from IO action

*	<code>panelFilename</code>: IO String.

*	<code>listMonitors</code>: IO [String].


{% highlight haskell %}
do_panel :: IO ()
do_panel = do
    panel <- panelFilename
    monitors <- listMonitors
    
    mapM_ (\monitor -> do
            system(panel ++ " " ++ show(monitor) ++ " &")
          ) monitors

    return ()
{% endhighlight %}

<code>panelFilename</code> IO action:

{% highlight haskell %}
customFilename :: String -> String
customFilename home = home ++ path     where path = "/.config/herbstluftwm/haskell/panel-lemonbar"
    where path = "/.config/herbstluftwm/bash/dzen2/panel.sh"
    
panelFilename :: IO String
panelFilename = do
    home <- getHomeDirectory
    let file = customFilename home

    isExist <- doesFileExist file
    permission <- getPermissions file
    let exec_ok = executable permission
    
    -- need to check for executable permission also
    let panel = if(isExist && exec_ok)
            then file    
            else "/etc/xdg/herbstluftwm/panel.sh"
    
    -- uncomment to debug in terminal
    -- putStrLn panel
    
    return panel
{% endhighlight %}

<code>listMonitors</code> IO action:

{% highlight haskell %}
listMonitors :: IO [String]
listMonitors = do
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
    let monitors = lines raw  -- or splitOn instead

    return monitors
{% endhighlight %}

Yes it is very long compared with BASH/Perl/Python counterpart.

-- -- --

### Run Baby Run

This is the last part.
It is intended to be modified.
Everyone has their own personal preferences.

<code class="code-file">startup.hs</code>

{% highlight haskell %}
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

*	[gitlab.com/.../dotfiles/.../haskell/startup.hs][dotfiles-haskell-startup]

{% include post/2017/05/herbstlustwm-modularized-startup.md %}

-- -- --

### Putting It All Together.

The last part is going to main script
and putting it all back together.

	Now the flow is clear

<code class="code-file">Header Part: autostart.hs</code>

{% highlight haskell %}
import System.Process

import MyGMC
import MyConfig
import MyHelper
import MyStartup
{% endhighlight %}

<code class="code-file">Procedural Part: autostart.hs</code>

{% highlight haskell %}
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

*	[gitlab.com/.../dotfiles/.../haskell/autostart.hs][dotfiles-haskell-autostart]

{% include post/2017/05/herbstlustwm-modularized-autostart.md %}

-- -- --

### Coming up Next

After the Window Manager, comes the Panel.

*	[HerbstluftWM Tag Status in Haskell][local-haskell-tag-status]

*	[HerbstluftWM Event Idle in Haskell][local-haskell-event-idle]

-- -- --

Happy Configuring.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/05' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/herbstluftwm' %}

[local-haskell-tag-status]:   {{ site.url }}/desktop/2017/06/08/herbstlustwm-tag-status-haskell.html
[local-haskell-event-idle]:   {{ site.url }}/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html

[image-ss-hlwm-nopanel]: {{ asset_path }}/herbstluftwm-nopanel.png
[photo-ss-hlwm-nopanel]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipM5QN3sl9KsZs3xlb87UivcHZeLGbSuk2Z8Jx0W?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn

[image-hlwm-02-tag-status]:   {{ asset_path }}/hlwm-02-tag-status.png
[image-hlwm-03-debug-config]: {{ asset_path }}/hlwm-03-debug-config-half.png
[photo-hlwm-03-debug-config]: https://photos.google.com/share/AF1QipMO53TtSJVXrkn8R0s4wre4QWgX7_G5CoaSkFMneVHFp9Tu5STBmdjW3M3fpA2eEw/photo/AF1QipMG-pilS2yhwarThAT23bBYV54z3rYcs2wnaL0E?key=WGIySDVOaVpibkJCRkV5NWVZUUs3UnNLNHR1MVpn
[image-haskell-01-tree]:      {{ asset_path }}/hlwm-haskell-01-tree.png

[local-overview]: {{ site.url }}/desktop/2017/05/01/herbstlustwm-modularized-overview.html
[local-haskell-loop]: {{ site.url }}/code/2017/05/13/haskell-loop-with-map.html

[dotfiles-haskell-directory]: https://gitlab.com/epsi-rns/dotfiles/tree/master/herbstluftwm/haskell
[dotfiles-haskell-autostart]: {{ dotfiles_path }}/haskell/autostart.hs
[dotfiles-haskell-gmc]:       {{ dotfiles_path }}/haskell/MyGMC.hs
[dotfiles-haskell-config]:    {{ dotfiles_path }}/haskell/MyConfig.hs
[dotfiles-haskell-helper]:    {{ dotfiles_path }}/haskell/MyHelper.hs
[dotfiles-haskell-startup]:   {{ dotfiles_path }}/haskell/MyStartup.hs
