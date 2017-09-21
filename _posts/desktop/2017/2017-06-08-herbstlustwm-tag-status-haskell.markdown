---
layout: post
title:  "HerbstluftWM Tag Status in Haskell"
date:   2017-06-08 17:35:15 +0700
categories: desktop
tags: [coding, haskell, herbstluftwm, statusbar]
author: epsi

excerpt:
  HersbtluftWM Tag Status using Dzen2 or Lemonbar.
  Modularized implementation in Haskell script.

related_link_ids: 
  - 17060135  # Tag Status Overview
  - 17060235  # Tag Status BASH
  - 17060335  # Tag Status Perl
  - 17060435  # Tag Status Python
  - 17060535  # Tag Status Ruby
  - 17060635  # Tag Status PHP
  - 17060735  # Tag Status Lua
  - 17060835  # Tag Status Haskell
  - 17060935  # Tag Status Global Notes
  
---

### Preface

> Goal: Show the Herbstclient Tag.

	Focusing in "herbstclient tag_status". 

![HerbstluftWM: Tag Status][image-hlwm-02-tag-status]{: .img-responsive }

This tutorial cover Lemonbar, and in order to use Dzen2,
any reader could use the source code in github.

-- -- --

#### Reading

Before you jump off to scripting,
you might desire to read this overview.

*	[HerbstluftWM Tag Status Overview][local-overview]

*	[Modularized HerbstluftWM in Haskell][local-haskell-config]

*	[Piping and Forking in Haskell][local-haskell-pipe]

#### All The Source Code:

Impatient coder like me, like to open many tab on browser.

*	**Dzen2**: 
	[github.com/.../dotfiles/.../haskell/][dotfiles-dzen2-haskell]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../haskell/][dotfiles-lemon-haskell]

-- -- --

{% include post/2017/06/herbstlustwm-tag-status-language.md %}

-- -- --

### Screenshot

Since window manager is out of topic in this tutorial,
I present **only panel** HerbstluftWM screenshot.

#### Dzen2

![Statusbar: Dzen2 Screenshot][image-hlwm-ss-dzen2]{: .img-responsive }

#### Lemonbar

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

-- -- --

### Directory Structure

Directory Structure has been explained in preface. 
For both Dzen2 and Lemonbar, the structure are the same.
This figure will explain how it looks
in <code>Haskell script</code> directory.

![Statusbar: Directory Structure][image-01-tree-haskell]{: .img-responsive }

Special customization can be done in output script,
without changing the whole stuff.

-- -- --

### Get Geometry

Let's have a look at <code class="code-file">helper.hs</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../haskell/helper.sh][dotfiles-dzen2-haskell-helper]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../haskell/helper.sh][dotfiles-lemon-haskell-helper]

{% include post/2017/06/herbstlustwm-tag-status-helper.md %}

#### Get Script Argument

The original herbstluftwm  panel example,
contain statusbar for each monitor.
The default is using monitor 0,
although you can use other monitor as well.

{% highlight bash %}
$ ./panel.hs 0
{% endhighlight %}

I do not implement statusbar in multi monitor since I only have my notebook.
But I'll pass the argument anyway for learning purpose.

Here it is our code in Haskell.
<code class="code-file">helper.hs</code>
Using guards as an alternate to ternary operator.

{% highlight haskell %}
module MyHelper (getMonitor) where

getMonitor :: [String] -> Int
getMonitor args
  | length(args) > 0 = read (args !! 0) :: Int
  | otherwise        = 0
{% endhighlight %}

In order to use <code>getArgs</code>,
we need to import <code>System.Environment</code> module

{% highlight haskell %}
import System.Environment
import MyHelper
{% endhighlight %}

Now in main code we can call

{% highlight haskell %}
main = do
    args <- getArgs
    let monitor = getMonitor args
    putStrLn $ show monitor
{% endhighlight %}

This will display <code>0</code> or else such as <code>1</code>,
depend on the script argument given.

{% highlight conf %}
0
{% endhighlight %}

#### Get Monitor Geometry

HerbstluftWM give this little tools to manage monitor geometry
by getting monitor rectangle.

{% highlight bash %}
$ herbstclient monitor_rect
{% endhighlight %}

This will show something similar to this.

{% highlight conf %}
0 0 1280 800
{% endhighlight %}

![HerbstluftWM: Monitor Rectangle][image-hlwm-02-monitor-rect]{: .img-responsive }

Consider wrap the code into function.
And get an array as function return.
This function need a few standard library.


*	In order to use <code>createProcess</code>,
	we need to import <code>System.Process</code> module.

*	In order to use <code>hGetContents</code>,
	we also need to import <code>System.IO</code> module.

*	In order to use <code>exitSuccess</code>,
	we also need to import <code>System.Exit</code> module.

*	In order to use <code>when</code>,
	we also need to import <code>Control.Monad</code> module.

<code class="code-file">helper.hs</code>

{% highlight haskell %}
module MyHelper
( getMonitor
, getGeometry
) where

import System.Process
import System.IO
import System.Exit

import Control.Monad

getGeometry :: Int -> IO [Int]
getGeometry monitor = do 
    let args = ["monitor_rect", show(monitor)]

    (_, Just pipe_out, _, ph) <- 
        createProcess (proc "herbstclient" args)
        { std_out = CreatePipe } 
        
    raw <- hGetContents pipe_out   
    _ <- waitForProcess ph
    
    when (raw == "") $ do
        putStrLn $ "Invalid monitor " ++ show(monitor)
        exitSuccess

    let geometry = map (read::String->Int) (words raw)
    
    return geometry
{% endhighlight %}

Consider call this function from script later.
The <code>show</code> enable Integer to String conversion.

To print array in Haskell, we just have to wrap it in
<code>intercalate " " $ map show geometry</code>.
In order to use <code>intercalate</code>,
we need to import <code>Data.List</code> module.

{% highlight haskell %}
import System.Environment
import Data.List
import MyHelper

main = do
    args <- getArgs
    let monitor = getMonitor args
        
    geometry <- getGeometry monitor
    putStrLn $ show geometry
    putStrLn $ intercalate " " $ map show geometry
{% endhighlight %}

This will produce

{% highlight conf %}
0 0 1280 800
{% endhighlight %}

#### Get Panel Geometry

The Panel geometry is completely depend on the user flavor and taste.
You can put it, on top, or bottom, or hanging somewhere.
You can create gap on both left and right.

Consider this example:
<code class="code-file">helper.hs</code>
We import <code>XYWH constructor</code>
in <code>data XYWH</code> using
<code>module MyHelper (XYWH (XYWH)) where </code>

{% highlight haskell %}
module MyHelper
( getMonitor
, getGeometry
, XYWH (XYWH)
, getTopPanelGeometry
, getBottomPanelGeometry
) where

data XYWH = XYWH String String String String

getBottomPanelGeometry :: Int -> [Int] -> XYWH
getBottomPanelGeometry 
    height geometry = XYWH 
                      (show ((geometry !! 0) + 24))
                      (show ((geometry !! 3) - height))
                      (show ((geometry !! 2) - 48))
                      (show height)
{% endhighlight %}

We are going to use this <code>X Y W H</code>,
to get lemonbar parameter.

{% highlight haskell %}
import System.Environment
import MyHelper

panelHeight = 24

main = do
    args <- getArgs
    let monitor = getMonitor args
        
    geometry <- getGeometry monitor
    let XYWH xpos ypos width height = getBottomPanelGeometry 
                                      panelHeight geometry 

    putStrLn $ "Lemonbar geometry: " ++ width ++ "x" ++ height
            ++ "+" ++ xpos ++ "+" ++ ypos
{% endhighlight %}

This will show something similar to this result,
depend on your monitor size.

{% highlight conf %}
Lemonbar geometry: 1280x24+24+776
{% endhighlight %}

#### Get Lemonbar Parameters

We almost done. 
This is the last step.
We wrap it all inside this function below.

<code class="code-file">helper.hs</code>

{% highlight haskell %}
getLemonParameters :: Int -> [Int] -> [String]
getLemonParameters 
    panelHeight geometry = [
          "-g", geom_res,  "-u", "2",
          "-B", bgcolor, "-F", fgcolor,
          "-f", font_takaop,
          "-f", font_awesome,
          "-f", font_symbol
        ]
      where
        -- calculate geometry
        XYWH xpos ypos width height = getBottomPanelGeometry 
                                      panelHeight geometry        

        -- geometry: -g widthxheight++y
        geom_res = width ++ "x" ++ height
            ++ "+" ++ xpos ++ "+" ++ ypos

        -- color, with transparency    
        bgcolor = "#aa000000"
        fgcolor = "#ffffff"
        
        -- XFT: require lemonbar_xft_git 
        font_takaop  = "takaopgothic-9"
        font_bottom  = "monospace-9"
        font_symbol  = "PowerlineSymbols-11"
        font_awesome = "FontAwesome-9"
{% endhighlight %}

-- -- --

### Testing The Parameters

Consider this code <code class="code-file">01-testparams.hs</code>.
The script call the above function to get lemon parameters.

{% highlight haskell %}
import System.Environment
import Data.List
import MyHelper

panelHeight = 24

main = do
    args <- getArgs
    let monitor = getMonitor args
        
    geometry <- getGeometry monitor
    let lemonParameters = getLemonParameters panelHeight geometry
    
    putStrLn $ intercalate " " $ lemonParameters
{% endhighlight %}

This will produce output
something similar to this result

{% highlight conf %}
-g 1232x24+24+776 -u 2 -B #aa000000 -F #ffffff 
-f takaopgothic-9 -f FontAwesome-9 -f PowerlineSymbols-11
{% endhighlight %}

Or in Dzen2 version:

{% highlight conf %}
-x 0 -y 0 -w 1280 -h 24 -ta l 
-bg #000000 -fg #ffffff -title-name dzentop 
-fn -*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*
{% endhighlight %}

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../haskell/01-testparams.hs][dotfiles-dzen2-haskell-testparams]

*	**Lemon**: 
	[github.com/.../dotfiles/.../haskell/01-testparams.hs][dotfiles-lemon-haskell-testparams]

{% include post/2017/06/herbstlustwm-tag-status-testparams.md %}

-- -- --

### Adjusting the Desktop

Since we want to use panel, we have to adjust the desktop gap,
giving space at the top and bottom.

{% highlight bash %}
$ herbstclient pad 0 24 0 24 0
{% endhighlight %}

For more information, do <code>$ man herbsluftclient</code>,
and type <code>\pad</code> to search what it means.

In script, it looks like this below.

{% highlight haskell %}
main = do
    system $ "herbstclient pad " ++ show(monitor) ++ " "
        ++ show(panelHeight) ++ " 0 " ++ show(panelHeight) ++ " 0"
{% endhighlight %}

-- -- --

### Color Schemes

Using a simple data structure **key-value pairs**,
we have access to google material color
for use with dzen2 or lemonbar.
Having a nice pallete to work with,
makes our panel more fun.

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

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../haskell/gmc.hs][dotfiles-dzen2-haskell-gmc]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../haskell/gmc.hs][dotfiles-lemon-haskell-gmc]

{% include post/2017/06/herbstlustwm-tag-status-gmc.md %}

-- -- --

### Preparing Output

Let's have a look at <code class="code-file">output.hs</code> in github.

#### View Source File:

*	**Dzen2**: 
	[github.com/.../dotfiles/.../haskell/output.hs][dotfiles-dzen2-haskell-output]

*	**Lemonbar**: 
	[github.com/.../dotfiles/.../hasell/output.hs][dotfiles-lemon-haskell-output]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

-- -- --

### Global Variable and Constant

Haskell designed not to have a global variable.

#### Simulate Mutable State

Officialy there is a no way to define global variable Haskell.
But Haskell provide a few workaround to simulate mutable state.
The easiest one is using <code>unsafePerformIO</code>,
and currently that also the only method I understand.

Haskell distinct clearly between global constant (immutable)
and global variable (mutable).
While with immutable global constant we can make function easily,
with mutable global variable, action is unavoidable.

The issue with using <code>unsafePerformIO</code> is,
"it is an IO action".
Therefore we need <code>Functor <$> operator</code>
to make the action works with function.

	By using global variable, IO action is unavoidable


#### Mutable State: Segment Variable

The different between interval based and event based is that,
with interval based all panel segment are recalculated,
while with event based only recalculate the trigerred segment.

In this case, we only have two segment in panel.

*	Tag

*	Title

<code class="code-file">output.hs</code>
In script, we initialize the variable as below

{% highlight haskell %}
segmentWindowtitle :: IORef String
segmentWindowtitle = unsafePerformIO $ newIORef "" -- empty string

tagsStatus :: IORef [String]
tagsStatus = unsafePerformIO $ newIORef []         -- empty string 
{% endhighlight %}

Each segment buffered.
And will be called while rendering the panel.

#### Global Constant: Tag Name 

Assuming that herbstclient tag status
only consist of nine number element.

{% highlight bash %}
$ herbstclient tag_status
	#1	:2	:3	:4	:5	.6	.7	.8	.9	
{% endhighlight %}

We can manage custom tag names,
consist of nine string element.
We can also freely using *unicode* string instead of plain one.

<code class="code-file">output.hs</code>

{% highlight haskell %}
tagShows :: [String] 
tagShows = ["一 ichi", "二 ni", "三 san", "四 shi", 
    "五 go", "六 roku", "七 shichi", "八 hachi", "九 kyū", "十 jū"]
{% endhighlight %}

#### Global Constant: Decoration

<code class="code-file">output.hs</code>
Decoration consist lemonbar formatting tag.

{% highlight haskell %}
import MyGMC

-- decoration
separator = "%{B-}%{F" ++ myColor "yellow500" ++ "}|%{B-}%{F-}"

-- Powerline Symbol
rightHardArrow = "\57520"
rightSoftArrow = "\57521"
leftHardArrow  = "\57522"
leftSoftArrow  = "\57523"

-- theme
preIcon    = "%{F" ++ myColor "yellow500" ++ "}"
postIcon   = "%{F-}"
{% endhighlight %}

-- -- --

### Segment Variable

As response to herbstclient event idle,
these two function set the state of segment variable.

<code class="code-file">output.hs</code>
This is an IO action.

{% highlight haskell %}
setTagValue :: Int -> IO ()
setTagValue monitor = do
    let args = ["tag_status", show(monitor)]

    (_, Just pipe_out, _, ph) <- 
        createProcess (proc "herbstclient" args)
        { std_out = CreatePipe } 
        
    raw <- hGetContents pipe_out   
    _ <- waitForProcess ph

    let statusList = words raw
    writeIORef tagsStatus statusList
{% endhighlight %}

This function above turn the tag status string
into array of tags for later use.

<code class="code-file">output.hs</code>
This is also an IO action.

{% highlight haskell %}
setWindowtitle :: String -> IO ()
setWindowtitle windowtitle = do
    let icon = preIcon ++ "\61444" ++ postIcon
    let text = " " ++ icon ++ " %{B-}"
               ++ "%{F" ++ myColor "grey700" ++ "} " ++ windowtitle
    writeIORef segmentWindowtitle text
{% endhighlight %}

We will call these two functions later.

-- -- --

### Decorating: Window Title

This is self explanatory.
I put separator, just in case you want to add other segment.
And then returning string as result.

<code class="code-file">output.hs</code>
This is an IO action.

{% highlight haskell %}
outputByTitle :: IO String
outputByTitle = do
    segment <- readIORef segmentWindowtitle
    let text  = segment ++ " " ++ separator ++ "  "

    return text
{% endhighlight %}

-- -- --

### Decorating: Tag Status

This transform each plain tag such as <code>.2</code>,
to decorated tag names such as <code>二 ni</code>.
Note that it only process one tag.
We process all tags in a loop in other function.

This has some parts:

*	Pre Text: Color setting for Main Text
	(Background, Foreground, Underline).
	Arrow before the text, only for active tag.

*	Main Text: Tag Name by number,
	each with their tag state <code>#</code>, <code>+</code>,
	<code>.</code>, <code>|</code>, <code>!</code>,
	and each tag has clickable area setting.

*	Post Text: 
	Arrow after the text, only for active tag.

*	Color Reset: <code>%{B-}</code>,
	<code>%{F-}</code>, <code>%{-u}</code>
	(Background, Foreground, Underline).

<code class="code-file">output.hs</code>
This is an function. Although it looks long, 
there is no sequence of command within this function,

{% highlight haskell %}
outputByTag :: Int -> String -> String
outputByTag monitor tagStatus = 
    textPre ++ textName ++ textPost ++ textClear
  where
    -- text = ''

    tagIndex  = drop 1 tagStatus 
    tagMark   = take 1 tagStatus 
    index     = (read::String->Int) tagIndex - 1     -- zero based
    tagName   = tagShows !! index

    ----- pre tag
    
    textPre   = case tagMark of
        "#" -> "%{B" ++ myColor "blue500" ++ "}"
            ++ "%{F" ++ myColor "black" ++ "}"
            ++ "%{U" ++ myColor "white" ++ "}%{+u}" 
            ++ rightHardArrow
            ++ "%{B" ++ myColor "blue500" ++ "}"
            ++ "%{F" ++ myColor "white" ++ "}"
            ++ "%{U" ++ myColor "white" ++ "}%{+u}"
        "+" -> "%{B" ++ myColor "yellow500" ++ "}"
            ++ "%{F" ++ myColor "grey400" ++ "}"
        ":" -> "%{B-}"
            ++"%{F" ++ myColor "white" ++ "}"
            ++ "%{U" ++ myColor "red500" ++ "}%{+u}"
        "!" -> "%{B" ++ myColor "red500" ++ "}"
            ++ "%{F" ++ myColor "white" ++ "}"
            ++ "%{U" ++ myColor "white" ++ "}%{+u}"
        _   -> "%{B-}"
            ++ "%{F" ++ myColor "grey600" ++ "}%{-u}"

    ----- tag by number
    
    -- clickable tags
    textName  = "%{A:herbstclient focus_monitor \"" 
        ++ show(monitor) ++ "\" && " ++ "herbstclient use \"" 
        ++ tagIndex ++ "\":} " ++ tagName ++ " %{A} "
   
    -- non clickable tags
    -- textName = " " ++ tagName ++ " "

    ----- post tag

    textPost  = if (tagMark == "#")
                    then "%{B-}"
                      ++ "%{F" ++ myColor "blue500" ++ "}"
                      ++ "%{U" ++ myColor "red500" ++ "}%{+u}"
                      ++ rightHardArrow
                    else ""
    
    textClear = "%{B-}%{F-}%{-u}"
{% endhighlight %}

-- -- --

### Combine The Segments

Now it is time to combine all segments to compose one panel.
Lemonbar is using <code>%{l}</code> to align left segment,
and <code>%{r}</code> to align right segment.
All tags processed in a loop.

<code class="code-file">output.hs</code>
This is an IO action. Note that <code><$></code> operator.

{% highlight haskell %}
getStatusbarText :: Int -> IO String
getStatusbarText monitor = do
    tags <- readIORef tagsStatus
    let tagText =  "%{l}" ++ (join $ map (outputByTag monitor) tags)
    let titleText = ("%{r}" ++) <$> outputByTitle
    let text = (tagText ++) <$> titleText
    text
{% endhighlight %}

-- -- --

### Testing The Output

Consider this code <code class="code-file">02-testoutput.hs</code>.
The script using pipe as feed to lemonbar.

We append <code>-p</code> parameter to make the panel persistent.

{% highlight haskell %}
import System.Environment
import System.Process
import System.IO
import GHC.IO.Handle

import MyHelper
import MyOutput

-- initialize
panelHeight = 24

-- process handler
testLemon :: Int -> [String] -> IO ()
testLemon monitor parameters = do
    let command_out = "lemonbar"

    (Just pipe_in, _, _, ph)  <- 
        createProcess (proc command_out (parameters ++ ["-p"]) )
        { std_in = CreatePipe }

    -- initialize statusbar before loop
    setTagValue monitor 
    setWindowtitle "test"
    
    text <- getStatusbarText monitor

    hPutStrLn pipe_in text
    hFlush pipe_in
    
    hClose pipe_in

main = do
    args <- getArgs
    let monitor = getMonitor args

    geometry <- getGeometry monitor
    let lemonParameters = getLemonParameters panelHeight geometry

    -- test
    system $ "herbstclient pad " ++ show(monitor) ++ " "
        ++ show(panelHeight) ++ " 0 " ++ show(panelHeight) ++ " 0"

    testLemon monitor lemonParameters

    -- end of IO
    return ()

{% endhighlight %}

This will produce a panel on top.

![Statusbar: Lemonbar Screenshot][image-hlwm-ss-lemon]{: .img-responsive }

The panel only contain the initialized version of the text.
It does not really interact with the HerbstluftWM event.

You can also click the clickable area to see it's result.
It only show text, not executed yet.

{% highlight bash %}
herbstclient focus_monitor "0" && herbstclient use "2"
herbstclient focus_monitor "0" && herbstclient use "3"
{% endhighlight %}

#### View Source File:

*	**dzen2**: 
	[github.com/.../dotfiles/.../haskell/01-testoutput.hs][dotfiles-dzen2-haskell-testoutput]

*	**lemonbar**: 
	[github.com/.../dotfiles/.../haskell/01-testoutput.hs][dotfiles-lemon-haskell-testoutput]

{% include post/2017/06/herbstlustwm-tag-status-output.md %}

-- -- --

### Coming up Next

It is already a long tutorial.
It is time to take a break for a while.

We are going to continue on next tutorial
to cover interaction between the script process
and HerbstluftWM idle event.

*	[HerbstluftWM Event Idle in Haskell][local-haskell-idle]

-- -- --

Enjoy the statusbar !


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/desktop/2017/06' %}
{% assign dotfiles_dzen2 = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/dzen2-hlwm' %}
{% assign dotfiles_lemon = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lemon-hlwm' %}

[image-hlwm-02-tag-status]:   {{ asset_path }}/herbstclient-02-tag-status.png
[image-hlwm-02-monitor-rect]: {{ asset_path }}/herbstclient-03-monitor-rect.png
[image-hlwm-ss-dzen2]: {{ asset_path }}/hlwm-dzen2-ss.png
[image-hlwm-ss-lemon]: {{ asset_path }}/hlwm-lemon-ss.png

[image-01-tree-haskell]:  {{ asset_path }}/hlwm-statusbar-01-tree-haskell.png

[local-haskell-config]: {{ site.url }}/desktop/2017/05/08/herbstlustwm-modularized-haskell.html
[local-haskell-pipe]:   {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html
[local-haskell-idle]:   {{ site.url }}/desktop/2017/06/18/herbstlustwm-event-idle-haskell.html

[local-overview]: {{ site.url }}/desktop/2017/06/01/herbstlustwm-tag-status-overview.html

[dotfiles-dzen2-haskell-testparams]:  {{ dotfiles_dzen2 }}/haskell/01-testparams.hs
[dotfiles-dzen2-haskell-testoutput]:  {{ dotfiles_dzen2 }}/haskell/02-testoutput.hs
[dotfiles-dzen2-haskell-panel]:       {{ dotfiles_dzen2 }}/haskell/panel.hs
[dotfiles-dzen2-haskell-gmc]:         {{ dotfiles_dzen2 }}/haskell/MyGMC.hs
[dotfiles-dzen2-haskell-helper]:      {{ dotfiles_dzen2 }}/haskell/MyHelper.hs
[dotfiles-dzen2-haskell-output]:      {{ dotfiles_dzen2 }}/haskell/MyOutput.hs
[dotfiles-dzen2-haskell-pipehandler]: {{ dotfiles_dzen2 }}/haskell/MyPipeHandler.hs

[dotfiles-lemon-haskell-testparams]:  {{ dotfiles_lemon }}/haskell/01-testparams.hs
[dotfiles-lemon-haskell-testoutput]:  {{ dotfiles_lemon }}/haskell/02-testoutput.hs
[dotfiles-lemon-haskell-panel]:       {{ dotfiles_lemon }}/haskell/panel.hs
[dotfiles-lemon-haskell-gmc]:         {{ dotfiles_lemon }}/haskell/MyGMC.hs
[dotfiles-lemon-haskell-helper]:      {{ dotfiles_lemon }}/haskell/MyHelper.hs
[dotfiles-lemon-haskell-output]:      {{ dotfiles_lemon }}/haskell/MyOutput.hs
[dotfiles-lemon-haskell-pipehandler]: {{ dotfiles_lemon }}/haskell/MyPipeHandler.hs
