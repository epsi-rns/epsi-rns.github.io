---
layout: post
title:  "Openbox Config - Key and Mouse Binding"
categories: desktop
date:   2018-05-03 09:25:15 +0700
tags: [openbox]
author: epsi

excerpt:
  A brief explanation about Openbox rc.xml Configuration

---

{% include post/2018/05/toc-openbox-config.html %}

> Goal: Explaining openbox key and mouse binding in rc.xml configuration 

### Key Binding

There is a whole article for this.

*	[http://openbox.org/wiki/Help:Bindings](http://openbox.org/wiki/Help:Bindings#Key_bindings)

#### Source

*	[github.com/.../dotfiles/.../rc.xml][dotfiles-rc-xml]

#### Format

The format is:

{% highlight xml %}
  <keyboard>
    ...
    <keybind key="C-A-Left">
      <action name="GoToDesktop">
        <to>left</to>
        <wrap>no</wrap>
      </action>
    </keybind>
    ...
  </keyboard>
{% endhighlight %}

#### Custom Key Binding.

I add two more keybind.

![openbox Keybinding: Undecorate][image-ss-w-a-x]{: .img-responsive }

{% highlight xml %}
    <keybind key="W-A-z">
      <action name="Decorate"/>
    </keybind>
    <keybind key="W-A-x">
      <action name="Undecorate"/>
    </keybind>
{% endhighlight %}

With courtesy of my friend (Asem Bused),
I can show you the visual representation of this keybinding.

![openbox Keybinding: Decorate][image-ss-w-a-z]{: .img-responsive }

I like terminal with no window border.

Of course you can add your own keybind.

#### Summarize

Rather than, go deep into <code class="code-file">rc.xml</code>.
I just want to put my key binding here.
I have limited brain capacity.
I need to write down as below.

{% highlight conf %}
# Keybindings for desktop switching

C-A-Left:   GoToDesktop:   left
C-A-Right:  GoToDesktop:   right
C-A-Up:     GoToDesktop:   up
C-A-Down:   GoToDesktop:   down

S-A-Left:   SendToDesktop: left
S-A-Right:  SendToDesktop: right
S-A-Up:     SendToDesktop: up
S-A-Down:   SendToDesktop: down

W-F1:       GoToDesktop:   1
W-F2:       GoToDesktop:   2
W-F3:       GoToDesktop:   3
W-F4:       GoToDesktop:   4

W-d:        ToggleShowDesktop

# Keybindings for windows

A-F4:       Close
A-Escape:   Lower, FocusToBottom, Unfocus
A-space:    ShowMenu:      client-menu

# Keybindings for window switching 

A-Tab:      NextWindow - Focus, Raise, Unshade
A-S-Tab:    PreviousWindow - Focus, Raise, Unshade
C-A-Tab:    NextWindow - Focus, Raise, Unshade

# Keybindings for window switching with the arrow keys 

W-S-Right:  DirectionalCycleWindows: right
W-S-Left:   DirectionalCycleWindows: left
W-S-Up:     DirectionalCycleWindows: up
W-S-Down:   DirectionalCycleWindows: above

# Keybindings for running applications

W-e:        Execute: urxvt
{% endhighlight %}

The last line, it is actually xterm terminal,
but I change it to urxvt.

-- -- --

### Mouse Binding

The format is:

{% highlight xml %}
  <mouse>
    ...
    <context name="Frame">
      <mousebind button="A-Left" action="Press">
        <action name="Focus"/>
        <action name="Raise"/>
      </mousebind>
      ...
    </context>
  </mouse>
{% endhighlight %}

#### Summarize

{% highlight conf %}
# Context: Frame

A-Left:     Press:         Focus, Raise
A-Left:     Click:         Unshade
A-Left:     Drag:          Move

A-Right:    Press:         Focus, Raise, Unshade
A-Right:    Drag:          Resize

A-Middle:   Press:         Lower, FocusToBottom, Unfocus

A-Up:       Click:         GoToDesktop:    previous
A-Down:     Click:         GoToDesktop:    next

C-A-Up:     Click:         GoToDesktop:    previous
C-A-Down:   Click:         GoToDesktop:    next

A-S-Up:     Click:         SendToDesktop:  previous
A-S-Down:   Click:         SendToDesktop:  next

# Context: Titlebar

Left:       Drag:          Move
Left:       DoubleClick:   ToggleMaximize
Up:         Click:         if not shaded:  Shade, FocusToBottom, Unfocus, Lower
Down:       Click:         if shaded:  Unshade, Raise

# Context: Titlebar Top Right Bottom Left TLCorner TRCorner BRCorner BLCorner

Left:       Press:         Focus, Raise, Unshade
Middle:     Press:         Lower, FocusToBottom, Unfocus
Right:      Press:         Focus, Raise, ShowMenu

# Context: Top

Left:       Drag:          Resize:         top

# Context: Left

Left:       Drag:          Resize:         left

# Context: Right

Left:       Drag:          Resize:         right

# Context: Bottom

Left:       Drag:          Resize:         bottom
Right:      Press:         Focus, Raise, ShowMenu

# Context: TRCorner BRCorner TLCorner BLCorner

Left:       Press:         Focus, Raise, Unshade
Left:       Drag:          Resize

# Context: Client

Left:       Press:         Focus, Raise
Middle:     Press:         Focus, Raise
Right:      Press:         Focus, Raise

# Context: AllDesktops

Left:       Press:         Focus, Raise, Unshade
Left:       Click:         ToggleOmnipresent

# Context: Desktop

Up:         Click:         GoToDesktop:    previous
Down:       Click:         GoToDesktop:    next
A-Up:       Click:         GoToDesktop:    previous
A-Down:     Click:         GoToDesktop:    next
C-A-Up:     Click:         GoToDesktop:    previous
C-A-Down:   Click:         GoToDesktop:    next
Left:       Press:         Focus, Raise
Right:      Press:         Focus, Raise

# Context: Root

Middle:     Press:         ShowMenu:       client-list-combined-menu
Right:      Press:         ShowMenu:       root-menu

# Context: MoveResize

Up:         Click:         GoToDesktop:    previous
Down:       Click:         GoToDesktop:    next
A-Up:       Click:         GoToDesktop:    previous
A-Down:     Click:         GoToDesktop:    next
{% endhighlight %}

#### Summmarize The Buttons

{% highlight conf %}
# Context: Icon

Left:       Press:         Focus, Raise, Unshade, ShowMenu
Right:      Press:         Focus, Raise, ShowMenu

# Context: Shade

Left:       Press:         Focus, Raise
Left:       Click:         ToggleShade

# Context: Iconify

Left:       Press:         Focus, Raise
Left:       Click:         Iconify

# Context: Maximize

Left:       Press:         Focus, Raise, Unshade
Middle:     Press:         Focus, Raise, Unshade
Right:      Press:         Focus, Raise, Unshade
Left:       Click:         ToggleMaximize
Middle:     Click:         ToggleMaximize: vertical  
Right:      Click:         ToggleMaximize: horizontal

# Context: Close

Left:       Press:         Focus, Raise, Unshade
Left:       Click:         Close
{% endhighlight %}

-- -- --

### More Custom Key Binding.

Mostly borrowed from my i3 configuration.

#### Summary

{% highlight conf %}
W-e:        oblogout
W-Return:   xfce4-terminal
W-A-d:      dmenu_run
W-S-d:      rofi -show run -opacity 90
W-Tab:      rofi -show window -opacity 90
{% endhighlight %}

### OB Log Out

*	[https://wiki.archlinux.org/index.php/Oblogout](https://wiki.archlinux.org/index.php/Oblogout)

{% highlight conf %}
    <keybind key="W-x">
      <action name="Execute">
        <startupnotify>
          <enabled>true</enabled>
          <name>oblogut</name>
        </startupnotify>
        <command>oblogout</command>
      </action>
    </keybind>
{% endhighlight %}

#### Terminal

{% highlight conf %}
    <keybind key="W-Return">
      <action name="Execute">
        <command>xfce4-terminal</command>
      </action>
    </keybind>
{% endhighlight %}

#### dmenu

{% highlight conf %}
    <keybind key="W-A-d">
      <action name="Execute">
        <command>dmenu_run</command>
      </action>
    </keybind>
{% endhighlight %}

#### Rofi Run

{% highlight conf %}
    <keybind key="W-S-d">
      <action name="Execute">
        <command>rofi -show run -opacity 90</command>
      </action>
    </keybind>
{% endhighlight %}

#### Rofi Window

{% highlight conf %}
    <keybind key="W-Tab">
      <action name="Execute">
        <command>rofi -show window -opacity 90</command>
      </action>
    </keybind>
{% endhighlight %}

-- -- --

### What's Next

Consider continue reading [ [Config: Rules][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]: {{ dotfiles }}/rc.xml

[local-part-config]:  /desktop/2018/05/04/openbox-config.html

[image-ss-w-a-x]:          {{ asset_path }}/openbox-keybind-W-A-x.png
[image-ss-w-a-z]:          {{ asset_path }}/openbox-keybind-W-A-z.png
