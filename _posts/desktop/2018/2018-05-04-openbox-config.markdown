---
layout: post
title:  "Openbox Config - Key and Mouse Binding"
categories: desktop
date:   2018-05-04 09:25:15 +0700
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

### Custom Key Binding.

#### Openbox Key Binding.

I add two more keybind.

{% highlight conf %}
W-A-Z:   Decorate
W-A-X:   Undecorate
{% endhighlight %}

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

#### OB Log Out

More about OB Logout in:

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


#### My Custom Binding

*	[github.com/.../dotfiles/.../keybind.xml.txt][dotfiles-keybind]

Mostly borrowed from my i3 configuration.

{% highlight conf %}
W-x:        oblogout
W-Return:   xfce4-terminal
W-A-d:      dmenu_run
W-S-d:      rofi -show run -opacity 90
W-Tab:      rofi -show window -opacity 90
{% endhighlight %}

#### Addy's Custom Binding

*	[github.com/addy-dclxvi/.../rc.xml](https://github.com/addy-dclxvi/almighty-dotfiles/blob/master/.config/openbox/rc.xml)

{% highlight conf %}
W-Tab:      skippy-xd

XF86AudioStop:   mpc stop
XF86AudioPlay:   mpc toggle
XF86AudioPrev:   mpc prev
XF86AudioNext:   mpc next

XF86AudioRaiseVolume: amixer -D pulse sset Master '5%+'
XF86AudioLowerVolume: amixer -D pulse sset Master '5%-'
XF86AudioMute:        amixer set Master toggle
{% endhighlight %}

In my system, I use

{% highlight bash %}
$ amixer -D default sset Master '5%+'
{% endhighlight %}

Pulseaudio user may use:

{% highlight bash %}
$ pactl set-sink-volume 0 +5%
{% endhighlight %}

#### Arcolinux Custom Binding

I must admit that, these are cool.

*	[github.com/arcolinux/.../rc.xml](https://github.com/arcolinux/arcolinux-openbox-configs/blob/master/rc.xml)

{% highlight conf %}
W-Escape:      xkill

W-Right:       UnmaximizeFull, MaximizeVert, MoveResizeTo: 50%, MoveToEdgeEast
W-Left:        UnmaximizeFull, MaximizeVert, MoveResizeTo: 50%, MoveToEdgeWest
W-Up:          MaximizeFull
W-Down:        UnmaximizeFull, MoveResizeTo: 80% 80%, MoveToCenter
W-A-Right:     UnmaximizeFull, MaximizeVert, MoveResizeTo: -0 0 50%
W-A-Left:      UnmaximizeFull, MaximizeVert, MoveResizeTo:  0 0 50%
{% endhighlight %}

Now we can emulate simple tiling easily.

![openbox Keybinding: Tiling][image-ss-tiling]{: .img-responsive }

There are also <code>variety</code> wallapaper if you want,
but I'd rather not using it.

-- -- --

### What's Next

Consider continue reading [ [Config: Rules][local-part-config] ].

[//]: <> ( -- -- -- links below -- -- -- )
{% assign asset_path = '/assets/posts/desktop/2018/05' %}
{% assign dotfiles = 'https://github.com/epsi-rns/dotfiles/tree/master/openbox/config' %}

[dotfiles-rc-xml]:  {{ dotfiles }}/rc.xml
[dotfiles-keybind]: {{ dotfiles }}/keybind.xml.txt

[local-part-config]:  /desktop/2018/05/05/openbox-config.html

[image-ss-w-a-x]:          {{ asset_path }}/openbox-keybind-W-A-x.png
[image-ss-w-a-z]:          {{ asset_path }}/openbox-keybind-W-A-z.png
[image-ss-tiling]:         {{ asset_path }}/openbox-keybind-tiling.png
