---
layout: post
title:  "Audio Command Line, a must have Knowledge"
date      : 2018-06-22 09:45:15 +0700
categories: system
tags      : [thought, audio]
keywords  : [troubleshooting, audiopulse, alsamixer]
author: epsi

opengraph:
  image: /assets/site/images/topics/bash.png

excerpt:
  Even if you have installed your linux successfully 
  you still need to know your own hardware. 
  This article also useful to debug audio or sound issue, 
  especially for first time linux install.

related_link_ids: 
  - 14040246  # Arch Install

---

### Preface

>	Know Your System, Focusing on Audio

I love listening to music.
A linux without sound is killing me.
After all this linux years,
I am still curious about this sound thing.

Altough I'm using openSUSE.
This article can be a reference to other linux as well.

I mostly pour my daily linux troubleshooting in a blog,
so I can find the solution easily,
if I have similar issue another day.

-- -- --

### The Operating System Part

Before getting into detail
on how the sound works perfectly or no sound at all,
first we should examine the system wide setting,
that is provided by the operating system.

#### The Hardware Information

I always use <code>lspci</code> as my starting point.
Well, I'm not an expert. I could be wrong.
Other people mught have different starting point.

{% highlight bash %}
% /sbin/lspci | grep -i audio
00:1b.0 Audio device: Intel Corporation NM10/ICH7 Family High Definition Audio Controller (rev 01)
{% endhighlight %}

![audio: lspci grep audio][image-ss-lspci-grep]{: .img-responsive }

Now that we have this <code>00:1b.0</code>,
we can have a more verbose information.

{% highlight bash %}
% /sbin/lspci -vnn -s 00:1b.0
00:1b.0 Audio device [0403]: Intel Corporation NM10/ICH7 Family High Definition Audio Controller [8086:27d8] (rev 01)
    Subsystem: Gigabyte Technology Co., Ltd GA-D525TUD (Realtek ALC887) [1458:a002]
    Flags: bus master, fast devsel, latency 0, IRQ 24
    Memory at fdff8000 (64-bit, non-prefetchable) [size=16K]
    Capabilities: <access denied>
    Kernel driver in use: snd_hda_intel
    Kernel modules: snd_hda_intel
{% endhighlight %}

It is crazy. This command know that I have realtek ALC887.

Note with this suspicious **access denied** message,
my sound card still works perfectly.

{% highlight bash %}
    Capabilities: <access denied>
{% endhighlight %}


![audio: lspci verbose][image-ss-lspci-verbose]{: .img-responsive }

Now, how about lsmod.

{% highlight bash %}
% /bin/lsmod | grep -i intel
snd_hda_intel          45056  3
snd_hda_codec         147456  3 snd_hda_intel,snd_hda_codec_generic,snd_hda_codec_realtek
snd_hda_core           90112  4 snd_hda_intel,snd_hda_codec,snd_hda_codec_generic,snd_hda_codec_realtek
snd_pcm               147456  3 snd_hda_intel,snd_hda_codec,snd_hda_core
snd                    98304  13 snd_hda_intel,snd_hwdep,snd_hda_codec,snd_timer,snd_hda_codec_generic,snd_hda_codec_realtek,snd_pcm
{% endhighlight %}

![audio: lsmod grep intel][image-ss-lsmod-intel]{: .img-responsive }

-- -- --

#### Boot Messages

Simply issue <code>dmesg</code> (display message) command.
And filter it with grep looking for text contining snd.

{% highlight bash %}
% dmesg | grep -i snd
[   20.972887] snd_hda_codec_realtek hdaudioC0D2: autoconfig for ALC887-VD: line_outs=1 (0x14/0x0/0x0/0x0/0x0) type:line
[   20.975968] snd_hda_codec_realtek hdaudioC0D2:    speaker_outs=0 (0x0/0x0/0x0/0x0/0x0)
[   20.979016] snd_hda_codec_realtek hdaudioC0D2:    hp_outs=1 (0x1b/0x0/0x0/0x0/0x0)
[   20.982165] snd_hda_codec_realtek hdaudioC0D2:    mono: mono_out=0x0
[   20.985222] snd_hda_codec_realtek hdaudioC0D2:    inputs:
[   20.988224] snd_hda_codec_realtek hdaudioC0D2:      Rear Mic=0x18
[   20.991264] snd_hda_codec_realtek hdaudioC0D2:      Front Mic=0x19
[   20.994229] snd_hda_codec_realtek hdaudioC0D2:      Line=0x1a
[   20.997186] snd_hda_codec_realtek hdaudioC0D2:      CD=0x1c
{% endhighlight %}

![audio: dmesg grep snd][image-ss-dmesg-snd]{: .img-responsive }

Or you can issue a more specific filter.
But this is useless.

{% highlight bash %}
% dmesg | grep -i alc887
[   20.972887] snd_hda_codec_realtek hdaudioC0D2: autoconfig for ALC887-VD: line_outs=1 (0x14/0x0/0x0/0x0/0x0) type:line
{% endhighlight %}

![audio: dmesg grep alc][image-ss-dmesg-alc]{: .img-responsive }

I mean, please be careful on filtering.
You might lose important message that you are searching for.

-- -- --

#### Device

Device is listed on <code>/dev/snd</code>.

{% highlight bash %}
%  ls -l /dev/snd
total 0
drwxr-xr-x  2 root root       60 Jun 22 20:37 by-path
crw-rw----+ 1 root audio 116,  2 Jun 22 20:37 controlC0
crw-rw----+ 1 root audio 116,  6 Jun 22 20:37 hwC0D2
crw-rw----+ 1 root audio 116,  4 Jun 22 20:38 pcmC0D0c
crw-rw----+ 1 root audio 116,  3 Jun 22 21:11 pcmC0D0p
crw-rw----+ 1 root audio 116,  5 Jun 22 20:37 pcmC0D2c
crw-rw----+ 1 root audio 116,  1 Jun 22 20:37 seq
crw-rw----+ 1 root audio 116, 33 Jun 22 20:37 timer
{% endhighlight %}

![audio: /dev/snd][image-ss-dev-snd]{: .img-responsive }

-- -- --

#### Process

The <code>/dev/snd</code> directory is not the only place to go.
How about <code>/proc/asound</code> ?

{% highlight bash %}
% ls -l /proc/asound/
total 0
dr-xr-xr-x 5 root root 0 Jun 22 22:07 card0
-r--r--r-- 1 root root 0 Jun 22 22:07 cards
-r--r--r-- 1 root root 0 Jun 22 22:07 devices
-r--r--r-- 1 root root 0 Jun 22 22:07 hwdep
lrwxrwxrwx 1 root root 5 Jun 22 22:07 Intel -> card0
-r--r--r-- 1 root root 0 Jun 22 22:07 modules
dr-xr-xr-x 2 root root 0 Jun 22 22:07 oss
-r--r--r-- 1 root root 0 Jun 22 22:07 pcm
dr-xr-xr-x 2 root root 0 Jun 22 22:07 seq
-r--r--r-- 1 root root 0 Jun 22 22:07 timers
-r--r--r-- 1 root root 0 Jun 22 22:07 version
{% endhighlight %}

![audio: /proc/asound][image-ss-proc-asound]{: .img-responsive }

We can use this later to get the codec.

{% highlight bash %}
% cat /proc/asound/card0/codec\#2 | grep -i codec
Codec: Realtek ALC887-VD
{% endhighlight %}

![audio: codec /proc][image-ss-proc-codec]{: .img-responsive }

{% highlight bash %}
% cat /proc/asound/version
Advanced Linux Sound Architecture Driver Version k4.14.9-1-default.
{% endhighlight %}

![audio: /proc/asound/version][image-ss-proc-version]{: .img-responsive }

-- -- --

#### User and Group

One said that most the important aspect is the permission.

{% highlight bash %}
% cat /etc/group | grep audio
audio:x:496:pulse,mpd
{% endhighlight %}

![audio: user group][image-ss-user-group]{: .img-responsive }

-- -- --

### The Audio Part

Now it is depend on the driver and audio handler in your system.
Mine could be difference with yours.

#### Pulseaudio

My openSUSE as <code>pulseaudio</code> set by default.

{% highlight bash %}
% pulseaudio --check
% pulseaudio --kill
% pulseaudio --start
{% endhighlight %}

![audio: pulseaudio][image-ss-pulseaudio]{: .img-responsive }

#### aplay: List Devices

You must have been heard about this <code>aplay</code> tool.

Consider issue <code>aplay -l</code> command.
However, be aware of lowercase argument.

{% highlight bash %}
% aplay --list-devices
**** List of PLAYBACK Hardware Devices ****
card 0: Intel [HDA Intel], device 0: ALC887-VD Analog [ALC887-VD Analog]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
{% endhighlight %}

![audio: aplay list devices][image-ss-aplay-list]{: .img-responsive }

Now it is time to test if the audio works.

{% highlight bash %}
% aplay /usr/share/sounds/alsa/test.wav
Playing WAVE '/usr/share/sounds/alsa/test.wav' : Signed 16 bit Little Endian, Rate 44100 Hz, Stereo
{% endhighlight %}

![audio: aplay test using wav file][image-ss-aplay-test]{: .img-responsive }

You should hear something from your speaker by now.

#### aplay: List PCM

Consider issue <code>aplay -L</code> command.
However, be aware of uppercase argument.

{% highlight bash %}
% aplay --list-pcm
null
    Discard all samples (playback) or generate zero samples (capture)
default
    Default ALSA Output (currently PulseAudio Sound Server)
sysdefault:CARD=Intel
    HDA Intel, ALC887-VD Analog
    Default Audio Device
front:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    Front speakers
surround21:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    2.1 Surround output to Front and Subwoofer speakers
surround40:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    4.0 Surround output to Front and Rear speakers
surround41:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    4.1 Surround output to Front, Rear and Subwoofer speakers
surround50:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    5.0 Surround output to Front, Center and Rear speakers
surround51:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    5.1 Surround output to Front, Center, Rear and Subwoofer speakers
surround71:CARD=Intel,DEV=0
    HDA Intel, ALC887-VD Analog
    7.1 Surround output to Front, Center, Side, Rear and Woofer speaker
{% endhighlight %}

![audio: aplay list pcm][image-ss-aplay-pcm]{: .img-responsive }

> PCM: Pulse Code Modulation

-- -- --

#### alsamixer

Now you can djust volume by issue <code>alsamixer</code> command.

{% highlight bash %}
% alsamixer
{% endhighlight %}

![audio: alsamixer][image-ss-alsamixer]{: .img-responsive }

And press <kbd>ESC</kbd> to quit.

-- -- --

#### xfce4-mixer

If you wish for GUI, you can use <code>xfce4-mixer</code> as well.

{% highlight bash %}
% xfce4-mixer &!
{% endhighlight %}

![audio: xfce4-mixer][image-ss-xfce4-mixer]{: .img-responsive }

-- -- --

#### pavucontrol

Or more eleganly for pulseaudio, you can use <code>pavucontrol</code>.

{% highlight bash %}
% pavucontrol &!
{% endhighlight %}

![audio: pavucontrol][image-ss-pavucontrol]{: .img-responsive }

-- -- --

### Miscellanous

#### Volume Control: amixer

{% highlight bash %}
$ amixer -D pulse sset Master '5%+'
$ amixer -D pulse sset Master '5%-'
$ amixer set Master toggle
{% endhighlight %}

![audio: amixer][image-ss-amixer-volume]{: .img-responsive }

#### Volume Control: pactl

<code>pactl</code> and <code>pacmd</code>.

{% highlight bash %}
% pactl set-sink-volume 0 +1%
% pactl set-sink-volume 0 80%
{% endhighlight %}

![audio: pactl][image-ss-pactl-volume]{: .img-responsive }

#### Information: pactl

{% highlight bash %}
% pactl info
Server String: /run/user/1000/pulse/native
Library Protocol Version: 32
Server Protocol Version: 32
Is Local: yes
Client Index: 110
Tile Size: 65472
User Name: epsi
Host Name: andalan
Server Name: pulseaudio
Server Version: 11.1
Default Sample Specification: s16le 2ch 44100Hz
Default Channel Map: front-left,front-right
Default Sink: alsa_output.pci-0000_00_1b.0.analog-stereo
Default Source: alsa_output.pci-0000_00_1b.0.analog-stereo.monitor
Cookie: d448:eedf
{% endhighlight %}

![audio: pactl][image-ss-pactl-info]{: .img-responsive }

-- -- --

### Conclusion

That's not all. Not at all.

There are still a lot of things to explore with audio sound.
But I have only limited knowledge.

Thank you for reading

[//]: <> ( -- -- -- links below -- -- -- )

{% assign system_path = 'https://epsi-rns.github.io/system' %}

[image-ss-lspci-grep]:    {{ system_path }}/2018/06/audio-lspci-grep-audio.png
[image-ss-lspci-verbose]: {{ system_path }}/2018/06/audio-lspci-verbose.png
[image-ss-lsmod-intel]:   {{ system_path }}/2018/06/audio-lsmod-intel.png

[image-ss-dmesg-alc]:     {{ system_path }}/2018/06/audio-dmesg-alc.png
[image-ss-dmesg-snd]:     {{ system_path }}/2018/06/audio-dmesg-snd.png

[image-ss-dev-snd]:       {{ system_path }}/2018/06/audio-dev-snd.png
[image-ss-proc-asound]:   {{ system_path }}/2018/06/audio-proc-asound.png
[image-ss-proc-codec]:    {{ system_path }}/2018/06/audio-proc-codec.png
[image-ss-proc-version]:  {{ system_path }}/2018/06/audio-proc-version.png

[image-ss-user-group]:    {{ system_path }}/2018/06/audio-user-group.png

[image-ss-pulseaudio]:    {{ system_path }}/2018/06/audio-pulseaudio.png

[image-ss-aplay-list]:    {{ system_path }}/2018/06/audio-aplay-list-devices.png
[image-ss-aplay-test]:    {{ system_path }}/2018/06/audio-aplay-test-wav.png
[image-ss-aplay-pcm]:     {{ system_path }}/2018/06/audio-aplay-list-pcm.png

[image-ss-alsamixer]:     {{ system_path }}/2018/06/audio-alsamixer.png
[image-ss-xfce4-mixer]:   {{ system_path }}/2018/06/audio-xfce4-mixer.png
[image-ss-pavucontrol]:   {{ system_path }}/2018/06/audio-pavucontrol.png

[image-ss-amixer-volume]: {{ system_path }}/2018/06/audio-amixer-volume.png
[image-ss-pactl-volume]:  {{ system_path }}/2018/06/audio-pactl-volume.png
[image-ss-pactl-info]:    {{ system_path }}/2018/06/audio-pactl-info.png

