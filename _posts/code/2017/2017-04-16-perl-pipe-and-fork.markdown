---
layout     : post
title      : "Piping and Forking in Perl"
date       : 2017-04-16 17:35:15 +0700
categories : code
tags       : [coding, conky, perl]
keywords   : [pipe and fork, dzen2, lemonbar]
author     : epsi
toc        : toc/2017/04/pipe-and-fork-language.html

opengraph:
  image: /assets/site/images/topics/perl.png

excerpt:
  How to be a Perl Plumber.
  
related_link_ids: 
  - 17042335  # Pipe and Fork Overview
  - 17041535  # Pipe and Fork BASH
  - 17041635  # Pipe and Fork Perl
  - 17041735  # Pipe and Fork Python
  - 17041835  # Pipe and Fork Ruby
  - 17041935  # Pipe and Fork PHP
  - 17042035  # Pipe and Fork Lua
  - 17042135  # Pipe and Fork Haskell

---

### Perl Plumber.

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky.

Before you dip your toe to scripting,
you might desire to know the reason by reading this overview.

**Reading**

*	[Piping and Forking in Linux Script][local-overview]

-- -- --

### Start Simple

Welcome to n00berland. Begin with simple script.
We will use this loop as a source feed to pipe.
This step won't introduce Pipe nor Fork.

This script only show an infinite loop showing local time.
Each updated in one second interval.
We manage this interval by delaying,
using <code>sleep</code> code.


**Source**:

*	[gitlab.com/.../dotfiles/.../perl-01-basic.pl][dotfiles-perl-01-basic]

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;
use Time::Piece;
use IO::Pipe;

my $timeformat = '%a %b %d %H:%M:%S';

my $datestr;
while(1) {
    $datestr = localtime->strftime($timeformat);
    print "$datestr \n";
      
    sleep 1;
}
{% endhighlight %}

Call to this simple code would produce time marching,
one after another, below the command line prompt.

![Pipe: Basic][image-time-basic]{: .img-responsive }

{% include toc/2017/04/pipe-and-fork-similar-01.html %}

-- -- --

### External Command as Source Feed

Beside previous simple loop that is used as Internal Command,
this tutorial also provide Conky as External Command
in <code class="code-file">asset</code> directory.
I made it as simple as possible.

**Source**:

*	[gitlab.com/.../dotfiles/.../conky.lua][dotfiles-conky]

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

-- -- --

### Spawning Using System Shell

Using system shell is simple and straightforward.
But it does not have any ability,
to stream internal function process,
that required later on this article.


**Source**:

*	[gitlab.com/.../dotfiles/.../perl-02-system.pl][dotfiles-perl-02-system]

{% highlight perl %}
#!/usr/bin/perl
# Using system shell

use warnings;
use strict;
use File::Basename;

# reset the terminal, for use with less
system('reset');

my $dirname = dirname(__FILE__);
my $path    = "$dirname/../assets";
my $cmdin   = "conky -c $path/conky.lua";
my $cmdout  = "less"; # or dzen2
my $cmd     = "$cmdin | $cmdout";

# execute pipe
system($cmd);
{% endhighlight %}

{% include toc/2017/04/pipe-and-fork-similar-02-system.html %}

-- -- --

### A Unidirectional Pipe Between External Command

Unidirectional means one way communication.

	A pipe has a read end and a write end.

This step is overview of Pipe between two external command.
This short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I add <code>$dirname</code>, relative to the Perl source,
to locate the conky script assets.

At least, there are four mechanism to pipe in Perl.
First three using native <code>open</code>,
<code>IPC::Open2</code> and <code>IPC::Open3</code>,
and the last, using object oriented <code>IO::Pipe</code.

**Source**:

*	[gitlab.com/.../dotfiles/.../perl-02-uni-io.pl][dotfiles-perl-02-uni-io]

*	[gitlab.com/.../dotfiles/.../perl-02-uni-open.pl][dotfiles-perl-02-uni-open]

*	[gitlab.com/.../dotfiles/.../perl-02-uni-open2.pl][dotfiles-perl-02-uni-open2]

Using <code>open</code>:

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;
use File::Basename;

my $dirname = dirname(__FILE__);
my $path    = "$dirname/../assets";
my $cmdin   = "conky -c $path/conky.lua";
my $cmdout  = "less"; # or dzen2

open my $pipein, "-|", $cmdin
    or die "Could not open filehandle: $!";

open my $pipeout, "|-", $cmdout
    or die "Could not open filehandle: $!";
    
while(<$pipein>) {
    print $pipeout $_;
    flush $pipeout;
}

close $pipein;
close $pipeout;
{% endhighlight %}

Using <code>IO::Pipe</code>:

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;
use File::Basename;
use IO::Pipe;

my $dirname = dirname(__FILE__);
my $path    = "$dirname/../assets";
my $cmdin   = "conky -c $path/conky.lua";
my $cmdout  = "less"; # or dzen2

my $pipein  = IO::Pipe->new();
my $hnd_in  = $pipein->reader($cmdin);

my $pipeout = IO::Pipe->new();
my $hnd_ou  = $pipeout->writer($cmdout);

while(<$pipein>) {
    print $pipeout $_;
    flush $pipeout;
}
    
$pipein->close();
$pipeout->close();
{% endhighlight %}

Using <code>IPC::Open2</code>:

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
use IPC::Open2;

my $dirname = dirname(__FILE__);
my $path    = "$dirname/../assets";
my $cmdin   = "conky -c $path/conky.lua";
my $cmdout  = "dzen2";

my ($rhin, $whin);
my $pidin  = open2 ($rhin,  $whin,  $cmdin)  
    or die "can't pipein: $!";

my ($rhout, $whout);
my $pidout = open2 ($rhout, $whout, $cmdout) 
    or die "can't pipeout: $!";

my $line = '';
while ($line = <$rhin>) {
    print $whout $line;
}

waitpid( $pidin,  0 );
waitpid( $pidout, 0 );

{% endhighlight %}

You can see, how simple it is.
This would have <code>less</code> output similar to this below.

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

{% include toc/2017/04/pipe-and-fork-similar-02.html %}

-- -- --

### How does it works ?

Perl act as middle man.
<code>$pipein</code> act as standar output handle (stdout),
while <code>$pipeout</code>  act as standar input handle (stdin),
It read in <code>$_</code> handle <code>$pipein</code>,
and write to handle <code>$pipeout</code>.
If you don't flush, it will be buffered until the handle closed.
It means, you will never see the output until it closed.

{% highlight perl %}
while(<$pipein>) {
    print $pipeout $_;
    flush $pipeout;
}
{% endhighlight %}

-- -- --

### A Unidirectional Pipe from Internal Function

Using internal function as source feed
to external command is straight forward.
This should be self explanatory.

	Do not forget to flush.

As previous example, we are using three mechanism,
<code>open</code>, <code>IPC:Open2</code> and <code>IO::Pipe</code>.
Note that <code>IPC:Open2</code> has bidirectional capability
that we need in next guidance.
Bidirectional means, a process can read and write at the same time.

**Source**:

*	[gitlab.com/.../dotfiles/.../perl-03-pipe-io.pl][dotfiles-perl-03-pipe-io]

*	[gitlab.com/.../dotfiles/.../perl-03-pipe-open.pl][dotfiles-perl-03-pipe-open]

*	[gitlab.com/.../dotfiles/.../perl-03-pipe-open2.pl][dotfiles-perl-03-pipe-open2]

Using <code>open</code>:

{% highlight perl %}
#!/usr/bin/perl
# https://docstore.mik.ua/orelly/perl3/prog/ch16_03.htm

use warnings;
use strict;
use Time::Piece;

my $cmdout  = "less"; # or dzen2

open my $pipeout, "|-", $cmdout
    or die "Could not open filehandle: $!";
    
my $datestr;
my $timeformat = '%a %b %d %H:%M:%S';

while(1) {
    $datestr = localtime->strftime($timeformat);
    print $pipeout "$datestr \n";
    
    flush $pipeout;
    sleep 1;
}

close $pipeout
{% endhighlight %}

Using <code>IO::Pipe</code>:

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;
use Time::Piece;
use IO::Pipe;

my $cmdout  = "less"; # or dzen2

my $pipeout = IO::Pipe->new();
my $handle = $pipeout->writer($cmdout);

my $datestr;
my $timeformat = '%a %b %d %H:%M:%S';

while(1) {
    $datestr = localtime->strftime($timeformat);
    print $pipeout "$datestr \n";
    
    flush $pipeout;    
    sleep 1;
}

$pipeout->close();
{% endhighlight %}

Using <code>IPC::Open2</code>:

{% highlight perl %}
#!/usr/bin/perl
# https://docstore.mik.ua/orelly/perl3/prog/ch16_03.htm

use warnings;
use strict;

use Time::Piece;
use IPC::Open2;

my $cmdout  = "dzen2";

my ($rhout, $whout);
my $pidout = open2 ($rhout, $whout, $cmdout) 
    or die "can't pipeout: $!";
    
my $datestr;
my $timeformat = '%a %b %d %H:%M:%S';

while(1) {
    $datestr = localtime->strftime($timeformat);
    print $whout "$datestr \n";
    
    sleep 1;
}

waitpid( $pidout, 0 );
{% endhighlight %}

{% include toc/2017/04/pipe-and-fork-similar-03.html %}

-- -- --

### How does it works ?

The same as previous.
But instead of reading from stdout <code>$pipein</code>,
it is managed by internal process using <code>print $pipeout</code>.

{% highlight perl %}
    $datestr = localtime->strftime($timeformat);
    print $pipeout "$datestr \n";
{% endhighlight %}

-- -- --

### Fork Overview

This step use internal function as source feed,
as continuation of previous step..
To avoid complexity of longer script,
most code is written inside function

This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[gitlab.com/.../dotfiles/.../perl-05-fork.pl][dotfiles-perl-05-fork]

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;
use Time::Piece;
use IO::Pipe;

sub get_dzen2_parameters { 
    my $xpos    = 0;
    my $ypos    = 0;
    my $width   = 640;
    my $height  = 24;
    my $fgcolor = "#000000";
    my $bgcolor = "#ffffff";
    my $font    = "-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*";

    my $parameters = "  -x $xpos -y $ypos -w $width -h $height";
    $parameters   .= " -fn '$font'";
    $parameters   .= " -ta c -bg '$bgcolor' -fg '$fgcolor'";
    $parameters   .= " -title-name dzentop";

    # print $parameters . "\n";    
    return $parameters;
}

sub generated_output {
    my $pipeout = shift;

    my $datestr;
    my $timeformat = '%a %b %d %H:%M:%S';
    
    while(1) {
        $datestr = localtime->strftime($timeformat);
        print $pipeout "$datestr \n";

        flush $pipeout;
        sleep 1;
    }
}

sub run_dzen2 { 

    my $parameters = get_dzen2_parameters();

    my $pipeout = IO::Pipe->new();
    my $childhandle = $pipeout->writer("dzen2 $parameters");

    generated_output($pipeout);
    $pipeout->close();
}

sub detach_dzen2 { 
    my $pid = fork;
    return if $pid;     # in the parent process
    
    run_dzen2();
    exit; 
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
system('pkill dzen2');

# run process in the background
detach_dzen2();
{% endhighlight %}

This step also add system command that kill
any previous dzen2 instance. So it will be guaranteed,
that the dzen2 shown is coming from the latest script.

{% include toc/2017/04/pipe-and-fork-similar-05.html %}

-- -- --

### How does it works ?

Any code after the <code>fork</code> executed in both parent and child.
The child process has been detached from parent process.
The only different is the <code>$pid</code>.

{% highlight perl %}
sub detach_dzen2 { 
    my $pid = fork;
    return if $pid;     # in the parent process
    
    run_dzen2();
    exit; 
}
{% endhighlight %}

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[gitlab.com/.../dotfiles/.../perl-07-conky.pl][dotfiles-perl-07-conky]

{% highlight perl %}
#!/usr/bin/perl

use warnings;
use strict;
use File::Basename;
use IO::Pipe;

sub get_dzen2_parameters { 
    my $xpos    = 0;
    my $ypos    = 0;
    my $width   = 640;
    my $height  = 24;
    my $fgcolor = "#000000";
    my $bgcolor = "#ffffff";
    my $font    = "-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*";

    my $parameters = "  -x $xpos -y $ypos -w $width -h $height";
    $parameters   .= " -fn '$font'";
    $parameters   .= " -ta c -bg '$bgcolor' -fg '$fgcolor'";
    $parameters   .= " -title-name dzentop";

    # print $parameters . "\n";    
    return $parameters;
}

sub generated_output {
    my $pipeout = shift;
    my $pipein  = IO::Pipe->new();
     
    my $dirname = dirname(__FILE__);
    my $path    = "$dirname/../assets";       
    my $cmd     = "conky -c $path/conky.lua";
    my $handle  = $pipein->reader($cmd);

    while(<$pipein>) {
        print $pipeout $_;
        flush $pipeout;
    }
    
    $pipein->close();
}

sub run_dzen2 { 

    my $pipeout = IO::Pipe->new();
    
    my $cmd = "dzen2 " . get_dzen2_parameters();
    my $handle = $pipeout->writer($cmd);
    
    generated_output($pipeout);
    $pipeout->close();
}

sub detach_dzen2 { 
    my $pid = fork;
    return if $pid;     # in the parent process
    
    run_dzen2();
    exit; 
}

sub detach_transset { 
    my $pid = fork;
    return if $pid;     # in the parent process
    
    sleep 1;
    system('transset .8 -n dzentop >/dev/null');
    
    exit; 
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
system('pkill dzen2');

# run process in the background
detach_dzen2();

# optional transparency
detach_transset();
{% endhighlight %}

This would have <code>dzen2</code> output similar to this below.

![Pipe: to Dzen2][image-time-dzen]{: .img-responsive }

	You may use transset-df instead of transset.

{% include toc/2017/04/pipe-and-fork-similar-07.html %}

-- -- --

### How does it works ?

	Nothing new here.

-- -- --

### Lemonbar

I also provide Lemonbar, instead of Dzen2.
The code is very similar.

**Source**:
*	[gitlab.com/.../dotfiles/.../perl-17-conky.pl][dotfiles-perl-17-conky]

{% include toc/2017/04/pipe-and-fork-similar-17.html %}

-- -- --

### Coming up Next

There already an advance case of Pipe and Fork.
Multitier, and bidirectional.

*	[HerbstluftWM Event Idle in Perl][local-perl-idle]

-- -- --

There above are some simple codes I put together. 
I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://gitlab.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua
[local-perl-idle]:   {{ site.url }}/desktop/2017/06/13/herbstlustwm-event-idle-perl.html

[dotfiles-perl-01-basic]:     {{ dotfiles_path }}/perl/perl-01-basic.pl
[dotfiles-perl-02-system]:    {{ dotfiles_path }}/perl/perl-02-system-shell.pl
[dotfiles-perl-02-uni-io]:    {{ dotfiles_path }}/perl/perl-02-uni-io.pl
[dotfiles-perl-02-uni-open]:  {{ dotfiles_path }}/perl/perl-02-uni-open.pl
[dotfiles-perl-03-pipe-io]:   {{ dotfiles_path }}/perl/perl-03-pipe-io.pl
[dotfiles-perl-03-pipe-open]: {{ dotfiles_path }}/perl/perl-03-pipe-open.pl
[dotfiles-perl-05-fork]:      {{ dotfiles_path }}/perl/perl-05-fork-sub.pl
[dotfiles-perl-07-conky]:     {{ dotfiles_path }}/perl/perl-07-fork-conky.pl
[dotfiles-perl-17-conky]:    {{ dotfiles_path }}/perl/perl-17-fork-conky.pl

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-perl.png

[local-overview]: {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
