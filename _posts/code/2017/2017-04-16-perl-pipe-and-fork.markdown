---
layout: post-sidemenu-wm
title:  "Piping and Forking in Perl"
categories: code
date:   2017-04-16 17:35:15 +0700
tags: [coding, conky, perl]
author: epsi

excerpt:
  How to be a Perl Plumber.
  
---

### Perl Plumber.

	Goal: A script that continuously show date and time,
	with Dzen2, and Conky.

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

*	[github.com/.../dotfiles/.../perl-01-basic.pl][dotfiles-perl-01-basic]

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

-- -- --

### Spawning Using System Shell

Using system shell is simple and straightforward.
But it does not have any ability,
to stream internal function process,
that required later on this article.


**Source**:

*	[github.com/.../dotfiles/.../perl-02-system.pl][dotfiles-perl-02-system]

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

At least, there are two mechanism to pipe in Perl.
First using native <code>open</code>,
and the second by using object oriented <code>IO::Pipe</code.

**Source**:

*	[github.com/.../dotfiles/.../perl-02-uni-io.pl][dotfiles-perl-02-uni-io]

*	[github.com/.../dotfiles/.../perl-02-uni-open.pl][dotfiles-perl-02-uni-open]

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

You can see, how simple it is.
This would have <code>less</code> output similar to this below.

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

-- -- --

### A Unidirectional Pipe from Internal Function

Using internal function as source feed
to external command is straight forward.
This should be self explanatory.

	Do not forget to flush.

As previous example, we are using two mechanism,
<code>open</code> and <code>IO::Pipe</code>

**Source**:

*	[github.com/.../dotfiles/.../perl-03-pipe-io.pl][dotfiles-perl-03-pipe-io]

*	[github.com/.../dotfiles/.../perl-03-pipe-open.pl][dotfiles-perl-03-pipe-open]

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

-- -- --

### Fork Overview

This step use internal function as source feed,
as continuation of previous step.

This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[github.com/.../dotfiles/.../perl-05-fork.pl][dotfiles-perl-05-fork]

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

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[github.com/.../dotfiles/.../perl-07-conky.pl][dotfiles-perl-07-conky]

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
    system('transset .8 -n dzentop >/dev/null 2');
    
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

-- -- --

There above are some simple codes I put together. 
I’m mostly posting codes so I won’t have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lang' %}

[dotfiles-perl-01-basic]:     {{ dotfiles_path }}/perl/perl-01-basic.pl
[dotfiles-perl-02-system]:    {{ dotfiles_path }}/perl/perl-02-system-shell.pl
[dotfiles-perl-02-uni-io]:    {{ dotfiles_path }}/perl/perl-02-uni-io.pl
[dotfiles-perl-02-uni-open]:  {{ dotfiles_path }}/perl/perl-02-uni-open.pl
[dotfiles-perl-03-pipe-io]:   {{ dotfiles_path }}/perl/perl-03-pipe-io.pl
[dotfiles-perl-03-pipe-open]: {{ dotfiles_path }}/perl/perl-03-pipe-open.pl
[dotfiles-perl-05-fork]:      {{ dotfiles_path }}/perl/perl-05-fork-sub.pl
[dotfiles-perl-07-conky]:     {{ dotfiles_path }}/perl/perl-07-fork-conky.pl

[image-time-less]: {{ asset_path }}/pipe-time-less.png
[image-time-dzen]: {{ asset_path }}/pipe-time-dzen.png

