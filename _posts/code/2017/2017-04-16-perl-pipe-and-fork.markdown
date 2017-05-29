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

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky.

Before you dip your toe to scripting,
you might desire to know the reason by reading this overview.

**Reading**

*	[Piping and Forking in Linux Script][local-overview]

-- -- --

### Piping and Forking in Many Languages

This article is one part of a collection.
All integrated, on related to another.
So we can compare each other quickly.

Tutorial/ Guidance/ Article:
[[ Pipe Overview ]][local-overview]
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

Call to this simple code would produce time marching,
one after another, below the command line prompt.

![Pipe: Basic][image-time-basic]{: .img-responsive }

Similar Code: 
[[ BASH basic ]][dotfiles-bash-01-basic]
[[ Perl basic ]][dotfiles-perl-01-basic]
[[ Python basic datetime ]][dotfiles-python-01-basic-date]
[[ Python basic time ]][dotfiles-python-01-basic-time]
[[ Ruby basic ]][dotfiles-ruby-01-basic]
[[ PHP basic ]][dotfiles-php-01-basic]
[[ Lua basic ]][dotfiles-lua-01-basic]
[[ Haskell basic ]][dotfiles-haskell-01-basic]

-- -- --

### External Command as Source Feed

Beside previous simple loop that is used as Internal Command,
this tutorial also provide Conky as External Command
in <code class="code-file">asset</code> directory.
I made it as simple as possible.

**Source**:

*	[github.com/.../dotfiles/.../conky.lua][dotfiles-conky]

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

Similar Code: 
[[ Perl system ]][dotfiles-perl-02-system]
[[ Python system ]][dotfiles-python-02-system]
[[ Python popen ]][dotfiles-python-02-popen]
[[ Ruby system ]][dotfiles-ruby-02-system]
[[ Ruby spawn ]][dotfiles-ruby-02-spawn]
[[ Ruby shell ]][dotfiles-ruby-02-shell]
[[ Haskell system ]][dotfiles-haskell-02-system]

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

Similar Code: 
[[ BASH native ]][dotfiles-bash-02-native]
[[ Perl uni IO ]][dotfiles-perl-02-uni-io]
[[ Perl uni open ]][dotfiles-perl-02-uni-open]
[[ Python subProcess]][dotfiles-python-02-subprocess]
[[ Ruby popen ]][dotfiles-ruby-02-popen]
[[ PHP popen ]][dotfiles-php-02-popen]
[[ Lua popen ]][dotfiles-lua-02-popen]
[[ Haskell createProcess ]][dotfiles-haskell-02-process]

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

Similar Code: 
[[ BASH pipe ]][dotfiles-bash-03-pipe]
[[ Perl pipe open ]][dotfiles-perl-03-pipe-open]
[[ Perl pipe IO ]][dotfiles-perl-03-pipe-io]
[[ Python subProcess ]][dotfiles-python-03-subprocess]
[[ Ruby pipe IO ]][dotfiles-ruby-03-pipe-io]
[[ Ruby popen ]][dotfiles-ruby-03-popen]
[[ Ruby open3 ]][dotfiles-ruby-03-open3]
[[ Ruby PTY ]][dotfiles-ruby-03-pty]
[[ PHP popen ]][dotfiles-php-03-popen]
[[ Lua popen ]][dotfiles-lua-03-popen]
[[ Haskell createProcess ]][dotfiles-haskell-03-process]

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

Similar Code: 
[[ BASH fork ]][dotfiles-bash-05-fork]
[[ Perl fork ]][dotfiles-perl-05-fork]
[[ Python fork ]][dotfiles-python-05-fork]
[[ Ruby fork ]][dotfiles-ruby-05-fork]
[[ PHP fork ]][dotfiles-php-05-fork]
[[ Lua fork ]][dotfiles-lua-05-fork]
[[ Haskell fork ]][dotfiles-haskell-05-fork]

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

	You may use transset-df instead of transset.

Similar Code: 
[[ BASH conky ]][dotfiles-bash-07-conky]
[[ Perl conky ]][dotfiles-perl-07-conky]
[[ Python conky ]][dotfiles-python-07-conky]
[[ Ruby conky ]][dotfiles-ruby-07-conky]
[[ PHP conky ]][dotfiles-php-07-conky]
[[ Lua conky ]][dotfiles-lua-07-conky]
[[ Haskell conky ]][dotfiles-haskell-07-conky]

-- -- --

### How does it works ?

	Nothing new here.

-- -- --

There above are some simple codes I put together. 
I’m mostly posting codes so I won’t have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/lang' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[dotfiles-perl-01-basic]:     {{ dotfiles_path }}/perl/perl-01-basic.pl
[dotfiles-perl-02-system]:    {{ dotfiles_path }}/perl/perl-02-system-shell.pl
[dotfiles-perl-02-uni-io]:    {{ dotfiles_path }}/perl/perl-02-uni-io.pl
[dotfiles-perl-02-uni-open]:  {{ dotfiles_path }}/perl/perl-02-uni-open.pl
[dotfiles-perl-03-pipe-io]:   {{ dotfiles_path }}/perl/perl-03-pipe-io.pl
[dotfiles-perl-03-pipe-open]: {{ dotfiles_path }}/perl/perl-03-pipe-open.pl
[dotfiles-perl-05-fork]:      {{ dotfiles_path }}/perl/perl-05-fork-sub.pl
[dotfiles-perl-07-conky]:     {{ dotfiles_path }}/perl/perl-07-fork-conky.pl

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-perl.png

[local-overview]: {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
[local-BASH]:     {{ site.url }}/code/2017/04/15/bash-pipe-and-fork.html
[local-Perl]:     {{ site.url }}/code/2017/04/16/perl-pipe-and-fork.html
[local-python]:   {{ site.url }}/code/2017/04/17/python-pipe-and-fork.html
[local-Ruby]:     {{ site.url }}/code/2017/04/18/ruby-pipe-and-fork.html
[local-PHP]:      {{ site.url }}/code/2017/04/19/php-pipe-and-fork.html
[local-Lua]:      {{ site.url }}/code/2017/04/20/lua-pipe-and-fork.html
[local-Haskell]:  {{ site.url }}/code/2017/04/21/haskell-pipe-and-fork.html

[dotfiles-BASH]:    {{ dotfiles_path }}/bash
[dotfiles-Perl]:    {{ dotfiles_path }}/perl
[dotfiles-python]:  {{ dotfiles_path }}/python
[dotfiles-Ruby]:    {{ dotfiles_path }}/ruby
[dotfiles-PHP]:     {{ dotfiles_path }}/php
[dotfiles-Lua]:     {{ dotfiles_path }}/lua
[dotfiles-Haskell]: {{ dotfiles_path }}/haskell

[dotfiles-bash-01-basic]:   {{ dotfiles_path }}/bash/bash-01-basic.sh
[dotfiles-perl-01-basic]:     {{ dotfiles_path }}/perl/perl-01-basic.pl
[dotfiles-python-01-basic-date]: {{ dotfiles_path }}/python/python-01-basic-date.py
[dotfiles-python-01-basic-time]: {{ dotfiles_path }}/python/python-01-basic-time.py
[dotfiles-ruby-01-basic]:   {{ dotfiles_path }}/ruby/ruby-01-basic.rb
[dotfiles-php-01-basic]:   {{ dotfiles_path }}/php/php-01-basic.php
[dotfiles-lua-01-basic]:   {{ dotfiles_path }}/lua/lua-01-basic.lua
[dotfiles-haskell-01-basic]:   {{ dotfiles_path }}/haskell/haskell-01-basic.hs

[dotfiles-perl-02-system]:    {{ dotfiles_path }}/perl/perl-02-system-shell.pl
[dotfiles-python-02-system]:  {{ dotfiles_path }}/python/python-02-system-shell.py
[dotfiles-python-02-popen]:   {{ dotfiles_path }}/python/python-02-popen.py
[dotfiles-ruby-02-system]:    {{ dotfiles_path }}/ruby/ruby-02-system.rb
[dotfiles-ruby-02-spawn]:     {{ dotfiles_path }}/ruby/ruby-02-spawn.rb
[dotfiles-ruby-02-shell]:     {{ dotfiles_path }}/ruby/ruby-02-shell.rb
[dotfiles-haskell-02-system]: {{ dotfiles_path }}/haskell/haskell-02-system.hs

[dotfiles-bash-02-native]:       {{ dotfiles_path }}/bash/bash-02-native.sh
[dotfiles-perl-02-uni-io]:       {{ dotfiles_path }}/perl/perl-02-uni-io.pl
[dotfiles-perl-02-uni-open]:     {{ dotfiles_path }}/perl/perl-02-uni-open.pl
[dotfiles-python-02-subprocess]: {{ dotfiles_path }}/python/python-02-subprocess-open.py
[dotfiles-ruby-02-popen]:        {{ dotfiles_path }}/ruby/ruby-02-popen.rb
[dotfiles-php-02-popen]:         {{ dotfiles_path }}/php/php-02-popen.php
[dotfiles-lua-02-popen]:         {{ dotfiles_path }}/lua/lua-02-popen.lua
[dotfiles-haskell-02-process]:   {{ dotfiles_path }}/haskell/haskell-02-process.hs

[dotfiles-bash-03-pipe]:         {{ dotfiles_path }}/bash/bash-03-pipe.sh
[dotfiles-perl-03-pipe-io]:      {{ dotfiles_path }}/perl/perl-03-pipe-io.pl
[dotfiles-perl-03-pipe-open]:    {{ dotfiles_path }}/perl/perl-03-pipe-open.pl
[dotfiles-python-03-subprocess]: {{ dotfiles_path }}/python/python-03-subprocess-simple.py
[dotfiles-ruby-03-open3]:        {{ dotfiles_path }}/ruby/ruby-03-open3.rb
[dotfiles-ruby-03-pipe-io]:      {{ dotfiles_path }}/ruby/ruby-03-pipe-io.rb
[dotfiles-ruby-03-popen]:        {{ dotfiles_path }}/ruby/ruby-03-popen.rb
[dotfiles-ruby-03-pty]:          {{ dotfiles_path }}/ruby/ruby-03-pty.rb
[dotfiles-php-03-popen]:         {{ dotfiles_path }}/php/php-03-popen.php
[dotfiles-lua-03-popen]:         {{ dotfiles_path }}/lua/lua-03-popen.lua
[dotfiles-haskell-03-process]:   {{ dotfiles_path }}/haskell/haskell-03-process.hs

[dotfiles-bash-05-fork]:    {{ dotfiles_path }}/bash/bash-05-fork.sh
[dotfiles-perl-05-fork]:    {{ dotfiles_path }}/perl/perl-05-fork-sub.pl
[dotfiles-python-05-fork]:  {{ dotfiles_path }}/python/python-05-fork-def.py
[dotfiles-ruby-05-fork]:    {{ dotfiles_path }}/ruby/ruby-05-fork-def.rb
[dotfiles-php-05-fork]:     {{ dotfiles_path }}/php/php-05-fork-function.php
[dotfiles-lua-05-fork]:     {{ dotfiles_path }}/lua/lua-05-fork-function.lua
[dotfiles-haskell-05-fork]: {{ dotfiles_path }}/haskell/haskell-05-fork.hs

[dotfiles-bash-07-conky]:    {{ dotfiles_path }}/bash/bash-07-conky.sh
[dotfiles-perl-07-conky]:    {{ dotfiles_path }}/perl/perl-07-fork-conky.pl
[dotfiles-python-07-conky]:  {{ dotfiles_path }}/python/python-07-fork-conky.py
[dotfiles-ruby-07-conky]:    {{ dotfiles_path }}/ruby/ruby-07-fork-conky.rb
[dotfiles-php-07-conky]:     {{ dotfiles_path }}/php/php-07-fork-conky.php
[dotfiles-lua-07-conky]:     {{ dotfiles_path }}/lua/lua-07-fork-function.lua
[dotfiles-haskell-07-conky]: {{ dotfiles_path }}/haskell/haskell-07-fork.hs
