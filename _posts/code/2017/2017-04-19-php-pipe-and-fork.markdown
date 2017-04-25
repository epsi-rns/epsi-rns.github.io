---
layout: post-sidemenu-wm
title:  "Piping and Forking in PHP"
categories: code
date:   2017-04-19 17:35:15 +0700
tags: [coding, conky, php]
author: epsi

excerpt:
  How to be a PHP Plumber.
  
---

### PHP Plumber.

	You need a wesbite. You have a website. Why not ceate your own ?

PHP is known as a language to create a Website.
Despite of this, PHP can served as multiporpuse script.
For historical reason. BASH evolve to Perl, Perl Evolve to PHP.
Since we have similar behaviour borrowed form the predecessor,
it would not be so difficult to include PHP in this Tutorial group.

In fact this PHP script version is very similar with Lua script version.

	Goal: A script that continuously show date and time,
	with Dzen2, and Conky.

Before you dip your toe to scripting,
you might desire to know the reason by reading this preface.

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

### Compatibility

For accounting reason in my company.
I'm still using PHP56 in my notebook.
You are free to switch the <code>#shebang</code> to PHP7.

Do not worry, I do not use any sophisticated code.

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

*	[github.com/.../dotfiles/.../php-01-basic.php][dotfiles-php-01-basic]

{% highlight php %}
#!/usr/bin/php56  
<?php 

$timeformat = '%a %b %d %H:%M:%S';

do {
    print strftime($timeformat)."\n";
    sleep(1);
} while (true);
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

### A Unidirectional Pipe Between External Command

This step is overview of Pipe between two external command.
Instead of system command, this utilize <code>popen</code>
using two handles: <code>pipein</code> and <code>pipeout</code>.

This is a very simple.  Just a few self explanatory lines.
This very short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I add <code>_ dir _</code>, relative to the PHP source,
to locate the conky script assets.

**Source**:

*	[github.com/.../dotfiles/.../php-02-popen.php][dotfiles-php-02-popen]

{% highlight php %}
#!/usr/bin/php56  
<?php 

# http://php.net/manual/en/function.popen.php

$path    = __dir__."/../assets";
$cmdin   = 'conky -c '.$path.'/conky.lua';
$cmdout  = 'less'; # or 'dzen2'

# handle
$pipein  = popen($cmdin,  "r");
$pipeout = popen($cmdout, "w");

while(!feof($pipein)) {
    $buffer = fgets($pipein);
    fwrite($pipeout, $buffer);
    flush();
}

pclose($pipein);
pclose($pipeout);

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

PHP act as middle man, anything read from
<code>$pipein</code> written to <code>$pipeout</code>

{% highlight php %}
while(!feof($pipein)) {
    $buffer = fgets($pipein);
    fwrite($pipeout, $buffer);
    flush();
}
{% endhighlight %}

-- -- --

### A Unidirectional Pipe from Internal Function

Still with <code>popen</code>.
Instead of using <code>pipein</code> ,
we use internal function as source feed.
And <code>pipeout</code> to external command.

	Do not forget to flush.

**Source**:

*	[github.com/.../dotfiles/.../php-03-popen.php][dotfiles-php-03-popen]

{% highlight php %}
#!/usr/bin/php56  
<?php 

$timeformat = '%a %b %d %H:%M:%S';

$cmdout  = 'less'; # or 'dzen2'
$pipeout = popen($cmdout, "w");

do {
    $datestr = strftime($timeformat)."\n";
    fwrite($pipeout, $datestr);
    flush();
    sleep(1);
} while (true);

pclose($pipeout);
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
But instead of reading from <code>$pipein</code>,
it is managed by internal process using <code>fwrite($pipeout, ...)</code>.

{% highlight php %}
    $datestr = strftime($timeformat)."\n";
    fwrite($pipeout, $datestr)
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

*	[github.com/.../dotfiles/.../php-05-fork.php][dotfiles-php-05-fork]

{% highlight php %}
#!/usr/bin/php56  
<?php 

function get_dzen2_parameters() 
{ 
    $xpos    = 0;
    $ypos    = 0;
    $width   = 640;
    $height  = 24;
    $fgcolor = "#000000";
    $bgcolor = "#ffffff";
    $font    = "-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*";

    $parameters  = "  -x $xpos -y $ypos -w $width -h $height";
    $parameters .= " -fn '$font'";
    $parameters .= " -ta c -bg '$bgcolor' -fg '$fgcolor'";
    $parameters .= " -title-name dzentop";

    return $parameters;
}

function generated_output($process)
{
    $timeformat = '%a %b %d %H:%M:%S';

    do {
        $datestr = strftime($timeformat)."\n";
        fwrite($process, $datestr);
        flush();
        sleep(1);
    } while (true);
}

function run_dzen2() 
{ 
    $cmdout  = 'dzen2 '.get_dzen2_parameters();
    $pipeout = popen($cmdout, "w");

    generated_output($pipeout);

    pclose($pipeout);
}

function detach_dzen2() 
{ 
    $pid = pcntl_fork();
    
    switch($pid) {         
         case -1 : die('could not fork'); // fork errror         
         case 0  : run_dzen2(); break;    // we are the child
         default : return $pid;           // we are the parent             
    }    
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

Any code after the <code>pcntl_fork()</code>
executed in both parent and child.
The child process has been detached from parent process.
The only different is the <code>$pid</code>.

{% highlight php %}
function detach_dzen2() 
{ 
    $pid = pcntl_fork();
    
    switch($pid) {         
         case -1 : die('could not fork'); // fork errror         
         case 0  : run_dzen2(); break;    // we are the child
         default : return $pid;           // we are the parent             
    }    
}
{% endhighlight %}

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[github.com/.../dotfiles/.../php-07-conky.php][dotfiles-php-07-conky]

{% highlight php %}
#!/usr/bin/php56  
<?php 

function get_dzen2_parameters() 
{ 
    $xpos    = 0;
    $ypos    = 0;
    $width   = 640;
    $height  = 24;
    $fgcolor = "#000000";
    $bgcolor = "#ffffff";
    $font    = "-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*";

    $parameters  = "  -x $xpos -y $ypos -w $width -h $height";
    $parameters .= " -fn '$font'";
    $parameters .= " -ta c -bg '$bgcolor' -fg '$fgcolor'";
    $parameters .= " -title-name dzentop";

    return $parameters;
}

function generated_output($process)
{
    $path    = __dir__."/../assets";
    $cmdin   = 'conky -c '.$path.'/conky.lua';
    $pipein  = popen($cmdin,  "r"); # handle
    
    while(!feof($pipein)) {
        $buffer = fgets($pipein);
        fwrite($process, $buffer);
        flush();
    }
    
    pclose($pipein);
}

function run_dzen2() 
{ 
    $cmdout  = 'dzen2 '.get_dzen2_parameters();
    $pipeout = popen($cmdout, "w");

    generated_output($pipeout);

    pclose($pipeout);
}

function detach_dzen2() 
{ 
    $pid = pcntl_fork();
    
    switch($pid) {         
         case -1 : die('could not fork'); // fork errror         
         case 0  : run_dzen2(); break;    // we are the child
         default : return $pid;           // we are the parent             
    }    
}

function detach_transset() { 
    $pid = pcntl_fork();
    if ($pid == 0) { 
        sleep(1);
        system('transset .8 -n dzentop >/dev/null 2');
    }
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

[dotfiles-php-01-basic]:   {{ dotfiles_path }}/php/php-01-basic.php
[dotfiles-php-02-popen]:   {{ dotfiles_path }}/php/php-02-popen.php
[dotfiles-php-03-popen]:   {{ dotfiles_path }}/php/php-03-popen.php
[dotfiles-php-05-fork]:    {{ dotfiles_path }}/php/php-05-fork-function.php
[dotfiles-php-07-conky]:   {{ dotfiles_path }}/php/php-07-fork-conky.php

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-php.png

[local-overview]: {{ site.url }}/pages/desktop/pipe.html
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
