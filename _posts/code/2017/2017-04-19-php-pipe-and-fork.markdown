---
layout: post
title:  "Piping and Forking in PHP"
categories: code
date:   2017-04-19 17:35:15 +0700
tags: [coding, conky, php]
author: epsi

excerpt:
  How to be a PHP Plumber.
  
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

### PHP Plumber.

	You need a wesbite. You have a website. Why not ceate your own ?

PHP is known as a language to create a Website.
Despite of this, PHP can served as multiporpuse script.
For historical reason. BASH evolve to Perl, Perl Evolve to PHP.
Since we have similar behaviour borrowed form the predecessor,
it would not be so difficult to include PHP in this Tutorial group.

In fact this PHP script version is very similar with Lua script version.

> Goal: A script that continuously show date and time,
> with Dzen2, and Conky.

Before you dip your toe to scripting,
you might desire to know the reason by reading this overview.

**Reading**

*	[Piping and Forking in Linux Script][local-overview]

-- -- --

{% include post/2017/04/pipe-and-fork-language.md %}

-- -- --

### Compatibility

For accounting reason in my company.
I'm still using PHP56 in my notebook.
But for the reader, I moved on to PHP7.
You are free to switch the <code>#shebang</code>.

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

{% include post/2017/04/pipe-and-fork-similar-01.md %}

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

Next guidance need bidirectional capability.
Bidirectional means, a process can read and write at the same time.
So I append <code>proc_open</code> method.

This is a very simple.  Just a few self explanatory lines.
This very short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I add <code>_ dir _</code>, relative to the PHP source,
to locate the conky script assets.

**Source**:

*	[github.com/.../dotfiles/.../php-02-popen.php][dotfiles-php-02-popen]

*	[github.com/.../dotfiles/.../php-02-proc-open.php][dotfiles-php-02-proc-open]

{% highlight php %}
#!/usr/bin/php
<?php # using PHP7

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

Using <code>proc_open</code>:

{% highlight php %}
#!/usr/bin/php
<?php # using PHP7

# http://php.net/manual/en/function.proc-open.php

$path    = __dir__."/../assets";
$cmdin   = 'conky -c '.$path.'/conky.lua';
$cmdout  = 'dzen2';

$descriptorspec = array(
   0 => array('pipe', 'r'),  // stdin
   1 => array('pipe', 'w'),  // stdout
   2 => array('pipe', 'w',)  // stderr
);

$procin  = proc_open($cmdin,  $descriptorspec, $pipein);
$procout = proc_open($cmdout, $descriptorspec, $pipeout);

if (is_resource($procin)) {
    while(!feof($pipein[1])) {
        $buffer = fgets($pipein[1]);
        fwrite($pipeout[0], $buffer);
    }

    proc_close($procin);
    proc_close($procout);
}
{% endhighlight %}

You can see, how simple it is.
This would have <code>less</code> output similar to this below.

![Pipe: to Less][image-time-less]{: .img-responsive }

	Your wallpaper might be different than mine.

{% include post/2017/04/pipe-and-fork-similar-02.md %}

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

Also <code>proc_open</code> that has bidirectional capability.

**Source**:

*	[github.com/.../dotfiles/.../php-03-popen.php][dotfiles-php-03-popen]

*	[github.com/.../dotfiles/.../php-03-proc-open.php][dotfiles-php-03-proc-open]

{% highlight php %}
<?php # using PHP7

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

Using <code>proc_open</code>:

{% highlight php %}
#!/usr/bin/php
<?php # using PHP7

$timeformat = '%a %b %d %H:%M:%S';
$cmdout  = 'dzen2';

$descriptorspec = array(
   0 => array('pipe', 'r'),  // stdin
   1 => array('pipe', 'w'),  // stdout
   2 => array('pipe', 'w',)  // stderr
);

$procout = proc_open($cmdout, $descriptorspec, $pipeout);

if (is_resource($procout)) {
    do {
        $datestr = strftime($timeformat)."\n";
        fwrite($pipeout[0], $datestr);

        sleep(1);
    } while (true);

    proc_close($procout);
}
{% endhighlight %}

{% include post/2017/04/pipe-and-fork-similar-03.md %}

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
<?php # using PHP7

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

{% include post/2017/04/pipe-and-fork-similar-05.md %}

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
<?php # using PHP7

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
        system('transset .8 -n dzentop >/dev/null');
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

	You may use transset-df instead of transset.

{% include post/2017/04/pipe-and-fork-similar-07.md %}

-- -- --

### How does it works ?

	Nothing new here.

-- -- --

### Lemonbar

I also provide Lemonbar, instead of Dzen2.
The code is very similar.

**Source**:
*	[github.com/.../dotfiles/.../php-17-conky.php][dotfiles-php-17-conky]

{% include post/2017/04/pipe-and-fork-similar-17.md %}

-- -- --

### Coming up Next

There already an advance case of Pipe and Fork.
Multitier, and bidirectional.

*	[HerbstluftWM Event Idle in PHP][local-php-idle]

-- -- --

There above are some simple codes I put together. 
I'm mostly posting codes so I won't have
any problems finding it in the future.

Thank you for reading.


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/code/2017/04' %}
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua
[local-php-idle]:   {{ site.url }}/desktop/2017/06/16/herbstlustwm-event-idle-php.html

[dotfiles-php-01-basic]:   {{ dotfiles_path }}/php/php-01-basic.php
[dotfiles-php-02-popen]:   {{ dotfiles_path }}/php/php-02-popen.php
[dotfiles-php-03-popen]:   {{ dotfiles_path }}/php/php-03-popen.php
[dotfiles-php-05-fork]:    {{ dotfiles_path }}/php/php-05-fork-function.php
[dotfiles-php-07-conky]:   {{ dotfiles_path }}/php/php-07-fork-conky.php

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-php.png

[local-overview]: {{ site.url }}/code/2017/04/23/overview-pipe-and-fork.html
