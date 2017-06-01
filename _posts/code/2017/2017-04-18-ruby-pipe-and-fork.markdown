---
layout: post-sidemenu-wm
title:  "Piping and Forking in Ruby"
categories: code
date:   2017-04-18 17:35:15 +0700
tags: [coding, conky, ruby]
author: epsi

excerpt:
  How to be a Ruby Plumber.
  
---

### Ruby Plumber.

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

*	[github.com/.../dotfiles/.../ruby-01-basic.rb][dotfiles-ruby-01-basic]

{% highlight ruby %}
#!/usr/bin/ruby

timeformat = '%a %b %d %H:%M:%S'

while true do
    localtime = Time.now
    datestr = localtime.strftime(timeformat)
    puts datestr

    sleep(1)
end
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

Ruby provide a dozen Pipe mechanism. Here we have <code>system</code>, 
<code>spawn</code>, and <code>shell.transact</code>. 
These three related to system shell.

Using system shell is simple and straightforward.
But it does not have any ability,
to stream internal function process,
that required later on this article.

**Source**:

*	[github.com/.../dotfiles/.../ruby-02-shell.rb][dotfiles-ruby-02-shell]

*	[github.com/.../dotfiles/.../ruby-02-spawn.rb][dotfiles-ruby-02-spawn]

*	[github.com/.../dotfiles/.../ruby-02-system.rb][dotfiles-ruby-02-system]

Using <code>system</code>:

{% highlight perl %}
#!/usr/bin/ruby

path    = __dir__+ "/../assets"
cmdin   = 'conky -c ' + path + '/conky.lua'
cmdout  = 'less' # or 'dzen2'
cmd     = cmdin + ' | ' + cmdout

system(cmd
{% endhighlight %}

Using <code>spawn</code>:

{% highlight perl %}
#!/usr/bin/ruby

path    = __dir__+ "/../assets"
cmdin   = 'conky -c ' + path + '/conky.lua'
cmdout  = 'dzen2' 

read, write = IO.pipe

spawn(cmdin, out: write)
spawn(cmdout, in: read )

write.close
{% endhighlight %}

Using <code>shell.transact</code>:

{% highlight perl %}
#!/usr/bin/ruby

require 'shell'

path    = __dir__+ "/../assets"
cmdin   = 'conky -c ' + path + '/conky.lua'
cmdout  = 'dzen2'

sh = Shell.new

sh.transact { system(cmdin) | system(cmdout) }
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

This step is overview of Pipe between two external command.
This short script is using <code>conky</code>
as pipe source feed and <code>less</code> as pipe target.
Showing time and date forever in the console.

	This infinite pipe run in time-less fashioned.

I add <code>_ dir _</code>, relative to the Ruby source,
to locate the conky script assets.

We use this flexible Ruby's <code>IO.popen</code> mechanism.

**Source**:

*	[github.com/.../dotfiles/.../ruby-02-popen.rb][dotfiles-ruby-02-popen]

{% highlight ruby %}
#!/usr/bin/ruby

# http://ruby-doc.org/core-1.8.7/IO.html#method-c-popen

def generated_output(stdin)
    path    = __dir__+ "/../assets"
    cmdin   = 'conky -c ' + path + '/conky.lua'
    
    IO.popen(cmdin, "r") do |f| 
        while f do
          stdin.puts f.gets
        end
        f.close()    
    end
end

cmdout  = 'less' # or 'dzen2'

IO.popen(cmdout, "w") do |f| 
    generated_output(f) 
        
    f.close()    
end
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

First IO.popen using cmdout create stdin.
Second IO.popen using cmdin create stdout.
Ruby act as middle man, stdout read by Ruby, and put in stdin.

{% highlight ruby %}
    IO.popen(cmdin, "r") do |f| 
        while f do
          stdin.puts f.gets
        end
        f.close()    
    end
{% endhighlight %}

-- -- --

### A Unidirectional Pipe from Internal Function

Again, Ruby provide a dozen Pipe mechanism.
Here we have <code>Open3.pipeline_w</code>, <code>IO.pipe</code>, 
<code>IO.popen</code>, and <code>PTY.spawn</code>. 
These three related unidirectional capability that we need.
Or to be precise, they have bidirectional capability.

Using internal function as source feed
to external command is straight forward.
This should be self explanatory.

	Do not forget to flush.

**Source**:

*	[github.com/.../dotfiles/.../ruby-03-open3.rb][dotfiles-ruby-03-open3]

*	[github.com/.../dotfiles/.../ruby-03-pipe-io.rb][dotfiles-ruby-03-pipe-io]

*	[github.com/.../dotfiles/.../ruby-03-popen.rb][dotfiles-ruby-03-popen]

*	[github.com/.../dotfiles/.../ruby-03-pty.rb][dotfiles-ruby-03-pty]

Using <code>IO.pipe</code>:

{% highlight ruby %}
#!/usr/bin/ruby

# https://ruby-doc.org/core-2.2.0/Process.html#method-c-spawn

def generated_output(stdin)
  timeformat = '%a %b %d %H:%M:%S'

  while true do
    localtime = Time.now
    datestr = localtime.strftime(timeformat)
    stdin.puts datestr

    sleep(1)
  end
end

cmdout  = 'less' # or 'dzen2'

rd, wr = IO.pipe

if fork
  wr.close
  spawn(cmdout, in: rd )
  rd.close
  Process.wait
else
  rd.close
  generated_output(wr)
  wr.close
end
{% endhighlight %}

Using <code>IO.popen</code>:

{% highlight ruby %}
#!/usr/bin/ruby

# http://ruby-doc.org/core-1.8.7/IO.html#method-c-popen

def generated_output(stdin)
  timeformat = '%a %b %d %H:%M:%S'

  while true do
    localtime = Time.now
    datestr = localtime.strftime(timeformat)
    stdin.puts datestr

    sleep(1)
  end
end

cmdout  = 'less' # or 'dzen2'

IO.popen(cmdout, "w") { |f| generated_output(f) }
{% endhighlight %}

Using <code>Open3.pipeline_w</code>:

{% highlight ruby %}
#!/usr/bin/ruby

# http://ruby-doc.org/stdlib-2.4.1/libdoc/open3/rdoc/Open3.html#method-c-pipeline_w

require 'open3'

def generated_output(stdin)
  timeformat = '%a %b %d %H:%M:%S'

  while true do
    localtime = Time.now
    datestr = localtime.strftime(timeformat)
    stdin.puts datestr

    sleep(1)
  end
end

cmdout  = 'less' # or 'dzen2'

Open3.pipeline_w(cmdout) {|i, ts| generated_output(i) }

{% endhighlight %}

Using <code>PTY.spawn</code>:

{% highlight ruby %}
#!/usr/bin/ruby

# https://ruby-doc.org/stdlib-2.2.3/libdoc/pty/rdoc/PTY.html

require 'pty'

def generated_output(stdin)
  timeformat = '%a %b %d %H:%M:%S'

  while true do
    localtime = Time.now
    datestr = localtime.strftime(timeformat)
    stdin.puts datestr

    sleep(1)
  end
end

cmdout  = 'dzen2' 

PTY.spawn(cmdout) { |output, input, pid| generated_output(input) }
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
But instead of reading from stdout,
it is managed by internal process using <code>stdin.puts</code>.

{% highlight ruby %}
    datestr = localtime.strftime(timeformat)
    stdin.puts datestr
{% endhighlight %}

-- -- --

### Fork Overview

This step use internal function as source feed,
as continuation of previous step.
To avoid complexity of longer script,
most code is written inside function


This step use dzen2, with complete parameters. 
This dzen2 is forked, running in the background.
Detached from the script,
no need to wait for dzen2 to finish the script.

**Source**:

*	[github.com/.../dotfiles/.../ruby-05-fork.rb][dotfiles-ruby-05-fork]

{% highlight ruby %}
#!/usr/bin/ruby

def get_dzen2_parameters()
    xpos    = '0'
    ypos    = '0'
    width   = '640'
    height  = '24'
    fgcolor = '#000000'
    bgcolor = '#ffffff'
    font    = '-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'

    parameters  = "  -x #{xpos} -y #{ypos} -w #{width} -h #{height}"
    parameters << " -fn '#{font}'"
    parameters << " -ta c -bg '#{bgcolor}' -fg '#{fgcolor}'"
    parameters << " -title-name dzentop"

    return parameters
end

def generated_output(stdin)
    timeformat = '%a %b %d %H:%M:%S'

    while true do
        localtime = Time.now
        datestr = localtime.strftime(timeformat)
        stdin.puts datestr

        sleep(1)
    end
end

def run_dzen2()
    cmdout  = 'dzen2 ' + get_dzen2_parameters()
    IO.popen(cmdout, "w") do |f| 
        generated_output(f) 
        
        f.close()    
    end
end

def detach_dzen2()
    pid = fork { run_dzen2() }
    Process.detach(pid)
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
system('pkill dzen2')

# run process in the background
detach_dzen2()
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

Any code inside the <code>fork</code> block
executed under new child process with new <code>pid</code>
The child process could be detached from parent process
using <code>Process.detach(pid)</code>.

{% highlight ruby %}
def detach_dzen2()
    pid = fork { run_dzen2() }
    Process.detach(pid)
end
{% endhighlight %}

-- -- --

### Polishing The Script

This step, we use conky again, as a source feed.
And also parameterized dzen2 as continuation of previous step.

This step add optional transset transparency,
detached from script. So we two forks, dzen and transset.

**Source**:

*	[github.com/.../dotfiles/.../ruby-07-conky.rb][dotfiles-ruby-07-conky]

{% highlight ruby %}
#!/usr/bin/ruby

def get_dzen2_parameters()
    xpos    = '0'
    ypos    = '0'
    width   = '640'
    height  = '24'
    fgcolor = '#000000'
    bgcolor = '#ffffff'
    font    = '-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*'

    parameters  = "  -x #{xpos} -y #{ypos} -w #{width} -h #{height}"
    parameters << " -fn '#{font}'"
    parameters << " -ta c -bg '#{bgcolor}' -fg '#{fgcolor}'"
    parameters << " -title-name dzentop"

    return parameters
end

def generated_output(stdin)
    path    = __dir__+ "/../assets"
    cmdin   = 'conky -c ' + path + '/conky.lua'
    
    IO.popen(cmdin, "r") do |f| 
        while f do
          stdin.puts f.gets
        end
        f.close()    
    end
end

def run_dzen2()
    cmdout  = 'dzen2 ' + get_dzen2_parameters()
    IO.popen(cmdout, "w") do |f| 
        generated_output(f) 
        
        f.close()    
    end
end

def detach_dzen2()
    pid = fork { run_dzen2() }
    Process.detach(pid)
end

def detach_transset()
    pid = fork do
        sleep(1)
        system('transset .8 -n dzentop >/dev/null')        
    end
    
    Process.detach(pid)
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
system('pkill dzen2')

# run process in the background
detach_dzen2()

# optional transparency
detach_transset()
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
{% assign dotfiles_path = 'https://github.com/epsi-rns/dotfiles/blob/master/standalone/pipe' %}

[dotfiles-conky]: {{ dotfiles_path }}/assets/conky.lua

[dotfiles-ruby-01-basic]:   {{ dotfiles_path }}/ruby/ruby-01-basic.rb
[dotfiles-ruby-02-shell]:   {{ dotfiles_path }}/ruby/ruby-02-shell.rb
[dotfiles-ruby-02-spawn]:   {{ dotfiles_path }}/ruby/ruby-02-spawn.rb
[dotfiles-ruby-02-system]:  {{ dotfiles_path }}/ruby/ruby-02-system.rb
[dotfiles-ruby-02-popen]:   {{ dotfiles_path }}/ruby/ruby-02-popen.rb
[dotfiles-ruby-03-open3]:   {{ dotfiles_path }}/ruby/ruby-03-open3.rb
[dotfiles-ruby-03-pipe-io]: {{ dotfiles_path }}/ruby/ruby-03-pipe-io.rb
[dotfiles-ruby-03-popen]:   {{ dotfiles_path }}/ruby/ruby-03-popen.rb
[dotfiles-ruby-03-pty]:     {{ dotfiles_path }}/ruby/ruby-03-pty.rb
[dotfiles-ruby-05-fork]:    {{ dotfiles_path }}/ruby/ruby-05-fork-def.rb
[dotfiles-ruby-07-conky]:   {{ dotfiles_path }}/ruby/ruby-07-fork-conky.rb

[image-time-less]:  {{ asset_path }}/pipe-time-less.png
[image-time-dzen]:  {{ asset_path }}/pipe-time-dzen.png
[image-time-basic]: {{ asset_path }}/pipe-basic-ruby.png

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
