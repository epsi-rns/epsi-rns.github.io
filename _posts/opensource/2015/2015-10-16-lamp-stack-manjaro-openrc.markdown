---
layout: post
title:  "Setup LAMP stack with Manjaro OpenRC"
date:   2015-10-16 12:49:15 +0700
categories: opensource
tags: [lamp, openrc]
author: epsi
---

Preface
----------

Hello all.<br/>
I hope everyone that is reading this, is having a really good day.

I decide to share my ordinary log, <br/>
about setting up LAMP stack which is very common.

With special customization

* OpenRC instead of systemd

* MariaDB instead of MySQL

* Manjaro 'pacman' instead of debian-based 'apt-get'.

<br/>
Have Fun

![LAMP Open-RC]({{ site.url }}/assets/posts/opensource/2015/10/lamp-manjaro-openrc-terminal.png)

Install
----------

### [terminal]

{% highlight bash %}
$ sudo pacman -S apache mariadb php php-apache phpmyadmin apache-openrc mysql-openrc

$ sudo apachectl start

$ sudo rc-update add httpd default
 * service httpd added to runlevel default

$ sudo rc-update add mysql default
 * service mysql added to runlevel default
{% endhighlight %} 

### [browser: test]

> http://localhost/

Basic Apache Configuration
--------------------

### [editor: /etc/httpd/conf/httpd.conf]

{% highlight conf %}
# epsi!
LoadModule mpm_prefork_module modules/mod_mpm_prefork.so
LoadModule php5_module modules/libphp5.so

# comment out
#LoadModule mpm_event_module modules/mod_mpm_event.so

# uncomment
LoadModule rewrite_module modules/mod_rewrite.so

# epsi!
ServerName localhost

<IfModule mime_module>
    # ...    
    
 # epsi !
 AddType application/x-httpd-php .php
 AddType application/x-httpd-php-source .phps
 
</IfModule>

# epsi!
Include conf/extra/php5_module.conf
{% endhighlight %} 

### [editor: /usr/lib/tmpfiles.d/]

{% highlight conf %}
d /run/httpd 0755 http http -
{% endhighlight %}

### [terminal]

{% highlight bash %}
$ sudo rc-service httpd restart
{% endhighlight %}

Localhost Test
--------------------

### [terminal]

{% highlight bash %}
$ sudo sh -c 'echo "<html><body>miauw</body><html>" > /srv/http/hello.html'
$ sudo sh -c 'echo "<html><body>miauw</body><html>" > /srv/http/hello.php'
$ sudo sh -c 'echo "<?php phpinfo(); ?>" > /srv/http/phpinfo.php'
{% endhighlight %}

### [browser: test]

> http://localhost/hello.html
> http://localhost/hello.php
> http://localhost/phpinfo.php

mariadb
--------------------

{% highlight bash %}
$ sudo /usr/bin/mysqld_safe --datadir='/var/lib/mysql'
{% endhighlight %}

### [editor: /etc/mysql/my.cnf]

{% highlight conf %}
# The MariaDB server
port = 3306
socket = /run/mysqld/mysqld.sock

# epsi !
user        = mysql
basedir     = /usr
datadir     = /var/lib/mysql
pid-file    = /run/mysql/mysql.pid
{% endhighlight %}

{% highlight bash %}
$ sudo rc-service mysql restart
 * Checking mysqld configuration for mysql ...                                                                                       [ ok ]
 * Starting mysql ...
 * /run/mysql: creating directory
 * /run/mysql: correcting owner 
 
$ cd /media/Works/Backup.Temp/
$ mysql -u root < sf_book2.sql
$ mysql -u root < joomla30.sql
{% endhighlight %}
 
phpmyadmin
--------------------

### [reading]

> <https://wiki.archlinux.org/index.php/PhpMyAdmin>

### [terminal]

{% highlight bash %}
$ sudo touch ls /etc/httpd/conf/extra/phpmyadmin.conf
{% endhighlight %}

### [editor: /etc/httpd/conf/extra/phpmyadmin.conf]

{% highlight conf %}
Alias /phpmyadmin "/usr/share/webapps/phpMyAdmin"
<Directory "/usr/share/webapps/phpMyAdmin">
    DirectoryIndex index.php
    AllowOverride All
    Options FollowSymlinks
    Require all granted
</Directory>
{% endhighlight %}

### [editor: /etc/httpd/conf/httpd.conf]

{% highlight conf %}
# epsi!
# phpMyAdmin configuration
Include conf/extra/phpmyadmin.conf
{% endhighlight %}

### [editor: /etc/php/php.ini]

{% highlight ini %}
# set
date.timezone = "Asia/Jakarta"

# uncomment
extension=mysqli.so
extension=mcrypt.so

# add directory
open_basedir = /srv/http/:/home/:/tmp/:/usr/share/pear/:/usr/share/webapps/:/etc/webapps/
{% endhighlight %}

### [terminal]

{% highlight conf %}
$ cat /etc/webapps/phpmyadmin/config.inc.php | less

$ sudo rc-service httpd restart
{% endhighlight %}

### [browser: test]

> http://localhost/phpmyadmin

Local Host
--------------------

### [editor: /etc/httpd/conf/httpd.conf]

{% highlight conf %}
<Directory "/srv/http">
    # change
    AllowOverride All
</Directory>
{% endhighlight %}

### [terminal]

{% highlight bash %}
$ cd /srv/http
$ sudo ln -s /media/Works/Development/www/symfony2/book2/ book2
$ sudo ln -s /media/Works/Development/www/drupal/ drupal
$ sudo ln -s /media/Works/Development/www/sites/ sites

$ sudo rc-service httpd restart
{% endhighlight %}

Virtual Host
--------------------

### [terminal]

{% highlight bash %}
$ sudo mkdir /etc/httpd/conf/vhosts
$ sudo touch /etc/httpd/conf/vhosts/localhost.conf
$ sudo touch /etc/httpd/conf/vhosts/book2.conf
{% endhighlight %}

### [editor: /etc/hosts]

{% highlight conf %}
#<ip-address>   <hostname.domain.org>   <hostname>
127.0.0.1       localhost
127.0.1.1       axioo
127.0.0.1       book2
127.0.0.1       localhost.localdomain   localhost
::1             localhost.localdomain   localhost
{% endhighlight %}

### [editor: /etc/httpd/conf/vhosts/localhost.conf]

{% highlight conf %}
<VirtualHost *:80>
    ServerName localhost
    DocumentRoot /srv/http
</VirtualHost>
{% endhighlight %}

### [editor: /etc/httpd/conf/vhosts/book2.conf]

{% highlight conf %}
<VirtualHost *:80>

    ServerName book2
    DocumentRoot /media/Works/Development/www/symfony2/book2/web
    DirectoryIndex app.php
    ErrorLog /var/log/httpd/book2.log
    CustomLog /var/log/httpd/book2.log common

    <Directory "/media/Works/Development/www/symfony2/book2/web">
        AllowOverride All
     Require all granted
    </Directory>
    
</VirtualHost>
{% endhighlight %}

### [editor: /etc/httpd/conf/httpd.conf]

{% highlight conf %}
# epsi!
# Enabled Vhosts:
Include conf/vhosts/localhost.conf
Include conf/vhosts/book2.conf
{% endhighlight %}

### [editor: /etc/php/php.ini]

{% highlight ini %}
# uncomment
extension=mysql.so
extension=pdo_mysql.so

# add folder
open_basedir = /srv/http/:/home/:/tmp/:/usr/share/pear/:/usr/share/webapps/:/etc/webapps/:/media/Works/Development/www/
{% endhighlight %}

### [terminal]

{% highlight bash %}
$ sudo rc-service httpd restart
$ httpd -S
{% endhighlight %}

![LAMP Open-RC]({{ site.url }}/assets/posts/opensource/2015/10/lamp-manjaro-openrc-browser.png)

Finalization
--------------------

Sleep

Have a nice dream.

