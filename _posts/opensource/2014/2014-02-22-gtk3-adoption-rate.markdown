---
layout: post
title:  "Gtk3 Adoption Rate in Debian."
date:   2014-02-22 21:29:15 +0700
categories: opensource
tags: [debian, package manager]
author: epsi
excerpt:

---

On January 2012, I wrote myself a note.
A complain about how slow python3 migration is.
And also about gtk3 adoption rate in Debian.

Why Debian? Because it is the most conservative one. 
Arch already had Python3 only distro that time. 
But Mageia currently still have a very similar issue.

-- - --

Progress for package migration

* python-gtk2    : python3-gi,

* python-kde     : pykde??, python3-sip, python3-pyqt4

* python-dbus    : python3-dbus already in ubuntu precise

* python-cairo   : python3-cairo already in ubuntu oneiric

* python-uno     : python3-uno still in debian experimental

* python-xapian  : python3-xapian que in debian wishlist

* libgtk-perl    : libgtk3-perl que in debian wishlist

Note: In Debian, Python2 space is simply called Python, without 2 suffix, while Python3 space is Python3. 

* thought: need some time to switch from python-gtk2 to python3-gi. should rewrite script.

* thought: Some popular application had switched from libgtk2.0 to libgtk-3.

* thought: Python based app server/ web framework will be ported slower. Python 2 will still around longer.

-- -- --

Now, it's 22 february 2014.<br/>
Two years has past.

* python-gtk2    : Hard dependency for Gnome package

* python-kde     : Not a dependency anymore

* python-dbus    : Hard dependency for Gnome-Shell package

* python-cairo   : Hard dependency for GIMP package

* python-uno     : Replaced by python3-uno 

* python-xapian  : Can be removed. But still no python3-xapian

* libgtk-perl    : Package just vanished. Obsolete

* Dolphin still has hard dependency with Python2

* libgtk2.0      : WHY CAN'T YOU JUST DISSAPEAR!!!

note: in Mageia, it is called lib64gtk+-x11-2.0.0, what a crazy naming space.

-- -- --

Issue: 

* Python2 is still a gnome3 dependency problem.

Conclusion:

* You can have Python3 only debian (testing/jessie), but you should give up KDE, gnome-shell, xapian and gimp.

Hope this will be fixed soon within a year in debian (testing/jessie).

~epsi

[//]: <> ( -- -- -- links below -- -- -- )
