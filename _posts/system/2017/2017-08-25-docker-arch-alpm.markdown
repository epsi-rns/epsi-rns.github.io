---
layout: post
title: "Docker - Arch ALPM - Part Two"
date: 2017-08-25 13:15:35 +0700
categories: system
tags: [docker, distro, package manager, debian]
author: epsi

excerpt:
  Examine APT step by step,
  using Debian container in Docker.
  One of Three Parts Article.

related_link_ids: 
  - 17081045  # Docker Flow Distribution
# - 17082215  # Debian Portage
  - 17082015  # Slackware Package
  - 17081845  # Fedora DNF
  - 17081515  # openSUSE Zypper
  - 17081545  # Crux Ports
  - 17081415  # LFS Build
  - 17081345  # Void XBPS
  - 17081145  # Gentoo Portage

---

-- -- --

### Not Finished Yet

TO DO

Thank you for reading


[//]: <> ( -- -- -- links below -- -- -- )

{% assign asset_path = site.url | append: '/assets/posts/system/2017/08' %}
{% assign asset_post = site.url | append: '/assets/posts/system/2017/08/docker-arch' %}
{% assign asset_pull = site.url | append: '/assets/posts/system/2017/08/docker-pull' %}

[local-part-two]:   {{ site.url }}/system/2017/08/24/docker-arch-alpm.html

[image-ss-pull-arch]:		{{ asset_pull }}/arch.png
[image-ss-running-arch]:	{{ asset_post }}/00-running-image.png
[image-ss-docker-ps]:       {{ asset_post }}/00-docker-ps.png
[image-ss-getting-started]: {{ asset_post }}/00-getting-started.png

[image-ss-pm-refresh]:		{{ asset_post }}/01-refresh.png
[image-ss-pm-upgradable]:	{{ asset_post }}/01-upgradable.png
[image-ss-pm-download]:		{{ asset_post }}/01-download.png
[image-ss-pm-noconfirm]:	{{ asset_post }}/01-noconfirm.png
[image-ss-pm-sysupgrade]:	{{ asset_post }}/01-sysupgrade.png
[image-ss-pacman-syu]:		{{ asset_post }}/01-pacman-syu.png

[image-ss-pm-install]:		{{ asset_post }}/13-install.png
[image-ss-pm-sync-target]:	{{ asset_post }}/13-syu-target.png
[image-ss-pm-rm-cascade]:	{{ asset_post }}/13-remove-cascade.png
[image-ss-pm-rm-recursive]:	{{ asset_post }}/13-remove-recursive.png
[image-ss-pm-query-search]:	{{ asset_post }}/13-query-search.png
[image-ss-pm-sync-search]:	{{ asset_post }}/13-sync-search.png
[image-ss-pm-sync-info]:	{{ asset_post }}/13-sync-info.png
