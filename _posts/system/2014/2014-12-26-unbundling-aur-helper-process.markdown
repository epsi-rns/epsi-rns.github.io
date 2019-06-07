---
layout: post
title:  "Unbundling AUR Helper Process"
date      : 2014-12-26 18:08:15 +0700
categories: system
tags      : [arch, package manager]
keywords  : [aur, yaourt]
author: epsi

excerpt:
  For those who are really tired of how easy yaourt (or packer) is. 
  You may consider 'cower' to download  PKGBUILD from AUR manually. 
  So you can 'makepkg' manually.

related_link_ids: 
  - 16062127  # Install Yaourt
  - 14040246  # Arch Install
  - 16020803  # Update Arch no Bloated  
  - 14042750  # BlackArch as Repository
  - 14122758  # Selectively BlackArch Tools"  

---

One advantage of Arch based distrbution is access to AUR (Arch User Repository).
You can do automatic compilation of AUR package with yaourt, or yaourt-gui.

For those who are really tired of how easy <code>yaourt</code> (or <code>packer</code>) is. 
You may consider <code>cower</code> to download  PKGBUILD from AUR manually. 
So you can <code>makepkg</code> manually.

[![AUR Cower][image-aur-cower]{: .img-responsive }][photo-aur-cower]

If you are not familiar with yaourt. 
I Edited this article in 2016 and give a screenshot of yaourt..

[![AUR Yaourt][image-aur-yaourt]{: .img-responsive }][photo-aur-yaourt]

Instead of yaourt, there are other cool AUR helper.
e.g. this Haskell based <code>aura</code>.

[![AUR AURA][image-aur-aura]{: .img-responsive }][photo-aur-aura]

Note: I apologize for my english,
and also my boring desktop customization.

[//]: <> ( -- -- -- links below -- -- -- )


[image-aur-cower]: {{ site.url }}/assets/posts/system/2014/12/aur-cower.png
[photo-aur-cower]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipOC0IpdIpo5eZ552oBEXtB8tCdsSxoNhehop2wk
[image-aur-yaourt]: {{ site.url }}/assets/posts/system/2014/12/aur-yaourt.png
[photo-aur-yaourt]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipPVSt_YN4D_Z-utSAu5iTtZGw3lLyTQwFBbW2C1
[image-aur-aura]: {{ site.url }}/assets/posts/system/2014/12/aur-aura.png
[photo-aur-aura]: https://photos.google.com/album/AF1QipNVMF7qcC-QwEs9Hb8xD2ywBu9GrLt09jCFT_4U/photo/AF1QipMsayLX9hEZmpiprQ4U_ZXiEk6OT16IxcDSj3WJ

[related-arch-install]: {{ site.url }}/opensource/2014/04/02/arch-install-log.html
[related-arch-no-bloated]: {{ site.url }}/opensource/2016/02/08/pacman-ignorepkg.html
[related-blackarch-repository]: {{ site.url }}/opensource/2014/04/27/install-blackarch-as-repository.html
[related-blackarch-selectively]: {{ site.url }}/opensource/2014/12/27/selectively-install-blackarch-tools.html
