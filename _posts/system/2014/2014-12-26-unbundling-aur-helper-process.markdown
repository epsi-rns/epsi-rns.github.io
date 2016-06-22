---
layout: post
title:  "Unbundling AUR Helper Process"
date:   2014-12-26 18:08:15 +0700
categories: system
tags: [arch, aur, package manager]
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

For those who are really tired of how easy yaourt (or packer) is. 
You may consider "cower" to download  PKGBUILD from AUR manually. 
So you can "makepkg" manually.

[![AUR Cower][image-aur-cower]{: .img-responsive }][picasa-aur-cower]

If you are not familiar with yaourt. 
I Edited this article in 2016 and give a screenshot of yaourt..

[![AUR Yaourt][image-aur-yaourt]{: .img-responsive }][picasa-aur-yaourt]

Instead of yaourt, therea are other cool AUR helper.
e.g. thi Haskell based AURA.

[![AUR AURA][image-aur-aura]{: .img-responsive }][picasa-aur-aura]

Note: I apologize for my english,
and also my boring desktop customization.

[//]: <> ( -- -- -- links below -- -- -- )


[image-aur-cower]: {{ site.url }}/assets/posts/system/2014/12/aur-cower.png
[picasa-aur-cower]: https://lh3.googleusercontent.com/-hUfSxrPxvaA/Vz2mKT5QdbI/AAAAAAAAAOs/vP_na7AbrFEzeWdhlkSXJ3VUd0Op8snSQCCo/s0/aur-cower.png
[image-aur-yaourt]: {{ site.url }}/assets/posts/system/2014/12/aur-yaourt.png
[picasa-aur-yaourt]: https://lh3.googleusercontent.com/-E4oPZQK-Ii4/Vz2mLgOJ9kI/AAAAAAAAAOs/QPxlecb-au0fifWMPnFsa3SUs5J3OcK3QCCo/s0/aur-yaourt.png
[image-aur-aura]: {{ site.url }}/assets/posts/system/2014/12/aur-aura.png
[picasa-aur-aura]: https://lh3.googleusercontent.com/-sdrokwSygGE/Vz2mLAZG4PI/AAAAAAAAAOs/CgkIuwZQrQUACgvbvUhh1Q975KU2csv5gCCo/s0/aur-aura.png

[related-arch-install]: {{ site.url }}/opensource/2014/04/02/arch-install-log.html
[related-arch-no-bloated]: {{ site.url }}/opensource/2016/02/08/pacman-ignorepkg.html
[related-blackarch-repository]: {{ site.url }}/opensource/2014/04/27/install-blackarch-as-repository.html
[related-blackarch-selectively]: {{ site.url }}/opensource/2014/12/27/selectively-install-blackarch-tools.html
