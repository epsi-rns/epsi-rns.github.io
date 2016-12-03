---
layout: post-sidemenu-wm
title:  "Install Termite"
categories: desktop
date:   2016-09-19 20:15:15 +0700
tags: [ricing]
author: epsi

excerpt:
  Installing Termite in Arch based distribution is easy,
  the thing get tough when installing vte3-ng in Debian based.
  And installing termite require vte-ng as dependency.

---

Termite is my latest favorite terminal, it is simple,
it supports themable text colors in configuration. 
It is lightweight, looks good for ricing.
It is also very suitable in combination with tiling window manager.
It has mono Dependency, but that's okay for me.

Installing Termite in Arch based distribution
is easy since it is in community repository.

Installing Termite in Debian distribution require
manual compilation from git since it is not in official repository.
Manual compilation is not a hard thing to do.

But thing get tough when installing vte3-ng in Debian.
And installing termite require vte-ng as dependency.
There is no guidance about vte-ng compilation dependency.

-- -- --

### Sample Configuration

You can check my dotfiles here. 

* [Termite Configuration][source-termite]

I don't really write my own config and themes.
Actually I copied them form many sources. 

-- -- --

### Arch Install

Very simple. Termite is in community package. 
No need to touch any AUR. This will also install vte3-ng.

{% highlight bash %}
$ sudo pacman -S termite
{% endhighlight %}

This show bright theme

[![Termite Install Arch Bright][image-arch-bright]{: .img-responsive }][picasa-arch-bright]

-- -- --

### Manjaro Install

Similar with Arch. 

{% highlight bash %}
$ sudo pacman -S termite
{% endhighlight %}

This show dark theme

[![Termite Install Manjaro Dark][image-manjaro-dark]{: .img-responsive }][picasa-manjaro-dark]

-- -- --

### Install VTE-NG in Debian

In order to install termite in Debian, you need install vte-ng from git.

Since we desire to install manually from git,
we shall prepare the directory.

{% highlight bash %}
$ mkdir ~/git-src
$ cd ~/git-src
{% endhighlight %}


Before you compile vte-ng you should install required dependency.

{% highlight bash %}
$ sudo apt install gtk-doc-tools valac libgirepository1.0-dev /
  libgtk-3-dev libgnutls28-dev libxml2-utils gperf
{% endhighlight %}

-- -- --
  
After this you can safely run

{% highlight bash %}
$ git clone https://github.com/thestinger/vte-ng.git
{% endhighlight %}

![VTE-NG git clone][image-debian-vte-ng-git-clone]{: .img-responsive }

{% highlight bash %}
$ cd vte-ng
{% endhighlight %}

{% highlight bash %}
$ ./autogen.sh 
{% endhighlight %}

[![VTE-NG autogen][image-debian-vte-ng-autogen]{: .img-responsive }][picasa-debian-vte-ng-autogen]

{% highlight bash %}
$ make 
{% endhighlight %}

[![VTE-NG make][image-debian-vte-ng-make]{: .img-responsive }][picasa-debian-vte-ng-make]

{% highlight bash %}
$ sudo make install 
{% endhighlight %}

[![VTE-NG make install][image-debian-vte-ng-make-install]{: .img-responsive }][picasa-debian-vte-ng-make-install]

-- -- --

### VTE-NG Dependency

Here is the detail of Dependency requirement

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
./autogen.sh: 11: ./autogen.sh: gtkdocize: not found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install gtk-doc-tools
{% endhighlight %}


-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
src/Makefile.am:205: error: ENABLE_VAPIGEN does not appear in AM_CONDITIONAL
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install valac
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
src/Makefile.am:178: error: HAVE_INTROSPECTION does not appear in AM_CONDITIONAL
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libgirepository1.0-dev
{% endhighlight %}

This will also install g++

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
No package 'gtk+-3.0' found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libgtk-3-dev
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
No package 'gnutls' found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libgnutls28-dev
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ ./autogen.sh
configure: error: xmllint not found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install libxml2-utils
{% endhighlight %}

-- -- --
Error Message:
{% highlight bash %}
$ make
../missing: 81: ../missing: gperf: not found
{% endhighlight %}

Solution:
{% highlight bash %}
$ sudo apt install gperf
{% endhighlight %}

-- -- --

### Install Termite in Debian

Let's get back to our git directory

{% highlight bash %}
$ cd ~/git-src
{% endhighlight %}

{% highlight bash %}
$ git clone --recursive https://github.com/thestinger/termite.git
{% endhighlight %}

![Termite git clone][image-debian-termite-git-clone]{: .img-responsive }

{% highlight bash %}
$ cd termite 
{% endhighlight %}

{% highlight bash %}
$ make 
{% endhighlight %}

[![Termite make][image-debian-termite-make]{: .img-responsive }][picasa-debian-termite-make]

{% highlight bash %}
$ sudo make install
{% endhighlight %}

![Termite make][image-debian-termite-make-install]{: .img-responsive }

No need to install either libglib3.0-cil-dev or gnutls-bin

Let's test

{% highlight bash %}
$ termite -v
termite v11-33-g7a7021f
{% endhighlight %}

![Termite Version][image-debian-termite-version]{: .img-responsive }

-- -- --

I think that's all.


[//]: <> ( -- -- -- links below -- -- -- )

[source-termite]: https://github.com/epsi-rns/dotfiles/tree/master/config/termite

[image-arch-bright]: {{ site.url }}/assets/posts/desktop/2016/09/arch-termite-bright.png
[picasa-arch-bright]: https://lh3.googleusercontent.com/S1X5_dK7syVx84dtFVPerRmoD0zCZ6jvWBgOFI8JSzbn7uCg684fVmy3Vpo4dMlFcK3Wa4NtvO8Yq9I8ymAagfvLTCeqOo-GMqC-oeg34x4vVTOJxwLUjfSOnzVzcWNBo7rZ4QjD7CbTmYYVzptg8XK1PRvx5g_ocGAnIA-ozUPu2YigsbNqBp3ZhW1wzSe0k6dN3nQL-SWG7eRYS_JbroYkZscAquPBBe49LFv9i0kjW1hKwQ-cgL6vsz_uK_PFYGD95ZlAAZO6hAbgfiUPEBO49qE8kKfK2sPpgzSSso_XUHx90YY2s8CMr_zDG-x-9FhBMAiQOXUtGicLOsPpiRIvUDvOAxnZY0BkOkGZN6hQ8e-je942Um_bHLY2U6gbdqzBmB4Fgg5MFZQl5b9FV_QbsqPntyQJGklLPPIp9M47WngA4va2e4Vifa7GnZHSESzsSutglFLqC1D-pzAygHHV3sKXMo6DEWOzUo_HOKkiZxX6FMwtEX7w4M5aO2-ixeSxqAKiOXOUzGJ1MgPMilkibl22hVML5p0DMz0VQwE1OSkandQd3Rz-whjQcBEs3Z4ud60FQ5J4TsYLgm0TiYFToLPYx6tortt_QQKnPEfLNTrt=w645-h408-no

[image-manjaro-dark]: {{ site.url }}/assets/posts/desktop/2016/09/manjaro-termite-dark.png
[picasa-manjaro-dark]: https://lh3.googleusercontent.com/_Nec7zcLTpeZLarnfRTjGn07VkQ_1li7-Spc8mqsvAfLgS00WvUTKiZ34E8tevQ8cw_EggnLYW8rPruQ5Un5UCjE81WkWyrQWJvPc7-iTNq5dNOOkp21NzMcJiETqdIQyioc5Yfr-98eQ3n7XDch-tbrDx4gojhzCNP7eYKu2fFUXoRV_Gel8PNt8bFXERu4fiSNo25HMMzpaXMtdKaVmimD96570svAmshxkxz_blm18sV16y2e4rswphA5xw5ta6b1iU14E6_g_-2_b4zWybygNthWFKgIhHcNOzsz-i76u7ODancIkQJh_doU8M3fRS71nUFk6OVIPWRaZMs3J28HdvmK-gmsdMDZtoaDinS2KsIMpNbSbNX-Ch1Ikcnvf27wXWe1ASF5G14XSUKXQRy-1uS9Ulue0JcOWPz6RoMcV89cAIuq_B-RZnwOn97h1ESQXpV0WYtc6pbA51RoHcZTmE2nuWHtpHD3W8bsLs2XQ5AKTLiByJWGNX8SnFN-vYiAgzK_E4I0jStgeGvTQb8dOaBndkAuuk_2fRthe5H7z_LBitReueJzLagMgEVUB0WvNG3jA2QEg74ysl7rj1vkrHcWcqKtu3MYGA-tgmsqdOhs=w645-h407-no

[image-debian-vte-ng-git-clone]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-git-clone.png

[image-debian-vte-ng-autogen]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-autogen-half.png
[picasa-debian-vte-ng-autogen]: https://lh3.googleusercontent.com/oC9_BxB2AWP_VlGJfOEPdPivs_uZeO1D0BNUGaix6HBpSFqTrIJ75sxO7uxjZ8rE0ZrKOfvx103M6iWVGvU7epscgBZ1e_CVNAhKpefa1_fqZIy5VeuAO5otgq1SGyfTkPjtWfesu9gas0daDaAP3hABE43R_uvJ47TsW7u3jP3OPulDeB2FzcHXSnXeacI9_4d5PMu6iRnMYGwgrC0UEDyes92xSY8s_CP-9c_7D2mhpaTsPhIWExwf0ogaIPmjwNlfdmbAQn9UNDEh78IOWWcE-62vyrsitalOO0VlWtGmlwyjWpe7-REKAy756GqdhEd3_5ZE_PW6PA4JmmnUl9rahPIaSCg7c-Ho1r2FhMbKMI-Dud-EVk74V-NOSKr1OiUhhl7m554lwzxoPS4n66giCm6JWhJ_xEqFzNKuwvNHoaZwb6bU3PFOqSH_iWspeSs7LSUwfXfwna28qC109cIKbrh-H72UnO6FtTI3normKmaNeB80jJgSDmueN1XxvwV3Yr1i9GV6hP8tYRxwLXHgo4K91-L6phJXX9OpgaDgjW6G_wp9MNt7D45CjCiX1LZhUBo_V2amibkLc_y4tj9OSQNMx2YIBo_DhVmwGLqHA9j2=w540-h469-no

[image-debian-vte-ng-make]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-make-half.png
[picasa-debian-vte-ng-make]: https://lh3.googleusercontent.com/G9da2wQ8EIY9u_tLibfypvusSllYu1my3YzvGJ03E1ArVWUXVeK7QP2A4X_ANTQxrNPl5cxhad0qakpy30TGwNQdl8wryYwi1X31zPI_T4YRdlAdRwh0TsRFlEutcS5VjsSmK8PvETdrNWu3I0xv0r9Y7WqfKNtIPsJUoRvQaXvFvUu7GMVaCzfKjG8uJFQJjttPrAwNTgdqGRuRIX7F7nZVuGNGVNW5dCakzKBwYirJQBhez7HD9Q7Iqi-PYwdmIBh1j4bZ4ZBAh1VV1chJuVh1UsyM8yMaXLK7xxWbgSPLCX4bTsTNj3Vsy4doCVNEuS3QwawhCH1En4RO5tUf32Sq0fZsebyKMpGDDQCkwa3S_erP_2mOpiMacLIb3YQRQQXnRq2PrHdZeI2EtYY1IsPlmHGMvCPNyz5DxP0IAPKn_ESPaaeCXxffaArOL9_TmI8b0emHWo8aXY1DRCDnsD2mgiJhyejcr-Q1sd8lpzoximil-EfOuaujTXAWVCtyWzkinWLMSUpOO2FPneAIkof6sGKzb5Aq34Sza8xbp1cGye1DbSjkVsDc7Mf2CK6aCDaipgIsfQZhU0yD9f05-u14ZtLnlmn_NXjhJquqLgxS4JTb=w543-h517-no

[image-debian-vte-ng-make-install]: {{ site.url }}/assets/posts/desktop/2016/09/debian-vte-ng-make-install-half.png
[picasa-debian-vte-ng-make-install]: https://lh3.googleusercontent.com/wp5gIoycY7immNLtBwQHBcyQ2izfpUmpsEFiW4CzXSBuD6koil2aiNcrvGgXNd3xVL8CNNAmAcDl0QYfMW5Sh92EKoyAdvTOSTeAuSRZySLlyfuT1EbwMwkbUwqz0SNIG5Mx8IUVdYaSw9PQpRcXVOiZvVlI71YPMDTI8lqsXoBffAVkRurmFrTNTLvY-UWScdS6UPtrnET3UCH8uZZ-FSe6e-iBGEAN5bsSEAL1fkPKKXmh6-em4zJAPlfOLpN6oDPgd-PoJVqr8mjI6HrRa7fH6s8cmLU7hIoFA0HgwK89y1b0T-tul1_CuP3gPXz-4VCwm6gRdvbBgOejoG4VFK6bnZkGahHqBSnrgbhJYSZ1hiTrWPpWHltzxns3oqdqmxH4e-Zhx66sG6vsw5b96rOI1ZLg0me7DC2Mk1lRt7zPnmjzUsd5XplqQQeDaoWDB36KvXjTSZ8Qt-xEYPhX1aiYqftS7SUh8GPxi-OSv3F4jseYBLVlo7lbBFEG94yYOHLkXQWX7WbpTUfkDH93d3VGxQE9-DutGvmQU7dPPGdV8-i_3xJco1G5BlX_WsamfwkiayYmSoBM6jL8EsW9QQvSIdioTJJubOOVjfvCH6HDrhhi=w540-h514-no

[image-debian-termite-git-clone]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-git-clone.png

[image-debian-termite-make]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-make-half.png
[picasa-debian-termite-make]: https://lh3.googleusercontent.com/d20EyYra9wxo_d6uKhsFZkv_RNSKQodO2uXxKsn5Vxlps8k69hE8ugbr1dMcNUyZeqIzrZpN7bwrvsWsAtXi1zh3Z-kpjwOgR-whagviRnBxYR2saUrUtuPsMpEnNuR29uiFWWL22u64fhpHViJIEzqqQNPIgOubfhBU_OeJ0GAkED2fGv4GSTOkMjruirUfoR12rJJtAb86zLjEOMiVseD8MwGFI26hPEZRiHPO5tN2nH0xCRRRahQBGMrUTt3QindKzwAQOr5wcCeCGGfYQ2_a_T08hJr_UwKVQFOzzIEYxxzuDVl6CExeLzgGBWX46NaV6wx6-0Huyl5YeuGqUI8VFcBFl5Q5a0UchAX_es4wFD9_VlDao5Dn8Vz5-CRS_FSEakJwcPqJuci-HW3xX_QX7XwJAhPaeWFtD5EvtP2KI6CBVH2zpBL-CWpK_MHMJChuB-_xvxXHykgq9G6C-7zHmAWuljVZntHuJ2ICpIqPBYxJ4VDoLEnWhS0SlPZnkKUr018En_HFEq8cDO72TOKaqb3AhwWItSOjxsKrqp2b1r2oWpj0FfexjZAuBW3VtQ_SahrzpN0lWolRnBTU3KNc9P_QL2N_bKLk9OijTKJed3dn=w537-h402-no

[image-debian-termite-make-install]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-make-install.png

[image-debian-termite-version]: {{ site.url }}/assets/posts/desktop/2016/09/debian-termite-version.png
