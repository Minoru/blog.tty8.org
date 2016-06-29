---
title: Русские маны и UTF-8: моё решение
published: 2008-08-09T12:31:00Z
categories: 
tags: linux
---

После последнего реинсталла системы возникла проблема с манами: они отображаются коряво. О том, что же с ними случилось, и что я сделал, и пойдёт речь в этом посте.<br /><a name='more'></a><br /><i><b>Описание проблемы:</b></i> некорректное отображение русских манов, если установлена локаль UTF-8.<br />К примеру, вот так выглядел вывод <code>man chown</code>:<br /><br /><a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://2.bp.blogspot.com/_Nb6QYFUvBjY/SJ2Qw9L3ffI/AAAAAAAAAAw/yrEEn4af1CA/s1600-h/man_chown.png"><img style="margin: 0px auto 10px; display: block; text-align: center; cursor: pointer;" src="http://2.bp.blogspot.com/_Nb6QYFUvBjY/SJ2Qw9L3ffI/AAAAAAAAAAw/yrEEn4af1CA/s400/man_chown.png" alt="уменьшённая копия скриншота вывода команды man chown; кликните для просмотра полного скриншота" id="BLOGGER_PHOTO_ID_5232497512806841842" border="0" /></a><br /><i><b>Решение проблемы</b></i><br /><br />Вообще-то, это не решение - это костыль. Костыли я не люблю до ужаса, но в данном случае ничего иного не придумал. Итак, от рута пишем:<div class="code"><code>apt-get remove --purge manpages-ru</code></div><br />Да-да, господа, именно: я удалил русские маны из своей системы. Печально, но... иного выхода я не вижу. Если кто-то знает Истинное решение - просьба написать комментарий к этой статье. Придётся учить английский (к счастью, этот язык сейчас очень популярен и востребован, так что пригодится даже в случае, если когда-нибудь все мои маны будут на русском).

<h3 id='hakyll-convert-comments-title'>Comments</h3>


