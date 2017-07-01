---
title: Внезапные траблы из-за погони за юзер-френдли
published: 2010-03-01T23:51:00Z
categories: 
tags: linux
description: Автомонтирование флешек подсунуло свинью при создании загрузочной
    флешки RIPLinux.
---

Пытаясь создать загрузочную флешку RIPLinux с помощью стокового mkusb.sh, наткнулся на трабл:
<pre><code><span style="color: green">%</span> sudo ./mkusb.sh -f RIPLinux-9.3-non-X.iso /dev/sdb
*** Creating partition on /dev/sdb...
ERROR: Fdisk had a problem partitioning /dev/sdb
<span style="color: green">%</span></code></pre>
Оказалось, всё дело в автомонтировании — mkusb создавал FAT32 раздел, который тут же подхватывался udev'ом и монтировался. Соответственно, mkusb раздел примонтировать уже не мог и падал с ошибкой.

Такая вот не совсем очевидная проблемка всплыла.

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-03-03T15:36:25.856+02:00, Flycat wrote:</p>
<p class='hakyll-convert-comment-body'>
Дык, автомонтирование -- зло (ИМХО, конечно)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-03-03T19:19:07.594+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Зло, конечно, но вот попробовал — и затянуло… :(

Думаю сейчас над каким-то полуавтоматическим монтированием — типа тыкаешь флешку, а тебя спрашивают — будем маунтить или нет.
</p>
</div>



