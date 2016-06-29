---
title: mount, точка монтирования и device is busy
published: 2009-02-24T20:18:00Z
categories: 
tags: linux,tips'n'tricks
---

Недавно столкнулся с забавной, но поучительной ситуацией. Как писали древние геометры под своими чертежами-решениями задач, "Смотри!"<a name='more'></a>:<br /><br /><div class="code"><code>cd /mnt<br />sudo mount -t vfat /dev/sda1 /mnt<br />cd /mnt<br />.........<br />cd ..<br />umount /mnt</code></div><br />Оп-ля! Device is busy, говорит система, и она права.<br /><br />Прикол заключался в том, что я во время действий, обозначенных троеточиями, совершенно забыл о том, что su я выполнял, находясь в /mnt — т.е. пусть под рутом я уже и не в /mnt, но мой предыдущий пользователь всё ещё там, а значит, девайс нельзя отмонтировать.<br /><br />Дальше всё просто:<br /><br /><div class="code"><code>exit<br />cd ..<br />sudo umount /mnt</code></div><br />Вот так вот :)

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>


