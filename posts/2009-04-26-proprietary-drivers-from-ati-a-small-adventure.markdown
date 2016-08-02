---
title: "Проприетарные драйвера от ATI: маленькое приключение"
published: 2009-04-26T19:48:00Z
categories: 
tags: linux,debian
---

Я являюсь (почти) счастливым обладателем видеокарты ATI Radeon X550. Однажды я поставил проприетарные дрова в виде готовых бинарников, то есть без сборки .deb'ов под свою систему (этого можно добиться, просто запустив .run-файл без каких-либо параметров). Через некоторое время захотелось вернуться на открытые дрова, вот только одно «но» — непонятно, как. Если бы я собирал .deb'ы, то я просто удалил бы из системы установленные пакеты. Как же быть с установленными бинарниками, если у того же .run'а нет параметров удаления, я не знал. До сегодняшнего дня.

Но, как оказалось, среди устанавливаемых файлов присутствует сценарий удаления — <code>fglrx-uninstall.sh</code>. У меня он находился вот здесь: <code>/usr/share/ati/fglrx-uninstall.sh</code>. Его запуска вполне хватило для удаления проприетарного драйвера.

<i>P.S. Скрипт также возвращает ваш старый <code>xorg.conf</code>.</i>

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-20T02:14:43.470+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
```
bash ati-driver-installer*.run --listpkg
bash ati-driver-installer*.run --buildpkg Ubuntu/9.04
```
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-20T18:50:15.574+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Уважаемый <B>Аноним</B>, Вы не находите, что Ваш комментарий немного «не в тему»?

P.S. Здесь (имеется в виду не только комментарий, но и блог) «выкание» считается вежливостью, а не грубостью.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-06-12T22:01:10.794+03:00, GQ wrote:</p>
<p class='hakyll-convert-comment-body'>
А всё потому что нельзя в дебиан так ставить драйвера:

http://gq.net.ru/2007/05/05/installing-ati-or-nvidia-drivers-into-debian-etch/
</p>
</div>



