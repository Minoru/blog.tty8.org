---
title: konqueror и страницы man/info
published: 2008-12-02T10:56:00Z
categories: 
tags: linux,tips'n'tricks
description: Оказывается, в Konqueror можно читать маны и страницы info.
---

В процессе чтения <a href=http://tldp.org/LDP/intro-linux/ target="_blank">Introduction to Linux</a> был выяснен интересный факт: Konqueror, стандартный браузер KDE, умеет отображать man- и info-страницы! Для этого достаточно вбить в адресную строку такое:
```
man:sed
```
или
```
#sed
```
или
```
info:sed
```
или
```
man:info(1)
```
(в скобках указана секция; если секция не указана и найдено несколько манов, отображается их список), а также
```
man:/home/cp.1.gz
```
(указание абсолютного пути до файла с маном).

Всё отображается в очень удобоваримом виде — ссылки в секции SEE ALSO являются гиперссылками, если мана нет &mdash; так и говорят, если манов несколько &mdash; показывают список на выбор. Короче, очень удобная штука, чтобы приучить новенького пользователя к чтению документации (большинство ведь поначалу боятся терминала больше, чем огня). Жаль только, что умеет это только Konqueror, а значит, что доступная данная фича только пользователям KDE.

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2008-12-04T21:33:00.000+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Вместо man можно набрать #
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2008-12-05T17:58:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Действительно, можно. Пост поправил, комментатору спасибо.
</p>
</div>



