---
title: Пишем man-страницы
published: 2010-01-19T10:54:00Z
categories: 
tags: linux, howto
description: Краткое руководство по синтаксису манов. Оказывается, их можно
    писать руками!
---

Привет!

Недавно заинтересовался тем, как же писать man-страницы — всё-таки, <a href="http://github.com/Minoru/EasyPK">EasyPK</a> уже перерос в небольшую утилиту, которую хорошо бы документировать не только ключами <code>-h</code>, но и чем-то посолиднее. Как оказалось, начать писать маны очень просто — достаточно только просмотреть <a href="http://babbage.cs.qc.edu/courses/cs701/Handouts/man_pages.html">вот этот</a> документик, и вы уже знакомы с основами. Должен предупредить, что в указанной статье дан очень минималистский набор опций, так что вот пара трюков, которые я хотел бы добавить.

**Первым моментом**, который не освещён в статье, являются абзацы с тегами. С помощью такой заумно названной штуки очень удобно организовывать, скажем, списки опций: абзацем будет описание, а тегом — сама опция. Отображается это дело вполне корректно. Список опций строится достаточно просто:
```
.SH OPTIONS
.TP
.B \-h
Display a short help
.TP
.B tar, tbz, tbz2, tgz, bz2, gz, 7z, zip, rar
Resulting archive format
.TP
.B \-d
Delete input files after adding them to archive
.TP
.BI \-o " outfile"
Use
.I
outfile
as output file
```
Теги я выделил полужирным — так красивее.

Вышеприведённый код содержит в себе <span style="font-weight:bold;">второй трюк</span> — последняя опция отобразится вот так:

> <b>-o</b> <u>outfile</u><br/>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Use <u>outfile</u> as output file.

Почему не сделать так:
```
.B \-o
.I outfile
```
? А вот не знаю :) Не работает, и всё. Зато <code>.BI</code> с радостью сделает то, что требуется, так что запомним этот финт и пойдём дальше.

**Третье** — e-mail'ы. Для того, чтобы они выглядели так:

> You can mail author at &lt;<u>author@companyname.com</u>&gt;

достаточно написать такой код:
<pre><code>You can mail author at &lt;\fIauthor@companyname.com\fR&gt;
</code></pre>
Как видите, всё очень просто, не нужно даже делать лишних переносов строк.

Наконец, **последний момент** — это ссылки на другие ман-страницы (секция SEE ALSO). Тут просто даю кусок кода:
```
.SH SEE ALSO
.BR unpk (1),
.BR atool (1),
.BR bzip2 (1),
.BR gzip (1),
.BR pv (1),
.BR rar (1),
.BR tar (1),
.BR 7z (1)
```
Надеюсь, этот пост и труд Christopher Vickery сослужит хорошую службу тем, кто хочет написать man-страничку к своему проекту, но не знает, как.

P.S. Кстати, в Linux правила форматирования описаны в <code>man 7 man</code>.

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-19T14:40:29.740+02:00, muhas wrote:</p>
<p class='hakyll-convert-comment-body'>
просто-то просто, но жудко неудобно.  я вот txt2man заюзал для этих целей - очень рад
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-19T16:12:23.798+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
txt2man, конечно, хорошо (хоть я его и не юзал), но формат man-страниц не так уж страшен — можно и руками писать, пусть это и не сильно удобно.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-19T18:00:44.542+02:00, Сергей wrote:</p>
<p class='hakyll-convert-comment-body'>
А ещё можно маны в разметке markdown писать. <a href="http://johnmacfarlane.net/pandoc/pandoc.1.md" rel="nofollow">Вот так</a>. По-моему гораздо читаемее. Компилируется в groff-разметку командой

<b>pandoc -s -w man pandoc.1.md -o pandoc.1</b>
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-23T14:39:38.329+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
2 Сергей:

<b>А ещё можно маны в разметке markdown писать.</b><br/>
А вот это уже интересно! Пример впечатлил, markdown прост и понятен. Надо будет попробовать на досуге. Спасибо!
</p>
</div>



