---
title: Пишем man-страницы
published: 2010-01-19T10:54:00Z
categories: 
tags: linux
---

Привет!<br /><br />Недавно заинтересовался тем, как же писать man-страницы — всё-таки, <a href="http://github.com/Minoru/EasyPK">EasyPK</a> уже перерос в небольшую утилиту, которую хорошо бы документировать не только ключами <code>-h</code>, но и чем-то посолиднее. Как оказалось, начать писать маны очень просто — достаточно только просмотреть <a href="http://babbage.cs.qc.edu/courses/cs701/Handouts/man_pages.html">вот этот</a> документик, и вы уже знакомы с основами. Должен предупредить, что в указанной статье дан очень минималистский набор опций, так что вот пара трюков, которые я хотел бы добавить.<br /><a name='more'></a><br /><span style="font-weight:bold;">Первым моментом</span>, который не освещён в статье, являются абзацы с тегами. С помощью такой заумно названной штуки очень удобно организовывать, скажем, списки опций: абзацем будет описание, а тегом — сама опция. Отображается это дело вполне корректно. Список опций строится достаточно просто:<br /><div class="code"><code>.SH OPTIONS<br />.TP<br />.B \-h<br />Display a short help<br />.TP<br />.B tar, tbz, tbz2, tgz, bz2, gz, 7z, zip, rar<br />Resulting archive format<br />.TP<br />.B \-d<br />Delete input files after adding them to archive<br />.TP<br />.BI \-o " outfile"<br />Use<br />.I<br />outfile<br />as output file</code></div>Теги я выделил полужирным — так красивее.<br /><br />Вышеприведённый код содержит в себе <span style="font-weight:bold;">второй трюк</span> — последняя опция отобразится вот так:<br /><br /><b>-o</b> <u>outfile</u><br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Use <u>outfile</u> as output file.<br /><br />Почему не сделать так:<div class="code"><code>.B \-o<br />.I outfile</code></div>? А вот не знаю :) Не работает, и всё. Зато <code>.BI</code> с радостью сделает то, что требуется, так что запомним этот финт и пойдём дальше.<br /><br /><span style="font-weight:bold;">Третье</span> — e-mail'ы. Для того, чтобы они выглядели так:<br /><br />You can mail author at &lt;<u>author@companyname.com</u>&gt;<br /><br />достаточно написать такой код:<br /><div class="code"><code>You can mail author at &lt;\fIauthor@companyname.com\fR&gt;</code></div>Как видите, всё очень просто, не нужно даже делать лишних переносов строк.<br /><br />Наконец, <span style="font-weight:bold;">последний момент</span> — это ссылки на другие ман-страницы (секция SEE ALSO). Тут просто даю кусок кода:<br /><div class="code"><code>.SH SEE ALSO<br />.BR unpk (1),<br />.BR atool (1),<br />.BR bzip2 (1),<br />.BR gzip (1),<br />.BR pv (1),<br />.BR rar (1),<br />.BR tar (1),<br />.BR 7z (1)</code></div>Надеюсь, этот пост и труд Christopher Vickery сослужит хорошую службу тем, кто хочет написать man-страничку к своему проекту, но не знает, как.<br /><br />P.S. Кстати, в Linux правила форматирования описаны в <code>man 7 man</code>.

<h3 id='hakyll-convert-comments-title'>Comments</h3>
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
А ещё можно маны в разметке markdown писать. <a href="http://johnmacfarlane.net/pandoc/pandoc.1.md" rel="nofollow">Вот так</a>. По-моему гораздо читаемее. Компилируется в groff-разметку командой<br /><br /><b>pandoc -s -w man pandoc.1.md -o pandoc.1</b>
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-23T14:39:38.329+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
2 Сергей:<br /><br /><b>А ещё можно маны в разметке markdown писать.</b><br />А вот это уже интересно! Пример впечатлил, markdown прост и понятен. Надо будет попробовать на досуге. Спасибо!
</p>
</div>



