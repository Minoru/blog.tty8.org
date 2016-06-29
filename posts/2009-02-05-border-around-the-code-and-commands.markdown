---
title: Рамочка вокруг кода и команд
published: 2009-02-05T17:35:00Z
categories: 
tags: tips'n'tricks,blog
---

Случайно заглянул на свой ЖЖ, который когда-то пытался вести. Обнаружил там довольно интересный html код, который уже притащил на этот блог.<a name='more'></a> Итак, для того, чтобы сделать что-то типа такого:<br /><div class="code"><code>aptitude search flash | grep ^i</code></div>нужно писать так:<div class="code"><code>&lt;div class="code"&gt;&lt;code&gt;текст, который обрамляется&lt;/code&gt;&lt;/div&gt;</code></div>Естественно, стиль code для тега <code>div</code> надо сначала создать. Для этого открываем редактор кода и добавляем между тегами &lt;b:skin&gt;&lt;/b:skin&gt; (следите, чтобы ваш код не попал в комментарии — они начинаются с «/*» и заканчиваются «*/») следующее:<div class="code"><code>div.code {<br />&nbsp;&nbsp;background: #eeeeee;<br />&nbsp;&nbsp;border: 1px dashed #999999;<br />&nbsp;&nbsp;padding: 5px;<br />&nbsp;&nbsp;overflow: auto;<br />&nbsp;&nbsp;font-family: monospace;<br />}</code></div>Учтите, что тег &lt;div&gt; автоматически вставляет после себя перевод строки (неявный: не ожидайте увидеть настоящий перевод строки в HTML-редакторе Blogger или &lt;br /&gt;, если вы смотрите на HTML через просмотрщик исходного кода страницы).<br />Да, и ещё одно: в предпросмотре Blogger рамочка и фон не отображаются :(<br />Удачи!<br /><br />При подготовке заметки использовались наработки других блоггеров:<br /><ol><li>Статья <a href="http://konishchevdmitry.blogspot.com/2007/09/blogger.html">«Несколько примочек для Blogger»</a> Дмитрия Конищева</li><li>Статья <a href="http://mydebianblog.blogspot.com/2008/07/blogger.html">«Как запостить программный код на Blogger?»</a> Михаила Конника</li></ol><br /><b>UPD</b>: сегодня (11.12.2011) заметил, что во всех тегах <code>pre</code> содержимое «вытянулось» в одну строку. Ранее в этом посте описывалось форматирование кода именно с помощью <code>pre</code>, но теперь я заменил рецепт на тот, что вы видите сейчас. <br /><br />Кроме того, в новой (очередной) версии редактора рамочки и фон стали отображаться, но также увеличился междустрочный интвервал. Не обращайте внимания — после публикации всё будет окей.<br />

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-02-05T21:34:00.000+02:00, Dr.AKULAvich wrote:</p>
<p class='hakyll-convert-comment-body'>
Элегантно смотрится. Возьму на карандаш :-)<BR/>Спасибо
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-02-07T14:04:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Пожалуйста ;)<BR/>Спасибо за внимание к блогу.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T23:27:00.000+03:00, duke wrote:</p>
<p class='hakyll-convert-comment-body'>
"Учтите, что тег pre автоматически вставляет после себя перевод строки"<BR/>Важное замечание, как-то не думал об этом, буду знать :)
</p>
</div>



