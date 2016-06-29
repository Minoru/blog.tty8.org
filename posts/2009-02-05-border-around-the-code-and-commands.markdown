---
title: Рамочка вокруг кода и команд
published: 2009-02-05T17:35:00Z
categories: 
tags: tips'n'tricks,blog
---

Случайно заглянул на свой ЖЖ, который когда-то пытался вести. Обнаружил там довольно интересный html код, который уже притащил на этот блог.<a name='more'></a> Итак, для того, чтобы сделать что-то типа такого:
```
aptitude search flash | grep ^i
```
нужно писать так:
```
<div class="code"><code>текст, который обрамляется</code></div>
```
Естественно, стиль code для тега `div` надо сначала создать. Для этого открываем редактор кода и добавляем между тегами &lt;b:skin&gt;&lt;/b:skin&gt; (следите, чтобы ваш код не попал в комментарии — они начинаются с «/\*» и заканчиваются «*/») следующее:
```
div.code {
  background: #eeeeee;
  border: 1px dashed #999999;
  padding: 5px;
  overflow: auto;
  font-family: monospace;
}
```
Учтите, что тег &lt;div&gt; автоматически вставляет после себя перевод строки (неявный: не ожидайте увидеть настоящий перевод строки в HTML-редакторе Blogger или &lt;br /&gt;, если вы смотрите на HTML через просмотрщик исходного кода страницы).
Да, и ещё одно: в предпросмотре Blogger рамочка и фон не отображаются :(
Удачи!

При подготовке заметки использовались наработки других блоггеров:
<ol><li>Статья <a href="http://konishchevdmitry.blogspot.com/2007/09/blogger.html">«Несколько примочек для Blogger»</a> Дмитрия Конищева</li><li>Статья <a href="http://mydebianblog.blogspot.com/2008/07/blogger.html">«Как запостить программный код на Blogger?»</a> Михаила Конника</li></ol>
<b>UPD</b>: сегодня (11.12.2011) заметил, что во всех тегах <code>pre</code> содержимое «вытянулось» в одну строку. Ранее в этом посте описывалось форматирование кода именно с помощью <code>pre</code>, но теперь я заменил рецепт на тот, что вы видите сейчас. 

Кроме того, в новой (очередной) версии редактора рамочки и фон стали отображаться, но также увеличился междустрочный интвервал. Не обращайте внимания — после публикации всё будет окей.


<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-02-05T21:34:00.000+02:00, Dr.AKULAvich wrote:</p>
<p class='hakyll-convert-comment-body'>
Элегантно смотрится. Возьму на карандаш :-)<br/>
Спасибо
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-02-07T14:04:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Пожалуйста ;)<br/>
Спасибо за внимание к блогу.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T23:27:00.000+03:00, duke wrote:</p>
<p class='hakyll-convert-comment-body'>
"Учтите, что тег pre автоматически вставляет после себя перевод строки"<br/>
Важное замечание, как-то не думал об этом, буду знать :)
</p>
</div>



