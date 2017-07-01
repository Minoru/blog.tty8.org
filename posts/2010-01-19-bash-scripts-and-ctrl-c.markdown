---
title: BASH-скрипты и Ctrl+C
published: 2010-01-19T11:20:00Z
categories: 
tags: linux,tips'n'tricks
description: Обрабатываем нажатие Ctrl+C.
---

<a href="http://github.com/Minoru/EasyPK">EasyPK</a> всегда страдал тем, что не мог корректно завершиться при нажатии Ctrl+C — он просто прерывал процесс упаковки/распаковки текущего архива и переходил к следующему. Это раздражало, т.к. приходилось клацать Ctrl+C, пока архивы не закончатся. Я почему-то считал, что решить эту проблему будет стоить немалой крови — и я ошибался. В bash есть <a href="http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_12_02.html">ловушки (traps)</a>, о которых и рассказать-то особо нечего — они просто выполняют команду при поступлении сигнала. Например, я дописал в начало скриптов такую строку:
```
trap "exit 3" KILL HUP INT TERM
```
Это значит, что при получении <code>SIGTERM</code>, <code>SIGKILL</code>, <code>SIGINT</code> или <code>SIGHUP</code> мой скрипт сразу же сделает <code>exit 3</code>, т.е. завершится с кодом выхода 3. Легко, понятно, эффективно.

Приятного кодинга!

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-19T16:09:57.419+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
KILL невозможно перехватить в процессе. man 7 signal.

Также будет полезно раскрыть тему о том, что в комбинации ctrl-c нет ничего магического: просто так по умолчанию настроен терминальный драйвер -- он генерирует SIGINT (man stty, man tefminfo).
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-19T16:11:17.019+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Аутентификации по openid не работает (по крайней мере -- на myopenid.com). Впрочем, это не Ваша вина, скорее всего, а движка.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-23T14:36:42.587+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
2 Анонимный:

<b>KILL невозможно перехватить в процессе. man 7 signal.</b><br/>
Не знал. Не нашёл в упомянутом мане такой информации, может, процитируете? Тем не менее, эксперимент подтверждает Ваши слова.

О stty и прочем писать не хочу, т.к. и сам-то не разбираюсь. Пусть уж лучше народ прочтёт Ваш комментарий и поинтересуется сам.

Что касается OpenID — это довольно известная проблема Blogger&#39;а. Гугловцы почему-то не фиксят… :(
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-24T20:00:14.352+02:00, Kirikaza wrote:</p>
<p class='hakyll-convert-comment-body'>
Насчёт перехвата KILL...

Вообще с сигналами можно делать одну из трёх вещей: перехватить, блокировать и игнорировать. Перехват как раз осуществляется в данном примере, блокировка откладывает сигнал &quot;на будущее&quot;, а игнор забивает на сигнал. Но можно с сигналами вообще ничего не делать, тогда ядро выполнит действие по умолчанию.

Для каждого сигнала жёстко задано, что  с ним можно делать. Для мягкого завершения процесс есть TERM, который можно блокировать, чтобы программа смогла сохранить данные. Для жёсткого завершения -- KILL, который блокировать нельзя и все данные программы теряются. При выключении GNU/Linux можно увидеть сообщения, из которых ясно, что сперва всем процессам рассылаются сигналы TERM, а затем самым упорным из них -- KILL.

По просьбам трудящихся цитата из man 7 signal: &quot;The signals SIGKILL and SIGSTOP cannot be caught, blocked, or ignored.&quot; 

P.S.: Вообще говоря, сигнал KILL в принципе не доходит до процесса -- ядро отбирает у процесса все ресурсы, в том числе и процессорное время, так что процесс уже работать не может и становится зомби.


P.P.S.: Крайне рекомендую книгу &quot;Немет Э., Снайдер Г., Сибасс С., Хейн Т. Р. UNIX: руководство системного администратора&quot;. У самого под рукой всегда <a href="http://www.books.ru/shop/books/21999?partner=436634" rel="nofollow">третье издание</a>. Есть ещё вариант для Linux почти тех же авторов -- <a href="http://www.books.ru/shop/books/499617?partner=436634" rel="nofollow">Руководство администратора Linux</a>
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-01-25T00:27:28.348+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Kirikaza</b>, спасибо за мини-лекцию! Я, признаться, совершенно не интересовался механизмом работы сигналов, а в man 7 signal основное внимание уделил таблице, а не тексту.

За рекомендации спасибо — но я пока не настолько глубоко интересуюсь работой nix, чтобы их читать :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-03-02T04:00:15.353+02:00, NucleoFag wrote:</p>
<p class='hakyll-convert-comment-body'>
Идея решения чем-то напиминает пропускание фильтром через yes для собственно ответа &quot;y&quot; (yes) на какие-либо вопросы)
</p>
</div>



