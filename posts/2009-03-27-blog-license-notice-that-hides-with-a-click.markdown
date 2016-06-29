---
title: Лицензия на материалы блога со скрытием по клику
published: 2009-03-27T21:30:00Z
categories: 
tags: blog,web
---

Недавно посетил блог <a href="http://www.blogger.com/profile/00833871247004051501">Andrey</a>'я, в последнее время активно отписывающегося в комментариях — <a href="http://www.debianwatch.com/">DebianWatch.com</a>. Помимо статьи <a href="http://www.debianwatch.com/2009/03/debian.html">«Debian: краткая история большого проекта»</a> нашёл там несколько интересных дизайнерских решений.<br /><a name='more'></a><br />Примерно неделю тому назад я вставил в шаблон лицензию на материалы блога (вдохновил меня на это аналогичный блок в блоге <a href="http://www.blogger.com/profile/12420257446841864325">virens</a>'а <a href="http://www.mydebianblog.blogspot.com">«Записки дебианщика»</a>). Но смотрелось это как-то не очень.<br /><br />И вот на <a href="http://www.debianwatch.com/">DebianWatch.com</a> я вижу как раз то, что мне нужно: лицензия показана в колонке справа, а внизу висит только иконка Creative Commons. Прекрасно! 10 минут возни с шаблоном — и у меня уже есть такая же штуковина :)<br /><br />Но всё неймётся: правая колонка теперь разрослась до невиданной высоты, что начинает нервировать, если пост маленький. В жертву приносится дерево постов, но колонка все равно слишком высокая. Тут на глаза попадается учебник по HTML/JavaScript и начинается почти часовое приключение :)<br /><br />Нужно отметить, что web-дизайнер из меня никудышный, и JavaScript я не знаю.<br /><br />Тем не менее, в течении часа — не без помощи добрых людей, конечно (см. пост скриптум) — моя задумка была реализована: по умолчанию отображается надпись «Показать лицензию», если по ней кликнуть, лицензия отображается, а внизу показывается кликабельная надпись «Скрыть лицензию». Делается это так:<br /><ol><li>Добавляем в правую колонку (или куда вы хотите пихнуть лицензию) гаджет «HTML/JavaScript»;</li><li>в заголовке пишем что-то вроде «Лицензия на материалы этого блога»;</li><li>в тело гаджета суём такой код:<pre class="code">&lt;div id="license-header" display="block" onclick="document.getElementById('license-body').style.display='block';<br />document.getElementById('license-header').style.display='none'"&gt;<br />&lt;h5&gt;Показать лицензию&lt;h5&gt;<br />&lt;/h5&gt;&lt;/h5&gt;&lt;/div&gt;<br /><br />&lt;div id="license-body" style="display:none"&gt;<br />Лицензия будет здесь<br />&lt;h5 onclick="document.getElementById('license-body').style.display='none';<br />document.getElementById('license-header').style.display='block'"&gt;Скрыть лицензию&lt;/h5&gt;<br />&lt;/div&gt;</pre></li><li>надпись «Лицензия будет здесь» заменяем на собственно лицензию, сохраняем это всё — вуаля, лицензия со скрытием готова!</li></ol><br /><br />В обсуждении с комментаторами родилась идея: не скрывать картинку, которая содержится в лицензии, а сделать её кликабельной — своего рода кнопкой. Курсор при наведении меняется на «руку». Результат вы можете лицезреть справа, код — ниже:<pre class="code">&lt;div id="license-header" onclick="toggleLicense()" onmouseover="document.body.style.cursor='pointer'" onmouseout="document.body.style.cursor='auto'"&gt;<br />&lt;table align='center'&gt;<br />&lt;tr&gt;<br />&lt;td&gt;<br />&lt;script type="text/JavaScript"&gt;<br />function toggleLicense()<br />{<br />document.getElementById('license-body').style.display == 'none'? document.getElementById('license-body').style.display='block' : document.getElementById('license-body').style.display='none';<br />}                                                      <br />&lt;/script&gt;<br />&lt;img alt="Creative Commons License" style="border-width: 0pt;" src="http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png"/&gt;<br />&lt;/td&gt;<br />&lt;td&gt;<br />&lt;span style="font-size:11px"&gt;кликните для показа/скрытия лицензии&lt;/span&gt;<br />&lt;/td&gt;<br />&lt;/tr&gt;<br />&lt;/table&gt;<br />&lt;/div&gt;<br /><br />&lt;div id="license-body" style="display:none"&gt;<br />This work is licensed under a &lt;a href="http://creativecommons.org/licenses/by-nc-sa/3.0/" rel="license"&gt;Creative Commons Attribution-Noncommercial-Share Alike 3.0 Unported License&lt;/a&gt;.&lt;br/&gt;<br /><br />Материалы сайта &lt;a href="http://debiania.blogspot.com/"&gt;Debiania.blogspot.com&lt;/a&gt; написаны &lt;a href="http://www.blogger.com/profile/15979236009981641914"&gt;Programmaster&lt;/a&gt;'ом и доступны на условиях лицензии Creative Commons Attribution-Non-Commercial-Share Alike 3.0 Unported License. Вы можете копировать, распространять, показывать эту работу, и создавать производные работы &lt;b&gt;в некоммерческих целях на условиях&lt;/b&gt;:&lt;br/&gt;<br />&lt;ol&gt;&lt;li&gt;обязательной ссылки на автора (&lt;a href="http://www.blogger.com/profile/15979236009981641914"&gt;Programmaster&lt;/a&gt;, &lt;a href="http://debiania.blogspot.com/"&gt;Debiania.blogspot.com&lt;/a&gt;)&lt;/li&gt;&lt;li&gt;распространении любых производных работ на условиях этой же лицензии (&lt;a href="http://creativecommons.org/licenses/by-nc-sa/3.0/" rel="license"&gt;ссылка на лицензию&lt;/a&gt; обязательна!).&lt;/li&gt;&lt;/ol&gt;Это означает, например, что Вы можете копировать тексты с этого сайта на свой, помещая ссылки на мой сайт и на условия лицензии, но не можете размещать рядом с этими текстами контекстную рекламу.&lt;br/&gt;<br />&lt;b&gt;Пожалуйста, соблюдайте условия лицензии!&lt;/b&gt;<br />&lt;/div&gt</pre><br /><br />Если хочется ещё и иконку внизу страницы — это тоже несложно:<ol><li>Открываем Панель инструментов Blogger;</li><li>переходим в «Макет»;</li><li>сменяем вкладку на «Изменить HTML»;</li><li>в самый конец кода, перед &lt;/body&gt; ставим такой код:<pre class="code">&lt;a href="http://creativecommons.org/licenses/by-nc-sa/3.0/"&gt;&lt;img alt='Creative Commons License' src='http://i.creativecommons.org/l/by-nc-sa/3.0/80x15.png'/&gt;&lt;/a&gt;</pre></li><li>готово!</li></ol><br />И последнее на сегодня. Со временем вам наверняка захочется запихнуть ещё одну-две иконки рядом с предыдущей. Вот только если поставить теги img подряд, пусть даже в одну строку, изображения будут расположены подряд <b>друг под другом</b>. Выход — таблицы:<pre class="code">&lt;table align='center'><br />&lt;tr&gt;<br />&lt;td&gt;<br />&lt;!-- Site Meter XHTML Strict 1.0 --&gt;<br />&lt;script src='http://s51.sitemeter.com/js/counter.js?site=s51debiania' type='text/javascript'&gt;<br />&lt;/script&gt;<br />&lt;!-- Copyright (c)2006 Site Meter --&gt;<br />&lt;/td&gt;<br />&lt;td&gt;<br />&lt;!-- Creative Commons License Logo --&gt;<br />&lt;a href="http://creativecommons.org/licenses/by-nc-sa/3.0/"&gt;&lt;img alt='Creative Commons License' src='http://i.creativecommons.org/l/by-nc-sa/3.0/80x15.png'/&gt;&lt;/a&gt;<br />&lt;/td&gt;<br />&lt;/tr&gt;<br />&lt;/table&gt;</pre>Здесь мы видим два изображения (первое генерируется скриптом, но можете считать, что там тег img): иконки SiteMeter и Creative Commons.<br /><br />Удачи ;)<br /><br /><i>P.S. Спасибо dmage и kane с irc://irc.turli.net/#linux за помощь в разборках с JavaScript.<br />Также выражаю благодарность Andrey'ю и Dr.AKULAvich'у за содержательные и креативные комментарии.</i>

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T10:29:00.000+02:00, Dr.AKULAvich wrote:</p>
<p class='hakyll-convert-comment-body'>
Интересный вариант.<BR/>А можно ли сделать кнопки показать/скрыть псевдоссылками? То есть, чтобы курсор менялся при наведении.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T10:45:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо за отзыв, очень приятно! Самое главное, за что я люблю блоги, это решение, найденное кем-то, улучшается. Идея скрытия и показа текста лицензии мне очень понравилась. Думаю реализовать на своем блоге, тем более мои друзья уже обращали внимание, что дизайн впорядке, но вот блок с лицензией смотрится громоздко.<BR/><BR/>Кстати, можно ведь оставить кнопочку видимой, а текст скрывать :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T11:19:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<A HREF="http://www.blogger.com/profile/10567533341020480269" REL="nofollow">Dr.AKULAvich</A> пишет…<BR/><BR/><B>А можно ли сделать кнопки показать/скрыть псевдоссылками? То есть, чтобы курсор менялся при наведении.</B><BR/>Честно говоря, не знаю: как я уже писал в посте, JavaScript'ом не владею. Но покопаюсь, самому интересно стало :)<BR/><BR/><A HREF="http://www.blogger.com/profile/00833871247004051501" REL="nofollow">Andrey</A> пишет…<BR/><BR/><B>Самое главное, за что я люблю блоги, это решение, найденное кем-то, улучшается.</B><BR/>Open Source, ей-богу! :)<BR/><BR/><B>Кстати, можно ведь оставить кнопочку видимой, а текст скрывать :)</B><BR/>Кстати, идея! Сейчас сделаем :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T12:13:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Обратил внимание, что CC кнопочка внизу страницы не имеет ссылку на лицензию, можно добавить, ведь пользователи любят иногда щелкать по кнопочкам, пузомеркам и счтетчикам :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T13:39:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<B>2Andrey:</B><BR/>Спасибо за замечание, исправил.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T14:18:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
<B>2Programmaster</B><BR/>Вариант с нескрываемой кнопкой можно тоже добавить в статью, чтобы так сказать два типа решения было. Ну если хочешь конечно :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T14:37:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<B><A HREF="http://www.blogger.com/profile/10567533341020480269" REL="nofollow">Dr.AKULAvich</A> пишет…</B><BR/><B>А можно ли сделать кнопки показать/скрыть псевдоссылками? То есть, чтобы курсор менялся при наведении.</B><BR/>Вот, нашёл решение!<BR/>Нужно в тег добавить параметры onmouseover (когда мышь на объекте) и onmouseout (когда мышь уходит с объекта), например:<BR/>&lt;img alt=&quot;Creative Commons License&quot; style=&quot;border-width: 0pt;&quot; onmouseover=&quot;document.body.style.cursor=&#39;pointer&#39;&quot; onmouseout=&quot;document.body.style.cursor=&#39;auto&#39;&quot; src=&quot;http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png&quot;&gt;, то есть если мышь на картинке, то сменить курсор на «руку», а если мышь уходит — вернуть «auto».<BR/><A HREF="http://www.javascriptkit.com/dhtmltutors/csscursors.shtml" REL="nofollow">Список существующих видов курсоров</A>.<BR/><BR/><B><A HREF="http://www.blogger.com/profile/00833871247004051501" REL="nofollow">Andrey</A> пишет…</B><BR/><B>Вариант с нескрываемой кнопкой можно тоже добавить в статью, чтобы так сказать два типа решения было.</B><BR/>Да, естетсвенно. Будет через часок — надо подкрепиться :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T15:09:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
<B>2Programmaster</B><BR/>А как все-таки с текстом быть, чтобы курсор менялся?<BR/>Может неплохо чтобы все-таки была надпись "подробнее" или "условия лицензии"?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T16:29:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<B>А как все-таки с текстом быть, чтобы курсор менялся?</B><BR/>Точно так же, как с изображением:<BR/>&lt;h6 onmouseover=&quot;document.body.style.cursor=&#39;pointer&#39;&quot; onmouseout=&quot;document.body.style.cursor=&#39;auto&#39;&quot;&gt;показать или скрыть лицензию&lt;/h6&gt;<BR/><BR/><B>Может неплохо чтобы все-таки была надпись "подробнее" или "условия лицензии"?</B><BR/>Да что-то не подходит ничего… Пока что вон воткнул надпись под изображение (ближе не получается, чёрт) — пусть будет так.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T16:52:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Над надписью курсор остается неизменным
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T17:10:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Если ты про надпись в правой колонке — курсор над надписью и не должен меняться, так как она не «кликабельна».<BR/><BR/>А тот код, что я приводил, работоспособен — проверено.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T21:04:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Я сделал надпись по-ближе к кнопке. Просто убрал надпись из блока кода ява-скрипт и вписал надпись обычным текстом после кода. Можешь посмотреть в моем блоге.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-28T21:20:00.000+02:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
update: сделал по-другому, надпись вставил в скрипт после тега див(там где кнопка и текст: кликните для показа/сокрытия лицензии) див=div, так пишу, потому что иначе коммент не отправляется.<BR/><BR/>кликните для показа/сокрытия лицензии в тегах h6 Соответственно убираем<BR/><BR/>Со всеми моими изменениями работу скрипта смотри в моем блоге.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T00:49:00.000+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Да, я тоже текст под иконкой вынес за пределы первого div&#39;а и заключил в span вместо h6. Кроме того, вручную выставил высоту шрифта в 11 пикселей — теперь как раз то, о чём мечталось :)<BR/><BR/>Маленький хинт по постингу тегов в камментах (да и в блоге пригодится): тег (например, &lt;div&gt;) записывается так: «&amp;lt;div&amp;gt;». Сам ампресанд («&amp;» который) выглядит как «&amp;amp;».<BR/><BR/>;)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T10:53:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Интересный хинт, буду иметь ввиду. Сейчас сделал как у тебя - обычный шрифт 11-го размера, смотрится реально лучше.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T11:17:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
update: Переместил текст справа от картинки, тоже интересно смотрится :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T12:58:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
О, вот так мне нравится!<BR/>Сейчас поправлю свою и обновлю пост.<BR/>Спасибо!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T13:26:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Пожалуйста! Это наш коллективный труд, код получился отличный, так же как и сам блок с лицензией.<BR/><BR/>Только вот правильно может писать не сокрытия а скрытия?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T13:34:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Наверное, да. От «сокрытия» какой-то уголовщиной тянет :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T17:44:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
В опере почему-то курсор не меняется при наведении на кнопку
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T17:49:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Да, а ещё у некоторых пользователей ADBlock+ не отображается кнопка и, соответственно, не разворачивается лицензия.<BR/><BR/>Надо будет этими багами заняться (для оперы можно попробовать менять курсор на hand, а не pointer; можно сделать кликабельной не только картинку, но и надпись — для этого не div с img и span заключаем в таблицу, а в div заключаем таблицу с img и span внутри).
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T18:31:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
А почему adblock режет скрипт?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T21:22:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
update: наконец-то удалось научить курсор меняться при наведении на текст, но пришлось для этого прописать стиль в css.<BR/>Вообщем новая концепция блока копирайта у меня в блоге.<BR/>Большой плюс, то что сейчас открытие менюшки не привязано к кнопке (не все включают графику + картинка может быть вырезана adblock`ом), еще плюс, то что в опере курсор при наведении на текст тоже меняется.<BR/><BR/>Теперь получается, что кнопка ведет на сайт CC, а псевдо-ссылка открывает менюшку. Я вот думаю кнопку вообще сделать неактивной, как считаешь?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T22:27:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Может быть, это уже стереотип, но я ожидаю, что картинка будет действовать как кнопка. Всё-таки лучше, чтобы весь блок был кликабелен и разворачивал лицензию — так, как на данный момент сделано у меня (только что поправил).<BR/><BR/>Касательно курсора — странно, я считал, что параметр style=&quot;тут обычный CSS код&quot; у тега равносилен class=&quot;тут имя стиля, объявленного в &lt;style&gt;&quot;.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T22:35:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Наверное равносильно, надо попробовать.<BR/>Кстати, ты как реализовал сейчас код? У тебя реализация сейчас то что надо.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T22:41:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Да, style и class равносильны, я просто прописал в style cursor: pointer; и при наведении на текст курсор меняется. Обнови пожалуйста блок кода в статье, хочу твою реализацию посмотреть. У меня почти как у тебя, только кнопка неактивна. Сейчас буду ее делать.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T22:58:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Дык сейчас в статье как раз тот код, что и в гаджете. Символ в символ.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T23:03:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
Точно :)<BR/>Реализовал один в один как у тебя, только надпись сделал в виде ссылки (подчеркнутый текст) и надпись звучит как "показать/скрыть лицензию"<BR/>Твой вариант считаю идеальным с точки зрения написания кода.<BR/>Поздравляю, гаджет готов :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T23:14:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Только что взглянул на блог через Opera (из-под винды, правда — у меня оперы нету и ставить долго, а комп сестры как раз под рукой…) — «рука» отображается нормально.<BR/>Правда, у меня в Iceweasel она отображается только после полной загрузки страницы — или около того (не знаю, как в Opera на винде — там интернет побыстрее и я не успеваю заметить :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T23:27:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Да, пожалуй, это финал :) Большего не надо.<BR/><BR/>«Идеал — это не когда нечего добавить, а когда нечего выбросить» (не помню, кто это сказал :(<BR/><BR/>Прими встречные поздравления.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-29T23:34:00.000+03:00, Andrey wrote:</p>
<p class='hakyll-convert-comment-body'>
А чего смайлик такой - :(<BR/>:)<BR/><BR/>Встречные поздравления принимаю. Багов вроде быть не должно. Под оперой в Дебиане смотрел щас, лапа отображается нормально. Интернет у меня быстрый, поэтому не могу проследить сразу или нет отображается лапа на курсоре
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T12:20:00.000+03:00, virens wrote:</p>
<p class='hakyll-convert-comment-body'>
Да, занятное решение. На меня тут напала очередная волна минимализЬма, думаю применить.<BR/><BR/>Programmaster, ты, конечно, поскромничал, когда сказал, что не знаешь языков программирования. Та впечатляющая конструкция, которая тут приведена, говорит за обратное.<BR/><BR/>Надо будет приделать это к моему многострадальному и веками переделываемому шаблону :-)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T12:28:00.000+03:00, duke wrote:</p>
<p class='hakyll-convert-comment-body'>
Присоединяюсь к <B>virens</B>`у, код действительно отличный :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T12:31:00.000+03:00, duke wrote:</p>
<p class='hakyll-convert-comment-body'>
2<B>virens</B><BR/>У меня есть еще одна идея для Вашего многострадального шаблона, отправил мыло на mydebianblog[dog]gmail[dot]com
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T18:39:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<B><A HREF="http://www.blogger.com/profile/12420257446841864325" REL="nofollow">virens</A> комментирует…</B><BR/><B>Programmaster, ты, конечно, поскромничал, когда сказал, что не знаешь языков программирования.</B><BR/>Я говорил только про JavaScript:<BR/><B>&gt;Нужно отметить, что web-дизайнер из меня никудышный, и JavaScript я не знаю.</B><BR/>:P<BR/>Да и заявление «никудышный дизайнер» относится к отсутствию таланта созидать красивое, а не к незнанию HTML/JavaScript. Хотя в первом я таки немного ориентируюсь :)<BR/><BR/>Кстати, не думаю, что эта идея впишется в твой шаблон — то, как лицензия выглядит сейчас (у тебя, я имею в виду), вполне нормально.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-03-30T18:51:00.000+03:00, duke wrote:</p>
<p class='hakyll-convert-comment-body'>
<B>2Programmaster</B><BR/>Ты еще хотел копирайт к посту прикрутить, мы с тобой обсуждали если помнишь.
</p>
</div>



