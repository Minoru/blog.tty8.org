---
title: Blogger: запрет индексирования главной
published: 2009-12-16T18:00:00Z
categories: 
tags: blog
---

Давненько уже подумывал о том, чтобы запретить индексацию главной — хочется избежать такой вот чепухи:<br /><img src="http://lh5.ggpht.com/_Nb6QYFUvBjY/Syfmvj-zT6I/AAAAAAAAAG4/lfihi2HXdRc/s800/crap.png" alt="Чепуха в выдаче Гугла" style="text-align: center; border: 0pt none" /><br />Позавчера таки собрался и сделал, пока что работает :)<a name='more'></a><br /><br />Для того, чтобы добавить в свой блог аналогичную фичу, выполните следующее (желательно предварительно сохранить бекап своего шаблона; сделайте это после выполнения пункта 2):<ol><li>перейдите на <i>Панель инструментов</i> и кликните по ссылке <i>Настройка</i> рядом с названием вашего блога</li><li>перейдите на вкладку <i>Дизайн</i>, а там — в секцию <i>Изменить HTML</i></li><li>в самое начало кода, сразу после <code>&lt;title&gt;&lt;data:blog.pageTitle/&gt;&lt;/title&gt;</code>, допишите:<pre class="code">&lt;b:if cond='data:blog.pageType == &quot;index&quot;'&gt;<br />&lt;!-- allow robots to follow links on main page, but forbid page indexing --&gt;<br />&lt;meta content='noindex,follow' name='robots'/&gt;<br />&lt;/b:if&gt;</pre></li><li>нажмите <i>Сохранить шаблон</i></li></ol>Дело сделано! При следующем заходе на ваш блог боты не станут индексировать главную, так что она не будет всплывать в выдаче поисковиков.<br /><br /><h4>Для любопытных</h4>Теперь объясню, что это мы только что написали :)<br /><br />Код представляет собой простое условие, состоящее в определении типа страницы — «index» означает главную. <code>data:blog.pageType</code> — переменная, в которой хранится тип текущей страницы. Итак, мы выяснили, что говорит условие: «если мы на главной, то…».<br /><br />Тег <code>meta</code> является стандартным HTML тегом, используемым для передачи метаинформации (неожиданно, да? :) вроде заголовка страницы (который отображается в заголовке окна браузера), адресов RSS и Atom фидов, указания кодировки страницы и прочего. Одним из возможных передаваемых значений являются указания поисковым роботам, которые мы используем.<br /><br />Итак, <code>name='robots'</code> указывает на то, что тег предназначен для роботов, а <code>content='noindex,follow'</code> говорит, что роботам можно переходить по ссылкам с этой страницы (что приведёт их к постам), но нельзя запоминать содержимое текущей страницы (то есть индексировать её). Таким образом, боты будут знать только о постах и при поиске выдача будет нормальной, без чепухи, приведённой на скрине выше.<br /><br /><br /><br /><i>При подготовке материала использовалась система помощи Blogger.</i>

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-17T00:27:30.095+02:00, sash-kan wrote:</p>
<p class='hakyll-convert-comment-body'>
google-spider, «заходящий» на blogspot.com???<br />что ему там делать???
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-17T01:02:26.418+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Почему нет? А как ещё постам в выдачу гугла попасть, кроме как через бота? Вы же сами видите скриншот.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-19T19:59:49.051+02:00, ffsdmad wrote:</p>
<p class='hakyll-convert-comment-body'>
а что он индексировать будет?<br />блоггер даёт sitemap.xml - как пауки найдут новые страницы?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-20T04:38:42.944+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Не знаю,  что там с sitemap.xml, но после применения описанного решения главная из выдачи гугла исчезла. А новые страницы бот найдёт без проблем — по ссылкам-то с главной ходить можно.
</p>
</div>



