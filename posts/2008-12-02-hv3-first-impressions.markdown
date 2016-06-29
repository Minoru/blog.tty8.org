---
title: Hv3: первое впечатление
published: 2008-12-02T20:39:00Z
categories: 
tags: review,web
---

Недавно на <a href="http://www.blogger.com/www.linux.org.ru" target="_blank">www.linux.org.ru</a> появился <a href="http://www.linux.org.ru/view-message.jsp?msgid=3299226" target="_blank">коротенький анонс</a> новенького браузера — Hv3. Меня, конечно же, захлестнуло любопытство, и я его скачал. О моих впечатлениях от 20-тиминутного тестинга читайте ниже.

... Итак, я кликнул по ссылке "Подробности" и попал на <a href="http://tkhtml.tcl.tk/hv3.html" target="_blank">сайт разработчика</a>. Сразу бросился в глаза простенький, но понятный и приятный дизайн — люди знают толк в минимализме. Быстро пробежавшись по странице глазами, я определил, что именно качать (любезные разработчики даже дали конкретный листинг команд для Linux-пользователей). Итак, в консоль было вбито:
```
wget http://tkhtml.tcl.tk/hv3-linux-nightly-08_0203.gz
gunzip hv3-linux-nightly-08_0203.gz
chmod 755 hv3-linux-nightly-08_0203
./hv3-linux-nightly-08_0203
```
Три минуты ожидания (архив всего 2,5 Мб, но канал у меня медленный) — и я уже счастливый обладатель новенького браузера. Ну что же, поехали тестить!
Первым делом программа радостно сообщила мне, что хочет поставить чего-то в текущую директорию, при этом мягко, но вполне понятно предупредив, что если откажусь, она будет надоедать этим сообщением каждый раз, когда я буду запускать браузер. Т.к. весь процесс выполнялся в tmp, я бесстрашно принял все требования и жмакнул ОК.

<div class="center">
<a href="/images/00. Install hv3_polipo request.png">
<img src="/images/00. Install hv3_polipo request-thumbnail.png"
    width="320px" height="131px"
    alt="Запрос Hv3 на установку hv3_polite"
    class="fullscreen" />
</a>
</div>

Бразуер шустренько (доли секунды, что и не странно — там той hv3_polipo всего 160 кб) поставил чего хотел и запустился. Вот так выглядит главное окно программы:

<div class="center">
<a href="/images/01. Hv3 main window.png">
<img src="/images/01. Hv3 main window-thumbnail.png"
    width="320px" height="280px"
    alt="Главное окно программы"
    class="fullscreen" />
</a>
</div>

Первым делом я посетил домашнюю страничку самого Hv3 — просто чтобы проверить способность этого браузера хоть что-то отображать. Оказалось, что со связкой HTML+CSS программа справляется достаточно хорошо. Сразу же воспользовался опцией самого Hv3 — в контекстном меню области отображения выбрал Debug, а далее Exec firefox -remote, после чего та же страничка открылась и в Iceweasel. Найти отличий мне так и не удалось.

<div class="center">
<a href="/images/02. Hv3' homepage - view in Hv3.png">
<img src="/images/02. Hv3' homepage - view in Hv3-thumbnail.png"
    width="320px" height="280px"
    alt="Домашняя страничка Hv3, открытая в самом Hv3"
    class="fullscreen" />
</a>
</div>

Ну что же, настал черед более серьёзных сайтов. В адресную строку было вбито первое, что взбрело в голову — а именно адрес блога <b>virens</b>'а, <a href="http://mydebianblog.blogspot.com/">mydebianblog.blogspot.com</a>. Как и все нормальные браузеры, Hv3 добавил префикс протокола — http:// — и принялся грузить страничку, активно информируя меня о происходящем посредством строки состояния.
Загрузка отняла у секунд 20 — кстати, Iceweasel тратит на это примерно столько же.
Скроллнув страничку чуть ниже, я понял, что зря так хвалил новый браузер — он не смог отобразить ни маленьких аватарок пользователей, являющихся постоянными читателями "Записок дебианщика", ни календаря постов. На скрине вы можете увидеть Hv3 и Iceweasel и почувствовать разницу в отображении. Тем не менее, фирменный оранжевый микроскоп <b>virens</b>'а таки отобразился — а значит, браузер всё-таки не безнадёжен (демонстрация на втором скрине).

<div class="center">
<a href="/images/03. Differences in views.png">
<img src="/images/03. Differences in views-thumbnail.png"
    width="320px" height="244px"
    alt="Разница в отображении блога virens'а"
    class="fullscreen" />
</a>
</div>

<div class="center">
<a href="/images/04. Differences in views - 2.png">
<img src="/images/04. Differences in views - 2-thumbnail.png"
    width="320px" height="244px"
    alt="Разница в отображении блога virens'а - 2"
    class="fullscreen" />
</a>
</div>

"Куда бы ещё ткнуться?" — спросил внутренний голос, в то время как пальцы уже набивали на клавиатуре адрес сайта, выглядящего достаточно удобным тестом на отображение (картинок довольно много, плюс ещё есть немного рюшечек) — mail.ru.
К моему удивлению (а в некоторой степени и удовлетворению), страницы выглядели почти одинаковыми, что вы и можете лицезреть на скриншотах чуть ниже.

<div class="center">
<a href="/images/06. Mail.ru in Hv3.png">
<img src="/images/06. Mail.ru in Hv3-thumbnail.png"
    width="320px" height="244px"
    alt="Mail.ru in Hv3"
    class="fullscreen" />
</a>
</div>

<div class="center">
<a href="/images/05. Mail.ru in Iceweasel.png">
<img src="/images/05. Mail.ru in Iceweasel-thumbnail.png"
    width="320px" height="244px"
    alt="Mail.ru in Iceweasel"
    class="fullscreen" />
</a>
</div>

Ладно, с отображением вроде разобрались — теперь взглянём на возможности, касающиеся закладок и прочего.
Диалог добавления новой закладки ничем особым не выделятся, всё как обычно:

<div class="center">
<a href="/images/07. Creating new bookmark.png">
<img src="/images/07. Creating new bookmark-thumbnail.png"
    width="320px" height="244px"
    alt="Bookmarks in Hv3"
    class="fullscreen" />
</a>
</div>

Собственно, на этом весёлом моменте мне захотелось спать :), поэтому тестинг браузера был отложен на попозже. Пока что мой вердикт таков: эту программу вполне можно использовать как браузер, если у вас нет ничего другого. В любом случае, это лучше чем lynx :)

Ждите продолжения обзора!
