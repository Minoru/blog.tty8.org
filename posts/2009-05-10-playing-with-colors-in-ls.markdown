---
title: Играем с цветами вывода ls
published: 2009-05-10T11:24:00Z
categories: 
tags: linux
description: Правим ~/.dir_colors по собственному вкусу.
---

Вы работаете в консоли или терминале с тёмным фоном? Вы используете <code>ls --color=auto</code>? Как вам цвет для директорий, нравится?

На мой взгляд, тёмно-синий шрифт на чёрном фоне — это просто убийство глаз, поэтому я поставил себе за цель поменять цвет на какой-то более читабельный и удобный.

За цвета для <code>ls</code> «отвечает» утилита <code>dircolors</code>. При запуске она выдаёт команды, которые нужно передать шеллу для настройки цветов. Обычно в конфигах шеллов есть отдельные строки, которые запускают <code>dircolors</code> и создают алиас для <code>ls</code> вида <code>ls='ls --color=auto'</code>. В стандартном конфиге для bash в Debian Lenny эти строки выглядят так:
```
if [ "$TERM" != "dumb" ]; then
  eval "`dircolors -b`"
  alias ls='ls -l -F --color=auto'
fi
```
Для zsh, который я сейчас использую, необходимые строки надо добавить самостоятельно:
```
eval `dircolors`
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
```
Отлично. Уже ясно, откуда берутся настройки цветов — осталось только научиться их менять. Для этого читаем ман…

…Читаем ман и узнаём, что `dircolors`'у можно передать файл, из которого будут прочитаны настройки. Для создания файла выполняем простую инструкцию:
```
dircolors -p >~/.dir_colors
```
В результате получаем в домашнем каталоге дотфайл, который осталось лишь поправить.

Пожалуй, стоит начать с чтения комментариев. Из них мы узнаём, что вид вывода задаётся перечислением атрибутов через точку с запятой. Доступны следующие атрибуты:<table border="1px"><tr><td colspan="2" align="center">Атрибуты</td></tr><tr><td>00</td><td>ничего</td></tr><tr><td>01</td><td>полужирный</td></tr><tr><td>04</td><td>подчёркивание</td></tr><tr><td>05</td><td>мигание</td></tr><tr><td>07</td><td>реверс</td></tr><tr><td>08</td><td>скрытие</td></tr><tr><td colspan="2" align="center">Цвета текста</td></tr><tr><td>30</td><td>чёрный</td></tr><tr><td>31</td><td><font color="red">красный</font></td></tr><tr><td>32</td><td><font color="green">зелёный</font></td></tr><tr><td>33</td><td><font color="yellow">жёлтый</font></td></tr><tr><td>34</td><td><font color="blue">синий</font></td></tr><tr><td>35</td><td><font color="magenta">розовый</font></td></tr><tr><td>36</td><td><font color="cyan">циан</font></td></tr><tr><td>37</td><td>белый</td></tr><tr><td colspan="2" align="center">Цвета фона</td></tr><tr><td>40</td><td bgcolor="black"><font color="white">чёрный</font></td></tr><tr><td>41</td><td bgcolor="red">красный</td></tr><tr><td>42</td><td bgcolor="green">зелёный</td></tr><tr><td>43</td><td bgcolor="yellow">жёлтый</td></tr><tr><td>44</td><td bgcolor="blue">синий</td></tr><tr><td>45</td><td bgcolor="magenta">розовый</td></tr><tr><td>46</td><td bgcolor="cyan">циан</td></tr><tr><td>47</td><td>белый</td></tr></table>

За цвет директории отвечает параметр DIR. У меня он выглядел так:
```
DIR 01;34 #directory
```
Я заменил это на следующее:
```
DIR 01;30;47 # directory
```
, то есть чёрный шрифт на белом фоне. Выглядит немного неуклюже и необычно, но это оттого, что я ни разу не дизайнер :) Найдёте приятную комбинацию — обязательно напишите комментарий или e-mail!

Осталась самая малость — научить шелл применять сделанные нами настройки. Это просто — в уже приводившихся участках конфигов <code>dircolors</code> должен быть заменён на <code>dircolors ~/.dir_colors</code>. Настройки будут применены после сохранения конфига и перезагрузки шелла :)

Have fun!


<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-10T17:36:00.000+03:00, Maxim G. Ivanov wrote:</p>
<p class='hakyll-convert-comment-body'>
Познавательно, но когда передо мной возникла такая же проблема, я её решил без использования этой утилиты (по какой-то причине не знал о ней до прочтения этой заметки).  Я правил настройки терминала в ~/.Xresources (естественно для тех, которые ими управляются, например, xterm, rxvt-unicode).

Цвета директорий определяются переменной color12.  Так, у меня записано для rxvt-unicode в ~/.Xresources следующее:
```
urxvt*color12:     #8FB9DC
```

цвет явно помягче дефолтного, что и требовалось.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-10T18:34:00.000+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Привет!

<B>по какой-то причине не знал о ней до прочтения этой заметки</B><br/>
Я же по какой-то причине не знал о возможности поправить это через ~/.Xresources :)

В принципе, твоё решение более гибкое, т.к. позволяет задать произвольный цвет в RRGGBB. С другой стороны, ~/.Xresources играет роль только в иксах — в tty он уже побоку. Я, вобщем-то, не так уж часто бываю в tty, но всё же случается — иногда комп включается только для того, чтобы залить на флешку какой-то файлик.

В общем, спасибо за информацию! :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-07-10T10:32:28.848+03:00, razum2um wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>тёмно-синий шрифт на чёрном фоне</b><br/>
Это да, страшно, наверное. <br/>
Но не вижу ничего плохого в синем полужирном на сером фоне (aka &quot;Темная пастель&quot; в kde)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-07-10T12:03:10.457+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<b><a href="http://razum2um.alwaysdata.net/" rel="nofollow">razum2um</a> пишет…</b><br/>
<b>&gt; тёмно-синий шрифт на чёрном фоне<br/>
Это да, страшно, наверное.</b><br/>
Сначала ничего, а вот к вечеру, когда глаза устанут, становится очень неприятно…

<b>Но не вижу ничего плохого в синем полужирном на сером фоне (aka &quot;Темная пастель&quot; в kde)</b><br/>
Дело в том, что от KDE я отошёл уже достаточно давно, в качестве терминала юзаю urxvt — там никаких цветовых схем нет, просто окошко с шеллом, поэтому проблему пришлось решать именно со стороны шелла. Хотя вариант, предложенный Максимом Ивановым (первый комментарий), тоже имеет право на жизнь — может быть, даже большее, чем мой.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-05-18T00:26:21.377+03:00, sessile polyp wrote:</p>
<p class='hakyll-convert-comment-body'>
Добрый день. замечательная статья, спасибо огромное:-)... Интересный сайт хорошо описали
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-05-18T02:33:03.582+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо ☺ Заходите ещё! ;)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-05-13T19:28:32.167+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Огромное спасибо, человек! Умаялся, настраивая цвета для urxvt-терминала. Ставлю, тестирую на ls и нулевой эффект.<br/>
Добавлю-ка я блог в букмарки. :)<br/>
~Zyamilon
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-05-13T19:56:01.313+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Всегда пожалуйста ;) Обрати внимание, что я переехал на сторонний хостинг (**UPD**: т.е. сюда, на blog.debiania.in.ua), так что в букмарки следует добавлять уже новый блог. Хотя в старом тоже есть что почитать, да.
</p>
</div>



