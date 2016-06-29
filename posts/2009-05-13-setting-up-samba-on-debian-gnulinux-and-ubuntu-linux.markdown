---
title: Установка и настройка Samba в Debian GNU/Linux и Ubuntu Linux
published: 2009-05-13T16:09:00Z
categories: 
tags: mirror,linux,debian
---

Копия статьи с ныне мёртвого проекта debian-ubuntu-linux.ru.<br/>
Обратите внимание, что я всего лишь скопировал статью сюда — я лишь немного исправил её (в основном опечатки), и не имею никакого отношения к её содержимому (более того, с некоторыми моментами я категорически не согласен). С другой стороны, я считаю эти статьи полезными для новичка, и потому копирую их.<br/>
Все авторские права принадлежат Дмитрию Белоусову. Мною выполнены мелкие правки (исправление опечаток).

<div align="right">Дмитрий Белоусов<br/>
Последнее обновление: 02.02.2008</div>

<b>Samba</b> — это эффективный способ не только организовать взаимодействие компьютеров под управлением Windows и Linux, но и в сетях, состоящих только из Linux-машин он позволяет быстро организовать общий доступ к ресурсам. Файл конфигурации Samba может достигать огромной длины и учитывать множество параметров, однако в большинстве случаев достаточно гораздо меньшего количества настроек.


<h3>I. Установка Samba</h3>
Если мы хотим и расшаривать сами и иметь доступ к файлам на других компьютерах, то необходимо установить три пакета:
```
$ sudo aptitude install samba smbclient smbfs
```

<h3>II. Настройка Samba</h3>
1\. Создайте резервную копию <code>/etc/samba/smb.conf</code>:
```
$ sudo cp /etc/samba/smb.conf /etc/samba/smb.conf.backup
```
Теперь откройте файл <code>/etc/samba/smb.conf</code> для редактирования в вашем любимом текстовом редакторе. Например так:
```
$ sudo nano /etc/samba/smb.conf
```
Все то, что там есть – нам не нужно. Можете отчистить файл полностью, и добавить, например, такую конфигурацию:

```
[global]
workgroup = home
netbios name = desktop
server string = anonymous lan file server
security = share
browseable = yes

[user]
path = /home/user
comment = mediafiles
readonly = No
guest ok = Yes

[ftp]
path = /home/ftp
comment = ftpfiles
readonly = No
guest ok = Yes
```

Теперь можете заменить информацию на свою:

* <code>workgroup</code> – это имя вашей сети, должно быть одинаковым для всех компьютеров, как рабочая группа в Windows.
* <code>netbios name</code> – имя вашего компьютера в сети, сделайте всем машинам уникальные, как имя компьютера в Windows.
* <code>server string</code> — описание компьютера, аналог подобного значения в Windows.
* <code>security</code> — определяет доступ к расшариваемым каталогам.
* <code>browseable</code> — хотите ли вы сделать доступными все подкаталоги рашариваемого каталога. Этот параметр также можно использовать отдельно для каждого расшариваемого каталога.
* <code>path</code> — путь до расшариваемого каталога. В данном конкретном примере будут расшарены домашний каталог пользователя user и домашний каталог пользователя ftp.
* <code>comment</code> — комментарий.
* <code>readonly</code> – только для чтения. Обратите внимание, что Samba может ограничить права пользователя, но не может расширить права, заданные системой. То есть если на рсшариваемом каталоге не стоят права на запись для всех в самой системе, Samba не сможет разрешить запись в него сторонним пользователям. Однако если на каталоге стоят права 777, то задав параметр <code>readonly = Yes</code> вы сможете ограничить доступ на запись для пользователей, подключающихся из сети.
Аналогично вы можете создать любое количество сетевых каталогов.

После завершения конфигурации выполните команду
```
$ testparm
```
она автоматически проверит файл конфигурации. После этого перезагрузите Samba:
```
$ sudo /etc/init.d/samba restart
```
Обратите внимание, что после загрузки компьютеры не сразу появляются в сети, что связанно с особенностями протокола.

Это простейшая конфигурация Samba, однако для большинства ситуаций ее вполне достаточно.

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-24T10:10:36.631+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Отличное руководство для новичков!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-06-09T11:32:29.425+03:00, virens wrote:</p>
<p class='hakyll-convert-comment-body'>
Хе-хе, а вот виндоуз-пользователи вас не поймут: нет настройки кириллицы в конфиге. Но я это скоро поправлю своим постом :-)

P.S&gt; Так держать: правильно выкладываешь хорошие посты, а то некоторые несознательные товарищи потом удаляют ресурсы\форумы вместе со статьями.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-06-09T22:52:43.037+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Хе-хе, а вот виндоуз-пользователи вас не поймут: нет настройки кириллицы в конфиге. Но я это скоро поправлю своим постом :-)</b>

Твои сведения устарели: у меня samba с такими настройками уже месяц работает и проблемы с киррилицей ни разу не наблюдались. Сейчас специально виндузятников просил проверить — говорят, всё видно.

<b>P.S&gt; Так держать: правильно выкладываешь хорошие посты, а то некоторые несознательные товарищи потом удаляют ресурсы\форумы вместе со статьями.</b>

Ну, за сознательность автора ручаться не буду — может, он это и сознательно удалил, а может, просто нет денег продлить хостинг, — но да, жалко, что такой материал пропадает — потому и скопировал.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-06-09T23:06:37.636+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
Ки<i>р</i>ил<i>л</i>ицы, конечно.

P.S. Yahoo! Наконец-то починили баг с переводом строки в комментарии — надобность в &lt;br /&gt; после b, i и прочих отпала :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-11-26T19:15:56.442+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Не подскажете, какие порты следует прокинуть роутеру, чтобы видеть вне роутерной сети?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-07-04T23:39:04.105+03:00, taz wrote:</p>
<p class='hakyll-convert-comment-body'>
@anonim <br/>
netstat -nlp и смотри что слушает smd
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-08-08T17:56:22.382+03:00, vovane2 wrote:</p>
<p class='hakyll-convert-comment-body'>
А если я хочу чтоб папка была доступна только для чтения и для всех,в то же время требуя пароль для записи?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-08-09T01:13:19.188+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>2 vovane2</b>:<br/>
Я, честно говоря, в самбе разбираюсь на очень поверхностном уровне, а статью перепостил потому, что она хорошая. Ответа на твой вопрос, увы, не знаю, но советую спросить на <a href="unixforum.org" rel="nofollow">unixforum.org</a>
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-08-13T12:29:59.428+03:00, Георгий wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо. Хорошая статья. Все работает.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-12-02T14:06:09.370+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Здорово! спасибо большущее :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-03-23T09:59:50.672+02:00, Dragovalovskiy wrote:</p>
<p class='hakyll-convert-comment-body'>
вот спасибки. 

А я мудак с naitilus-sare возился.

подскажите пожалуйста люди добрые как на ntfs разделе папку расшарить, и ntfs-config не запускается, и как подключать ntfs не знаю.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-03-25T21:53:42.466+02:00, Slon wrote:</p>
<p class='hakyll-convert-comment-body'>
Все супер, все работает :)<br/>
Вот еще бы как квоты установить на расшаренные папки в том же стиле как и эта статья.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-03-25T22:56:23.372+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>2 Dragovalovskiy:</b><br/>
<i>&gt; подскажите пожалуйста люди добрые как на ntfs разделе папку расшарить, и ntfs-config не запускается, и как подключать ntfs не знаю.</i>

Папки с NTFS расшариваются точно так же, как и с любой другой файловой системы — дописываешь в конфиг Samba соответствующие пути и радуешься жизни. Для того, чтобы примонтировать NTFS раздел, нужно сначала установить пакет ntfs-3g, а потом воспользоваться терминалом и командой mount (или Dolphin/Nautilus, если предпочитаешь GUI).


<b>2 Slon:</b><br/>
<i>&gt; Вот еще бы как квоты установить на расшаренные папки в том же стиле как и эта статья.</i>

Увы, сам я в Samba не разбираюсь, так что за ответом тебе придётся обратиться на какой-нибудь форум — например, <a href="http://unixforum.org/" rel="nofollow">unixforum.org</a>
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-01T20:29:09.642+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Ошибка в файле /etc/samba/smb.conf:  bios name = desktop ---&gt; netbios name = desktop
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-01T20:39:22.224+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо, исправлено!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-27T18:51:27.872+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Сделал как написано автором статьи. В терминале под рутом ношу эту строку /etc/init.d/samba restart и получаю ответ что такого файла не существует.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-27T20:37:24.782+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Анонимный</b>, samba точно установилась? Может, там сервис переименовали? Я её (samba) не юзаю, могу новостей не знать — вдруг там переименовали чего. Телепаты в отпуске, звиняй.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-28T08:51:18.727+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Честно сказать, незнаю!<br/>
Вот что пишет терминал
```
Следующие НОВЫЕ пакеты будут установлены:        
  samba{b} 
0 пакетов обновлено, 1 установлено новых, 0 пакетов отмечено для удаления, и 128 пакетов не обновлено.
Необходимо получить 7 637 kB архивов. После распаковки 21,6 MB будет занято.
Следующие пакеты имеют неудовлетворённые зависимости:
  samba: Зависит: libwbclient0 (= 2:3.5.6~dfsg-3squeeze6) но установлен 2:3.6.3-1~bpo60+1 
Следующие действия разрешат зависимости:

     Сохранить для следующих пакетов их текущие версии:
1)     samba [Не установлен]                           



Принять данное решение? [Y/n/q/?] y
Ни одного пакета не будет установлено, обновлено или удалено.
0 пакетов обновлено, 0 установлено новых, 0 пакетов отмечено для удаления, и 128 пакетов не обновлено.
Необходимо получить 0 B архивов. После распаковки 0 B будет занято.

А вот что пишет при установке
# aptitude install libwbclient0
Ни одного пакета не будет установлено, обновлено или удалено.
0 пакетов обновлено, 0 установлено новых, 0 пакетов отмечено для удаления, и 128 пакетов не обновлено.
Необходимо получить 0 B архивов. После распаковки 0 B будет занято
```
 И В ИТОГЕ НИЧЕГО НЕ ПРОИСХОДИТ!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-28T14:44:49.508+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Анонимный</b>, ты не можешь поставить samba, потому что у тебя система в каком-то полуобновлённом состоянии (видишь же, пишут, что 128 пакетов не обновлено). Советую сделать sudo su -c &#39;aptitude update &amp;&amp; aptitude safe-upgrade&#39;, если останутся необновлённые пакеты — sudo aptitude dist-upgrade, в случае проблем — sudo apt-get install -f.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-28T18:47:31.292+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо.Я как только написал сюда, сразу обновил. ну и после этого попробовал установить samby. Все получилось. все заработало. Осталось дело за малым - это настройкой входа их winXP
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-06-15T14:13:14.741+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Помогите плиииз....

Мне нужно создать доменную группу пользователей.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-06-15T22:52:59.938+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<i>&gt; Мне нужно создать доменную группу пользователей.</i>

<b>Анонимный</b>, я в таких тонкостях не разбираюсь — обратись на <a href="http://unixforum.org/" rel="nofollow">unixforum.org</a>, например.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-08-26T08:02:33.498+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Есть такая прога GADMIN SAMBA - в ней настройка делается в GUI - нтуитивно понятная. Натраивается всё, в том числе с директориями, паролями и уровнями доступа. 
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-10-17T09:34:16.234+03:00, Леонид wrote:</p>
<p class='hakyll-convert-comment-body'>
Не линуксоид. Можно сказать нуб :) Поставил на старенький системник Kerio Control 7.2.2. Ядро дебиановское, но очень урезана система по функционалу. Н-р, аптитьюд или апт-гет за команду не считает, и т.п.<br/>
Самба вроде установлена. Самба.конф переделал. Машину перезагрузил. Тестпарм показывает, что всё в порядке. А из сети машину не видно... хотя в правилах файрвола настроил доступ по телнету, ссх, фтп и т.д.

Где копать? :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-10-17T14:19:30.663+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Леонид</b>, с такими вопросами лучше всё-таки на форумы, например, <a href="unixforum.org" rel="nofollow">unixforum.org</a>. От себя могу посоветовать только nmap&#39;ом посмотреть на свой внешний интерфейс (nmap твой_IP_в_сети) — есть ли там порт Samba?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2013-07-24T18:30:48.117+03:00, Шихов Алексей wrote:</p>
<p class='hakyll-convert-comment-body'>
&gt;&gt;а то некоторые несознательные товарищи потом удаляют ресурсы\форумы вместе со статьями.

А ты не думал, что с автором могло что нибудь случится? Может быть он лежит в больнице, может быть его больше нет, и некому проплатить хостинг?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2015-03-07T22:47:46.445+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Может уже поздно по поводу доменной сети, но тут линк есть, если че <br/>
http://www.linuxatwork.ru/2011/05/19/ubuntu-to-active-directory-manual-1/
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2016-01-30T06:48:50.459+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Отлично! Я забодался делать свой комп доступным по сети в Opensuse. Видеть видят, но зайти не могут! А в Debian + твой конфиг - всё работает! Причём этот же конфиг в Opensuse - всё равно не пускает...
</p>
</div>



