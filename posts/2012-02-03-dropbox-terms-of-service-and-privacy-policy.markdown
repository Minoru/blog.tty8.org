---
title: Dropbox Terms of Service и Privacy Policy
published: 2012-02-03T14:43:00Z
categories: 
tags: EULA
---

Уже упоминавшийся в предыдущем посте разговор также сподвиг меня на прочтение <a href='https://www.dropbox.com/terms#terms'>условий сервиса</a>, которым я больше не пользуюсь — Dropbox.

В своё время я удалил свой аккаунт на дропбоксе из-за того, что в их условиях появилось противоречие: в одной части они гарантировали, что шифруют все мои файлы и что никто из сотрудников не может получить к ним доступ, а в другой сообщали, что мои файлы могут быть переданы властям в случае судебного разбирательства. И хотя я никогда не хранил на Dropbox'е что-либо, защищённое копирайтом третьих сторон, это несоответствие очень сильно поколебило моё доверие к сервису. В итоге я попросту удалил оттуда свой аккаунт, о чём даже не пришлось жалеть — замена в лице <code>rsync</code> чудесно справляется со всеми возникающими задачами.

Итак, давайте посмотрим, поменялось ли что-то с тех далёких времён.
<a name='more'></a>
<i>To be clear, aside from the rare exceptions we identify in our Privacy Policy, no matter how the Services change, we won’t share your content with others, <b>including law enforcement</b>, for any purpose unless you direct us to. How we collect and use your information generally is also explained in our Privacy Policy.</i>

Ну, вроде обещают защищать даже от стражей порядка, за исключением ситуаций, оговоренных в <a href='https://www.dropbox.com/terms#privacy'>Privacy Policy</a>.

<b><i>You acknowledge that if you wish to protect your transmission of data or files to Dropbox, it is your responsibility to use a secure encrypted connection to communicate with the Services.</i></b>

А вот это уже интересно. Ещё интереснее станет, когда прочтём <a href='https://www.dropbox.com/terms#security'>Security Overview</a>, в котором сказано, что при передаче используется 256-ибитный AES. Да, там же оговорено, что сторонние приложения могут не использовать шифрование, а некоторые устройства могут попросту его не поддерживать, но требование все равно остаётся довольно мутным (по крайней мере, для меня), т.к. непонятно, как же пользователь должен это своё обязательство реализовывать.


Некоторый интерес представляет также <a href='https://www.dropbox.com/terms#privacy'>Dropbox Privacy Policy</a> (рассматривается версия от 2-го июля 2011-го года).

<i><u>Geo-Location Information:</u> Some Devices allow applications to access real-time location-based information (for example, GPS). Our mobile apps do not collect such information as of the date this policy went into effect, but may do so in the future with your consent to improve our Services. <b>Some photos you place in Dropbox may contain recorded location information. We may use this information to optimize your experience. Also, some of the information we collect from a Device, for example IP address, can sometimes be used to approximate a Device’s location.</b></i>

По-моему, этот параграф противоречит самому себе. С одной стороны гарантируется, что с помощью GPS твоё местоположение отслеживать не будут, а с другой оставляется лазейка через геотеги и IP.

<i><u>Compliance with Laws and Law Enforcement Requests; Protection of Dropbox's Rights.</u> We may disclose to parties outside Dropbox files stored in your Dropbox and information about you that we collect when we have a good faith belief that disclosure is reasonably necessary to <b>(a) comply with a law, regulation or compulsory legal request;</b> (b) protect the safety of any person from death or serious bodily injury; (c) prevent fraud or abuse of Dropbox or its users; or (d) to protect Dropbox’s property rights. If we provide your Dropbox files to a law enforcement agency as set forth above, we will remove Dropbox’s encryption from the files before providing them to law enforcement. However, Dropbox will not be able to decrypt any files that you encrypted prior to storing them on Dropbox.</i>

Те самые ситуации, в которых Dropbox оставляет за собой право передавать третьим лицам информацию пользователей. Мне кажется, что пункт (a) противоречит тому, что написано в ToS.


Итог: за пару лет (или сколько там прошло с того момента, как я удалил свой аккаунт) ничего не поменялось — условия использования Dropbox всё так же противоречивы, как и раньше. Я не призываю вас удалять свои аккаунты, но советую задуматься над тем, какие права вы даёте людям, хранящим ваши файлы где-то за бугром.

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-10T10:23:56.281+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Щитаю, можно отбросить паранойю по поводу контактов дропбокса с властями, по крайней мере для России это совсем не актуально. Количество подобных запросов правоохранительных органов только недавно сдвинулось с нулевой отметки и все еще пренебрежительно мало. Напр.: http://www.google.com/transparencyreport/governmentrequests/RU/?p=2011-06


Но факт остается фактом: дропбокс занимает нишу наиболее массового облачного стореджа. Он по определению хочет быть самым удобным. Удобство для пользователя мало когда совместимо с безопасностью.Почти всегда приходится выбирать что-то одно. А параноидальные облачные сервисы для данных тоже есть, типа jungledisk или tarsnap.


Вопрос: дропбокс заменил rsync ― с чем? Что-то другое облачное или домашний сервер?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-10T13:42:45.651+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<i>&gt; Количество подобных запросов правоохранительных органов только недавно сдвинулось с нулевой отметки и все еще пренебрежительно мало</i>

Предлагаешь ждать дня, когда тебе дадут срок за сделанные тобой же для себя любимого mp3&#39;шки?

Касательно массовости ты абсолютно прав. Вот ещё недавно <a href="http://laforge.gnumonks.org/weblog/2011/06/11/" rel="nofollow">был пост про GMail</a> — тоже в тему.

<i>&gt; Вопрос: дропбокс заменил rsync ― с чем? Что-то другое облачное или домашний сервер?</i>

От Dropbox&#39;а мне нужна была только возможность синхронизировать две машинки, хранение в облаке — это просто фича. Сейчас изредка руками делаю rsync, этого хватает.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-11T02:38:55.777+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
&gt;Предлагаешь ждать дня, когда тебе дадут срок за сделанные тобой же для себя любимого mp3&#39;шки?

Я только предлагаю сравнить вероятность двух событий: 

― Менты отправят запрос в дропбокс, узнают о хранении там мп3 файлов, проведут экспертизу контента и придут задавать вопросы про легальность первода в мп3 купленного диска.

― Во время ночной прогулки менты остановят меня на улице и под каким-либо предлогом начнут вымогать взятку.

С первым я не сталкивался никогда, с другим ― один раз в жизни. И второе  событие мне представляется более вероятным примерно в 100500 раз. При этом я продолжаю выходить из дома.

Характерно, что по моей ссылке речь идет в том числе и о запросах связанных с ютюбом. Их тоже около ноля. 

Я тоже не пользуюсь дропбоксом. И даже заливая личные бекапы на Amazon S3 дополнительно шифрую файлы. И на гмейле держу переписку только за последний год-полтора, а остальное экспортирую в более безопасное пространство. 

Но, по-моему, это только игра. Это гиковский интерес к наиболее безопасным и совершенным технологиям, а не ответ реальным рискам.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-11T15:08:41.315+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
При регистрации нужно учитывать не только сегодняшние риски, но и завтрашние. Привыкнешь к Dropbox, станешь на него полагаться — а там внезапно появится какой-нибудь закон, по которому тебя быстренько и упакуют.

Кроме того, ты совершенно упускаешь из виду тот факт, что Dropxbox хранит данные в амазоновском облаке, которое наверняка где-то в США, а значит, подвергается юрисдикции Штатов. Так что тот факт, что в России всем на твои mp3&#39;шки плевать, вовсе не значит, что никто к тебе приколупываться не будет.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-11T18:28:05.510+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
1\. <br/>
&gt; Кроме того, ты совершенно упускаешь из виду тот факт, что Dropxbox хранит данные в амазоновском облаке, которое наверняка где-то в США, а значит, подвергается юрисдикции Штатов.

Увы, нет. Дело в том, что в юрисдикцию США входит весь интернет как таковой. По их законодательству, независимо от того где хранятся данные, откуда действует пользователь, если одна из сторон ― гражданин США, или юрлицо (правообладатель и его представители в т.ч.), зарегистрированное в США. то все действия должны соответствовать законам США. Кажется, подробнее об этом рассказывал Носик здесь: 
http://expert.ru/2011/11/16/ugol-zreniya/

Так что реальное расположение серверов значения не имеет. Но если оно тебя интересует ― при прямой работе  с Амазоном, тебе дается выбор страны, где будут находиться сервера. У меня это Сингапур, потому что Ли Куан Ю ― наше всё.

2\. <br/>
Про юрисдикцию это смешно, конечно. По твоей ссылке выше чувак говорит совершенно разумную вещь: не надо умножать сущности, хватит с нас германских бюрократов, давайте обойдемся без американских.

Если я попробую сказать, что российский хостинг более защищен от бюрократов, чинуш и оборотней чем американский ― никто мне не поверит, даже я сам. Аршином общим не измерить, чо.


3\. <br/>
&gt; При регистрации нужно учитывать не только сегодняшние риски, но и завтрашние.

Т.е. завтра у нас полиция начнет действовать эффективно и справедливо, а право собственности станет священным? Прекрасное завтра, скорей бы! 

Но и тогда мне нечего бояться, я и так не храню нелегальный контент, а необоснованных претензий ко мне не будет. Ведь полиция будет действовать эффективно и справедливо.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-11T19:06:29.052+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
С ссылкой соврал, не там он говорил, а в другом интервью, по поводу дела США против Ассанджа
http://echo.msk.ru/programs/oblozhka-1/729871-echo/

Но вообще на эту тему легко гуглятся и другие пруфы.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-11T23:12:27.234+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
1. О такой особенности их законодательства не знал — всегда думал, что соответствующие уточнения в EULA/ToS являются не более чем попыткой облегчить себе жизнь, перенеся возможные юридические разбирательства на знакомую территорию.

2. Мне в том посте больше приглянулась другая мысль: не следует централизировать данные.
</p>
</div>



