---
title: Как определить длительность DNS-запроса
published: 2009-12-14T00:44:00Z
categories: 
tags: linux,tips'n'tricks
---

После открытия <a href="http://code.google.com/speed/public-dns/">Google Public DNS</a> многие пользователи заметно озаботились ускорением своих DNS запросов. При этом лишь немногие задают себе вопрос — «а оно мне надо?», — что не может не огорчать. Не спорю, иногда гугловский днс нужен не столько для скорости, сколько для стабильности (у моего бывшего провайдера заметная часть «поломок» интернета составляли именно падения DNS), но я также уверен, что в большинстве случаев у провайдера есть отличный кеширующий прокси (который, к тому же, по-любому ближе к Вам, чем любой внешний DNS). Но если вам всё же хочется определить целесообразность перехода на другой DNS, прочтите эту заметку.<a name='more'></a><br /><br />Недавно ко мне по RSS прилетела ссылочка на статью под названием <a href="http://www.webupd8.org/2009/12/faster-browsing-in-linux-with-local-dns.html">«Faster Browsing In Linux With Local DNS Cache»</a>. Материал сам по себе довольно интересный, но я в первую очередь почерпнул оттуда название замечательной утилитки <code>dig</code>, способной показать время DNS запроса. В Debian утилита эта расположена в пакете <code>dnsutils</code>, который совсем нетрудно поставить штатными средствами дистрибутива:<br /><pre class="code">sudo aptitude install dnsutils</pre>После этого говорим нечто вроде<pre class="code">dig google.com</pre>и наблюдаем результат:<pre class="code">; <<>> DiG 9.7.0-P1 <<>> google.com<br />;; global options: +cmd<br />;; Got answer:<br />;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 28785<br />;; flags: qr rd ra; QUERY: 1, ANSWER: 6, AUTHORITY: 4, ADDITIONAL: 4<br /><br />;; QUESTION SECTION:<br />;google.com.   IN A<br /><br />;; ANSWER SECTION:<br />google.com.  168 IN A 74.125.87.103<br />google.com.  168 IN A 74.125.87.147<br />google.com.  168 IN A 74.125.87.99<br />google.com.  168 IN A 74.125.87.104<br />google.com.  168 IN A 74.125.87.105<br />google.com.  168 IN A 74.125.87.106<br /><br />;; AUTHORITY SECTION:<br />google.com.  110951 IN NS ns3.google.com.<br />google.com.  110951 IN NS ns1.google.com.<br />google.com.  110951 IN NS ns2.google.com.<br />google.com.  110951 IN NS ns4.google.com.<br /><br />;; ADDITIONAL SECTION:<br />ns3.google.com.  107156 IN A 216.239.36.10<br />ns1.google.com.  107156 IN A 216.239.32.10<br />ns4.google.com.  107156 IN A 216.239.38.10<br />ns2.google.com.  107156 IN A 216.239.34.10<br /><br />;; Query time: 8 msec<br />;; SERVER: 195.95.171.2#53(195.95.171.2)<br />;; WHEN: Mon May  3 17:10:01 2010<br />;; MSG SIZE  rcvd: 260</pre>Основной интерес представляет строка Query time (можно дописать после <code>dig</code> нечто вроде <code>| grep "Query time"</code>, чтобы не засорять терминал лишним выводом) — из неё-то мы и узнаём время, затраченное на наш DNS запрос. Итак, теперь каждый из нас может объективно сравнить пользу от перехода на Google Public DNS (или любой другой DNS сервис) — достаточно просто прогнать <code>dig</code> пару сот раз и посчитать среднее время:<pre class="code">x=0; server=8.8.8.8; host="yandex.ru"; queries=128; for i in `seq $queries`; do \<br />let x+=`dig @${server} $host | grep "Query time" | cut -f 4 -d " "`; \<br />done && echo "scale=3;($x/${queries})" | bc</pre>В переменной <code>server</code> — IP (хотя можно и имя, но это наверняка повлияет на результат теста); в <code>host</code> — проверяемый хост; <code>queries</code> — количество запросов (чем больше, тем лучше, естественно).<br /><br />Спокойной ночи! :)

<h3 id='hakyll-convert-comments-title'>Comments</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-14T08:52:11.394+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Надо в начале обнулить переменную Х, а то результат суммируется.<br />[code]x=0; server=8.8.8.8; host=&quot;yandex.ru&quot;; queries=128; for i in `seq $queries`; do let x+=`dig @${server} $host | grep &quot;Query time&quot; | cut -f 4 -d &quot; &quot;`; done &amp;&amp; echo &quot;scale=3;($x/${queries})&quot; | bc[/code]<br /><br />Андрей
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-14T12:26:29.127+02:00, puzan wrote:</p>
<p class='hakyll-convert-comment-body'>
А в gentoo dig в каком пакете никто не знает?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-14T14:33:48.012+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Андрей комментирует:<br /><br />&gt;Надо в начале обнулить переменную Х, а то результат суммируется.</b><br /><br />Да, точно. Спасибо, исправлено!<br /><br /><br /><b><a href="http://www.blogger.com/profile/10819950786268963683" rel="nofollow">puzan</a> комментирует:<br /><br />&gt;А в gentoo dig в каком пакете никто не знает?</b><br /><br />Gentoo-Portage Search <a href="http://gentoo-portage.com/Search?search=dig" rel="nofollow">говорит</a>, что в <a href="http://gentoo-portage.com/net-dns/bind-tools" rel="nofollow">net-dns/bind-tools</a>
</p>
</div>



