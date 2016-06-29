---
title: VPN соединение в системе, установленной с Debian netinst
published: 2009-08-20T13:52:00Z
categories: 
tags: linux,debian
---

Недавно ставил Debian с netinst'а и столкнулся с тем, что VPN из установленной системы поднять невозможно по причине отсутствия необходимых пакетов. Поэтому всем, кто будет ставить Debian на машинки, получающие инет по VPN-соединению, советую залить на флешку <code>libpcap0.8</code>, <code>ppp</code> и <code>pptp-linux</code>. Версии пакетов, естественно, должны быть одинаковыми ;)

<b>UPDATE</b><br/>
Значит, поправочка: я банально лохонулся :)<br/>
Все необходимые пакеты на netinst'е присутствуют, просто из-за того, что при установке системы я vpn не конфигурировал, а делал это после, пакеты не были установлены. Я же, по наивности полагая, что у меня в системе поставлено всё, что было на диске, даже не подумал сделать <code>apt-cdrom add</code>… Так-то :)<br/>
Спасибо Alex_P за замечание!

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-09-18T19:24:04.407+03:00, duke wrote:</p>
<p class='hakyll-convert-comment-body'>
Можно кстати просто в /etc/apt/sources.list разкомментировать строчку с диском netinstall :)
</p>
</div>



