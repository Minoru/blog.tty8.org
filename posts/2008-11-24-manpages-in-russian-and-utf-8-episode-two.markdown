---
title: Русские маны и UTF-8: эпизод второй
published: 2008-11-24T05:00:00Z
categories: 
tags: linux
---

Помните, несколько месяцев назад я <a href="http://debiania.blogspot.com/2008/08/utf-8.html" target="_blank">писал о своих проблемах с русскими манами</a> (ссылка откроется в новом окне/вкладке)? Тогда я удалил <code>manpages-ru</code> и на некоторое время забыл о существовании манов вообще (просто не приходилось их читать, т.к. ничего не настраивал). Сегодня же я опишу гораздо более элегантное и правильное решение :)<br /><a name='more'></a><br /><i><b>Примечание:</b> очень большую &mdash; по сути, главную &mdash; роль в решении проблемы сыграл Assaron с канала #linux в TURLINet, за что ему большое человеческое спасибо!</i><br /><br />Итак, решение более чем тривиально: нужно поставить less<br /><div class="code"><code>sudo aptitude install less</code></div>а потом сделать его умолчальным pager'ом (листалкой вывода, который не помещается на экран)<br /><div class="code"><code>sudo update-alternatives --config pager</code></div><br />Вот, собственно, и весь рецепт :) Перелогинтесь в систему, чтобы изменения вступили в силу, и читайте маны!<br /><br /><a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://4.bp.blogspot.com/_Nb6QYFUvBjY/SSltgfh3kmI/AAAAAAAAABI/Jp72SKCeAck/s1600-h/man_chown_correct.png" target="_blank"><img style="margin: 0px auto 10px; display: block; text-align: center; cursor: pointer; width: 320px; height: 226px;" src="http://4.bp.blogspot.com/_Nb6QYFUvBjY/SSltgfh3kmI/AAAAAAAAABI/Jp72SKCeAck/s320/man_chown_correct.png" alt="" id="BLOGGER_PHOTO_ID_5271865243800670818" border="0" /></a><br /><br />Удачи ;)

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>


