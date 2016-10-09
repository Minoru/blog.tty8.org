---
title: "Русские маны и UTF-8: эпизод второй"
published: 2008-11-24T05:00:00Z
categories: 
tags: linux
---

Помните, несколько месяцев назад я [писал о своих проблемах с русскими манами](/posts/2008-08-09-manpages-in-russian-and-utf-8-my-solution.html)? Тогда я удалил <code>manpages-ru</code> и на некоторое время забыл о существовании манов вообще (просто не приходилось их читать, т.к. ничего не настраивал). Сегодня же я опишу гораздо более элегантное и правильное решение :)

<i><b>Примечание:</b> очень большую — по сути, главную &mdash; роль в решении проблемы сыграл Assaron с канала #linux в TURLINet, за что ему большое человеческое спасибо!</i>

Итак, решение более чем тривиально: нужно поставить less
```
sudo aptitude install less
```
а потом сделать его умолчальным pager'ом (листалкой вывода, который не помещается на экран)
```
sudo update-alternatives --config pager
```
Вот, собственно, и весь рецепт :) Перелогинтесь в систему, чтобы изменения вступили в силу, и читайте маны!

<div class="center">
<a href="/images/man-chown-correct.png">
<img src="/images/man-chown-correct-thumbnail.jpg"
    width="400px" height="282px"
    alt="man chown"
    class="bleed" />
</a>
</div>

Удачи ;)
