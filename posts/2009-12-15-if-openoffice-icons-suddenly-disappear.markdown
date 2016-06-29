---
title: Если вдруг исчезли иконки в OpenOffice…
published: 2009-12-15T20:32:00Z
categories: 
tags: linux,tips'n'tricks
---

…то не горюйте, а просто переустановите все <code>openoffice.org-style</code>-пакеты:
```
% aptitude search openoffice.org-style-
i A openoffice.org-style-andromeda       - Default symbol style for OpenOffice.org
i A openoffice.org-style-crystal         - Crystal symbol style for OpenOffice.org
v   openoffice.org-style-default         -
p   openoffice.org-style-hicontrast      - Hicontrast symbol style for OpenOffice.org
p   openoffice.org-style-industrial      - Industrial symbol style for OpenOffice.org
i A openoffice.org-style-tango           - Tango symbol style for OpenOffice.org
```
Дальше шуруем в <i>Tools — Options — View</i> (<i>Сервис — Параметры — Вид</i>) и в соответствующих выпадающих списках выбираем размер и стиль иконок. В Debian по умолчанию должно быть <i>Large</i> (<i>Большие</i>) и <i>Industrial</i>.

Решение стянуто <a href="http://ubuntuforums.org/showthread.php?t=528369">отсюда</a>.

P.S. Если верить интернетам, иконки пропали из-за смены темы — я, например, недавно игрался темами GTK…

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-16T05:59:36.744+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
&gt;Решение стянуто отсюда.<br/>
А ссыорчки то нет. ;)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-12-16T17:19:24.840+02:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Анонимный комментирует... 

&gt;А ссыорчки то нет. ;)</b><br/>
Да, банально опечатался в «href» — написал «hef». Спасибо за ответ!
</p>
</div>



