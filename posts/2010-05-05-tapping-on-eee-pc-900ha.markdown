---
title: Таппинг (tapping) на Eee PC 900HA
published: 2010-05-05T12:57:00Z
categories: 
tags: Eee PC,linux,debian
---

Недельки этак три назад у меня внезапно поломался таппинг — возможность совершать клики ударом по тачпаду. Благо, кнопки на тачпаде всё ещё работали, да и кликать приходилось только в браузере — менеджер окон, <code>dwm</code>, завязан на клавиатуру.<br /><br />Ох, сколько же времени я провёл в гугле, пытаясь найти хоть какую-то ниточку к решению…<br /><br />Сегодня же бродил по спискам рассылки Debian и внезпно это самое решение нашёл. Как ни странно, оно <a href="http://en.gentoo-wiki.com/wiki/Synaptics_Touchpad#Tapping_does_not_work_anymore">расположено в wiki Gentoo</a> :D<br /><a name='more'></a><br />Вкратце рецепт сводится к следующему действию: файл <code>/usr/share/X11/xorg.conf.d/50-synaptics.conf</code> следует привести к такому виду:<br /><div class="code">Section "InputClass"<br />        Identifier "touchpad catchall"<br />        Driver "synaptics"<br />        MatchIsTouchpad "on"<br />        Option      "TapButton1" "1"<br />        Option      "TapButton2" "2"<br />        Option      "TapButton3" "3"<br />EndSection</div>Ура! ☺

<h3 id='hakyll-convert-comments-title'>Comments</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-05-05T23:18:19.349+03:00, Михаил wrote:</p>
<p class='hakyll-convert-comment-body'>
Как-то также мучился, решил сделать через synclient.<br /><br />Прописал в ~/.bashrc<br />synclient TapButton1=1<br />synclient TapButton2=2<br />synclient TapButton3=3<br /><br />но ваш вариант выглядит лучше :)<br /><br />P.S. Своё решение тоже нарыл на форуме  gentoo :D
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-05-05T23:20:44.315+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
А, так значит synclient — это программка такая? А я как раз сегодня тупил, думая, что за опция такая интересная и куда её писать… :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-05-06T01:30:04.490+03:00, Михаил wrote:</p>
<p class='hakyll-convert-comment-body'>
Ага, через неё можно просмотреть остальные опции:<br />synclient -l<br />и собственно изменить их значения. Или добавить эти опции в /usr/share/X11/xorg.conf.d/50-synaptics.conf соответственно.
</p>
</div>



