---
title: XF86KScreenSaver
published: 2012-02-15T22:30:00Z
categories: 
tags: linux
---

На пробельной клавише <a href='http://debiania.blogspot.com/2011/08/asus-p52f-380m-debian.html'>моего Asus P52F-380M</a> нарисован какой-то бегущий человечек (JFYI: клавиши у P52F не такие, как на фото, но это не важно):<div class="separator" style="clear: both; text-align: center;"><a href="http://1.bp.blogspot.com/-_gvT0x2QWP8/Tzwtn1azCgI/AAAAAAAAAlw/cSyDoQuR74U/s1600/asus-p52f-so003x.jpg" imageanchor="1" style="margin-left:1em; margin-right:1em"><img border="0" height="94" width="400" src="http://1.bp.blogspot.com/-_gvT0x2QWP8/Tzwtn1azCgI/AAAAAAAAAlw/cSyDoQuR74U/s400/asus-p52f-so003x.jpg" /></a></div><code>xev</code> услужливо подсказал, что в комбинации с Fn пробел генерирует специальный код — <code>XF86KScreenSaver</code>. Я немного параноик и постоянно блокирую клавиатуру с помощью <code>xtrlock</code>, запуская последний при помощи Xmonad'овского <a href='http://hackage.haskell.org/packages/archive/xmonad-contrib/0.10/doc/html/XMonad-Prompt-Shell.html#v:shellPrompt'><code>shellPrompt</code></a> (аналоги в других WM/DE/OS: Alt-R, WinKey-R), что, согласитесь, не верх удобства. В общем, забиндил я на этот код вызов <code>xtrlock</code> и нарадоваться не могу — теперь вместо Alt-P, xtr, &lt;Enter&gt; я просто жму Fn-Space и убегаю ☺ Жаль, что на нетбуке такой фичи нет…

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-16T19:59:44.350+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
А ctrl+alt+L в xmonad нету?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-16T22:16:01.192+02:00, cF8 wrote:</p>
<p class='hakyll-convert-comment-body'>
А я уже довольно давно использую для этого сочетание r_Super+Menu в dwm, жмется правой рукой двумя пальцами, очень удобно :3
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-16T22:39:19.571+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Анонимный</b>, по умолчанию — нет. Но это ж не Windows, всё настраиваемо.<br /><br /><b>cF8</b>, у меня и r_Super-то нету :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-19T10:15:26.626+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
&gt;  r_Super+Menu<br />а разве это не одна итаже клавиша &quot;Шиндовш&quot;?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-02-20T00:05:58.337+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>Анонимный</b>, нет. Menu — это клавиша открытия контекстного меню, находится справа от r_Super.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2012-03-29T11:30:24.518+03:00, imelnik wrote:</p>
<p class='hakyll-convert-comment-body'>
Ещё с виндовых времён привык к Super+L, на openbox так и оставил. Универсально и запоминается.
</p>
</div>



