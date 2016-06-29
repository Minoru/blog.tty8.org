---
title: MPD и плавный переход между песнями
published: 2009-05-18T06:48:00Z
categories: 
tags: linux
---

MPD — очень мощный плеер, и не мудрено, что он обладает возможностью плавного перехода между песнями. Но не всем это нравится, а потому сия краткая заметочка раскажет вам, как эту функцию отключить :)

Цитируя [«Music Player Daemon Crossfading Troubleshooting»](http://mpd.wikia.com/wiki/Music_Player_Daemon_Crossfading_Troubleshooting):

> MPD does crossfading by using the buffer. The amount of buffer used for
> crossfading is the size of the buffer minus the amount of space reserved for
> `buffer_before_play`. By default (2MB buffer and 25% `buffer_before_play`)
> this is approximately 9 seconds of crossfading for 44.1 khz, 16-bit, stereo
> audio. **If you have the `buffer_before_play` set to 100%, there will be no
> crossfading.** If you'd like more crossfading than the default buffer
> settings permit, increase the `buffer_size` and/or decrease
> `buffer_before_play` in your config file (a sample config file is included
> with the MPD source and in the man page). Note that setting the crossfade
> amount only sets the maximum amount of crossfading, it does not guarantee
> that much crossfading will be performed. 

Решение я выделил полужирным. В переводе на русский:<ol><li>открываем файл <code>/etc/mpd.conf</code> (или тот <code>mpd.conf</code>, который вы создали у себя в домашнем каталоге) в своём любимом редакторе;</li><li>находим строку <code>buffer_before_play "0%"</code> (она может быть закомментирована, то есть содержать знак «#» в начале; в таком случае её надо раскомментировать) и меняем 0% на 100%;</li><li>перезагружаем MPD. Это можно сделать вот так:
```
mpd --kill && mpd
```
или так:
```
/etc/init.d/mpd restart
```
</li></ol>Вуаля, у нас больше нет плавного перехода!

Эта заметка также была размещена на <a href="http://welinux.ru/post/811">welinux.ru</a>

P.S. Недавно правил плейлист в ncmpc и случайно жмакнул по «x». Увидев сообщение «Кроссфейд 0 секунд», вдруг прозрел — кроссфейдинг можно отключить из клиента :)

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-21T17:28:56.654+03:00, tux wrote:</p>
<p class='hakyll-convert-comment-body'>
Гениально))))
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-21T19:30:06.373+03:00, Programmaster wrote:</p>
<p class='hakyll-convert-comment-body'>
<B>tux</B>, а чем вызвана такая реакция? Столкнувшись с этим самым плавным переходом, я сразу заинтересовался, как же его отключить, но русские интернеты пожлобились на ответ, так что пришлось гуглить на английском. Найдя рецепт (и применив его на практике), записал, дабы самому не забыть и другим помочь. Даже на welinux.ru кросспост сделал (он теперь первый в выдаче гугла по «mpd плавный переход», а мой блог хрен знает где =\ Будь проклят nofollow в постах!).

Так чем же вызвана такая реакция?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2009-05-22T12:04:22.673+03:00, tux wrote:</p>
<p class='hakyll-convert-comment-body'>
Собсно мне тоже не нравится фейд между треками.
</p>
</div>



