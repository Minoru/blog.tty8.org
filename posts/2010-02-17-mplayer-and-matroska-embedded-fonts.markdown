---
title: MPlayer и Matroska embedded fonts
published: 2010-02-17T16:19:00Z
categories: 
tags: tips'n'tricks
description: Mplayer ожидает, что у шрифтов будет MIME-тип
    application/x-truetype-font или application/x-font, а mkvtoolnix использует
    application/x-ttf-font или application/octet-stream.
---

Сейчас собирал mkv'шки и потратил почти полчаса на поиски причины, по которой MPlayer не хотел отображать встроенные в файлы шрифты.

Оказалось, что при добавлении ttf фонта <code>mmg</code> 2.7.0-0.2 (пакет <code>mkvtoolnix-gui</code>; обратите внимание на версию — это пакет из Squeeze) автоматически прописывал им тип <code>application/x-ttf-font</code> (<code>mmg</code> в Lenny прописывает фонтам <code>application/octet-stream</code>). MPlayer такое MIME почему-то не хавает, ему нужно <code>application/x-truetype-font</code>.

Посмотрел спецификацию Матрёшки и ничего по теме не нашёл — наверное, за вложениями должен следить сам юзер.

Не знаю, чем <code>x-ttf-font</code> так сильно отличается от <code>x-truetype-font</code>, но пару файлов пришлось пересобрать.

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-17T21:38:36.232+02:00, koct9i wrote:</p>
<p class='hakyll-convert-comment-body'>
просто зоопарк<br/>
http://mx.gw.com/pipermail/file/2009/000400.html

а в mplayer захардкожено application/x-truetype-font и application/x-font
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-17T23:54:41.992+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
2 koct9i:<br/>
М, вот оно как — захардкоджено, значит. Учтём-с, спасибо!
</p>
</div>



