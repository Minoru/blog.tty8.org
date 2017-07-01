---
title: Скриншоты в PDF с GTK 3
published: 2011-06-20T22:01:00Z
categories: 
tags: programming,linux
description: Перевод поста о том, как с помощью специального модуля для GTK3
    делать векторные скриншоты окон.
---

Для тех, кто не читает <a href='http://planet.debian.org/'>Planet Debian</a>, маленький итог из двух тамошних постов.

<a href='http://www.joachim-breitner.de/'>Joachim Breitner</a> разработал небольшой модуль, который, будучи загруженным вместе с GTK3-приложением, отображает кнопку, с помощью которой можно создать скриншот окна и сохранить его в PDF, SVG, PostScript или даже PNG. Преимуществом таких скриншотов является то, что они представляют собой векторную графику, а значит:

* текст на них индексируем и по нему можно производить поиск
* они идеально масштабируются, а значит, электронные мануалы с их использованием выглядят привлекательно вне зависимости от разрешения экрана и размеров окна вьювера

На пример такого скриншота можно поглядеть в <a href='http://www.joachim-breitner.de/various/pdf_screenshot_3.pdf'>этой pdf'ке</a>, а <a href='http://www.joachim-breitner.de/various/pdf_screenshot_3.ogv'>здесь</a> можно скачать скринкаст (Ogg Theora, 2Mb), в котором показан процесс её создания. Архивы с исходниками можно скачать <a href='http://www.joachim-breitner.de/archive/gtk-vector-screenshot/'>здесь</a>, а репозиторий — <a href='https://gitorious.org/gtk-vector-screenshot'>найти на gitorious.org</a>.

Собственно, те самые два поста, в которых Joachim описывает свой модуль:<ol><li><a href='http://www.joachim-breitner.de/blog/archives/494-Better-PDF-screenshots-with-gtk-3.html'>Better PDF screenshots with gtk 3</a></li><li><a href='https://www.joachim-breitner.de/blog/archives/502-gtk-vector-screenshot-code-published.html'>gtk-vector-screenshot code published</a></li></ol>
