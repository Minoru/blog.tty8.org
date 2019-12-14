---
title: Пропуск проверки ФС при загрузке с CTRL-C
published: 2010-08-06T21:34:00Z
categories: 
tags: linux,debian
description: Добавьте в /etc/e2fsck.conf в секцию options строчку
    allow_cancellation=true.
---

Эта маленькая заметка — перевод <a href='http://www.lucas-nussbaum.net/blog/?p=511'>поста Лукаса Нуссбаума (Lucas Hussbaum)</a>.

Согласно закону Мерфи, проверка <code>fsck</code>, происходящая каждые n загрузок, всегда случается в самое неподходящее время. По умолчанию, прерывание проверки с помощью <code>CTRL-C</code> заставляет <code>fsck</code> возвращать код ошибки, что приводит к перемонтированию файловой системы в режиме «только чтение». Но это легко меняется правкой <code>/etc/e2fsck.conf</code>:
```
[options]
allow_cancellation = true
```
