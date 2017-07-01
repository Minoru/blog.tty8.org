---
title: Fluxbox и аппаратные горячие клавиши
published: 2009-03-08T15:38:00Z
categories: 
tags: hardware,fluxbox
description: Обрабатываем мультимедийные клавиши клавиатуры средствами Fluxbox.
---

Итак, имеется клавиатура [A4Tech KL-7MU](http://www.a4tech.com/ENNEW/product.asp?cid=2&scid=101&id=279) (с рядом горячих клавиш, выполненных в виде отдельных кнопок) и Debian GNU/Linux с Fluxbox'ом в качестве менеджера окон. Давайте-ка заставим горячие кнопки делать то, что им положено — к примеру, управлять MPD.

Собственно, управление сводится к добавлению в ~/.fluxbox/keys строк вида:
```
None      ИмяКнопки      :ExecCommand mpc действие
```
Как видите, совершенно ничего сложного. Осталось только определится с именами кнопок и действиями для mpc.

Итак, какие в иксах есть кнопки для управления звуком:
```
Имя кнопки             Производимое действие
XF86AudioPrev          Предыдущий трек
XF86AudioNext          Следующий трек
XF86AudioPlay          Играть/пауза
XF86AudioStop          Остановить воспроизведение
XF86AudioRaiseVolume   Повысить громкость
XF86AudioLowerVolume   Понизить громкость
```
Всё, что нам нужно сделать — объяснить X.Org'у, где какие клавиши. Клавиши компьютер воспринимает по так называемым кэйкодам (keycode). Для того, чтобы их определить, надо открыть терминал и запустить программу xev. Появится маленькое квадратное окно — именно оно нам и нужно. Не переключаясь на другие окна, поочерёдно нажимайте горячие клавиши и записывайте из кэйкоды (в моём примере вывода они выделены полужирным):
<pre><code>KeyPress event, serial 32, synthetic NO, window 0x2200001,
root 0x87, subw 0x0, time 12449423, (-155,433), root:(584,456),
state 0x10, <b>keycode 234</b> (keysym 0x0, <font color="green">NoSymbol</font>), same_screen YES,
XLookupString gives 0 bytes: 
XmbLookupString gives 0 bytes: 
XFilterEvent returns: False

KeyRelease event, serial 35, synthetic NO, window 0x2200001,
root 0x87, subw 0x0, time 12449548, (-155,433), root:(584,456),
state 0x10, <b>keycode 234</b> (keysym 0x0, <font color="green">NoSymbol</font>), same_screen YES,
XLookupString gives 0 bytes: 
XFilterEvent returns: False</code></pre>
Обратите внимание на то, что выделено зелёным — важно, чтобы там было именно NoSymbol. Если там какое-то другое значение, значит этой кнопке уже назначено имя. В таком случае не записывайте код, запишите имя — и пропустите следующий шаг, где мы будем модифицировать .Xmodmap. Хотя для надёжности наблюдайте, чтобы имя совпадало со значением кнопки — и, если оно не совпадает, записывайте кэйкод, чтобы переназначить имя.

Ну что же, у нас есть кэйкоды — теперь будем назначать имена.

Для этого создадим и отредактируем файл ~/.Xmodmap. Он должен выглядеть примерно так:
```
keycode 144 = XF86AudioPrev
keycode 153 = XF86AudioNext
keycode 164 = XF86AudioStop
keycode 162 = XF86AudioPlay
keycode 176 = XF86AudioRaiseVolume
keycode 174 = XF86AudioLowerVolume
keycode 160 = XF86AudioMute
```
Для того, чтобы сразу применить созданную схему, наберите:
```
xmodmap ~/.Xmodmap
```
Итого, осталась самая малость — прописать хоткеи в конфиге Fluxbox'а.
Делается это, естественно, в ~/fluxbox/keys.

<i><b>Маленькое замечание:</b> перед прописыванием команд в конфиг вам нужно выяснить, как управлять вашим плеером командами из консоли. У меня MPD, так что я буду использовать mpc. Если у вас amaroK, поглядите в сторону dcop. По остальным плеерам советовать не берусь — гуглите.</i>
Я не буду долго объяснять, что да как писать — просто продемонстрирую свой участок конфига:
```
None    XF86AudioPrev        :ExecCommand mpc prev
None    XF86AudioPlay        :ExecCommand mpc toggle
None    XF86AudioStop        :ExecCommand mpc stop
None    XF86AudioNext        :ExecCommand mpc next
None    XF86AudioRaiseVolume :ExecCommand mpc volume +5
None    XF86AudioLowerVolume :ExecCommand mpc volume -5
Control XF86AudioPrev        :ExecCommand mpc seek -00:00:05
Control XF86AudioNext        :ExecCommand mpc seek +00:00:05
```
Пробуйте. Удачи! ;)
<i>P.S. Спасибо Bob R.'у за идею с Ctrl+кнопка — очень удобно :)</i>
