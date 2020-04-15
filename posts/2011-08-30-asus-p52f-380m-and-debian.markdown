---
title: Asus P52F-380M и Debian
published: 2011-08-30T20:34:00Z
categories: 
tags: hardware,debian, linux
description: Отчёт о том, как ведёт себя Wheezy на ноутбуке Asus P52F-380M.
---

Недавно я обзавёлся новой игрушкой — Asus P52F-380M. В общем и целом ноут хороший, но некоторые моменты вызывают раздражение. Например, клавиатура немного прогибается (в прямом смысле слова), но когда привыкаешь и начинаешь набирать, не глядя (я, к стыду своему, всё ещё не обучился слепому набору), этот недостаток перестаёт волновать. Гораздо больше раздражает идиотское расположение стрелочек:

<div class="center">
<img src="/images/stupid_arrow_keys_design_ASUS_P52F-380.jpg"
    width="588px" height="400px"
    alt="Глупое расположение стрелочек на Asus P52F-380"
    class="bleed" />
</div>

То есть кнопка «вправо» находится в области, занимаемой numpad'ом, что, поверьте мне, очень неудобно (в основном из-за огромного RgtCtrl, который я постоянно нажимаю вместо стрелочки влево). Даже на EeePC сделали лучше:

<div class="center">
<img src="/images/arrow_keys_design_ASUS_EeePC_900HA.jpg"
    width="481px" height="400px"
    alt="Расположение стрелочек на Asus EeePC-900HA"
    class="bleed" />
</div>

Думаю, теперь я таки перейду на hjkl в vim'е ☺

В остальном дизайн и сборка, к счастью, претензий не вызывают, так что перейдём наконец к теме поста — установке и последующей настройке моего любимого Debian.

Замечу, что в данном ноутбуке используется процессор Intel Core i3, а значит, лучше всего использовать Debian, собранный под amd64. Надеюсь, читатели этого блога в состоянии сами установить систему (если нет — <a href='http://mydebianblog.blogspot.com/2006/08/in-true-debian-way.html'>замечательная инструкция virens'а вам в помощь</a>).

Сначала я попытался поставить Squeeze, но после установки наткнулся на неприятный баг: внешние колонки ноутбука продолжают играть даже когда воткнуты наушники. Мириться с этим никак нельзя, тем более что в LiveCD Ubuntu 10.10, оказавшемся под рукой, всё работало. Признаюсь, я не стал долго ковыряться и просто поставил Wheezy, в котором, к счастью, всё работало из коробки.

Впрочем, даже в Wheezy с ходу не заработали некоторые вещи, в частности:

<ul><li>веб-камера выдавала перевёрнутое изображение;</li><li><code>wicd-curses</code> не видел мою Wi-Fi-точку;</li><li>из Fn-клавиш работали только три: Fn+F5/Fn+F6, регулирующие яркость монитора, и Fn+F7, которая монитор выключала;</li><li><code>pm-suspend</code> и <code>pm-hibernate</code> вроде как работали, но не выключали машину;</li><li>противно пищал PC speaker (он не настоящий, конечно же, просто эмуляция).</li></ul>

Карт-ридер я на работоспособность не проверял из-за отсутствия подходящих карточек, но наличие в выводе <code>lspci</code> SD Bridge внушает надежду.

Ну-с, проблемы перечислены — вперёд, на барикады! ☺

К счастью, для камеры и suspend'а решения приведены в <a href='http://www.linlap.com/wiki/asus+p52f'>Linux Laptop Wiki</a>. Для полноты поста я размещу здесь вольный пересказ этих рецептов.

<h3>Веб-камера</h3>

Починить это раз и навсегда (слегка поправив какой-нибудь конфиг), к сожалению, не получится. Единственный выход — все приложения, использующие веб-камеру, запускать с библиотекой <code>v4l1compat.so</code>. Для начала ставим соответствующий пакетик:
```
$ sudo aptitude install libv4l-0
```

После этого все приложения, использующие веб-камеру, следует запускать следующим образом (на примере MPlayer'а):
```
LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libv4l/v4l1compat.so mplayer tv://
```
Для удобства можно создать алиас — библиотека не нарушает работоспособность программы, так что можно подгружать её даже в тех случаях, когда веб-камера вам не нужна.

<h3>Suspend и hibernate</h3>

Как гласит Linux Laptop Wiki, питание не выключается из-за неотключенных USB-устройств. Чинится всё написанием скриптов, которые перед засыпанием отключают устройства, а потом подключают обратно, но тут всплывает неприятный баг: машинка успешно засыпает и просыпается, но в точках монтирования флешек становится пусто. Почему так и как это исправить, я ещё не понял. Stay tuned.

В общем, сделать нужно следующее:

1. ставим любимый пакетик для suspend/hibernate. Я пользуюсь <code>pm-utils</code>:

    ```
    $ sudo aptitude install pm-utils
    ```

2. создаём конфиг с модулями, которые нужно выгружать перед suspend'ом:

    ```
    $ sudoedit /etc/pm/config.d/modules
    ```

    и пихаем в него вот что:

    ```
    SUSPEND_MODULES="usb_storage mac80211 cfg80211 btusb bluetooth ahci libahci sdhci_pci sdhci led_class ath9k ath9k_common ath rfcomm sco bnep l2cap asus_laptop ehci_hcd"
    ```

    Не забываем разрешить утилитам читать его:

    ```
    $ sudo chmod a+r /etc/pm/config/d/modules
    ```

3. создаём скриптик, который будет отключать и подключать устройства:

    ```
    $ sudoedit /etc/pm/sleep.d/10_unbindusb
    ```

    и помещаем в него следующее:

    ```bash
    #!/bin/bash
    function unbind_usb {
       for driver in ehci ohci uhci; do
           cd "/sys/bus/pci/drivers/${driver}_hcd";
           ids=$(ls | grep :);
           echo $ids > /tmp/DISABLED_$driver;
           for id in $ids; do
               echo "Unbinding $id";
               echo -n "$id" > unbind;
               disabled="$disabled $id";
           done;
       done;
    }
    function bind_usb {
       for driver in ehci ohci uhci; do
           cd "/sys/bus/pci/drivers/${driver}_hcd";
           for id in $(cat /tmp/DISABLED_$driver); do
               echo "Binding $id";
               echo -n "$id" > bind;
           done;
           rm /tmp/DISABLED_$driver;
       done;
    }
    case "$1" in
       hibernate|suspend)
           unbind_usb;
       ;;
       thaw|resume)
           bind_usb;
           unbind_usb;
           bind_usb;
       ;;
       *)
           exit 1;
       ;;
    esac;
    exit 0;
    ```

4. даём скрипту необходимые права:

    ```
    $ sudo chmod u=rwx,go=rx /etc/pm/sleep.d/10_unbindusb
    ```

Теперь можно спокойно ложить машинку в suspend командой <code>pm-suspend</code> (от рута, конечно же). Не забывайте также о hibernate и suspend-hybrid.

Если после просыпания у вас не определяются USB-устройства, перезагрузите модуль <code>ehci_hcd</code>:
```
$ sudo rmmod ehci_hcd
$ sudo modprobe ehci_hcd
```

<b>UPD 14.02.2012:</b> даже после вышеприведённых инструкций машинка иногда не просыпалась (точнее, просыпалась, но на этапе загрузки образа уходила в ребут). Только что поставил <code>linux-image-3.2.0-1-amd64</code> — шесть циклов hibernate пройдены на «ура» (причём в одном из случаев система засыпала от AC, а просыпалась от аккумулятора; в ещё одном случае было наоборот; во всех остальных — AC).

<h3>Wi-Fi</h3>

Беспроводная сеть была чуть ли не самой простой проблемой из всех. Следать нужно следующее:

1. поставить менеджер сетевых соединений wicd (помимо curses-клиента существуют также gtk и cli):

    ```
    $ sudo aptitude install wicd wicd-curses
    ```

2. прописать в <code>/etc/network/interfaces</code> беспроводной интерфейс:

    ```
    $ sudoedit /etc/network/interfaces
    ```

    В конец файла нужно дописать следующее:

    ```
    allow-hotplug wlan0
    iface wlan0 inet manual
    ```
3. прописать <code>wlan0</code> в настройках wicd, для чего в curses-интерфейсе (запускаемом с помощью команды <code>wicd-curses</code>) нужно нажать P (это большая английская буква P) и тут же в поле Wireless Interface написать заветное «wlan0» (без кавычек)

4. жмакнуть F10, чтобы сохранить настройки

После этих нехитрых манипуляций ноутбук начинает-таки видеть беспроводные точки доступа.

Кстати говоря, Fn+F2 работает из коробки, только лампочка на передней панели, показывающая состояние Wi-Fi, не работает. Меня, впрочем, это мало волнует, поэтому я (пока что) не разбирался, почему так.

<b>UPD</b>: за лампочки отвечает модуль <code>asus_laptop</code>. Не знаю, что там у меня случилось во время написания статьи, но сейчас, спустя неделю, модуль загружается сам, так что лампочки в порядке.

<h3>Fn-клавиши</h3>Сразу после установки работали только клавиши, регулирующие яркость монитора, и Fn+F2, отключающая/включающая Wi-Fi. Мне очень сильно не хватало возможности регулировать громкость звука (и вообще выключать его), так что пришлось отлавливать события acpi и писать обработчики. На самом деле это не так сложно, как может показаться. Следуйте инструкции:

1. создаём файлы, задающие события, на которые мы хотим реагировать (надеюсь, вы уже запомнили команду <code>sudoedit</code>, потому что на этот раз я просто покажу содержимое файлов, не расписывая процесс их создания):

    ```
    $ cat /etc/acpi/events/volume_mute
    event=hotkey ATK0100:00 00000032
    action=/etc/acpi/volume_mute.sh
    ```

    ```
    $ cat /etc/acpi/events/volume_up
    event=hotkey ATK0100:00 00000030
    action=/etc/acpi/volume_up.sh
    ```

    ```
    $ cat /etc/acpi/events/volume_down
    event=hotkey ATK0100:00 00000031
    action=/etc/acpi/volume_down.sh
    ```

    Если какой-то из Fn-хоткеев у вас работать не будет, запустите <code>acpi_listen</code>, понажимайте хоткей и посмотрите, соответствует ли вывод строке «event» (обратите внимание, что acpi присваивает событиям последовательные номера — их включать в строку event <b>нельзя</b>)

2. создаём обработчики:

    ```
    $ cat /etc/acpi/volume_mute.sh
    #!/bin/sh
    amixer sset 'Master' toggle
    ```

    ```
    $ cat /etc/acpi/volume_up.sh
    #!/bin/sh
    amixer sset 'Master' 2+
    ```

    ```
    $ cat /etc/acpi/volume_down.sh
    #/bin/sh
    amixer sset 'Master' 2-
    ```

3. заставляем <code>acpid</code> перечитать настройки:

    ```
    $ sudo pkill -SIGHUP acpid
    ```

<h3>PC speaker</h3>«Пищалка» в данном ноуте детская, не настоящая даже — вместо взрослого PC speaker'а ноутбук пищит колонками. Отключается это тоже по-детски: достаточно открыть <code>alsamixer</code> (выполнив в терминале одноимённую команду), выбрать ползунок Beep и выключить его (нажав m).

<hr />

Как видим, ни одной серьёзной проблемы — все вещи настраиваются парой изменений в конфигах. И кто после этого посмеет сказать, что Linux не готов для потребителя? ☺

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-09-05T14:50:26.971+03:00, GalS wrote:</p>
<p class='hakyll-convert-comment-body'>
ссылка на инструкцию virens&#39;а - на редактирование поста?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-09-05T15:28:56.227+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Упс… Спасибо, <b>GalS</b> — пофикшено!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-09-06T07:57:36.590+03:00, goblinyara wrote:</p>
<p class='hakyll-convert-comment-body'>
&gt;&gt;Правда, в TTY спикер все равно будет эмулироваться — как это побороть, я не знаю. 

alsamixer, там будет PCM, убавляете в ноль, и не надо шаманства с xset=)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-09-06T13:05:52.938+03:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>goblinyara</b>, PCM — это, так сказать, «общесистемный» регулятор. Если убрать его в ноль, звук пропадёт во всей системе. Но все равно спасибо — пока проверял PCM, заметил Beep. Mute&#39;им этот регулятор — и «пищалка» замолкает даже в TTY. Спасибо!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2011-09-11T03:17:01.243+03:00, virens wrote:</p>
<p class='hakyll-convert-comment-body'>
Знатный пост, баянщики и графоманы одобряют :-)

Про acpi_listen - это не панацея, надо сказать. У меня на ноутбуке оно вообще нифига не слышит (никаких выхлопов на нажатие спецклавиш). Тем не менее, Яркость, ВайФай и переключение на внешний монитор работают. <a href="http://mydebianblog.blogspot.com/2007/09/acpi-intel-prowireless-asus-m5200ae.html" rel="nofollow">Ни разу не из коробки</a>, между прочим.

Про вайфай - тебе крупно повезло, отец, что в ядре поддерживается radio frequency kill switch (rf_kill). Это была большая боль в пояснице, ибо найти это в конфигах ядра было делом непростым (см. ту же ссылку).

И да, спасибо за ссылку на мой старый пост по дебиану. Забавно, но это до сих пор один из самых посещаемых постов по статистике....
</p>
</div>



