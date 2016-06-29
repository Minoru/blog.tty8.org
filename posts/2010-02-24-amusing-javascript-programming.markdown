---
title: Занимательное JavaScript программирование
published: 2010-02-24T15:49:00Z
categories: 
tags: programming
---

Привет!

Недавно мой сосед показал мне главную страничку ВКонтакта со счётчиком <strike>идиотов</strike>зарегистрированных пользователей и обратил внимание на то, что инет отключен, а счётчик работает дальше. Ясен пень, что реализовано сиё счастье с помощью JavaScript, потому и работает в оффлайне. Сосед получил разъяснения и успокоился, а у меня зачесались руки — стало интересно, как же работают такие счётчики.

Я взял код <a href="http://vkontakte.ru/">главной ВКонтакта</a>, а также <a href="http://gmail.com/">login-страницы GMail</a>, на которой присутствует счётчик предоставляемого пользователям места, и проанализировал используемый там код. Если вам всё ещё интересен весь этот бред — читайте дальше :)

Начнём с гугла.<br/>
Код:
```javascript
// Estimates of nanite storage generation over time.
var CP = [
 [ 1199433600000, 6283 ],
 [ 1224486000000, 7254 ],
 [ 2144908800000, 10996 ],
 [ 2147328000000, 43008 ],
 [ 46893711600000, Number.MAX_VALUE ]
];


function updateQuota() {
  if (!quota_elem) {
  return;
  }
  var now = (new Date()).getTime();
  var i;
  for (i = 0; i < CP.length; i++) {
    if (now < CP[i][0]) {
      break;
    }
  }
  if (i == 0) {
    setTimeout(updateQuota, 1000);
  } else if (i == CP.length) {
    quota_elem.innerHTML = CP[i - 1][1];
  } else {
    var ts = CP[i - 1][0];
    var bs = CP[i - 1][1];
    quota_elem.innerHTML = format(((now-ts) / (CP[i][0]-ts) * (CP[i][1]-bs)) + bs);
    setTimeout(updateQuota, 1000);
  } 
}
```
Теперь пошагово разберём процесс получения надписи вроде “Over 2757.272164 megabytes (and counting) of free storage”.

Итак, часть первая — массив CP. Комментарий любезно подсказывает, что в массиве этом содержится ничто иное, как прикидки на количество места, которое Гугл будет предоставлять в определённый момент времени. Дальше идёт несколько пар значений вида «таймштамп UNIX (в миллисекундах)»—«мегабайты».

Часть вторая — собственно функция, которая обновляет элемент с мегабайтами. Вот код, но уже с моими комментариями:
```javascript
function updateQuota() {
  // существует ли элемент, в который надо выводить мегабайты?
  if (!quota_elem) {
  // элемента нет — закругляемся
  return;
  }

  // получаем текущее время в виде UNIX timestamp в миллисекундах
  var now = (new Date()).getTime();

  // объявляем переменную для цикла
  var i;

  // перебираем все записи в CP
  for (i = 0; i < CP.length; i++) {
    // если текущий таймштамп меньше того, что в массиве — прерываем цикл
    if (now < CP[i][0]) {
      break;
    }
  }


  if (i == 0) {
    // если таймштамп, на котором прервались — первый в массиве, то…
    // вызвать данную функцию снова через секунду
    setTimeout(updateQuota, 1000);
    // по-русски это звучит как «ждать наступления февраля 2008-го года»
  } else if (i == CP.length) {
    // если вышеприведённое условие не сработало и
    // если все таймштампы в массиве меньше текущего времени, то
    // записать количество мегабайт из последней записи в массиве
    quota_elem.innerHTML = CP[i - 1][1];
    // это на случай, если код долго не будут апдейтить — тогда в качестве мегабайт будет принято
    // самое большое число, допустимое в JavaScript — это около 1.7976931348623157e+308
  } else {
    // если ни одно из вышеприведённых условий не сработало, то
    // переменной ts присвоить таймштамп последней записи,
    // а переменной bs — кол-во мегабайт, соответствующих этому таймштампу
    var ts = CP[i - 1][0];
    var bs = CP[i - 1][1];
    // обновить запись с помощью нехитрой математики:
    // now-ts — разность между текущим временем и используемым таймштампом
    // CP[i][0]-ts — это разность во времени между текущим и следующим таймштампами
    // CP[i][1]-bs — разность в мегабайтах между текущим и следующим таймштампами
    // ((now-ts) / (CP[i][0]-ts) * (CP[i][1]-bs)) — сколько дополнительного места начал
    //     давать Гугл относительно текущего таймштампа
    // +bs — относительную величину (прирост мегабайт) превращаем в абсолютную (сколько же
    //     мегабайт выдают теперь)
    quota_elem.innerHTML = format(((now-ts) / (CP[i][0]-ts) * (CP[i][1]-bs)) + bs);
    // выставляем таймер — через секунду опять обновить значение
    setTimeout(updateQuota, 1000);
  } 
}
```

Так-то. Теперь перейдём к ВКонтакту. Программисты Дурова, очевидно, очень любят математику, в особенности статистику и генерацию случайных чисел. Вот их код:
```javascript
var memCount = 63301534;
var memPerSec = 2.16273601705;
function updateCount() {
 next = -(1000 / memPerSec)*Math.log(Math.random());
 memCountString = '' + memCount;
 len = memCountString.length;
 memCountString = memCountString.substr(0,len-6)+'<span style="font-size:8px"> </span>'+memCountString.substr(len-6,3)+'<span style="font-size:8px"> </span>'+memCountString.substr(len-3,3);
 ge('memCount').innerHTML = memCountString;
 memCount = memCount + 1;
 setTimeout(updateCount, next);
}
```
Как видите, всё очень лаконично: сейчас насчитывается <code>memCount</code> пиплов, каждую секунду к ним присоединяется ещё <code>memPerSec</code> тел. Вот только незадача: каждую секунду добавлять по чуть более чем два человека некрасиво. Лучше уж почаще, но по одному целому. Т.к. люди несовершенны, они не регистрируются каждые, скажем, 500 милисекунд — это тоже надо учесть. Короче, пришлось им придумать специальную формулу, которую я подробно рассматриваю в коде ниже:
```javascript
// текущее количество пользователей Вконтакта
var memCount = 63301534;
// примерное количество регистрирующихся в секунду
var memPerSec = 2.16273601705;

// собственно функция, обновляющая счётчик
function updateCount() {
 // хитрая математическая формула, вычисляющая длинну промежутков между
 //     регистрациями
 // 1000 / memPerSec — среднее время между регистрациями
 // random возвращает дробное число от 0 до 1
 // логарифм используется для того, чтобы хотя бы иногда получать время
 //    _больше_ среднего
 // минус перед формулой компенсирует тот факт, что логарифм числа из
 //    диапазона 0..1 отрицателен
 next = -(1000 / memPerSec)*Math.log(Math.random());
 // конвертируем число в строку
 memCountString = '' + memCount;
 // вычисляем длинну строки, чтобы потом красиво разбить его на разряды
 len = memCountString.length;
 // собственно разбивка на разряды
 // жду миллиардного пользователя, на котором эта конструкция обломается и выдаст «1000 000 000»
 memCountString = memCountString.substr(0,len-6)+'<span style="font-size:8px"> </span>'+memCountString.substr(len-6,3)+'<span style="font-size:8px"> </span>'+memCountString.substr(len-3,3);
 // выводим данные
 ge('memCount').innerHTML = memCountString;
 // наконец, добавляем к счётчику свежезарегистрировавшегося пользователя
 memCount = memCount + 1;
 // выставляем таймер, чтобы через next миллисекунд обновить счётчик
 setTimeout(updateCount, next);
}
```

Не знаю, насколько полезно такое вот спонтанное и бессистемное ковыряние в коде, но мне это нравится. Надеюсь, я не один такой, и этот пост будет интересен кому-то ещё ;)

<h3 id='hakyll-convert-comments-title'>Comments (migrated from Blogger)</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-24T21:02:21.282+02:00, Kona-chan wrote:</p>
<p class='hakyll-convert-comment-body'>
Капитан разбушевался.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-24T21:07:53.796+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
и шо?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-24T21:50:10.517+02:00, AntiNarod wrote:</p>
<p class='hakyll-convert-comment-body'>
меня напрягает множество ЦП в коде
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-24T22:08:57.904+02:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
В описании дважды гугловский код FAIL!

Реквестирую качественное капитанство, так-то.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-24T23:12:45.461+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
Спасибо комментаторам за их замечания, исправил.

Ежели к «капитанству» было отнесено обилие комментариев или сам факт существования этого поста, то извиняйте — волен писать о том, о чём хочу. Может быть, какому-то начинающему web-дизайнеру будет интересно почитать, как такие счётчики делать.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-25T04:35:49.365+02:00, Alexey Romanenko wrote:</p>
<p class='hakyll-convert-comment-body'>
Почему Вы считаете, что программисты Дурова любят генерацию случайных чисел?
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-25T16:40:22.444+02:00, Minoru wrote:</p>
<p class='hakyll-convert-comment-body'>
<b>2 Alexey Romanenko:</b><br/>
Потому что мне кажется, что 2\*rand — более очевидное решение, нежели log(rand).

А вообще абзац перед кодом ВКонтакта нужно воспринимать шутливо — там ни капли злобы или наезда, клянусь.
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-02-25T17:08:25.931+02:00, Alexey Romanenko wrote:</p>
<p class='hakyll-convert-comment-body'>
не-не-не, я вовсе не защищаю ВК, просто там же все очевидно. Когда нужно показать статические данные, либо сложные мат формулы, либо аякс =)
</p>
</div>



