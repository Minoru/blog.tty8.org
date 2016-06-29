---
title: Окна в vim
published: 2010-04-16T23:35:00Z
categories: 
tags: vim
---

Я никогда не пробовал работать с окнами в vim, потому что ни на моём десктопном монике (17", 1280x1024), ни на нетбуке (8,9", 1024x600) они не дают никаких преимуществ — места и так довольно мало. Но неделю назад мне довелось пару дней поработать на ноуте с широкоформатным экраном, и там несколько файлов в разных окнах выглядят просто замечательно!<br /><br />Вообще-то тема окон в vim не является чем-то очень новым или сложным — основную инфу можно легко нагуглить или получить, набрав в vim'е <code>:help window</code> — но я всё же решил написать эту заметку: и себе на память, и потому, что <a href="http://welinux.ru/post/2791/#cmnt51342">idler с welinux'а напомнил</a> :)<br /><a name='more'></a><br />Прежде чем начать, замечу, что я использую исключительно консольную версию vim'а. Как нижеприведённые команды поведут себя в gVim, я не знаю. Не думаю, что вы что-то сломаете, но все равно — я предупредил :) И да, скриншоты я делал на своём десктопе, так что они будут 4:3.<br /><br />Итак, окна в vim ничем не отличаются от оконо в вашем любимом DE или WM — это просто участки экрана, содержащие какую-то информацию. В случае с вимом в роли информации чаще всего выступают файлы, хотя в окне может быть и плагин (например, <a href="http://www.vim.org/scripts/script.php?script_id=1658">NERD_TREE</a>). vim с несколькими открытыми окнами выглядит так:<br /><br /><a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://1.bp.blogspot.com/_Nb6QYFUvBjY/S8j8QLseHMI/AAAAAAAAAMM/jM9CanyIZOo/s1600/vim_windows_1.png"><img style="display:block; margin:0px auto 10px; text-align:center;cursor:pointer; cursor:hand;width: 320px; height: 255px;" src="http://1.bp.blogspot.com/_Nb6QYFUvBjY/S8j8QLseHMI/AAAAAAAAAMM/jM9CanyIZOo/s320/vim_windows_1.png" border="0" alt="vim с несколькими окнами" id="BLOGGER_PHOTO_ID_5460891903134145730" /><center><font size="-2">vim с несколькими окнами<br />Слева NERD_TREE, справа — сорцы любимого ядрышка</font></center></a><br /><br />Заставить vim открыть несколько файлов в разных окнах, расположив их горизонтально, можно такой командой:<br /><div class="code">vim -o attr.c open.c sync.c</div><br />Окно NERD_TREE добавляется выполнением команды <code>:NERDTree</code> Разбивка по вертикали задаётся опцией <code>-O</code>:<br /><div class="code">vim -O attr.c open.c sync.c</div><br /><br /><a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://1.bp.blogspot.com/_Nb6QYFUvBjY/S8j9P4QPbII/AAAAAAAAAMU/wnYCDfjoLmg/s1600/vim_windows_2.png"><img style="display:block; margin:0px auto 10px; text-align:center;cursor:pointer; cursor:hand;width: 320px; height: 255px;" src="http://1.bp.blogspot.com/_Nb6QYFUvBjY/S8j9P4QPbII/AAAAAAAAAMU/wnYCDfjoLmg/s320/vim_windows_2.png" border="0" alt="Результат выполнения vim -O attr.c open.c sync.c"id="BLOGGER_PHOTO_ID_5460892997427096706" /><center><font size="-2">Результат выполнения <code>vim -O attr.c open.c sync.c</code></font></center></a><br /><br />Если vim уже открыт, новое окно можно создать несколькими путями:<ul><li>пустое окно — <code>ctrl+w n</code></li><li>горизонтально разбить текущее окно на два — <code>ctrl+w s</code></li><li>вертикально разбить текущее окно на два — <code>ctrl+w v</code></li></ul>Закрывается окно комбинацией <code>ctrl+w c</code> Впрочем, для закрытия окна может использоваться и традиционное <code>:q</code> (вместе с модификациями вроде <code>:q!</code> и <code>:wq</code>). Чтобы закрыть все окна и покинуть vim, наберите <code>:qall</code><br /><br />Теперь перейдём к самой частой операции — перемещению между окнами. Собственно говоря, здесь всё просто: жмём <code>ctrl+w</code> и указываем направление: <code>h</code>, <code>j</code>, <code>k</code> или <code>l</code> (не пугайтесь — стрелочки тоже работают :). Если кто забыл раскладку, напоминаю:<br /><div class="code">  k<br />h j l</div> Не забывайте про то, что вы в vim'е, т.е. очень продуманной и мощной среде — не стесняйтесь делать <code>ctrl+w 3l</code> и т.п. :)<br /><br />Для переключения на предыдущее и следующее окна есть команды попроще — <code>ctrl+w p</code> и <code>ctrl+w w</code> соответственно.<br /><br />Более редкими операциями является передвижение и ресайз окон. Тут нет ничего сложного — двигать окна почти так же просто, как и переключаться между ними: нажимаем <code>ctr+w</code> и говорим, куда двигать — <code>H</code>, <code>J</code>, <code>K</code> или <code>L</code> (обратите внимание на регистр). Увеличение и уменьшение окон также не представляют собой ничего сложного — сделать окно шире или у́же можно с помощью клавиш <code>&gt;</code> (шире) и <code>&lt;</code> (у́же), выше или ниже — с помощью <code>+</code> и <code>-</code>. Все эти команды меняют размер на единицу, но можно указывать и число — т.е. <code>ctrl+w 5+</code> является абсолютно валидной командой.<br /><br />Напоследок хотелось бы привести некоторые команды, имеющие отношение к вимовским окнам. По большей части они будут полезны программистам:<br /><code>vim -d файл1 файл2</code> позволяет сравнивать файлы<br /><br /><a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://2.bp.blogspot.com/_Nb6QYFUvBjY/S8kFGpj8PXI/AAAAAAAAAMc/BeDbSZ3c4yg/s1600/vim_windows_3.png"><img style="display:block; margin:0px auto 10px; text-align:center;cursor:pointer; cursor:hand;width: 320px; height: 255px;" src="http://2.bp.blogspot.com/_Nb6QYFUvBjY/S8kFGpj8PXI/AAAAAAAAAMc/BeDbSZ3c4yg/s320/vim_windows_3.png" border="0" alt="vim -d pk.new pk.old" id="BLOGGER_PHOTO_ID_5460901634957393266" /><center><font size="-2"><code>vim -d pk.new pk.old</code></font></center></a><br /><br /><code>ctrl-w ]</code> делит текущее окно, во второй половине открывает определение того, что под курсором<br /><code>ctrl+w [idf]</code> разделяет окно и во второй половине открывает:<ul><li><code>i</code> — определение переменной</li><li><code>d</code> — определение (функции или чего там под курсором)</li><li><code>f</code> — файл, путь к которому находится под курсором</li></ul><code>ctrl+w o</code> закрывает все окна, кроме текущего<br /><br />Пожалуй, всё. Напомню, что более подробно об окнах вы можете почитать в самом виме, набрав <code>:help window</code><br /><br />Happy vimming!

<h3 id='hakyll-convert-comments-title'>Comments</h3>
<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-04-19T22:46:23.528+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
^W+_ - раскрыть текущее окно на весь экран, оставив от остальных по одной строке
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-04-21T00:55:39.638+03:00, Анонимный wrote:</p>
<p class='hakyll-convert-comment-body'>
Автор спасибо! <br />Очень интересно и познавательно, пиши еще!
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2010-08-30T13:46:14.504+03:00, idler wrote:</p>
<p class='hakyll-convert-comment-body'>
idler перечитал статью и шлет благодарность :)
</p>
</div>

<div class='hakyll-convert-comment'>
<p class='hakyll-convert-comment-date'>On 2016-02-03T16:16:30.788+02:00, Владимир Володин wrote:</p>
<p class='hakyll-convert-comment-body'>
:-)))!!! Начал пробовать все предложенные комбинации клавиш, промахнулся, даже не понял что нажал (что-то левой рукой после ctrl+w и кажется с shift_ом) vim свернулся вверх в желтую полоску, при этом открыл окно терминала. Минут пять клацал по клавиатуре пока не вернул свой файл... кажется это была комбинация ctrl+q...
</p>
</div>



