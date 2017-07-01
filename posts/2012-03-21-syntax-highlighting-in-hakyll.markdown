---
title: Syntax highlighting in Hakyll
description: How to get syntax highlighting working in Hakyll.
language: english
---

I am a complete newbie to [Hakyll](http://jaspervdj.be/hakyll/), so this post
may be terribly incorrect, but I googled the problem and didn't find any
solution (see *post scriptum* note, though), so…

So the problem is: Hakyll is using
[pandoc](http://johnmacfarlane.net/pandoc/index.html) to re-format things from,
say, markdown to HTML. pandoc in turn uses [pygments](http://pygments.org/) to
highlight the code blocks. And it really does: if I write something like

    ```Haskell
    fibonacci :: Integer -> Integer
    fibonacci = fibo !!
        where fibo = 0 : 1 : zipWith (+) fibo (tail fibo)
    ```

it produces the following HTML code:

```Html
<pre class="sourceCode Haskell"><code class="sourceCode haskell">
<span class="ot">fibonacci ::</span> <span class="dt">Integer</span>
        <span class="ot">-&gt;</span> <span class="dt">Integer</span>
fibonacci <span class="fu">=</span> fibo <span class="fu">!!</span>
<span class="kw">where</span>
fibo <span class="fu">=</span> <span class="dv">0</span>
    <span class="fu">:</span> <span class="dv">1</span>
    <span class="fu">:</span>
    <span class="fu">zipWith</span>
        (<span class="fu">+</span>)
        fibo
        (<span class="fu">tail</span> fibo)
</code></pre>
```

(formatting is mine). But it doesn't really *highlight* anything — i.e. code is
still just black at white.

So Hakyll seem to wrap the code in different styling classes, but how does
browser know that `kw` class should be displayed in, say, bold green?
Obviously, you need some [CSS][css] describing how to highlight things.
Unfortunatelly, the only way to get one is to copy [`web/css/syntax.css` from
Hakyll's repo][style.css]. I really wish there were some better way (generating
it locally using `pygmentize`?), but I couldn't find one.

Oh, yes: [`-fhighlighting` receipe][fhighlighting] still applies, i.e. to get
highlighting you may need to reinstall pandoc and hakyll with

    $ cabal install --reinstall -fhighlighting pandoc hakyll

Hope this helps.

*P.S. A kind soul named Anomareh [put all pygments styles at
Github][pygments-styles-dump] (along with the script used to do so). Kudos to
Vladimir Ivanov for pointing that out.* 

[css]: https://en.wikipedia.org/wiki/Cascading_Style_Sheets "Cascading Style Sheets"
[style.css]: https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css "Hakyll repo"
[fhighlighting]: http://jaspervdj.be/hakyll/tutorials/faq.html#does-hakyll-support-syntax-highlighting "Does Hakyll support syntax highlighting?"
[pygments-styles-dump]: https://github.com/Anomareh/pygments-styles-dump "A dump of all the CSS styles included with Pygments"
