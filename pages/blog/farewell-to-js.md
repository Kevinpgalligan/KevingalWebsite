title: A Farewell to JavaScript
date: 2021-02-03
description: I purged the use of JavaScript CDNs from my site.
imgthumbnail: img/farewell-to-js/thumbnail.jpg
publish: y
tags: web meta

I feel uneasy making people download MathJax from a content delivery network (CDN) in order to render the LaTeX on my blog. What if the CDN disappears or gets hacked? Why should someone have to interact a server other than the one that hosts my blog? It's unnecessary, because in theory I could render the LaTeX at build time.

So today, I purged it. I now use [markdown-katex](https://github.com/mbarkhau/markdown-katex), an extension for Python Markdown.

TODO:

* fix tag page getting shifted slightly.
* bundle KaTeX CSS & fonts.
* make sure that equations don't overflow.
* fix thumbnail link.
