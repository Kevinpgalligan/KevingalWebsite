title: A Farewell to JavaScript
date: 2021-02-06
description: I purged JavaScript from my site.
imgthumbnail: img/farewell-to-js/thumbnail.jpg
tags: web meta vim
publish: y

The Board of Directors are pleased to announce that the blog's LaTeX rendering is now done at build time.

Previously, readers had to download MathJax from a CDN somewhere. Well, their browsers did. Then the MathJax script converted all LaTeX on the page to HTML, and downloaded CSS files and fonts to make the HTML look pretty.

This always stuck in my throat. It meant that the site couldn't render properly without JavaScript or without an internet connection. The CDN could vanish without warning. And it seemed unnecessary -- why shouldn't I be able to render when I build the site, rather than forcing people to download a (probably massive) rendering script?

I came across a Python Markdown plugin the other day called [markdown-katex](https://github.com/mbarkhau/markdown-katex) that resolved this gag-worthy state of affairs.

Here's the build process and how markdown-katex now fits into it:

1. I push the shiny "build" button. This spins up a Flask server. Flask is a Python web server with lots of plugins.
2. A Flask plugin called Frozen-Flask crawls the site and dumps all the files it finds to a folder.
3. The blog posts are written in Markdown, which is like HTML but easier on the eyes. When Frozen-Flask hits a blog post, the post gets converted from Markdown to HTML. markdown-katex comes in here and handles the LaTeX parts. From what I understand, markdown-katex is bundled with a copy of KaTeX (a Node.js LaTeX renderer), and all it does is take the LaTeX bits from the Markdown document and pass them to KaTeX.
4. I take the HTML files dumped by Frozen-Flask and upload them to a GitHub Pages repository where they're served to the world.

Oh, I also needed to start serving the KaTeX CSS files and fonts, which was just a copy-and-paste job. The whole thing seems baroque when I write it out fully, but I'm happy that JavaScript is no longer required to read the blog.

Now for some vim trivia. I had to go back and change the LaTeX delimiters in all the blog posts. MathJax uses $ as a delimiter for inline math mode, and $$ for display math mode. markdown-katex uses $\` and \`$ for inline math mode. For display math mode, as the opening delimiter it uses three backticks directly followed by "math", and as the closing delimiter it uses three backticks. I used a vim macro to replace the inline delimiters, with the following key presses: ``qa/\$RETa`ESCni`ESCwq``. It finds the next pair of LaTeX delimiters (e.g. `$x^2+1$`) and converts them to the new format (``$`x^2+1`$``).

Here's a breakdown of how that works.

* `qa` starts recording a macro in register `a`.
* `/\$RET` searches for the next dollar sign and moves the cursor there. RET is the enter/return key.
* ``a`ESC`` enters insert mode *after* the cursor, writes a backtick, and exits insert mode. ESC is the escape key. 
* `n` continues the search, moving the cursor to the next dollar sign.
* ``i`ESC`` enters insert mode *before* the cursor, writes a backtick, and exits insert mode.
* `w` moves the cursor forward by 1 word so that the macro doesn't get stuck on the $ we just processed.
* `q` stops recording the macro.

I could type `@a` to run the macro once, then `.` to repeat it after that. It's much faster and less error-prone than fixing such things manually, obviously, but I also feel that vim is more suited to this task than, say, a Python script. It's so easy to record a macro, and you can catch errors as they happen. I would go with Python if I had to change more than a handful of files, though.
