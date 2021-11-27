title: The mysteries of two's complement
date: 2021-11-20
description: My attempt to explain two's complement clearly.
imgthumbnail: img/twos/thumbnail.jpg
requires: math
tags: programming

If you've ever taken a computer science class, then you've probably heard of two's complement. What you might have been told is that it's a system for representing positive and negative numbers, and that to convert a number to two's complement form, you invert all the bits and add 1.

For example, let's say you have a decimal number, $`5_d`$. That's $`0101_b`$ in binary, assuming you have 4 bits. To convert this number to two's complement, you change the 0s to 1s and the 1s to 0s, giving $`1010_b`$, and then you add 1, giving $`1011_b`$. That's -5 in the two's complement system.

"Okay," you may have thought to yourself on hearing this information, unconvinced, "but where does that seemingly arbitrary binary transformation come from? And what's the POINT of two's complement?"

These are the questions I will attempt to briefly answer.

First, the motivation for two's complement. If you were to design a computer number system from scratch, and you had 8 bits to represent positive and negative numbers, then your first thought might be to reserve a single bit for the sign of a number, and the remaining 7 bits for its value. Assuming the leftmost bit represented the sign, and that 0 stood for negative and 1 stood for positive, then $`1_d`$ would be represented in memory as 10000001, while $`-1_d`$ would be represented as 00000001.

This is all well and good, but: (1) two zeros although I'm not yet sure why that's sooooo bad; (2) you need special handling for addition, subtraction, multiplication; (3) subtraction is especially annoying.

But what if... you could avoid having a special representation for negative numbers. You treated them just like regular, boring old positive numbers that can be added together just like positive numbers! That's what two's complement is: a clever way to represent negative numbers as positive numbers. In a sense.

You take a number k, and treat 2^B-k as its negation, because k+(2^B-k) = 2^B = 0.

<img src="{{ url_for('static', filename='img/twos/twos-complement-map.png') }}"
     alt="The two's complement number line for 8-bit values. 8 bits can take on 2^8, or 256, possible values. The mapping of raw binary values, to their values in two's complement, is represented by 256 boxes in a row, indexed by the numbers 0 to 255. The first 128 boxes contain the numbers 0 to 127. The remaining boxes contain the numbers -128 to -1, in descending order."
     class="centered">

Yay, addition and subtraction and multiplication work like they do for adding positive numbers! You can just pretend everything is positive.

So where does the mindless trick come from? It's a simple way of computing 2^B-k. 2^8 (decimal) = 11111111b + 1b, 11111111b + 1b - 00101101b = 11010010b + 1b = 11010011b = 211d, and yes that's 256d-45d.

Reading material: <https://en.wikipedia.org/wiki/Two%27s_complement>

TODO: replace the thumbnail!
