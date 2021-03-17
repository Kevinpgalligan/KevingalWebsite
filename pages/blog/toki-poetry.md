title: toki poems
date: 2021-03-17
description: Computer-generated poems in the toki pona language.
imgthumbnail: img/toki-poetry/thumbnail.jpg
tags: creative data
publish: y

I wrote a cute little program to generate poetry in toki pona, the 125-word language that was published by Sonja Lang in 2001.

Here are some of its sick rhymes:

> jan sama pi jan,  
> ike li pona ala,  
> tawa mi la tan.  
>  
> tenpo ni e kala.

And my pants-on-head attempt at a translation:

> person same of person,  
> bad? not good,  
> to me from.  
>  
> time this fish.

Before I explain how it works, I should share some relevant facts about toki pona:

* It has only 14 letters (aeioumnptkswlj).
* Unlike English, it's spelled phonetically. No contradictory spellings like through and cough, thank you very much.
* The only letters that could trip up an English speaker are j, which is pronounced like y, and the vowels, which are the same as Spanish vowels: a is ah, e is ay as in yay, o is oh, u is ooh, i is ee as in free.
* And, relevant to the task of a machine poet, it's easy to break the words up into syllables.

Now for an **Explanation**. The first thing I needed was a bunch of toki pona text (a "corpus"), which I gathered by downloading comments from [r/tokipona](https://www.reddit.com/r/tokipona/) using a script that I happened to have lying around. Then I created a big ol' Markov chain based on the corpus, using another script that I happened to have lying around.

<figure>

<img src="{{ url_for('static', filename='img/toki-poetry/chain.png') }}"
     alt="A Markov chain / graph where the nodes are toki pona words and the edges between the nodes show the probability of going from one word to another. There are 4 nodes: start, toki, pona and awen. An arrow from start to toki has the label 11; start to pona has 2; toki to pona has 5; toki to awen has 1; and pona to awen has 2."
     class="centered">

<figcaption>A Markov chain with toki pona words!</figcaption>
</figure>

So to generate text with this Markov chain graph thing, you randomly traverse the graph, beginning at the node called *start*, which represents the start of a sentence. The probability of going from one node to another is weighted by the number of times the words appear next to each other in the corpus. The weight is stored in the edge between the nodes. In this example, the word "toki" appeared at the start of 11 sentences, which means there's a probability of 11/(11+2) that you go from *start* to *toki*. Similarly, there's a probability of 2/(11+2) that you go from *start* to *pona*. Don't panic!, the probabilities add up to 1.

<figure>
<img src="{{ url_for('static', filename='img/toki-poetry/path.png') }}"
     alt="A path through the Markov chain from before."
     class="centered">

<figcaption>A possible path through the Markov chain.</figcaption>
</figure>

Here's a path we can follow through the Markov chain, resulting in the sentence "toki pona awen". It depends on which random numbers we pull from our hat, though. We could also end up with "toki awen" or "pona awen".

That's fine for normal text generation, but when you're generating poems, there are extra constraints on the output, such as the number of syllables per line, or maybe the final word of a line should rhyme with another word. (TODO: fix this crappy sentence).

Here's an example of how the algorithm works with the addition of constraints. Let's say you're generating a line in a poem using the Markov chain from before. You've gone down the following path: *start* -> *toki* -> *awen*. But you need the last word to rhyme with "sona", which "awen" doesn't.

DIAGRAM HERE.

So you retrace your steps back to *toki*.

DIAGRAM HERE.

Then you try picking *pona* as the next node, giving: *start* -> *toki* -> *pona*.

DIAGRAM HERE.

"pona" happens to rhyme with "sona", so you can stop. You'd also have to backtrack if you exceeded the number of syllables allowed per line.

That's pretty much how the poem generation works. A concise description would be: weighted random search through a graph, with backtracking.

Another small detail: how do you count the syllables in a word? The simple structure and pronunciation of toki pona makes it easy. Just count the number of vowels: "a" has 1 syllable, "ala" has 2, "pimeja" has 3. And how do you know whether words rhyme? There's [a type of rhyme](https://en.wikipedia.org/wiki/Rhyme) where the last syllables of the words are the same. To detect that type of rhyme, break the words into syllables (maybe using a regex like `([aeiou]+[^aeiou]+[aeiou]+$)|([aeiou]+[^aeiou]+$)`) and compare the last syllables.

INSERT MORE POEMS HERE.

Maybe, rather than throwing this poem generator on the heap of all the useless programs I've written, I'll use it as the basis for a Twitter or Mastodon bot. Stay tuned. In the meantime, [here's the code](https://github.com/Kevinpgalligan/toki-poems).

### Appendix A: all the rhymes
Here are the groups of rhyming words, according to my definition of a rhyme. There are 61 rhyming words, split between 21 groups. That leaves 64 words without a rhyme buddy :(

* ala utala pakala kala
* alasa nasa
* ali pali
* anpa nanpa
* awen sitelen open len kiwen kepeken ken en
* esun mun
* ike sike
* jan wan tan pan
* jelo telo sijelo selo
* kalama sama mama kama
* kili lili
* kin sinpin sin pilin olin nasin lukin
* kon lon
* kute mute
* laso waso taso
* lawa wawa tawa
* lupa supa
* meli soweli seli
* noka poka
* ona sona pona
* poki toki

### Appendix B: data spelunking
It would be a waste not to do anything with the text data from r/tokipona, so here are some quick plots.

Here's a word cloud. The bigger a word is, the more common it is on r/tokipona.

<img src="{{ url_for('static', filename='img/toki-poetry/word-cloud.png') }}"
     alt="A word cloud where the biggest words are 'a', 'toki', 'pona', 'jan'."
     class="centered">

Here's [Zipf's law](https://en.wikipedia.org/wiki/Zipf%27s_law) in action, via a cumulative frequency distribution. It basically shows that the top 10 words make up about 50% of word occurrences, while the top 50 words make up almost 90%.

<img src="{{ url_for('static', filename='img/toki-poetry/cdf.png') }}"
     alt="Cumulative frequency distribution for words on r/tokipona. It shows an inverse power relationship (grows fast at first but slows down exponentially)."
     class="centered">

Aaaand here's a table of all the words, ordered by frequency. BUT WAIT. Something just occurred to me. A lot of people write in English on r/tokipona, so the letter "a" probably appears more popular here than it is in reality.

<div class="cooltablewrap">
<table>
<thead>
<tr><th>word</th><th>count</th><th>%</th>
<th>word</th><th>count</th><th>%</th>
<th>word</th><th>count</th><th>%</th></tr>
</thead>
<tbody>
<tr><td>a</td><td>2331</td><td>10.59</td><td>ale</td><td>123</td><td>0.56</td><td>kule</td><td>39</td><td>0.18</td></tr>
<tr><td>pona</td><td>1419</td><td>6.45</td><td>jo</td><td>120</td><td>0.55</td><td>pan</td><td>39</td><td>0.18</td></tr>
<tr><td>li</td><td>1370</td><td>6.22</td><td>lipu</td><td>118</td><td>0.54</td><td>seli</td><td>36</td><td>0.16</td></tr>
<tr><td>toki</td><td>1154</td><td>5.24</td><td>moku</td><td>117</td><td>0.53</td><td>ko</td><td>36</td><td>0.16</td></tr>
<tr><td>e</td><td>1055</td><td>4.79</td><td>lawa</td><td>107</td><td>0.49</td><td>mije</td><td>36</td><td>0.16</td></tr>
<tr><td>mi</td><td>981</td><td>4.46</td><td>sama</td><td>102</td><td>0.46</td><td>mani</td><td>34</td><td>0.15</td></tr>
<tr><td>ni</td><td>700</td><td>3.18</td><td>ilo</td><td>94</td><td>0.43</td><td>palisa</td><td>32</td><td>0.15</td></tr>
<tr><td>jan</td><td>621</td><td>2.82</td><td>tomo</td><td>92</td><td>0.42</td><td>unpa</td><td>32</td><td>0.15</td></tr>
<tr><td>pi</td><td>616</td><td>2.80</td><td>suli</td><td>91</td><td>0.41</td><td>olin</td><td>31</td><td>0.14</td></tr>
<tr><td>la</td><td>563</td><td>2.56</td><td>nasa</td><td>89</td><td>0.40</td><td>awen</td><td>31</td><td>0.14</td></tr>
<tr><td>ala</td><td>495</td><td>2.25</td><td>pakala</td><td>84</td><td>0.38</td><td>akesi</td><td>29</td><td>0.13</td></tr>
<tr><td>tawa</td><td>455</td><td>2.07</td><td>pana</td><td>84</td><td>0.38</td><td>open</td><td>28</td><td>0.13</td></tr>
<tr><td>sina</td><td>401</td><td>1.82</td><td>nanpa</td><td>84</td><td>0.38</td><td>esun</td><td>28</td><td>0.13</td></tr>
<tr><td>lon</td><td>400</td><td>1.82</td><td>suno</td><td>84</td><td>0.38</td><td>kute</td><td>27</td><td>0.12</td></tr>
<tr><td>mute</td><td>388</td><td>1.76</td><td>kulupu</td><td>84</td><td>0.38</td><td>alasa</td><td>26</td><td>0.12</td></tr>
<tr><td>tenpo</td><td>303</td><td>1.38</td><td>luka</td><td>83</td><td>0.38</td><td>pimeja</td><td>25</td><td>0.11</td></tr>
<tr><td>ona</td><td>293</td><td>1.33</td><td>pini</td><td>82</td><td>0.37</td><td>uta</td><td>24</td><td>0.11</td></tr>
<tr><td>sona</td><td>262</td><td>1.19</td><td>kalama</td><td>82</td><td>0.37</td><td>suwi</td><td>23</td><td>0.10</td></tr>
<tr><td>sitelen</td><td>253</td><td>1.15</td><td>sewi</td><td>80</td><td>0.36</td><td>jaki</td><td>23</td><td>0.10</td></tr>
<tr><td>ken</td><td>246</td><td>1.12</td><td>soweli</td><td>76</td><td>0.35</td><td>poki</td><td>21</td><td>0.10</td></tr>
<tr><td>wan</td><td>235</td><td>1.07</td><td>sike</td><td>76</td><td>0.35</td><td>loje</td><td>21</td><td>0.10</td></tr>
<tr><td>o</td><td>223</td><td>1.01</td><td>sin</td><td>75</td><td>0.34</td><td>len</td><td>21</td><td>0.10</td></tr>
<tr><td>wile</td><td>221</td><td>1.00</td><td>anu</td><td>71</td><td>0.32</td><td>selo</td><td>20</td><td>0.09</td></tr>
<tr><td>ike</td><td>202</td><td>0.92</td><td>ali</td><td>65</td><td>0.30</td><td>lete</td><td>20</td><td>0.09</td></tr>
<tr><td>nimi</td><td>199</td><td>0.90</td><td>moli</td><td>61</td><td>0.28</td><td>kipisi</td><td>20</td><td>0.09</td></tr>
<tr><td>kama</td><td>199</td><td>0.90</td><td>poka</td><td>60</td><td>0.27</td><td>jelo</td><td>20</td><td>0.09</td></tr>
<tr><td>pilin</td><td>194</td><td>0.88</td><td>wawa</td><td>60</td><td>0.27</td><td>oko</td><td>18</td><td>0.08</td></tr>
<tr><td>ma</td><td>188</td><td>0.85</td><td>linja</td><td>58</td><td>0.26</td><td>waso</td><td>18</td><td>0.08</td></tr>
<tr><td>tan</td><td>175</td><td>0.80</td><td>kiwen</td><td>55</td><td>0.25</td><td>sinpin</td><td>17</td><td>0.08</td></tr>
<tr><td>nasin</td><td>171</td><td>0.78</td><td>insa</td><td>55</td><td>0.25</td><td>monsi</td><td>16</td><td>0.07</td></tr>
<tr><td>tu</td><td>168</td><td>0.76</td><td>kin</td><td>54</td><td>0.25</td><td>laso</td><td>16</td><td>0.07</td></tr>
<tr><td>lili</td><td>163</td><td>0.74</td><td>telo</td><td>54</td><td>0.25</td><td>anpa</td><td>15</td><td>0.07</td></tr>
<tr><td>kepeken</td><td>157</td><td>0.71</td><td>mama</td><td>51</td><td>0.23</td><td>nena</td><td>14</td><td>0.06</td></tr>
<tr><td>taso</td><td>145</td><td>0.66</td><td>sijelo</td><td>50</td><td>0.23</td><td>pipi</td><td>14</td><td>0.06</td></tr>
<tr><td>musi</td><td>143</td><td>0.65</td><td>kili</td><td>49</td><td>0.22</td><td>noka</td><td>12</td><td>0.05</td></tr>
<tr><td>ante</td><td>141</td><td>0.64</td><td>mu</td><td>48</td><td>0.22</td><td>lape</td><td>11</td><td>0.05</td></tr>
<tr><td>ijo</td><td>135</td><td>0.61</td><td>meli</td><td>47</td><td>0.21</td><td>kala</td><td>11</td><td>0.05</td></tr>
<tr><td>pali</td><td>133</td><td>0.60</td><td>utala</td><td>47</td><td>0.21</td><td>walo</td><td>11</td><td>0.05</td></tr>
<tr><td>en</td><td>133</td><td>0.60</td><td>mun</td><td>45</td><td>0.20</td><td>namako</td><td>7</td><td>0.03</td></tr>
<tr><td>pu</td><td>131</td><td>0.60</td><td>weka</td><td>44</td><td>0.20</td><td>supa</td><td>4</td><td>0.02</td></tr>
<tr><td>seme</td><td>130</td><td>0.59</td><td>kon</td><td>43</td><td>0.20</td><td>lupa</td><td>3</td><td>0.01</td></tr>
<tr><td>lukin</td><td>125</td><td>0.57</td><td>kasi</td><td>40</td><td>0.18</td></tr>
</tbody>
</table>
</div>
