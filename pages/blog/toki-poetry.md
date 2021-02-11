title: My computer writes toki poetry
date: 2021-02-07
description: Computer-generated poems in the toki pona language.
imgthumbnail: img/toki-poetry/thumbnail.jpg
tags: creative data lang

I wrote a program to generate random poetry in toki pona!

> insert poem here

Here's my pants-on-head attempt at a translation:

> blah

toki pona is a constructed language with just 125 words, published by Sonja Lang in 2001. I thought it would be nice for a project like this because it's tiny and simple. It has only 14 letters (aeioumnptkswlj) and, unlike English, it's pronounced as it's written. The only letters that could trip up an English speaker are j (pronounced like y) and the vowels (pronounced like Spanish: a is ah, e is ay as in yay, o is oh, u is ooh, i is ee as in fee). It's easy to break the words up into syllables, which is required for generating poetry. No natural language processing library necessary.

explain how i gone and dun it

limericks & sonnets

## Appendix A: all the rhymes
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

## Appendix B: data spelunking
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
