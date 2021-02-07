title: My computer writes toki poetry
date: 2021-02-07
description: Computer-generated poems in the toki pona language.
imgthumbnail: img/toki-poetry/thumbnail.jpg
tags: creative data lang

I taught my computer how to write poetry in toki pona!

> insert poem here

Here's my poor attempt at a translation:

> blah

explain toki pona

explain how i gone and dun it

limericks & sonnets

## Appendix A: data spelunking
Since I had a bunch of text data from r/tokipona, I decided to run some quick numbers on it.

Here's a word cloud. The bigger a word is, the more common it is on r/tokipona.

<img src="{{ url_for('static', filename='img/toki-poetry/word-cloud.png') }}"
     alt="A word cloud where the biggest words are 'a', 'toki', 'pona', 'jan'."
     class="centered">

Here's [Zipf's law](https://en.wikipedia.org/wiki/Zipf%27s_law) in action, via a cumulative frequency distribution. It basically shows that the top 10 words make up about 50% of word occurrences. The top 50 words make up almost 90%.

<img src="{{ url_for('static', filename='img/toki-poetry/cdf.png') }}"
     alt="Cumulative frequency distribution for words on r/tokipona. It shows an inverse power relationship (grows fast at first but slows down exponentially)."
     class="centered">

Aaaand here are all the words sorted by frequency.

<div class="cooltablewrap">
<table>
<thead>
<tr><th>word</th><th>count</th><th>percentage</th></tr>
</thead>
<tbody>
<tr><td>a</td><td>2331</td><td>10.590641%</td></tr>
<tr><td>pona</td><td>1419</td><td>6.4470696%</td></tr>
<tr><td>li</td><td>1370</td><td>6.2244434%</td></tr>
<tr><td>toki</td><td>1154</td><td>5.2430716%</td></tr>
<tr><td>e</td><td>1055</td><td>4.793276%</td></tr>
<tr><td>mi</td><td>981</td><td>4.457065%</td></tr>
<tr><td>ni</td><td>700</td><td>3.1803725%</td></tr>
<tr><td>jan</td><td>621</td><td>2.8214447%</td></tr>
<tr><td>pi</td><td>616</td><td>2.7987278%</td></tr>
<tr><td>la</td><td>563</td><td>2.5579283%</td></tr>
<tr><td>ala</td><td>495</td><td>2.2489777%</td></tr>
<tr><td>tawa</td><td>455</td><td>2.0672421%</td></tr>
<tr><td>sina</td><td>401</td><td>1.8218992%</td></tr>
<tr><td>lon</td><td>400</td><td>1.8173558%</td></tr>
<tr><td>mute</td><td>388</td><td>1.762835%</td></tr>
<tr><td>tenpo</td><td>303</td><td>1.376647%</td></tr>
<tr><td>ona</td><td>293</td><td>1.3312131%</td></tr>
<tr><td>sona</td><td>262</td><td>1.190368%</td></tr>
<tr><td>sitelen</td><td>253</td><td>1.1494775%</td></tr>
<tr><td>ken</td><td>246</td><td>1.1176738%</td></tr>
<tr><td>wan</td><td>235</td><td>1.0676965%</td></tr>
<tr><td>o</td><td>223</td><td>1.0131758%</td></tr>
<tr><td>wile</td><td>221</td><td>1.004089%</td></tr>
<tr><td>ike</td><td>202</td><td>0.91776466%</td></tr>
<tr><td>nimi</td><td>199</td><td>0.9041345%</td></tr>
<tr><td>kama</td><td>199</td><td>0.9041345%</td></tr>
<tr><td>pilin</td><td>194</td><td>0.8814175%</td></tr>
<tr><td>ma</td><td>188</td><td>0.8541572%</td></tr>
<tr><td>tan</td><td>175</td><td>0.7950931%</td></tr>
<tr><td>nasin</td><td>171</td><td>0.7769196%</td></tr>
<tr><td>tu</td><td>168</td><td>0.7632894%</td></tr>
<tr><td>lili</td><td>163</td><td>0.74057245%</td></tr>
<tr><td>kepeken</td><td>157</td><td>0.71331215%</td></tr>
<tr><td>taso</td><td>145</td><td>0.6587915%</td></tr>
<tr><td>musi</td><td>143</td><td>0.6497047%</td></tr>
<tr><td>ante</td><td>141</td><td>0.6406179%</td></tr>
<tr><td>ijo</td><td>135</td><td>0.61335754%</td></tr>
<tr><td>pali</td><td>133</td><td>0.60427076%</td></tr>
<tr><td>en</td><td>133</td><td>0.60427076%</td></tr>
<tr><td>pu</td><td>131</td><td>0.595184%</td></tr>
<tr><td>seme</td><td>130</td><td>0.5906406%</td></tr>
<tr><td>lukin</td><td>125</td><td>0.56792367%</td></tr>
<tr><td>ale</td><td>123</td><td>0.5588369%</td></tr>
<tr><td>jo</td><td>120</td><td>0.5452067%</td></tr>
<tr><td>lipu</td><td>118</td><td>0.53611994%</td></tr>
<tr><td>moku</td><td>117</td><td>0.5315766%</td></tr>
<tr><td>lawa</td><td>107</td><td>0.48614267%</td></tr>
<tr><td>sama</td><td>102</td><td>0.46342573%</td></tr>
<tr><td>ilo</td><td>94</td><td>0.4270786%</td></tr>
<tr><td>tomo</td><td>92</td><td>0.41799182%</td></tr>
<tr><td>suli</td><td>91</td><td>0.41344842%</td></tr>
<tr><td>nasa</td><td>89</td><td>0.40436167%</td></tr>
<tr><td>pakala</td><td>84</td><td>0.3816447%</td></tr>
<tr><td>pana</td><td>84</td><td>0.3816447%</td></tr>
<tr><td>nanpa</td><td>84</td><td>0.3816447%</td></tr>
<tr><td>suno</td><td>84</td><td>0.3816447%</td></tr>
<tr><td>kulupu</td><td>84</td><td>0.3816447%</td></tr>
<tr><td>luka</td><td>83</td><td>0.37710133%</td></tr>
<tr><td>pini</td><td>82</td><td>0.37255794%</td></tr>
<tr><td>kalama</td><td>82</td><td>0.37255794%</td></tr>
<tr><td>sewi</td><td>80</td><td>0.36347115%</td></tr>
<tr><td>soweli</td><td>76</td><td>0.3452976%</td></tr>
<tr><td>sike</td><td>76</td><td>0.3452976%</td></tr>
<tr><td>sin</td><td>75</td><td>0.3407542%</td></tr>
<tr><td>anu</td><td>71</td><td>0.32258064%</td></tr>
<tr><td>ali</td><td>65</td><td>0.2953203%</td></tr>
<tr><td>moli</td><td>61</td><td>0.27714676%</td></tr>
<tr><td>poka</td><td>60</td><td>0.27260336%</td></tr>
<tr><td>wawa</td><td>60</td><td>0.27260336%</td></tr>
<tr><td>linja</td><td>58</td><td>0.26351658%</td></tr>
<tr><td>kiwen</td><td>55</td><td>0.24988641%</td></tr>
<tr><td>insa</td><td>55</td><td>0.24988641%</td></tr>
<tr><td>kin</td><td>54</td><td>0.24534303%</td></tr>
<tr><td>telo</td><td>54</td><td>0.24534303%</td></tr>
<tr><td>mama</td><td>51</td><td>0.23171286%</td></tr>
<tr><td>sijelo</td><td>50</td><td>0.22716947%</td></tr>
<tr><td>kili</td><td>49</td><td>0.22262608%</td></tr>
<tr><td>mu</td><td>48</td><td>0.2180827%</td></tr>
<tr><td>meli</td><td>47</td><td>0.2135393%</td></tr>
<tr><td>utala</td><td>47</td><td>0.2135393%</td></tr>
<tr><td>mun</td><td>45</td><td>0.20445251%</td></tr>
<tr><td>weka</td><td>44</td><td>0.19990914%</td></tr>
<tr><td>kon</td><td>43</td><td>0.19536574%</td></tr>
<tr><td>kasi</td><td>40</td><td>0.18173558%</td></tr>
<tr><td>kule</td><td>39</td><td>0.17719218%</td></tr>
<tr><td>pan</td><td>39</td><td>0.17719218%</td></tr>
<tr><td>seli</td><td>36</td><td>0.16356201%</td></tr>
<tr><td>ko</td><td>36</td><td>0.16356201%</td></tr>
<tr><td>mije</td><td>36</td><td>0.16356201%</td></tr>
<tr><td>mani</td><td>34</td><td>0.15447524%</td></tr>
<tr><td>palisa</td><td>32</td><td>0.14538845%</td></tr>
<tr><td>unpa</td><td>32</td><td>0.14538845%</td></tr>
<tr><td>olin</td><td>31</td><td>0.14084508%</td></tr>
<tr><td>awen</td><td>31</td><td>0.14084508%</td></tr>
<tr><td>akesi</td><td>29</td><td>0.13175829%</td></tr>
<tr><td>open</td><td>28</td><td>0.12721491%</td></tr>
<tr><td>esun</td><td>28</td><td>0.12721491%</td></tr>
<tr><td>kute</td><td>27</td><td>0.122671515%</td></tr>
<tr><td>alasa</td><td>26</td><td>0.11812812%</td></tr>
<tr><td>pimeja</td><td>25</td><td>0.113584734%</td></tr>
<tr><td>uta</td><td>24</td><td>0.10904135%</td></tr>
<tr><td>suwi</td><td>23</td><td>0.104497954%</td></tr>
<tr><td>jaki</td><td>23</td><td>0.104497954%</td></tr>
<tr><td>poki</td><td>21</td><td>0.095411174%</td></tr>
<tr><td>loje</td><td>21</td><td>0.095411174%</td></tr>
<tr><td>len</td><td>21</td><td>0.095411174%</td></tr>
<tr><td>selo</td><td>20</td><td>0.09086779%</td></tr>
<tr><td>lete</td><td>20</td><td>0.09086779%</td></tr>
<tr><td>kipisi</td><td>20</td><td>0.09086779%</td></tr>
<tr><td>jelo</td><td>20</td><td>0.09086779%</td></tr>
<tr><td>oko</td><td>18</td><td>0.08178101%</td></tr>
<tr><td>waso</td><td>18</td><td>0.08178101%</td></tr>
<tr><td>sinpin</td><td>17</td><td>0.07723762%</td></tr>
<tr><td>monsi</td><td>16</td><td>0.07269423%</td></tr>
<tr><td>laso</td><td>16</td><td>0.07269423%</td></tr>
<tr><td>anpa</td><td>15</td><td>0.06815084%</td></tr>
<tr><td>nena</td><td>14</td><td>0.063607454%</td></tr>
<tr><td>pipi</td><td>14</td><td>0.063607454%</td></tr>
<tr><td>noka</td><td>12</td><td>0.054520674%</td></tr>
<tr><td>lape</td><td>11</td><td>0.049977284%</td></tr>
<tr><td>kala</td><td>11</td><td>0.049977284%</td></tr>
<tr><td>walo</td><td>11</td><td>0.049977284%</td></tr>
<tr><td>namako</td><td>7</td><td>0.031803727%</td></tr>
<tr><td>supa</td><td>4</td><td>0.018173557%</td></tr>
<tr><td>lupa</td><td>3</td><td>0.0136301685%</td></tr>
</tbody>
</table>
</div>
