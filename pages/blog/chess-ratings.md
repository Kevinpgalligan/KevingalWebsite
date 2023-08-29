title: Fluctuations in chess ratings
date: 2023-05-23
description: Investigating chess ratings and how much they're expected to fluctuate.
imgthumbnail: img/thumbnail.jpg
requires: math
tags: probability chess

<script src="/static/js/chess.js"></script>
<p>Player 1 ELO rating:<input id="p1" type="range" value="1800" min="0" max="4000" oninput="updateExpectedScore()"/> <span id="p1out"></span></p>
<p>Player 2 ELO rating:<input id="p2" type="range" value="1800" min="0" max="4000" oninput="updateExpectedScore()"/> <span id="p2out"></span></p>
<p>Probability of player 1 winning: <span id="result">N/A</p>
<script>updateExpectedScore()</script>
