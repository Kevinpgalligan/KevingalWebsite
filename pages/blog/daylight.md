title: The grand stretch in the evening, visualised
date: 2021-04-02
description: Cute graphs of daylight hours.
imgthumbnail: img/daylight/thumbnail.jpg
publish: y
tags: data ireland

I made some graphs that show how much daylight there will be throughout 2021 in different parts of the world. I'm sure it has been done before, but when has that ever stopped a shameless plagiarist like myself?

Here's Dublin, Ireland.

<img src="{{ url_for('static', filename='img/daylight/ireland.png') }}"
     alt="Ireland's daylight hours. Month on the x-axis, time of day on the y-axis. Daylight hours are shaded in yellow. It peaks at about 16 hours of daylight in summer, and drops to about 7 hours of daylight in winter."
     class="centered">

Irish people speak with reverence of the "grand stretch in the evening" during summer months. That's because we have long sunny evenings, which last until 9 or 10 o'clock in July. You can also see the sudden jumps caused by daylight savings ("spring forward, fall back"), and why that might be desirable. At the end of March, the little sunlight we have is being squandered during the early morning when everyone is still in bed. A clock shift avoids that.

In Kittilä, Finland, things are even more extreme. So extreme that it exposed several bugs in my plotting code.

<img src="{{ url_for('static', filename='img/daylight/finland.png') }}"
     alt="Finland's daylight hours. There are days during the summer where it's never dark, and days during the winter where it's always dark."
     class="centered">

They experience something called "midnight sun" during the summer, where the sun is visible 24/7. And "polar night" during the winter, where it's dark 24/7. Such is the fate of those who dwell near the north pole.

Then you have boring, plain-Jane Quito in Ecuador[^quito]. Daylight hours don't change much in Quito because it's near the equator. Truly no match for the grand stretch in Ireland.

<img src="{{ url_for('static', filename='img/daylight/ecuador.png') }}"
     alt="Ecuador's daylight hours. Remains constant at 12-ish hours"
     class="centered">

The beautiful, industrial-strength code behind these powerful and insightful graphs can be found at <https://github.com/Kevinpgalligan/daylight-plots>. There is no irony in the previous sentence. I pulled the sunrise and sunset data from <https://dateandtime.info/>.

[^quito]: I know nothing about Quito and I'm sure it's a lovely place.
