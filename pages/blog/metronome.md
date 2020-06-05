title: "Obese websites and planet-sized metronomes"
date: 2020-06-05
description: Publishing an Earth-sized metronome app on the fat web.
draft: yes

As you may have heard, we're in the midst of a crisis.

No, not that one. I'm talking about the [website obesity crisis](https://idlewords.com/talks/website_obesity.htm).

The homepage of the [first ever website](http://info.cern.ch/hypertext/WWW/TheProject.html), published by Tim Berners-Lee in 1991, was 2.2KB in size. It was a sparse list of links, accessed through black-screened terminals with green text. Pure, aerodynamic HTML.

<figure>
<img src="{{ url_for('static', filename='img/metronome/firstpage.png') }}"
     alt="The first ever webpage, published by Tim Berners-Lee in 1991. Black background, green text."
     class="centered">

<figcaption>The first webpage.</figcaption>
</figure>

Our expectations of the World Wide Web have grown since then, and so have websites. The median webpage size in 2020 is 2MB, and it has been increasing at a rate of about 165KB per year for the past 10 years<sup>[1](https://httparchive.org/reports/state-of-the-web)</sup>. It is now 800 times larger than Tim's original homepage.

Images and scripts are responsible for driving this newfound corpulence. Images account for almost 60% of the collective mass of the web, once you exclude the smallest 10% and largest 10% of webpages. A further 25% is used for JavaScript, followed by 6% for custom fonts, 5% for CSS, and 3% for video, until finally, a mere 2% of the pie is left for HTML.

<figure>
<img src="{{ url_for('static', filename='img/metronome/breakdown.png') }}"
     alt="Bar chart showing the web's mass by resource type. HTML 2.01%, videos 2.75%, CSS 4.93%, fonts 6.35%, JavaScript 24.95%, images 59.01%."
     class="centered">

<figcaption>Percentage of the web's total mass taken up by common data types, after the smallest 10% and largest 10% of webpages have been excluded. These figures were extracted from the <a href="https://httparchive.org">HTTP Archive</a>. Methodology described in Appendix A.</figcaption>
</figure>

Most websites don't have this exact breakdown of weight, but it's roughly what you can expect to download when you visit a large number of websites on the modern web.

The "average" 2MB webpage, then -- which, in fact, [doesn't exist](https://www.igvita.com/2016/01/12/the-average-page-is-a-myth/), but is nonetheless a useful concept -- comes with 1.2MB of image data. That's rather a lot of eyeball stimulation. The below portrait of William Howard Taft, the most voluminous ever president of the United States and basis of the [Taft Test](https://tafttest.com/), uses up just 1.5% of that 1.2MB budget. We can only assume either that modern websites include a *lot* of Taft-sized images, or that their images are absurdly big.

<figure>
<img src="{{ url_for('static', filename='img/metronome/taft.jpg') }}"
     alt="A picture of President Taft, in black & white. He appears to be a good-humoured man. He has a moustache. He's sitting in a chair. He's rotund."
     class="centered">

<figcaption>President Taft, 1857-1930.</figcaption>
</figure>

And let's be honest with ourselves: most images on the internet have less utility and aesthetic value than Taft.

More offensive still, the "average" webpage is now bundled with 500KB of JavaScript. If you wrote 200 lines of code per day at 50 characters per line, it would take 50 days to write that much JavaScript. This is astonishing, when you consider that the average website is less interactive than a tree stump. You could fit 15 copies of the original Super Mario Bros into 500KB. Yet, rather than a delightful platforming adventure worth hours of fun, we are instead subjected to ads, tracking scripts and [mountains of garbage](http://lea.verou.me/2020/05/todays-javascript-from-an-outsiders-perspective/) pulled in by the JavaScript ecosystem.

To summarise our flying tour of the modern web: it's fat. Unhealthily so.

### The cost of the fat web
While some of us live in parts of the world with affordable, fast internet, others are not so fortunate. As of May 2020, a 2MB webpage is worth approximately 6 minutes of labour in Mauritania ([whatdoesmysitecost.com](https://whatdoesmysitecost.com/#gniCost) estimates that the download would cost 1.29% of Mauritania's daily gross national income per capita, and 1.29% of an 8-hour workday is ~6 minutes). Imagine having to work a full hour in order to earn the privilege of reading 10 webpages!

A [2014 Time article](https://time.com/3589909/internet-next-billion-mobile/) predicted that the next billion internet adoptees would be smartphone users. If we want the web to remain accessible for these newcomers, who presumably have crap hardware and crap networks, then the slimmer it is, the better.

The overweight web also has an environmental impact. The internet as a whole consumes more electricity than the entire United Kingdom<sup>[2](https://www.websitecarbon.com/)</sup>. So as well as having shorter showers, consider shedding some of the dead weight from your website.

### Making an Earth-sized web app
Recently, with this glum state of affairs at the back of my mind, I found myself in need of a metronome web app. A metronome, if you didn't know, is a tool that ticks at regular intervals. It's a music thing.

I didn't like any of the apps I found on the web. They were extremely overweight (as large as 11.35MB), mobile-unfriendly (I can't express the horror of trying to set precise numeric values with a slider), and full of trackers (hi Google!).

And so, I set myself a challenge: to make my own, mobile-friendly, slider-free metronome app, with the constraint that it be less than 1KB in size. That's half the size of Tim Berners-Lee's original webpage. A tight margin, for sure, but since people make [1KB JavaScript games](https://js1k.com/) for fun, I was confident that it would be possible.

The finished app can be found <a href="{{ url_for("specific_app", name="metronome") }}">here</a>. The first version was 2616 bytes, but by applying various dirty tricks (detailed in Appendix B, along with some technical details of the app), I managed to shrink it down to 981 bytes, or ~3.5 tweets.

    :::html
    <!DOCTYPE html><title>Metronome</title><meta name=viewport content=width=device-width>
    <style>body{display:grid;grid-template-columns:50%50%;max-width:150px}
    #b,button{text-align:center;line-height:50px}</style><script>p=!1,b=100,i=.025,
    l=.1,n=0,M=Math,D=document,x=new AudioContext,H=((e,n)=>e.innerHTML=n),
    h=((e,n)=>H(D.getElementById(e),n)),s=(()=>{for(;n<x.currentTime+l;)o=x.createOscillator(),
    v=x.createGain(),o.connect(v),v.connect(x.destination),o.frequency.value=200,o.start(n),
    o.stop(n+.1),G=v.gain,G.setValueAtTime(.01,0),G.exponentialRampToValueAtTime(1,n,n+.05),
    G.linearRampToValueAtTime(0,n+.1),n+=60/b}),r=(()=>p&&(s(),setTimeout(r,i))),
    c=(()=>{p=!p,p?(h("c","stop"),n=x.currentTime,r()):h("c","play")}),
    a=(e=>h("b",b=M.min(M.max(20,b+e),240))),window.onload=(()=>[-1,1,-5,5,-25,25].forEach(
    n=>(e=D.createElement("button"),e.addEventListener("click",()=>a(n)),H(e,(n<0?"":"+")+n),
    D.body.appendChild(e))));</script><p id=b>100</p><button id=c onclick=c();>play</button>

The purpose of this exercise? Mostly as a point of comparison for other metronome apps on the market, which I believe are symptomatic of the horrid state of the web, and which we will examine in the next section.

### Comparing size
Here's a size comparison of my metronome versus the top 7 metronomes (labelled A-G) that come up when you search "metronome online" with Generic Search Engine. The y-axis is on a logarithmic scale, which means that it counts from 1 to 10 to 100, instead of 1 to 2 to 3. Otherwise, some of the bars would be microscopically tiny. Metronome A is 11.35MB (around 10<sup>8</sup> bytes), metronome G is 217.56KB (10<sup>6</sup>), and my metronome is 981 bytes (10<sup>3</sup>).

<img src="{{ url_for('static', filename='img/metronome/size.png') }}"
     alt="Bar chart comparing sizes of various metronome apps to mine."
     class="centered">

Yes, folks, you heard it here first: mine is smaller. To get a sense for how much smaller, consider this depiction of Earth and Jupiter ([source](https://theplanets.org/jupiter/)):

<img src="{{ url_for('static', filename='img/metronome/planets.jpg') }}"
     alt="todo"
     class="centered">

Quoting universetoday.com<sup>[3](https://www.universetoday.com/37124/volume-of-the-planets/)</sup>:

> The largest planet in our Solar System, Jupiter’s size is astounding. Jupiter has a volume of 1.43 x 1015 cubic kilometers. To show what this number means, you could fit 1321 Earths inside of Jupiter. It is hard to imagine how large that actually is.

If my metronome app were Earth, then Metronome A would have the volume of **eight Jupiters**. It is hard to imagine how large that actually is. Or what it could possibly be doing with all of that volume, since none of the larger metronomes offer any functionality beyond basic ticking. Although, admittedly, they're much prettier.

### Final thoughts
I'm not suggesting that we all become masochist monks, purging the tiniest strips of fat from our websites with puritan fury. While fun, compressing a website takes a lot of effort, and the end result tends to look like shit. Let's just try to make our websites Earth-sized, please, or even Neptune-sized. This can be achieved by not inflicting humongous images and obscene amounts of JavaScript on the denizens of the web. Not everyone can afford to download Jupiter.

### Appendix A: HTTP Archive query
The [HTTP Archive](https://httparchive.org) is a community-run effort to capture data and statistics about the web. Every month, they trawl millions of webpages and publish summary statistics on their website. The data is publicly available.

After loading the HTTP Archive database into Google BigQuery (instructions [here](https://github.com/HTTPArchive/httparchive.org/blob/master/docs/gettingstarted_bigquery.md), it's free and takes 5 minutes), I ran this query to fetch the data I needed for my bar chart. 

    :::sql
    SELECT
      SUM(bytesHtml) as html,
      SUM(bytesJS) as js,
      SUM(bytesCSS) as css,
      SUM(bytesImg) as img,
      SUM(bytesFont) as font,
      SUM(bytesVideo) as video,
      COUNT(*) as count
    FROM
      `httparchive.summary_pages.2020_04_01_desktop`
    WHERE
      bytesTotal >= 456601 AND bytesTotal <= 7363789

It loads data from April 1st, 2020. The bounds on bytesTotal are the p10 and p90 values of page size, plucked from [here](https://httparchive.org/reports/page-weight).

And here are the results:

    html 188774196140
    js 2337263808937
    css 462249675219
    img 5528893867126
    font 595097490698
    video 257342218994
    count 3728058

Another interesting query I ran: how many webpages have at least one image?

    :::sql
    SELECT
      COUNT(*) as count
    FROM
      `httparchive.summary_pages.2020_04_01_desktop`
    WHERE
      bytesImg > 0

Result: 4,658,956. There are about 4,660,072 webpages in the database, making the answer more than 99.9%.

### Appendix B: website golf
Here I'll describe how the <a href="{{ url_for("specific_app", name="metronome") }}">metronome web app</a> works, and also the series of gruesome hacks I employed to shrink it below 1KB.

When the user starts the metronome, it kicks off a scheduling function in the main thread that runs every 25 milliseconds. The scheduling function calls JavaScript's WebAudio interface to scheule any beeps that are due to play in the next 100 milliseconds. WebAudio then plays the beeps in a separate thread. If we played the audio directly in the main thread, which is what I tried before [finding enlightenment](https://www.html5rocks.com/en/tutorials/audio/scheduling/), then there would be unacceptable delays in sound whenever we lost control of the main thread to the browser's UI and book-keeping functions.

And here are some of the things I tried to make the app smaller:

* Removing unnecessary HTML. Some tips described [here](https://blog.notryan.com/013.txt). My favourite bits: 1) attributes don't need quotes if the value doesn't contain spaces, making `<p id=x>` the same as `<p id="x">`; and 2) if you omit &lt;html&gt;, &lt;head&gt; and &lt;body&gt; tags, they'll be generated by the browser.
* Various [JavaScript golf techniques](https://dev.to/emnudge/js-code-golfing-how-to-ruin-everyone-s-day-40h3), such as replacing all regular functions with arrow functions, replacing local variables with global ones, and reducing names to 1 letter.
* Running the HTML, CSS and JavaScript through minifiers to remove whitespace. The [JavaScript one](https://javascript-minifier.com/) in particular introduced some neat abbreviations, like replacing `false` with `!1`.
* Programmatically generating some of the buttons on page load, saving about 50 bytes.
