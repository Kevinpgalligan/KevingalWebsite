title: "Obese websites and Earth-sized metronomes"
date: 2020-05-19
description: Making an Earth-sized metronome app.
draft: yes

As you may have heard, we're in the midst of a crisis.

No, not that one. I'm talking about the [website obesity crisis](https://idlewords.com/talks/website_obesity.htm).

The [first ever webpage](http://info.cern.ch/hypertext/WWW/TheProject.html) was 2.2KB in size. It was a sparse list of links, accessed through black-screened terminals with green text. Pure, aerodynamic HTML.


<figure>
<img src="{{ url_for('static', filename='img/metronome/firstpage.png') }}"
     alt="The first ever webpage, published by Tim Berners-Lee in 1991. Black background, green text."
     class="centered">

<figcaption>The first webpage.</figcaption>
</figure>

Our expectations of the World Wide Web have grown since then, and so have websites. The median webpage size, as of 2020, is 2MB, and it has been increasing at a rate of about 165KB per year for the past 10 years<sup>[1](https://httparchive.org/reports/state-of-the-web)</sup>.

Images and scripts are mostly responsible for driving this newfound corpulence. Images account for almost 60% of the collective mass of the web, once you exclude the smallest and largest 10% of webpages. JavaScript accounts for a further 25%, followed by 6% for custom fonts, 5% for CSS, 3% for video, and a mere 2% for HTML. 

<figure>
<img src="{{ url_for('static', filename='img/metronome/breakdown.png') }}"
     alt="Bar chart showing the web's mass by resource type. HTML 2.01%, videos 2.75%, CSS 4.93%, fonts 6.35%, JavaScript 24.95%, images 59.01%."
     class="centered">

<figcaption>Percentage of the web's total mass taken up by common data types, after the smallest 10% and largest 10% of webpages have been excluded. These figures were extracted from the <a href="https://httparchive.org">HTTP Archive</a>. Methodology described in Appendix A.</figcaption>
</figure>

Most websites don't have this exact weight breakdown, but it's roughly what you can expect to download when you visit a large number of websites on the modern web.

The "average" 2MB webpage, then -- which is, in fact, a [myth](https://www.igvita.com/2016/01/12/the-average-page-is-a-myth/) -- comes with 1.2MB of image data. That's rather a lot of eyeball stimulation. The below portrait of William Howard Taft, the most voluminous ever president of the United States and basis of the [Taft Test](https://tafttest.com/), uses up only 1.5% of that 1.2MB budget. We can only assume either that modern websites include a *lot* of Taft-sized images, or that their images are excessively high resolution.

<img src="{{ url_for('static', filename='img/metronome/taft.jpg') }}"
     alt="A picture of President Taft, in black & white. He appears to be a good-humoured man. He has a moustache. He's sitting in a chair. He's rotund."
     class="centered">

And let's be honest with ourselves: most images on the internet have less utility and aesthetic value than Taft.

More offensive still, the "average" webpage is now bundled with 500KB of JavaScript. To put this in perspective, if you wrote 200 lines of code per day at 50 characters per line, it would take you 50 days to write that much JavaScript. Considering that the average website less interactive than a treestump, this is *astonishing* to me. You could fit 15 copies of the original Super Mario Bros into 500KB. Instead of a delightful platforming adventure worth hours of fun, you are instead subjected to ads, tracking scripts and mountains of garbage pulled in by the JavaScript ecosystem<sup>[2](http://lea.verou.me/2020/05/todays-javascript-from-an-outsiders-perspective/)</sup>.

To summarise our flying tour of the modern web: it's fat. Unhealthily so.

### "So what? The internet is faster now."
This might not seem like a big deal, BUT Mauritania. Look at this shit: <https://whatdoesmysitecost.com/#gniCost>

Hours of work required to pay for an xMB mobile plan / to load a site, this is a nice way to think about how expensive it is: <https://time.com/3589909/internet-next-billion-mobile/> OR <https://deviceatlas.com/blog/understanding-web-page-weight>

### Making an Earth-sized metronome
Recently, with this glum state of affairs at the back of my mind, I found myself in need of a metronome web app. A metronome, if you didn't know, is a tool that ticks at regular intervals. It's a music thing.

I didn't like any of the apps I found. They were extremely overweight (as large as 11.35MB), mobile-unfriendly (I can't express the horror of trying to set a precise numeric value with a slider), and full of trackers (hi Google!).

And so, I decided to make my own. It would be mobile-friendly and slider-free. To add spice to the task, it would also be less than 1KB in size. Relative to the 217KB bulk of the smallest metronome app I could find, 1KB seemed tight. But since people make [1KB JavaScript games for fun](https://js1k.com/), I was confident that it would be possible.

### The metronome: human-readable version
The final app, in its beautiful ugliness, can be found <a href="{{ url_for("specific_app", name="metronome") }}">here</a>. I'll first present the human-readable version of the source, which is quite a bit larger than 1KB.

The HTML is simple, consisting of just a play button and a few buttons to change the BPM (beats per minute) of the metronome. It weighs in at 777 bytes.

    :::html
    <html>
    <head>
        <title>Just a F*cking Metronome</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" type="text/css" href="/static/apps/metronome/m.css">
    </head>
    <body>
    <script src="/static/apps/metronome/a.js"></script>
    <div>
    <span id="bpm">100 bpm</span>
    <button id="change" class="play" onclick="change();">play</button>
    <button onclick="adjustBpm(-1);">-1</button>
    <button onclick="adjustBpm(1);">+1</button>
    <button onclick="adjustBpm(-5);">-5</button>
    <button onclick="adjustBpm(5);">+5</button>
    <button onclick="adjustBpm(-25);">-25</button>
    <button onclick="adjustBpm(25);">+25</button>
    </div>
    <p>Just a F*cking Metronome | <a href="/">kevingal.com</a></p>
    </body>
    </html>

The CSS is similarly light-weight, at 266 bytes. It includes some extravagant styling options, like colour for the play button, which I had to tear out later in order to get the size down.

    :::css
    div {
        display: grid;
        grid-template-columns: 50% 50%;
        max-width: 150px;
    }
    #bpm {
        text-align: center;
        height: 50px;
        line-height: 50px;
    }
    .play {
        background-color: #4CAF50;
    }
    .stop {
        background-color: red;
    }
    button {
        height: 50px;
    }

Finally, the JavaScript, which weighs in at a chunky 1573 bytes. When the user starts the metronome, it kicks off a scheduling function in the main thread that runs every 25 milliseconds. The scheduling function calls the WebAudio interface to scheule any beeps that are due to play in the next 100 milliseconds. WebAudio then plays the beeps in a separate thread. If we played the audio directly in the main thread, which is what I tried before [finding enlightenment](https://www.html5rocks.com/en/tutorials/audio/scheduling/), then there would be unacceptable delays in sound whenever we lost control of the main thread to the browser's UI and book-keeping functions.

    :::javascript
    var play = false;
    var bpm = 100;
    var interval = 0.025;
    var lookahead = 0.1;
    var nextNoteTime = null;

    var audioContext = new AudioContext();

    function run() {
        if (play) {
            schedule();
            setTimeout(run, interval);
        }
    }

    function schedule() {
        while (nextNoteTime < audioContext.currentTime + lookahead) {
            scheduleNote(nextNoteTime);
            nextNoteTime = calculateNextNoteTime(nextNoteTime);
        }
    }

    function scheduleNote(t) {
        var osc = audioContext.createOscillator();
        var vol = audioContext.createGain();
        osc.connect(vol);
        vol.connect(audioContext.destination);
        osc.frequency.value = 200;
        osc.start(t);
        osc.stop(t + 0.1);
        vol.gain.setValueAtTime(0.01, 0);
        vol.gain.exponentialRampToValueAtTime(1, t, t + 0.05);
        vol.gain.linearRampToValueAtTime(0, t + 0.1);
    }

    function calculateNextNoteTime(previousNoteTime) {
        return previousNoteTime + 60/bpm;
    }

    function change() {
        play = !play;
        if (play) {
            updatePlayButton("stop");
            nextNoteTime = audioContext.currentTime;
            run();
        } else {
            updatePlayButton("play");
        }
    }

    function updatePlayButton(s) {
        g("change").innerHTML = s;
        g("change").className = s;
    }

    function adjustBpm(d) {
        if (bpm+d < 20 || bpm+d > 240) {
            g("bpm").style.color = "red";
            setTimeout(function () {
                g("bpm").style.color = "black";
            }, 200);
        }
        bpm = Math.min(Math.max(20, bpm+d), 240);
        g("bpm").innerHTML = bpm + " bpm";
    }

    function g(id) {
        return document.getElementById(id);
    }

In total, this added up to **2616 bytes**, or 260% of the target size. It took quite a bit of effort to slim this down.

### The metronome: minified version
Here's the minified version, which weighs **971 bytes**. That's about 3.5 tweets.

    :::html
    <!DOCTYPE html><title>Metronome</title><meta name=viewport content=width=device-width>
    <style>body{display:grid;grid-template-columns:50%50%;max-width:150px}#b,button{
    text-align:center;line-height:50px}</style><script>p=!1,b=100,i=.025,l=.1,n=0,
    M=Math,D=document,x=new AudioContext,H=((e,n)=>e.innerHTML=n),h=((e,n)=>H(
    D.getElementById(e),n)),s=(()=>{for(;n<x.currentTime+l;)o=x.createOscillator(),
    v=x.createGain(),o.connect(v),v.connect(x.destination),o.frequency.value=200,o.start(n),
    o.stop(n+.1),G=v.gain,G.setValueAtTime(.01,0),G.exponentialRampToValueAtTime(1,n,n+.05),
    G.linearRampToValueAtTime(0,n+.1),n+=60/b}),r=(()=>p&&(s(),setTimeout(r,i))),
    c=(()=>{p=!p,p?(h("c","stop"),n=x.currentTime,r()):h("c","play")}),a=(e=>h("b",
    b=M.min(M.max(20,b+e),240))),window.onload=(()=>[-1,1,-5,5,-25,25].forEach(n=>
    (e=D.createElement("button"),e.addEventListener("click",()=>a(n)),H(e,""+n),
    D.body.appendChild(e))));</script><p id=b>100</p><button id=c onclick=c();>play</button>

To shrink the waistline of the website below the target 1KB, I employed many dirty tricks:

* Removing all content and style unrelated to the functionality of the metronome. No more pretty colours or descriptive text.
* Removing unnecessary HTML. Some tips described [here](https://blog.notryan.com/013.txt), for example. My favourite bits: 1) attributes don't need quotes if the value doesn't contain spaces, making `<p id=x>` the same as `<p id="x">`; and 2) if you omit &lt;html&gt;, &lt;head&gt; and &lt;body&gt; tags, they'll be generated by the browser.
* Applying various [JavaScript golf techniques](https://dev.to/emnudge/js-code-golfing-how-to-ruin-everyone-s-day-40h3), such as replacing all regular functions with arrow functions, replacing local variables with global ones, and reducing names to 1 letter.
* Running the HTML, CSS and JavaScript through minifiers to remove whitespace. The [JavaScript one](https://javascript-minifier.com/) in particular introduced some nice tricks, like replacing `false` with `!1`.
* Dynamically generating the BPM-changing buttons on page load, saving about 50 bytes. Here's the snippet: `window.onload=(()=>[-1,1,-5,5,-25,25].forEach(n=>(e=D.createElement("button"),e.addEventListener("click",()=>a(n)),H(e,""+n),D.body.appendChild(e))));`.

Functionally, it's exactly the same as before.

### The metronome: size comparison
Here's a size comparison of my metronome versus the top 7 metronomes (labelled A-G) that come up when you search "metronome online" with {Insert Evil Search Engine Here}. Note that the scale on the y-axis is logarithmic, so it goes from 1 to 10 to 100 to 1000, and so on. If it weren't logarithmic, then my metronome's bar would be microscopically tiny. Metronome A is 11.35MB (around 10<sup>8</sup> bytes), metronome G is 217.56KB (10<sup>6</sup>), and my metronome is 971 bytes (10<sup>3</sup>).

<img src="{{ url_for('static', filename='img/metronome/size.png') }}"
     alt="Bar chart comparing sizes of various metronome apps to mine."
     class="centered">

Yes, folks, you heard it here first: mine is smaller. To get a sense for just how much smaller, consider this depiction of Earth beside Jupiter (source: https://theplanets.org/jupiter/):

<img src="{{ url_for('static', filename='img/metronome/planets.jpg') }}"
     alt="todo"
     class="centered">

[Quote](https://www.universetoday.com/37124/volume-of-the-planets/):

> The largest planet in our Solar System, Jupiter’s size is astounding. Jupiter has a volume of 1.43 x 1015 cubic kilometers. To show what this number means, you could fit 1321 Earths inside of Jupiter. It is hard to imagine how large that actually is.

If you imagine my metronome app as Earth, then Metronome A has the volume of **8 Jupiters**. It is hard to imagine how large that actually is. Or what it could possibly be doing with all of that volume, since none of the larger metronomes offer any functionality beyond ticking at regular intervals. Although, admittedly, they're much prettier.

### Final thoughts
Based on my experiences, I would like to propose the First Law of Slim Websites:

> The size of a webpage should be proportional to its functionality.

That's it. There's no Second Law.

I'm not suggesting that everyone becomes a masochistic and tries to squeeze their website into the smallest possible size. While fun, it takes a lot of effort and tends to look like shit. Just make your websites Earth-sized, please. Or even Neptune-sized. Not everyone can afford to download 8 Jupiters.

### Appendix A: HTTP Archive query
The [HTTP Archive](https://httparchive.org) is a community-run effort to capture data and statistics about the web. Every month, millions of webpages are trawled and summary statistics are published on the HTTP Archive website. This data is publicly available.

After loading the HTTP Archive database into Google BigQuery (instructions [here](https://github.com/HTTPArchive/httparchive.org/blob/master/docs/gettingstarted_bigquery.md), takes 5 minutes), I ran this query to fetch the data I needed for my bar chart. 

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

It fetches data from April 1st, 2020. The bounds on bytesTotal are the p10 and p90 values of page size, plucked from [here](https://httparchive.org/reports/page-weight).

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

Result: 4,658,956. There are about 4,660,072 webpages in the database, making the answer over 99.9%.
