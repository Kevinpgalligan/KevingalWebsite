title: "The fat web meets a 971-byte metronome"
date: 2020-05-19
description: Making an Earth-sized metronome app.
draft: yes
Sources:

You might have heard already, but we're in the middle of a pandemic.

A [website obesity pandemic](https://idlewords.com/talks/website_obesity.htm).

The median webpage size in 2020 is 2MB<sup>[1](https://httparchive.org/reports/state-of-the-web)</sup>. That's almost 1000 times larger than the [first ever website](http://info.cern.ch/hypertext/WWW/TheProject.html), which was published by Tim Berners-Lee in 1991 and which consisted of 2.4 kilobytes (KB) of pure HTML.

In this article, we'll explore just how big the typical webpage has become. Frustrated by the inaccessibility and bloatedness of a selection of metronome apps, I was driven to create my own. For fun, I made it in less than 1KB. So I'll present that too and compare it to the metronome apps I found on the web.


### The obese web
According to the HTTP Archive<sup>[2](https://httparchive.org/reports/page-weight)</sup>, the average webpage now includes on top of that 2.4KB an extra 23KB of HTML. Something something understandable.

On top of that extra HTML, typically, you see 980KB of images. Said images may or may not pass the [Taft Test](https://tafttest.com/), a sanity check that tests whether the aesthetic of a webpage would be improved by replacing all of its images with pictures of William Howard Taft, the most voluminous ever president of the U.S.

You then have 450KB of JavaScript. To get an idea of how much code that is, assume that each line consists of 50 characters, and that you can write 200 lines of code per day. It would then take you 45 days to write all of that JavaScript. Considering how much dynamic functionality is actually necessary to use the typical web page - which appears to be just text and possibly a few images - this seems *preposterous*. We can only account for this fat through ads, tracking, and bloat that has been dragged in from the JavaScript ecosystem<sup>[3](missing link)</sup>.

Finally, you have 1392KB of video. TODO figure out if this includes stuff like YouTube videos. And clarify why the figures don't add up (450+980+1392>2000), or rephrase such that this clarification isn't necessary.

### So what? Computers are faster now
This might not seem like a big deal, BUT Mauritania. Look at this shit: <https://whatdoesmysitecost.com/#gniCost>

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

### Further reading
* [The Average Page is a Myth](https://www.igvita.com/2016/01/12/the-average-page-is-a-myth/)
