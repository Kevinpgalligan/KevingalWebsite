title: "Just a Fucking Metronome: an N-byte metronome app"
date: 2020-05-01
description: An ad-free, no-bullshit, human-friendly metronome web app.
draft: yes

You might have heard already, but we're in the middle of a pandemic.

A [website obesity pandemic](https://idlewords.com/talks/website_obesity.htm).

The weight of the average website in 2017 was 3MB [[1]](https://discuss.httparchive.org/t/tracking-page-weight-over-time/1049/2). That's 3.5 times the size of a copy of The Brothers Karamazov [[2]](http://www.gutenberg.org/ebooks/28054). Often, less than 1% of the weight is actual content. Most of the weight comes from ads and uncompressed, [Taft test](https://tafttest.com/)-failing images. This gets worse every year, making the web slower and less accessible for everyone.

Recently, with this glum state of affairs on the back of my mind, I was searching for a metronome web app. A metronome, by the way, is a tool that emits sound at fixed intervals, allowing you to practice playing music to a regular beat. I didn't like any of them. Either they were extremely overweight (as large as 11.35MB), or they were mobile-unfriendly (it's annoying to set numeric values precisely using a slider, like the "beats per minute" (BPM) of a metronome), or they were full of trackers (hi Google and Facebook!).

And so, I decided to make my own. Mobile-friendliness would be essential. I also wanted to design the interface without sliders, because *fuck sliders*. Finally, to add spice to the task, I wanted to make the app less than 1KB in size. The smallest metronome app I found was 217KB, but since people make [1KB JavaScript games for fun](https://js1k.com/), I was confident that this would be possible.

### Human-readable version (N KB)
The final app can be found <a href="{{ url_for("specific_app", name="metronome") }}">here</a>. I'll first present the human-readable version, which is quite a bit larger than the minified version.

The HTML is simple, consisting of just a play button and a few buttons to change the BPM of the metronome. It weighs in at 777 bytes.

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

The CSS is similarly light-weight, at 266 bytes. It includes some luxurious styling options, like colouring the play button red or green.

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

Finally, the JavaScript, which weighs in at a chunky 1573 bytes. In the main thread, when the user starts the metronome, it kicks off a scheduling function that runs every 25 milliseconds. The scheduling function calls the WebAudio interface to scheule any beeps that are due to play in the next 100 milliseconds. WebAudio then plays the beeps in a separate thread. If we played the audio directly in the main thread (which is what I tried before reading [this excellent article](https://www.html5rocks.com/en/tutorials/audio/scheduling/)), then there would be unacceptable delays in sound whenever we lost control to the browser's UI and book-keeping functions.

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
    function g(id) {
        return document.getElementById(id);
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

### Minified version (976 bytes)
To shrink the waistline of the website below the target 1KB, I employed a few strategies:

* Removing unnecessary HTML. Described [here](https://blog.notryan.com/013.txt), for example. Highlights: tag attributes not needing quotes if the attribute value doesn't contain spaces, making `<p id=x>` the same as `<p id="x">`; and being able to omit \<html\>, \<head\> and \<body\> tags.
* Making IDs, variable names, function names etc. as small as possible.

After going through all that, I was left with a 1-line, 976-byte file. It was more space efficient to include the CSS and JavaScript inline. Other dirty tricks worth mentioning: using JS to generate the buttons...what else? Removed unnecessary stuff. Extravagant use of colour.

How to make the 1-liner wrap?

    :::html
    <!DOCTYPE html><title>Metronome ⌛</title><meta name=viewport content=width=device-width><style>body{display:grid;grid-template-columns:50%50%;max-width:150px}#b,button{text-align:center;line-height:50px}</style><script>p=!1,b=100,i=.025,l=.1,n=0,M=Math,D=document,x=new AudioContext,H=((e,n)=>e.innerHTML=n),h=((e,n)=>H(D.getElementById(e),n)),s=(()=>{for(;n<x.currentTime+l;)o=x.createOscillator(),v=x.createGain(),o.connect(v),v.connect(x.destination),o.frequency.value=200,o.start(n),o.stop(n+.1),G=v.gain,G.setValueAtTime(.01,0),G.exponentialRampToValueAtTime(1,n,n+.05),G.linearRampToValueAtTime(0,n+.1),n+=60/b}),r=(()=>p&&(s(),setTimeout(r,i))),c=(()=>{p=!p,p?(h("c","stop"),n=x.currentTime,r()):h("c","play")}),a=(e=>h("b",b=M.min(M.max(20,b+e),240))),window.onload=(()=>[-1,1,-5,5,-25,25].forEach(n=>(e=D.createElement("button"),e.addEventListener("click",()=>a(n)),H(e,""+n),D.body.appendChild(e))));</script><p id=b>100</p><button id=c onclick=c();>play</button>


Golfing: https://dev.to/emnudge/js-code-golfing-how-to-ruin-everyone-s-day-40h3

Minifier: https://javascript-minifier.com/

### Comparison
5.18MB, 11.35MB, 546.96KB, 374.94KB, 591.47KB, 217.56KB, 3.87MB
