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
    osc.connect(audioContext.destination);
    osc.frequency.value = 220.0;
    osc.start(t);
    osc.stop(t + 0.05);
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
