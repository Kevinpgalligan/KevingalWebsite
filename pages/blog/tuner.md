title: "Tiny Metronome now has a 903-byte sibling"
date: 2024-02-22
description: It's a 903-byte guitar tuner.
requires: code
publish: y
tags: web

With the help of Charlie from RC, I managed to make a [tiny guitar tuner](/apps/tuner.html) in 903 bytes. It uses the same code-golfing tricks as the [tiny metronome](/blog/metronome.html): `!1` is `false`, HTML attribute values don't need to be surrounded by quotes, ternary conditionals can be shorter than if-else statements (`p?(s1,s2):(s3,s4)` saves 8 bytes over `if(p){s1;s2;}else{s3;s4;}`), buttons are created dynamically with JavaScript, etc.

It uses the WebAudio API to make sound. The most annoying part was making sure there were no pops or crackles in the audio output, especially when it was started and stopped abruptly. This was possible to resolve by ramping the gain (kinda like the volume) up and down, and by tweaking its max value.

Here's the code, with newlines added so that it doesn't spill out of the page. Cower before its obscurity!

    :::html
    <!DOCTYPE html><title>Tuner</title><meta name=viewport content=width=
    device-width><style>body{display:grid;grid-template-columns:50%50%;
    max-width:150px}#b,button{text-align:center;line-height:50px}</style>
    <script>f=[82,110,147,196,247,330];N="E,A,D,G,B,E".split(",");
    p=F=x=!1;I=.025;l=.1;n=0;L=(G,v,t)=>G.linearRampToValueAtTime(v,t);
    D=document;x=new AudioContext;s=(()=>{if(n<=x.currentTime+l)
    {var o=x.createOscillator();var v=x.createGain();o.connect(v);
    v.connect(x.destination);o.frequency.value=F;G=v.gain;
    G.setValueAtTime(0.01,n);L(G,(1*(F<111?1.3:1))/15,n+.01);
    L(G,0,n+1.9);o.start(n);n+=2;setTimeout(()=>(v.disconnect(),
    o.disconnect()),5000);}});r=(()=>p&&(s(),setTimeout(r,I)));
    window.onload=(()=>[...Array(6)].map((_,i)=>(e=D.createElement("button"),
    e.innerHTML=N[i],e.addEventListener("click",()=>(p&&(F==f[i])?(p=!1,F=0)
    :(p=!0,n=x.currentTime,F=f[i],r()))),D.body.appendChild(e))));</script>

