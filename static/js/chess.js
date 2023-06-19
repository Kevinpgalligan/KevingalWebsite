function expectedScore(r1, r2) {
    return 1/(1 + Math.pow(10, (r2-r1)/400));
}

function updateExpectedScore() {
    var D = s => document.getElementById(s);
    var r1 = parseInt(D("p1").value);
    var r2 = parseInt(D("p2").value);
    D("p1out").innerHTML = r1;
    D("p2out").innerHTML = r2;
    D("result").innerHTML = expectedScore(r1, r2);
}
