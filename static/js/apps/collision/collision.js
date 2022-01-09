var K_LIMIT = 100000;
var DIGITS = 5;

/*
For future me, this may allow a fix for the numerical precision issues:

  https://stackoverflow.com/questions/16742578/bigdecimal-in-javascript

Or: use fancy numerical tricks to avoid dealing with very large or very small values, e.g. taking logs.

Cases that would be nice to fix:
    k=1000, N=2^64 (exact method gives probability=0)
    k=1000, N=2^128 (even approximate method gives probability=0)
*/

function updateResults() {
    setError("");
    var k = parseInt(document.getElementById("items").value);
    var N = parseInt(document.getElementById("buckets").value);
    var buckettype = document.getElementById("buckettype").value;
    if (buckettype === "bits") {
        N = Math.pow(2, N);
    }
    if (k != k || N != N) {
        // They're NaN!
        setError("Invalid input!");
        return;
    }
    if (N == Infinity) {
        setError("N is too large!");
        return;
    }
    if (k < 0 ) {
        setError("k must be non-negative!");
        return;
    }
    if (N <= 0) {
        setError("N must be non-negative!");
        return;
    }
    var resultIds = ["exact-prob", "approx-prob", "rel-err"];
    resultIds.forEach((freud) => document.getElementById(freud).innerHTML = "...");
    var tooHard = k <= N && k > K_LIMIT;
    var exactProb = null;
    if (!tooHard) {
        exactProb = bdayProb(k, N);
    }
    var approxProb = approxBdayProb(k, N);
    document.getElementById("approx-prob").innerHTML = makeFloatPresentable(approxProb);
    document.getElementById("exact-prob").innerHTML = tooHard ? "wahh, too hard!" : makeFloatPresentable(exactProb);
    document.getElementById("rel-err").innerHTML = tooHard ? "ditto" : makeFloatPresentable(relativeError(exactProb, approxProb));
}

function setError(msg) {
    if (msg.length > 0) {
        msg = "error: " + msg;
    }
    document.getElementById("error-box").innerHTML = msg;
}

function bdayProb(k, N) {
    if (k > N) {
        return 1.0;
    }
    var result = 1.0;
    for (var i = 0; i < k; i = i+1) {
        var term = 1 - i/N;
        if (term > 0) {
            result *= term;
        }
    }
    return 1 - result;
}

function approxBdayProb(k, N) {
    if (k > N) {
        return 1.0;
    }
    return 1 - Math.exp((-k*(k-1)/(2*N)));
}

function relativeError(x, y) {
    if (x == y) {
        return 0;
    }
    return Math.abs(x-y)/x;
}

function makeFloatPresentable(e) {
    if ((parseFloat(trimDigits(e, DIGITS)) == 0) && e != 0) {
        // Too many zeros, use scientific notation.
        e = e.toExponential().toString();
        if (e === trimPostPeriodDigits(e)) {
            return e;
        }
        return "~" + trimPostPeriodDigits(e);
    }
    // Don't need huuuuge precision or anything.
    var s = trimDigits(e, DIGITS);
    if (s !== e.toString()) {
        s = "~" + s;
    }
    return s;
}

function trimDigits(f, n) {
    var potensh = f.toFixed(n);
    if (f.toString().length <= potensh.length) {
        return f.toString();
    }
    return potensh;
}

// Cool name.
function trimPostPeriodDigits(e) {
    e = e.toString();
    let periodI = e.search(".");
    let expI = e.search("e");
    return e.substring(0, periodI+1)
        + e.substring(periodI+1, Math.min(periodI+1+DIGITS, expI))
        + e.substring(expI);
}
