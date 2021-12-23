var K_LIMIT = 100000;
var DIGITS = 5;

// Test cases to iron out:
// 1. k=1000, N=2^64 (gives p_e=0). Should be able to handle this case, need a smarter dumb algorithm.
// 2. k=1000, N=2^1000000 (gives p_e=NaN and p_a=0; should just error out if N is too big).
// 3. k=0, N=0 (don't accept bad input).
// 4. k=-1, N=-1 (don't accept bad input).
// ALSO, it should react as you type, you shouldn't have to press enter.

function updateResults() {
    var k = parseInt(document.getElementById("items").value);
    var N = parseInt(document.getElementById("buckets").value);
    var buckettype = document.getElementById("buckettype").value;
    if (buckettype === "bits") {
        N = Math.pow(2, N);
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

function bdayProb(k, N) {
    if (k > N) {
        return 1.0;
    }
    var result = 1.0;
    for (var i = 0; i < k; i = i+1) {
        // May lose precision if individual terms are too small.
        result *= (N-i)/N;
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
