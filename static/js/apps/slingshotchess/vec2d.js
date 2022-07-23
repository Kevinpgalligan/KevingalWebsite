function vec2d(x, y) {
    return {x: x, y: y};
}

function vecX(v) {
    return v.x;
}

function vecY(v) {
    return v.y;
}

function addVec(v1, v2) {
    return vec2d(v1.x+v2.x, v1.y+v2.y);
}

function scaleVec(a, v) {
    return vec2d(a*v.x, a*v.y);
}

function subVec(v1, v2) {
    return vec2d(v1.x-v2.x, v1.y-v2.y);
}

function toIntegerVec(v) {
    return vec2d(Math.round(v.x), Math.round(v.y));
}

function isZeroVec(v) {
    return v.x == 0 && v.y == 0;
}

function toUnitVec(v) {
    var length = vecLength(v);
    return vec2d(v.x/length, v.y/length);
}

function vecLength(v) {
    return Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2));
}

function euclideanDistance(v1, v2) {
    return vecLength(subVec(v1, v2));
}

function dotProduct(v1, v2) {
    return v1.x*v2.x + v1.y*v2.y;
}

function direction(v1, v2) {
    return toUnitVec(subVec(v2, v1));
}

function projectOnto(v1, v2) {
    // Projects v1 onto v2.
    var v2AsUnit = toUnitVec(v2);
    return scaleVec(dotProduct(v1, v2AsUnit), v2AsUnit);
}
