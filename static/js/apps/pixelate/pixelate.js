var imageData = null;

function saveImageData(event) {
    var selectedFile = event.target.files[0];

    var canvas = document.getElementById("input-img-display");
    var ctx = canvas.getContext('2d');
    var img = new Image();
    img.onload = function() {
        canvas.width = img.width;
        canvas.height = img.height;
        ctx.drawImage(img, 0, 0);
        imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
        drawPixelated();
    }
    img.src = URL.createObjectURL(selectedFile);
}

function drawPixelated() {
    var blockSize = parseInt(document.getElementById("block-size").value);
    var newBlockSize = parseInt(document.getElementById("new-block-size").value);
    var blackAndWhite = document.getElementById("black-n-white").checked;
    var lightnessThreshold = parseInt(document.getElementById("lightness-threshold").value);

    var blockWidth = Math.ceil(imageData.width / blockSize);
    var blockHeight = Math.ceil(imageData.height / blockSize);

    var outputWidth = blockWidth * newBlockSize;
    var outputHeight = blockHeight * newBlockSize;
    var outputCanvas = document.getElementById("output-img-display");
    outputCanvas.width = outputWidth;
    outputCanvas.height = outputHeight;
    var ctx = outputCanvas.getContext('2d');
    var pixelatedImg = ctx.createImageData(outputWidth, outputHeight);

    for (var i = 0; i < imageData.height; i += blockSize) {
        for (var j = 0; j < imageData.width; j += blockSize) {
            // Calculate average of RGBA in the block.
            var blockValues = [0, 0, 0, 0]; // R, G, B, A
            var blockElements = 0;
            for (var blockI = 0; blockI < Math.min(blockSize, imageData.height - i); blockI++) {
                for (var blockJ = 0; blockJ < Math.min(blockSize, imageData.width - j); blockJ++) {
                    var rgba = getPixel(imageData, i + blockI, j + blockJ);
                    for (var pixelIndex = 0; pixelIndex < rgba.length; pixelIndex++) {
                        blockValues[pixelIndex] += rgba[pixelIndex];
                    }
                    blockElements++;
                }
            }
            for (var blockValIndex = 0; blockValIndex < blockValues.length; blockValIndex++) {
                blockValues[blockValIndex] = Math.round(blockValues[blockValIndex] / blockElements);
            }
            // Black&white-ify the pixels in the block, if necessary.
            if (blackAndWhite) {
                blockValues[3] = 255;
                // Standard formula for estimating "luminance" or "brightness" of RGB.
                var lightness = Math.round(0.2126*blockValues[0] + 0.7152*blockValues[1] + 0.0722*blockValues[2]);
                var newVal = (lightness > lightnessThreshold) ? 255 : 0;
                for (var blockValIndex = 0; blockValIndex < 3; blockValIndex++) {
                    blockValues[blockValIndex] = newVal;
                }
            }
            // Now set pixels based on calculated average in block.
            var pixelatedI = newBlockSize * Math.floor(i / blockSize);
            var pixelatedJ = newBlockSize * Math.floor(j / blockSize);
            for (var blockI = 0; blockI < newBlockSize; blockI++) {
                for (var blockJ = 0; blockJ < newBlockSize; blockJ++) {
                    setPixel(pixelatedImg, pixelatedI + blockI, pixelatedJ + blockJ, blockValues);
                }
            }
        }
    }
    ctx.putImageData(pixelatedImg, 0, 0);
}

function getPixel(img, i, j) {
    var indexes = pixelIndexes(img, i, j);
    for (var k = 0; k < indexes.length; k++) {
        indexes[k] = img.data[indexes[k]];
    }
    return indexes;
}

function setPixel(img, i, j, values) {
    var indexes = pixelIndexes(img, i, j);
    for (var k = 0; k < indexes.length; k++) {
        img.data[indexes[k]] = values[k];
    }
}

function pixelIndexes(img, i, j) {
    var red = i * (img.width * 4) + j * 4;
    return [red, red + 1, red + 2, red + 3];
}
