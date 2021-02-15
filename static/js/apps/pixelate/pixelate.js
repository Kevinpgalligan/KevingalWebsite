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
    var invertedBW = document.getElementById("inverted-bw").checked;
    var dither = document.getElementById("dithered").checked;

    var blockWidth = Math.ceil(imageData.width / blockSize);
    var blockHeight = Math.ceil(imageData.height / blockSize);

    // Intermediate grid, save the block RGB values here before
    // they're blown up.
    var blocks = [];

    var outputWidth = blockWidth * newBlockSize;
    var outputHeight = blockHeight * newBlockSize;
    var outputCanvas = document.getElementById("output-img-display");
    outputCanvas.width = outputWidth;
    outputCanvas.height = outputHeight;
    var ctx = outputCanvas.getContext('2d');
    var pixelatedImg = ctx.createImageData(outputWidth, outputHeight);

    // First pass. Group pixels into blocks and average them.
    var blockIndex = 0;
    for (var i = 0; i < imageData.height; i += blockSize) {
        blocks.push([]);
        for (var j = 0; j < imageData.width; j += blockSize) {
            // Calculate average of RGBA in the block.
            var blockRGB = [0, 0, 0, 0]; // R, G, B, A
            var pixelsInBlock = 0;
            for (var blockI = 0; blockI < Math.min(blockSize, imageData.height - i); blockI++) {
                for (var blockJ = 0; blockJ < Math.min(blockSize, imageData.width - j); blockJ++) {
                    var rgba = getPixel(imageData, i + blockI, j + blockJ);
                    for (var pixelIndex = 0; pixelIndex < rgba.length; pixelIndex++) {
                        blockRGB[pixelIndex] += rgba[pixelIndex];
                    }
                    pixelsInBlock++;
                }
            }
            for (var blockValIndex = 0; blockValIndex < blockRGB.length; blockValIndex++) {
                blockRGB[blockValIndex] = Math.round(blockRGB[blockValIndex] / pixelsInBlock);
            }
            blocks[blockIndex].push(blockRGB);
        }
        blockIndex += 1;
    }

    // Second pass. B&W-ify the blocks, if required.
    if (blackAndWhite) {
        for (var i = 0; i < blockHeight; i += 1) {
            for (var j = 0; j < blockWidth; j += 1) {
                var rgba = blocks[i][j];
                var lightness = calcLuminance(rgba);
                var newVal;
                if (invertedBW) {
                    newVal = (lightness > lightnessThreshold) ? 0 : 255;
                } else {
                    newVal = (lightness > lightnessThreshold) ? 255 : 0;
                }
                if (dither) {
                    // Propagate error to neighbour pixels using
                    // the Floyd-Steinberg algorithm.
                    [[0,  1, 7/16],
                     [1, -1, 3/16],
                     [1,  0, 5/16],
                     [1,  1, 1/16]].forEach((params) => {
                        var propI = i+params[0];
                        var propJ = j+params[1];
                        var multiplier = params[2];
                        if (0 <= propI && propI < blockHeight
                                && 0 <= propJ && propJ < blockWidth) {
                            blocks[propI][propJ][0] += multiplier*(rgba[0]-newVal);
                            blocks[propI][propJ][1] += multiplier*(rgba[1]-newVal);
                            blocks[propI][propJ][2] += multiplier*(rgba[2]-newVal);
                        }
                    });
                }
                blocks[i][j] = [newVal, newVal, newVal, 255];
            }
        }
    }

    // Final pass, blow the blocks up to their final size.
    // (Each block can be multiple pixels).
    for (var blockI = 0; blockI < blockHeight; blockI++) {
        for (var blockJ = 0; blockJ < blockWidth; blockJ++) {
            var blockRGB = blocks[blockI][blockJ];
            for (var pixelI = 0; pixelI < newBlockSize; pixelI++) {
                for (var pixelJ = 0; pixelJ < newBlockSize; pixelJ++) {
                    setPixel(pixelatedImg,
                             newBlockSize*blockI + pixelI,
                             newBlockSize*blockJ + pixelJ,
                             blockRGB);
                }
            }
        }
    }

    // Now write the image to the page.
    ctx.putImageData(pixelatedImg, 0, 0);
    document.getElementById('outputResolution').innerHTML = outputWidth + "x" + outputHeight + " (" + (outputWidth * outputHeight) + " pixels)";
}

function calcLuminance(rgba) {
    // Probably stole this from Wikipedia.
    return Math.round(0.2126*rgba[0] + 0.7152*rgba[1] + 0.0722*rgba[2]);
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
