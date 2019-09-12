function generateEmojipasta(text) {
    var blocks = splitIntoBlocks(text);
    var newBlocks = [];
    blocks.forEach(block => {
        newBlocks.push(block);
        emojis = generateEmojisFrom(block);
        if (emojis) {
            newBlocks.push(" " + emojis);
        }
    });
    return newBlocks.join("");
}

function splitIntoBlocks(text) {
    // todo
}

function generateEmojisFrom(block) {
    // todo
}

