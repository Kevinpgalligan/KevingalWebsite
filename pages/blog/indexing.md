title: 0-indexing versus 1-indexing on a case-by-case basis
date: 2020-10-22
description: blah
imgthumbnail: img/basketball/thumbnail.jpg

### Get the last element
0-indexing:

    arr[len(arr)-1]

1-indexing:

    arr[len(arr)]

There are *n* elements in the array. The *n*th element.

### Negative indexing
Symmetry when 1-indexing: arr[1] and arr[-1]

### Offsets
You have index k, you wanna loop over 5 elements starting from k.

    for (i = k; i < k+5; i++) {
        // do something with arr[i]
    }

### Intervals
It's the same regardless of the indexing scheme you use.

Except... number of elements starting at index *n* and back to the start.

0-indexing:

    n+1

1-indexing:

    n

Number of elements BEFORE index *n* (exclusive).

0-indexing:

    n

1-indexing:

    n-1
