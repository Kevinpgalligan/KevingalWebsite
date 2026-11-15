title: Noodling with Haskell
date: 2026-08-13
description: TODO
requires: code
tags: haskell programming

Notes:

* The setup was a pain in the ass.
* REPL isn't ideal (reload modules -> lose all existing vars)
* Cryptopals! My byte representation, crypton library...
* (Protip) If I had an issue where a type wasn't what I expected, it's probable that I had some buggy / nonsense code
  that was causing the wrong type to be inferred. It helped to specify the type I expected, as this would
  then narrow down where the issue was.

Example:

```
f :: Int -> [Int]
f n = aux n []
    where (...)
          aux :: Int -> [Int] -> [Int] <-- add this
          aux n xs = aux (n-1) (n:xs)  <-- error here
```
