# miscellaneous notes

Enabling `Wall` (all warnings) can catch non-exhaustive pattern matches.

You can enter code blocks into GHCi using `:{` and `:}`!
```haskell
λ> :{
*Main| let isItTwo :: Integer -> Bool
*Main|     isItTwo 2 = True
*Main|     isItTwo _ = False
*Main| :}
```
