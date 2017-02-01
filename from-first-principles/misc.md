# miscellaneous notes

- Enabling `Wall` (all warnings) can catch non-exhaustive pattern matches.

- You can enter code blocks into GHCi using `:{` and `:}`!

  ```haskell
  λ> :{
  *Main| let isItTwo :: Integer -> Bool
  *Main|     isItTwo 2 = True
  *Main|     isItTwo _ = False
  *Main| :}
  ```

- Quasiquotes are nice for avoiding escaping stuff in GHCi:

  ```haskell
  λ> :set -XQuasiQuotes
  λ> import Text.RawString.QQ
  λ> [r|{"red": "123"}|]
  "{\"red\": \"123\"}"
  ```
