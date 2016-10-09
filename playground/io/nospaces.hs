import Control.Monad

main = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main

-- "when" is imported from Control.Monad
-- it encapsulates the if (bool) then (some IO action) else return ()
