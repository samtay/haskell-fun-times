import Data.Char
import Data.List
-- This is one way
{--
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards"
--}

{-- FUNCTOR LAWS

1. If we map the id function over a functor, the functor that we get back should be the same as the original functor

2. Composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one



-- But we can use fmap since we have instance Functor IO
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn $ "You said " ++ line ++ " backwards"
