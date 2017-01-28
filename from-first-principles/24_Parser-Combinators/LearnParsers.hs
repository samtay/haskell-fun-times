module LearnParsers where

import Text.Trifecta

one :: CharParsing m => m Char
one = char '1'

stop :: Parser a
stop = unexpected "stop"

one' = one >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'
-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
