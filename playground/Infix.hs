{--
Any functions defined solely with symbols are auto infix
This line below is a "fixity" declaration and how tight the operator binds (priority) and whether left or right associative
--}
infixr 5 :-:

data List a = Empty | a  :-: (List a) deriving (Show, Read, Eq, Ord)
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 6 ++.
(++.) :: List a -> List a -> List a
Empty ++. ys       = ys
(x :-: xs) ++. ys  = x :-: (xs ++. ys)
