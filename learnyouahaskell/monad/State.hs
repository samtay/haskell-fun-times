-- learnyouahaskell is dated -- current State monads look pretty different.
-- @TODO come back in here after other tutorials and bring learnyouahaskell info up to date
module Stack where
-- While functions' inability to change global state/variables leads to better programs,
-- some problems are inherently stateful
-- This can be tedious in Haskell, but the state monad helps
import Control.Monad.State

type Stack = [Int]

pop' :: Stack -> (Int,Stack)
pop' (x:xs) = (x,xs)

push' :: Int -> Stack -> ((),Stack)
push' a xs = ((),a:xs)

-- stackOps' [1,2,3,4] == (1,[2,3,4])
stackOps' :: Stack -> (Int, Stack)
stackOps' stack = let
    ((),secondStack) = push' 3 stack
    (a ,thirdStack)  = pop' secondStack
    in pop' thirdStack

{-- ^^ A bit tedious. enter the State monad

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState
--}
pop :: StateT Stack Int
pop = StateT $ \(x:xs) -> (x,xs)

push :: Int -> StateT Stack ()
push a = StateT $ \xs -> ((),a:xs)

-- Note, we are NOT passing stateful stack as an argument -- thats what we wanted to avoid, huzzah
stackOps :: StateT Stack Int
stackOps = do
    push 3
    a <- pop
    pop
