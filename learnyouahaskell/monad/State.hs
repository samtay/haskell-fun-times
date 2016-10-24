module Stack where
import Control.Applicative
import Control.Monad (liftM, ap)
-- While functions' inability to change global state/variables leads to better programs,
-- some problems are inherently stateful
-- This can be tedious in Haskell, but the state monad helps

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

-- ^^ A bit tedious. enter the State monad

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap = liftM
instance Applicative (State s) where
    pure = return
    (<*>) = ap
instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState
pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

-- Note, we are NOT passing stateful stack as an argument -- thats what we wanted to avoid, huzzah
stackOps :: State Stack Int
stackOps = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5 then push 5 else do
        push 3
        push 8

combinedOps :: State Stack ()
combinedOps = do
    a <- stackOps
    if a == 100
       then stackStuff
       else return ()

-- MonadState implements get and put as below

get = State $ \s -> (s,s)
put newState = State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
       then put [8,3,1]
       else put [9,2,1]
