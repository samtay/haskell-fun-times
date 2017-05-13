{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as VM

-- Exercise 1
-- Write Version 1 of the calculator: an evaluator for
-- ExprT, with the signature
-- eval :: ExprT -> Integer
-- For example,
-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
-- The UI department has internalized the focus group data and is
-- ready to synergize with you. They have developed the front-facing
-- user-interface: a parser that handles the textual representation of the
-- selected language. They have sent you the module Parser.hs, which
-- exports parseExp, a parser for arithmetic expressions. If you pass
-- the constructors of ExprT to it as arguments, it will convert Strings
-- representing arithmetic expressions into values of type ExprT. For example:
--
-- Calc> parseExp Lit Add Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- Calc> parseExp Lit Add Mul "2+3*4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
-- Calc> parseExp Lit Add Mul "2+3*"
-- Nothing
--
-- Leverage the assets of the UI team to implement the value-added function
-- evalStr :: String -> Maybe Integer
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- Exercise 3
-- Create a type class called Expr with three methods called lit, add,
-- and mul which parallel the constructors of ExprT. Make an instance of
-- Expr for the ExprT type, in such a way that
-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
--   == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4
-- Make instances of Expr for each of the following types:
-- • Integer — works like the original calculator
-- • Bool — every literal value less than or equal to 0 is interpreted
--   as False, and all positive Integers are interpreted as True;
--   “addition” is logical or, “multiplication” is logical and
-- • MinMax — “addition” is taken to be the max function, while
--   “multiplication” is the min function
-- • Mod7 — all values should be in the ranage 0 . . . 6, and
--   all arithmetic is done modulo 7; for example, 5 + 3 = 1.
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (&&)
  mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (a + b)
  mul (MinMax a) (MinMax b) = MinMax (a * b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

-- Exercise 5
-- Write a version of your calculator that will emit assembly language for the
-- new processor. The CPU supports six operations, as embodied in the StackExp data type:
-- data StackExp = PushI Integer
--               | PushB Bool
--               | Add
--               | Mul
--               | And
--               | Or
--               deriving Show
-- type Program = [StackExp]
-- PushI and PushB push values onto the top of the stack, which can
-- store both Integer and Bool values. Add, Mul, And, and Or each pop
-- the top two items off the top of the stack, perform the appropriate
-- operation, and push the result back onto the top of the stack. For
-- example, executing the program
-- [PushB True, PushI 3, PushI 6, Mul]
-- will result in a stack holding True on the bottom, and 18 on top of that.
-- If there are not enough operands on top of the stack, or if an operation
-- is performed on operands of the wrong type, the processor
-- will melt into a puddle of silicon goo. For a more precise specification
-- of the capabilities and behavior of the custom CPU, consult the
-- reference implementation provided in StackVM.hs.
-- Your task is to implement a compiler for arithmetic expressions.
-- Simply create an instance of the Expr type class for Program, so that
-- arithmetic expressions can be interpreted as compiled programs. For
-- any arithmetic expression exp :: Expr a => a it should be the case that
-- stackVM exp == Right [IVal exp]
-- Note that in order to make an instance for Program (which is a
-- type synonym) you will need to enable the TypeSynonymInstances
-- language extension. Then write the function
-- compile :: String -> Maybe Program
-- which takes Strings representing arithmetic expressions and compiles
-- them into programs that can be run on the custom CPU.

instance Expr VM.Program where
  lit n = [VM.PushI n]
  add e1 e2 = e2 ++ e1 ++ [VM.Add]
  mul e1 e2 = e2 ++ e1 ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
