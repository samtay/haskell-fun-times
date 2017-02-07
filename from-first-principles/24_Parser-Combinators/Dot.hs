#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package trifecta
  --package hspec
-}
{-# LANGUAGE OverloadedStrings #-}
module Dot where

import Text.Trifecta
import Control.Applicative ((<|>))
import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

{-
-- 10 --
Write a parser for the DOT language that Graphviz uses to
express graphs in plain-text.
We suggest you look at the AST datatype in Haphviz for ideas
on how to represent the graph in a Haskell datatype. If you’re
feeling especially robust, you can try using fgl.
--
I'm greatly simplifying here so I don't end up spending a ton of time on this.
--
In hindsight, my parsers will probably need (or at least, be made much simpler by)
statement types & parsers that are then used to create the datatype.
(See language-dot for examples).
For example, parse DOT into statement types,
then use statement types to compile a graph type
-}

-- Parsing funcs

parseGraph :: Parser DotGraph
parseGraph =
  Graph <$> parseGType
        <*> (spaces >> between (symbol "{") (symbol "}") parseDot)

parseGType :: Parser GraphType
parseGType =
      (try (string "graph") >> return UndirectedGraph)
  <|> (string "digraph" >> return DirectedGraph)

parseDot :: Parser Dot
parseDot =
      try parseNode
  <|> try parseEdge
  <|> try parseDotSeq
  <|> parseDotEmpty

parseNode :: Parser Dot
parseNode =
  Node <$> (NodeId . T.pack <$> some (noneOf " \n\t"))
       <*> (option [] $ char ' ' >> parseAttributes)

parseAttributes :: Parser [Attribute]
parseAttributes = between (symbol "[") (symbol "]") $
  parseAttribute `sepBy` symbol ","

parseAttribute :: Parser Attribute
parseAttribute = do
  n <- some alphaNum
  _ <- char '='
  v <- some alphaNum
  return (T.pack n, T.pack v)

parseEdge :: Parser Dot
parseEdge = undefined

parseDotSeq :: Parser Dot
parseDotSeq = undefined

parseDotEmpty :: Parser Dot
parseDotEmpty = undefined

-- | Type of a graph, directed or undirected.
--
-- This also specifies what edge declarations look like.
data GraphType =
    UndirectedGraph
  | DirectedGraph
  deriving (Show, Eq)

-- | Attribute name: just text
type AttributeName = Text

-- | Attribute value: just text
type AttributeValue = Text

-- | Attribute: a tuple of name and value.
type Attribute = (AttributeName, AttributeValue)

-- | A node identifier.
--
-- This is either a user supplied name or a generated numerical identifier.
data NodeId = NodeId Text
  deriving (Show, Eq)

-- | Haphviz internal graph content AST
data Dot =
    Node NodeId [Attribute]
  | Edge NodeId NodeId [Attribute]
  | DotSeq Dot Dot
  | DotEmpty
  deriving (Show, Eq)

-- | A Haphviz Graph
data DotGraph = Graph GraphType Dot
  deriving (Show, Eq)

-- | Dot is a monoid, duh, that's the point.
instance Monoid Dot where
  mempty = DotEmpty
  mappend DotEmpty d = d
  mappend d DotEmpty = d
  mappend d (DotSeq d1 d2) = DotSeq (mappend d d1) d2
  mappend d1 d2 = DotSeq d1 d2

-- Testing

-- This is done because Result doesn't have Show instance
toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _           = Nothing

-- Runtime

main :: IO ()
main = hspec $
  describe "Parsing DOT language" $
    it "passes tests" $
      pendingWith "need to write parser"
