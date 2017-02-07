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

{-
-- 10 --
Write a parser for the DOT language that Graphviz uses to
express graphs in plain-text.
We suggest you look at the AST datatype in Haphviz for ideas
on how to represent the graph in a Haskell datatype. If you’re
feeling especially robust, you can try using fgl.
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
parseDot = foldr ((<|>) . try) (try parseNode)
  [ parseEdge
  , parseDeclaration
  , parseRanksame
  , parseSubgraph
  , parseRawDot
  , parseLabel
  , parseRankdir
  , parseDotSeq
  , parseDotEmpty ]

parseNode :: Parser Dot
parseNode = undefined

parseEdge :: Parser Dot
parseEdge = undefined

parseDeclaration :: Parser Dot
parseDeclaration = undefined

parseRanksame :: Parser Dot
parseRanksame = undefined

parseSubgraph :: Parser Dot
parseSubgraph = undefined

parseRawDot :: Parser Dot
parseRawDot = undefined

parseLabel :: Parser Dot
parseLabel = undefined

parseRankdir :: Parser Dot
parseRankdir = undefined

parseDotSeq :: Parser Dot
parseDotSeq = undefined

parseDotEmpty :: Parser Dot
parseDotEmpty = undefined

-- Types - mostly stolen from Haphiz, but simplified a bit

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
data NodeId =
    UserId Text
  | Nameless Int
  deriving (Show, Eq)

-- | Declaration type
--
-- Used to declare common attributes for nodes or edges.
data DecType =
    DecGraph
  | DecNode
  | DecEdge
  deriving (Show, Eq)

-- | A Haphviz Graph
data DotGraph = Graph GraphType Dot
  deriving (Show, Eq)

-- | Rankdir Type
--
-- Used to specify the default node layout direction
data RankdirType =
    LR
  | RL
  | TB
  | BT
  deriving (Show, Eq)

-- | Haphviz internal graph content AST
data Dot =
    Node NodeId [Attribute]
  | Edge NodeId NodeId [Attribute]
  | Declaration DecType [Attribute]
  | Ranksame Dot
  | Subgraph Text Dot
  | RawDot Text
  | Label Text
  | Rankdir RankdirType
  | DotSeq Dot Dot
  | DotEmpty
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
