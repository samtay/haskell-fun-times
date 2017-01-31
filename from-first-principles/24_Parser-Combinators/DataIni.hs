{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

-- Header

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

headerEx :: ByteString
headerEx = "[blah]"

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

-- Assignments

assignmentEx :: ByteString
assignmentEx = "foo=bar"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some (letter <|> char '-') -- allow hyphens why not
  char '='
  val <- some (noneOf "\n")
  skipEOL -- this allows us to not clobber next assignments
  return (name, val)

-- Comments

commentEx :: ByteString
commentEx =
  "; todo: something\
  \ wutwut"

commentEx' :: ByteString
commentEx' =
  "; blah\n: woot\n  \n;haha"

skipComments :: Parser ()
skipComments = skipMany $ do
  char ';' <|> char '#'
  skipMany (noneOf "\n")
  skipEOL

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- Sections

data Section =
  Section Header Assignments
  deriving (Eq, Show)

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

-- Entire Config

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) =
  M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let sectionsMap = foldr rollup M.empty sections
   in return (Config sectionsMap)

-- Testing

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do

  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m  = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("foo", "bar")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Comment Parsing" $
    it "can skip comments before a header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Section Parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx'
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected' = Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'

  describe "Ini File Parsing" $ do
    it "can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList
                          [ ("alias", "claw")
                          , ("host", "wikipedia.org") ]
          whatisitValues = M.fromList
                           [("red", "intoothandclaw")]
          expected' = Just (Config
                            (M.fromList
                             [ (Header "section" , sectionValues)
                             , (Header "whatisit" , whatisitValues)]))
      print m
      r' `shouldBe` expected'

    it "can parse a firefox file found on my computer" $ do
      let m = parseByteString parseIni mempty firefoxFileContent
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just firefoxMarshalledContent

firefoxFileContent :: ByteString
firefoxFileContent =
  [r|; This file is not used. If you modify it and want the application to use
; your modifications, move it under the browser/ subdirectory and start with
; the "-app /path/to/browser/application.ini" argument.
[App]
Vendor=Mozilla
Name=Firefox
RemotingName=firefox
Version=50.1.0
BuildID=20161213204721
ID={ec8030f7-c20a-464f-9b0e-13a3a9e97384}

[Gecko]
MinVersion=50.1.0
MaxVersion=50.1.0

[XRE]
EnableProfileMigrator=1
|]

firefoxMarshalledContent :: Config
firefoxMarshalledContent = Config $
  M.fromList
    [ (Header "App", M.fromList [ ("BuildID","20161213204721")
                                , ("ID","{ec8030f7-c20a-464f-9b0e-13a3a9e97384}")
                                , ("Name","Firefox")
                                , ("RemotingName","firefox")
                                , ("Vendor","Mozilla")
                                , ("Version","50.1.0")])
    , (Header "Gecko", M.fromList [ ("MaxVersion","50.1.0")
                                  , ("MinVersion","50.1.0")])
    , (Header "XRE", M.fromList [("EnableProfileMigrator","1")])]
