# Monads gone wild
This chapter is a brief survey of "how Haskellers write code when they think no one is looking". Mostly just code samples pasted for future reference.

### 19.2 Monoid
#### Templating content in Scotty
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html
    (mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>"])
```

#### Concatenating connection parameters
From Aditya Bhargava's "Making a Website With Haskell":
```haskell
runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = do
  let connStr =
    foldr (\(k,v) t ->
      t <> (encodeUtf8 $
      k <> "=" <> v <> " "))
    "" params
  runResourceT . withPostgresqlConn connStr
    $ runSqlConn query
```
Scotty tutorials will make more sense have exploring monad transformers.

#### Concatenating key configurations
This example is an Xmonad configuration snippet, to `mappend` custom keys with the defaults:
```haskell
import XMonad
import XMonad.Actions.Volume
import Data.Map.Lazy (fromList)
import Data.Monoid (mappend)

main = do
  xmonad def { keys =
    \c -> fromList [
      ((0, xK_F7),
        lowerVolume 4 >> return ()),
      ((0, xK_F8),
        raiseVolume 4 >> return ())
    ] `mappend` keys defaultConfig c
  }

-- where
keys :: !(XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
```
The exclamation point is syntax for *strictness annotation* which forces that field of the product record to weak-head normal form. Anyway, the monoid instance above is for functions:
```haskell
instance Monoid b => Monoid (a -> b)
```

### 19.3 Functor
#### Lifting over IO
Lifting a normal time function into IO:
```haskell
import Data.Time.Clock

addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
getCurrentTime :: IO UTCTime

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $
    getCurrentTime
```

Lifting some text transformation functions over the UUID library touching an outside resource to generate random identifiers:
```haskell
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

textUuid :: IO Text
textUuid =
  fmap (T.pack . UUID.toString) UUIDv4.nextRandom
```

#### Lifting over web app monads
Often when writing web apps, there is a custom datatype to describe the web app with a `Monad` instance.
The "app context" will have a type parameter to describe the result of running the web app.
Here is an exammple lifting over both `AppHandler` and `Maybe`:
```haskell
userAgent :: AppHandler (Maybe UserAgent)
userAgent = (fmap . fmap) userAgent' getRequest

userAgent' :: Request -> Maybe UserAgent
userAgent' req =
  getHeader "User-Agent" req
```
The functor is necessary here because we can't pattern match on the `AppHandler`.
Basically, Snap keeps all of the context in this `AppHandler`.. it doesn't make sense to do all the legwork of configuration, server initialization, sending a request, etc., every time we need a value that is only obtained during the course of running a web app.
The Functor let's us write functions over this structure to handle all that.
The function is passed to the app handler in a way that says "apply this to a thing that results from an HTTP request, if one comes along".

### 19.4 Applicative
#### hgrev
```haskell
jsonSwitch :: Parser (a -> a)
jsonSwitch =
  infoOption $(hgRevStateTH jsonFormat)
  $ long "json"
  <> short 'J'
  <> help "Display JSON version information"

parserInfo :: ParserInfo (a -> a)
parserInfo = info (helper <*> verSwitch <* jsonSwitch) fullDesc

-- where x <* y throws away result of the second action
(<*) :: Applicative f => f a -> f b -> f a
```

#### More parsing
Here is a very common use case of applicative to lift data constructors over parsers:
```haskell
-- JSON
parseJSON :: Value -> Parser a
(.:) :: FromJSON a => Object -> Text -> Parser a

instance FromJSON Payload where
  parseJSON (Object v) = Payload <$> v .: "from"
                                 <*> v .: "to"
                                 <*> v .: "subject"
                                 <*> v .: "body"
                                 <*> v .: "offset_seconds"
  parseJSON v          = typeMismatch "Payload" v

-- CSV
parseRecord :: Record -> Parser a

instance FromRecord Release where
  parseRecord v
    | V.length v == 5 = Release <$> v .! 0
                                <*> v .! 1
                                <*> v .! 2
                                <*> v .! 3
                                <*> v .! 4
    | otherwise = mzero
```

A more complex, yet very terse example using functor, applicative, and monad techniques:
```haskell
instance Deserializeable ShowInfoResp where
  parser = e2err =<< convertPairs . HM.fromList <$> parsePairs
    where
      parsePairs :: Parser [(Text, Text)]
      parsePairs = parsePair `sepBy` endOfLine
      parsePair  = liftA2 (,) parseKey parseValue
      parseKey   = takeTill (== ':') <* kvSep
      kvSep      = string ": "
      parseValue = takeTill isEndOfLine
```

Lifting into a function space!
```haskell
module Web.Shipping.Utils ((<||>)) where

import Control.Applicative (liftA2)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)
```
With this utility, we can avoid something like:
```haskell
>>> (\n -> f n || g n) arg
```
and write
```haskell
>>> (f <||> g) arg
```

### 19.5 Monad
Since effectful programming is constrained in Haskell in `IO`, examples of monad usage are abundant. Here are a few different, interesting use cases.

#### Opening a network socket
```haskell
import Network.Socket
openSocket :: FilePath -> IO Socket
openSocket p = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock sockAddr
  return sock
  where sockAddr = SockAddrUnix . encodeString $ p
```

#### Binding over failure in initialization
This `main` function is more typical; the outermost monad is `IO` but the monad transformer variant of `Either`, called `EitherT`, binds over the possibility of failure to pull up correct configuration in constructing an initialization function:
```haskell
main :: IO ()
main = do
  initAndFp <- runEitherT $ do
    fp <- tryHead NoConfig =<< lift getArgs
    initCfg <- load' fp
    return (initCfg, fp)
  either bail (uncurry boot) initAndFp
  where
    boot initCfg fp =
      void $ runMVC mempty
             oracleModel (core initCfg fp)
    bail NoConfig =
      errorExit "Please pass a config"
    bail (InvalidConfig e) =
      errorExit ("Invalid config " ++ show e)
    load' fp =
      hoistEither
      . fmapL InvalidConfig =<< lift (load fp)
```

### 19.6 An end-to-end example: URL shortener
See [shawty](./shawty) for code.

#### Brief aside about polymorphic literals
We start off [our code](./shawty/src/Main.hs) with the language extension
`{-# LANGUAGE OverloadedStrings #-}`.
This is a way to make `String` literals polymorphic, just like numeric literals are polymorphic over the `Num` typeclass. This way we can use string literals as `Text` and `ByteString` values. Notice the similarities:
```haskell
class IsString a where
  fromString :: String -> a

class Num a where
  -- irrelevant bits elided
  fromInteger :: Integer -> a

class Num a => Fractional a where
  -- Elision again
  fromRational :: Rational -> a
```
and see the effect:
```haskell
Prelude> :set -XOverloadedStrings
Prelude> :t 1
1 :: Num a => a
Prelude> :t 1.0
1.0 :: Fractional a => a
Prelude> :t "blah"
"blah" :: IsString a => a
```
