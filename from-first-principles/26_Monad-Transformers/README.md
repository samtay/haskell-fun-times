# Monad transformers
Chapter [25](../25_Composing-Types) demonstrated the need for monad transformers
and what they accomplish.
This chapter is a deeper dive to get more comfortable with them in practice.

### 26.2 MaybeT
Just as the Maybe monad is extremely useful,
its transformer variant MaybeT is an important transformer.
```haskell
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }
```
Notice the similarity in the structures of the MaybeT and Compose types;
that structure leads to reusing the same pattern for functor & applicative instances:
```haskell
instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT . pure . pure
  (MaybeT mMab) <*> (MaybeT mMa) =
    MaybeT $ liftA2 (<*>) mMab mMa
    -- or
    MaybeT $ (<*>) <$> mMab <*> mMa
```
Again, we only need to do something different once we get to the monad instances,
since functors & applicatives are closed under composition in general.

#### MaybeT Monad instance
Nothing too surprising here:
```haskell
instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  (MaybeT mMa) >>= aMmb =
    MaybeT $ do
      -- v :: Maybe a
      v <- mMa
      case v of
        Nothing -> return Nothing
        Just y  -> runMaybeT $ aMmb y
```

### 26.3 EitherT
This is the newtype for the Either transformer:
```haskell
newtype EitherT e m a =
  EitherT { runEitherT :: m (EitherT e a) }
```

#### Exercises: EitherT
1. Write the Functor instance for EitherT.

  ```haskell
  instance (Functor f) =>
    Functor (EitherT e f) where
      fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma
  ```

2. Write the Applicative instance for EitherT.

  ```haskell
  instance (Applicative f) =>
    Applicative (EitherT e f) where
      pure = EitherT . pure . pure

      (EitherT mf) <*> (EitherT ma) =
        EitherT $ (<*>) <$> mf <*> ma
  ```

3. Write the Monad instance for EitherT.

  ```haskell
  instance (Monad m) =>
    Monad (EitherT e m) where
      return = pure

      (EitherT ma) >>= f = EitherT $ do
        -- a :: Either e a
        a <- ma
        case a of
          Left e  -> return $ Left e
          Right x -> runEitherT $ f x
  ```

4. Write the `swapEitherT` helper function for EitherT.

  ```haskell
  swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
  swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma
  ```

5. Write the transformer variant of the `either` catamorphism.

  ```haskell
  eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
  eitherT f g (EitherT ma) = ma >>= either f g
  ```

### 26.4 ReaderT
ReaderT is one of the most common transformers in conventional Haskell.
It's just like Reader, but generating additional structure in the return type:
```haskell
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
```
I decided to write the instances myself,
and they look a bit different than the book's,
but they type check just fine:
```haskell
instance (Functor m) =>
  Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance (Applicative f) =>
  Applicative (ReaderT e f) where
    pure x = ReaderT $ const (pure x)

    (<*>) :: ReaderT r f (a -> b)
          -> ReaderT r f a
          -> ReaderT r f b
    (ReaderT mf) <*> (ReaderT ma) =
      ReaderT $ \r -> (mf r) <*> (ma r)

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT ma) >>= f =
    ReaderT $ \r -> do
    a <- ma r
    runReaderT (f a) r
```

### 26.5 StateT
StateT is State with additional monadic structure around the function result, just like Reader and ReaderT.
```haskell
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }
```

#### Exercises: StateT
We're implementing the strict variant (as opposed to lazy), which is the "obvious" implementation:
```haskell
instance (Functor m) =>
  Functor (StateT s m) where
    fmap f (StateT smf) = StateT $ \s ->
      fmap (\(x, s1) -> (f x, s1)) (smf s)

instance (Monad m) =>
  Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x, s)

    (<*>) :: StateT s m (a -> b)
          -> StateT s m a
          -> StateT s m b
    (StateT smf) <*> (StateT sma) = StateT $ \s1 -> do
      (ab, s2) <- smf s1
      (a, s3)  <- sma s2
      return (ab a, s3)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  (StateT sma) >>= f = StateT $ \s1 -> do
    (a, s2) <- sma s1
    (runStateT (f a)) s2
```
Notice we need a monadic constraint for the applicative instance;
this is needed to feed the successive states through each computation,
as opposed to just feeding the initial state through each computation.

### 26.6 Types you probably don't want to use
Not every type will necessarily be performant or make sense, such as ListT and Writer/WriterT.

#### Why not use Writer or WriterT?
First of all, State can do what Writer can do plus more.
In addition, it is commonly too lazy or too strict for a given problem,
which results in too much memory usage.
Writer can accumulate unevaluated thunks, causing memory leaks.
For example, it is inappropriate for logging in long-running processes,
since the logged values can't be retrieved until the computation is complete.

#### The ListT you want isn't made from the List type
The obvious implementation of ListT is generally not recommended because:

1. The obvious obvious attempt isn't associative.
2. It's not very fast.
3. Streaming libraries like [pipes](http://hackage.haskell.org/package/pipes) and [conduit](http://hackage.haskell.org/package/conduit) do it better for most cases.

Lists in Haskell are as much a control structure as they are a data structure,
so streaming libraries generally suffice when we need transformers.

### 26.7 Recovering an ordinary type from a transformer
We can use the `Identity` type to recover non-transformer versions of transformer variants:
```haskell
type MyIdentity a = IdentityT Identity a
type Maybe a      = MaybeT Identity a
type Either e a   = EitherT e Identity a
type Reader r a   = ReaderT e Identity a
type State s a    = StateT s Identity a
```
We don't ordinarily need to do this for transformers that already have corresponding non-transformer types.
However, `ReaderT` is part of the Scotty environment and you can't easily retrieve the `Reader` type out of it
because `Reader` is not a type that exists on its own.
Thus, it may present a situation where we only need `Reader`, not `ReaderT`,
and `ReaderT Identity` can act as a `Reader` compatible with Scotty.

#### The transformers library
In general, we don't need to make these transformer types ourselves -
many of them are in base or the `transformers` library.

#### A note on ExceptT
Note that the `either` library on Hackage provides an `EitherT` type,
however most Haskellers are moving to the identical `ExceptT` type in the `transformers` library.
This is mainly because `transformers` comes packaged with GHC already.

### 26.8 Lexically inner is structurally outer
One tricky part about monad transformers:
the **lexical representation** of the types are *counterintuitive*
with respect to the **structure of their values**.
Consider the definitions of these transfomer types:
```haskell
newtype ExceptT e m a =
  ExceptT { runExceptT :: m (Either e a)) }
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
```
Note that the additional structure `m` is always wrapped *around* the value.
It's only wrapped around things we can **have**, not things we **need**,
as can be seen in `ReaderT`.
Consequentially, a series of monad transformers will begin with the innermost type:
```haskell
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once because it's one big Monad
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

unwrap :: () -> IO (Either String (Maybe Int))
unwrap = (runReaderT . runExceptT . runMaybeT) embedded

λ> unwrap ()
Right (Just 1)
```
See how the structure of the values (`Right (Just x))`) is the reverse of the type compositions
(`MaybeT (ExceptT x y)`).
When we say **base monad** we mean the structurally outermost monad.
In the example above the base monad of `embedded` is Reader,
and the base monad of `unwrap ()` is Either.

### 26.9 MonadTrans
We often want to lift functions into a larger context:
```haskell
fmap :: Functor f =>      (a -> b) -> f a -> f b
liftA :: Applicative f => (a -> b) -> f a -> f b
liftM :: Monad m =>       (a -> r) -> m a -> m r
```
but sometimes we need to lift over more structure than these types permit,
such as the innermost position in a stack of monad transformers.

#### The typeclass that lifts
MonadTrans is a typeclass with one core method: `lift`.
It is for lifting actions in some Monad over a transformer type
which wraps itself in the original monad.
```haskell
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a
```
Let's motivate this with a simple example:

#### Motivating MonadTrans
Scotty has monad transformers that are themselves newtypes for monad transformer stacks:
```haskell
newtype ScottyT e m a =
  ScottyT { runS :: State (ScottyState e m) a }
  deriving ( Functor, Applicative, Monad )
newtype ActionT e m a =
  ActionT { runAM :: ExceptT (ActionError e)
                             (ReaderT ActionEnv
                             (StateT ScottyResponse m)) a }
  deriving ( Functor, Applicative )
type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO
```

And this is a hello world example **with a type error**:
```haskell
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    -- we cant use IO willy nilly here
    putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```
We can't use `putStrLn :: IO ()` because the `do` block has type `ActionM ()`.
Instead, we can lift:
```haskell
import Control.Monad.Trans.Class
lift $ putStrLn "hello"
```
Note that `lift` is leveraging the `MonadTrans` instance of `ActionT e`,
hence `lift :: (MonadTrans t) => IO a -> t IO a` specializes to
`lift :: IO a -> ActionM a`.
Running `main` in GHCi and navigating to `http://localhost:3000/beam`
results in both "Scotty, beam me up!" in the browser and "hello" in the terminal.

Since `ActionT` is itself defined in terms of three more monad transformers,
it can simply wrap around composed `lift`s:
```haskell
instance MonadTrans (ActionT e) where
  lift = ActionT . lift . lift . lift
```

Now, if we take all of the individual implementations of those `lift` functions,
we can see the benefit of this typeclass and how much terseness we gain:
```haskell
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
      (ActionT
      . (ExceptT . fmap Right)
      . ReaderT . const
      . \m -> StateT (\s -> do
                         a <- m
                         return (a, s))
      ) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

To summarize, lifting is embedding an expression within a larger context
by adding structure that doesn't do anything.

#### MonadTrans instances
```haskell
-- IdentityT
instance MonadTrans IdentityT where
  lift = IdentityT

-- MaybeT
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

-- ReaderT
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-- 1. EitherT
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

-- 2. StateT
instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)
```

#### Prolific lifting is the failure mode
Sometimes with conrete, explicitly typed monad transformers you see this hell:
```haskell
addSubWidget sub w =
  do master <- liftHandler getYesod
    let sr = fromSubRoute sub master
    i <- GWidget $ lift $ lift $ lift $ lift
                 $ lift $ lift $ lift get
    w' <- liftHandler
          $ toMasterHandlerMaybe sr (const sub) Nothing
          $ flip runStateT i $ runWriterT $ runWriterT
          $ runWriterT $ runWriterT $ runWriterT
          $ runWriterT $ runWriterT $ unGWidget w
    let ((((((((a,
                body),
               title),
              scripts),
             stylesheets),
            style),
           jscript),
          h),
         i') = w'
    GWidget $ do
      tell body
      lift $ tell title
      lift $ lift $ tell scripts
      lift $ lift $ lift $ tell stylesheets
      lift $ lift $ lift $ lift $ tell style
      lift $ lift $ lift $ lift $ lift $ tell jscript
      lift $ lift $ lift $ lift $ lift $ lift $ tell h
      lift $ lift $ lift $ lift
           $ lift $ lift $ lift $ put i'
      return a
```
Do **not** write code like this.

#### Wrap it, smack it, pre-lift it.
There are many ways to avoid this hell,
but one of the most robust and common is
newtyping your Monad stack and abstracting away the representation,
then provide functionality leveraging the representation as part of your API.
Scotty does this well:
```haskell
λ> import Web.Scotty
λ> :info ActionM
Web.Scotty.Internal.Types.ActionT Text IO
```
Scotty hides the underlying type in an Internal module
because normally developers don't need to worry about it when using the library.
If they do happen to need it, they can just import the Internal module:
```haskell
λ> import Web.Scotty.Internal.Types
λ> :info ActionT
newtype ActionT e (m :: * -> *) a
  = ActionT {runAM :: ExceptT
                      (ActionError e)
                      (ReaderT ActionEnv (StateT ScottyResponse m))
                      a}
```
This is good practice;
it hides unnecessary noise from consumers of the library
and also reduces manual lifting within the Monad.
For example, above we only needed one `lift` to perform an IO action in `ActionM`
even though the underlying implementation has a bunch of transformers in the stack.

### 26.10 MonadIO aka zoom-zoom
MonadIO is yet another way to lift an action over additional structure.
MonadIO is different from MonadTrans because rather than lifting through one layer at a time,
MonadIO keeps lifting your IO action until it is lifted over *all* structure embedded
within the the outermost IO type.
Thus, this class is for monads in which IO computations may be embedded.
*Any* monad built by applying a sequence of monad transformers to the IO monad
can be an instance of this class.
```haskell
class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

-- Laws

-- 1 --
liftIO . return = return

-- 2 --
liftIO (m >>= f) = liftIO m >>= (liftIO . f)
```

We can modify the previous Scotty example
```haskell
import Control.Monad.IO.Class
liftIO (putStrLn "hello")
```

#### MonadIO instances
```haskell
-- IdentityT
instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

-- EitherT
instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

-- 1. MaybeT
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- 2. ReaderT
instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO

-- 3. StateT
instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO x = StateT $ \s -> do
    a <- liftIO x
    return (a, s)
```
