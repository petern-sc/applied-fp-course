{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level05.AppM
-- Question: Is (..) wildcard export? (..)
  ( AppM(..)
  , liftEither
  , runAppM
  ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Text              (Text)

import           Level05.Types          (Error)

import           Data.Bifunctor         (first)
import           Data.Either                        (Either (Left, Right),
                                                     either)

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will create a newtype `AppM` that is a shorthand way of
-- describing the return type of a function that may contain an error.
--
-- Our `AppM` type will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value. With the added bonus of allowing us to perform `IO` actions!
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     needsAButMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (IO (Either Error a))
-- This structure allows us to start writing our functions in terms of
-- constraints. As an example, if we wanted to abstract over IO and indicate
-- that instead of the concrete type we wanted a constraint that allows for IO
-- actions. Our AppM would look more like this:
--
-- AppM m a = AppM ( m (Either Error a) )
--
-- Then our functions would look like:
--
-- foo :: MonadIO m => Int -> AppM m a
--
-- Or we could not use a concrete type for Error
--
-- AppM e m a = AppM ( m (Either e a) )

runAppM
  :: AppM a
  -> IO (Either Error a)
runAppM (AppM m) =
  m

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  -- fmap fn (AppM (IO (Right a'))) = do
  fmap fn (AppM ioA) =
    let
      b' = fmap (fmap fn) ioA
    in AppM b'
  -- fmap _ (AppM (IO (Left e))) = AppM (pure (Left e))
  -- fmap _ (AppM (IO e@(Left _))) = AppM (pure e)

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM (pure (Right a))
  -- pure = AppM . (pure . Right)

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) (AppM ioFn) (AppM ioA) = let
      b' = do
        fn <- ioFn
        a' <- ioA
        let errorOrb' = fn <*> a'
        return errorOrb'
    in AppM b'

instance Monad AppM where
  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  (>>=) (AppM ioEitherA) fn =
    let
      ioEitherB = do
        eitherA <- ioEitherA -- eitherA :: Either[Error, A]
        let eitherB' = fmap fn eitherA -- b' :: Either[Error, AppM[B]] 
        case eitherB' of
          (Left value) -> pure (Left value) -- IO[Left[B]]
          (Right (AppM io)) -> io -- IO[ErrorOr[b]]
    in AppM ioEitherB

  -- question about pure
      -- b' = fmap (fmap fn) ioA  
  -- (>>=) (AppM(IO (Right a'))) fn = fn a'
  -- (>>=) (AppM(IO (Left e))) _ = AppM(pure (Left e))
  -- (>>=) a' fn =
  --   let fa = _ fn -- :: AppM(a -> b)
  --   in fa <*> a'

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO = AppM . fmap Right

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  -- throwError e = AppM (fail $ show e)
  throwError e = AppM (pure (Left e))

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM ioA) recoverFn =
    let
      eitherAToAppM = either recoverFn (AppM . pure . Right) -- :: Either Error a -> AppM a
      ioAppMA = fmap eitherAToAppM ioA -- IO (AppM a)
      ioA' = ioAppMA >>= (\(AppM x) -> x) -- IO (Either Error a)
    in AppM ioA'

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither = either throwError pure
-- liftEither errorOrA = AppM $ pure errorOrA 
-- Question: Why throw?

-- Go to 'src/Level05/DB.hs' next.
