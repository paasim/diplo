{-# LANGUAGE NoImplicitPrelude #-}
module Error where

import RIO
import Text.Trifecta ( ErrInfo (..) )

-- Error handling
data Validated a = ParsingError ErrInfo   -- from parsing, ie. Trifecta
                 | ValidationError String -- from other validation
                 | Valid a                -- valid stuff

instance Functor Validated where
  fmap f (Valid a) = Valid (f a)
  fmap f (ParsingError e) = ParsingError e
  fmap f (ValidationError s) = ValidationError s

instance Applicative Validated where
  pure = Valid
  (Valid f) <*> (Valid a)                      = Valid (f a)
  (ValidationError s) <*> (ValidationError s') = ValidationError (s <> "\n" <> s')
  (ParsingError e) <*> _                       = ParsingError e
  _ <*> (ParsingError e)                       = ParsingError e
  (ValidationError s) <*> _                    = ValidationError s
  _ <*> (ValidationError s)                    = ValidationError s

instance Monad Validated where
  Valid a           >>= f = f a
  ParsingError e    >>= f = ParsingError e
  ValidationError s >>= f = ValidationError s

-- There must be a better way to only print the errDoc
extractErrDoc (ErrInfo errDoc _) = errDoc

instance Show a => Show (Validated a) where
  show (Valid a)             = show a
  show (ParsingError e)      = show (extractErrDoc e) <> "\n"
  show (ValidationError s) = "Validation errors:\n" <> s

-- this is for handling IO with Validated
newtype ValidatedT m a = ValidatedT { runValidatedT :: m (Validated a) }

instance Functor m => Functor (ValidatedT m) where
  fmap f (ValidatedT mva) = ValidatedT $ fmap f <$> mva

instance Applicative m => Applicative (ValidatedT m) where
  pure                            = ValidatedT . pure . pure
  ValidatedT f <*> ValidatedT mva = ValidatedT $ (<*>) <$> f <*> mva 

instance Monad m => Monad (ValidatedT m) where
  (ValidatedT mva) >>= f = ValidatedT $ do 
    va <- mva
    case va of
      (Valid a) -> runValidatedT (f a)
      (ParsingError e) -> return (ParsingError e)
      (ValidationError s) -> return (ValidationError s)

instance MonadTrans ValidatedT where
  lift ma = ValidatedT $ fmap Valid ma

joinInsideValidatedT :: Monad m => ValidatedT m (Validated a) -> ValidatedT m a
joinInsideValidatedT (ValidatedT mvva) = ValidatedT . fmap join $ mvva

