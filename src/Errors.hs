{-# LANGUAGE NoImplicitPrelude #-}
module Errors where

import RIO
import Text.Trifecta ( ErrInfo )

-- Error handling
data Validated a = ParsingError ErrInfo | ValidationError String | Valid a

instance Functor Validated where
  fmap f (Valid a) = Valid (f a)
  fmap f (ParsingError e) = ParsingError e
  fmap f (ValidationError s) = ValidationError s

instance Applicative Validated where
  pure a = Valid a
  (Valid f) <*> (Valid a)                      = Valid (f a)
  (ValidationError s) <*> (ValidationError s') = ValidationError (s ++ "\n" ++ s')
  (ParsingError e) <*> _                       = ParsingError e
  _ <*> (ParsingError e)                       = ParsingError e
  (ValidationError s) <*> _                    = ValidationError s
  _ <*> (ValidationError s)                    = ValidationError s

instance Monad Validated where
  Valid a           >>= f = f a
  ParsingError e    >>= f = ParsingError e
  ValidationError s >>= f = ValidationError s

instance Show a => Show (Validated a) where
  show (Valid a)             = show a ++ "\n"
  show (ParsingError e)      = show e ++ "\n"
  show (ValidationError s) = "Validation errors:\n" ++ s


data ValidatedT m a = ValidatedT { runValidatedT :: m (Validated a) }

instance Functor m => Functor (ValidatedT m) where
  fmap f (ValidatedT mva) = ValidatedT $ (fmap f) <$> mva

instance Applicative m => Applicative (ValidatedT m) where
  pure a                          = ValidatedT . pure . pure $ a
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

