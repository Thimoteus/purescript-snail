module Snail.Env where

import Prelude

import Control.Coercible (class Coercible, coerce)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Node.Process as Process
import Snail.Types (Snail)

-- | Get the current environment.
printEnv :: forall e. Snail e (StrMap String)
printEnv = liftEff Process.getEnv

-- | Set the given environment variable to the given string value.
setVar :: forall e. String -> String -> Snail e Unit
setVar k = liftEff <<< Process.setEnv k

newtype Optional a = Optional (Maybe a)

instance coercibleMaybeOptional :: Coercible (Maybe a) (Optional a) where
  coerce = Optional

instance showOptional :: Show (Optional String) where
  show (Optional (Just x)) = x
  show _ = "undefined"

-- | Get the given environment variable's value.
getVar :: forall e. String -> Snail e String
getVar s = do
  m <- liftEff $ Process.lookupEnv s
  pure $ show $ coerce m :: Optional String
