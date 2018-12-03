module Snail.Env where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Node.Process as Process
import Snail.Types (Snail)

-- | Get the current environment.
printEnv :: Snail (Object String)
printEnv = liftEffect Process.getEnv

-- | Set the given environment variable to the given string value.
setVar :: String -> String -> Snail Unit
setVar k = liftEffect <<< Process.setEnv k

newtype Optional a = Optional (Maybe a)
derive instance newtypeOptional ∷ Newtype (Optional a) _

instance showOptional :: Show (Optional String) where
  show (Optional (Just x)) = x
  show _ = "undefined"

-- | Get the given environment variable's value.
getVar :: String -> Snail String
getVar s = do
  m <- liftEffect $ Process.lookupEnv s
  pure $ show $ wrap m :: Optional String
