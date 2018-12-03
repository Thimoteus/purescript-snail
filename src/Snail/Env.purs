module Snail.Env where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Node.Process as Process

-- | Get the current environment.
printEnv :: Aff (Object String)
printEnv = liftEffect Process.getEnv

-- | Set the given environment variable to the given string value.
setVar :: String -> String -> Aff Unit
setVar k = liftEffect <<< Process.setEnv k

newtype Optional a = Optional (Maybe a)
derive instance newtypeOptional ∷ Newtype (Optional a) _

instance showOptional :: Show (Optional String) where
  show (Optional (Just x)) = x
  show _ = "undefined"

-- | Get the given environment variable's value.
getVar :: String -> Aff String
getVar s = do
  m <- liftEffect $ Process.lookupEnv s
  pure $ show $ wrap m :: Optional String
