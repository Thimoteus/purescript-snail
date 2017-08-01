module Test.Main where

import Snail
import Prelude (Unit, bind, pure, (>>=), (<>), discard)
import Data.Either (Either(..))
import Control.Monad.Eff.Random as Random
import Control.Monad.Aff (attempt)

testBind :: forall e a. String -> Snail e a -> Snail e a
testBind msg s = attempt s >>= case _ of
  Left err -> 1 !? msg
  Right succ -> pure succ

main :: Script ( random :: Random.RANDOM ) Unit
main = crawl do
  tilde <- testBind "home test failed" home
  echo "Echo test" ||| 1 !? "echo test failed"
  sleep 1 ||| 1 !? "sleep test failed"
  mkdir (tilde <> folder "/testsnail") ||| 1 !? "mkdir test failed"
  rmdir (tilde <> folder "/testsnail") ||| 1 !? "rmdir test failed"
  touch (tilde </> file "/testsnail") ||| 1 !? "touch test failed"
  rm (tilde </> file "/testsnail") ||| 1 !? "rm test failed"
  0 !? "Tests succeeded"

