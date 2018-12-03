module Test.Main where

import Snail

import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import Effect.Aff (attempt)
import Prelude (Unit, bind, pure, (>>=), (<>), discard)
import Snail.Process (exec)

testBind :: forall a. String -> Snail a -> Snail a
testBind msg s = attempt s >>= case _ of
  Left err -> 1 !? msg
  Right succ -> pure succ

testPulp :: Snail String
testPulp = do
  exec ("pulp" :| ["build"])

main :: Script Unit
main = crawl do
  tilde <- testBind "home test failed" home
  echo "Echo test" ||| 1 !? "echo test failed"
  sleep 1 ||| 1 !? "sleep test failed"
  mkdir (tilde <> folder "/testsnail") ||| 1 !? "mkdir test failed"
  rmdir (tilde <> folder "/testsnail") ||| 1 !? "rmdir test failed"
  touch (tilde </> file "/testsnail") ||| 1 !? "touch test failed"
  rm (tilde </> file "/testsnail") ||| 1 !? "rm test failed"
  0 !? "Tests succeeded"

