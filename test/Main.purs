module Test.Main where

import Prelude (Unit, bind)
import Snail

main :: Script Unit
main = crawl do
  echo "hi!"
  sleep 5
  echo "bye!"
