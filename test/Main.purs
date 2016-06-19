module Test.Main where

import Prelude
import Snail (Snail, Script, fork, fromJust, ls, folder, fromMaybe, home, params, echo, sleep, crawl)
import Data.Array as Array
import Data.Array ((!!))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Node.Path (extname)

main :: Script ( random :: RANDOM ) Unit
main = crawl do
  echo "Prepare for musical greatness ... "
  sleep 2
  echo "Playing!"
  app

app :: Snail ( random :: RANDOM ) Unit
app = do
  ps <- Array.head <$> params
  tilde <- home
  dir <- fromMaybe (tilde <> folder "/Music/") $ folder <$> ps
  files' <- ls dir
  let files = Array.filter (eq ".mp3" <<< extname) files'.files
  i <- liftEff $ randomInt 0 $ Array.length files - 1
  mp3 <- fromJust "No files available" $ files !! i
  fork "vlc" [mp3]

