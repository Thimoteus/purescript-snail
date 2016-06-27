module Test.MusicPlayer where

import Prelude
import Snail

import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Random as Random

import Data.Array as Array
import Data.Array ((!!))

import Node.Path (extname)

main :: Script ( random :: Random.RANDOM ) Unit
main = crawl do
  input <- Array.head <$> params
  tilde <- home
  dir <- fromMaybe (tilde <> folder "/Music/") $ folder <$> input
  files' <- ls dir
  let files = Array.filter (eq ".mp3" <<< extname) files'.files
  i <- Eff.liftEff $ Random.randomInt 0 $ Array.length files - 1
  mp3 <- fromJust "No files available" $ files !! i
  fork "vlc" [mp3]

