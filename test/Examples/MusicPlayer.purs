module Test.MusicPlayer where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.NonEmpty ((:|))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt) as Random
import Node.Path (extname)
import Snail (crawl, folder, fromJust, fromMaybe, home, ls, params)
import Snail.Process (fork)

main :: Effect Unit
main = crawl do
  input <- Array.head <$> params
  tilde <- home
  dir <- fromMaybe (tilde <> folder "/Music/") $ folder <$> input
  files' <- ls dir
  let files = Array.filter (eq ".mp3" <<< extname) files'.files
  i <- liftEffect $ Random.randomInt 0 $ Array.length files - 1
  mp3 <- fromJust "No files available" $ files !! i
  fork ("vlc" :| [mp3])

