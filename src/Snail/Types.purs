module Snail.Types
  ( FILE, FOLDER
  , File, Folder
  , file
  , folder
  , runFile
  , runFolder
  , class Address
  , getAddress
  ) where

import Data.String.Yarn (TagString, tag, runTag)


data FILE
data FOLDER

type File = TagString (file :: FILE)
type Folder = TagString (folder :: FOLDER)

file :: String -> File
file = tag

folder :: String -> Folder
folder = tag

runFile :: File -> String
runFile = runTag

runFolder :: Folder -> String
runFolder = runTag

class Address a where
  getAddress :: a -> String

instance addressTagString :: Address (TagString a) where
  getAddress = runTag
