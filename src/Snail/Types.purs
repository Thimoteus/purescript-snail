module Snail.Types
  ( Snail
  , Script
  , SnailEff
  , FILE, FOLDER
  , File, Folder
  , file
  , folder
  , runFile
  , runFolder
  , class Address
  , getAddress
  ) where

import Data.String.Yarn (TagString, tag, runTag)

import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.OS (OS)
import Node.ChildProcess (CHILD_PROCESS)
import Node.Process (PROCESS)

import Control.Monad.Promise (Promise)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)

type SnailEff e =
  ( fs :: FS
  , console :: CONSOLE
  , cp :: CHILD_PROCESS
  , process :: PROCESS
  , err :: EXCEPTION
  , exception :: EXCEPTION
  , buffer :: BUFFER
  , os :: OS
  | e )

type Snail e = Promise (SnailEff e)

type Script e = Eff (SnailEff e)

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
