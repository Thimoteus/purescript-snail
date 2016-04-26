module Snail.Types
  ( Snail
  , Script
  , SnailEff
  , FILE, FOLDER
  , File, Folder
  , TagString
  , file
  , folder
  , runFile
  , runFolder
  ) where

import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.OS (OS)
import Node.ChildProcess (CHILD_PROCESS)
import Node.Process (PROCESS)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)

type SnailEff = ( fs :: FS
                , console :: CONSOLE 
                , cp :: CHILD_PROCESS
                , process :: PROCESS
                , err :: EXCEPTION
                , buffer :: BUFFER
                , os :: OS
                )

type Snail = Aff SnailEff

type Script = Eff SnailEff

data FILE
data FOLDER
newtype TagString (a :: # *) = Tag String

type File = TagString (file :: FILE)
type Folder = TagString (folder :: FOLDER)

runTag :: forall a. TagString a -> String
runTag (Tag s) = s

file :: String -> File
file = Tag

folder :: String -> Folder
folder = Tag

runFile :: File -> String
runFile = runTag

runFolder :: Folder -> String
runFolder = runTag
