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
  , class Address
  , getAddress
  ) where

import Prelude

import Data.Monoid (class Monoid)
import Data.Generic (class Generic)

import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.OS (OS)
import Node.ChildProcess (CHILD_PROCESS)
import Node.Process (PROCESS)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)

type SnailEff e = ( fs :: FS
                  , console :: CONSOLE 
                  , cp :: CHILD_PROCESS
                  , process :: PROCESS
                  , err :: EXCEPTION
                  , buffer :: BUFFER
                  , os :: OS
                  | e
                  )

type Snail e = Aff (SnailEff e)

type Script e = Eff (SnailEff e)

newtype TagString (a :: # *) = Tag String

derive instance eqTagString :: Eq (TagString a)

derive instance ordTagString :: Ord (TagString a)

derive instance genericTagString :: Generic (TagString a)

instance semigroupTagString :: Semigroup (TagString a) where
  append (Tag s) (Tag t) = Tag (s <> t)

instance monoidTagString :: Monoid (TagString a) where
  mempty = Tag ""

data FILE
data FOLDER

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

class Address a where
  getAddress :: a -> String

instance addressTagString :: Address (TagString a) where
  getAddress = runTag
