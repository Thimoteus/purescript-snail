-- | Basic utility functions for dealing with paths
module Snail.Path where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Snail.Types (File, Folder, Snail, file, runFolder)
import Node.Process as Process

pathpend :: Folder -> File -> File
pathpend fld ext = file (runFolder fld) <> ext

infixl 6 pathpend as </>

chdir :: forall e. Folder -> Snail e Unit
chdir fld = liftEff $ Process.chdir $ runFolder fld
