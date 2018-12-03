-- | Basic utility functions for dealing with paths
module Snail.Path where

import Prelude

import Effect.Class (liftEffect)
import Node.Process as Process
import Snail.Types (File, Folder, Snail, file, runFolder)

pathpend :: Folder -> File -> File
pathpend fld ext = file (runFolder fld) <> ext

infixl 6 pathpend as </>

chdir :: Folder -> Snail Unit
chdir fld = liftEffect $ Process.chdir $ runFolder fld
