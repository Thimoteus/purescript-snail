-- | Basic utility functions for dealing with paths
module Snail.Path where

import Prelude
import Snail.Types (File, Folder, runFolder, file)

pathpend :: Folder -> File -> File
pathpend fld ext = file (runFolder fld) <> ext

infixl 6 pathpend as </>
