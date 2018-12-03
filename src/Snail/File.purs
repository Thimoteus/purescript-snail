module Snail.File where

import Prelude

import Data.Array (partition, zip)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, readTextFile, readdir, stat, unlink, writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Perms (Perms)
import Node.FS.Stats (isFile)
import Snail.Control (exists, existsCheck, notExistsCheck, (~?>))
import Snail.Path ((</>))
import Snail.Types (Snail)
import Snail.Types as T

-- | Given a folder, partition its contents into files and subfolders.
ls :: T.Folder -> Snail { files :: Array String, folders :: Array String }
ls f = existsCheck f do
  let f' = T.runFolder f
  arr <- map (append f') <$> readdir f'
  stats <- traverse stat arr
  let res = partition (isFile <<< snd) $ zip arr stats
      toFilesFolders {yes, no} = {files: map fst yes, folders: map fst no}
  pure $ toFilesFolders res

-- | Append a given string to the end of a given file.
appendFile :: String -> T.File -> Snail Unit
appendFile s f = existsCheck f $ appendTextFile UTF8 (T.runFile f) s

infixl 4 appendFile as >>

-- | Create or overwrite the given file with the given UTF8 string.
writeFile :: String -> T.File -> Snail Unit
writeFile s f = writeTextFile UTF8 (T.runFile f) s

infixl 4 writeFile as +>

-- | Get the contents of the given file.
cat :: T.File -> Snail String
cat f = existsCheck f $ readTextFile UTF8 (T.runFile f)

-- | Add a given string to the beginning of the given file.
prependFile :: String -> T.File -> Snail Unit
prependFile str pth = do
  content <- cat pth
  str <> "\n" <> content +> pth

-- | Create a file with no contents.
touch :: T.File -> Snail Unit
touch fp = not <$> exists fp ~?> \ _ -> writeFile "" fp

-- | Delete a given file.
rm :: T.File -> Snail Unit
rm f = existsCheck f $ unlink $ T.runFile f

-- | Delete a given folder.
rmdir :: T.Folder -> Snail Unit
rmdir f = existsCheck f $ FS.rmdir $ T.runFolder f

-- | Create a given directory.
mkdir :: T.Folder -> Snail Unit
mkdir f = notExistsCheck f $ FS.mkdir $ T.runFolder f

-- | Move a file to a folder with an optional new name.
mv :: T.File -> T.Folder -> Maybe String -> Snail Unit
mv file folder = case _ of
  Just newName -> FS.rename (T.runFile file) (T.runFile $ folder </> T.file newName)
  _ -> FS.rename (T.runFile file) (T.runFile $ folder </> file)

cp :: T.File -> T.Folder -> Maybe String -> Snail Unit
cp file folder name = do
  contents <- cat file
  contents +> folder </> maybe file T.file name

chmod :: T.File -> Perms -> Snail Unit
chmod file perms = FS.chmod (T.runFile file) perms
