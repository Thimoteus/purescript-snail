module Snail
  ( module T
  , module OS
  , crawl
  , command
  , run
  , echo
  , err
  , (|>)
  , sleep
  , loop
  , cat
  , appendFile, (>>)
  , writeFile, (+>)
  , prependFile
  , andandand, (&&&)
  , ororor, (|||)
  , exists
  , mIf, (~~>)
  , when, (~?>)
  , touch
  , rm
  , rmdir
  , mkdir
  , exit
  , printEnv
  , getVar
  , setVar
  , exitWith, (!?)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.StrMap (StrMap)
import Data.Coercible (class Coercible, coerce)

import Snail.Types as T
import Snail.Types (Snail, Script)
import Snail.OS as OS

import Control.Apply ((*>))
import Control.Monad.Aff (Aff, makeAff, runAff, later', attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)

import Node.Buffer (toString)
import Node.Process (argv, exit, getEnv, setEnv, lookupEnv) as Process
import Node.ChildProcess (CHILD_PROCESS, ExecResult, execFile, defaultExecOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readdir, appendTextFile, writeTextFile, readTextFile, unlink)
import Node.FS.Aff (exists, rmdir, mkdir) as FS
import Node.Path (FilePath)

crawl :: forall a. Snail a -> Script Unit
crawl = runAff (error <<< message) \ _ -> pure unit

echo :: String -> Snail String
echo s = log s *> pure s

err :: String -> Snail String
err s = (liftEff $ error s) *> pure s

infixl 4 bind as |>

sleep :: Int -> Snail Unit
sleep n = later' (n * 1000) $ pure unit

loop :: forall a b. Int -> Snail a -> Snail b
loop n sn = forever $ sn *> sleep n

runFileImp :: forall e. String -> Array String -> Aff ( cp :: CHILD_PROCESS | e ) ExecResult
runFileImp cmd as = makeAff execFileImpl
  where
    execFileImpl fc sc =
      let onRes res = case res.error of
            Just x -> fc x
            _ -> sc res
       in execFile cmd as defaultExecOptions onRes

run :: String -> Array String -> Snail String
run cmd as = do
  res <- runFileImp cmd as
  liftEff $ toString UTF8 res.stdout

command :: String -> Array String -> Snail Unit
command cmd = void <<< run cmd

ls :: T.Folder -> Snail (Array String)
ls = readdir <<< T.runFolder

appendFile :: String -> T.File -> Snail Unit
appendFile s f = appendTextFile UTF8 (T.runFile f) s

infixl 4 appendFile as >>

writeFile :: String -> T.File -> Snail Unit
writeFile s f = writeTextFile UTF8 (T.runFile f) s

infixl 4 writeFile as +>

cat :: T.File -> Snail String
cat = readTextFile UTF8 <<< T.runFile

prependFile :: String -> T.File -> Snail Unit
prependFile str pth = cat pth |> \c -> writeFile (str <> "\n" <> c) pth

andandand :: forall a b. Snail a -> Snail b -> Snail b
andandand sa sb = do
  a <- attempt sa
  case a of
       Left e -> throwError e
       _ -> sb

infixr 3 andandand as &&&

ororor :: forall a b. Snail a -> Snail b -> Snail Unit
ororor sa sb = do
  a <- attempt sa
  case a of
       Right _ -> pure unit
       _ -> sb *> pure unit

infixr 2 ororor as |||

exists :: FilePath -> Snail Boolean
exists = FS.exists

mIf :: forall m a. Monad m => m Boolean -> m a -> m a -> m a
mIf mb mf ms = do
  b <- mb
  if b
     then mf
     else ms

infixr 0 mIf as ~~>

when :: forall m a. Monad m => m Boolean -> m a -> m Unit
when mb ma = (mb ~~> void ma) (pure unit)

infixr 0 when as ~?>

touch :: T.File -> Snail Unit
touch fp = exists (T.runFile fp) ~?> writeFile "" fp

rm :: T.File -> Snail Unit
rm = unlink <<< T.runFile

rmdir :: T.Folder -> Snail Unit
rmdir = FS.rmdir <<< T.runFolder

mkdir :: T.Folder -> Snail Unit
mkdir = FS.mkdir <<< T.runFolder

args :: Snail (Array String)
args = liftEff Process.argv

exit :: forall a. Int -> Snail a
exit = liftEff <<< Process.exit

printEnv :: Snail (StrMap String)
printEnv = liftEff Process.getEnv

setVar :: String -> String -> Snail Unit
setVar k = liftEff <<< Process.setEnv k

newtype Optional a = Optional (Maybe a)

instance coercibleMaybeOptional :: Coercible (Maybe a) (Optional a) where
  coerce = Optional

instance showOptional :: Show (Optional String) where
  show (Optional (Just x)) = x
  show _ = "undefined"

getVar :: String -> Snail String
getVar s = do
  m <- liftEff $ Process.lookupEnv s
  let o = coerce m :: Optional String
  pure $ show m

exitWith :: forall a. Int -> String -> Snail a
exitWith code msg = err msg &&& exit code

infix 0 exitWith as !?
