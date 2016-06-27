## Module Snail

#### `crawl`

``` purescript
crawl :: forall e a. Snail e a -> Script e Unit
```

Runs a Snail computation

#### `echo`

``` purescript
echo :: forall e. String -> Snail e String
```

Print a string to standard output

#### `err`

``` purescript
err :: forall e. String -> Snail e String
```

Print a string to standard error

#### `(|>)`

``` purescript
infixl 4 Control.Bind.bind as |>
```

#### `sleep`

``` purescript
sleep :: forall e. Int -> Snail e Unit
```

Pause the script for a given number of seconds

#### `loop`

``` purescript
loop :: forall a b e. Int -> Snail e a -> Snail e b
```

Given a number of seconds n and a computation c, loop c every n seconds

#### `run`

``` purescript
run :: forall e. String -> Array String -> Snail e String
```

Run a command with an array of arguments, getting the output as a UTF8
encoded string.

#### `command`

``` purescript
command :: forall e. String -> Array String -> Snail e Unit
```

Run a command, disregarding the output.

#### `fork`

``` purescript
fork :: forall e. String -> Array String -> Snail e Unit
```

Send a process into the background.

#### `ls`

``` purescript
ls :: forall e. Folder -> Snail e { files :: Array String, folders :: Array String }
```

Given a folder, partition its contents into files and subfolders.

#### `appendFile`

``` purescript
appendFile :: forall e. String -> File -> Snail e Unit
```

Append a given string to the end of a given file.

#### `(>>)`

``` purescript
infixl 4 appendFile as >>
```

#### `writeFile`

``` purescript
writeFile :: forall e. String -> File -> Snail e Unit
```

Create or overwrite the given file with the given UTF8 string.

#### `(+>)`

``` purescript
infixl 4 writeFile as +>
```

#### `cat`

``` purescript
cat :: forall e. File -> Snail e String
```

Get the contents of the given file.

#### `prependFile`

``` purescript
prependFile :: forall e. String -> File -> Snail e Unit
```

Add a given string to the beginning of the given file.

#### `andandand`

``` purescript
andandand :: forall a b e. Snail e a -> Snail e b -> Snail e b
```

Run the first computation, and stop if that fails. Otherwise run the second.

#### `(&&&)`

``` purescript
infixr 3 andandand as &&&
```

#### `ororor`

``` purescript
ororor :: forall a b e. Snail e a -> Snail e b -> Snail e Unit
```

Run the first computation, and stop if it succeeds.

#### `(|||)`

``` purescript
infixr 2 ororor as |||
```

#### `exists`

``` purescript
exists :: forall address e. Address address => address -> Snail e Boolean
```

Check if a file or folder exists

#### `mIf`

``` purescript
mIf :: forall m a. Bind m => m Boolean -> (Unit -> m a) -> (Unit -> m a) -> m a
```

Given a monadic boolean, run either the first thunked computation or the second.

#### `(~~>)`

``` purescript
infixr 1 mIf as ~~>
```

#### `when`

``` purescript
when :: forall m a. Monad m => m Boolean -> (Unit -> m a) -> m Unit
```

Run a given computation when a monadic boolean is true.

#### `(~?>)`

``` purescript
infixr 1 when as ~?>
```

#### `touch`

``` purescript
touch :: forall e. File -> Snail e Unit
```

Create a file with no contents.

#### `rm`

``` purescript
rm :: forall e. File -> Snail e Unit
```

Delete a given file.

#### `rmdir`

``` purescript
rmdir :: forall e. Folder -> Snail e Unit
```

Delete a given folder.

#### `mkdir`

``` purescript
mkdir :: forall e. Folder -> Snail e Unit
```

Create a given directory.

#### `args`

``` purescript
args :: forall e. Snail e (Array String)
```

Get all the arguments to the script.

#### `params`

``` purescript
params :: forall e. Snail e (Array String)
```

Get the arguments to the script, minus the first two, the first of which is
always "node", the second is the name of the file being executed.

#### `input`

``` purescript
input :: forall e. Snail e String
```

Get the first non-mandatory argument, which must have necessarily been
provided.

#### `exit`

``` purescript
exit :: forall a e. Int -> Snail e a
```

Exit the script with the given exit code.

#### `printEnv`

``` purescript
printEnv :: forall e. Snail e (StrMap String)
```

Get the current environment.

#### `setVar`

``` purescript
setVar :: forall e. String -> String -> Snail e Unit
```

Set the given environment variable to the given string value.

#### `getVar`

``` purescript
getVar :: forall e. String -> Snail e String
```

Get the given environment variable's value.

#### `exitWith`

``` purescript
exitWith :: forall a e. Int -> String -> Snail e a
```

Exit the script with the given exit code, printing the given message to
standard output if the exit code is 0, and standard error otherwise.

#### `(!?)`

``` purescript
infix 5 exitWith as !?
```

#### `fromJust`

``` purescript
fromJust :: forall a e. String -> Maybe a -> Snail e a
```

Extract the value of a Just, failing with a given error message otherwise.

#### `fromMaybe`

``` purescript
fromMaybe :: forall a e. a -> Maybe a -> Snail e a
```

Extract the value of a Maybe with a default if Nothing is found.

#### `fromEither`

``` purescript
fromEither :: forall e a b. Show a => Either a b -> Snail e b
```

Extract the Right value of an Either, otherwise turn the Left into an error.


### Re-exported from Snail.OS:

#### `uptime`

``` purescript
uptime :: forall e. Snail e Seconds
```

#### `totalmem`

``` purescript
totalmem :: forall e. Snail e Number
```

#### `tmp`

``` purescript
tmp :: forall e. Snail e Folder
```

#### `release`

``` purescript
release :: forall e. Snail e String
```

#### `platform`

``` purescript
platform :: forall e. Snail e Platform
```

#### `ostype`

``` purescript
ostype :: forall e. Snail e String
```

#### `networkInterfaces`

``` purescript
networkInterfaces :: forall e. Snail e (StrMap (Array NetworkInterface))
```

#### `loadavg`

``` purescript
loadavg :: forall e. Snail e { one :: Number, five :: Number, fifteen :: Number }
```

#### `hostname`

``` purescript
hostname :: forall e. Snail e String
```

#### `home`

``` purescript
home :: forall e. Snail e Folder
```

#### `freemem`

``` purescript
freemem :: forall e. Snail e Number
```

#### `endianness`

``` purescript
endianness :: forall e. Snail e Endianness
```

#### `cpus`

``` purescript
cpus :: forall e. Snail e (Array CPU)
```

#### `arch`

``` purescript
arch :: forall e. Snail e Arch
```

### Re-exported from Snail.Path:

#### `pathpend`

``` purescript
pathpend :: Folder -> File -> File
```

#### `(</>)`

``` purescript
infixl 6 pathpend as </>
```

### Re-exported from Snail.Types:

#### `SnailEff`

``` purescript
type SnailEff e = (fs :: FS, console :: CONSOLE, cp :: CHILD_PROCESS, process :: PROCESS, err :: EXCEPTION, buffer :: BUFFER, os :: OS | e)
```

#### `Snail`

``` purescript
type Snail e = Aff (SnailEff e)
```

#### `Script`

``` purescript
type Script e = Eff (SnailEff e)
```

#### `Folder`

``` purescript
type Folder = TagString (folder :: FOLDER)
```

#### `File`

``` purescript
type File = TagString (file :: FILE)
```

#### `FOLDER`

``` purescript
data FOLDER
```

#### `FILE`

``` purescript
data FILE
```

#### `Address`

``` purescript
class Address a where
  getAddress :: a -> String
```

##### Instances
``` purescript
Address (TagString a)
```

#### `runFolder`

``` purescript
runFolder :: Folder -> String
```

#### `runFile`

``` purescript
runFile :: File -> String
```

#### `folder`

``` purescript
folder :: String -> Folder
```

#### `file`

``` purescript
file :: String -> File
```

