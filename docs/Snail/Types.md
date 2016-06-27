## Module Snail.Types

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

#### `FILE`

``` purescript
data FILE
```

#### `FOLDER`

``` purescript
data FOLDER
```

#### `File`

``` purescript
type File = TagString (file :: FILE)
```

#### `Folder`

``` purescript
type Folder = TagString (folder :: FOLDER)
```

#### `file`

``` purescript
file :: String -> File
```

#### `folder`

``` purescript
folder :: String -> Folder
```

#### `runFile`

``` purescript
runFile :: File -> String
```

#### `runFolder`

``` purescript
runFolder :: Folder -> String
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


