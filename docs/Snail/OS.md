## Module Snail.OS

#### `uptime`

``` purescript
uptime :: forall e. Snail e Seconds
```

#### `cpus`

``` purescript
cpus :: forall e. Snail e (Array CPU)
```

#### `freemem`

``` purescript
freemem :: forall e. Snail e Number
```

#### `totalmem`

``` purescript
totalmem :: forall e. Snail e Number
```

#### `home`

``` purescript
home :: forall e. Snail e Folder
```

#### `tmp`

``` purescript
tmp :: forall e. Snail e Folder
```

#### `hostname`

``` purescript
hostname :: forall e. Snail e String
```

#### `release`

``` purescript
release :: forall e. Snail e String
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

#### `arch`

``` purescript
arch :: forall e. Snail e Arch
```

#### `endianness`

``` purescript
endianness :: forall e. Snail e Endianness
```

#### `platform`

``` purescript
platform :: forall e. Snail e Platform
```


