module Snail.OS where

import Snail.Types

import Data.Time.Duration (Seconds)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Node.OS as OS
import Prelude ((<$>))

uptime :: Snail Seconds
uptime = liftEffect OS.uptime

cpus :: Snail (Array OS.CPU)
cpus = liftEffect OS.cpus

freemem :: Snail Number
freemem = liftEffect OS.freemem

totalmem :: Snail Number
totalmem = liftEffect OS.totalmem

home :: Snail Folder
home = folder <$> liftEffect OS.homedir

tmp :: Snail Folder
tmp = folder <$> liftEffect OS.tmpdir

hostname :: Snail String
hostname = liftEffect OS.hostname

release :: Snail String
release = liftEffect OS.release

ostype :: Snail String
ostype = liftEffect OS.ostype

networkInterfaces :: Snail (Object (Array OS.NetworkInterface))
networkInterfaces = liftEffect OS.networkInterfaces

loadavg :: Snail { one :: Number, five :: Number, fifteen :: Number }
loadavg = liftEffect OS.loadavg

arch :: Snail OS.Arch
arch = liftEffect OS.arch

endianness :: Snail OS.Endianness
endianness = liftEffect OS.endianness

platform :: Snail OS.Platform
platform = liftEffect OS.platform
