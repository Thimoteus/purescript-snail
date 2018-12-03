module Snail.OS where

import Snail.Types

import Data.Time.Duration (Seconds)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Node.OS as OS
import Prelude ((<$>))

uptime :: Aff Seconds
uptime = liftEffect OS.uptime

cpus :: Aff (Array OS.CPU)
cpus = liftEffect OS.cpus

freemem :: Aff Number
freemem = liftEffect OS.freemem

totalmem :: Aff Number
totalmem = liftEffect OS.totalmem

home :: Aff Folder
home = folder <$> liftEffect OS.homedir

tmp :: Aff Folder
tmp = folder <$> liftEffect OS.tmpdir

hostname :: Aff String
hostname = liftEffect OS.hostname

release :: Aff String
release = liftEffect OS.release

ostype :: Aff String
ostype = liftEffect OS.ostype

networkInterfaces :: Aff (Object (Array OS.NetworkInterface))
networkInterfaces = liftEffect OS.networkInterfaces

loadavg :: Aff { one :: Number, five :: Number, fifteen :: Number }
loadavg = liftEffect OS.loadavg

arch :: Aff OS.Arch
arch = liftEffect OS.arch

endianness :: Aff OS.Endianness
endianness = liftEffect OS.endianness

platform :: Aff OS.Platform
platform = liftEffect OS.platform
