module Snail.OS where

import Prelude ((<$>))

import Snail.Types

import Node.OS as OS

import Data.Time (Seconds)
import Data.StrMap (StrMap)

import Control.Monad.Eff.Class (liftEff)

uptime :: Snail Seconds
uptime = liftEff OS.uptime

cpus :: Snail (Array OS.CPU)
cpus = liftEff OS.cpus

freemem :: Snail Number
freemem = liftEff OS.freemem

totalmem :: Snail Number
totalmem = liftEff OS.totalmem

home :: Snail Folder
home = folder <$> liftEff OS.homedir

tmp :: Snail Folder
tmp = folder <$> liftEff OS.tmpdir

hostname :: Snail String
hostname = liftEff OS.hostname

release :: Snail String
release = liftEff OS.release

ostype :: Snail String
ostype = liftEff OS.ostype

networkInterfaces :: Snail (StrMap (Array OS.NetworkInterface))
networkInterfaces = liftEff OS.networkInterfaces

loadavg :: Snail { one :: Number, five :: Number, fifteen :: Number }
loadavg = liftEff OS.loadavg

arch :: Snail OS.Arch
arch = liftEff OS.arch

endianness :: Snail OS.Endianness
endianness = liftEff OS.endianness

platform :: Snail OS.Platform
platform = liftEff OS.platform
