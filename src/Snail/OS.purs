module Snail.OS where

import Prelude ((<$>))

import Snail.Types

import Node.OS as OS

import Data.Time.Duration (Seconds)
import Data.StrMap (StrMap)

import Control.Monad.Eff.Class (liftEff)

uptime :: forall e. Snail e Seconds
uptime = liftEff OS.uptime

cpus :: forall e. Snail e (Array OS.CPU)
cpus = liftEff OS.cpus

freemem :: forall e. Snail e Number
freemem = liftEff OS.freemem

totalmem :: forall e. Snail e Number
totalmem = liftEff OS.totalmem

home :: forall e. Snail e Folder
home = folder <$> liftEff OS.homedir

tmp :: forall e. Snail e Folder
tmp = folder <$> liftEff OS.tmpdir

hostname :: forall e. Snail e String
hostname = liftEff OS.hostname

release :: forall e. Snail e String
release = liftEff OS.release

ostype :: forall e. Snail e String
ostype = liftEff OS.ostype

networkInterfaces :: forall e. Snail e (StrMap (Array OS.NetworkInterface))
networkInterfaces = liftEff OS.networkInterfaces

loadavg :: forall e. Snail e { one :: Number, five :: Number, fifteen :: Number }
loadavg = liftEff OS.loadavg

arch :: forall e. Snail e OS.Arch
arch = liftEff OS.arch

endianness :: forall e. Snail e OS.Endianness
endianness = liftEff OS.endianness

platform :: forall e. Snail e OS.Platform
platform = liftEff OS.platform
