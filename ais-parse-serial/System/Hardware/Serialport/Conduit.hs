{-# LANGUAGE ScopedTypeVariables #-}

module System.Hardware.Serialport.Conduit where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString as BS
import Data.Conduit
import System.Hardware.Serialport

sourceSerial :: forall m.(MonadIO m, MonadResource m) => FilePath -> SerialPortSettings -> Source m ByteString
sourceSerial path settings = bracketP (openSerial path settings) closeSerial go
  where
  	go :: SerialPort -> Source m ByteString
  	go port = do
  		        r <- liftIO $ recv port 256
  		        case BS.length r of
  		          0 -> (liftIO $ threadDelay 5000) >> go port
  		          _ -> (yield r) >> go port
