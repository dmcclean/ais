{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Hardware.Serialport
import System.Hardware.Serialport.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary.Strict.BitGet
import Data.ByteString
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import Data.Conduit.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.AIS
import Network.AIS.NMEA

main :: IO ()
main = do
         let settings = daisySettings
         [port] <- getArgs
         T.putStrLn "AIS Serial Port Parser"
         T.putStrLn $ "Serial Port: " `T.append` (T.pack port)
         -- putStrLn $ show settings -- no show instance
         T.putStrLn ""
         let source = sourceSerial port settings
         let cond = decode utf8 =$= Data.Conduit.Text.lines =$= CL.isolate 10000 =$= conduitParser aisMessage =$= CL.map snd =$= mergeFragments =$= CL.map (\m -> runBitGet m getMessage) =$= CL.map (T.pack . show)
         let sink = transPipe liftIO $ CL.mapM_ T.putStrLn
         runResourceT (source $$ cond =$ sink)

daisySettings :: SerialPortSettings
daisySettings = defaultSerialSettings { commSpeed = CS38400 }

mergeFragments :: (Monad m) => Conduit AisMessageFragment m ByteString
mergeFragments = do
                   frag <- await
                   case frag of
                     Nothing -> return ()
                     (Just f) | fragmentNumber f == 1 -> gatherFragments (fragments f) [f]
                              | otherwise             -> mergeFragments

gatherFragments :: (Monad m) => Int -> [AisMessageFragment] -> Conduit AisMessageFragment m ByteString
gatherFragments cnt fs | Prelude.length fs == cnt = yield (merge fs) >> mergeFragments
                       | otherwise = do
                                       f <- await
                                       case f of
                                         Nothing -> return ()
                                         (Just f') | (fragments f' == cnt) && (fragmentNumber f' == 1 + Prelude.length fs) -> gatherFragments cnt (fs ++ [f'])
                                                   | otherwise -> mergeFragments
