module Network.AIS.Main where

import Network.AIS
import Network.AIS.NMEA
import Control.Monad.Trans
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary.Strict.BitGet
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Text

main :: IO ()
main = do
         let path = "C:\\Users\\Doug\\Downloads\\nmea-sample\\nmea-sample.txt"
         let source = sourceFile path
         let cond = decode utf8 =$= Data.Conduit.Text.lines =$= conduitParser aisMessage =$= CL.map (f . snd)
         let sink = transPipe lift $ CL.mapM_ (\x -> do 
                                                       putStrLn x
                                                       _ <- getLine
                                                       return ())
         runResourceT (source $$ cond =$ sink)

f :: AisMessageFragment -> String
f x | fragments x == 1 = show $ runBitGet (payloadFragment x) getMessage
    | otherwise = "Fragmented message."
