module Main where

import Network.AIS
import Network.AIS.NMEA
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary.Strict.BitGet
import Data.ByteString
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Text
import qualified Data.Text as T

main :: IO ()
main = do
         let path = "C:\\Users\\Douglas\\Downloads\\nmea-sample\\nmea-sample.txt"
         let source = sourceFile path
         let cond = decode utf8 =$= Data.Conduit.Text.lines =$= CL.isolate 10000 =$= conduitParser aisMessage =$= CL.map snd =$= mergeFragments =$= CL.map (\m -> runBitGet m getMessage) =$= CL.filter filt =$= CL.map show
         let sink = CL.map (++ "\n") =$= CL.map T.pack =$= encode utf8 =$ sinkFile "C:\\Users\\Douglas\\Downloads\\nmea-sample\\decoded.txt"
         runResourceT (source $$ cond =$ sink)

filt :: Either String AisMessage -> Bool
filt _ = True

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
