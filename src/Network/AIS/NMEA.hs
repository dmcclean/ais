{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS.NMEA
(
  parseAis
, parseAisMessagePackedInNmeaMessage
, translateChar
, AisMessageFragment(..)
)
where

import Data.Attoparsec.Text
import Data.ByteString
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Text as T
import Data.Binary.BitPut

data AisMessageFragment = AisMessageFragment 
                    { fragments :: Int
                    , fragmentNumber :: Int
                    , messageID :: Int
                    , channelCode :: Char
                    , payloadFragment :: ByteString
                    , fillBits :: Int  
                    }
  deriving (Eq, Show)

parseAis :: Text -> AisMessageFragment
parseAis input = let (Right output) = parseAisMessagePackedInNmeaMessage input
                  in output

parseAisMessagePackedInNmeaMessage :: Text -> Either String AisMessageFragment
parseAisMessagePackedInNmeaMessage = parseOnly (aisMessage <* endOfInput)

aisMessage :: Parser AisMessageFragment
aisMessage = do
               _ <- char '!'
               _ <- string "AIVDM"
               _ <- char ','
               fragments <- decimal
               _ <- char ','
               fragmentNumber <- decimal
               _ <- char ','
               messageID <- option 0 decimal
               _ <- char ','
               channelCode <- anyChar
               _ <- char ','
               rawPayload <- many' $ satisfy (inClass "0-9A-Wa-w:;<=>?@`")
               _ <- char ','
               fillBits <- decimal
               _ <- char '*'
               _ <- anyChar
               _ <- anyChar
               let payloadFragment = translate rawPayload fillBits
               return $ AisMessageFragment { .. }

translate :: [Char] -> Int -> ByteString
translate cs n = toStrict $ runBitPut $ putThem cs
  where
    putThem [] = putNBits 0 (0 :: Int)
    putThem (x:[]) = putNBits (6 - n) (translateChar x)
    putThem (x:xs) = putNBits 6 (translateChar x) >> putThem xs


translateChar :: Char -> Int
translateChar c | c' > 88 = fromIntegral (c' - 56)
                | otherwise = fromIntegral (c' - 48)
  where
    c' = ord c
