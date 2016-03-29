{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS.NMEA
(
  parseAis
, parseAisMessagePackedInNmeaMessage
, translateChar
)
where

import Data.Attoparsec.Text
import Data.ByteString
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Text as T
import Data.Binary.BitPut

data AisNmeaMessage = AisNmeaMessage 
                    { fragments :: Int
                    , fragmentNumber :: Int
                    , messageID :: Int
                    , channelCode :: Char
                    , payload :: ByteString
                    , fillBits :: Int  
                    }

parseAis :: Text -> AisNmeaMessage
parseAis input = let (Right output) = parseAisMessagePackedInNmeaMessage input
                  in output

parseAisMessagePackedInNmeaMessage :: Text -> Either String AisNmeaMessage
parseAisMessagePackedInNmeaMessage = parseOnly (aisMessage <* endOfInput)

aisMessage :: Parser AisNmeaMessage
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
               let payload = translate rawPayload fillBits
               return $ AisNmeaMessage { .. }

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
