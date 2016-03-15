{-# LANGUAGE OverloadedStrings #-}

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

parseAis :: Text -> ByteString
parseAis input = let (Right output) = parseAisMessagePackedInNmeaMessage input
                  in output

parseAisMessagePackedInNmeaMessage :: Text -> Either String ByteString
parseAisMessagePackedInNmeaMessage = parseOnly (aisMessage <* endOfInput)

aisMessage :: Parser ByteString
aisMessage = do
               char '!'
               string "AIVDM"
               char ','
               fragments <- decimal
               char ','
               fragmentNumber <- decimal
               char ','
               messageID <- option 0 decimal
               char ','
               channelCode <- anyChar
               char ','
               payload <- many' $ satisfy (inClass "0-9A-Wa-w:;<=>?@`")
               char ','
               fillBits <- decimal
               char '*'
               anyChar
               anyChar
               return $ translate payload fillBits

translate :: [Char] -> Int -> ByteString
translate cs fill = toStrict $ runBitPut $ mapM (putNBits 6) (fmap translateChar cs) >> putNBits fill zero
  where
    zero = 0 :: Int

translateChar :: Char -> Int
translateChar c | c' > 88 = fromIntegral (c' - 56)
                | otherwise = fromIntegral (c' - 48)
  where
    c' = ord c
