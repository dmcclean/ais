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

parseAis :: Text -> (ByteString, Int)
parseAis input = let (Right output) = parseAisMessagePackedInNmeaMessage input
                  in output

parseAisMessagePackedInNmeaMessage :: Text -> Either String (ByteString, Int)
parseAisMessagePackedInNmeaMessage = parseOnly (aisMessage <* endOfInput)

aisMessage :: Parser (ByteString, Int)
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
               payload <- many' $ satisfy (inClass "0-9A-Wa-w:;<=>?@`")
               _ <- char ','
               fillBits <- decimal
               _ <- char '*'
               _ <- anyChar
               _ <- anyChar
               let result = translate payload fillBits
               let valid = (((6 * Prelude.length payload) - fillBits) - (8 * (Data.ByteString.length result - 1))) `mod` 8

               return (result, valid)

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
