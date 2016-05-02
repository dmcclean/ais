{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS.NMEA
(
  parseAis
, parseAisMessagePackedInNmeaMessage
, translateChar
, aisMessage
, merge
, AisMessageFragment(..)
)
where

import Data.Attoparsec.Text
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Text as T
import Data.Binary.BitPut
import Data.Word (Word8)
import Network.AIS.Vocabulary (Channel(..))

data AisMessageFragment = AisMessageFragment 
                    { fragments :: Int
                    , fragmentNumber :: Int
                    , messageID :: Int
                    , channelCode :: Maybe Channel
                    , payloadFragment :: ByteString
                    , fillBits :: Int  
                    , checksumValid :: Bool
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
               (t, result) <- match checkable
               _ <- char '*'
               h <- anyChar
               l <- anyChar
               let statedChecksum = fromIntegral $ 16 * digitToInt h + digitToInt l
               let computedChecksum = T.foldl' (\x c -> x `xor` (fromIntegral $ ord c)) (0 :: Word8) t
               skipSpace
               return $ result { checksumValid = statedChecksum == computedChecksum }
  where
    checkable = do
                  _ <- string "AIVDM"
                  _ <- char ','
                  fragments <- decimal
                  _ <- char ','
                  fragmentNumber <- decimal
                  _ <- char ','
                  messageID <- option 0 decimal
                  _ <- char ','
                  channelCode <- option Nothing $ parseChannel <$> satisfy (inClass "0-9A-Za-z")
                  _ <- char ','
                  rawPayload <- many' $ satisfy (inClass "0-9A-Wa-w:;<=>?@`")
                  _ <- char ','
                  fillBits <- decimal
                  let payloadFragment = translate rawPayload fillBits
                  let checksumValid = True
                  return $ AisMessageFragment { .. }


parseChannel :: Char -> Maybe Channel
parseChannel 'A' = Just AisChannelA
parseChannel 'a' = Just AisChannelA
parseChannel '1' = Just AisChannelA
parseChannel 'B' = Just AisChannelB
parseChannel 'b' = Just AisChannelB
parseChannel '2' = Just AisChannelB
parseChannel _ = Nothing

translate :: [Char] -> Int -> ByteString
translate cs n = toStrict $ runBitPut $ putThem cs
  where
    putThem [] = putNBits 0 (0 :: Int)
    putThem (x:[]) = putNBits (6 - n) (translateChar x)
    putThem (x:xs) = putNBits 6 (translateChar x) >> putThem xs

merge :: [AisMessageFragment] -> ByteString
merge [] = BS.empty
merge [f] = payloadFragment f
merge fs = toStrict $ runBitPut $ mapM_ put fs
  where
    put f = putLeftByteString (p, v)
      where
        p = payloadFragment f
        v = 8 - fillBits f

translateChar :: Char -> Int
translateChar c | c' > 88 = fromIntegral (c' - 56)
                | otherwise = fromIntegral (c' - 48)
  where
    c' = ord c
