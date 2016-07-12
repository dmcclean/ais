{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS.NMEA
(
  parseAisFragment
, parseAisFragment'
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

-- | A fragment of an AIS message.
data AisMessageFragment = AisMessageFragment 
                    { fragments :: Int -- ^ The total number of fragments in the message.
                    , fragmentNumber :: Int -- ^ The 1-indexed sequence number of the fragment within its message.
                    , messageID :: Int -- ^ An identifier for the message of which this is a fragment.
                    , channelCode :: Maybe Channel -- ^ An optional 'Channel' indicating where the message of which this is a fragment was received.
                    , payloadFragment :: ByteString -- ^ A fragment of the message itself.
                    , fillBits :: Int -- ^ The number of fill bits added to the end of the message payload.
                    , checksumValid :: Bool -- ^ Whether the message fragment carried a valid checksum.
                    }
  deriving (Eq, Show)

parseAisFragment' :: Text -> Maybe AisMessageFragment
parseAisFragment' = either (const Nothing) Just . parseAisFragment

parseAisFragment :: Text -> Either String AisMessageFragment
parseAisFragment = parseOnly (aisMessage <* endOfInput)

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
