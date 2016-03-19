{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS
(
  AisMessage
, getClassAPositionReport
, example
)
where

import Control.Monad
import Data.Bits
import Data.Binary.Strict.BitGet
import Data.ByteString as BS
import Data.Int
import Data.Text as T
import Data.Word
import Network.AIS.Vocabulary

sixBitCharacters :: Text
sixBitCharacters = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^- !\"#$%&`()*+,-./0123456789:;<=>?"

-- | Gets a 6-bit character string whose length is specified in characters.
getSixBitText :: Int -> BitGet Text
getSixBitText n = do
                    cs <- replicateM n $ fmap (T.index sixBitCharacters . fromIntegral) $ getAsWord8 6
                    return $ T.pack cs

getRemainingSixBitText :: BitGet Text
getRemainingSixBitText = do
                           bits <- remaining
                           getSixBitText $ bits `div` 6

getAsInt8 :: Int -> BitGet Int8
getAsInt8 n = do
                x <- getAsWord8 n
                let y = x `shiftL` (8 - n)
                let y' = fromIntegral y `shiftR` (8 - n)
                return y'

getAsInt16 :: Int -> BitGet Int16
getAsInt16 n = do
                 x <- getAsWord16 n
                 let y = x `shiftL` (16 - n)
                 let y' = fromIntegral y `shiftR` (16 - n)
                 return y'

getAsInt32 :: Int -> BitGet Int32
getAsInt32 n = do
                 x <- getAsWord32 n
                 let y = x `shiftL` (32 - n)
                 let y' = fromIntegral y `shiftR` (32 - n)
                 return y'

getAsInt64 :: Int -> BitGet Int64
getAsInt64 n = do
                 x <- getAsWord64 n
                 let y = x `shiftL` (64 - n)
                 let y' = fromIntegral y `shiftR` (64 - n)
                 return y'  

data AisMessage = ClassAPositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , navigationalStatus :: NavigationalStatus
                  , rateOfTurn :: Int8
                  , speedOverGround :: Word16
                  , positionAccuracy :: Bool
                  , longitude :: Int32
                  , latitude :: Int32
                  , courseOverGround :: Word16
                  , trueHeading :: Word16
                  , timeStamp :: Word8
                  , manueverIndicator :: Word8
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | AddressedSafetyRelatedMessage
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , sequenceNumber :: Word8
                  , destinationID :: MMSI
                  , retransmitFlag :: Bool
                  , safetyRelatedText :: Text                                     
                  }
                | SafetyRelatedBroadcastMessage
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , safetyRelatedText :: Text
                  }
  deriving (Eq, Show)

getMessageType :: BitGet MessageID
getMessageType = fmap (toEnum . subtract 1 . fromIntegral) $ getAsWord8 6

getNavigationalStatus :: BitGet NavigationalStatus
getNavigationalStatus = fmap (toEnum . fromIntegral) $ getAsWord8 4

getMMSI :: BitGet MMSI
getMMSI = fmap MMSI $ getAsWord32 30

getSyncState :: BitGet SyncState
getSyncState = fmap (toEnum . fromIntegral) $ getAsWord8 2

getSOTDMACommunicationsState :: BitGet CommunicationsState
getSOTDMACommunicationsState = do
                                 syncState <- getSyncState
                                 slotTimeout <- getAsWord8 3
                                 submessage <- getSubmessage slotTimeout
                                 return $ SOTDMA { .. }

getSubmessage :: Word8 -> BitGet SOTDMASubmessage
getSubmessage 0 = fmap SlotOffset $ getAsWord16 14
getSubmessage 1 = do
                    hour <- getAsWord8 5
                    minute <- getAsWord8 7
                    return $ UTCHourAndMinute hour minute
getSubmessage n | even n = fmap SlotNumber $ getAsWord16 14
                | otherwise = fmap ReceivedStations $ getAsWord16 14

getITDMACommunicationsState :: BitGet CommunicationsState
getITDMACommunicationsState = do
                                syncState <- getSyncState
                                slotIncrement' <- getAsWord16 13
                                numberOfSlots' <- getAsWord8 3
                                keep <- getBit
                                let (slotIncrement, numberOfSlots) = parseIncrementAndSlots slotIncrement' numberOfSlots'
                                return $ ITDMA { .. }
  where
    parseIncrementAndSlots :: Word16 -> Word8 -> (Word16, Word8)
    parseIncrementAndSlots inc n | n <= 4    = (inc, n + 1)
                                 | otherwise = (inc + 8192, n - 4)

getClassAPositionReport :: BitGet AisMessage
getClassAPositionReport = do
                            messageType <- getMessageType
                            repeatIndicator <- getAsWord8 2
                            userID <- getMMSI
                            navigationalStatus <- getNavigationalStatus
                            rateOfTurn <- getAsInt8 8
                            speedOverGround <- getAsWord16 10
                            positionAccuracy <- getBit
                            longitude <- getAsInt32 28
                            latitude <- getAsInt32 27
                            courseOverGround <- getAsWord16 12
                            trueHeading <- getAsWord16 9
                            timeStamp <- getAsWord8 6
                            manueverIndicator <- getAsWord8 2
                            skip 3
                            raimFlag <- getBit
                            communicationsState <- case messageType of
                                                     MScheduledClassAPositionReport -> getSOTDMACommunicationsState
                                                     MAssignedScheduledClassAPositionReport -> getSOTDMACommunicationsState
                                                     MSpecialClassAPositionReport -> getITDMACommunicationsState
                                                     _ -> getSOTDMACommunicationsState
                            return $ ClassAPositionReport { .. }

getAddressedSafetyRelatedMessage :: BitGet AisMessage
getAddressedSafetyRelatedMessage = do
                                     messageType <- getMessageType
                                     repeatIndicator <- getAsWord8 2
                                     sourceID <- getMMSI
                                     sequenceNumber <- getAsWord8 2
                                     destinationID <- getMMSI
                                     retransmitFlag <- getBit
                                     skip 1
                                     safetyRelatedText <- getRemainingSixBitText
                                     return $ AddressedSafetyRelatedMessage { .. }

getSafetyRelatedBroadcastMessage :: BitGet AisMessage
getSafetyRelatedBroadcastMessage = do
                                     messageType <- getMessageType
                                     repeatIndicator <- getAsWord8 2
                                     sourceID <- getMMSI
                                     skip 2
                                     safetyRelatedText <- getRemainingSixBitText
                                     return $ SafetyRelatedBroadcastMessage { .. }

example :: ByteString
example = BS.pack [0b00000100, 0b00110000, 0b11110101, 0b01000011, 0b01111011, 0b11100000, 0b00000000,
                   0b00001000, 0b00010100, 0b00101100, 0b10000010, 0b00011101, 0b01010000, 0b01010111,
                   0b01100100, 0b01010011, 0b11111111, 0b11010000, 0b00001001, 0b01000001, 0b11110011]
