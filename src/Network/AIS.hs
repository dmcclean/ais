{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS
(
  ClassAPositionReport
, getClassAPositionReport
, example
)
where

import Data.Bits
import Data.Binary.Strict.BitGet
import Data.ByteString
import Data.Int
import Data.Word

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

type MessageID = Word8
type MMSI = Word32
type NavigationalStatus = Word8

data ClassAPositionReport = ClassAPositionReport
                          {
                            messageType :: MessageID
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
                          , communicationState :: Word32
                          }
  deriving (Eq, Show)

getClassAPositionReport :: BitGet ClassAPositionReport
getClassAPositionReport = do
                            messageType <- getAsWord8 6
                            repeatIndicator <- getAsWord8 2
                            userID <- getAsWord32 30
                            navigationalStatus <- getAsWord8 4
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
                            communicationState <- getAsWord32 19
                            return $ ClassAPositionReport { ..}

example :: ByteString
example = pack [0b00000100, 0b00110000, 0b11110101, 0b01000011, 0b01111011, 0b11100000, 0b00000000,
                0b00001000, 0b00010100, 0b00101100, 0b10000010, 0b00011101, 0b01010000, 0b01010111,
                0b01100100, 0b01010011, 0b11111111, 0b11010000, 0b00001001, 0b01000001, 0b11110011]
