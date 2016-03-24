{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS
(
  AisMessage
, getMessage
, example
)
where

import Control.Monad
import Data.Bits
import Data.Binary.Strict.BitGet
import Data.ByteString as BS
import Data.Int
import Data.Text as T
import Data.Time
import Data.Word
import Network.AIS.Vocabulary
import Numeric.Units.Dimensional.FixedPoint (changeRep)
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.Coercion

type Latitude = PlaneAngle Double
type Longitude = PlaneAngle Double

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

getLatitude :: BitGet Latitude
getLatitude = do
                raw <- getAsInt32 27
                return $ changeRep $ (coerce raw :: Latitude')

getLongitude :: BitGet Longitude
getLongitude = do
                 raw <- getAsInt32 28
                 return $ changeRep $ (coerce raw :: Longitude')

getAsInt8 :: Int -> BitGet Int8
getAsInt8 n = fmap (signExtendRightAlignedWord n) (getAsWord8 n)

getAsInt16 :: Int -> BitGet Int16
getAsInt16 n = fmap (signExtendRightAlignedWord n) (getAsWord16 n)

getAsInt32 :: Int -> BitGet Int32
getAsInt32 n = fmap (signExtendRightAlignedWord n) (getAsWord32 n)

getAsInt64 :: Int -> BitGet Int64
getAsInt64 n = fmap (signExtendRightAlignedWord n) (getAsWord64 n)

-- Assumes but does not verify that a and b have the same finite size.
signExtendRightAlignedWord :: (FiniteBits a, FiniteBits b, Integral a, Integral b) => Int -> a -> b
signExtendRightAlignedWord n x = fromIntegral (x `shiftL` s) `shiftR` s
  where
    s = finiteBitSize x - n
{-# INLINE signExtendRightAlignedWord #-}    

data AisMessage = ClassAPositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , navigationalStatus :: NavigationalStatus
                  , rateOfTurn :: Int8
                  , speedOverGround :: Word16
                  , positionAccuracy :: Bool
                  , longitude :: Longitude
                  , latitude :: Latitude
                  , courseOverGround :: Word16
                  , trueHeading :: Word16
                  , timeStamp :: Maybe Word8
                  , positionFixingStatus :: PositionFixingStatus
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
                | AddressedBinaryMessage
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , sequenceNumber :: Word8
                  , destinationID :: MMSI
                  , retransmitFlag :: Bool
                  , applicationIdentifier :: ApplicationIdentifier
                  , payload :: ByteString
                  }
                | BinaryBroadcastMessage
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , applicationIdentifier :: ApplicationIdentifier
                  , payload :: ByteString
                  }
                | AcknowledgementMessage
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , acknowledgements :: [Acknowledgement]
                  }
                | BaseStationReport
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , utcTime :: UTCTime
                  , positionAccuracy :: Bool
                  , longitude :: Longitude
                  , latitude :: Latitude
                  , positionFixingDevice :: PositionFixingDevice
                  , doNotSuppressLongRangeMessages :: Bool
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | TimeInquiry
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , destinationID :: MMSI
                  }
                | TimeResponse
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , utcTime :: UTCTime
                  , positionAccuracy :: Bool
                  , longitude :: Longitude
                  , latitude :: Latitude
                  , positionFixingDevice :: PositionFixingDevice
                  , doNotSuppressLongRangeMessages :: Bool
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | ClassAStaticData
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , aisVersionIndicator :: Word8
                  , imoNumber :: Word32
                  , callSign :: Text
                  , name :: Text
                  , typeOfShipAndCargo :: Word8
                  , vesselDimensions :: VesselDimensions
                  , positionFixingDevice :: PositionFixingDevice
                  , eta :: Word32
                  , draught :: Word8
                  , destination :: Text
                  , dteNotReady :: Bool
                  }
                | StaticDataReportPartA
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , partNumber :: Word8
                  , name :: Text
                  }
                | StaticDataReportPartB
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , userID :: MMSI
                  , partNumber :: Word8
                  , typeOfShipAndCargo :: Word8
                  , vendorID :: Text
                  , callSign :: Text
                  , vesselDimensions :: VesselDimensions
                  , positionFixingDevice :: PositionFixingDevice
                  }
                | AidToNavigationReport
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , aidID :: MMSI
                  , typeOfAid :: AidToNavigation
                  , name :: Text
                  , positionAccuracy :: Bool
                  , longitude :: Longitude
                  , latitude :: Latitude
                  , aidDimensions :: VesselDimensions
                  , positionFixingDevice :: PositionFixingDevice
                  , timeStamp :: Maybe Word8
                  , positionFixingStatus :: PositionFixingStatus
                  , offPosition :: Bool
                  , aidStatus :: Word8
                  , raimFlag :: Bool
                  , virtualFlag :: Bool
                  , assignedModeFlag :: Bool
                  }
                | InterrogationMessage
                  { messageType :: MessageID
                  , repeatIndicator :: Word8
                  , sourceID :: MMSI
                  , interrogations :: [Interrogation]
                  }

  deriving (Eq, Show)

getMessageType :: BitGet MessageID
getMessageType = fmap (toEnum . fromIntegral) $ getAsWord8 6

getNavigationalStatus :: BitGet NavigationalStatus
getNavigationalStatus = fmap (toEnum . fromIntegral) $ getAsWord8 4

getTimeStamp :: BitGet (Maybe Word8, PositionFixingStatus)
getTimeStamp = do
                 n <- getAsWord8 6
                 return $ case n of
                            60 -> (Nothing, PosStatusNormal)
                            61 -> (Nothing, PosStatusManual)
                            62 -> (Nothing, PosStatusEstimated)
                            63 -> (Nothing, PosStatusInoperative)
                            _  -> (Just n,  PosStatusNormal)

getMMSI :: BitGet MMSI
getMMSI = fmap MMSI $ getAsWord32 30

getSyncState :: BitGet SyncState
getSyncState = fmap (toEnum . fromIntegral) $ getAsWord8 2

getAidToNavigation :: BitGet AidToNavigation
getAidToNavigation = fmap (toEnum . fromIntegral) $ getAsWord8 5

getApplicationIdentifier :: BitGet ApplicationIdentifier
getApplicationIdentifier = do
                             designatedAreaCode <- getAsWord16 10
                             functionIdentifier <- getAsWord8 6
                             return $ ApplicationIdentifier { .. }

getAcknowledgement :: BitGet Acknowledgement
getAcknowledgement = do
                       destinationID <- getMMSI
                       sequenceNumber <- getAsWord8 2
                       return $ Acknowledgement { .. }

getVesselDimensions :: BitGet VesselDimensions
getVesselDimensions = do
                        forwardOfReferencePoint <- getAsWord16 9
                        aftOfReferencePoint <- getAsWord16 9
                        portOfReferencePoint <- getAsWord16 6
                        starboardOfReferencePoint <- getAsWord16 6
                        return $ VesselDimensions { .. }

getPositionFixingDevice :: BitGet PositionFixingDevice
getPositionFixingDevice = fmap (f . fromIntegral) $ getAsWord8 4
  where
    f 15 = PosFixInternalGnss
    f n = toEnum n

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

getMessage :: BitGet AisMessage
getMessage = do
               messageType <- getMessageType
               case messageType of
                 MNone -> undefined
        {-  1 -} MScheduledClassAPositionReport -> getClassAPositionReport MScheduledClassAPositionReport getSOTDMACommunicationsState
        {-  2 -} MAssignedScheduledClassAPositionReport -> getClassAPositionReport MAssignedScheduledClassAPositionReport getSOTDMACommunicationsState
        {-  3 -} MSpecialClassAPositionReport -> getClassAPositionReport MSpecialClassAPositionReport getITDMACommunicationsState
        {-  4 -} MBaseStationReport -> getBaseStationReportMessage
        {-  5 -} MStaticAndVoyageData -> getClassAStaticData
        {-  6 -} MBinaryAddressedMessage -> getAddressedBinaryMessage
        {-  7 -} MBinaryAcknowledgement -> getAcknowledgementMessage MBinaryAcknowledgement
        {-  8 -} MBinaryBroadcastMessage -> getBinaryBroadcastMessage
        {-  9 -} MStandardSarAircraftPositionReport -> undefined
        {- 10 -} MTimeInquiry -> getTimeInquiry
        {- 11 -} MTimeResponse -> getTimeResponse
        {- 12 -} MAddressedSafetyRelatedMessage -> getAddressedSafetyRelatedMessage
        {- 13 -} MSafetyRelatedAcknowledgement -> getAcknowledgementMessage MSafetyRelatedAcknowledgement
        {- 14 -} MSafetyRelatedBroadcastMessage -> getSafetyRelatedBroadcastMessage
        {- 15 -} MInterrogation -> getInterrogationMessage
        {- 16 -} MAssignmentModeCommand -> undefined
        {- 17 -} MDgnssBroadcastBinaryMessage -> undefined
        {- 18 -} MStandardClassBPositionReport -> undefined
        {- 19 -} MExtendedClassBPositionReport -> undefined
        {- 20 -} MDataLinkManagementMessage -> undefined
        {- 21 -} MAidToNavigationReport -> getAidToNavigationReport
        {- 22 -} MChannelManagement -> undefined
        {- 23 -} MGroupAssignmentCommand -> undefined
        {- 24 -} MStaticDataReport -> getStaticDataReport
        {- 25 -} MSingleSlotBinaryMessage -> undefined
        {- 26 -} MMultipleSlotBinaryMessage -> undefined
        {- 27 -} MLongRangePositionReport -> undefined

getClassAPositionReport :: MessageID -> BitGet CommunicationsState -> BitGet AisMessage
getClassAPositionReport messageType getCommState = do
                            repeatIndicator <- getAsWord8 2
                            userID <- getMMSI
                            navigationalStatus <- getNavigationalStatus
                            rateOfTurn <- getAsInt8 8
                            speedOverGround <- getAsWord16 10
                            positionAccuracy <- getBit
                            longitude <- getLongitude
                            latitude <- getLatitude
                            courseOverGround <- getAsWord16 12
                            trueHeading <- getAsWord16 9
                            (timeStamp, positionFixingStatus) <- getTimeStamp
                            manueverIndicator <- getAsWord8 2
                            skip 3
                            raimFlag <- getBit
                            communicationsState <- getCommState
                            return $ ClassAPositionReport { .. }

getAddressedSafetyRelatedMessage :: BitGet AisMessage
getAddressedSafetyRelatedMessage = do
                                     let messageType = MAddressedSafetyRelatedMessage
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
                                     let messageType = MSafetyRelatedBroadcastMessage
                                     repeatIndicator <- getAsWord8 2
                                     sourceID <- getMMSI
                                     skip 2
                                     safetyRelatedText <- getRemainingSixBitText
                                     return $ SafetyRelatedBroadcastMessage { .. }

getAddressedBinaryMessage :: BitGet AisMessage
getAddressedBinaryMessage = do
                              let messageType = MBinaryAddressedMessage
                              repeatIndicator <- getAsWord8 2
                              sourceID <- getMMSI
                              sequenceNumber <- getAsWord8 2
                              destinationID <- getMMSI
                              retransmitFlag <- getBit
                              skip 1
                              applicationIdentifier <- getApplicationIdentifier
                              n <- remaining
                              payload <- getLeftByteString n
                              if (n `mod` 8 == 0)
                                then return $ AddressedBinaryMessage { .. }
                                else error "Length of payload was not an even number of bytes."

getBinaryBroadcastMessage :: BitGet AisMessage
getBinaryBroadcastMessage = do
                              let messageType = MBinaryBroadcastMessage
                              repeatIndicator <- getAsWord8 2
                              sourceID <- getMMSI
                              skip 2
                              applicationIdentifier <- getApplicationIdentifier
                              n <- remaining
                              payload <- getLeftByteString n
                              if (n `mod` 8 == 0)
                                then return $ BinaryBroadcastMessage { .. }
                                else error "Length of payload was not an even number of bytes."

getBaseStationReportMessage :: BitGet AisMessage
getBaseStationReportMessage = do
                                let messageType = MBaseStationReport
                                repeatIndicator <- getAsWord8 2
                                userID <- getMMSI
                                year <- getAsWord16 14
                                month <- getAsWord16 4
                                day <- getAsWord16 5
                                hour <- getAsWord16 5
                                minute <- getAsWord16 6
                                second <- getAsWord16 6
                                positionAccuracy <- getBit
                                longitude <- getLongitude
                                latitude <- getLatitude
                                positionFixingDevice <- getPositionFixingDevice
                                doNotSuppressLongRangeMessages <- getBit
                                skip 9
                                raimFlag <- getBit
                                communicationsState <- getSOTDMACommunicationsState
                                let utcTime = makeUtcTime year month day hour minute second
                                return $ BaseStationReport { .. }

getInterrogationMessage :: BitGet AisMessage
getInterrogationMessage = do
                            let messageType = MInterrogation
                            repeatIndicator <- getAsWord8 2
                            sourceID <- getMMSI
                            skip 2
                            interrogations <- getInterrogations
                            return $ InterrogationMessage { .. }
  where
    getInterrogations :: BitGet [Interrogation]
    getInterrogations = do
                          interrogatedID <- getMMSI
                          requestedMessageType <- getMessageType
                          requestedSlotOffset <- getAsWord16 12
                          getInterrogationsAfterFirst $ Interrogation { .. }
    getInterrogationsAfterFirst :: Interrogation -> BitGet [Interrogation]
    getInterrogationsAfterFirst x = do
                                      n <- remaining
                                      if n >= 20
                                        then do
                                          skip 2
                                          requestedMessageType <- getMessageType
                                          requestedSlotOffset <- getAsWord16 12
                                          if (requestedMessageType == MNone && requestedSlotOffset == 0)
                                            then getInterrogationsAfterSecond [x]
                                            else getInterrogationsAfterSecond [x, Interrogation { interrogatedID = interrogatedID x, requestedMessageType = requestedMessageType, requestedSlotOffset = requestedSlotOffset } ]
                                        else return [x]
    getInterrogationsAfterSecond xs = do
                                        n <- remaining
                                        if n >= 52
                                          then do
                                            skip 2
                                            interrogatedID <- getMMSI
                                            requestedMessageType <- getMessageType
                                            requestedSlotOffset <- getAsWord16 12
                                            skip 2
                                            return $ xs ++ [Interrogation { .. }]
                                          else return xs

getTimeInquiry :: BitGet AisMessage
getTimeInquiry = do
                   let messageType = MTimeInquiry
                   repeatIndicator <- getAsWord8 2
                   sourceID <- getMMSI
                   skip 2
                   destinationID <- getMMSI
                   skip 2
                   return $ TimeInquiry { .. }

getTimeResponse :: BitGet AisMessage
getTimeResponse = do
                    let messageType = MTimeResponse
                    repeatIndicator <- getAsWord8 2
                    userID <- getMMSI
                    year <- getAsWord16 14
                    month <- getAsWord16 4
                    day <- getAsWord16 5
                    hour <- getAsWord16 5
                    minute <- getAsWord16 6
                    second <- getAsWord16 6
                    positionAccuracy <- getBit
                    longitude <- getLongitude
                    latitude <- getLatitude
                    positionFixingDevice <- getPositionFixingDevice
                    doNotSuppressLongRangeMessages <- getBit
                    skip 9
                    raimFlag <- getBit
                    communicationsState <- getSOTDMACommunicationsState
                    let utcTime = makeUtcTime year month day hour minute second
                    return $ TimeResponse { .. }

getClassAStaticData :: BitGet AisMessage
getClassAStaticData = do
                        let messageType = MStaticAndVoyageData
                        repeatIndicator <- getAsWord8 2
                        userID <- getMMSI
                        aisVersionIndicator <- getAsWord8 2
                        imoNumber <- getAsWord32 30
                        callSign <- getSixBitText 7
                        name <- getSixBitText 20
                        typeOfShipAndCargo <- getAsWord8 8
                        vesselDimensions <- getVesselDimensions
                        positionFixingDevice <- getPositionFixingDevice
                        eta <- getAsWord32 20
                        draught <- getAsWord8 8
                        destination <- getSixBitText 20
                        dteNotReady <- getBit
                        skip 1
                        return $ ClassAStaticData { .. }

getStaticDataReport :: BitGet AisMessage
getStaticDataReport = do
                        let messageType = MStaticDataReport
                        repeatIndicator <- getAsWord8 2
                        userID <- getMMSI
                        partNumber <- getAsWord8 2
                        case partNumber of
                          0 -> do
                                 name <- getSixBitText 20
                                 return $ StaticDataReportPartA { .. }
                          1 -> do
                                 typeOfShipAndCargo <- getAsWord8 8
                                 vendorID <- getSixBitText 7
                                 callSign <- getSixBitText 7
                                 vesselDimensions <- getVesselDimensions
                                 positionFixingDevice <- getPositionFixingDevice
                                 skip 2
                                 return $ StaticDataReportPartB { .. }

getAcknowledgementMessage :: MessageID -> BitGet AisMessage
getAcknowledgementMessage messageType = do
                                          repeatIndicator <- getAsWord8 2
                                          sourceID <- getMMSI
                                          skip 2
                                          n <- remaining
                                          if (n `mod` 32 == 0)
                                            then do
                                                   acknowledgements <- replicateM (n `div` 32) getAcknowledgement
                                                   return $ AcknowledgementMessage { .. }
                                            else error "Acknowledgement message contained partial acknowledgements of incorrect length."

getAidToNavigationReport :: BitGet AisMessage
getAidToNavigationReport = do
                             let messageType = MAidToNavigationReport
                             repeatIndicator <- getAsWord8 2
                             aidID <- getMMSI
                             typeOfAid <- getAidToNavigation
                             initialName <- getSixBitText 20
                             positionAccuracy <- getBit
                             longitude <- getLongitude
                             latitude <- getLatitude
                             aidDimensions <- getVesselDimensions
                             positionFixingDevice <- getPositionFixingDevice
                             (timeStamp, positionFixingStatus) <- getTimeStamp
                             offPosition <- getBit
                             aidStatus <- getAsWord8 8
                             raimFlag <- getBit
                             virtualFlag <- getBit
                             assignedModeFlag <- getBit
                             skip 1
                             (n, s) <- fmap (`divMod` 6) remaining
                             extendedName <- getSixBitText n
                             skip s
                             let name = T.concat [initialName, extendedName]
                             return $ AidToNavigationReport { .. }

makeUtcTime :: Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> UTCTime
makeUtcTime y mo d h m s = UTCTime { .. }
  where
    y' = fromIntegral y
    mo' = fromIntegral mo
    d' = fromIntegral d
    h' = fromIntegral h
    m' = fromIntegral m
    s' = fromIntegral s
    utctDay = fromGregorian y' mo' d'
    utctDayTime = secondsToDiffTime $ s' + 60 * m' + 60 * 60 * h'

example :: ByteString
example = BS.pack [0b00000100, 0b00110000, 0b11110101, 0b01000011, 0b01111011, 0b11100000, 0b00000000,
                   0b00001000, 0b00010100, 0b00101100, 0b10000010, 0b00011101, 0b01010000, 0b01010111,
                   0b01100100, 0b01010011, 0b11111111, 0b11010000, 0b00001001, 0b01000001, 0b11110011]
