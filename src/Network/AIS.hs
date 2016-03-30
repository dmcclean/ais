{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AIS
(
  AisMessage(..)
, getMessage
, isPositionReport
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
import Numeric.Units.Dimensional.Coercion

sixBitCharacters :: Text
sixBitCharacters = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^- !\"#$%&`()*+,-./0123456789:;<=>?"

-- | Gets a 6-bit character string whose length is specified in characters.
getSixBitText :: Int -> BitGet Text
getSixBitText n = T.pack <$> replicateM n getSixBitChar

getSixBitChar :: BitGet Char
getSixBitChar = T.index sixBitCharacters . fromIntegral <$> getAsWord8 6

getRemainingSixBitText :: BitGet Text
getRemainingSixBitText = do
                           bits <- remaining
                           getSixBitText $ bits `div` 6

getLongitude :: BitGet (Maybe Longitude)
getLongitude = fmap f $ getAsInt32 28
  where
    f x | -54000000 <= x && x <= 54000000 = Just $ coerce x
        | otherwise                       = Nothing

getLatitude :: BitGet (Maybe Latitude)
getLatitude = fmap f $ getAsInt32 27
  where
    f x | -108000000 <= x && x <= 108000000 = Just $ coerce x
        | otherwise                         = Nothing

getLowResolutionLongitude :: BitGet (Maybe Longitude)
getLowResolutionLongitude = fmap f $ getAsInt32 18
  where
    f x | -54000 <= x && x <= 54000 = Just $ coerce (x * 1000)
        | otherwise                 = Nothing

getLowResolutionLatitude :: BitGet (Maybe Latitude)
getLowResolutionLatitude = fmap f $ getAsInt32 17
  where
    f x | -108000 <= x && x <= 108000 = Just $ coerce (x * 1000)
        | otherwise                   = Nothing

getSpeed :: Int -> BitGet (Speed s Word16)
getSpeed n = do
               let na = (1 `shiftL` n) - 1
               let h = na - 1
               x <- getAsWord16 n
               return $ case x of
                          x' | x' == na -> SpeedNotAvailable
                             | x' == h -> SpeedHigh
                          _ -> SpeedSpecified $ coerce x

--getAsInt8 :: Int -> BitGet Int8
--getAsInt8 n = signExtendRightAlignedWord n <$> getAsWord8 n

--getAsInt16 :: Int -> BitGet Int16
--getAsInt16 n = signExtendRightAlignedWord n <$> getAsWord16 n

getAsInt32 :: Int -> BitGet Int32
getAsInt32 n = signExtendRightAlignedWord n <$> getAsWord32 n

--getAsInt64 :: Int -> BitGet Int64
--getAsInt64 n = signExtendRightAlignedWord n <$> getAsWord64 n

-- Assumes but does not verify that a and b have the same finite size.
signExtendRightAlignedWord :: (FiniteBits a, FiniteBits b, Integral a, Integral b) => Int -> a -> b
signExtendRightAlignedWord n x = fromIntegral (x `shiftL` s) `shiftR` s
  where
    s = finiteBitSize x - n
{-# INLINE signExtendRightAlignedWord #-}    

data AisMessage = ClassAPositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , navigationalStatus :: NavigationalStatus
                  , rateOfTurn :: PackedRateOfTurn
                  , speedOverGround :: VelocityTenthsOfKnot
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , courseOverGround :: Maybe Course
                  , trueHeading :: Maybe Heading
                  , timeStamp :: Maybe Word8
                  , positionFixingStatus :: PositionFixingStatus
                  , maneuverIndicator :: SpecialManeuverIndicator
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | SafetyRelatedMessage
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , sequenceNumber :: Maybe Word8
                  , addressee :: Addressee
                  , retransmitFlag :: Bool
                  , safetyRelatedText :: Text                                     
                  }
                | BinaryMessage
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , sequenceNumber :: Maybe Word8
                  , addressee :: Addressee
                  , retransmitFlag :: Bool
                  , applicationIdentifier :: Maybe ApplicationIdentifier
                  , payload :: ByteString
                  , optionalCommunicationsState :: Maybe CommunicationsState
                  }
                | AcknowledgementMessage
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , acknowledgements :: [Acknowledgement]
                  }
                | BaseStationReport
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , utcTime :: UTCTime
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , positionFixingDevice :: PositionFixingDevice
                  , doNotSuppressLongRangeMessages :: Bool
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | TimeInquiry
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , destinationID :: MMSI
                  }
                | TimeResponse
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , utcTime :: UTCTime
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , positionFixingDevice :: PositionFixingDevice
                  , doNotSuppressLongRangeMessages :: Bool
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | ClassAStaticData
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , aisVersionIndicator :: Word8
                  , imoNumber :: Word32
                  , callSign :: Text
                  , name :: Text
                  , typeOfShipAndCargo :: Word8
                  , vesselDimensions :: VesselDimensions
                  , positionFixingDevice :: PositionFixingDevice
                  , eta :: Word32 -- TODO: improve type of this field, which is month / day / hour / minute
                  , draught :: LengthDecimeters Word8
                  , destination :: Text
                  , dteNotReady :: Bool
                  }
                | StaticDataReportPartA
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , partNumber :: Word8
                  , name :: Text
                  }
                | StaticDataReportPartB
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
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
                  , repeatIndicator :: RepeatIndicator
                  , aidID :: MMSI
                  , typeOfAid :: AidToNavigation
                  , name :: Text
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
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
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , interrogations :: [Interrogation]
                  }
                | DataLinkManagementMessage
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , reservations :: [Reservation]
                  }
                | AssignmentModeCommand
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , assignments :: [Assignment]
                  }
                | DgnssBroadcastMessage
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , sourceID :: MMSI
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , payload :: ByteString
                  }
                | SarAircraftPositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , altitude :: Altitude
                  , speedOverGround' :: VelocityKnots
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , courseOverGround :: Maybe Course
                  , timeStamp :: Maybe Word8
                  , positionFixingStatus :: PositionFixingStatus
                  , altitudeSensor :: AltitudeSensor
                  , dteNotReady :: Bool
                  , assignedModeFlag :: Bool
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | StandardClassBPositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , speedOverGround :: VelocityTenthsOfKnot
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , courseOverGround :: Maybe Course
                  , trueHeading :: Maybe Heading
                  , timeStamp :: Maybe Word8
                  , positionFixingStatus :: PositionFixingStatus
                  , capabilities :: ClassBCapabilities
                  , assignedModeFlag :: Bool
                  , raimFlag :: Bool
                  , communicationsState :: CommunicationsState
                  }
                | ExtendedClassBPositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , speedOverGround :: VelocityTenthsOfKnot
                  , positionAccuracy :: Bool
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , courseOverGround :: Maybe Course
                  , trueHeading :: Maybe Heading
                  , timeStamp :: Maybe Word8
                  , positionFixingStatus :: PositionFixingStatus
                  , name :: Text
                  , typeOfShipAndCargo :: Word8
                  , vesselDimensions :: VesselDimensions
                  , positionFixingDevice :: PositionFixingDevice
                  , raimFlag :: Bool
                  , dteNotReady :: Bool
                  , assignedModeFlag :: Bool
                  }
                | ChannelManagementCommand
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , channelA :: Channel
                  , channelB :: Channel
                  , transmissionMode :: TransmissionMode
                  , useLowPower :: Bool
                  , targetDesignation :: TargetDesignation
                  , transitionalZoneSize :: LengthNauticalMiles Word8
                  }
                | GroupAssignmentCommand
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , region :: Region
                  , stationType :: StationType
                  , typeOfShipAndCargo :: Word8
                  , transmissionMode :: TransmissionMode
                  , reportingInterval :: AssignedReportingInterval
                  , quietTime :: TimeMinutes Word8
                  }
                | BaseStationCoverageAreaMessage
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , region :: Region
                  }
                | LongRangePositionReport
                  { messageType :: MessageID
                  , repeatIndicator :: RepeatIndicator
                  , userID :: MMSI
                  , positionAccuracy :: Bool
                  , raimFlag :: Bool
                  , navigationalStatus :: NavigationalStatus
                  , longitude :: Maybe Longitude
                  , latitude :: Maybe Latitude
                  , speedOverGround' :: VelocityKnots
                  , courseOverGround :: Maybe Course
                  , positionLatency :: Bool
                  }
                | InvalidMessage
  deriving (Eq, Show)

isPositionReport :: AisMessage -> Bool
isPositionReport (ClassAPositionReport { .. }) = True
isPositionReport (StandardClassBPositionReport { .. }) = True
isPositionReport (ExtendedClassBPositionReport { .. }) = True
isPositionReport (SarAircraftPositionReport { .. }) = True
isPositionReport _ = False

getMessageType :: BitGet MessageID
getMessageType = f . fromIntegral <$> getAsWord8 6
  where
    f n | n > 27 = MNone
        | otherwise = toEnum n

getRepeatIndicator :: BitGet RepeatIndicator
getRepeatIndicator = getAsWord8 2

getNavigationalStatus :: BitGet NavigationalStatus
getNavigationalStatus = toEnum . fromIntegral <$> getAsWord8 4

getReservation :: BitGet Reservation
getReservation = do
                   reservedOffsetNumber <- getAsWord16 12
                   reservedNumberOfSlots <- getAsWord8 4
                   reservationTimeout <- coerce <$> getAsWord8 3
                   reservationIncrement <- getAsWord16 11
                   return $ Reservation { .. }

getAssignment :: BitGet Assignment
getAssignment = do
                  targetID <- getMMSI
                  assignedSlotOffset <- getAsWord16 12
                  assignedIncrement <- getAsWord16 10
                  return $ Assignment { .. }

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
getMMSI = MMSI <$> getAsWord32 30

getSyncState :: BitGet SyncState
getSyncState = toEnum . fromIntegral <$> getAsWord8 2

getAidToNavigation :: BitGet AidToNavigation
getAidToNavigation = toEnum . fromIntegral <$> getAsWord8 5

getSpecialManeuverIndicator :: BitGet SpecialManeuverIndicator
getSpecialManeuverIndicator = toEnum . fromIntegral <$> getAsWord8 2

getApplicationIdentifier :: BitGet ApplicationIdentifier
getApplicationIdentifier = do
                             designatedAreaCode <- getAsWord16 10
                             functionIdentifier <- getAsWord8 6
                             return $ ApplicationIdentifier { .. }

getAcknowledgement :: BitGet Acknowledgement
getAcknowledgement = do
                       destinationID <- getMMSI
                       acknowledgedSequenceNumber <- getAsWord8 2
                       return $ Acknowledgement { .. }

getVesselDimensions :: BitGet VesselDimensions
getVesselDimensions = do
                        forwardOfReferencePoint   <- coerce <$> getAsWord16 9
                        aftOfReferencePoint       <- coerce <$> getAsWord16 9
                        portOfReferencePoint      <- coerce <$> getAsWord8 6
                        starboardOfReferencePoint <- coerce <$> getAsWord8 6
                        return $ VesselDimensions { .. }

getTransitionalZoneSize :: BitGet (LengthNauticalMiles Word8)
getTransitionalZoneSize = coerce . (+ 1) <$> getAsWord8 3

getRateOfTurn :: BitGet PackedRateOfTurn
getRateOfTurn = do
                  n <- getAsWord8 8
                  return $ case n of
                             -128 -> RateNotAvailable
                             -127 -> RatePortNoSensor
                             127 -> RateStarboardNoSensor
                             r -> RateSpecified r

getCourse :: BitGet (Maybe Course)
getCourse = do
              n <- getAsWord16 12
              return $ if n < 3600
                         then Just $ coerce n
                         else Nothing

getLowResolutionCourse :: BitGet (Maybe Course)
getLowResolutionCourse = do
                           n <- getAsWord16 9
                           return $ if n < 360
                                      then Just $ coerce (n * 10)
                                      else Nothing

getHeading :: BitGet (Maybe Heading)
getHeading = do
               n <- getAsWord16 9
               return $ if n < 360
                          then Just $ coerce n
                          else Nothing

getAltitude :: BitGet Altitude
getAltitude = do
                n <- getAsWord16 12
                return $ case n of
                           4095 -> AltNotAvailable
                           4094 -> AltHigh
                           _ -> AltSpecified $ coerce n

getAltitudeSensor :: BitGet AltitudeSensor
getAltitudeSensor = fmap (\x -> if x then AltBarometric else AltGnss) getBit

getPositionFixingDevice :: BitGet PositionFixingDevice
getPositionFixingDevice = f . fromIntegral <$> getAsWord8 4
  where
    f 15 = PosFixInternalGnss
    f n | 9 <= n && n <= 14 = PosFixReserved
        | otherwise         = toEnum n

toStationType :: Word8 -> StationType
toStationType n | n <= 9    = toEnum . fromIntegral $ n
                | otherwise = StationsReserved

getTransmissionMode :: Int -> BitGet TransmissionMode
getTransmissionMode n = f . fromIntegral <$> getAsWord8 n
  where
    f :: Int -> TransmissionMode
    f 0 = TransmitAB
    f 1 = TransmitA
    f 2 = TransmitB
    f _ = TransmitReserved

getAssignedReportingInterval :: BitGet AssignedReportingInterval
getAssignedReportingInterval = f . fromIntegral <$> getAsWord8 4
  where
    f n = if n >= 12
            then IntervalReserved
            else toEnum n

getChannel :: BitGet Channel
getChannel = ItuRM1084Channel <$> getAsWord16 12 -- TODO: recognize A and B by their numbers?

getTargetDesignation :: BitGet TargetDesignation
getTargetDesignation = do
                         isAddresed <- lookAhead $ skip 70 *> getBit
                         result <- if isAddresed
                                     then do
                                            a1 <- getMMSI
                                            skip 5
                                            a2 <- getMMSI
                                            skip 5
                                            return $ AddressedTargetDesignation [a1, a2]
                                     else GeographicTargetDesignation <$> getRegion
                         skip 1 -- skip the addressed/regional designation bit that we looked ahead for
                         return result

getRegion :: BitGet Region
getRegion = do
              Just east <- getLowResolutionLongitude
              Just north <- getLowResolutionLatitude
              Just west <- getLowResolutionLongitude
              Just south <- getLowResolutionLatitude
              return $ Region { .. }

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

getTaggedCommunicationsState :: BitGet CommunicationsState
getTaggedCommunicationsState = do
                                 isItdma <- getBit
                                 if isItdma
                                   then getITDMACommunicationsState
                                   else getSOTDMACommunicationsState

getClassBCapabilities :: BitGet ClassBCapabilities
getClassBCapabilities = do
                          carrierSenseUnit <- getBit
                          equippedWithDisplay <- getBit
                          equippedWithDigitalSelectiveCalling <- getBit
                          capableOfOperatingOverEntireMarineBand <- getBit
                          supportsChannelManagement <- getBit
                          return $ ClassBCapabilities { .. }

getMessage :: BitGet AisMessage
getMessage = do
               messageType <- getMessageType
               case messageType of
                 MNone -> return InvalidMessage
        {-  1 -} MScheduledClassAPositionReport -> getClassAPositionReport MScheduledClassAPositionReport getSOTDMACommunicationsState
        {-  2 -} MAssignedScheduledClassAPositionReport -> getClassAPositionReport MAssignedScheduledClassAPositionReport getSOTDMACommunicationsState
        {-  3 -} MSpecialClassAPositionReport -> getClassAPositionReport MSpecialClassAPositionReport getITDMACommunicationsState
        {-  4 -} MBaseStationReport -> getBaseStationReportMessage
        {-  5 -} MStaticAndVoyageData -> getClassAStaticData
        {-  6 -} MBinaryAddressedMessage -> getAddressedBinaryMessage
        {-  7 -} MBinaryAcknowledgement -> getAcknowledgementMessage MBinaryAcknowledgement
        {-  8 -} MBinaryBroadcastMessage -> getBinaryBroadcastMessage
        {-  9 -} MStandardSarAircraftPositionReport -> getSarAircraftPositionReport
        {- 10 -} MTimeInquiry -> getTimeInquiry
        {- 11 -} MTimeResponse -> getTimeResponse
        {- 12 -} MAddressedSafetyRelatedMessage -> getAddressedSafetyRelatedMessage
        {- 13 -} MSafetyRelatedAcknowledgement -> getAcknowledgementMessage MSafetyRelatedAcknowledgement
        {- 14 -} MSafetyRelatedBroadcastMessage -> getSafetyRelatedBroadcastMessage
        {- 15 -} MInterrogation -> getInterrogationMessage
        {- 16 -} MAssignmentModeCommand -> getAssignmentModeCommand
        {- 17 -} MDgnssBroadcastBinaryMessage -> getDgnssBroadcastMessage
        {- 18 -} MStandardClassBPositionReport -> getStandardClassBPositionReport
        {- 19 -} MExtendedClassBPositionReport -> getExtendedClassBPositionReport
        {- 20 -} MDataLinkManagementMessage -> getDataLinkManagementMessage
        {- 21 -} MAidToNavigationReport -> getAidToNavigationReport
        {- 22 -} MChannelManagement -> getChannelManagementCommand
        {- 23 -} MGroupAssignmentCommand -> getGroupAssignmentCommand
        {- 24 -} MStaticDataReport -> getStaticDataReport
        {- 25 -} MSingleSlotBinaryMessage -> getSingleSlotBinaryMessage
        {- 26 -} MMultipleSlotBinaryMessage -> getMultipleSlotBinaryMessage
        {- 27 -} MLongRangePositionReport -> getLongRangePositionReport

getClassAPositionReport :: MessageID -> BitGet CommunicationsState -> BitGet AisMessage
getClassAPositionReport messageType getCommState = do
                            repeatIndicator <- getRepeatIndicator
                            userID <- getMMSI
                            navigationalStatus <- getNavigationalStatus
                            rateOfTurn <- getRateOfTurn
                            speedOverGround <- getSpeed 10
                            positionAccuracy <- getBit
                            longitude <- getLongitude
                            latitude <- getLatitude
                            courseOverGround <- getCourse
                            trueHeading <- getHeading
                            (timeStamp, positionFixingStatus) <- getTimeStamp
                            maneuverIndicator <- getSpecialManeuverIndicator
                            skip 3
                            raimFlag <- getBit
                            communicationsState <- getCommState
                            return $ ClassAPositionReport { .. }

getSarAircraftPositionReport :: BitGet AisMessage
getSarAircraftPositionReport = do
                                 let messageType = MStandardSarAircraftPositionReport
                                 repeatIndicator <- getRepeatIndicator
                                 userID <- getMMSI
                                 altitude <- getAltitude
                                 speedOverGround' <- getSpeed 10
                                 positionAccuracy <- getBit
                                 longitude <- getLongitude
                                 latitude <- getLatitude
                                 courseOverGround <- getCourse
                                 (timeStamp, positionFixingStatus) <- getTimeStamp
                                 altitudeSensor <- getAltitudeSensor
                                 skip 7
                                 dteNotReady <- getBit
                                 skip 3
                                 assignedModeFlag <- getBit
                                 raimFlag <- getBit
                                 communicationsState <- getTaggedCommunicationsState
                                 return $ SarAircraftPositionReport { .. }

getAddressedSafetyRelatedMessage :: BitGet AisMessage
getAddressedSafetyRelatedMessage = do
                                     let messageType = MAddressedSafetyRelatedMessage
                                     repeatIndicator <- getRepeatIndicator
                                     sourceID <- getMMSI
                                     sequenceNumber <- Just <$> getAsWord8 2
                                     addressee <- Addressed <$> getMMSI
                                     retransmitFlag <- getBit
                                     skip 1
                                     safetyRelatedText <- getRemainingSixBitText
                                     return $ SafetyRelatedMessage { .. }

getSafetyRelatedBroadcastMessage :: BitGet AisMessage
getSafetyRelatedBroadcastMessage = do
                                     let messageType = MSafetyRelatedBroadcastMessage
                                     repeatIndicator <- getRepeatIndicator
                                     sourceID <- getMMSI
                                     skip 2
                                     safetyRelatedText <- getRemainingSixBitText
                                     let addressee = Broadcast
                                     let retransmitFlag = False
                                     let sequenceNumber = Nothing
                                     return $ SafetyRelatedMessage { .. }

getAddressedBinaryMessage :: BitGet AisMessage
getAddressedBinaryMessage = do
                              let messageType = MBinaryAddressedMessage
                              repeatIndicator <- getRepeatIndicator
                              sourceID <- getMMSI
                              sequenceNumber <- Just <$> getAsWord8 2
                              addressee <- Addressed <$> getMMSI
                              retransmitFlag <- getBit
                              skip 1
                              applicationIdentifier <- Just <$> getApplicationIdentifier
                              n <- remaining
                              payload <- getLeftByteString n
                              let optionalCommunicationsState = Nothing
                              if (n `mod` 8 == 0)
                                then return $ BinaryMessage { .. }
                                else error "Length of payload was not an even number of bytes."

getBinaryBroadcastMessage :: BitGet AisMessage
getBinaryBroadcastMessage = do
                              let messageType = MBinaryBroadcastMessage
                              repeatIndicator <- getRepeatIndicator
                              sourceID <- getMMSI
                              skip 2
                              applicationIdentifier <- Just <$> getApplicationIdentifier
                              n <- remaining
                              payload <- getLeftByteString n
                              let addressee = Broadcast
                              let retransmitFlag = False
                              let sequenceNumber = Nothing
                              let optionalCommunicationsState = Nothing
                              if (n `mod` 8 == 0)
                                then return $ BinaryMessage { .. }
                                else error "Length of payload was not an even number of bytes."

getBaseStationReportMessage :: BitGet AisMessage
getBaseStationReportMessage = do
                                let messageType = MBaseStationReport
                                repeatIndicator <- getRepeatIndicator
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

getDataLinkManagementMessage :: BitGet AisMessage
getDataLinkManagementMessage = do
                                 let messageType = MDataLinkManagementMessage
                                 repeatIndicator <- getRepeatIndicator
                                 sourceID <- getMMSI
                                 skip 2
                                 r <- getReservation
                                 (n, s) <- (`divMod` 30) <$> remaining
                                 rs <- replicateM n getReservation
                                 skip s
                                 let reservations = Prelude.filter isValidReservation (r:rs)
                                 return $ DataLinkManagementMessage { .. }

getAssignmentModeCommand :: BitGet AisMessage
getAssignmentModeCommand = do
                             let messageType = MAssignmentModeCommand
                             repeatIndicator <- getRepeatIndicator
                             sourceID <- getMMSI
                             skip 2
                             assignmentA <- getAssignment
                             n <- remaining
                             assignments <- case n of
                                              4 -> do
                                                     skip 4
                                                     return $ [assignmentA]
                                              56 -> do
                                                      assignmentB <- getAssignment
                                                      return $ [assignmentA, assignmentB]
                                              _ -> do return $ [assignmentA] -- TODO: this isn't right, once we have a better monad this should be an error
                             return $ AssignmentModeCommand { .. }

getInterrogationMessage :: BitGet AisMessage
getInterrogationMessage = do
                            let messageType = MInterrogation
                            repeatIndicator <- getRepeatIndicator
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
                   repeatIndicator <- getRepeatIndicator
                   sourceID <- getMMSI
                   skip 2
                   destinationID <- getMMSI
                   skip 2
                   return $ TimeInquiry { .. }

getTimeResponse :: BitGet AisMessage
getTimeResponse = do
                    let messageType = MTimeResponse
                    repeatIndicator <- getRepeatIndicator
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
                        repeatIndicator <- getRepeatIndicator
                        userID <- getMMSI
                        aisVersionIndicator <- getAsWord8 2
                        imoNumber <- getAsWord32 30
                        callSign <- getSixBitText 7
                        name <- getSixBitText 20
                        typeOfShipAndCargo <- getAsWord8 8
                        vesselDimensions <- getVesselDimensions
                        positionFixingDevice <- getPositionFixingDevice
                        eta <- getAsWord32 20
                        draught <- fmap coerce $ getAsWord8 8
                        destination <- getSixBitText 20
                        dteNotReady <- getBit
                        skip 1
                        return $ ClassAStaticData { .. }

getStaticDataReport :: BitGet AisMessage
getStaticDataReport = do
                        let messageType = MStaticDataReport
                        repeatIndicator <- getRepeatIndicator
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
                          _ -> return InvalidMessage

getAcknowledgementMessage :: MessageID -> BitGet AisMessage
getAcknowledgementMessage messageType = do
                                          repeatIndicator <- getRepeatIndicator
                                          sourceID <- getMMSI
                                          skip 2
                                          n <- remaining
                                          acknowledgements <- replicateM (n `div` 32) getAcknowledgement
                                          return $ AcknowledgementMessage { .. }

getDgnssBroadcastMessage :: BitGet AisMessage
getDgnssBroadcastMessage = do
                             let messageType = MDgnssBroadcastBinaryMessage
                             repeatIndicator <- getRepeatIndicator
                             sourceID <- getMMSI
                             skip 2
                             longitude <- getLowResolutionLongitude
                             latitude <- getLowResolutionLatitude
                             skip 5
                             n <- remaining
                             payload <- getLeftByteString n
                             return $ DgnssBroadcastMessage { .. }

getChannelManagementCommand :: BitGet AisMessage
getChannelManagementCommand = do
                                let messageType = MChannelManagement
                                repeatIndicator <- getRepeatIndicator
                                userID <- getMMSI
                                skip 2
                                channelA <- getChannel
                                channelB <- getChannel
                                transmissionMode <- getTransmissionMode 4
                                useLowPower <- getBit
                                targetDesignation <- getTargetDesignation
                                skip 2
                                transitionalZoneSize <- getTransitionalZoneSize
                                skip 23
                                return $ ChannelManagementCommand { .. }

-- The result may instead be a BaseStationCoverageAreaMessage under certain conditions.
getGroupAssignmentCommand :: BitGet AisMessage
getGroupAssignmentCommand = do
                              let messageType = MGroupAssignmentCommand
                              repeatIndicator <- getRepeatIndicator
                              userID <- getMMSI
                              skip 2
                              region <- getRegion
                              rawStationType <- getAsWord8 4
                              typeOfShipAndCargo <- getAsWord8 8
                              skip 22
                              transmissionMode <- getTransmissionMode 2
                              reportingInterval <- getAssignedReportingInterval
                              quietTime <- fmap coerce $ getAsWord8 4
                              skip 6
                              return $ if rawStationType == 10
                                         then BaseStationCoverageAreaMessage { .. }
                                         else let stationType = toStationType rawStationType
                                               in GroupAssignmentCommand { .. }

getAidToNavigationReport :: BitGet AisMessage
getAidToNavigationReport = do
                             let messageType = MAidToNavigationReport
                             repeatIndicator <- getRepeatIndicator
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

getStandardClassBPositionReport :: BitGet AisMessage
getStandardClassBPositionReport = do
                                    let messageType = MStandardClassBPositionReport
                                    repeatIndicator <- getRepeatIndicator
                                    userID <- getMMSI
                                    skip 8
                                    speedOverGround <- getSpeed 10
                                    positionAccuracy <- getBit
                                    longitude <- getLongitude
                                    latitude <- getLatitude
                                    courseOverGround <- getCourse
                                    trueHeading <- getHeading
                                    (timeStamp, positionFixingStatus) <- getTimeStamp
                                    skip 2
                                    capabilities <- getClassBCapabilities
                                    assignedModeFlag <- getBit
                                    raimFlag <- getBit
                                    communicationsState <- getTaggedCommunicationsState
                                    return $ StandardClassBPositionReport { .. }

getExtendedClassBPositionReport :: BitGet AisMessage
getExtendedClassBPositionReport = do
                                    let messageType = MExtendedClassBPositionReport
                                    repeatIndicator <- getRepeatIndicator
                                    userID <- getMMSI
                                    skip 8
                                    speedOverGround <- getSpeed 10
                                    positionAccuracy <- getBit
                                    longitude <- getLongitude
                                    latitude <- getLatitude
                                    courseOverGround <- getCourse
                                    trueHeading <- getHeading
                                    (timeStamp, positionFixingStatus) <- getTimeStamp
                                    skip 4
                                    name <- getSixBitText 20
                                    typeOfShipAndCargo <- getAsWord8 8
                                    vesselDimensions <- getVesselDimensions
                                    positionFixingDevice <- getPositionFixingDevice
                                    raimFlag <- getBit
                                    dteNotReady <- getBit
                                    assignedModeFlag <- getBit
                                    skip 4
                                    return $ ExtendedClassBPositionReport { .. }

getSingleSlotBinaryMessage :: BitGet AisMessage
getSingleSlotBinaryMessage = do
                               let messageType = MSingleSlotBinaryMessage
                               repeatIndicator <- getRepeatIndicator
                               sourceID <- getMMSI
                               isAddressed <- getBit
                               hasApplicationID <- getBit
                               addressee <- if isAddressed
                                              then Addressed <$> getMMSI <* skip 2
                                              else return Broadcast
                               applicationIdentifier <- if hasApplicationID
                                                          then Just <$> getApplicationIdentifier
                                                          else return Nothing
                               n <- remaining
                               payload <- getLeftByteString n
                               let retransmitFlag = False
                               let sequenceNumber = Nothing
                               let optionalCommunicationsState = Nothing
                               return $ BinaryMessage { .. }

getMultipleSlotBinaryMessage :: BitGet AisMessage
getMultipleSlotBinaryMessage = do
                                 let messageType = MMultipleSlotBinaryMessage
                                 repeatIndicator <- getRepeatIndicator
                                 sourceID <- getMMSI
                                 isAddressed <- getBit
                                 hasApplicationID <- getBit
                                 addressee <- if isAddressed
                                                then Addressed <$> getMMSI <* skip 2
                                                else return Broadcast
                                 applicationIdentifier <- if hasApplicationID
                                                          then Just <$> getApplicationIdentifier
                                                          else return Nothing
                                 n <- remaining
                                 payload <- getLeftByteString (n - 24)
                                 skip 4
                                 communicationsState <- getTaggedCommunicationsState
                                 let retransmitFlag = False
                                 let sequenceNumber = Nothing
                                 let optionalCommunicationsState = Just communicationsState
                                 return $ BinaryMessage { .. }

getLongRangePositionReport :: BitGet AisMessage
getLongRangePositionReport = do
                               let messageType = MLongRangePositionReport
                               repeatIndicator <- getRepeatIndicator
                               userID <- getMMSI
                               positionAccuracy <- getBit
                               raimFlag <- getBit
                               navigationalStatus <- getNavigationalStatus
                               longitude <- getLowResolutionLongitude
                               latitude <- getLowResolutionLatitude
                               speedOverGround' <- getSpeed 6
                               courseOverGround <- getLowResolutionCourse
                               positionLatency <- getBit
                               skip 1
                               return $ LongRangePositionReport { .. }

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
