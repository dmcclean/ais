{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.AIS.Vocabulary
where

import qualified Data.ExactPi.TypeLevel as E
import Data.Char (digitToInt)
import Data.Int
import Data.Word
import qualified Numeric.IEEE
import Numeric.Units.Dimensional.FixedPoint
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.Float
import qualified Numeric.Units.Dimensional as D
import Text.Printf
import Prelude hiding ((+), (/), sqrt, (**))
import qualified Prelude as P

-- | A <https://en.wikipedia.org/wiki/Maritime_Mobile_Service_Identity Maritime Mobile Service Identity>.
newtype MMSI = MMSI Word32
  deriving (Eq, Ord)

instance Show MMSI where
  showsPrec d (MMSI n) = showParen (d > app_prec) showStr
    where
      showStr = showString $ printf "MMSI %0.9d" n
      app_prec = 10 -- precedence of application

-- | Maritime Identification Digits identifying a country or international
-- authority.
newtype MID = MID Word16
  deriving (Eq, Ord, Show)

toMid :: (Ord a, Integral a) => a -> Maybe MID
toMid n = if 200 <= n && n <= 799
            then Just . MID $ fromIntegral n
            else Nothing

-- | The type of an 'MMSI'.
--
-- MMSIs with certain leading digits are reserved for certain
-- classes of stations. For some of these classes, the MMSI also
-- includes 'MID' identifying the authority that issues the MMSI.
data MMSIType = MmsiInvalid
              | MmsiShipStation MID
              | MmsiGroupShipStation MID
              | MmsiCoastStation MID
              | MmsiSarAircraft MID
              | MmsiHandheldStation
              | MmsiEmergencyDevice EmergencyDeviceType
              | MmsiCraftAssociatedWithParentShip MID
              | MmsiNavigationalAid MID
  deriving (Eq, Ord, Show)

data EmergencyDeviceType = SarTransponder
                         | MobDevice
                         | Epirb
  deriving (Eq, Ord, Show, Read)

-- | Determines the type of an 'MMSI' using the structure documented in ITU-R M.585-7.
mmsiType :: MMSI -> MMSIType
mmsiType (MMSI n) | n <= 999999999 = case digits n of
                                       (0:0:ds)   | Just m <- takeMid ds -> MmsiCoastStation m
                                       (0:ds)     | Just m <- takeMid ds -> MmsiGroupShipStation m
                                       (1:1:1:ds) | Just m <- takeMid ds -> MmsiSarAircraft m
                                       (8:_)                             -> MmsiHandheldStation
                                       (9:7:0:_)                         -> MmsiEmergencyDevice SarTransponder
                                       (9:7:2:_)                         -> MmsiEmergencyDevice MobDevice
                                       (9:7:4:_)                         -> MmsiEmergencyDevice Epirb
                                       (9:8:ds)   | Just m <- takeMid ds -> MmsiCraftAssociatedWithParentShip m
                                       (9:9:ds)   | Just m <- takeMid ds -> MmsiNavigationalAid m
                                       ds         | Just m <- takeMid ds -> MmsiShipStation m
                                       _                                 -> MmsiInvalid
                  | otherwise = MmsiInvalid
  where
    takeMid :: [Int] -> Maybe MID
    takeMid (a:b:c:_) = toMid (100 P.* a P.+ 10 P.* b P.+ c)
    takeMid _ = Nothing

digits :: Word32 -> [Int]
digits n = digitToInt <$> printf "%0.9d" n

newtype IMONumber = IMO Word32
  deriving (Eq, Ord, Show)

isValidImoNumber :: IMONumber -> Bool
isValidImoNumber (IMO n) | n < 1000000 || 9999999 < n = False
                         | otherwise = n `mod` 10 == (fromIntegral . P.sum $ zipWith (P.*) (digits n) [7,6,5,4,3,2,0])

type RepeatIndicator = Word8

-- | A radio channel across which AIS messages may be conveyed.
data Channel = AisChannelA -- ^ AIS channel A, also known as VHF channel 87B.
             | AisChannelB -- ^ AIS channel B, also known as VHF channel 88B.
             | ItuRM1084Channel Word16 -- ^ A radio channel number of 25 kHz simplex, or simplex use of 25 kHz duplex,
                                       -- in accordance with <https://www.itu.int/rec/R-REC-M.1084/en Recommendation ITU-R M.1084>.
  deriving (Eq, Show, Read)

-- | Gets the radio frequency associated with a radio channel across which AIS messages may be conveyed.
channelFrequency :: Channel -> Maybe (Frequency Rational)
channelFrequency AisChannelA = Just $ 161.975 D.*~ mega hertz
channelFrequency AisChannelB = Just $ 162.025 D.*~ mega hertz
channelFrequency (ItuRM1084Channel n) |    6 == n              = Just $ 156.3 D.*~ mega hertz
                                      |    8 <= n && n <=   17 = fromBase 156.4   $ n P.- 8
                                      |   67 <= n && n <=   77 = fromBase 156.375 $ n P.- 67
                                      | 1006 == n              = Nothing
                                      | 1008 <= n && n <= 1017 = Nothing
                                      | 1001 <= n && n <= 1028 = fromBase 156.050 $ n P.- 1001
                                      | 1067 <= n && n <= 1077 = Nothing
                                      | 1060 <= n && n <= 1088 = fromBase 156.025 $ n P.- 1060
                                      | 2006 == n              = Nothing
                                      | 2008 <= n && n <= 2017 = Nothing
                                      | 2001 <= n && n <= 2028 = fromBase 160.650 $ n P.- 2001
                                      | 2067 <= n && n <= 2077 = Nothing
                                      | 2060 <= n && n <= 2088 = fromBase 160.625 $ n P.- 2060
                                      | otherwise              = Nothing
  where
    fromBase b o = Just $ (b D.*~ mega hertz) D.+ (50 D.*~ kilo hertz) D.* (realToFrac o D.*~ one)

data MessageID = MNone
               | MScheduledClassAPositionReport
               | MAssignedScheduledClassAPositionReport
               | MSpecialClassAPositionReport
               | MBaseStationReport
               | MStaticAndVoyageData
               | MBinaryAddressedMessage
               | MBinaryAcknowledgement
               | MBinaryBroadcastMessage
               | MStandardSarAircraftPositionReport
               | MTimeInquiry
               | MTimeResponse
               | MAddressedSafetyRelatedMessage
               | MSafetyRelatedAcknowledgement
               | MSafetyRelatedBroadcastMessage
               | MInterrogation
               | MAssignmentModeCommand
               | MDgnssBroadcastBinaryMessage
               | MStandardClassBPositionReport
               | MExtendedClassBPositionReport
               | MDataLinkManagementMessage
               | MAidToNavigationReport
               | MChannelManagement
               | MGroupAssignmentCommand
               | MStaticDataReport
               | MSingleSlotBinaryMessage
               | MMultipleSlotBinaryMessage
               | MLongRangePositionReport
  deriving (Eq, Enum, Show, Read)

-- | The synchronization state of an AIS station.
data SyncState = SyncUtcDirect -- ^ The station is synchronized directly to UTC. See Section 3.1.1.1.
               | SyncUtcIndirect -- ^ The station is synchronized indirectly to UTC, through . See section 3.1.1.2.
               | SyncBaseStation -- ^ The station is synchronized to a base station. See section 3.1.1.3.
               | SyncPeer -- ^ The station is synchronized to another station based on the highest number of received stations, or to another mobile station which is directly synchronized to a base station. See Sections 3.1.1.3 and 3.1.1.4.
  deriving (Eq, Enum, Show, Read)

-- | The navigational status of a vessel
data NavigationalStatus = NavUnderWayEngine -- ^ The vessel is under way using an engine.
                        | NavAtAnchor -- ^ The vessel is at anchor.
                        | NavNotUnderCommand -- ^ The vessel is not under command.
                        | NavRestrictedManeuverability -- ^ The vessel is operating with restricted maneuverability.
                        | NavConstrainedByDraught -- ^ The vessel's maneuverability is constrained by its draught.
                        | NavMoored -- ^ The vessel is moored.
                        | NavAground -- ^ The vessel is aground.
                        | NavEngagedInFishing -- ^ The vessel is engaged in fishing.
                        | NavUnderWaySailing -- ^ The vessel is under way sailing.
                        | NavReservedIMOHazardC -- ^ This navigation status is reserved for future amendment pertaining to ships carrying dangerous goods, harmful substances, marine pollutants, or IMO hazard or pollutant category C.
                        | NavReservedIMOHazardA -- ^ This navigation status is reserved for future amendment pertaining to ships carrying dangerous goods, harmful substances, marine pollutants, or IMO hazard or pollutant category A.
                        | NavPowerDrivenVesselTowingAstern -- ^ The vessel is a power-driven vessel towing astern.
                        | NavPowerDrivenVesselPushingAheadOrTowingAlongside -- ^ The vessel is a power-driven vessel pushing ahead or towing alongside, or is in another navigational status defined by regional use.
                        | NavReserved
                        | NavAisSartMobEpirb -- ^ The reporting device is an active AIS SART, MOB device, or EPIRB.
                        | NavUndefined -- ^ The navigational status of the vessel is unspecified.
  deriving (Eq, Enum, Show, Read)

-- | The state of a TDMA communications link.
data CommunicationsState = SOTDMA { syncState :: SyncState
                                  , slotTimeout :: Word8
                                  , submessage :: SOTDMASubmessage } -- ^ The state of an SOTDMA communications link, as specified in Section 3.3.7.2.2.
                         | ITDMA { syncState :: SyncState
                                 , slotIncrement :: Word16
                                 , numberOfSlots :: Word8
                                 , keep :: Bool }  -- ^ The state of an ITDMA communications link, as specified in Section 3.3.7.3.2.
  deriving (Eq, Show, Read)

-- | An SOTDMA communications state submessage, as described in Section 3.3.7.2.3.
data SOTDMASubmessage = ReceivedStations Word16 -- ^ The number of other stations (not including the own station) which the station is currently receiving.
                      | SlotNumber Word16 -- ^ The slot number used for this transmission.
                      | UTCHourAndMinute Word8 Word8 -- ^ If the transmitting station has access to UTC, the hour and minute of the current UTC time.
                      | SlotOffset Word16 -- ^ The slot offset to the slot in which transmission from the transmitting station will occur in the next frame. If zero, the slot should be deallocated.
  deriving (Eq, Show, Read)

-- | A device for determining the position of an object.
data PositionFixingDevice = PosFixUndefined -- ^ Position fix derived by unspecified means.
                          | PosFixGps -- ^ Positiion fix derived from the Global Positioning System or GPS.
                          | PosFixGlonass -- ^ Position fix derived from the Globalnaya Navigazionnaya Sputnikovaya Sistema or GLONASS.
                          | PosFixGpsGlonass -- ^ Position fix derived by combining both GPS and GLONASS.
                          | PosFixLoranC -- ^ Position fix derived from the LORAN-C radio navigation system.
                          | PosFixChayka -- ^ Position fix derived from the Chayka radio navigation system.
                          | PosFixIntegratedNavigationSystem -- ^ Position fix derived from an integrated navigation system.
                          | PosFixSurveyed -- ^ Position fix derived by surveying.
                          | PosFixGalileo -- ^ Position fix derived from the Galileo satellite navigation system.
                          | PosFixInternalGnss -- ^ Position fix derived from an internal GNSS receiver.
                          | PosFixReserved
  deriving (Eq, Enum, Show, Read)

-- | The status of a position fixing device.
data PositionFixingStatus = PosStatusNormal -- ^ The position fixing device is operating normally.
                          | PosStatusManual -- ^ The position fixing device is operating in manual input mode.
                          | PosStatusEstimated -- ^ The position fixing device is operating in estimated or dead reckoning mode.
                          | PosStatusInoperative -- ^ The position fixing device is inoperative.
  deriving (Eq, Enum, Show, Read)

-- | The nature of an aid to navigation.
data AidToNavigation = AidUnspecified -- ^ The type of aid to navigation is unspecified.
                     | AidReferencePoint -- ^ A reference point for use in navigation or communication.
                     | AidRacon -- ^ A radar beacon.
                     | AidFixedOffshoreStructure -- ^ A fixed structure off-shore, such as an oil platform or wind farm.
                     | AidEmergencyWreckMarkingBuoy -- ^ An emergency wreck marking buoy, generally used to mark the location of a recent and therefore uncharted wreck.
                     | AidLightWithoutSectors
                     | AidLightWithSectors
                     | AidLeadingLightFront
                     | AidLeadingLightRear
                     | AidBeaconCardinalN
                     | AidBeaconCardinalE
                     | AidBeaconCardinalS
                     | AidBeaconCardinalW
                     | AidBeaconPortHand
                     | AidBeaconStarboardHand
                     | AidBeaconPreferredChannelPortHand
                     | AidBeaconPreferredChannelStarboardHand
                     | AidBeaconIsolatedDanger
                     | AidBeaconSafeWater
                     | AidBeaconSpecialMark
                     | AidCardinalMarkN
                     | AidCardinalMarkE
                     | AidCardinalMarkS
                     | AidCardinalMarkW
                     | AidPortHandMark
                     | AidStarboardHandMark
                     | AidPreferredChannelPortHand
                     | AidPreferredChannelStarboardHand
                     | AidIsolatedDanger
                     | AidSafeWater
                     | AidSpecialMark
                     | AidLightVesselLanbyRig -- ^ A light vessel, Large Automated Navigation Buoy, or rig.
  deriving (Eq, Enum, Show, Read)

-- | 'True' if an 'AidToNavigation' type denotes a fixed aid to navigation.
isFixedAtoN :: AidToNavigation -> Bool
isFixedAtoN aid = 5 <= x && x <= 19
  where
    x = fromEnum aid

-- | 'True' if an 'AidToNavigation' type denotes a floating aid to navigation.
isFloatingAtoN :: AidToNavigation -> Bool
isFloatingAtoN aid = 20 <= x
  where
    x = fromEnum aid

data Altitude = AltNotAvailable
              | AltHigh
              | AltSpecified (Length Word16)
  deriving (Eq, Show)

-- | A means of sensing altitude.
data AltitudeSensor = AltGnss -- ^ Altitude measurement derived from GNSS position fix.
                    | AltBarometric -- ^ Altitude measurement derived from barometric altimetry.
  deriving (Eq, Enum, Show, Read)

-- | An identifier denoting the application format of a binary AIS message.
data ApplicationIdentifier = ApplicationIdentifier { designatedAreaCode :: Word16 -- ^ A code identifying the authority that codified this application.
                                                   , functionIdentifier :: Word8 -- ^ An identifier assigned by the authority contolling the 'designatedAreaCode'.
                                                   }
  deriving (Eq, Show)

-- | An acknowledgement of receipt of a previous AIS message.
data Acknowledgement = Acknowledgement { destinationID :: MMSI -- ^ The 'MMSI' of the station to whom the acknowledgement is addressed.
                                       , acknowledgedSequenceNumber :: Word8 -- ^ The sequence number of the message whose receipt is acknowledged.
                                       }
  deriving (Eq, Show)

-- | A request for an AIS station to transmit a specified AIS message during a specified slot.
data Interrogation = Interrogation { interrogatedID :: MMSI -- ^ The 'MMSI' of the interrograted station.
                                   , requestedMessageType :: MessageID -- ^ The type of message requested.
                                   , requestedSlotOffset :: Word16 -- ^ The offset from the current slot at which a reply is requested.
                                   }
  deriving (Eq, Show)

-- | The station or stations to whom a message is addressed.
data Addressee = Broadcast -- ^ The message is addressed to all stations.
               | Addressed MMSI -- ^ The message is addressed to the specified station or group of ship stations.
  deriving (Eq, Show)

data Reservation = Reservation { reservedOffsetNumber :: Word16
                               , reservedNumberOfSlots :: Word8
                               , reservationTimeout :: TimeMinutes Word8
                               , reservationIncrement :: Word16
                               }
  deriving (Eq, Show)

isValidReservation :: Reservation -> Bool
isValidReservation r = reservationTimeout r > _0 && reservedNumberOfSlots r > 0

-- | A classification of AIS stations.
data StationType = StationsAllMobile -- ^ All mobile stations.
                 | StationsClassAMobile -- ^ All mobile Class A stations.
                 | StationsClassBMobile -- ^ All mobile Class B stations.
                 | StationsSarAirborneMobile -- ^ All mobile airborne search and rescue stations.
                 | StationsClassBSelfOrganizingMobile -- ^ All mobile Class B stations with SOTDMA operating capability.
                 | StationsClassBCarrierSenseMobile -- ^ All mobile Class B stations limited to CSTDMA operating capability.
                 | StationsInlandWaterways -- ^ All AIS stations operating on inland waterways.
                 | StationsRegionalUseA
                 | StationsRegionalUseB
                 | StationsRegionalUseC
                 | StationsReserved
  deriving (Eq, Enum, Read, Show)

data SpecialManeuverIndicator = SpecialManeuverNotAvailable
                              | NotEngagedInSpecialManeuver
                              | EngagedInSpecialManeuver
                              | SpecialManeuverReserved
  deriving (Eq, Enum, Read, Show)

-- | A transmission mode controlling channel use by an AIS station.
data TransmissionMode = TransmitAB -- ^ Station transmits on both channel A and channel B.
                      | TransmitA -- ^ Station transmits on channel A only.
                      | TransmitB -- ^ Station transmits on channel B only.
                      | TransmitReserved
  deriving (Eq, Enum, Read, Show)

data Assignment = Assignment { targetID :: MMSI
                             , assignedSlotOffset :: Word16
                             , assignedIncrement :: Word16
                             }
  deriving (Eq, Show)

data TargetDesignation = GeographicTargetDesignation Region
                       | AddressedTargetDesignation [MMSI]
  deriving (Eq, Show)

-- | A quadrangle in an ellipsoidal coordinate system.
data Region = Region { east :: Longitude -- ^ The longitude of the Eastern boundary.
                     , north :: Latitude -- ^ The latitude of the Northern boundary.
                     , west :: Longitude -- ^ The longitude of the Western boundary.
                     , south :: Latitude -- ^ The latitude of the Southern boundary.
                     }
  deriving (Eq, Show)

-- | A reporting interval at which an AIS station is directed to report its position.
data AssignedReportingInterval = IntervalAsAutonomous -- ^ The station should report at the autonomously dervied reporting rate appropriate for its operating condition. See section 4.2.1.
                               | IntervalTenMinutes -- ^ The station should report at 10 minute intervals.
                               | Interval6Minutes -- ^ The station should report at 6 minute intervals.
                               | Interval3Minutes -- ^ The station should report at 3 minute intervals.
                               | Interval1Minute -- ^ The station should report at 1 minute intervals.
                               | Interval30Seconds -- ^ The station should report at 30 second intervals.
                               | Interval15Seconds -- ^ The station should report at 15 second intervals.
                               | Interval10Seconds -- ^ The station should report at 10 second intervals.
                               | Interval5Seconds -- ^ The station should report at 5 second intervals.
                               | IntervalNextShorter -- ^ The station should report at the next shorter reporting interval for its class than its operating conditions would dictate. See section 4.2.1.
                               | IntervalNextLonger -- ^ The station should report at the next longer reporting interval for its class than its operating conditions would dictate. See section 4.2.1.
                               | Interval2Seconds -- ^ The station should report at 2 second intervals.
                               | IntervalReserved
  deriving (Eq, Enum, Show, Read)

type TenThousandthOfArcMinute = E.Pi E./ (E.ExactNatural 108000000)
type Longitude = SQuantity TenThousandthOfArcMinute DPlaneAngle Int32
type Latitude = SQuantity TenThousandthOfArcMinute DPlaneAngle Int32

data Speed s a = SpeedNotAvailable
               | SpeedHigh
               | SpeedSpecified (SQuantity s DVelocity a)
  deriving (Eq, Show)

type VelocityTenthsOfKnot = Speed (E.ExactNatural 463 E./ E.ExactNatural 9000) Word16
type VelocityKnots = Speed (E.ExactNatural 463 E./ E.ExactNatural 900) Word16

-- | The rate at which a vessel is changing its heading.
--
-- The type parameter describes the format of measured turn rates.
data RateOfTurn a = RateNotAvailable -- ^ The rate of turn is not available.
                  | RateStarboardNoSensor -- ^ The vessel is turning to starboard at a rate exceeding 5 degrees per 30 seconds. A measured rate is not available.
                  | RatePortNoSensor -- ^ The vessel is turning to port at a rate exceeding 5 degrees per 30 seconds. A measured rate is not available.
                  | RateSpecified a -- ^ The vessel is turning at a specified rate. Negative values specify turns to port, positive values specify turns to starboard.
  deriving (Eq)

-- | The rate at which a vessel is changing its heading, with measured rates represented in a compact
-- non-linear transmission format specified in Table 48.
type PackedRateOfTurn = RateOfTurn Word8

-- | The rate at which a vessel is changing its heading, with measured rates represented
-- in floating-point 'AngularVelocity' format.
type UnpackedRateOfTurn = RateOfTurn (AngularVelocity Double)

-- | Converts a 'RateOfTurn' from the compact 'PackedRateOfTurn' format used in AIS messages
-- to the linear and convenient 'UnpackedRateOfTurn' format.
unpackRateOfTurn :: PackedRateOfTurn -> UnpackedRateOfTurn
unpackRateOfTurn RateNotAvailable = RateNotAvailable
unpackRateOfTurn RateStarboardNoSensor = RateStarboardNoSensor
unpackRateOfTurn RatePortNoSensor = RatePortNoSensor
unpackRateOfTurn (RateSpecified r) = RateSpecified r''
  where
    s = (realToFrac $ signum r) D.*~ (degree / minute)
    r' = (realToFrac r P./ 4.733) P.** 2 D.*~ (degree / minute)
    r'' = copySign r' s

-- | Converts a 'RateOfTurn' from the linear and convenient 'UnpackedRateOfTurn' format
-- to the compact 'PackedRateOfTurn' format used in AIS messages.
packRateOfTurn :: UnpackedRateOfTurn -> PackedRateOfTurn
packRateOfTurn RateNotAvailable = RateNotAvailable
packRateOfTurn RateStarboardNoSensor = RateStarboardNoSensor
packRateOfTurn RatePortNoSensor = RatePortNoSensor
packRateOfTurn (RateSpecified r) = RateSpecified r''''
  where
    dpm = r D./~ (degree / minute)
    s = signum dpm
    r' = 4.733 P.* P.sqrt (P.abs dpm)
    r'' = Numeric.IEEE.copySign r' s
    r''' = round r'' :: Int
    r'''' = fromIntegral $ max (-126) $ min 126 r'''

instance Show PackedRateOfTurn where
  showsPrec _ RateNotAvailable = showString "RateNotAvailable"
  showsPrec _ RateStarboardNoSensor = showString "RateStarboardNoSensor"
  showsPrec _ RatePortNoSensor = showString "RatePortNoSensor"
  showsPrec d r@(RateSpecified raw) = showParen (d > app_prec) showStr
    where
      showStr = showString "RateSpecified " . shows raw . showString " {- " . shows r' . showString " -}"
      app_prec = 10 -- precedence of application
      (RateSpecified r') = unpackRateOfTurn r

deriving instance Show UnpackedRateOfTurn

type Direction n a = SQuantity ((E.ExactNatural 2 E.* E.Pi) E./ E.ExactNatural n) DPlaneAngle a

type AngleTenthsOfDegree = Direction 3600 Word16
type AngleDegrees = Direction 360 Word16

type LengthNauticalMiles a = SQuantity (E.ExactNatural 1852) DLength a

type TimeMinutes a = SQuantity (E.ExactNatural 60) DTime a

type LengthDecimeters a = SQuantity (E.One E./ E.ExactNatural 10) DLength a

data VesselDimensions = VesselDimensions { forwardOfReferencePoint :: Length Word16
                                         , aftOfReferencePoint :: Length Word16
                                         , portOfReferencePoint :: Length Word8
                                         , starboardOfReferencePoint :: Length Word8
                                         }
  deriving (Eq, Show)

-- | Gets the overall length of a vessel from its 'VesselDimensions'.
overallLength :: VesselDimensions -> Length Word16
overallLength dims = forwardOfReferencePoint dims + aftOfReferencePoint dims

-- | Gets the overall beam (width) of a vessel from its 'VesselDimensions'.
overallBeam :: VesselDimensions -> Length Word8
overallBeam dims = portOfReferencePoint dims + starboardOfReferencePoint dims

-- | The class of an AIS mobile station.
data StationClass = ClassA -- ^ A Class A station.
                  | ClassB (ClassBCoordinationType) -- ^ A Class B station of the specified 'CoordinationType'.
  deriving (Eq, Ord, Show, Read)

-- | The coordination type of an AIS mobile station.
data ClassBCoordinationType = SelfOrganizing -- ^ The station is capable of participating in the SOTDMA access scheme.
                            | CarrierSensing -- ^ The station uses the CSTDMA access scheme.
  deriving (Eq, Ord, Show, Read)

data StationCapabilities = StationCapabilities { stationClass :: StationClass
                                               , equippedWithDisplay :: Bool
                                               , equippedWithDigitalSelectiveCalling :: Bool
                                               , capableOfOperatingOverEntireMarineBand :: Bool
                                               , supportsChannelManagement :: Bool
                                               }
  deriving (Eq, Show, Read)

classACapabilities :: StationCapabilities
classACapabilities = StationCapabilities { stationClass = ClassA
                                         , equippedWithDisplay = True
                                         , equippedWithDigitalSelectiveCalling = True
                                         , capableOfOperatingOverEntireMarineBand = True
                                         , supportsChannelManagement = True
                                         }

-- | A type of ship and cargo.
newtype TypeOfShipAndCargo = TypeOfShipAndCargo Word8
  deriving (Eq, Ord)

instance Show TypeOfShipAndCargo where
  show t@(TypeOfShipAndCargo n) = printf "TypeOfShipAndCargo %0.2d" n ++ showDecode (decodeTypeOfShip t)
    where
      showDecode (Nothing, _) = ""
      showDecode (Just t', Nothing) = " {- " ++ show t' ++ " -}"
      showDecode (Just t', Just h)  = " {- " ++ show t' ++ " carrying " ++ show h ++ " -}"

-- | Decodes a 'TypeOfShipAndCargo' by splitting it into possibly a 'ShipType' and possibly
-- a 'HazardOrPollutantCategory'. Reserved codes are decoded to 'Nothing'.
decodeTypeOfShip :: TypeOfShipAndCargo -> (Maybe ShipType, Maybe HazardOrPollutantCategory)
decodeTypeOfShip (TypeOfShipAndCargo n) = decode n
  where
    decode 30 = simple FishingVessel
    decode 31 = simple TowingVessel
    decode 32 = simple TowingVessel
    decode 33 = simple DredgingOrUnderwaterOperationsVessel
    decode 34 = simple DivingVessel
    decode 35 = simple MilitaryVessel
    decode 36 = simple SailingVessel
    decode 37 = simple PleasureCraft
    decode 50 = simple PilotVessel
    decode 51 = simple SearchAndRescueVessel
    decode 52 = simple Tug
    decode 53 = simple PortTender
    decode 54 = simple AntiPollutionVessel
    decode 55 = simple LawEnforcementVessel
    decode 58 = simple MedicalTransport
    decode 59 = simple NonPartyToArmedConflict
    decode n' | 20 <= n' && n' <= 29 = category WingInGroundEffect
              | 40 <= n' && n' <= 49 = category HighSpeedCraft
              | 60 <= n' && n' <= 69 = category PassengerShip
              | 70 <= n' && n' <= 79 = category CargoShip
              | 80 <= n' && n' <= 89 = category Tanker
              | otherwise            = (Nothing, Nothing)
    category t = (Just t, hazard)
    simple t = (Just t, Nothing)
    hazard = hazard' (n `mod` 10)
    hazard' 1 = Just HazardX
    hazard' 2 = Just HazardY
    hazard' 3 = Just HazardZ
    hazard' 4 = Just HazardOS
    hazard' _ = Nothing

-- | An IMO Hazard or Pollutant Category.
data HazardOrPollutantCategory = HazardX -- ^ IMO Hazard or Pollutant Category X
                               | HazardY -- ^ IMO Hazard or Pollutant Category Y
                               | HazardZ -- ^ IMO Hazard or Pollutant Category Z
                               | HazardOS -- ^ IMO Hazard or Pollutant Category OS
  deriving (Eq, Ord, Enum, Show, Read)

-- | A type of ship.
data ShipType = PilotVessel
              | SearchAndRescueVessel
              | Tug
              | PortTender
              | AntiPollutionVessel
              | LawEnforcementVessel
              | MedicalTransport
              | NonPartyToArmedConflict
              | FishingVessel
              | TowingVessel
              | DredgingOrUnderwaterOperationsVessel
              | DivingVessel
              | MilitaryVessel
              | SailingVessel
              | PleasureCraft
              | WingInGroundEffect
              | HighSpeedCraft
              | PassengerShip
              | CargoShip
              | Tanker
  deriving (Eq, Ord, Show, Read)
