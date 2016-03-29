{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.AIS.Vocabulary
where

import qualified Data.ExactPi.TypeLevel as E
import Data.Int
import Data.Word
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

type RepeatIndicator = Word8

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
                        | NavRestrictedManueverability -- ^ The vessel is operating with restricted manueverability.
                        | NavConstrainedByDraught -- ^ The vessel's manueverability is constrained by its draught.
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

data AidToNavigation = AidUnspecified
                     | AidReferencePoint
                     | AidRacon
                     | AidFixedOffshoreStructure
                     | AidEmergencyWreckMarkingBuoy
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
                     | AidLightVesselLanbyRig
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

data Addressee = Broadcast
               | Addressed MMSI
  deriving (Eq, Show)

data Reservation = Reservation { reservedOffsetNumber :: Word16
                               , reservedNumberOfSlots :: Word8
                               , reservationTimeout :: TimeMinutes Word8
                               , reservationIncrement :: Word16
                               }
  deriving (Eq, Show)

isValidReservation :: Reservation -> Bool
isValidReservation r = reservationTimeout r > _0 && reservedNumberOfSlots r > 0

data StationType = StationsAllMobile
                 | StationsClassAMobile
                 | StationsClassBMobile
                 | StationsSarAirborneMobile
                 | StationsClassBSelfOrganizingMobile
                 | StationsClassBCarrierSenseMobile
                 | StationsInlandWaterways
                 | StationsRegionalUseA
                 | StationsRegionalUseB
                 | StationsRegionalUseC
                 | StationsBaseStationCoverageArea
                 | StationsReserved
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

-- | A radio channel number of 25 kHz simplex, or simplex use of 25 kHz duplex,
-- in accordance with <https://www.itu.int/rec/R-REC-M.1084/en Recommendation ITU-R M.1084>.
type Channel = Word16

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
                  | RateSpecified a -- ^ The vessel is turning at a specified rate.
  deriving (Eq)

-- | The rate at which a vessel is changing its heading, with measured rates represented in a compact
-- non-linear transmission format specified in Table 48.
type PackedRateOfTurn = RateOfTurn Word8

-- | The rate at which a vessel is changing its heading, with measured rates represented in floating-point format.
type UnpackedRateOfTurn = RateOfTurn (AngularVelocity Double)

-- | Converts a 'RateOfTurn' from the compact 'PackedRateOfTurn' format to the linear and convenient 'UnpackedRateOfTurn' format.
unpackRateOfTurn :: PackedRateOfTurn -> UnpackedRateOfTurn
unpackRateOfTurn RateNotAvailable = RateNotAvailable
unpackRateOfTurn RateStarboardNoSensor = RateStarboardNoSensor
unpackRateOfTurn RatePortNoSensor = RatePortNoSensor
unpackRateOfTurn (RateSpecified r) = RateSpecified r''
  where
    s = (realToFrac $ signum r) D.*~ (degree / minute)
    r' = (realToFrac r P./ 4.733) P.** 2 D.*~ (degree / minute)
    r'' = copySign s r'

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

type Course = Direction 3600 Word16
type Heading = Direction 360 Word16

type LengthNauticalMiles a = SQuantity (E.ExactNatural 1852) DLength a

type TimeMinutes a = SQuantity (E.ExactNatural 60) DTime a

type LengthDecimeters a = SQuantity (E.One E./ E.ExactNatural 10) DLength a

data VesselDimensions = VesselDimensions { forwardOfReferencePoint :: LengthDecimeters Word16
                                         , aftOfReferencePoint :: LengthDecimeters Word16
                                         , portOfReferencePoint :: LengthDecimeters Word8
                                         , starboardOfReferencePoint :: LengthDecimeters Word8
                                         }
  deriving (Eq, Show)

overallLength :: VesselDimensions -> LengthDecimeters Word16
overallLength dims = forwardOfReferencePoint dims + aftOfReferencePoint dims

overallBeam :: VesselDimensions -> LengthDecimeters Word8
overallBeam dims = portOfReferencePoint dims + starboardOfReferencePoint dims

data ClassBCapabilities = ClassBCapabilities { carrierSenseUnit :: Bool
                                             , equippedWithDisplay :: Bool
                                             , equippedWithDigitalSelectiveCalling :: Bool
                                             , capableOfOperatingOverEntireMarineBand :: Bool
                                             , supportsChannelManagement :: Bool
                                             }
  deriving (Eq, Show, Read)
