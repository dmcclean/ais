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

data SyncState = SyncUtcDirect
               | SyncUtcIndirect
               | SyncBaseStation
               | SyncPeer
  deriving (Eq, Enum, Show, Read)

data NavigationalStatus = NavUnderWayEngine
                        | NavAtAnchor
                        | NavNotUnderCommand
                        | NavRestrictedManueverability
                        | NavConstrainedByDraught
                        | NavMoored
                        | NavAground
                        | NavEngagedInFishing
                        | NavUnderWaySailing
                        | NavReservedIMOHazardC
                        | NavReservedIMOHazardA
                        | NavPowerDrivenVesselTowingAstern
                        | NavPowerDrivenVesselPushingAheadOrTowingAlongside
                        | NavReserved
                        | NavAisSartMobEpirb
                        | NavUndefined
  deriving (Eq, Enum, Show, Read)

data CommunicationsState = SOTDMA { syncState :: SyncState
                                  , slotTimeout :: Word8
                                  , submessage :: SOTDMASubmessage }
                         | ITDMA { syncState :: SyncState
                                 , slotIncrement :: Word16
                                 , numberOfSlots :: Word8
                                 , keep :: Bool }
  deriving (Eq, Show, Read)

data SOTDMASubmessage = ReceivedStations Word16
                      | SlotNumber Word16
                      | UTCHourAndMinute Word8 Word8
                      | SlotOffset Word16
  deriving (Eq, Show, Read)

data PositionFixingDevice = PosFixUndefined
                          | PosFixGps
                          | PosFixGlonass
                          | PosFixGpsGlonass
                          | PosFixLoranC
                          | PosFixChayka
                          | PosFixIntegratedNavigationSystem
                          | PosFixSurveyed
                          | PosFixGalileo
                          | PosFixInternalGnss
  deriving (Eq, Enum, Show, Read)

data PositionFixingStatus = PosStatusNormal
                          | PosStatusManual
                          | PosStatusEstimated
                          | PosStatusInoperative
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

isFixedAtoN :: AidToNavigation -> Bool
isFixedAtoN aid = 5 <= x && x <= 19
  where
    x = fromEnum aid

isFloatingAtoN :: AidToNavigation -> Bool
isFloatingAtoN aid = 20 <= x
  where
    x = fromEnum aid

data Altitude = AltNotAvailable
              | AltHigh
              | AltSpecified (Length Word16)
  deriving (Eq, Show)

data AltitudeSensor = AltGnss
                    | AltBarometric
  deriving (Eq, Enum, Show, Read)

data ApplicationIdentifier = ApplicationIdentifier { designatedAreaCode :: Word16
                                                   , functionIdentifier :: Word8
                                                   }
  deriving (Eq, Show)

data Acknowledgement = Acknowledgement { destinationID :: MMSI
                                       , acknowledgedSequenceNumber :: Word8
                                       }
  deriving (Eq, Show)

data Interrogation = Interrogation { interrogatedID :: MMSI
                                   , requestedMessageType :: MessageID
                                   , requestedSlotOffset :: Word16
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

data TransmissionMode = TransmitAB
                      | TransmitA
                      | TransmitB
                      | TransmitReserved
  deriving (Eq, Enum, Read, Show)

data Assignment = Assignment { targetID :: MMSI
                             , assignedSlotOffset :: Word16
                             , assignedIncrement :: Word16
                             }
  deriving (Eq, Show)

type Channel = Word16

data TargetDesignation = GeographicTargetDesignation Region
                       | AddressedTargetDesignation [MMSI]
  deriving (Eq, Show)

data Region = Region { east :: Longitude
                     , north :: Latitude
                     , west :: Longitude
                     , south :: Latitude
                     }
  deriving (Eq, Show)

data AssignedReportingInterval = IntervalAsAutonomous
                               | IntervalTenMinutes
                               | Interval6Minutes
                               | Interval3Minutes
                               | Interval1Minute
                               | Interval30Seconds
                               | Interval15Seconds
                               | Interval10Seconds
                               | Interval5Seconds
                               | IntervalNextShorter
                               | IntervalNextLonger
                               | Interval2Seconds
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

data RateOfTurn a = RateNotAvailable
                  | RateStarboardNoSensor
                  | RatePortNoSensor
                  | RateSpecified a
  deriving (Eq)

type Direction n a = SQuantity ((E.ExactNatural 2 E.* E.Pi) E./ E.ExactNatural n) DPlaneAngle a

type Course = Direction 3600 Word16
type Heading = Direction 360 Word16

type PackedRateOfTurn = RateOfTurn Word8
type UnpackedRateOfTurn = RateOfTurn (AngularVelocity Double)

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
