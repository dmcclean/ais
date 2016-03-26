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
                               , reservationTimeoutMinutes :: Word8
                               , reservationIncrement :: Word16
                               }
  deriving (Eq, Show)

isValidReservation :: Reservation -> Bool
isValidReservation r = reservationTimeoutMinutes r > 0 && reservedNumberOfSlots r > 0

type TenThousandthOfArcMinute = E.Pi E./ (E.ExactNatural 108000000)
type Latitude = SQuantity TenThousandthOfArcMinute DPlaneAngle Int32
type Longitude = SQuantity TenThousandthOfArcMinute DPlaneAngle Int32

type VesselSpeed = SQuantity (E.ExactNatural 463 E./ E.ExactNatural 9000) DVelocity Word16
type AircraftSpeed = SQuantity (E.ExactNatural 463 E./ E.ExactNatural 900) DVelocity Word16

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

type VesselLength a = SQuantity (E.One E./ E.ExactNatural 10) DLength a

data VesselDimensions = VesselDimensions { forwardOfReferencePoint :: VesselLength Word16
                                         , aftOfReferencePoint :: VesselLength Word16
                                         , portOfReferencePoint :: VesselLength Word8
                                         , starboardOfReferencePoint :: VesselLength Word8
                                         }
  deriving (Eq, Show)

overallLength :: VesselDimensions -> VesselLength Word16
overallLength dims = forwardOfReferencePoint dims + aftOfReferencePoint dims

overallBeam :: VesselDimensions -> VesselLength Word8
overallBeam dims = portOfReferencePoint dims + starboardOfReferencePoint dims
