{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Network.AIS.Vocabulary
where

import qualified Data.ExactPi.TypeLevel as E
import Data.Int
import Data.Word
import Numeric.Units.Dimensional.FixedPoint hiding ((+))
import Numeric.Units.Dimensional.Quantities
import Text.Printf

newtype MMSI = MMSI Word32
  deriving (Eq, Ord)

instance Show MMSI where
  show (MMSI n) = printf "MMSI %0.9d" n

type RepeatIndicator = Word8

data MessageID = MScheduledClassAPositionReport
               | MAssignedScheduledClassAPositionReport
               | MSpecialClassAPositionReport
               | MBaseStationReport
               | MStaticAndVoyageData
               | MBinaryAddressedMessage
               | MBinaryAcknowledgement
               | MBinaryBroadcastMessage
               | MStandardSARAircraftPositionReport
               | MTimeInquiry
               | MTimeResponse
               | MAddressedSafetyRelatedMessage
               | MSafetyRelatedAcknowledgement
               | MSafetyRelatedBroadcastMessage
               | MInterrogation
               | MAssignmentModeCommand
               | MDGNSSBroadcastBinaryMessage
               | MStandardClassBPositionReport
               | MExtendedClassBPositionReport
               | MDataLinkManagementMessage
               | MAidsToNavigationReport
               | MChannelManagement
               | MGroupAssignmentCommand
               | MStaticDataReport
               | MSingleSlotBinaryMessage
               | MMultipleSlotBinaryMessage
               | MLongRangePositionReport
  deriving (Eq, Ord, Enum, Show, Read)

data SyncState = SyncUtcDirect
               | SyncUtcIndirect
               | SyncBaseStation
               | SyncPeer
  deriving (Eq, Ord, Enum, Show, Read)

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
  deriving (Eq, Ord, Enum, Show, Read)

data CommunicationsState = SOTDMA { syncState :: SyncState
                                  , slotTimeout :: Word8
                                  , submessage :: SOTDMASubmessage }
                         | ITDMA { syncState :: SyncState
                                 , slotIncrement :: Word16
                                 , numberOfSlots :: Word8
                                 , keep :: Bool }
  deriving (Eq, Ord, Show, Read)

data SOTDMASubmessage = ReceivedStations Word16
                      | SlotNumber Word16
                      | UTCHourAndMinute Word8 Word8
                      | SlotOffset Word16
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Enum, Show, Read)

type TenThousandthOfArcMinute = E.Pi E./ (E.ExactNatural 108000000)
type Latitude' = SQuantity TenThousandthOfArcMinute DPlaneAngle Int32
type Longitude' = SQuantity TenThousandthOfArcMinute DPlaneAngle Int32

data VesselDimensions = VesselDimensions { forwardOfReferencePoint :: Word16
                                         , aftOfReferencePoint :: Word16
                                         , portOfReferencePoint :: Word16
                                         , starboardOfReferencePoint :: Word16
                                         }
  deriving (Eq, Show)

overallLength :: VesselDimensions -> Word16
overallLength dims = forwardOfReferencePoint dims + aftOfReferencePoint dims

overallBeam :: VesselDimensions -> Word16
overallBeam dims = portOfReferencePoint dims + starboardOfReferencePoint dims
