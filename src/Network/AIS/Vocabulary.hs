module Network.AIS.Vocabulary
where

import Data.Word

newtype MMSI = MMSI Word32
  deriving (Eq, Ord, Show)

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
               | MTimeRepsonse
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
