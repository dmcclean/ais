{-# LANGUAGE NamedFieldPuns #-}

module Network.AIS.State where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word16)
import Network.AIS (AisMessage(..))
import Network.AIS.Vocabulary
import qualified Data.Set as S

type Slot = Word16
type SlotOffset = Word16

frameLength :: SlotOffset
frameLength = 2500

maxAge :: SlotOffset
maxAge = 8 * frameLength

data StationState = StationState { displayName :: Maybe Text
                                 , lastReceived :: Maybe Slot -- Nothing means a long time ago
                                 , lastReceivedChannel :: Channel
                                 , lastPosition :: Maybe (Longitude, Latitude)
                                 , mostRecentlyVictimized :: Maybe Slot
                                 , lastSyncState :: SyncState
                                 , lastNavigationState :: Maybe NavigationalStatus
                                 , lastReceivedStationsCount :: Maybe Word16
                                 , stationStereotype :: PossibleStationStereotype
                                 }
  deriving (Eq, Show)

data StationStereotype = BaseStation
                       | MobileStation StationClass
                       | SarAircraftStation
                       | NavaidStation
                       | EmergencyStation EmergencyDeviceType
  deriving (Eq, Ord, Show, Read)

newtype PossibleStationStereotype = PossibleStationStereotype (S.Set StationStereotype)
  deriving (Eq, Show)

unknownStation :: PossibleStationStereotype
unknownStation = PossibleStationStereotype . S.fromList $ [ BaseStation
                                                          , MobileStation ClassA
                                                          , MobileStation (ClassB SelfOrganizing)
                                                          , MobileStation (ClassB CarrierSensing)
                                                          , SarAircraftStation
                                                          , NavaidStation
                                                          , EmergencyStation SarTransponder
                                                          , EmergencyStation MobDevice
                                                          , EmergencyStation Epirb
                                                          ]

anyMobileStation :: PossibleStationStereotype
anyMobileStation = PossibleStationStereotype . S.fromList $ [ MobileStation ClassA
                                                            , MobileStation (ClassB SelfOrganizing)
                                                            , MobileStation (ClassB CarrierSensing)
                                                            ]

unconventionalStation :: PossibleStationStereotype
unconventionalStation = PossibleStationStereotype S.empty

singleton :: StationStereotype -> PossibleStationStereotype
singleton = PossibleStationStereotype . S.singleton

possibleStationStereotypes :: MMSIType -> PossibleStationStereotype
possibleStationStereotypes (MmsiShipStation _) = anyMobileStation
possibleStationStereotypes (MmsiCraftAssociatedWithParentShip _) = anyMobileStation
possibleStationStereotypes (MmsiCoastStation _) = singleton BaseStation
possibleStationStereotypes (MmsiSarAircraft _) = singleton SarAircraftStation
possibleStationStereotypes (MmsiNavigationalAid _) = singleton NavaidStation
possibleStationStereotypes (MmsiEmergencyDevice ty) = singleton (EmergencyStation ty)
possibleStationStereotypes _ = unconventionalStation

compatibleWithStereotype :: AisMessage -> StationStereotype -> Bool
compatibleWithStereotype (ClassAPositionReport { navigationalStatus })    (MobileStation ClassA) = navigationalStatus /= NavAisSartMobEpirb
compatibleWithStereotype (ClassAPositionReport { navigationalStatus })    (EmergencyStation _) = navigationalStatus == NavAisSartMobEpirb || navigationalStatus == NavUndefined
compatibleWithStereotype (StandardClassBPositionReport { capabilities })  (MobileStation c@(ClassB _)) = stationClass capabilities == c
compatibleWithStereotype (ExtendedClassBPositionReport { })               (MobileStation (ClassB _)) = True
compatibleWithStereotype (SarAircraftPositionReport { })                  SarAircraftStation = True
compatibleWithStereotype (ClassAStaticData { })                           (MobileStation ClassA) = True
compatibleWithStereotype (ClassAStaticData { typeOfShipAndCargo })        SarAircraftStation = typeOfShipAndCargo == TypeOfShipAndCargo 0
compatibleWithStereotype (StaticDataReportPartA {})                       (MobileStation _) = True
compatibleWithStereotype (StaticDataReportPartA {})                       SarAircraftStation = True
compatibleWithStereotype (StaticDataReportPartA {})                       BaseStation = True
compatibleWithStereotype (StaticDataReportPartB {})                       (MobileStation _) = True
compatibleWithStereotype (StaticDataReportPartB {})                       SarAircraftStation = True
compatibleWithStereotype (StaticDataReportPartB {})                       BaseStation = True
compatibleWithStereotype (SafetyRelatedMessage { })                       BaseStation = True
compatibleWithStereotype (SafetyRelatedMessage { })                       (MobileStation _) = True
compatibleWithStereotype (SafetyRelatedMessage { })                       SarAircraftStation = True
compatibleWithStereotype (SafetyRelatedMessage { addressee })             (EmergencyStation _) = addressee == Broadcast -- TODO: validate text?
compatibleWithStereotype (BaseStationReport { })                          BaseStation = True
compatibleWithStereotype (ChannelManagementCommand { })                   BaseStation = True
compatibleWithStereotype (GroupAssignmentCommand { })                     BaseStation = True
compatibleWithStereotype (AssignmentModeCommand { })                      BaseStation = True
compatibleWithStereotype (DataLinkManagementMessage { })                  BaseStation = True
compatibleWithStereotype (BaseStationCoverageAreaMessage { })             BaseStation = True
compatibleWithStereotype (DgnssBroadcastMessage { })                      BaseStation = True
compatibleWithStereotype _ _ = False

data StationDirectory = StationDirectory { directoryLastUpdated :: Slot
                                         , ownStationID :: Maybe MMSI
                                         , stations :: Map MMSI StationState
                                         }
  deriving (Eq, Show)

-- parameters are own station ID, current slot
makeStationDirectory :: Maybe MMSI -> Slot -> StationDirectory
makeStationDirectory ownStation t = StationDirectory { directoryLastUpdated = t
                                                     , ownStationID = ownStation
                                                     , stations = M.empty
                                                     }

tickStationDirectory :: Slot -> StationDirectory -> StationDirectory
tickStationDirectory t d = d { directoryLastUpdated = t, stations = (ageOutVictimization . ageOut) <$> stations d }
  where
    -- because the slot numbers roll over quickly, we need to age out any entries that are past a certain point
    ageOut :: StationState -> StationState
    ageOut s | Just t' <- lastReceived s, t - t' > maxAge = s { lastReceived = Nothing }
             | otherwise = s
    ageOutVictimization :: StationState -> StationState
    ageOutVictimization s | Just t' <- mostRecentlyVictimized s, t - t' > maxAge = s { mostRecentlyVictimized = Nothing }
                          | otherwise = s

updateStationDirectory :: Slot -> Channel -> AisMessage -> StationDirectory -> StationDirectory
updateStationDirectory t c m d | Just (userID m) == ownStationID d = d -- ignore messages from own station
                               | otherwise = d { stations = M.alter alteration (userID m) $ stations d }
  where
    alteration :: Maybe StationState -> Maybe StationState
    alteration (Just x) = Just $ f x
    alteration Nothing  = Just $ f defaultStationState
    defaultStationState = StationState { displayName = Nothing
                                       , lastReceived = Just t
                                       , lastReceivedChannel = c
                                       , lastPosition = Nothing
                                       , mostRecentlyVictimized = Nothing
                                       , lastSyncState = SyncPeer
                                       , lastNavigationState = Nothing
                                       , lastReceivedStationsCount = Nothing
                                       , stationStereotype = unknownStation
                                       }
    f = integrateMessage m . integrateMetadata
    extractPos :: AisMessage -> Maybe (Longitude, Latitude)
    extractPos msg = do
                       lon <- longitude msg
                       lat <- latitude msg
                       return $ (lon, lat)
    integrateMetadata :: StationState -> StationState
    integrateMetadata s = s { lastReceived = Just t, lastReceivedChannel = c }
    integrateMessage :: AisMessage -> StationState -> StationState
    integrateMessage (ClassAPositionReport {}) s = s { lastNavigationState = Just $ navigationalStatus m, lastPosition = extractPos m }
    integrateMessage (BaseStationReport {}) s = s { lastPosition = extractPos m }
    integrateMessage (TimeResponse {}) s = s { lastPosition = extractPos m }
    integrateMessage (AidToNavigationReport {}) s = s { lastPosition = extractPos m }
    integrateMessage (DgnssBroadcastMessage {}) s = s { lastPosition = extractPos m }
    integrateMessage (SarAircraftPositionReport {}) s = s { lastPosition = extractPos m }
    integrateMessage (StandardClassBPositionReport {}) s = s { lastPosition = extractPos m }
    integrateMessage (ExtendedClassBPositionReport {}) s = s { lastPosition = extractPos m }
    integrateMessage (LongRangePositionReport {}) s = s { lastPosition = extractPos m }
    integrateMessage _ s = s
