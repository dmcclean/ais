module Network.AIS.State where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word16)
import Network.AIS (AisMessage(..))
import Network.AIS.Vocabulary

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
                                 }
  deriving (Eq, Show)

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
